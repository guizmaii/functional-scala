package net.degoes

import scalaz.zio._

import scala.io.{Codec, Source}
import scala.util.{Failure, Success, Try}
import scalaz._
import Scalaz._

/**
  * Web crawler
  * -----------
  *
  * - start with a set of see URLs
  * - for each url:
  *   - retrieve the HTML content of the URL
  *     - handle failure gracefully
  *     - retry using a crazy complicated retry strategy
  *   - parse the HTML content
  *   - feed the content and URL to a page processor
  *   - identity all href links
  *   - filter the href link according to some criteria
  *   - update the workonk set of URLs to include the href links
  *   - continue the process for as long as we have URLs to crawl
  */
object FunctionalScala extends App {

  final case class URL private (value: String) extends AnyVal {
    def relative(page: String): Option[URL] = URL(s"$value/$page")
  }
  object URL {
    final def apply(url: String): Option[URL] =
      Try(java.net.URI.create(url)) match {
        case Success(_) => Some(new URL(url))
        case Failure(_) => None
      }
  }

  final case class HTML(content: String) extends AnyVal

  final def getContent(url: URL): IO[Throwable, HTML] =
    IO.syncThrowable {
      HTML(Source.fromURL(url.value)(Codec.UTF8).mkString)
    }

  final val urlExtractorPattern = "href=\"([^\"]+)\"".r

  final def extractURLs(rootURL: URL, html: HTML): List[URL] =
    Try {
      val matches = (for (n <- urlExtractorPattern.findAllMatchIn(html.content)) yield n.group(1)).toList

      for {
        m   <- matches
        url <- (URL(m) orElse rootURL.relative(m)).toList
      } yield url
    }.getOrElse(Nil)

  final case class Crawl[E, A](errors: E, value: A) {
    def leftMap[E2](f: E => E2): Crawl[E2, A] = Crawl(f(errors), value)
    def map[A2](f: A => A2): Crawl[E, A2]     = Crawl(errors, f(value))
  }
  object Crawl {
    implicit final def CrawlMonoid[E: Monoid, A: Monoid]: Monoid[Crawl[E, A]] =
      new Monoid[Crawl[E, A]] {
        override def zero: Crawl[E, A] = Crawl(mzero[E], mzero[A])
        override def append(l: Crawl[E, A], r: => Crawl[E, A]): Crawl[E, A] =
          Crawl(l.errors |+| r.errors, l.value |+| r.value)
      }
  }

  final val defaultCrawlSchedule: Schedule[Throwable, Unit] = Schedule.recurs(20).void

  final def crawl[E: Monoid, A: Monoid](
      seeds: Set[URL],
      router: URL => Set[URL],
      processor: (URL, HTML) => IO[E, A],
      schedule: Schedule[Throwable, Unit] = defaultCrawlSchedule
  ): IO[Throwable, Crawl[E, A]] = {
    def process1(url: URL, html: HTML): IO[Nothing, Crawl[E, A]] =
      processor(url, html)
        .redeemPure(e => Crawl(errors = e, value = mzero[A]), a => Crawl(errors = mzero[E], value = a))

    def loop(seeds: Set[URL], visited: Set[URL], crawl0: Crawl[E, A]): IO[Throwable, Crawl[E, A]] =
      IO.traverse(seeds) { url =>
          for {
            html  <- getContent(url).retry(schedule)
            crawl <- process1(url, html)
            links = extractURLs(url, html).toSet.flatMap(router) diff visited
          } yield (crawl, links)
        }
        .map(_.foldMap(identity)) // IO[List[(Crawl[E, A], Set[URL])] => (Crawl[E, A], Set[URL])] because Crawl is a Monoid as well as Set. `foldMap` beauty !
        .flatMap {
          case (crawl1, links) => loop(links, visited ++ seeds, crawl0 |+| crawl1)
        }

    loop(seeds, Set(), mzero[Crawl[E, A]])
  }

  final case class ProcessorError[E](error: E, url: URL, html: HTML)
  final def crawlE[E, A: Monoid](
      seed: Set[URL],
      router: URL => Set[URL],
      processor: (URL, HTML) => IO[E, A],
      schedule: Schedule[Throwable, Unit] = defaultCrawlSchedule
  ): IO[Throwable, Crawl[List[ProcessorError[E]], A]] =
    crawl(
      seed,
      router,
      (url, html) => processor(url, html).redeem(e => IO.fail(ProcessorError(e, url, html) :: Nil), IO.now),
      schedule
    )

  final def toUrl(s: String): IO[Exception, URL] =
    IO.fromOption(URL(s)).leftMap(_ => new Exception(s"Invalid url: $s"))

  def run(args: List[String]): IO[Nothing, ExitStatus] =
    (for {
      seeds <- IO.traverse(args)(toUrl).map(_.toSet)
      _     <- console.putStrLn(s"Seeds: ${seeds.mkString("\n")}")
      router    = (url: URL) => if (url.value.contains("zio")) Set(url) else Set.empty[URL]
      processor = (url: URL, html: HTML) => console.putStrLn(s"Traversing ${url.value}: ${html.content.take(100)}")
      crawl <- crawlE(seeds, router, processor)
      _     <- console.putStrLn(crawl.errors.mkString("\n"))
    } yield ()).redeemPure(_ => ExitStatus.ExitNow(1), _ => ExitStatus.ExitNow(0))
}

package net.degoes

import scalaz.Scalaz._
import scalaz._
import scalaz.zio._

import scala.io.{Codec, Source}
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

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
object FinalTaglessFunctionalScala extends App {

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

  final val defaultCrawlSchedule: Schedule[Throwable, Unit] = Schedule.recurs(20).void

  trait HttpClient[F[_, _]] {
    def getContent(url: URL): F[Throwable, HTML]
  }
  object HttpClient {
    final def apply[F[_, _]](implicit F: HttpClient[F]): HttpClient[F] = F

    implicit final val instance: HttpClient[IO] =
      new HttpClient[IO] {
        override def getContent(url: URL): IO[Throwable, HTML] =
          IO.syncThrowable(HTML(Source.fromURL(url.value)(Codec.UTF8).mkString)).retry(defaultCrawlSchedule)
      }
  }

  def getContent[F[_, _]: HttpClient](url: URL): F[Throwable, HTML] = HttpClient[F].getContent(url)

  trait Effect[F[+ _, + _]] {
    def monad[E]: Monad[F[E, ?]]

    def fail[E](e: E): F[E, Nothing]

    def redeem[E1, E2, A, B](f: F[E1, A])(err: E1 => F[E2, B], succ: A => F[E2, B]): F[E2, B]
  }
  object Effect {
    def apply[F[+ _, + _]](implicit F: Effect[F]): Effect[F] = F

    implicit final val instance: Effect[IO] =
      new Effect[IO] {
        override def monad[E]: Monad[IO[E, ?]] =
          new Monad[IO[E, ?]] {
            override def point[A](a: => A): IO[E, A]                          = IO.point(a)
            override def bind[A, B](fa: IO[E, A])(f: A => IO[E, B]): IO[E, B] = fa.flatMap(f)
          }

        def fail[E](e: E): IO[E, Nothing] = IO.fail(e)

        override def redeem[E1, E2, A, B](f: IO[E1, A])(err: E1 => IO[E2, B], succ: A => IO[E2, B]): IO[E2, B] =
          f.redeem(err, succ)
      }

  }

  implicit def EffectMonad[F[+ _, + _]: Effect, E]: Monad[F[E, ?]] =
    new Monad[F[E, ?]] {
      def point[A](a: => A): F[E, A] = Effect[F].monad.point(a)

      def bind[A, B](fa: F[E, A])(f: A => F[E, B]): F[E, B] =
        Effect[F].monad.bind(fa)(f)
    }

  implicit class EffectSyntax[F[+ _, + _], E1, A](fea: F[E1, A]) {
    def redeem[E2, B](err: E1 => F[E2, B], succ: A => F[E2, B])(implicit F: Effect[F]): F[E2, B] =
      F.redeem(fea)(err, succ)

    def redeemPure[B](err: E1 => B, succ: A => B)(implicit F: Effect[F]): F[Nothing, B] =
      redeem[Nothing, B](err.andThen(F.monad[Nothing].point[B](_)), succ.andThen(F.monad[Nothing].point[B](_)))
  }

  case class CrawlState(content: Map[URL, HTML], error: URL => Exception)

  case class CrawlTest[+E, +A](run: CrawlState => Either[E, A])

  object CrawlTest {
    implicit val HttpClientCrawlTest: HttpClient[CrawlTest] =
      new HttpClient[CrawlTest] {
        def getContent(url: URL): CrawlTest[Exception, HTML] =
          CrawlTest(state => state.content.get(url).fold[Either[Exception, HTML]](Left(state.error(url)))(Right(_)))
      }

    implicit val EffectCrawlTest: Effect[CrawlTest] =
      new Effect[CrawlTest] {
        def fail[E](e: E): CrawlTest[E, Nothing] =
          CrawlTest(state => Left(e))

        def monad[E]: Monad[CrawlTest[E, ?]] = new Monad[CrawlTest[E, ?]] {
          def point[A](a: => A): CrawlTest[E, A] = CrawlTest(_ => Right(a))
          def bind[A, B](fa: CrawlTest[E, A])(f: A => CrawlTest[E, B]): CrawlTest[E, B] =
            CrawlTest(
              state =>
                fa.run(state) match {
                  case Left(e)  => Left(e)
                  case Right(a) => f(a).run(state)
                }
            )
        }

        def redeem[E1, E2, A, B](
            fa: CrawlTest[E1, A]
        )(err: E1 => CrawlTest[E2, B], succ: A => CrawlTest[E2, B]): CrawlTest[E2, B] =
          CrawlTest(
            state =>
              fa.run(state) match {
                case Left(e1) => err(e1).run(state)
                case Right(a) => succ(a).run(state)
              }
          )
      }
  }

  val TestData1: CrawlState =
    CrawlState(
      Map(
        URL("http://scalaz.org").get -> HTML(
          """<a href="index.html">This link</a> <a href="missing-page.html">Bad link</a>"""
        ),
        URL("http://scalaz.org/index.html").get -> HTML(
          """<a href="overview.html">This link</a> <a href="index.html">Link to itself</a>"""
        ),
        URL("http://scalaz.org/overview.html").get -> HTML(
          """<a href="http://scalaz.org/index.html">Back to home</a>"""
        )
      ),
      url => new Exception("Bad URL: " + url)
    )

  val TestResult1 = crawlE(
    Set(URL("http://scalaz.org").get),
    Set(_),
    (url: URL, html: HTML) => /*_*/ CrawlTest(_ => Right((url, html) :: Nil)) /*_*/
  ).run(TestData1)

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

  def crawl[F[+ _, + _]: HttpClient: Effect, E: Monoid, A: Monoid](
      seeds: Set[URL],
      router: URL => Set[URL],
      processor: (URL, HTML) => F[E, A]
  ): F[Throwable, Crawl[E, A]] = {
    def loop(seeds: Set[URL], visited: Set[URL], crawl0: Crawl[E, A]): F[Throwable, Crawl[E, A]] =
      /*_*/
      seeds.toList
        .traverse { url =>
          for {
            html  <- getContent[F](url)
            crawl <- process1(url, html)
            links = extractURLs(url, html).toSet.flatMap(router) diff visited
          } yield (crawl, links)
        }
        .map(_.foldMap(identity))
        .flatMap { case (crawl1, links) => loop(links, visited ++ seeds, crawl0 |+| crawl1) }
    /*_*/

    def process1(url: URL, html: HTML): F[Nothing, Crawl[E, A]] =
      processor(url, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _))

    loop(seeds, Set(), mzero[Crawl[E, A]])
  }

  final case class ProcessorError[E](error: E, url: URL, html: HTML)
  def crawlE[F[+ _, + _]: Effect: HttpClient, E, A: Monoid](
      seeds: Set[URL],
      router: URL => Set[URL],
      processor: (URL, HTML) => F[E, A]
  ): F[Throwable, Crawl[List[ProcessorError[E]], A]] =
    crawl(
      seeds,
      router,
      (url, html) =>
        processor(url, html)
          .redeem(
            e => Effect[F].fail(List(ProcessorError(e, url, html))),
            a => Effect[F].monad[List[ProcessorError[E]]].point[A](a)
          )
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

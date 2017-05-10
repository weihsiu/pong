package pong.experiment

import cats.effect.Effect
import fs2._
import fs2.util.Attempt

import scala.concurrent.ExecutionContext
import scala.scalajs.js
import scala.concurrent.duration._
import scala.language.higherKinds
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

object FS2Sandbox extends js.JSApp {
  object Pull_ {
    def takeWhile[F[_], O](p: O => Boolean)(h: Handle[F, O]): Pull[F, O, Nothing] = for {
      (x, h) <- h.await1
      y <- if (p(x)) Pull.output1(x) >> takeWhile(p)(h) else Pull.done
    } yield y
    def intersperse[F[_], O](sep: O)(h: Handle[F, O]): Pull[F, O, Nothing] = for {
      h <- h.echo1
      (_, h) <- h.peek1
      y <- Pull.output1(sep) >> intersperse(sep)(h)
    } yield y
    def scan[F[_], O, O2](z: O2)(f: (O2, O) => O2)(h: Handle[F, O]): Pull[F, O2, Nothing] = for {
      (x, h) <- Pull.output1(z) >> h.await1
      y <- scan(f(z, x))(f)(h)
    } yield y
  }
  implicit class RichStream[F[_], O](stream: Stream[F, O]) {
    def repeat2: Stream[F, O] = stream ++ repeat2
    def drain2: Stream[F, Nothing] = stream >> Stream.empty
    def attempt2: Stream[F, Attempt[O]] = stream.map(Right(_)).onError(e => Stream.emit(Left(e)))
    def takeWhile2(p: O => Boolean): Stream[F, O] = stream.pull(Pull_.takeWhile(p))
    def intersperse2(sep: O): Stream[F, O] = stream.pull(Pull_.intersperse(sep))
  }
  def eval_2[F[_], A](fa: F[A]): Stream[F, Nothing] = Stream.eval(fa) >> Stream.empty
  object pipe2_ {
//    implicit val strategy: Strategy = Strategy
    implicit val scheduler = Scheduler.default
    def mergeHaltBoth[F[_] : Effect, O](implicit ec: ExecutionContext): Pipe2[F, O, O, O] = (s1, s2) => {
      def go(l: ScopedFuture[F, Pull[F, Nothing, (NonEmptyChunk[O], Handle[F, O])]],
             r: ScopedFuture[F, Pull[F, Nothing, (NonEmptyChunk[O], Handle[F, O])]]): Pull[F, O, Nothing] = {
        //        implicit val strategy: Strategy = Strategy.default
        implicit val scheduler = Scheduler.default
        //        implicit val strategy = implicitly[fs2.Strategy]
        (l race r).pull flatMap {
          case Left(l) => l.optional flatMap {
            case None => Pull.done
            case Some((c, h)) => Pull.output(c) >> h.awaitAsync.flatMap(go(_, r))
          }
          case Right(r) => r.optional flatMap {
            case None => Pull.done
            case Some((c, h)) => Pull.output(c) >> h.awaitAsync.flatMap(go(l, _))
          }
        }
      }

      s1.pull2(s2)((h1, h2) => for {
        l <- h1.awaitAsync
        r <- h2.awaitAsync
      } yield go(l, r))
    }
  }
  def main(): Unit = {
    import pipe2_._
    println(Stream(1, 0).repeat2.take(6).toList)
    println(Stream(1, 2, 3).drain.toList)
    println(eval_2(Task.delay(println("!!"))).runLog.unsafeRunSync)
    println((Stream(1, 2) ++ (throw new Exception("nooo"))).attempt2.asInstanceOf[Stream[Nothing, Attempt[Int]]].toList)
    println(Stream.range(0, 100).takeWhile2(_ < 7).toList)
    println(Stream.range(0, 100).takeWhile2(_ < 0).toList)
    println(Stream("alice", "bob", "carol").intersperse2("|").toList)
    println(Stream("alice").intersperse2("|").toList)
    println(Stream.range(1, 10).scan(0)(_ + _).toList)
    val s1 = time.awakeEvery[Task](500.millis).take(10)
    val s2 = time.awakeEvery[Task](300.millis).take(10)
    mergeHaltBoth(implicitly[Effect[Task]], implicitly[ExecutionContext])(s1, s2).runLog.unsafeRunAsyncFuture.foreach(println)
  }
}

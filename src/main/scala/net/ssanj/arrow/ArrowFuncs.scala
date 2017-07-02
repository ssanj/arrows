package net.ssanj.arrow

import cats.implicits._
import cats.arrow.Arrow

object ArrowFuncs {

  def combine[F[_, _]: Arrow, A, B, C](fab: F[A, B], fac: => F[A, C]): F[A, (B, C)] = {
    val fa = implicitly[Arrow[F]]
    fa.lmap[(A, A), (B, C), A](fa.split[A, B, A, C](fab, fac))(a => (a, a))
  }
}
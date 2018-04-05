import scala.language.postfixOps

trait Functor[F[_]]{
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]
  def map[A,B](ma: F[A])(f: A => B): F[B]=
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A,B) => C ): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a,b)))
}
object Monad {

  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
  val optionMonad = new Monad[Option] {
    def unit[A](a: => A) = Some(a)
    def flatMap[A,B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
  }
  def main(args: Array[String]): Unit = {
   assert(listFunctor.map(List(1,2,3))(1+)==List(2,3,4))
    assert(optionMonad.unit(2.0)==Some(2.0))
    assert(optionMonad.flatMap(Some(1))(x => Some(x - 1))==Some(0))
    assert(optionMonad.flatMap(None)(x => Some(1))== None)
  }
}
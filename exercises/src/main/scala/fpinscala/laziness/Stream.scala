package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def map[B](f: A => B): Stream[B] = {
    // this.foldRight(empty[B])((a, z) => cons(f(a), z))
    unfold(this: Stream[A]) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  }

  def filter(p: A => Boolean): Stream[A] = {
    this.foldRight(empty[A])((a, z) => if (p(a)) cons(a, z) else z)
  }

  def append[B>:A](s: => Stream[B]): Stream[B] = {
    this.foldRight(s)((a, z) => cons(a, z))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    this.foldRight(empty[B])((a, z) => f(a).append(z))
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.


  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = (n, this) match {
    case (0, _) => empty
    case (k, Cons(h, t)) => cons(h(), t().take(n-1))
  }

  def takeWithUnfold(n: Int): Stream[A] =
    unfold((n, this)) {
      case (0, _) => None
      case (k, Cons(h, t)) => Some((h(), (k-1, t())))
      case _ => None
    }

  def drop(n: Int): Stream[A] = (n, this) match {
    case (0, _) => this
    case (k, Cons(_, t)) => t().drop(k - 1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this.foldRight(empty: Stream[A])((a, z) => if (p(a)) cons(a, z) else empty)
  }

  def takeWhileWithUnfold(p: A=> Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(h, t), Cons(i, u)) => Some((f(h(), i()), (t(), u())))
      case _ => None
    }

  def zip[B](s: Stream[B]): Stream[(A, B)] = zipWith(s)((_,_))
  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s)) {
      case (Cons(h, t), Cons(i, u)) => Some((Some(h()), Some(i())), (t(), u()))
      case (Cons(h, t), _) => Some((Some(h()), None), (t(), empty))
      case (_, Cons(h, t)) => Some((None, Some(h())), (empty, t()))
      case _ => None
    }

  def forAll(p: A => Boolean): Boolean = {
    this.foldRight(true)((a, z) => p(a) && z)
  }

  def headOption: Option[A] =
    this.foldRight(None: Option[A])((a, z) => Some(a))

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = zipAll(s).takeWhile(!_._2.isEmpty)forAll {case (a,b) => a == b}

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Cons(h,t) => Some((cons(h(), t()), t()))
      case _ => None
    } append(Stream(empty))
  }

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    this.foldRight((Stream(z), z)){
      case (a, (s, zz)) =>
        val y = f(a, zz)
        ((cons(y,s), y))
    }._1
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = {
    // Stream.cons(1, ones)
    unfold(())(_ => Some((1, ())))
  }

  def constant[A](a: A): Stream[A] = {
    // lazy val tail: Stream[A] = Cons(() => a, () => tail)
    // tail

    unfold(())(_ => Some((a, ())))
  }
  def from(n: Int): Stream[Int] = {
    // cons(n, from(n+1))
    unfold(n)(k => Some((k, k + 1)))
  }

  def fibs: Stream[Int] = {
    // def fib(a: Int, b: Int): Stream[Int] = cons(a, fib(b, a + b))
    // fib(0, 1)
    unfold((0, 1)) {case (a,b) => Some((b, (b, a+b)))}
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
  }

}

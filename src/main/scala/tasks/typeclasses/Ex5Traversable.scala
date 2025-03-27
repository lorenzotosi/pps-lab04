package u04lab
import u03.Sequences.*
import Sequence.*
import u03.Optionals.*
import u03.Optionals.Optional.*

import scala.annotation.tailrec

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  def log[A](a: A): Unit = println("The next element is: "+a)
  def log2[A](a: A): Unit = println("The next x is: "+a)

  def logAll[A](seq: Sequence[A]): Unit = seq match
    case Cons(h, t) => log(h); logAll(t)
    case _ => ()

  def logAll[T[_], A](t: T[A], v: A => Unit = log)(using traversable: Traversable[T]): Unit =
    traversable.traverse(t)(v)

  trait Traversable[T[_]]:
    def traverse[A](t: T[A])(f: A => Unit): Unit

  given Traversable[Optional] with
    def traverse[A](t: Optional[A])(f: A => Unit): Unit = t match
      case Just(a) => f(a)
      case _ => ()

  given Traversable[Sequence] with
    def traverse[A](t: Sequence[A])(f: A => Unit): Unit = t match
      case Cons(h, t) => f(h); traverse(t)(f)
      case _ => ()

  @main def tryThis =
    val seq = Cons(1, Cons(2, Cons(3, Nil())))
    logAll(seq, log2)

    val opt = Just(5)
    logAll(opt)

    val seqOfOptionalInt: Sequence[Optional[Int]] = Cons(Just(1), Cons(Empty(), Cons(Just(3), Nil())))
    logAll(seqOfOptionalInt, log2)

    val seqOfString: Sequence[String] = Cons("Hello", Cons("World", Cons("Scala", Nil())))
    logAll(seqOfString)
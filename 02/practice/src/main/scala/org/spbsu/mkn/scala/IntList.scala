package org.spbsu.mkn.scala
import org.spbsu.mkn.scala.IntList.undef

sealed trait IntList {
  def head: Int
  def tail: IntList
  def drop(n: Int): IntList
  def take(n: Int): IntList
  def map(f: Int => Int): IntList
  def ::(elem: Int): IntList                  = List(elem, IntNil)
  def foldLeft[B](z: B)(op: (B, Int) => B): B = this match {
    case IntNil => z
    case List(x, xs) => xs.foldLeft(op(z, x)){op}
  }
}

object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq(seq: Seq[Int]): IntList = seq.foldRight(IntNil:IntList){List}
  def sum(intList: IntList): Int      = intList match {
    case IntNil => undef
    case _ => intList.foldLeft(0){_ + _}
  }
  def size(intList: IntList): Int     = intList.foldLeft(0){(a, _) => a + 1}
}

case class List(x: Int, xs: IntList) extends IntList {
  override def head: Int                    = this match { case List(x, _) => x }
  override def tail: IntList                = this match { case List(_, xs) => xs }
  override def drop(n: Int): IntList        = if (n <= 0) this else tail.drop(n-1)
  override def take(n: Int): IntList        = this match {
    case _ if n <= 0 => IntNil
    case List(x,xs) => List(x, xs.take(n-1))
  }
  override def map(f: Int => Int): IntList  = this match { case List(x, xs) => List(f(x), xs.map(f)) }
}

case object IntNil extends IntList {
  override def head: Int                    = undef
  override def tail: IntList                = undef
  override def drop(n: Int): IntList        = if (n == 0) this else undef
  override def take(n: Int): IntList        = if (n == 0) this else undef
  override def map(f: Int => Int): IntList  = this
}

package org.spbsu.mkn.scala

import org.scalatest.funsuite.AnyFunSuite
import org.spbsu.mkn.scala.MyGenericList.{ComparatorGList, ComparatorInt, MyNil, fromSeq, plus, size, sort, sum, turnerInt, turnerStr}

class MyGenericListTest extends AnyFunSuite {
  test("head") {
    assert(fromSeq(Seq(1,2,3)).head == 1)
    assert(fromSeq(Seq(1)).head == 1)
    assertThrows[UnsupportedOperationException](fromSeq(Seq()).head)
  }

  test("tail") {
    assert(fromSeq(Seq(1,2,3)).tail == fromSeq(Seq(2,3)))
    assert(fromSeq(Seq(1)).tail == MyNil)
  }

  test("drop") {
    assert(fromSeq(Seq(1,2,3)).drop(0) == fromSeq(Seq(1,2,3)))
    assert(fromSeq(Seq(1,2,3)).drop(2) == fromSeq(Seq(3)))
    assert(fromSeq(Seq(1,2,3)).drop(3) == MyNil)
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1,2,3)).drop(10))
  }

  test("take") {
    assert(fromSeq(Seq(1,2,3)).take(0) == MyNil)
    assert(fromSeq(Seq(1,2,3)).take(2) == fromSeq(Seq(1,2)))
    assert(fromSeq(Seq(1,2,3)).take(3) == fromSeq(Seq(1,2,3)))
    assertThrows[UnsupportedOperationException](fromSeq(Seq(1,2,3)).take(10))
  }

  test("map") {
    assert(MyNil[Int].map(_ * 2) == MyNil)
    assert(fromSeq(Seq(1,2,3)).map(_ * 2) == fromSeq(Seq(2,4,6)))
    assert(fromSeq(Seq(1,2,3)).map(identity) == fromSeq(Seq(1,2,3)))
  }

  test("size") {
    assert(size(MyNil) == 0)
    assert(size(fromSeq(Seq(1,2,3))) == 3)
  }

  test("sum") {
    assertThrows[UnsupportedOperationException](sum(MyNil[Int])(0))
    assert(sum(fromSeq(Seq(1,2,3)))(0) == 6)
    assert(sum(fromSeq(Seq(1)))(0) == 1)
    assert(sum(fromSeq(Seq("a", "bc", "de", "f")))("t")(Comparator.turnerStr2) == "tabcdef")
    assert(sum(fromSeq(Seq("a", "bc", "de", "f")))(0) == 6)
  }

  test("filter") {
    assert(fromSeq(Seq(1,2,3)).filter(_ == 2) == fromSeq(Seq(2)))
    assert(fromSeq(Seq(1,2,3)).filter(_ != 2) == fromSeq(Seq(1,3)))
    assert(fromSeq(Seq(1,2,3)).filter(_ > 6) == MyNil)
    assert(MyNil[Int].filter(_ == 1) == MyNil)
    assert(MyNil[Int].filter(_ != 1) == MyNil)
  }

  test("covariance") {
    abstract class Animal {
      def name: String
    }
    case class Cat(name: String) extends Animal
    case class Dog(name: String) extends Animal

    def animalListSize(animal: MyGenericList[Animal]): Int = size(animal)

    val cats: MyGenericList[Cat] = fromSeq(Seq(Cat("Cat1"), Cat("Cat2")))
    val dogs: MyGenericList[Dog] = fromSeq(Seq(Dog("Dog1"), Dog("Dog2")))
    assert (animalListSize(cats) == 2)
    assert (animalListSize(dogs) == 2)
  }

  test("plus") {
    assert(plus(fromSeq(Seq(1,2,3)),fromSeq(Seq(4,5,6))) == fromSeq(Seq(1,2,3,4,5,6)))
    assert(plus(MyNil,fromSeq(Seq(4,5,6))) == fromSeq(Seq(4,5,6)))
    assert(plus(fromSeq(Seq(1,2,3)),MyNil) == fromSeq(Seq(1,2,3)))
    assert(plus(MyNil,MyNil) == MyNil)
  }

  test("sort") {
    assert(sort(fromSeq(Seq(9,4,1,3,4,9,2,0))) == fromSeq(Seq(0,1,2,3,4,4,9,9)))
    assert(sort(fromSeq(Seq(1,2,3))) == fromSeq(Seq(1,2,3)))
    assert(sort(fromSeq(Seq(25,30,75,42,39,25,8,31,98,12,59,23,7,60,18)))  ==
      fromSeq(Seq(7,8,12,18,23,25,25,30,31,39,42,59,60,75,98)))
    assert(sort(fromSeq(Seq(5,2,21,2,3,2,5,1,5,2))) ==
      fromSeq(Seq(1,2,2,2,2,3,5,5,5,21)))
    assert(sort(fromSeq(Seq(fromSeq(Seq(1,2)), MyNil, fromSeq(Seq(1)), MyNil, fromSeq(Seq(3,1,2))))) ==
      fromSeq(Seq(MyNil, MyNil, fromSeq(Seq(1)), fromSeq(Seq(1,2)), fromSeq(Seq(3,1,2)))))
    assert(sort(fromSeq(Seq(fromSeq(Seq(1)), fromSeq(Seq(5)), fromSeq(Seq(2,2))))) ==
      fromSeq(Seq(fromSeq(Seq(1)), fromSeq(Seq(2,2)), fromSeq(Seq(5)))))
    assert(sort(fromSeq(Seq(MyNil, fromSeq(Seq(3,3,3,3)), fromSeq(Seq(3,3,3,2))))) ==
      fromSeq(Seq(MyNil, fromSeq(Seq(3,3,3,2)), fromSeq(Seq(3,3,3,3)))))

    assert(sort(fromSeq(Seq(fromSeq(Seq(1,2)), MyNil, fromSeq(Seq(1)), MyNil, fromSeq(Seq(3,1,2)))))(Comparator.ComparatorGList3) ==
      fromSeq(Seq(fromSeq(Seq(3,1,2)), fromSeq(Seq(1,2)), fromSeq(Seq(1)), MyNil, MyNil)))
    assert(sort(fromSeq(Seq(fromSeq(Seq(MyNil, MyNil)), fromSeq(Seq(MyNil, Seq(fromSeq(Seq(1,2))))))))(Comparator.ComparatorGList3) ==
      fromSeq(Seq(fromSeq(Seq(MyNil, Seq(fromSeq(Seq(1,2))))), fromSeq(Seq(MyNil, MyNil)))))
    assert(sort(fromSeq(Seq(fromSeq(Seq(1)), fromSeq(Seq(5)), fromSeq(Seq(2,2)))))(Comparator.ComparatorGList3) ==
      fromSeq(Seq(fromSeq(Seq(2,2)), fromSeq(Seq(5)), fromSeq(Seq(1)))))
    assert(sort(fromSeq(Seq(fromSeq(Seq(1)), fromSeq(Seq(5)), fromSeq(Seq(2,2)))))(Comparator.ComparatorGList2) ==
      fromSeq(Seq(fromSeq(Seq(5)), fromSeq(Seq(2,2)), fromSeq(Seq(1)))))
  }
}
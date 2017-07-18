package net.ssanj
package arrow

import cats.arrow.Arrow
import cats.implicits._
import model._

object Main extends App {

  val fa = Arrow[Function1]

  val name = Name("Nagate", "Tanikaze")

  val age = Age(22)

  private def doubleNumber: Int => Int = _ * 2

  private def upperFirstName: String => String = _.toUpperCase

  private def upperName: Name => Name = n => Name(upperFirstName(n.first), n.last)

  private def doubleAge: Age => Age = a => Age(doubleNumber(a.age))

  private def lift(): Unit = {
    val intF1 = fa.id[Int] //Function1[Int, Int]
    println(intF1(5))
  }

  private def first(): Unit = {
    val onlyNameF: ((Name, Age)) => (Name, Age) = fa.first[Name, Name, Age](upperName)
    val toPersonF: ((Name, Age)) => Person = onlyNameF andThen (Person.apply _).tupled
    val result: Person = toPersonF(name, age)
    println(s"first: $result")
  }

  private def second(): Unit = {
    val onlyAgeF: ((Name, Age)) => (Name, Age) = fa.second[Age, Age, Name](doubleAge)
    val toPersonF: ((Name, Age)) => Person = onlyAgeF andThen (Person.apply _).tupled
    val result: Person = toPersonF(name, age)
    println(s"second: $result")
  }

  private def split(): Unit = {
    val bothNameAndAgeF: ((Name, Age)) => (Name, Age) = fa.split[Name, Name, Age, Age](upperName, doubleAge)
    val toPersonF: ((Name, Age)) => Person = bothNameAndAgeF andThen (Person.apply _).tupled
    val result: Person = toPersonF(name, age)
    println(s"split: $result")
  }

  private def combine(): Unit = {

    val person = Person(name, age)

    val combineName: Person => String = {
      case Person(Name(first, last), _) => s"$first $last"
    }

    val combineAge: Person => Int = _.age.age

    val combineF: Person => (String, Int) = ArrowFuncs.combine(combineName, combineAge)
    val result: (String, Int) = combineF(person)
    println(s"combine: $result")
  }

  private def liftA2Ex(): Unit = {
    val person = Person(name, age)

    val combineName: Person => String = {
      case Person(Name(first, last), _) => s"$first $last"
    }

    val combineAge: Person => Int = _.age.age

    def makePersonString: String => Int => String = name => age => s"person[name='$name', age=$age]"

    val lifta2: Person => String = ArrowFuncs.liftA2(combineName, combineAge)(makePersonString)
    val result: String = lifta2(person)
    println(s"liftA2: $result")

  }

  lift()
  first()
  second()
  split()
  combine()
  liftA2Ex()
}
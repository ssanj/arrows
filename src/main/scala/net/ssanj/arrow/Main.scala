package net.ssanj
package arrow

import cats.arrow.Arrow
import cats.implicits._
import model._

object Main extends App {

  val fa = Arrow[Function1]

  val name = Name("Nagate", "Tanikaze")

  val age = Age(22)

  def doubleNumber: Int => Int = _ * 2

  def upperFirstName: String => String = _.toUpperCase

  def upperName: Name => Name = n => Name(upperFirstName(n.first), n.last)

  def doubleAge: Age => Age = a => Age(doubleNumber(a.age))

  def first(): Unit = {
    val onlyNameF: ((Name, Age)) => (Name, Age) = fa.first[Name, Name, Age](upperName)
    val toPersonF: ((Name, Age)) => Person = onlyNameF andThen (Person.apply _).tupled
    val result: Person = toPersonF(name, age)
    println(s"first: $result")
  }

  def second(): Unit = {
    val onlyAgeF: ((Name, Age)) => (Name, Age) = fa.second[Age, Age, Name](doubleAge)
    val toPersonF: ((Name, Age)) => Person = onlyAgeF andThen (Person.apply _).tupled
    val result: Person = toPersonF(name, age)
    println(s"second: $result")
  }

  def split(): Unit = {
    val bothNameAndAgeF: ((Name, Age)) => (Name, Age) = fa.split[Name, Name, Age, Age](upperName, doubleAge)
    val toPersonF: ((Name, Age)) => Person = bothNameAndAgeF andThen (Person.apply _).tupled
    val result: Person = toPersonF(name, age)
    println(s"split: $result")
  }

  def combine(): Unit = {

    val person = Person(name, age)

    val combineName: Person => String = {
      case Person(Name(first, last), _) => s"$first $last"
    }

    val combineAge: Person => Int = _.age.age

    val combineF: Person => (String, Int) = ArrowFuncs.combine(combineName, combineAge)
    val result: (String, Int) = combineF(person)
    println(s"combine: $result")
  }

  first()
  second()
  split()
  combine()
}
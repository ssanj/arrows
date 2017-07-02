package net.ssanj
package arrow

import cats.arrow.Arrow
import cats.implicits._
import model._

object Main extends App {

  val fa = Arrow[Function1]

  val name = Name("Nagate", "Tanikaze")

  val age = Age(22)

  def doubleAge: Int => Int = _ * 2

  def upperFirstName: String => String = _.toUpperCase

  def upperName: Name => Name = n => Name(upperFirstName(n.first), n.last)

  def first(): Unit = {
    val onlyNameF: ((Name, Age)) => (Name, Age) = fa.first[Name, Name, Age](upperName)
    val toPersonF: ((Name, Age)) => Person = onlyNameF andThen (Person.apply _).tupled
    val result: Person = toPersonF(name, age)
    println(result)
  }

  first()

}
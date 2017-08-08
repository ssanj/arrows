package net.ssanj
package arrow

import scala.collection.immutable.Range
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

  private def id(): Unit = {
    val intF1 = fa.id[Int] //Function1[Int, Int]
    val result = intF1(5)
    println(s"id(5): $result")
  }

  private def lift(): Unit = {
    val intF1 = fa.lift[Int, Boolean](_ > 10)
    val result = intF1(20)
    println(s"lift(Int => Boolean): $result")
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

  private def compose(): Unit = {
    def personA = fa.lift[(Name, Age), Person](na => Person(na._1, na._2))
    val makePersonStringA = fa.lift[Person, String](p =>  s"person[name='${p.name.first}' ${p.name.last}, age=${p.age} yrs]")

    val composeF: Tuple2[Name, Age] => String = personA >>> makePersonStringA
    val andThenF: Tuple2[Name, Age] => String =  makePersonStringA <<< personA
    val result1: String = composeF(name, age)
    val result2: String = andThenF(name, age)
    println(s"compose: $result1")
    println(s"andThen: $result2")
  }

  private def pipeline(): Unit = {
    import Functions._

    // User => (UserData => List[ItemId], (Item => ItemDescReq))
    val f1 = ArrowFuncs.combine(getSavedItems, idToDesc)

    // User => List[ItemDescReq]
    val f2 = f1 >>> { case (fi, fd) =>  fi(userData) map fd }

    //User => (Results, Results)
    val f3 = f2 >>> (_ map getDetails) >>> (_ map (_(itemData))) >>> (_.partition(_.isLeft))

    //(Results, Results) => (Results, List[ItemDetail])
    val f4 = fa.second[Results, List[ItemDetail], Results](_ collect { case Right(value) => value })

    //User => (Results, List[ItemDetail])
    val f5 = f3 >>> f4

    //(Results, List[ItemDetail]) => (Results, Tuple2[Range, Range] => ValuableItemsResponse)
    val f6 =
      fa.second[List[ItemDetail],
                Tuple2[Range, Range] => ValuableItemsResponse,
                Results](
        items => prices => valuableItemsResponse(prices)(items)
      )

     //User => (Results, Tuple2[Range, Range] => ValuableItemsResponse)
     val f7 = f5 >>>  f6

     //(Results, Tuple2[Range, Range] => ValuableItemsResponse) => (Results, ValuableItemsResponse)
     val f8 =
      fa.second[
        Tuple2[Range, Range] => ValuableItemsResponse,
        ValuableItemsResponse,
        Results](_(Range(500, 3000), Range(10000, 100000)))

    //User => (Results, ValuableItemsResponse)
    val f9 = f7 >>> f8

    //(Results, ValuableItemsResponse) => (String, String)
    val f10 = fa.split[Results, String, ValuableItemsResponse, String](
      errorString, valuableItemsResponseString
    )

    //User => (String, String)
    val f11 = f9 >>> f10

    val (errors, values) = f11(User("Guybrush threepwood", "1000"))

    println(s"pipeline: $values, errors: $errors")
  }

  private def pipelineCondensed(): Unit = {
    import Functions._

    val pipeline =
      ArrowFuncs.combine(getSavedItems, idToDesc) >>>
      { case (fi, fd) =>  fi(userData) map fd } >>>
      (_ map getDetails) >>>
      (_ map (_(itemData))) >>>
      (_.partition(_.isLeft)) >>>
      fa.second[Results, List[ItemDetail], Results](_ collect { case Right(value) => value }) >>>
      fa.second[List[ItemDetail], Tuple2[Range, Range] => ValuableItemsResponse, Results](
        items => prices => valuableItemsResponse(prices)(items)
      ) >>>
      fa.second[Tuple2[Range, Range] => ValuableItemsResponse, ValuableItemsResponse, Results](
        _(Range(500, 3000), Range(10000, 100000))
      ) >>>
      fa.split[Results, String, ValuableItemsResponse, String](
        errorString, valuableItemsResponseString
      )

      val (errors, values) = pipeline(User("Guybrush threepwood", "1000"))

      println(s"pipeline condensed: $values, errors: $errors")
  }

  id()
  lift()
  first()
  second()
  split()
  combine()
  liftA2Ex()
  compose()
  pipeline()
  pipelineCondensed()
}
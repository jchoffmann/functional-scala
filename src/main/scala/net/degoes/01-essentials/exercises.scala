// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.essentials

object types {
  type ??? = Nothing

  //
  // EXERCISE 1
  //
  // List all values of the type `Boolean`.
  //
  val BoolValues: List[Boolean] = List(true, false)

  //
  // EXERCISE 2
  //
  // List all values of the type `Either[Unit, Boolean]`.
  //
  val EitherUnitBoolValues: List[Either[Unit, Boolean]] = // Colon = "has type", "is element of"
  List(Left(Unit), Right(true), Right(false))

  //
  // EXERCISE 3
  //
  // List all values of the type `(Boolean, Boolean)`.
  //
  val TupleBoolBoolValues: List[(Boolean, Boolean)] =
  List((true, true), (true, false), (false, true), (false, false))

  //
  // EXERCISE 4
  //
  // List all values of the type `Either[Either[Unit, Unit], Unit]`.
  //
  val EitherEitherUnitUnitUnitValues: List[Either[Either[Unit, Unit], Unit]] = // Isomorphic to Ex. 2
  List(Left(Left(Unit)), Left(Right(Unit)), Right(Unit))

  //
  // EXERCISE 5
  //
  // Create a product type of `Int` and `String`, representing the age and
  // name of a person.
  //
  type Person = (Int, String)
  case class Person2(age: Int, name: String)

  //
  // EXERCISE 6
  //
  // Prove that `A * 1` is equivalent to `A` by implementing the following two
  // functions.
  //
  def to1[A](t: (A, Unit)): A = t._1
  def from1[A](a: A): (A, Unit) = (a, ()) // !!! Use the value (), not the type Unit !!!
  //def from1[A](a: A): (A, Unit) = a -> ((): Unit) // !!! Use the value (), not the type Unit !!!
  //
  // Can't do this with Nothing - there are no values of type Nothing
  // Product has to have everything

  //
  // EXERCISE 7
  //
  // Prove that `A * 0` is equivalent to `0` by implementing the following two
  // functions.
  //
  def to2[A](t: (A, Nothing)): Nothing = t._2
  def from2[A](n: Nothing): (A, Nothing) = n

  //
  // EXERCISE 8
  //
  // Create a sum type of `Int` and `String` representing the identifier of
  // a robot (a number) or a person (a name).
  //
  type Identifier = Either[Int, String]
  sealed trait Identifier2
  case class Robot(identifier: Int) extends Identifier2
  case class Human(name: String) extends Identifier2

  // Model sum types using sealed traits
  sealed trait ProgrammingLanguage // sealed means the following subtypes are the only ones
  case object Scala extends ProgrammingLanguage
  case object Haskell extends ProgrammingLanguage
  case object PureScript extends ProgrammingLanguage
  case object APL extends ProgrammingLanguage

  //
  // EXERCISE 9
  //
  // Prove that `A + 0` is equivalent to `A` by implementing the following two
  // functions.
  //
  def to3[A](t: Either[A, Nothing]): A = t match {
    case Left(a) => a
    case Right(n) => n // cannot exist, there are no values of type nothing; compiler will complain
  }
  def from3[A](a: A): Either[A, Nothing] = Left(a)
  //
  // Can't do this with Unit - how do you get an A out of Unit???

  //
  // EXERCISE 10
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // credit card, which has a number, an expiration date, and a security code.
  //
  //type CreditCard = ???
  case class CreditCard(number: Int, expiration: String, securityCode: Int)

  //
  // EXERCISE 11
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // payment method, which could be a credit card, bank account, or
  // cryptocurrency.
  //
  //type PaymentMethod = ???
  sealed trait PaymentMethod
  case object CreditCard extends PaymentMethod
  case object BankAccount extends PaymentMethod
  case object CryptoCurrency extends PaymentMethod

  //
  // EXERCISE 12
  //
  // Create either a sum type or a product type (as appropriate) to represent an
  // employee at a company, which has a title, salary, name, and employment date.
  //
  //type Employee = ???
  case class Employee(title: String, salary: Int, name: String, employmentDate: String)

  //
  // EXERCISE 13
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // piece on a chess board, which could be a pawn, rook, bishop, knight,
  // queen, or king.
  //
  //type ChessPiece = ???
  sealed trait ChessPiece
  case object Pawn extends ChessPiece
  case object Rook extends ChessPiece
  case object Bishop extends ChessPiece
  case object Knight extends ChessPiece
  case object Queen extends ChessPiece
  case object King extends ChessPiece

  //
  // EXERCISE 14
  //
  // Create an ADT model of a game world, including a map, a player, non-player
  // characters, different classes of items, and character stats.
  //
  //type GameWorld = ???
  case class GameWorld(map: GameMap)
  //sealed trait GameMap // or could use product type: realms, paths
  //case class Dungeon() extends GameMap
  //case class Plains() extends GameMap
  //case class Highlands() extends GameMap
  //case class GameMap(realms: List[Realm], paths: List[(Int, Int)]) // paths is the main problem, no validation
  case class GameMap(realms: List[Realm], paths: List[RealmId => RealmId])

  type RealmId = ??? // just for the compiler
  //class RealmId private (value: Int)
  //object RealmId {
  //  def apply(id: Int): Option[RealmId] = ???
  //}

  case class Realm(id: RealmId, realmType: RealmType, description: String, inv: List[Item], chars: List[Character])

  sealed trait RealmType
  case object Plains extends RealmType
  case object Highlands extends RealmType
  case object Caves extends RealmType
  case object Indoors extends RealmType
  case object Underwater extends RealmType

  sealed trait Item

  case class Character(inv: List[Item], charType: CharType)

  sealed trait CharType
  case class Player(inv: List[Item]) extends CharType
  case class NonPlayerCharacter(npcType: NPCType, inv: List[Item]) extends CharType

  sealed trait NPCType
  case object Ogre extends NPCType
  case object Troll extends NPCType
  case object Wizard extends NPCType
}

object functions {
  type ??? = Nothing


  //
  // EXERCISE 1
  //
  // Convert the following non-function into a function.
  //
  def parseInt1(s: String): Int = s.toInt // throws an exception
  // Can return Try, or an Option (wrap in Try and call .toOption)
  def parseInt2(s: String): Option[Int] = scala.util.Try { s.toInt }.toOption

  //
  // EXERCISE 2
  //
  // Convert the following non-function into a function.
  //
  def arrayUpdate1[A](arr: Array[A], i: Int, f: A => A): Unit =
    arr.update(i, f(arr(i)))
  // has a side effect, we should be able to replace it with () ...
  // also partial function (can throw an exception bu choosing a bad index
  // could return new array (problem 1) and return an option or don't change the array (problem 2)
  def arrayUpdate2[A](arr: Array[A], i: Int, f: A => A): Option[Array[A]] = ???

  //
  // EXERCISE 3
  //
  // Convert the following non-function into a function.
  //
  def divide1(a: Int, b: Int): Int = a / b // cannot divide by zero
  def divide2(a: Int, b: Int): Option[Int] = scala.util.Try { a / b }.toOption

  //
  // EXERCISE 4
  //
  // Convert the following non-function into a function.
  //
  var id = 0
  def freshId1(): Int = { // not deterministic
    val newId = id
    id += 1
    newId
  }
  // pass the current one in, return both old and fresh
  def freshId2(oldId: Int): (Int, Int) = (oldId + 1, oldId)

  //
  // EXERCISE 5
  //
  // Convert the following non-function into a function.
  //
  import java.time.LocalDateTime
  def afterOneHour1: LocalDateTime = LocalDateTime.now.plusHours(1) // not deterministic
  // pass now into the function => also better to test
  def afterOneHour2(now: LocalDateTime): LocalDateTime = now.plusHours(1)

  //
  // EXERCISE 6
  //
  // Convert the following non-function into function.
  //
  def head1[A](as: List[A]): A = { // might throw an exception => partial, but also side effect
    if (as.length == 0) println("Oh no, it's impossible!!!")
    as.head
  }
  def head2[A](as: List[A]): Either[String, A] = as match {
    case Nil => Left("Oh no, it's impossible!!!")
    case a :: _ => Right(a)
  }

  //
  // EXERCISE 7
  //
  // Convert the following non-function into a function.
  //
  trait Account
  trait Processor {
    def charge(account: Account, amount: Double): Unit
  }
  case class Coffee() {
    val price = 3.14
  }
  def buyCoffee1(processor: Processor, account: Account): Coffee = {
    val coffee = Coffee()
    processor.charge(account, coffee.price) // Side-effect here
    coffee
  }
  final case class Charge(account: Account, amount: Double) // seen as an event
  def buyCoffee2(account: Account): (Coffee, Charge) = {
    val coffee = Coffee()
    (coffee, Charge(account, coffee.price)) // like returning an entry to the ledger
  }

  //
  // EXERCISE 8
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def printLine(line: String): Unit = () // only way to implement this function

  //
  // EXERCISE 9
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def readLine: String = ???
  val readLine2: String = "john" // only way, doesn't change meaning of the program

  //
  // EXERCISE 10
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def systemExit(code: Int): Unit = () // again only way, fancy way of getting the unit value

  //
  // EXERCISE 11
  //
  // Rewrite the following non-function `printer1` into a pure function, which
  // could be used by pure or impure code.
  //
  def printer1(): Unit = {
    println("Welcome to the help page!")
    println("To list commands, type `commands`.")
    println("For help on a command, type `help <command>`")
    println("To exit the help page, type `exit`.")
  }
  def printer2[A](println: String => A, combine: (A, A) => A): A =
    List("Welcome to the help page!",
      "To list commands, type `commands`.",
      "For help on a command, type `help <command>`",
      "To exit the help page, type `exit`.")
        .map(println).reduce(combine)
  //
  // call it but it's not functional: printer2(println, (_, _) => ())
  // or pure functional code:         printer2(putStrLn, _ *> _) // combine IOs

  //
  // EXERCISE 12
  //
  // Create a purely-functional drawing library that is equivalent in
  // expressive power to the following procedural library.
  //
  trait Draw {
    def goLeft(): Unit
    def goRight(): Unit
    def goUp(): Unit
    def goDown(): Unit
    def draw(): Unit
    def finish(): List[List[Boolean]]
  }
  def draw1(size: Int): Draw = new Draw {
    val canvas = Array.fill(size, size)(false)
    var x = 0
    var y = 0

    def goLeft(): Unit = x -= 1
    def goRight(): Unit = x += 1
    def goUp(): Unit = y += 1
    def goDown(): Unit = y -= 1
    def draw(): Unit = {
      def wrap(x: Int): Int =
        if (x < 0) (size - 1) + ((x + 1) % size) else x % size

      val x2 = wrap(x)
      val y2 = wrap(y)

      canvas.updated(x2, canvas(x2).updated(y2, true))
    }
    def finish(): List[List[Boolean]] =
      canvas.map(_.toList).toList
  }

  // This works, but want another way:
//  type Bitmap = Array[Array[Boolean]]
//  type Cursor = (Int, Int)
//  type Operation = (Cursor, Bitmap) => (Cursor, Bitmap)
//  val Draw   : Operation = (c, b) => (c, ???) // see above
//  val GoLeft : Operation = (c, b) => ((c._1 - 1, c._2), b)
//  val GoRight: Operation = (c, b) =>
//  val GoUp   : Operation = (c, b) =>
//  val GoDown : Operation = (c, b) =>
//  def draw2(size: Int, op: Operation): Bitmap =
//    op((0, 0), Array.fill(size, size)(false))

  type Bitmap = Array[Array[Boolean]]
  sealed trait Operation
  case object GoLeft extends Operation
  case object GoRight extends Operation
  case object GoUp extends Operation
  case object GoDown extends Operation
  case object Draw extends Operation
  def draw2(size: Int, ops: List[Operation]): Bitmap = {
    def wrap(x: Int): Int =
      if (x < 0) (size - 1) + ((x + 1) % size) else x % size

    val start = (0, 0)
    val empty: Bitmap = Array.fill(size, size)(false)

    ops.foldLeft((start, empty))((state, op) => {
      val pos = state._1
      val canvas = state._2
      op match {
        case GoLeft  => ((wrap(pos._1 - 1), pos._2)          , canvas)
        case GoRight => ((wrap(pos._1 + 1), pos._2)          , canvas)
        case GoUp    => ((pos._1          , wrap(pos._2 + 1)), canvas)
        case GoDown  => ((pos._1          , wrap(pos._2 - 1)), canvas)
        case Draw    => (pos              , canvas.updated(pos._1, canvas(pos._1).updated(pos._2, true)))
      }
    })._2
  }
}

object higher_order {
  case class Parser[+E, +A]( // sum type: either fail with an error E, or consume part of the input (String) and return the remainder
    run: String => Either[E, (String, A)]) // type makes sure that we don't consume AND produce an error

  def fail[E](e: E): Parser[E, Nothing] = // model all failures with ordinary values => totality
    Parser(input => Left(e))

  def point[A](a: => A): Parser[Nothing, A] =
    Parser(input => Right((input, a)))

  def char[E](e: E): Parser[E, Char] =
    Parser(input =>
      if (input.length == 0) Left(e)
      else Right((input.drop(1), input.charAt(0))))

  //
  // EXERCISE 1
  //
  // Implement the following higher-order function.
  //
  def fanout[A, B, C](f: A => B, g: A => C): A => (B, C) = 
    (a: A) => (f(a), g(a))

  //
  // EXERCISE 2
  //
  // Implement the following higher-order function.
  //
  def cross[A, B, C, D](f: A => B, g: C => D): (A, C) => (B, D) = 
    (a: A, c: C) => (f(a), g(c))

  //
  // EXERCISE 3
  //
  // Implement the following higher-order function.
  //
  def either[A, B, C](f: A => B, g: C => B): Either[A, C] => B = {
    case Left(a) => f(a)
    case Right(c) => g(c)
  }

  //
  // EXERCISE 4
  //
  // Implement the following higher-order function.
  //
  def choice[A, B, C, D](f: A => B, g: C => D): Either[A, C] => Either[B, D] = {
    case Left(a) => Left(f(a))
    case Right(c) => Right(g(c))
  }

  //
  // EXERCISE 5
  //
  // Implement the following higer-order function.
  //
  def compose[A, B, C](f: B => C, g: A => B): A => C = 
    a => f(g(a))

  //
  // EXERCISE 6
  //
  // Implement the following higher-order function.
  //
  def alt[E1, E2, A, B](l: Parser[E1, A], r: Parser[E2, B]):
    Parser[E2, Either[A, B]] = Parser(input => l.run(input) match {
    case Left(_) => r.run(input) match {
      case Left(e2) => Left(e2)
      case Right((input2, b)) => Right((input2, Right(b)))
    }
    case Right((input2, a)) => Right((input2, Left(a)))
  })
}

object poly_functions {
  //
  // EXERCISE 1
  //
  // Create a polymorphic function of two type parameters `A` and `B` called
  // `snd` that returns the second element out of any pair of `A` and `B`.
  //
  object snd {
    def apply[A, B](a: A, b: B): B = b
  }
  // snd(1, "foo") // "foo"

  //
  // EXERCISE 2
  //
  // Create a polymorphic function called `repeat` that can take any
  // function `A => A`, and apply it repeatedly to a starting value
  // `A` the specified number of times.
  //
  object repeat {
    def apply[A](n: Int)(a: A, f: A => A): A =
      if (n <= 0) a
      else apply(n - 1)(f(a), f)
  }
  // repeat[Int](100)(0, _ + 1) // 100
  // repeat[String](10)("", _ + "*") // "**********"

  //
  // EXERCISE 3
  //
  // Count the number of unique implementations of the following method.
  //
  def countExample1[A, B](a: A, b: B): Either[A, B] = ???
  val countExample1Answer = 2 // Left(a), Right(b)

  //
  // EXERCISE 4
  //
  // Count the number of unique implementations of the following method.
  //
  def countExample2[A, B](f: A => B, g: A => B, a: A): B = ???
  val countExample2Answer = 2 // either use f or g

  //
  // EXERCISE 5
  //
  // Implement the function `groupBy`.
  //
  val Data =
    "poweroutage;2018-09-20;level=20" :: Nil
  val By: String => String =
    (data: String) => data.split(";")(1)
  val Reducer: (String, List[String]) => String =
    (date, events) =>
      "On date " +
        date + ", there were " +
        events.length + " power outages"
  val Expected =
    Map("2018-09-20" ->
      "On date 2018-09-20, there were 1 power outages")
  def groupBy1(
    l: List[String],
    by: String => String)( // how do I group them into buckets?
      reducer: (String, List[String]) => String): // how do I summarize the labeled buckets?
      Map[String, String] =
    l.map { str =>
      val b = by(str)
      b -> reducer(b, l)
    }.toMap
  // groupBy1(Data, By)(Reducer) == Expected

  //
  // EXERCISE 6
  //
  // Make the function `groupBy1` as polymorphic as possible and implement
  // the polymorphic function. Compare to the original.
  //
  object groupBy2 { // ways to screw it up has reduced, and we have more flexibility
    def apply[A, B, C](l: List[A], by: A => B)(
      reducer: (B, List[A]) => C):
    Map[B, C] =
      l.map { a =>
      val b = by(a)
      b -> reducer(b, l)
    }.toMap
  }
  // can also abstract away List[A] further (foldable?)
}

object higher_kinded {
  type ?? = Nothing
  type ???[A] = Nothing
  type ????[A, B] = Nothing
  type ?????[F[_]] = Nothing

  // Int    : *               // * = { x | x is a type in the Scala type system }
  //                          //   = { Person, Int, Boolean, String, Tree[Int], Tree[String], Unit, Nothing, ... }
  // List   : * => *          // _[_]
  // Map    : [*, *] => *     // _[_, _]
  // Tuple3 : [*, *, *] => *  // (Int, String, Boolean)
  //
  // def foo[A](???)       // A has kind *
  // def foo[A[_]](???)    // A has kind * -> *
  // def foo[A[_, _]](???) // A has kind [*, *] => *
  //
  // Could also use names: trait Foo[A[X, Y, Z], B, C[X, Y]] but X's and Y's are not relevant, so replace with _
  //
  //def foo[F[_], A](fa: F[A]): F[A] = ??? // F could be Future, List, ...
  case class Foo[F[_], A](fa: F[A]) // Is a type constructor. Kind of foo (figure out iteratively):
  // [SOMETHING, SOMETHING] => *
  // [* => *, SOMETHING] => *
  // [* => *, *] => *

  trait `* => *`[F[_]]
  trait `[*, *] => *`[F[_, _]]
  trait `(* => *) => *`[T[_[_]]]

  //
  // EXERCISE 1
  //
  // Identify a type constructor that takes one type parameter (i.e. has kind
  // `* => *`), and place your answer inside the square brackets.
  //
  type Answer1 = `* => *`[List] // One of many answers

  //
  // EXERCISE 2
  //
  // Identify a type constructor that takes two type parameters (i.e. has kind
  // `[*, *] => *`), and place your answer inside the square brackets.
  //
  type Answer2 = `[*, *] => *`[Either] // Could also use Map

  //
  // EXERCISE 3
  //
  // Create a trait with kind `*`.
  //
  trait Answer3 /*[]*/ // Do nothing, zero params

  //
  // EXERCISE 4
  //
  // Create a trait with kind `[*, *, *] => *`.
  //
  trait Answer4[A, B, C] /*[]*/

  //
  // EXERCISE 5
  //
  // Create a new type that has kind `(* -> *) -> *`.
  //
  type NewType1[F[_]]
  type Answer5 = `(* => *) => *`[NewType1]

  //
  // EXERCISE 6
  //
  // Create a trait with kind `[* => *, (* => *) => *] => *`.
  //
  trait Answer6[A[_], B[_[_]]] /*[]*/ // Could give name C but can't be used, so _ is fine

  //
  // EXERCISE 7
  //
  // Create an implementation of the trait `CollectionLike` for `List`.
  //
  trait CollectionLike[F[_]] {
    def empty[A]: F[A]

    def cons[A](a: A, as: F[A]): F[A]

    def uncons[A](as: F[A]): Option[(A, F[A])]

    final def singleton[A](a: A): F[A] =
      cons(a, empty[A])

    final def append[A](l: F[A], r: F[A]): F[A] =
      uncons(l) match {
        case Some((l, ls)) => append(ls, cons(l, r))
        case None => r
      }

    final def filter[A](fa: F[A])(f: A => Boolean): F[A] =
      bind(fa)(a => if (f(a)) singleton(a) else empty[A])

    final def bind[A, B](fa: F[A])(f: A => F[B]): F[B] =
      uncons(fa) match {
        case Some((a, as)) => append(f(a), bind(as)(f))
        case None => empty[B]
      }

    final def fmap[A, B](fa: F[A])(f: A => B): F[B] = {
      val single: B => F[B] = singleton[B](_)

      bind(fa)(f andThen single)
    }
  }
  val ListCollectionLike: CollectionLike[List] = new CollectionLike[List] {
    override def cons[A](a: A, as: List[A]): List[A] = a :: as

    override def empty[A]: List[A] = List.empty[A]

    override def uncons[A](as: List[A]): Option[(A, List[A])] = as match {
      case Nil => None
      case x :: xs => Some((x, xs))
    }
  }

  //
  // EXERCISE 8
  //
  // Implement `Sized` for `List`.
  //
  trait Sized[F[_]] { // Size accepts a single type parameter, and its Kind is * -> *
    // This method will return the number of `A`s inside `fa`.
    def size[A](fa: F[A]): Int
    //
    //val nonPolymorphicMethod: F[Unit] // non poly is also possible, not everything needs to be poly
  }
  val ListSized: Sized[List] = new Sized[List] {
    def size[A](fa: List[A]): Int = fa.length // this is a polymorphic method
  }
  ListSized.size(1 :: 2 :: Nil) // but also works for String

  //
  // EXERCISE 9
  //
  // Implement `Sized` for `Map`, partially applied with its first type
  // parameter to `String`.
  //
  // it is partially applied type constructor, since kind of Map is [*, *] => *
  // this only works with a compiler plugin for now (kind-projector), or use type lambdas
  // in Scala 3 we will be able to use _
  val MapSized1: Sized[Map[String, ?]] = new Sized[Map[String, ?]] {
    def size[A](fa: Map[String, A]): Int = fa.size
  }
  type MapString[V] = Map[String, V] // has kind * -> *
  MapSized1.size(Map("foo" -> 1, "bar" -> 2))

  //
  // EXERCISE 9
  //
  // Implement `Sized` for `Map`, partially applied with its first type
  // parameter to a user-defined type parameter.
  //
  // Polymorphic key type - method type polymorphism combined with data type polymorphism
  // almost cheating: seems like we have created a Sized for Map,
  // but due to polymorphism it is a family of Sizes for all choices of K
  def MapSized2[K]: Sized[Map[K, ?]] = new Sized[Map[K, ?]] {
    def size[A](fa: Map[K, A]): Int = fa.size
  }
  MapSized2[String].size[Int](Map[String, Int]("foo" -> 1))

  //
  // EXERCISE 10
  //
  // Implement `Sized` for `Tuple3`.
  //
  def Tuple3Sized[A, B]: Sized[(A, B, ?)] = new Sized[(A, B, ?)] {
    def size[C](fa: (A, B, C)): Int = 1 // Only one C in the C slot
  }
}

// Motivation for type classes: sorting
object example {
  // Motivation: sorting
  def sort(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case x :: xs =>
      val (lessThan, notLessThan) = xs.partition(_ < x)
      sort(lessThan) ++ List(x) ++ sort(notLessThan)
  }
  // had too many playing cards (e.g. List[Int]), all sorts of things can go wrong (can summon Ints out of thin air)
  // then introduce polymorphism to throw away the “Int” information, but thrown away too much info (can’t sort)
  //def sort[A](l: List[A]): List[A] = l match {
  //  case Nil => Nil
  //  case x :: xs =>
  //    val (lessThan, notLessThan) = xs.partition(_ < x) // doesn't compile
  //    sort(lessThan) ++ List(x) ++ sort(notLessThan)
  //}
  // re-introduce structure, but you need guarantees (transitivity, anti-reflexive)
  def sortAll1[A](l: List[A])(lt: (A, A) => Boolean): List[A] = l match {
    case Nil => Nil
    case x :: xs =>
      val (lessThan, notLessThan) = xs.partition(lt(_, x))
      sortAll1(lessThan)(lt) ++ List(x) ++ sortAll1(notLessThan)(lt)
  }
  // bundle capability and guarantees in trait
  // pass it around as implicit
  // then use context bounds
  // then define apply method to get rid of implicitly
  // then create implicit Syntax class to re-introduce < syntax, and others in terms of <

  /**
    * Every type class is a set of three things:
    *
    * - Types
    * - Operations on values of those types
    * - Laws governing the behaviour of the operations
    *
    * Every type class instance (or instance, for short) is an implementation of the type class for a set of given types
    *
    */
  trait LessThan[A] {
    // `lessThan` must satisfy the transitivity law
    // `lessThan(a, b) && lessThan(b, c) ==> lessThan(a, c)`
    def lessThan(left: A, right: A): Boolean

    final def notLessThan(left: A, right: A): Boolean =
      !lessThan(left, right)
  }
  object LessThan {
    def apply[A](implicit A: LessThan[A]): LessThan[A] = A

    // Put instance in the companion object if you created the type class
    implicit val LessThanInt: LessThan[Int] = (left: Int, right: Int) => left < right
  }
  implicit class LessThanSyntax[A](val l: A) {
    def < (r: A)(implicit A: LessThan[A]): Boolean = A.lessThan(l, r)

    def >= (r: A)(implicit A: LessThan[A]): Boolean = A.notLessThan(l, r)
  }

  def sortAll[A: LessThan](l: List[A]): List[A] = l match {
    case Nil => Nil
    case x :: xs =>
      val (lessThan, notLessThan) = xs.partition(_ < x)
      sortAll(lessThan) ++ List(x) ++ sortAll(notLessThan)
  }

  sortAll(1 :: 3 :: -1 :: 10 :: 45 :: 7 :: 2 :: Nil)

  case class Person(name: String, age: Int)
  object Person {
    // Put your own one in your companion object if you need to provide your own instance
    implicit val PersonLessThan: LessThan[Person] = (left: Person, right: Person) =>
      if (left.name < right.name) true
      else if (left.age < right.age) true
      else false
  }

  sortAll(Person("John", 40) :: Nil)

  case class DescendingPerson(person: Person)
  object DescendingPerson {
    implicit val DescendingPersonLessThan: LessThan[DescendingPerson] = (left: DescendingPerson, right: DescendingPerson) =>
      left.person >= right.person // not sure
  }
}

object typeclasses {
  /**
   * {{
   * Reflexivity:   a ==> equals(a, a)
   *
   * Transitivity:  equals(a, b) && equals(b, c) ==>
   *                equals(a, c)
   *
   * Symmetry:      equals(a, b) ==> equals(b, a)
   * }}
   */
  trait Eq[A] {
    def equals(l: A, r: A): Boolean
  }

//  // Remove duplicates from a list
//  def nub[A](list: List[A])(implicit eq: Eq[A]): List[A] = {
//    def contains(a1: A, l: List[A]): Boolean =
//      l.foldLeft(false)((b, a2) => b || eq.equals(a1, a2))
//
//    list.foldLeft[List[A]](Nil) {
//      case (acc, a) =>
//        if (contains(a, acc)) acc
//        else a :: acc
//    }
//  }
//  implicit val IntEq: Eq[Int] = new Eq[Int] {
//    def equals(l: Int, r: Int): Boolean = l == r
//  }
//  implicit def ListEq[A](implicit eq0: Eq[A]): Eq[List[A]] = new Eq[List[A]] {
//    def equals(l: List[A], r: List[A]): Boolean = (l, r) match {
//      case (Nil, Nil) => true
//      case (Nil, _) => false
//      case (_, Nil) => false
//      case (l :: ls, r :: rs) =>
//        eq0.equals(l, r) && equals(ls, rs)
//    }
//  }
//  nub(1 :: 2 :: 3 :: Nil)
//  nub(List(1, 2) :: List(1, 2) :: List(9, 2) :: Nil)

  object Eq {
    def apply[A](implicit eq: Eq[A]): Eq[A] = eq

    implicit val EqInt: Eq[Int] = new Eq[Int] {
      def equals(l: Int, r: Int): Boolean = l == r
    }
    implicit def EqList[A: Eq]: Eq[List[A]] =
      new Eq[List[A]] {
        def equals(l: List[A], r: List[A]): Boolean =
          (l, r) match {
            case (Nil, Nil) => true
            case (Nil, _) => false
            case (_, Nil) => false
            case (l :: ls, r :: rs) =>
              Eq[A].equals(l, r) && equals(ls, rs)
          }
      }
  }
  implicit class EqSyntax[A](val l: A) extends AnyVal {
    def === (r: A)(implicit eq: Eq[A]): Boolean =
      eq.equals(l, r)
  }

  //
  // Scalaz 7 Encoding
  //
  sealed trait Ordering
  case object EQUAL extends Ordering
  case object LT extends Ordering
  case object GT extends Ordering
  object Ordering {
    implicit val OrderingEq: Eq[Ordering] = new Eq[Ordering] {
      def equals(l: Ordering, r: Ordering): Boolean =
        (l, r) match {
          case (EQUAL, EQUAL) => true
          case (LT, LT) => true
          case (GT, GT) => true
          case _ => false
        }
    }
  }

  trait Ord[A] {
    def compare(l: A, r: A): Ordering
  }
  object Ord {
    def apply[A](implicit A: Ord[A]): Ord[A] = A

    implicit val OrdInt: Ord[Int] = new Ord[Int] {
      def compare(l: Int, r: Int): Ordering =
        if (l < r) LT else if (l > r) GT else EQUAL
    }
  }
  implicit class OrdSyntax[A](val l: A) extends AnyVal {
    def =?= (r: A)(implicit A: Ord[A]): Ordering =
      A.compare(l, r)

    def < (r: A)(implicit A: Ord[A]): Boolean =
      Eq[Ordering].equals(A.compare(l, r), LT)

    def <= (r: A)(implicit A: Ord[A]): Boolean =
      (l < r) || (this === r)

    def > (r: A)(implicit A: Ord[A]): Boolean =
      Eq[Ordering].equals(A.compare(l, r), GT)

    def >= (r: A)(implicit A: Ord[A]): Boolean =
      (l > r) || (this === r)

    def === (r: A)(implicit A: Ord[A]): Boolean =
      Eq[Ordering].equals(A.compare(l, r), EQUAL)

    def !== (r: A)(implicit A: Ord[A]): Boolean =
      !Eq[Ordering].equals(A.compare(l, r), EQUAL)
  }
  case class Person(age: Int, name: String)
  object Person {
    implicit val OrdPerson: Ord[Person] = new Ord[Person] {
      def compare(l: Person, r: Person): Ordering =
        if (l.age < r.age) LT else if (l.age > r.age) GT
        else if (l.name < r.name) LT else if (l.name > r.name) GT
        else EQUAL
    }
    implicit val EqPerson: Eq[Person] = new Eq[Person] {
      def equals(l: Person, r: Person): Boolean =
        l == r
    }
  }

  //
  // EXERCISE 1
  //
  // Write a version of `sort1` called `sort2` that uses the polymorphic `List`
  // type constructor, and which uses the `Ord` type class, including the
  // compare syntax operator `=?=` to compare elements.
  //
  def sort1(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case x :: xs =>
      val (lessThan, notLessThan) = xs.partition(_ < x)

      sort1(lessThan) ++ List(x) ++ sort1(notLessThan)
  }
  def sort2[A: Ord](l: List[A]): List[A] = l match {
    case Nil => Nil
    case x :: xs =>
      //val (lessThan, notLessThan) = xs.partition((a: A) =>  (a =?= x) == LT) // If we had Eq with implicit sugar, could use ===
      val (lessThan, notLessThan) = xs.partition(_ < x) // Possible because of OrdSyntax; looks the same as Int version above
      sort2(lessThan) ++ List(x) ++ sort2(notLessThan)
  }

  //
  // EXERCISE 2
  //
  // Create an instance of `Ord` for the type `String`.
  //
  implicit val OrdString: Ord[String] = new Ord[String] {
    override def compare(l: String, r: String): Ordering =
      if (l < r) LT else if (l > r) GT else EQUAL
  }


  //
  // Scalaz 8 Encoding
  //
  sealed abstract class InstanceOfModule {
    type InstanceOf[T] <: T
    def instanceOf[T](t: T): InstanceOf[T]
  }

  object InstanceOfModule {
    val impl: InstanceOfModule = new InstanceOfModule {
      override type InstanceOf[T] = T
      override def instanceOf[T](t: T) = t
    }
  }
  import InstanceOfModule.impl._

  type ???[A] = Nothing

  /**
   * {{
   * // Associativity:
   * (a <> b) <> c === a <> (b <> c)
   * }}
   */
  trait SemigroupClass[A] {
    def append(l: => A, r: => A): A
  }
  type Semigroup[A] = InstanceOf[SemigroupClass[A]]
  object SemigroupClass {
    def apply[A](implicit A: Semigroup[A]): Semigroup[A] = A

    implicit val SemigroupString: Semigroup[String] =
      instanceOf(
        new SemigroupClass[String] {
          def append(l: => String, r: => String): String = l + r
        })
    implicit def SemigroupList[A]: Semigroup[List[A]] =
      instanceOf(
        new SemigroupClass[List[A]] {
          def append(l: => List[A], r: => List[A]): List[A] = l ++ r
        })
  }
  implicit def AnyToSemigroupSyntax[A](a: => A): SemigroupSyntax[A] =
    new SemigroupSyntax(() => a)
  class SemigroupSyntax[A](l: () => A) {
    def <> (r: => A)(implicit A: Semigroup[A]): A = A.append(l(), r)
  }
  //
  // EXERCISE 3
  //
  // Create an instance of the `Semigroup` type class for `java.time.Duration`.
  //
  implicit val SemigroupDuration: Semigroup[java.time.Duration] = instanceOf(
    new SemigroupClass[java.time.Duration] {
      override def append(l: => java.time.Duration, r: => java.time.Duration): java.time.Duration = l.plus(r)
    }
  )

  //
  // EXERCISE 4
  //
  // Create an instance of the `Semigroup` type class for `Int`.
  //
  implicit val SemigroupInt: Semigroup[Int] = instanceOf(
    new SemigroupClass[Int] {
      override def append(l: => Int, r: => Int): Int = l + r
    }
  )

  //
  // EXERCISE 5
  //
  // Create an instance of the `Semigroup` type class for `Set[A]`.
  //
  implicit def SemigroupSet[A]: Semigroup[Set[A]] = instanceOf(
    new SemigroupClass[Set[A]] {
      override def append(l: => Set[A], r: => Set[A]): Set[A] = l ++ r
    }
  )

  //
  // EXERCISE 6
  //
  // Create an instance of the `Semigroup` type class for `Map[K, ?]`. Hint:
  // you will need some constraint applied to the values.
  //
  implicit def SemigroupMap[K, V: ???]: Semigroup[Map[K, V]] =
    ???

  //
  // EXERCISE 7
  //
  // Create a type class `Monoid[A]` that implies `Semigroup[A]` (that is, every
  // `Monoid[A]` must be a `Semigroup[A]`), which adds a single operation called
  // `zero`, which satisfies additional laws.
  //
  /**
   * {{
   * append(zero, a) === a
   * append(a, zero) === a
   * }}
   */
  trait MonoidClass[A] extends SemigroupClass[A] {
    /* ??? */
  }
  object MonoidClass {
    def apply[A](implicit A: Monoid[A]): Monoid[A] = ???
  }
  type Monoid[A] = InstanceOf[MonoidClass[A]]
  implicit def MonoidSemigroup[A](implicit M: Monoid[A]): Semigroup[A] =
    instanceOf(M)
  def empty[A: Monoid]: A = ???

  //
  // EXERCISE 8
  //
  // Create an instance of the `Monoid` type class for `java.time.Duration`.
  //
  implicit val MonoidInstant: Monoid[java.time.Duration] = ???

  //
  // EXERCISE 9
  //
  // Create an instance of the `Monoid` type class for `String`.
  //
  implicit val MonoidString: Monoid[String] = ???

  //
  // EXERCISE 10
  //
  // Create an instance of the `Monoid` type class for `List[A]`.
  //
  implicit def MonoidList[A]: Monoid[List[A]] = ???

  //
  // EXERCISE 11
  //
  // Create an instance of the `Monoid` type class for `Int`.
  //
  implicit val MonoidInt: Monoid[Int] = ???

  //
  // EXERCISE 12
  //
  // Using a newtype, create an instance of the `Monoid` type class for `Int`
  // representing the additive monoid, with addition as `append`, and 0 as
  // `zero`.
  //
  final case class Sum(run: Int)
  implicit val MonoidSum: Monoid[Sum] = ???

  //
  // EXERCISE 13
  //
  // Using a newtype, create an instance of the `Monoid` type class for `Int`
  // representing the multiplicative monoid, with multiplication as `append`,
  // and 1 as `zero`.
  //
  final case class Product(run: Int)
  implicit val MonoidProduct: Monoid[Product] = ???

  //
  // EXERCISE 14
  //
  // Create an instance of the `Collection` type class for `List`.
  //
  trait CollectionClass[F[_]] {
    def empty[A]: F[A]
    def cons[A](a: A, as: F[A]): F[A]
    def uncons[A](fa: F[A]): Option[(A, F[A])]
  }
  object CollectionClass {
    def apply[F[_]](implicit F: Collection[F]): Collection[F] = F
  }
  type Collection[F[_]] = InstanceOf[CollectionClass[F]]
  implicit val ListCollection: Collection[List] = ???
}

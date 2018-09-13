// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.abstractions

import scalaz._
import Scalaz._

object algebra {

  //
  // (A, A) => A // concatenation, reduce or foldLeft
  //
  ///**
  //  * Associativity:
  //  * x |+| (y |+| z) === (x |+| y) |+| z      // |+| is append
  //  */
  //trait Semigroup[A] {               // A of kind '*'
  //  def append(l: => A, r: => A): A  // e.g. using laziness
  //}
  //implicit class SemigroupSyntax[A](l: A) {
  //  def |+| (r: A)(implicit S: Semigroup[A]): A = S.append(l, r)
  //}
  //def append(l: => Int, r: => Int): Int = ??? // different semigroups for ints: could pick l, r, min, max, ...
  //// Newtyping
  //case class Sum(value: Int)
  //implicit val SumSemigroup: Semigroup[Sum] = new Semigroup[Sum] {
  //  override def append(l: => Sum, r: => Sum): Sum = Sum(l.value + r.value)
  //}
  //Sum(1) |+| Sum(2)

  //
  // EXERCISE 1
  //
  // Define a semigroup for `NotEmpty` below.
  //
  case class NotEmpty[+A](head: A, tail: Option[NotEmpty[A]])
  implicit def NotEmptySemigroup[A]: Semigroup[NotEmpty[A]] =
    new Semigroup[NotEmpty[A]] {
      override def append(l: NotEmpty[A], r: => NotEmpty[A]): NotEmpty[A] = // second arg is lazy, first should be too
        new NotEmpty[A](l.head, l.tail match {
          case None => Some(r)
          case Some(l) => Some(append(l, r)) // recurse
        })
    }
  val example1 = NotEmpty(1, None) |+| NotEmpty(2, None)

  ///**
  //  * append(mzero, a) === a
  //  * append(a, mzero) === a
  //  */
  //trait Monoid[A] {
  //  def mzero: A
  //}
  //trait Monoid[F] extends Semigroup[F] { self =>


  //
  // EXERCISE 2
  //
  // Design a permission system for securing some resource, together with a
  // monoid for the permission data structure.
  //
  // accountId: String => no semigroup possible (combining two account IDs doesn't make sense)
  // emails: List[String]
  sealed trait Capability
  case object Read extends Capability
  case object Write extends Capability
  type Email = String
  type Resource = String
  case class Permission(value: Map[Email, Map[Resource, Set[Capability]]])
  object Permission {
    def apply() = new Permission(Map())
  }
  //case class Permission(/***/)
  // mzero = empty map
  // append = merge maps, monoids already in scope from scalaz
  implicit val MonoidPermission: Monoid[Permission] = new Monoid[Permission] {
    override def zero: Permission = Permission()

    override def append(l: Permission, r: => Permission): Permission =
      Permission(l.value |+| r.value)
  }
  val example2 = mzero[Permission] |+| Permission()

  //
  // EXERCISE 3
  //
  // Define an instance of `Semigroup` for `(A, B)` when both `A` and
  // `B` form semigroups.
  //
  implicit def SemigroupTuple2[A: Semigroup, B: Semigroup]: // family of semigroup instances (poly), context bounds
    Semigroup[(A, B)] = new Semigroup[(A, B)] {
    override def append(l: (A, B), r: => (A, B)): (A, B) =
      (l._1 |+| r._1, l._2 |+| r._2)
  }
  //("foo", "bar") |+| ("baz", "boo") // ("foobaz", "barboo")
  //(3, "bar")     |+| (4, "boo")     // (7, "barboo") - depending on which Int semigroup picked
  // Monoids from boolean: AND (mzero = true), OR (mzero = false)
  //(3, false)     |+| (4, true)     // (7, false) - depending on which semigroups chosen

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Monoid` for `NotEmpty` for any type `A`.
  //
  implicit def MonoidNotEmpty[A]: Monoid[NotEmpty[A]] = ??? // can't exist!!!

  // All append ops:
  // mappend(1, mappend(2, mappend(3, 4)))
  // 1  ⊹  2  ⊹ 3   ⊹  4                     // unicode char
  // 1 |+| 2 |+| 3 |+| 4                     // the one we used
}

object functor {
  // String can't be a functor - wrong kind *
  // Can be: Option, Vector - kind * -> * (Set is not a functor, see later)
  // Map can't be by parameterizing over the key type

  /**
    * Identity:    fmap(identity) === identity
    * Composition: fmap(f.compose(g)) === fmap(f).compose(fmap(g))
    */
  //trait Functor[F[_]] {
  //  def map[A, B](fa: F[A])(f: A => B): F[B]
  //  // Equivalent but nicer and easier to understand (implemented in terms of map):
  //  def fmap[A, B](f: A => B): F[A] => F[B] = (fa: F[A]) => map(fa)(f)
  //}
  // Map example by type lambda:
  //implicit def FunctorMap[K]: Functor[Map[K, ?]] = new Functor[Map[K, ?]] {
  //
  //}
  // Composition law:
  //val g: Int => String = _.toString
  //val f: String => Int = _.length
  //val `f compose g`: Int => Int = f.compose(g)
  //val actual = List(1, 19, 200, 42, 38)
  //val expect = List(1, 2, 3, 2, 2) // count the number of digits
  //actual.map(g).map(f)       // map ints to strings, then length of strings; actual.map(f.compose(g))
  //fmap(f)(fmap(g)(actual))   // fmap(f.compose(g))(actual)
  // Identity law:
  //actual.map(identity)   // actual
  //fmap(identity)(actual) // actual
  //
  // Implementing functor for list:
  //implicit val FunctorList = new Functor[List] {
  //  override def map[A, B](fa: List[A])(f: A => B): List[B] =
  //    fa match {
  //      case Nil => Nil
  //      case a :: as => f(a) :: map(as)(f) // recurse
  //    }
  //} // satisfies both laws

  // Functor examples:
  implicit val OptionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa match {
        case None => None
        case Some(a) => Some(f(a))
      }
  }
  implicit val VectorFunctor: Functor[Vector] = new Functor[Vector] {
    override def map[A, B](fa: Vector[A])(f: A => B): Vector[B] =
      fa.map(f)
  }
  implicit val ListFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] =
      fa  match {
        case Nil => Nil
        case a :: as => f(a) :: map(as)(f)
      }
  }
  // Only possible functor for map
  implicit def MapFunctor[K]: Functor[Map[K, ?]] = new Functor[Map[K, ?]] { // partially apply type parameter
    override def map[A, B](fa: Map[K, A])(f: A => B): Map[K, B] =
      fa.mapValues(f)
  } // keys doesn't make sense as they are sets

  //
  // EXERCISE 1
  //
  // Define an instance of `Functor` for `BTree`.
  //
  sealed trait BTree[+A] // kind * -> *, so could implement functor
  case class Leaf[A](a: A) extends BTree[A]
  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]
  implicit val BTreeFunctor: Functor[BTree] =
    new Functor[BTree] {
      def map[A, B](fa: BTree[A])(f: A => B): BTree[B] =
        fa match {
          case Leaf(a) => Leaf(f(a))
          case Fork(l, r) => Fork(map(l)(f), map(r)(f))
        }
    }

  //
  // EXERCISE 2
  //
  // Define an instance of `Functor` for `Nothing`.
  //
  // is poly-kinded (*, * -> *, ...)
  implicit val NothingFunctor: Functor[Nothing] = new Functor[Nothing] {
    override def map[A, B](fa: Nothing)(f: A => B): Nothing = fa
  } // pass along the nothing, but no-one will be able to call the map as no-one can provide a nothing
  //
  // It's still useful, e.g. Either[Nothing, A] // still want to print it, equality, ...

  //
  // EXERCISE 3
  //
  // Define an instance of `Functor` for Parser[E, ?].
  //
  case class Parser[+E, +A](run: String => Either[E, (String, A)])
  implicit def ParserFunctor[E]: Functor[Parser[E, ?]] = new Functor[Parser[E, ?]] {
    override def map[A, B](fa: Parser[E, A])(f: A => B): Parser[E, B] =
      Parser(input => fa.run(input) match {
        case Left(e) => Left(e): Either[E, (String, B)]
        case Right((input, a)) => Right((input, f(a))): Either[E, (String, B)]
      })
  }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Functor` for the following data type.
  //
  case class DataType[A](f: A => A)
  implicit val DataTypeFunctor: Functor[DataType] = new Functor[DataType] {
    override def map[A, B](fa: DataType[A])(f: A => B): DataType[B] = ??? // impossible
      // DataType(f.compose(fa.f): A => B) // impossible, played all cards
  }
  // general rule: if type parameter occurs in covariant position (output), just the output, then there is a possibility you can write a functor for it
  // general rule: if type parameter occurs in contravariant position (input), you can definitely not write a functor for it

  //
  // EXERCISE 5
  //
  // Define an instance of `Functor` for `FunctorProduct`.
  //
  case class FunctorProduct[F[_], G[_], A](l: F[A], r: G[A])
  implicit def FunctorProductFunctor[F[_]: Functor, G[_]: Functor]:
    Functor[FunctorProduct[F, G, ?]] = new Functor[FunctorProduct[F, G, ?]] {
      override def map[A, B](fa: FunctorProduct[F, G, A])(f: A => B): FunctorProduct[F, G, B] =
        new FunctorProduct[F, G, B](fa.l.map(f), fa.r.map(f))
  }

  //
  // EXERCISE 6
  //
  // Define an instance of `Functor` for `FunctorSum`.
  //
  case class FunctorSum[F[_], G[_], A](run: Either[F[A], G[A]])
  implicit def FunctorSumFunctor[F[_]: Functor, G[_]: Functor]:
    Functor[FunctorSum[F, G, ?]] = new Functor[FunctorSum[F, G, ?]] {
      override def map[A, B](fa: FunctorSum[F, G, A])(f: A => B): FunctorSum[F, G, B] =
        new FunctorSum[F, G, B](
          fa.run match {
            case Left(fa) => Left(fa.map(f))
            case Right(ga) => Right(ga.map(f))
          }
        )
  }

  //
  // EXERCISE 7
  //
  // Define an instance of `Functor` for `FunctorNest`.
  //
  case class FunctorNest[F[_], G[_], A](run: F[G[A]])
  implicit def FunctorNestFunctor[F[_]: Functor, G[_]: Functor]:
    Functor[FunctorNest[F, G, ?]] = new Functor[FunctorNest[F, G, ?]] {
      override def map[A, B](fa: FunctorNest[F, G, A])(f: A => B): FunctorNest[F, G, B] =
        new FunctorNest[F, G, B](fa.run.map(_.map(f)))
  }

  /* Bank example

  case class USD(amount: BigDecimal)

  case class Amount[A](value: A)
//  Amount[USD](12.22).map()
  case class Account[Id](id: Id)
  case class Client[F[_], A](accounts: F[Account[A]])
  sealed trait Operation[A] {
    def map[B](f: A => B): Operation[B] = map(this, f)
    def zip[B](that: Operation[B]): Operation[(A, B)] = Both(this, that)
  }
  case class Deposit[Id, A](to: Account[Id], amount: Amount[A]) extends Operation[Amount[A]]
  case class Withdraw[Id, A](from: Account[Id], amount: Amount[A]) extends Operation[Amount[A]]
  case class Both[A,B](l: Operation[A], r: Operation[B]) extends Operation[(A,B)]
  case class Map[A,B](op:Operation[A], f: A => B) extends Operation[B]

  val acc = Account(1)
  (Deposit(acc, Amount(12.22)) zip (Withdraw(acc, Amount(12.22)))).map {
    case (deposit, withdraw) => deposit.value - withdraw.value
  }
  def commit[A](op: Operation[A]):A = ???
   */

  trait Apply[F[_]] extends Functor[F] {
    def zip[A, B](l: F[A], r: F[B]): F[(A, B)]
  }
  //(F[A], F[B]) => F[(A, B)]
  //type *[A, B] = (A, B)
  //F[A] * F[B] => F[A * B]
  implicit class ApplySyntax[F[_], A](l: F[A]) {
    // These ensure that both operations succeed, but we're only need one of the results
    // (e.g. two Futures making calls etc)
    def *> [B](r: F[B])(implicit F: Apply[F]): F[B] = // right fish, right sequence
      F.zip(l, r).map(_._2) // NOT equal to r

    def <* [B](r: F[B])(implicit F: Apply[F]): F[A] = // left fish, left sequence
      F.zip(l, r).map(_._1) // NOT equal to l
  }

  def zipOption[A, B](l: Option[A], r: Option[B]): Option[(A, B)] =
    (l, r) match {
      case (Some(a), Some(b)) => Some((a, b))
      case _ => None
    }

  def zipWith[A, B, C](l: Option[A], r: Option[B])(f: ((A, B)) => C): Option[C] =
    zipOption(l, r).map(f)

  def zipList1[A, B](l: List[A], r: List[B]): List[(A, B)] =
    (l, r) match {
      case (a :: as, bs) =>
        zipList1(as, bs) ++ bs.map(b => (a, b))
      case (Nil, bs) => Nil
    }
  def zipList2[A, B](l: List[A], r: List[B]): List[(A, B)] =
    (l, r) match {
      case (a :: as, b :: bs) => ((a, b)) :: zipList2(as, bs)
      case _ => Nil
    }

  val l = List(1, 2, 3)
  val r = List(9, 2)
  // two ways to zip two lists:
  val lr1 = List((1, 9), (1, 2), (2, 9), (2, 2), (3, 9), (3, 2)) // cartesian x-product
  val lr2 = List((1, 9), (2, 2))                                 // align index wise
  val lr1_mapped1 = lr1.map(_._1) // (l <* r) === List(1, 1, 2, 2, 3, 3) // we see it's not equals to l
  val lr1_mapped2 = lr1.map(_._2) // (l *> r) === List(9, 2, 9, 2, 9, 2) // we see it's not equals to r
  val lr2_mapped1 = lr2.map(_._1) // (l <* r) === List(1, 2)             // we see it's not equals to l
  val lr2_mapped2 = lr2.map(_._2) // (l *> r) === List(9, 2)             // we see it's not equals to r
  // for both definitions of zip

  implicit val ApplyOption: Apply[Option] = new Apply[Option] {
    //def ap[A, B](fa: => Option[A])(f: => Option[A => B]): Option[B] = // can be implemented in terms of zip
    //  zip(fa, f).map {
    //    case (a, ab) => ab(a)
    //  }

    // About ap / zip I think ap is the traditionnal operation for apply, except it is harder to grasp than zip but one can be written thanks to the other, I guess thats why we saw zip
    // zip is more general: combine two Fs into 1 (2 Options, Parsers, ...), then you can decide what to do with the combination (map)
    // ap/ap2 is about applying a function within an F; can be implemented in terms of zip

    override def zip[A, B](l: Option[A], r: Option[B]): Option[(A, B)] =
        (l, r) match {
          case (Some(a), Some(b)) => Some((a, b))
          case _ => None
        }

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = ???
  }

  //trait Applicative[F[_]] extends Apply[F] {
  //  // DOESN'T HOLD fa <* fb !== fa
  //  // DOESN'T HOLD fa *> fb !== fb
  //  //
  //  // fa <* point(b) === fa
  //  // point(b) *> fa === fa
  //  //
  //  def point[A](a: => A): F[A]
  //}

  //
  // EXERCISE 8
  //
  // Define `Applicative` for `Option`.
  //
  implicit val OptionApplicative: Applicative[Option] =
    new Applicative[Option] {
      def point[A](a: => A): Option[A] = Some(a)

      def ap[A, B](fa: => Option[A])(f: => Option[A => B]): Option[B] =
        (fa, f) match {
          case (Some(a), Some(f)) => point(f(a)) // or Some
          case _                  => None
        }
    }

  //
  // EXERCISE 9
  //
  // Implement `zip` in terms of the applicative composition using `|@|`.
  //
  // Bonus: Implement `ap2` in terms of `zip`.
  //
  val example1 = (Option(3) |@| Option(5))((_, _))  // ((a, b) => (a, b) takes two things and returns a tuple of two things
  val example2 = zip(Option(3), Option("foo")) : Option[(Int, String)]
  def zip[F[_]: Applicative, A, B](l: F[A], r: F[B]): F[(A, B)] =
    (l |@| r)((_, _))
  def ap2[F[_]: Applicative, A, B](fa: F[A], fab: F[A => B]): F[B] =
    zip(fa, fab).map { case (a, f) => f(a) }

  //
  // EXERCISE 10
  //
  // Define an instance of `Applicative` for `Parser[E, ?]`.
  //
  implicit def ApplicativeParser[E]: Applicative[Parser[E, ?]] =
    new Applicative[Parser[E, ?]] {
      def point[A](a: => A): Parser[E,A] =
        Parser(input => Right((input, a))) // our point parser doesn't have any effect

      def ap[A, B](fa: => Parser[E,A])(
        f: => Parser[E, A => B]): Parser[E,B] =
        Parser(input => f.run(input) match {
          case Left(e) => Left(e)
          case Right((string0, fab)) =>
            fa.run(string0) match {
              case Left(e) => Left(e)
              case Right((string1, a)) => point(fab(a)).run(string1) // Right((string1, fab(a)))
            }
        })
    } // the soul of scala parser combinators: parse things in sequence, and combine the results
  // parser1 ~ parser2 (that's zip)
  // parser1 <~ parser2 (left fish)

  // Map is not an applicative:
  def pointMap[K, V](v: V): Map[K, V] = Map() // only way to write that
  // also true for tuples

  // trait Monad[F[_]] extends Applicative[F] {
  //   def bind[A, B](fa: F[A])(f: A => F[B]): F[B] // doesn't necessarily produce any Bs, could be None, Nil, Future.fail...
  // }
  // // option.flatMap((a: A) => ??? : Option[B]): Option[B]
  // trait Future[+A] { self => // name outer this => self
  //   def flatMap[B](f: A => Future[B]): Future[B] = ??? // can only call f when an A is available, has been computed
  ////     new Future[B] {
  ////     self // this, to distinguish
  ////   }
  // }

  implicit val OptionMonad = new Monad[Option] {
    override def point[A](a: => A): Option[A] = Some(a)

    override def bind[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = {
      fa match {
        case None => None
        case Some(a) => f(a)
      }
    }
  }
  implicit val ListMonad = new Monad[List] {
    override def point[A](a: => A): List[A] = List(a)

    override def bind[A, B](fa: List[A])(f: A => List[B]): List[B] = { // return is concatenation of f applied to every element in the list
      fa match {
        case Nil => Nil
        case a :: as => f(a) ++ bind(as)(f)
      }
    }
  }
  /*
  // Procedural program: context sensitive, sequential computation - just like Monad (they're monadic)
  a = doX()
  b = doY(a)
  c = doZ(a, b)
  if (b > c) doW(b, c)
  else doU(a)
   */

  // EXERCISE 11
  //
  // Define an instance of `Monad` for `BTree`.
  //
  implicit val MonadBTree: Monad[BTree] = new Monad[BTree] {
    override def point[A](a: => A): BTree[A] = Leaf(a)

    override def bind[A, B](fa: BTree[A])(f: A => BTree[B]): BTree[B] =
      fa match {
        case Leaf(a) => f(a)
        case Fork(l, r) => Fork(bind(l)(f), bind(r)(f))
      }
  }

  //
  // EXERCISE 12
  //
  // Define an instance of `Monad` for `Parser[E, ?]`.
  //
  implicit def MonadParser[E]: Monad[Parser[E, ?]] = new Monad[Parser[E, ?]] {
    override def point[A](a: => A): Parser[E, A] =
      Parser(input => Right((input, a))) // our point parser doesn't have any effect


    // Just to compare with bind
    //def zip[A, B](l: Parser[E, A], r: Parser[E, B]): Parser[E, (A, B)] = // also sequential (first l then r), but r can't depend on the run time value of the l parser
    //  Parser(input => l.run(input) match {
    //    case Left(e) => Left(e)
    //    case Right((string0, a)) => r.run(string0) match {
    //      case Left(e) => Left(e)
    //      case Right((string1, b)) => Right((string1, (a, b)))
    //    }
    //  })

    override def bind[A, B](fa: Parser[E, A])(f: A => Parser[E, B]): Parser[E, B] = // monadic parser, you can look at the runtime value produced by the parsing
      Parser(input => fa.run(input) match {
        case Left(e) => Left(e)
        case Right((string0, a)) => f(a).run(string0)
      })
  }
}

// Functor      - Gives us the power to map values produced by programs without changing their structure
// Apply         - Adds the power to combine two programs into one by combining their values
// Applicative - Adds the power to produce a “pure” program that produces a given result
// [Bind          -  <falls in between, but not useful in practice> ]
// Monad       - Adds the power to feed the result of one program into a function, which can look at the runtime value and return a new program, which is used to produce the result of the bind

object foldable {
  // iteration:
  //val l = List(1, 2, 3)
  //val it = l.iterator
  //while (it.hasNext) {
  //  it.next
  //}

  //
  // EXERCISE 1
  //
  // Define an instance of `Foldable` for `BTree`.
  //
  sealed trait BTree[+A]
  case class Leaf[A](a: A) extends BTree[A]
  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]
  implicit val FoldableBTree: Foldable[BTree] = new Foldable[BTree] {
    def foldMap[A, B](fa: BTree[A])(f: A => B)(implicit F: scalaz.Monoid[B]): B = // there is no empty tree for BTree, so you don't need a monoid, semigroup is enough
      fa match {                                                                  // but others need some way of producing a B => Monoid
        case Leaf(a) => f(a)
        case Fork(l, r) => foldMap(l)(f) |+| foldMap(r)(f)
      }

    def foldRight[A, B](fa: BTree[A],z: => B)(f: (A, => B) => B): B =
      fa match {
        case Leaf(a) => f(a, z)
        case Fork(l, r) =>
          foldRight(l, foldRight(r,z)(f))(f)

      }
  }

  //
  // EXERCISE 2
  //
  // Try to define an instance of `Foldable` for `A => ?`.
  //
  implicit def FunctionFoldable[C]: Foldable[C => ?] = new Foldable[C => ?] {
    override def foldMap[A, B](fa: C => A)(f: A => B)(implicit F: Monoid[B]): B = mzero[B]

    override def foldRight[A, B](fa: C => A, z: => B)(f: (A, => B) => B): B = z
  }
  // This instance is not useful, there is no way to fold over the values of the function because we don't know; and we don't have as

  //
  // EXERCISE 3
  //
  // Define an instance of `Traverse` for `BTree`.
  //
  implicit val TraverseBTree: Traverse[BTree] = new Traverse[BTree] {
    //def traverse0[G[_]: Applicative, A, B](fa: BTree[A])(f: A => G[B]): G[BTree[B]] = ??? // API calls etc

    override def traverseImpl[G[_], A, B](fa: BTree[A])(f: A => G[B])(implicit F: Applicative[G]): G[BTree[B]] =
      fa match {
        case Leaf(a) => f(a).map[BTree[B]](Leaf(_)) // i can call map because of the applicative
        case Fork(l, r) =>
          val lg: G[BTree[B]] = traverseImpl(l)(f)
          val rg: G[BTree[B]] = traverseImpl(r)(f)
          (lg |@| rg)(Fork(_, _)) //((l: BTree[B], r: BTree[B]) => Fork(l, r): BTree[B])
      }
  }

  // Gives you the card: [G[A]] => G[F[A]]
  type Future[A] = List[A] // just to hack it
  type Response = String
  def collectAllAPIs(results: BTree[Future[Response]]): Future[BTree[Response]] =
    results.sequenceU // enabled by traverseImpl



  //
  // EXERCISE 4
  //
  // Try to define an instance of `Traverse` for `Parser[E, ?]`.
  //
  case class Parser[+E, +A](run: String => Either[E, (String, A)])
  implicit def TraverseParser[E]: Traverse[Parser[E, ?]] = ??? // can't do it :o)
}

object optics {
  // ADT toy example
  sealed trait Country
  object Country {
    val usa: Prism[Country, Unit] = // could use USA.type, but not necessary (it's a case object), Unit has no information at all and easier to call
      Prism[Country, Unit]({
        case USA => Some(())
        case _ => None
      }, _ => USA)
  }
  case object USA extends Country
  case object UK extends Country
  case object Poland extends Country

  case class Org(name: String, address: Address, site: Site) // site was list
  object Org {
    val site: Lens[Org, Site] =
      Lens[Org, Site](_.site, l => _.copy(site = l))
  }
  case class Address(
                      number: String,
                      street: String,
                      postalCode: String,
                      country: Country)
  case class Site(
                   manager: Employee,
                   address: Address,
                   employees: Set[Employee])
  object Site {
    val manager: Lens[Site, Employee] =
      Lens[Site, Employee](_.manager, m => _.copy(manager = m))
  }
  case class Employee(
                       name: String,
                       dob: java.time.Instant,
                       salary: BigDecimal,
                       address: Address)
  object Employee {
    val salary: Lens[Employee, BigDecimal] =
      Lens[Employee, BigDecimal](_.salary, s => _.copy(salary = s))
  }

  lazy val org: Org = ???

  lazy val org2 = // Decrease managers salary to 95%
    org.copy(site =
      org.site.copy(manager = org.site.manager.copy(
        salary = org.site.manager.salary * 0.95
      ))
    )
  // Boilerplate!!! Due to immutability, copies of copies
  //
  //type Optic[S, A]
  // S - Super structure
  // A - Sub structure
  // An Optic S A allows you to focus in on a sub-structure A inside a super-structure S, for purposes of accessing or modifying the sub-structure

  //
  // EXERCISE 1
  //
  // Implement the `>>>` method of `Lens`.
  //
  final case class Lens[S, A](
                               get: S => A,
                               set: A => (S => S)
                             ) { self =>
    def >>> [B](that: Lens[A, B]): Lens[S, B] = // Compose lenses
      Lens[S, B](
        get = (s: S) => that.get(self.get(s)),
        set = (b: B) => (s: S) => self.set(that.set(b)(self.get(s)))(s)
      )

    final def update(f: A => A): S => S =
      (s: S) => self.set(f(self.get(s)))(s)
  }

  // Now create lenses inside above, companion objects

  //
  // EXERCISE 2
  //
  // Create a version of `org2` that uses lenses to update the salaries.
  //
  val org2_lens: Org =
  (Org.site >>> Site.manager >>> Employee.salary).
    update(_ * 0.95)(org)

  //
  // EXERCISE 3
  //
  // Implement `>>>` for `Prism`.
  //
  final case class Prism[S, A](
                                get: S => Option[A],
                                set: A => S) { self =>
    def >>> [B](that: Prism[A, B]): Prism[S, B] =
      Prism(
        get = (s: S) => self.get(s).flatMap(that.get),
        set = self.set.compose(that.set)
      )

    final def select(implicit ev: Unit =:= A): S =
      set(ev(()))
  }

  //
  // EXERCISE 4
  //
  // Implement `_Left` and `_Right`.
  //
  def _Left[A, B]: Prism[Either[A, B], A] = // Either[A, B] super structure, A sub structure
    Prism[Either[A, B], A]({
      case Left(a) => Some(a)
      case _ => None
    }, Left(_))

  def _Right[A, B]: Prism[Either[A, B], B] = // Either[A, B] super structure, B sub structure
    Prism[Either[A, B], B]({
      case Right(b) => Some(b)
      case _ => None
    }, Right(_))

  // Optic[S, T, A, B] // Super structure S => T, sub structure from A => B

  // Now create prisms for country

  // Application of Optics:
  //case class Component1[S](lens: Lens[S, Component1.State]) {
  //  def run[S](state: S): (S, Boolean) = ???
  //}
  //object Component1 {
  //  case class Config(server: String, port: Int)
  //  case class State(config: Config)
  //}
  //case class Component2[S](lens: Lens[S, Component2.State]) {
  //  def run[S](state: S): (S, Int) = ???
  //}
  //object Component2 {
  //  case class Config(loggingDirectory: String)
  //  case class State(config: Config)
  //}
  //case class MyAppState(
  //                       c1: Component1.State,
  //                       c2: Component2.State
  //                     )
  //object MyAppState {
  //  val c1: Lens[MyAppState, Component1.State] = ???
  //  val c2: Lens[MyAppState, Component2.State] = ???
  //}
  //val c1 : Component1[MyAppState] = Component1(MyAppState.c1)
  //val c2 : Component2[MyAppState] = Component2(MyAppState.c2)

}

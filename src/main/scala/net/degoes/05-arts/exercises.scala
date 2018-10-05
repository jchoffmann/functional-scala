// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.arts

import scalaz._
import Scalaz._

object exercises {

  trait NaturalTransformation[F[_], G[_]] { // functions where the args have kind `* -> *`
    def apply[A](fa: F[A]): G[A]
  }
  type ~> [F[_], G[_]] = NaturalTransformation[F, G]
  def headOption: List ~> Option =
    new NaturalTransformation[List, Option] {
      def apply[A](fa: List[A]): Option[A] =
        fa.headOption
    }
  def right[A]: Either[A, ?] ~> Option = ???
  def left[B]: Either[?, B] ~> Option =
    new NaturalTransformation[Either[?, B], Option] {
      def apply[A](fa: Either[A, B]): Option[A] =
        fa match {
          case Left(a) => Some(a)
          case Right(_) => None
        }
    }
  left(Left(42))    // Some(42)
  left(Left("bar")) // Some("bar")
  left(Right(42))   // None
  import scalaz.zio.IO
  def fromOption: Option ~> IO[Unit, ?] =
    new NaturalTransformation[Option, IO[Unit, ?]] {
      def apply[A](fa: Option[A]): IO[Unit, A] =
        fa match {
          case None    => (IO.fail(()) : IO[Unit, A]) // only IO.fail, we don't have an a
          case Some(a) => (IO.now(a)   : IO[Unit, A])
        }
    } // for all as, we can transform an Option to an IO[Unit, A]

  // Free monad: Given a functor you can get a monad
  object free {
    sealed trait Free[F[_], A] { self => // each one is an individual instruction in your program, A is the return value
      def map[B](f: A => B): Free[F, B] =
        self.flatMap(f.andThen(Free.point[F, B](_)))

      final def flatMap[B](f: A => Free[F, B]): Free[F, B] =
        Free.FlatMap(self, f)

      final def <* [B](that: Free[F, B]): Free[F, A] =
        self.flatMap(a => that.map(_ => a))

      final def *> [B](that: Free[F, B]): Free[F, B] =
        self.flatMap(_ => that)

      final def fold[G[_]: Monad](interpreter: F ~> G): G[A] = // ~> natural transformation
        self match {
          case Free.Return(value0)  => value0().point[G]
          case Free.Effect(fa)      => interpreter(fa)
          case Free.FlatMap(fa0, f) => fa0.fold(interpreter).flatMap(a0 => f(a0).fold(interpreter))

        }
    }
    object Free {
      case class Return[F[_], A](value0: () => A) extends Free[F, A] {
        lazy val value = value0
      }
      case class Effect[F[_], A](effect: F[A]) extends Free[F, A]
      case class FlatMap[F[_], A0, A](fa0: Free[F, A0], f: A0 => Free[F, A]) extends Free[F, A]

      def point[F[_], A](a: => A): Free[F, A] = Return(() => a)
      def lift[F[_], A](fa: F[A]): Free[F, A] = Effect(fa)
    }
    // How to use the free monad:
    sealed trait ConsoleF[A]
    final case object ReadLine extends ConsoleF[String]
    final case class PrintLine(line: String) extends ConsoleF[Unit]

    def readLine: Free[ConsoleF, String] = Free.lift[ConsoleF, String](ReadLine)
    def printLine(line: String): Free[ConsoleF, Unit] = Free.lift[ConsoleF, Unit](PrintLine(line))

    val program: Free[ConsoleF, String] =
      for {
        _    <- printLine("Good morning! What is your name?")
        name <- readLine
        _    <- printLine(s"Good to meet you, $name!")
      } yield name

   import scalaz.zio.interop.scalaz72._
   val programIO: IO[Nothing, String] =
     program.fold[IO[Nothing, ?]](new NaturalTransformation[ConsoleF, IO[Nothing, ?]] {
       def apply[A](consoleF: ConsoleF[A]): IO[Nothing, A] =
         consoleF match {
           case ReadLine        => IO.sync(scala.io.StdIn.readLine())
           case PrintLine(line) => IO.sync(println(line))
         }
     })

    // Free monad approach to testing
    case class TestData(input: List[String], output: List[String])
    case class State[S, A](run: S => (S, A)) {
      def eval(s: S): A = run(s)._2
    }
    object State {
      implicit def MonadState[S]: Monad[State[S, ?]] =
        new Monad[State[S, ?]] {
          def point[A](a: => A): State[S, A] = State(s => (s, a))
          def bind[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
            State[S, B](s => fa.run(s) match {
              case (s, a) => f(a).run(s)
            })
        }

      def get[S]: State[S, S] = State(s => (s, s))
      def set[S](s: S): State[S, Unit] = State(_ => (s, ()))
      def modify[S](f: S => S): State[S, Unit] =
        get[S].flatMap(s => set(f(s)))
    }

    val programState: State[TestData, String] =
      program.fold[State[TestData, ?]](new NaturalTransformation[ConsoleF, State[TestData, ?]] {
        def apply[A](consoleF: ConsoleF[A]): State[TestData, A] =
          consoleF match {
            case ReadLine =>
              for {
                data <- State.get[TestData]
                line = data.input.head
                _    <- State.set(data.copy(input = data.input.drop(1)))
              } yield line

            case PrintLine(line) =>
              State.modify[TestData](d => d.copy(output = line :: d.output))
          }
      })

    programState.eval(TestData("John" :: Nil, Nil))
  }

  object design {
    /**
      Loyalty Points Management

      - Entities - Data
        - Customer
        - Loyalty Points
        - LP Account
        - LP Issuer - places you can get points from
        - LP Receiver - places you can spend points
        - Notification
        - LP Offers
        - Email ?
        - Tiers?

      - Services - Functions / Operations
        - Customer performs some action to earn LP from an issuer
        - Customer spends loyalty points at a receiver
        - Customer transfers LP to another account
        - Customer sees account details including LP balance
        - Customer opens / closes LP account
        - Customer signs up / opt-ins for LP offers
        - Notification & transactional email
      */
    sealed abstract class DatabaseError extends Exception // Leads to good type inference, auto widening
    trait Source[E, A] {
      def fold[Z](z: Z)(f: (Z, A) => Z): IO[E, Z]
    }
    type Database[A] = IO[DatabaseError, A]
    type DatabaseSource[A] = Source[DatabaseError, A]
    type DatabaseDerived[A, B] = DatabaseSource[A] => Database[B]

    // ScalaZ 8
    trait Number[A] {
      def zero: A
      def one: A
      def plus(l: A, r: A): A
      def minus(l: A, r: A): A
      def times(l: A, r: A): A
    }
    object Number {
      def apply[A](implicit N: Number[A]): Number[A] = N
    }
    implicit class NumberSyntax[A](l: A) {
      def + (r: A)(implicit N: Number[A]): A = N.plus(l, r)
      def - (r: A)(implicit N: Number[A]): A = N.minus(l, r)
      def * (r: A)(implicit N: Number[A]): A = N.times(l, r)
    }

    final case class Customer[AccountId, Num](
                               name: String,
                               email: String,
                               account: Account[AccountId, Num])
    final case class Account[AccountId, Num](
                            id: AccountId,
                            transactions: DatabaseSource[Transaction[AccountId, Num]] // fully auditable, event sourcing
                            )
    object Account { // these will require some capabilities on A
      import Transaction._
      type TxnDerived[A, B] = DatabaseDerived[Transaction[A, B], B]

      def balance[A, B: Number]: TxnDerived[A, B] =
        _.fold[B](Number[B].zero) {
          case (balance, Redeem(v, _))   => balance - v
          case (balance, Earn(v, _))     => balance + v
          case (balance, Transfer(v, _)) => balance - v
        }
      def status[A, B]: TxnDerived[A, Status] =
        _.fold[Status](Status.Open) {
          case (status, _) => status
        }
      def tier[A, B: Number: Order](tiers: Map[B, Tier]): TxnDerived[A, B] = ???

      sealed trait Status
      object Status {
        case object Open   extends Status
        case object Closed extends Status
      }
      sealed trait Tier
      case object Tier {
        case object Silver   extends Tier
        case object Gold     extends Tier
        case object Platinum extends Tier
      }
    }
    final case class Reward()
    final case class Purchase(id: java.util.UUID, description: String, quantity: Int)

    sealed trait Transaction[+AccountId, +Num] // appears in the context of the account, so don't need account ID
    object Transaction {
      final case class Redeem  [Num](amount: Num, reward: Reward) extends Transaction[Nothing, Num]
      final case class Earn    [Num](amount: Num, purchase: Purchase) extends Transaction[Nothing, Num]
      final case class Transfer[AccountId, Num](amount: Num, recipient: AccountId) extends Transaction[AccountId, Num]
    }

    // Final tagless style, abstract over effects

    trait Confirmation

    sealed trait LoyaltyError
    object LoyaltyError {
      case object InsufficientBalance extends LoyaltyError
      case object InvalidReward extends LoyaltyError
      case object InvalidPurchase extends LoyaltyError
      case object InvalidAccount extends LoyaltyError
      case object ClosedAccount extends LoyaltyError
    }

    trait LoyaltyTransactions[F[_]] {
      type AccountID = java.util.UUID

      def redeem(accountID: AccountID, points: Long, reward: Reward): F[Either[LoyaltyError, Confirmation]]

      def earn(accountID: AccountID, points: Long, reward: Purchase): F[Either[LoyaltyError, Confirmation]]

      def transfer(sourceAccountID: AccountID, transferAccountID: java.util.UUID, points: Long): F[Either[LoyaltyError, Confirmation]]
    }

    trait Batch[A] { // alternative to the free Monad
      def apply[F[_]: LoyaltyTransactions: Applicative]: F[A]
    }

    // Use like this;
    //atomic { new Batch[A] {
    //  def apply[F[_]: LoyaltyTransactions]: F[A] =
    //    LoyaltyTransactions[F].redeem(...) *>
    //    (LoyaltyTransactions[F].earn(...) |@|
    //    LoyaltyTransactions[F].transfer(...))((_, _))
    //}}
    // Applicative allows us to combine multiple operations, not only one, and can introspect the leaves of the program as the data structure is static
    // Monad would make it hard, because you make decisions on the runtime valuee so you would need to implement the atomic guarantees yourself instead of relying on the database

    trait LoyaltyProgram[F[_]] { // we could push failure to F, or reflect that in the type e.g. Either
      type AccountID = java.util.UUID

      def atomic[A](batch: Batch[A]): F[A]

      def open: F[AccountID]

      def close(accountID: AccountID): F[AccountID] // type implies that you cannot reopen an account

      def balance(accountID: AccountID): F[Long]
    }
    object LoyaltyProgram {
      private object internal {
        type AccountID = java.util.UUID

        // Data structure
        sealed trait Statement[A] { self =>
          final def map[B](f: A => B): Statement[B] =
            self.zipWith(Statement.point(()))((a, _) => f(a))

          final def zipWith[B, C](that: Statement[B])(f: (A, B) => C): Statement[C] =
            ZipWith(self, that, f)

          final def zip[B](that: Statement[B]): Statement[(A, B)] = zipWith(that)(_ -> _)

          final def *> [B](that: Statement[B]): Statement[B] = self.zip(that).map(_._2)

          final def <* [B](that: Statement[B]): Statement[A] = self.zip(that).map(_._1)
        }
        object Statement {
          final def point[A](a: => A): Statement[A] = Return(a)
        }
        final case class Return[A](value: A) extends Statement[A]
        final case class ZipWith[A, B, C](l: Statement[A], r: Statement[B], f: (A, B) => C) extends Statement[C]
        final case class Earn(accountID: AccountID, points: Long, purchase: Purchase) extends Statement[Either[LoyaltyError, Confirmation]]
        final case class Redeem(accountID: AccountID, points: Long, reward: Reward) extends Statement[Either[LoyaltyError, Confirmation]]
        final case class Transfer(sourceAccountID: AccountID, transferAccountID: AccountID, points: Long) extends Statement[Either[LoyaltyError, Confirmation]]

        implicit val LoyaltyTransactionsInstance: LoyaltyTransactions[Statement] with Applicative[Statement] =
          new LoyaltyTransactions[Statement] with Applicative[Statement] {
            def point[A](a: => A): Statement[A] = Return(a)

            def ap[A, B](fa: =>Statement[A])(f: =>Statement[A => B]): Statement[B] =
              f.zipWith(fa)((f, a) => f(a))

            def earn(accountID: AccountID, points: Long, purchase: Purchase): Statement[Either[LoyaltyError, Confirmation]] =
              Earn(accountID, points, purchase)

            def redeem(accountID: AccountID, points: Long, reward: Reward): Statement[Either[LoyaltyError, Confirmation]] =
              Redeem(accountID, points, reward)

            def transfer(sourceAccountID: AccountID, transferAccountID: AccountID, points: Long): Statement[Either[LoyaltyError, Confirmation]] =
              Transfer(sourceAccountID, transferAccountID, points)
          }
      }

      implicit val LoyaltyProgramIO: LoyaltyProgram[IO[Exception, ?]] =
        new LoyaltyProgram[IO[Exception, ?]] {
          import internal._
          import java.sql.ResultSet
          import java.sql.{Statement => JStatement}

          type Query = String

          def interpret[A](statement: Statement[A]): (JStatement, ResultSet => IO[Exception, A]) =
            ??? // construct SQL statements
          // if you want to do optimisations, you would possibly need intermediate types lower level than Statements

          def atomic[A](batch: Batch[A]): IO[Exception, A] = {
            val statement: Statement[A] = batch[Statement]

            val (jstatement, processor) = interpret(statement)

            val resultSet = IO.syncException(jstatement.executeBatch()) *>
              IO.syncException(jstatement.getResultSet)

            resultSet.flatMap(processor)
          }

          def open: IO[Exception, AccountID] = ???

          def close(accountID: AccountID): IO[Exception, AccountID] = ???

          def balance(accountID: AccountID): IO[Exception, Long] = ???
        }
    }
  }


  object fixpoint {
    sealed trait Json[+A]
    object Json {
      case object Null extends Json[Nothing]
      case class Bool[A](boolean: Boolean) extends Json[A]
      case class Str[A](string: String) extends Json[A]
      case class Num[A](number: BigDecimal) extends Json[A]
      case class Arr[A](array: List[A]) extends Json[A]
      case class Obj[A](obj: Map[String, A]) extends Json[A]

      implicit val TraverseJson: Traverse[Json] = ???
    }
  }

  object selectable {
    sealed trait Parser[+E, +A] { self =>
      import Parser._

      def map[B](f: A => B): Parser[E, B] = Map[E, A, B](self, f)

      def || [E1 >: E, B](that: Parser[E1, B]): Parser[E1, Either[A, B]] =
        Alternative(self, that)

      def * : Parser[E, List[A]] = Repeat(self)

      def ~ [E1 >: E, B](that: Parser[E1, B]): Parser[E1, (A, B)] = Zip(self, that)

      def <~ [E1 >: E, B](that: Parser[E1, B]): Parser[E1, A] = (self ~ that).map(_._1)

      def ~> [E1 >: E, B](that: Parser[E1, B]): Parser[E1, B] = (self ~ that).map(_._2)
    }
    object Parser {
      def fail[E](e: E): Parser[E, Nothing] = Fail(e)

      def char[E](e: E): Parser[E, Char] = Character(e)

      def select[E, A](cond: Parser[E, Boolean])(
        ifTrue: Parser[E, A], ifFalse: Parser[E, A]): Parser[E, A] =
        Select(cond, ifTrue, ifFalse)

      case class Fail[E](error: E) extends Parser[E, Nothing]
      case class Succeed[A](value: A) extends Parser[Nothing, A]
      case class Character[E](error: E) extends Parser[E, Char]
      case class Repeat[E, A](value: Parser[E, A]) extends Parser[E, List[A]]
      case class Alternative[E, A, B](left: Parser[E, A], right: Parser[E, B]) extends Parser[E, Either[A, B]]
      case class Zip[E, A, B](left: Parser[E, A], right: Parser[E, B]) extends Parser[E, (A, B)]
      case class Map[E, A0, A](value: Parser[E, A0], f: A0 => A) extends Parser[E, A]
      case class Select[E, A](
        condition: Parser[E, Boolean], ifTrue: Parser[E, A], ifFalse: Parser[E, A]) extends Parser[E, A]

      implicit def ApplicativeParser[E]: Applicative[Parser[E, ?]] =
        new Applicative[Parser[E, ?]] {
          def point[A](a: => A): Parser[E,A] = Succeed(a)
          def ap[A, B](fa: => Parser[E,A])(f: => Parser[E,A => B]): Parser[E,B] =
            Map[E, (A => B, A), B](Zip(f, fa), t => t._1(t._2))
        }
    }

    def compiler[E, A](parser: Parser[E, A]): String => Either[E, A] =
      (input: String) => {
        var index: Int = 0
        var error: E = null.asInstanceOf[E]
        var value: A = null.asInstanceOf[A]
        type Repr = () => Unit

        def compile0(parser: Parser[E, A]): Repr = ???

        compile0(parser)()

        if (error != null) Left(error) else Right(value)
      }
  }

  object hoas {
    
  }
}

// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.effects

import scalaz.zio._
import scalaz.zio.console._
import scala.concurrent.duration._

object notepad {
  //def println(line: String): Unit = () // These are the FP implementations
  //def readLine: String = ""

  // Instead of interacting with the real world, build an immutable data structure that describes interactions with the real world
  sealed trait Program[A] { self =>
    import Program.{Return, WriteLine, ReadLine}
    // No ScalaZ imported here, so make it Monadic by writing map, zip (to combine)
    //final def map[B](f: A => B): Program[B] = self match {
    //  case Return(thunk)         => Return(() => f(thunk))
    //  case WriteLine(line, next) => WriteLine(line, next.map(f))
    //  case ReadLine(next)        => ReadLine(input => next(input).map(f))
    //}
    final def map[B](f: A => B): Program[B] = flatMap(a => Program.point(f(a))) // flatMap(f andThen (Program.point(_)))

    final def zip[B](that: Program[B]): Program[(A, B)] =
      for {
        a <- self
        b <- that
      } yield (a, b)

    final def flatMap[B](f: A => Program[B]): Program[B] = self match {
      case Return(thunk)         => f(thunk())
      case WriteLine(line, next) => WriteLine(line, next.flatMap(f))
      case ReadLine(next) => ReadLine(input => next(input).flatMap(f))
    }

  }
  object Program {
    final case class Return[A](value: () => A) extends Program[A]
    final case class WriteLine[A](line: String, next: Program[A]) extends Program[A]
    final case class ReadLine[A](next: String => Program[A]) extends Program[A]

    // Helper functions
    def point[A](a: => A): Program[A] = Return(() => a)
    def writeLine(line: String): Program[Unit] = WriteLine(line, point(()))
    val readLine: Program[String] = ReadLine(point(_)) // was def, can change to a val (in purely functional programs, refential transparency)
  }

  import Program.{point, writeLine, readLine}
  // Can only write simple programs
  val hellowWorld = writeLine("Hello World!")
  val getInput = readLine

  // Implemented map and zip (via FlatMap)
  val getInt: Program[Option[Int]] = readLine.map(text => scala.util.Try(text.toInt).toOption)

  val program: Program[String] =
    for {
      _    <- writeLine("Hello World!")
      _    <- writeLine("What is your name?")
      name <- readLine
      _    <- writeLine("Hello, " + name + ", it is good to meet you!")
    } yield name
  //program // doesn't do anything, it is just a data structure describing interactions with the real world
  // Can now treat programs as first class citizens, can store in data structures etc
}

object zio_background {
  sealed trait Program[A] { self =>
    final def *> [B](that: Program[B]): Program[B] =
      self.flatMap(_ => that)
    final def <* [B](that: Program[B]): Program[A] =
      self.flatMap(a => that.map(_ => a))

    final def map[B](f: A => B): Program[B] =
      self match {
        case Program.ReadLine(next) =>
          Program.ReadLine(input => next(input).map(f))
        case Program.WriteLine(line, next) =>
          Program.WriteLine(line, next.map(f))
        case Program.Return(value) =>
          Program.Return(() => f(value()))
      }

    final def flatMap[B](f: A => Program[B]): Program[B] =
      self match {
        case Program.ReadLine(next) =>
          Program.ReadLine(input => next(input).flatMap(f))
        case Program.WriteLine(line, next) =>
          Program.WriteLine(line, next.flatMap(f))
        case Program.Return(value) =>
          f(value())
      }
  }
  object Program {
    final case class ReadLine[A](next: String => Program[A]) extends Program[A]
    final case class WriteLine[A](line: String, next: Program[A]) extends Program[A]
    final case class Return[A](value: () => A) extends Program[A]

    val readLine: Program[String] = ReadLine(point[String](_))
    def writeLine(line: String): Program[Unit] = WriteLine(line, point(()))
    def point[A](a: => A): Program[A] = Return(() => a)
  }

  import Program.{readLine, writeLine, point}

  val yourName1: Program[Unit] =
    writeLine("What is your name?").flatMap(_ =>
      readLine.flatMap(name =>
        writeLine("Hello, " + name + ", good to meet you!").flatMap(_ =>
          point(())
        )
      )
    )

  //
  // EXERCISE 1
  //
  // Rewrite `program1` to use a for comprehension.
  //
  val yourName2: Program[Unit] =
    for {
      _    <- writeLine("What is your name?")
      name <- readLine
      _    <- writeLine("Hello, " + name + ", good to meet you!")
    } yield ()
  // Scala translates for-comprehensions into a flatMaps and a final maps (and filters)
  // (problem with final map is when you have a recursive call, will leak heap)
  // In for-comprehensions: To the right of ever arrow make sure you have a monadic value (F), in yield normal value

  //
  // EXERCISE 2
  //
  // Rewrite `yourName2` using the helper function `getName`, which shows how
  // to create larger programs from smaller programs.
  //
  def yourName3: Program[Unit] =
    for {
      name <- getName
      _    <- writeLine("Hello, " + name + ", good to meet you!")
    } yield ()

  val getName: Program[String] =
    writeLine("What is your name?").flatMap(_ => readLine)

  //
  // EXERCISE 3
  //
  // Implement the following effectful procedure, which interprets
  // `Program[A]` into `A`. You can use this procedure to "run" programs.
  //
  def interpret[A](program: Program[A]): A =
    program match {
      case Program.Return(thunk)         => thunk()
      case Program.WriteLine(line, next) => println(line); interpret(next)
      case Program.ReadLine(next)        => interpret(next(scala.io.StdIn.readLine()))
    }

  //
  // EXERCISE 4
  //
  // Implement the following function, which shows how to write a combinator
  // that operates on programs.
  //
  def sequence[A](programs: List[Program[A]]): Program[List[A]] =
    programs match {
      case Nil => point(Nil)
      case p :: ps => for {
        a <- p
        as <- sequence(ps)
      } yield a :: as
  }

  //
  // EXERCISE 5
  //
  // Translate the following procedural program into a purely functional program
  // using `Program` and a for comprehension.
  //
  def ageExplainer1(): Unit = {
    println("What is your age?")
    scala.util.Try(scala.io.StdIn.readLine().toInt).toOption match {
      case Some(age) =>
        if (age < 12) println("You are a kid")
        else if (age < 20) println("You are a teenager")
        else if (age < 30) println("You are a grownup")
        else if (age < 50) println("You are an adult")
        else if (age < 80) println("You are a mature adult")
        else if (age < 100) println("You are elderly")
        else println("You are probably lying.")
      case None =>
        println("That's not an age, try again")

        ageExplainer1()
    }

  }
  def ageExplainer2: Program[Int] = // was: Program[Unit], but Program[Int] more interesting
    for {
      _    <- writeLine("What is your age?")
      line <- readLine
      age  <- scala.util.Try(line.toInt).toOption match {
               case Some(age) =>
                 (if      (age < 12)  writeLine("You are a kid")
                  else if (age < 20)  writeLine("You are a teenager")
                  else if (age < 30)  writeLine("You are a grownup")
                  else if (age < 50)  writeLine("You are an adult")
                  else if (age < 80)  writeLine("You are a mature adult")
                  else if (age < 100) writeLine("You are elderly")
                  else                writeLine("You are probably lying.")) *> point(age)
                case None =>
                  writeLine("That's not an age, try again")                 *> ageExplainer2
                // GOTCHA:
                // Since this is functional code, writeLine(..) followed by recursive calling ageExplainer2 would throw
                // the writeLine result away and the code would be equivalent to just ageExplainer2. We need to use the
                // fish operator to make sure both actions happen, and throw away one result
              }// type Program[Unit]
    } yield age
}

object zio_type {
  type ??? = Nothing

  // type IO[E, A] // synchronous, asynchronous, concurrent, parallel, effectful

  //
  // EXERCISE 1
  //
  // Write the type of `IO` values that can fail with an `Exception`, or
  // may produce an `A`.
  //
  type Exceptional[A] = IO[Exception, A]

  //
  // EXERCISE 2
  //
  // Write the type of `IO` values that can fail with a `Throwable`, or
  // may produce an `A`.
  //
  type Task[A] = IO[Throwable, A]
  // Throwable is the root of the hierarchy, contains errors and exceptions, but errors are not recoverable
  // So you might prefer Exceptional

  //
  // EXERCISE 3
  //
  // Write the type of `IO` values that cannot fail, but may produce an `A.`
  //
  type Infallible[A] = IO[Nothing, A] // Nothing => compiler guarantees that there can be no values of that type

  //
  // EXERCISE 4
  //
  // Write the type of `IO` values that cannot produce a value, but may fail
  // with an `E`.
  //
  type Unproductive[E] = IO[E, Nothing]

  //
  // EXERCISE 5
  //
  // Write the type of `IO` values that cannot fail or produce a value.
  //
  type Unending = IO[Nothing, Nothing] // Like a server that you never want to go down
}

object zio_values {
  //
  // EXERCISE 1
  //
  // Using the `IO.now` method, lift the integer `2` into a strictly-evaluated
  // `IO`.
  //
  val ioInteger: IO[Nothing, Int] = IO.now(2)

  //
  // EXERCISE 2
  //
  // Using the `IO.point` method, lift the string "Functional Scala" into a
  // lazily-evaluated `IO`.
  //
  val ioString: IO[Nothing, String] = IO.point("Functional Scala")

  //
  // EXERCISE 3
  //
  // Using the `IO.fail` method to lift the string "Bad Input" into a failed
  // `IO`.
  //
  val failedInput: IO[String, Nothing] = IO.fail("Bad Input")
}

object zio_composition {
  implicit class FixMe[A](a: A) {
    def ?[B] = ???
  }

  //
  // EXERCISE 1
  //
  // Map the `IO[Nothing, Int]` into an `IO[Nothing, String]` by converting the
  // integer into its string rendering using the `map` method of the `IO`
  // object.
  //
  (IO.point(42).map(_.toString) : IO[Nothing, String])

  //
  // EXERCISE 2
  //
  // Map the `IO[Int, Nothing]` into an `IO[String, Nothing]` by converting the
  // integer error into its string rendering using the `leftMap` method of the
  // `IO` object.
  //
  (IO.fail(42).leftMap(_.toString) : IO[String, Nothing])

  //
  // EXERCISE 3
  //
  // Using the `flatMap` and `map` methods of `IO`, add `ioX` and `ioY`
  // together.
  //
  val ioX: IO[Nothing, Int] = IO.point(42)
  val ioY: IO[Nothing, Int] = IO.point(58)
  val ioXPlusY: IO[Nothing, Int] = ioX.flatMap(x => ioY.map(x + _))

  //
  // EXERCISE 4
  //
  // Using the `flatMap` method of `IO`, implement `ifThenElse`.
  //
  def ifThenElse[E, A](bool: IO[E, Boolean])(
    ifTrue: IO[E, A], ifFalse: IO[E, A]): IO[E, A] =
    for {
      b <- bool
      a <- if (b) ifTrue else ifFalse // conditional needs to be right of the arrow
    } yield a
  val exampleIf = ifThenElse(IO.point(true))(IO.point("It's true!"), IO.point("It's false!"))

  //
  // EXERCISE 5
  //
  // Translate the following program, which uses for-comprehensions, to its
  // equivalent chain of `flatMap`'s, followed by a final `map`.
  //
  for {
    v1 <- IO.point(42)
    v2 <- IO.point(58)
  } yield "The total is: " + (v1 + v2).toString

  //
  // EXERCISE 6
  //
  // Rewrite the following procedural program, which uses conditionals, into its
  // ZIO equivalent.
  //
  def decode1(read: () => Byte): Either[Byte, Int] = {
    val b = read()
    if (b < 0) Left(b)
    else {
      Right(b.toInt +
      (read().toInt << 8) +
      (read().toInt << 16) +
      (read().toInt << 24))
    }
  }
  def decode2[E](read: IO[E, Byte]): IO[E, Either[Byte, Int]] =
    for {
      b1 <- read
      e <- if (b1 < 0) IO.now(Left(b1))
           else for {
             b2 <- read
             b3 <- read
             b4 <- read
           } yield Right(b1.toInt +
              (b2.toInt << 8) +
              (b3.toInt << 16) +
              (b4.toInt << 24))
    } yield e
  //
  // EXERCISE 7
  //
  // Rewrite the following procedural program, which uses conditionals, into its
  // ZIO equivalent.
  //
  def getName1(print: String => Unit, read: () => String): Option[String] = {
    print("Do you want to enter your name?")
    read().toLowerCase.take(1) match {
      case "y" => Some(read())
      case _ => None
    }
  }
  def getName2[E](print: String => IO[E, String], read: IO[E, String]): IO[E, Option[String]] = ???

  //
  // EXERCISE 8
  //
  // Translate the following loop into its ZIO equivalent.
  //
  def forever1(action: () => Unit): Unit =
    while (true) action()
  def forever2[A](action: IO[Nothing, A]): IO[Nothing, Nothing] =
    action.flatMap(_ => forever2(action))

  //
  // EXERCISE 9
  //
  // Translate the following loop into its ZIO equivalent.
  //
  def repeatN1(n: Int, action: () => Unit): Unit =
    if (n <= 0) ()
    else {
      action()
      repeatN1(n - 1, action)
    }
  def repeatN2[E](n: Int, action: IO[E, Unit]): IO[E, Unit] =
    if (n <= 0) IO.now(())
    else action.flatMap(_ => repeatN2(n - 1, action))

  //
  // EXERCISE 10
  //
  // Translate the following expression into its `flatMap` equivalent.
  //
  IO.point(42) *> IO.point(19)
  IO.point(42).flatMap(_ => IO.point(19))

  //
  // EXERCISE 11
  //
  // Translate the following expression into its `flatMap` equivalent.
  //
  IO.point(42) <* IO.point(19)
  IO.point(42).flatMap(left => IO.point(19).map(_ => left))

  //
  // EXERCISE 12
  //
  // Translate the following expression into an equivalent expression using
  // the `map` and `flatMap` methods of the `IO` object.
  //
  (IO.point(42) <* IO.point(19)) *> IO.point(1)
}

object zio_failure {
  implicit class FixMe[A](a: A) {
    def ?[B] = ???
  }

  //
  // EXERCISE 1
  //
  // Using the `IO.fail` method, create an `IO[String, Int]` value that
  // represents a failure with a string error message, containing a user-
  // readable description of the failure.
  //
  val stringFailure1: IO[String, Int] =
    ???

  //
  // EXERCISE 2
  //
  // Using the `IO.fail` method, create an `IO[Int, String]` value that
  // represents a failure with an integer error code.
  //
  val intFailure: IO[Int, String] = ???

  //
  // EXERCISE 3
  //
  // Transform the error of `intFailure` into its string representation using
  // the `leftMap` method of `IO`.
  //
  val stringFailure2: IO[String, String] = ???

  //
  // EXERCISE 4
  //
  // Translate the following exception-throwing program into its ZIO equivalent.
  //
  def accessArr1[A](i: Int, a: Array[A]): A =
    if (i < 0 || i >= a.length) throw new IndexOutOfBoundsException("The index " + i + " is out of bounds [0, " + a.length + ")")
    else a(i)
  def accessArr2[A](i: Int, a: Array[A]): IO[IndexOutOfBoundsException, A] =
    if (i < 0 || i >= a.length) IO.fail(new IndexOutOfBoundsException("The index " + i + " is out of bounds [0, " + a.length + ")"))
    else IO.point(a(i))

  //
  // EXERCISE 5
  //
  // Translate the following ZIO program into its exception-throwing equivalent.
  //
  trait DenomIsZero
  object DenomIsZero extends DenomIsZero {}
  def divide1(n: Int, d: Int): IO[DenomIsZero, Int] =
    if (d == 0) IO.fail(DenomIsZero)
    else IO.now(n / d)
  def divide2(n: Int, d: Int): Int = ???

  //
  // EXERCISE 6
  //
  // Recover from a division by zero error by returning `-1`.
  //
  val recovered1: IO[Nothing, Int] =
    divide1(100, 0).attempt.map {
      case Left(error) => -1
      case Right(value) => value
    }
  // attempt: IO[E, A] => IO[Nothing, Either[E, A]] moving error along, resulting IO cannot fail

  //
  // EXERCISE 7
  //
  // Recover from a division by zero error by using `redeem`.
  //
  val recovered2: IO[Nothing, Int] =
    divide1(100, 0).redeem(_ => IO.now(-1), IO.now)
  // removes boilerplate seen in attempt

  //
  // EXERCISE 8
  //
  // Use the `orElse` method of `IO` to try `firstChoice`, and fallback to
  // `secondChoice` only if `firstChoice` fails.
  //
  val firstChoice: IO[DenomIsZero, Int] = divide1(100, 0)
  val secondChoice: IO[Nothing, Int] = IO.now(400)
  val combined: IO[Nothing, Int] = firstChoice orElse secondChoice
}

object zio_effects {

  import scala.io.StdIn.readLine
  import scala.io.Source
  import java.io.File
  import java.util.concurrent.{Executors, TimeUnit}

  type ??? = Nothing

  implicit class FixMe[A](a: A) {
    def ?[B] = ???
  }

  //
  // EXERCISE 1
  //
  // Using the `IO.sync` method, wrap Scala's `println` method to import it into
  // the world of pure functional programming.
  //
  def putStrLn(line: String): IO[Nothing, Unit] = IO.sync(println(line))

  //
  // EXERCISE 2
  //
  // Using the `IO.sync` method, wrap Scala's `readLine` method to import it
  // into the world of pure functional programming.
  //
  val getStrLn: IO[Nothing, String] = IO.sync(readLine)
  // but readLine can fail, so this is not reflected in the types; Thread is terminated and reported to supervisor

  //
  // EXERCISE 3
  //
  // Using the `IO.syncException` method, wrap Scala's `getLines` method to
  // import it into the world of pure functional programming.
  //
  def readFile1(file: File): IO[Exception, List[String]] =
    IO.syncException(Source.fromFile(file).getLines.toList)

  //
  // EXERCISE 3.5
  //
  // Using the `IO.syncThrowable` method, wrap Scala's `getLines` method to
  // import it into the world of pure functional programming.
  //
  def readFile2(file: File): IO[Throwable, List[String]] =
    IO.syncThrowable(Source.fromFile(file).getLines.toList)

  //
  // EXERCISE 3.75
  //
  // Using the `IO.syncCatch` method, wrap Scala's `getLines` method to
  // import it into the world of pure functional programming.
  //
  import java.io.IOException

  def readFile3(file: File): IO[IOException, List[String]] =
    IO.syncCatch(Source.fromFile(file).getLines.toList) {
      case e: IOException => e // possible different error type
    }

  // only if you want to catch a specific type of exception

  //
  // EXERCISE 4
  //
  // Identify the correct method and error type to import `System.nanoTime`
  // safely into the world of pure functional programming.
  //
  def nanoTime: IO[Nothing, Long] = IO.sync(System.nanoTime)

  //
  // EXERCISE 5
  //
  // Identify the correct method, error, and value type to import `System.exit`
  // safely into the world of pure functional programming.
  //
  def sysExit(code: Int): IO[SecurityException, Nothing] = IO.syncCatch(System.exit(code)) { // Return type should be Nothing, since it fails or never returns
    case e: SecurityException => e
  } *> IO.terminate // you need to prove to the compiler that it can never terminate
  //} *> IO.never
  // more precise than: IO.syncException(System.exit(code))

  //
  // EXERCISE 6
  //
  // Identify the correct method, error, and value type to import
  // `Array.update` safely into the world of pure functional programming.
  //
  def arrayUpdate[A](a: Array[A], i: Int, f: A => A): IO[ArrayIndexOutOfBoundsException, Unit] =
    IO.syncCatch(a.update(i, f(a(i)))) {
      case e: ArrayIndexOutOfBoundsException => e
    }

  // Async:
  IO.async[Nothing, Int](k => k(ExitResult.Completed(42))): IO[Nothing, Int]
  //for {
  //  result <- callApi(x)
  //  result2 <- callOtherApi(y)
  //  result3 <- makeDatabaseQuery(result, result2)
  //} yield result3

  //
  // EXERCISE 7
  //
  // Use the `IO.async` method to implement the following `sleep` method, and
  // choose the correct error type.
  //
  val scheduledExecutor = Executors.newScheduledThreadPool(1)

  def sleep(l: Long, u: TimeUnit): IO[Nothing, Unit] = IO.async[Nothing, Unit] { callback =>
    scheduledExecutor.schedule(
      new Runnable {
        def run(): Unit = callback(ExitResult.Completed(()))
      }, l, u
    )
  }
  // Using Nothing, still computation can fail if executor has been shut down, but we consider that as a nonrecoverable error

  //
  // EXERCISE 8
  //
  // Translate the following procedural program into ZIO.
  //
  def playGame1(): Unit = {
    val number = scala.util.Random.nextInt(5)
    println("Enter a number between 0 - 5: ")
    scala.util.Try(scala.io.StdIn.readLine().toInt).toOption match {
      case None =>
        println("You didn't enter an integer!")
        playGame1
      case Some(guess) if (guess == number) =>
        println("You guessed right! The number was " + number)
      case _ =>
        println("You guessed wrong! The number was " + number)
    }
  }

  def playGame2: IO[Exception, Unit] =
    for {
      number <- IO.sync(scala.util.Random.nextInt(5))
      _      <- putStrLn("Enter a number between 0 - 5: ")
      line   <- getStrLn
      _      <- scala.util.Try(line.toInt).toOption match {
                  case None =>
                    putStrLn("You didn't enter an integer!")                *> playGame2
                  case Some(guess) if (guess == number) =>
                    putStrLn("You guessed right! The number was " + number)
                  case _ =>
                    putStrLn("You guessed wrong! The number was " + number)
                }
    } yield ()
}

object zio_concurrency {
  type ??? = Nothing

  implicit class FixMe[A](a: A) {
    def ?[B] = ???
  }

  //
  // EXERCISE 1
  //
  // Race `leftContestent1` and `rightContestent1` using the `race` method of
  // `IO` to see which one finishes first with a successful value.
  //
  val leftContestent1 = IO.never
  val rightContestent1 = putStrLn("Hello World")
  val raced1 = ???

  //
  // EXERCISE 2
  //
  // Race `leftContestent2` and `rightContestent2` using the `race` method of
  // `IO` to see which one finishes first with a successful value.
  //
  val leftContestent2: IO[Exception, Nothing] = IO.fail(new Exception("Uh oh!"))
  val rightContestent2: IO[Exception, Unit] = IO.sleep(10.milliseconds) *> putStrLn("Hello World")
  val raced2: ??? = ???

  //
  // EXERCISE 3
  //
  // Compute `leftWork1` and `rightWork1` in parallel using the `par` method of
  // `IO`.
  //
  val leftWork1: IO[Nothing, Int] = fibonacci(10)
  val rightWork1: IO[Nothing, Int] = fibonacci(10)
  val par1: ??? = ???

  //
  // EXERCISE 4
  //
  // Compute all values `workers` in parallel using `IO.parAll`.
  //
  val workers: List[IO[Nothing, Int]] =
    (1 to 10).toList.map(fibonacci(_))
  val workersInParallel: IO[Nothing, List[Int]] =
    ???

  //
  // EXERCISE 5
  //
  // Implement `myPar` by forking `left` and `right`, and then joining them
  // and yielding a tuple of their results.
  //
  def myPar[E, A, B](left: IO[E, A], right: IO[E, B]): IO[E, (A, B)] =
    ???

  //
  // EXERCISE 6
  //
  // Use the `IO.supervise` method to ensure that when the main fiber exits,
  // all fibers forked within it will be terminated cleanly.
  //
  val supervisedExample: IO[Nothing, Unit] =
    (for {
      fiber <- fibonacci(10000).fork
    } yield ())

  //
  // EXERCISE 7
  //
  // Use the `interrupt` method of the `Fiber` object to cancel the long-running
  // `fiber`.
  //
  val interrupted1: IO[Nothing, Unit] =
    for {
      fiber <- fibonacci(10000).fork
    } yield ()

  //
  // EXERCISE 8
  //
  // Use the `zipWith` method of the `Fiber` object to combine `fiber1` and
  // `fiber2` into a single fiber (by summing the results), so they can be
  // interrupted together.
  //
  val interrupted2: IO[Nothing, Unit] =
    for {
      fiber1 <- fibonacci(10).fork
      fiber2 <- fibonacci(20).fork
      both   <- (??? : IO[Nothing, Fiber[Nothing, Int]])
      _      <- both.interrupt
    } yield ()

  //
  // EXERCISE 9
  //
  // io.timeout[Option[A]](None)(Some(_))(60.seconds)

  def fibonacci(n: Int): IO[Nothing, Int] =
    if (n <= 1) IO.now(n)
    else fibonacci(n - 1).seqWith(fibonacci(n - 2))(_ + _)
}

object zio_resources {
  import java.io.{File, FileInputStream}
  class InputStream private (is: FileInputStream) {
    def read: IO[Exception, Option[Byte]] =
      IO.syncException(is.read).map(i => if (i < 0) None else Some(i.toByte))
    def close: IO[Exception, Unit] =
      IO.syncException(is.close())
  }
  object InputStream {
    def openFile(file: File): IO[Exception, InputStream] =
      IO.syncException(new InputStream(new FileInputStream(file)))
  }

  //
  // EXERCISE 1
  //
  // Rewrite the following procedural program to ZIO, using `IO.fail` and the
  // `bracket` method of the `IO` object.
  //
  def tryCatch1(): Unit =
    try throw new Exception("Uh oh")
    finally println("On the way out...")
  val tryCatch2: IO[Exception, Unit] =
    ???

  //
  // EXERCISE 2
  //
  // Rewrite the `readFile1` function to use `bracket` so resources can be
  // safely cleaned up in the event of errors, defects, or interruption.
  //
  def readFile1(file: File): IO[Exception, List[Byte]] = {
    def readAll(is: InputStream, acc: List[Byte]): IO[Exception, List[Byte]] =
      is.read.flatMap {
        case None => IO.now(acc.reverse)
        case Some(byte) => readAll(is, byte :: acc)
      }

    for {
      stream <- InputStream.openFile(file)
      bytes  <- readAll(stream, Nil)
    } yield bytes
  }
  def readFile2(file: File): IO[Exception, List[Byte]] = ???

  //
  // EXERCISE 3
  //
  // Implement the `tryCatchFinally` method using `bracket`.
  //
  def tryCatchFinally[E, A]
    (try0: IO[E, A])
    (catch0: PartialFunction[E, IO[E, A]])
    (finally0: IO[Nothing, Unit]): IO[E, A] =
      ???

  //
  // EXERCISE 4
  //
  // Use the `bracket` method to rewrite the following snippet to ZIO.
  //
  def readFileTCF1(file: File): List[Byte] = {
    var fis : FileInputStream = null

    try {
      fis = new FileInputStream(file)
      val array = Array.ofDim[Byte](file.length.toInt)
      fis.read(array)
      array.toList
    } catch {
      case e : java.io.IOException => Nil
    } finally if (fis != null) fis.close()
  }
  def readFileTCF2(file: File): IO[Exception, List[Byte]] =
    ???
}

object zio_schedule {
  implicit class FixMe[A](a: A) {
    def ? = ???
  }

  //
  // EXERCISE 1
  //
  // Using `Schedule.recurs`, create a schedule that recurs 5 times.
  //
  val fiveTimes: Schedule[Any, Int] =
    ???

  //
  // EXERCISE 2
  //
  // Using the `repeat` method of the `IO` object, repeat printing "Hello World"
  // five times to the console.
  //
  val repeated1 = putStrLn("Hello World")

  //
  // EXERCISE 3
  //
  // Using `Schedule.spaced`, create a schedule that recurs forever every 1
  // second.
  //
  val everySecond: Schedule[Any, Int] =
    ???

  //
  // EXERCISE 4
  //
  // Using the `&&` method of the `Schedule` object, the `fiveTimes` schedule,
  // and the `everySecond` schedule, create a schedule that repeats fives times,
  // every second.
  //
  val fiveTimesEverySecond = ???

  //
  // EXERCISE 5
  //
  // Using the `repeat` method of the `IO` object, repeat the action
  // putStrLn("Hi hi") using `fiveTimesEverySecond`.
  //
  val repeated2 = ???

  //
  // EXERCISE 6
  //
  // Using the `andThen` method of the `Schedule` object, the `fiveTimes`
  // schedule, and the `everySecond` schedule, create a schedule that repeats
  // fives times rapidly, and then repeats every second forever.
  //
  val fiveTimesThenEverySecond = ???

  //
  // EXERCISE 7
  //
  // Using the `retry` method of the `IO` object, retry the following error
  // a total of five times.
  //
  val error1 = IO.fail("Uh oh!")
  val retried5 = error1 ?

  //
  // EXERCISE 8
  //
  // Using the `&&` method of the `Schedule` object, the `fiveTimes` schedule,
  // and the `everySecond` schedule, create a schedule that repeats the minimum
  // of five times and every second.
  //
  val fiveTimesOrEverySecond = ???

  //
  // EXERCISE 9
  //
  // Produce a jittered schedule that first does exponential spacing (starting
  // from 10 milliseconds), but then after the spacing reaches 60 seconds,
  // switches over to fixed spacing of 60 seconds between recurrences, but will
  // only do that for up to 100 times, and produce a list of the results.
  //
  def mySchedule[A]: Schedule[A, List[A]] =
    ???
}

object zio_interop {
  implicit class FixMe[A](a: A) {
    def ? = ???
  }

  import scala.concurrent.Future
  import scalaz.zio.interop.future._
  import scala.concurrent.ExecutionContext.Implicits.global

  //
  // EXERCISE 1
  //
  // Use `IO.fromFuture` method to convert the following `Future` into an `IO`.
  //
  val future1 = () => Future.successful("Hello World")
  val io1 = IO.fromFuture(???)(global)

  //
  // EXERCISE 2
  //
  // Use the `toFuture` method on `IO` to convert the following `io` to `Future`.
  //
  val io2: IO[Throwable, Int] = IO.point(42)
  val future2: IO[Nothing, Future[Int]] = io2 ?

  //
  // EXERCISE 3
  //
  // Use the Fiber.fromFuture` method to convert the following `Future` into
  // an `IO`.
  //
  val future3 = () => Future.failed[Int](new Error("Uh ohs!"))
  val fiber1: Fiber[Throwable, Int] = Fiber.fromFuture(???)(global)

  import scalaz.zio.interop.Task
  import scalaz.zio.interop.catz._
  import cats.effect.concurrent.Ref

  //
  // EXERCISE 4
  //
  // The following example uses the `Ref` from `cats-effect`, demonstrating how
  // `cats-effect` structures work with ZIO.
  //
  class Worker(number: Int, ref: Ref[Task, Int]) {
    def work: Task[Unit] =
      for {
        c1 <- ref.get
        _  <- putStrLn(s"#$number >> $c1")
        c2 <- ref.modify(x => (x + 1, x))
        _  <- putStrLn(s"#$number >> $c2")
      } yield ()
  }

  val program: Task[Unit] =
    for {
      ref <- Ref.of[Task, Int](0)
      w1  = new Worker(1, ref)
      w2  = new Worker(2, ref)
      w3  = new Worker(3, ref)
      f   <- IO.forkAll(List(w1.work, w2.work, w3.work))
      _   <- f.join
    } yield ()
}

object zio_ref {

}

object zio_promise {

}

object zio_queue {

}

object zio_rts {

}

object zio_advanced {

}

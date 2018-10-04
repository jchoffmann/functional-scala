// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.applications

import scalaz.zio._
import scalaz.zio.console._
import scalaz.zio.interop.scalaz72._

import scalaz._
import Scalaz._

object exercises extends App {
  implicit class FixMe[A](a: A) {
    def ? = ???
  }

  case class CrawlState[E, A](visited: Set[URL], crawl: Crawl[E, A])
  object CrawlState {
    def visited[E: Monoid, A: Monoid](visited: Set[URL]): CrawlState[E, A] =
      CrawlState(visited, mzero[Crawl[E, A]])
    def crawled[E, A](crawl: Crawl[E, A]): CrawlState[E, A] =
      CrawlState(mzero[Set[URL]], crawl)

    implicit def MonoidCrawlState[E: Monoid, A: Monoid]: Monoid[CrawlState[E, A]] =
      new Monoid[CrawlState[E, A]] {
        def zero = CrawlState(mzero[Set[URL]], mzero[Crawl[E, A]])
        def append(l: CrawlState[E, A], r: => CrawlState[E, A]) =
          CrawlState(l.visited |+| r.visited, l.crawl |+| r.crawl)
      }
  }
  //
  // EXERCISE 1
  //
  // Implement the `crawlIO` function.
  //
  def crawlIO[E: Monoid, A: Monoid]( // we want to combine errors and output => monoid; if run forever, don't accumulate, only stats etc.
                                     // could be report summary etc., but we don't need to know anything about the types => makes implementation simpler
    seeds     : Set[URL],
    router    : URL => Set[URL],     // can rewrite URLs, or limit to a subdomain
    processor : (URL, String) => IO[E, A]): IO[Nothing, Crawl[E, A]] = { // could do indexing, count words, ...
    type Acc = IO[Nothing, CrawlState[E, A]]

    def loop(seeds: Set[URL], acc: CrawlState[E, A]): Acc =
      seeds.foldLeft[Acc](IO.now(acc |+| CrawlState.visited(seeds))) {
        case (acc, seed) =>
          acc.flatMap(acc =>
            getURL(seed).redeem(
              err => IO.now(acc), // don't add to the accumulator if we fail (in turn this can never fail)
              html => {
                val seeds2 = extractURLs(seed, html).toSet.flatMap(router) -- acc.visited // extract URLs and feed them to the router, eliminate duplicates

                for {
                  crawl2  <-  processor(seed, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _))
                                                                                                       // IO[E, A] => IO[Exception, Crawl[E, A]], use attempt or redeempure
                                                                                                       // we're handling the error case using redeem, this computation cannot fail
                                                                                                       // converted a failure into a value
                  acc     <-  loop(seeds2, acc |+| CrawlState.crawled(crawl2))
                  // NBL shadowing is good as it reduces the amount of cards we can play
                } yield acc
              }
            )
          )
      }

    loop(seeds, mzero[CrawlState[E, A]]).map(_.crawl)
  }

  // Use traverse
  def crawlIO1Traverse[E: Monoid, A: Monoid](
    seeds     : Set[URL],
    router    : URL => Set[URL],
    processor : (URL, String) => IO[E, A]): IO[Nothing, Crawl[E, A]] = {
    def loop(seeds: Set[URL], acc: CrawlState[E, A]): IO[Nothing, CrawlState[E, A]] =
      IO.traverse(seeds) { seed =>
        getURL(seed).redeem(
          err  => IO.now(acc),
          html => {
            val seeds2 = extractURLs(seed, html).toSet.flatMap(router) -- acc.visited

            for {
              crawl2  <-  processor(seed, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _))
              acc     <-  loop(seeds2, acc |+| CrawlState.crawled(crawl2))
            } yield acc
          }
        )
      }.map(_.foldMap())

    loop(seeds, mzero[CrawlState[E, A]]).map(_.crawl)
  }
  // Traverse difference: BFS vs DFS
  // branches out from same node could visit same URL
  // but we are avoiding cycles, because they will be in visited, yeah? even if we visit twice

  // Use Retry
  import scala.concurrent.duration._
  val policy = Schedule.spaced(10.seconds).jittered && Schedule.recurs(5)
  def crawlIO1Retry[E: Monoid, A: Monoid](
    seeds     : Set[URL],
    router    : URL => Set[URL],
    processor : (URL, String) => IO[E, A]): IO[Nothing, Crawl[E, A]] = {
    def loop(seeds: Set[URL], acc: CrawlState[E, A]): IO[Nothing, CrawlState[E, A]] =
      IO.traverse(seeds) { seed =>
        getURL(seed).retry(policy *> Schedule.identity[Exception]).redeem( // *> and identity required to feed through the errors
          err  => IO.now(acc),
          html => {
            val seeds2 = extractURLs(seed, html).toSet.flatMap(router) -- acc.visited

            for {
              crawl2  <-  processor(seed, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _))
              acc     <-  loop(seeds2, acc |+| CrawlState.crawled(crawl2))
            } yield acc
          }
        )
      }.map(_.foldMap())

    loop(seeds, mzero[CrawlState[E, A]]).map(_.crawl)
  }

  // Use Ref
  def crawlIO1Ref[E: Monoid, A: Monoid](
    seeds     : Set[URL],
    router    : URL => Set[URL],
    processor : (URL, String) => IO[E, A]): IO[Nothing, Crawl[E, A]] = {
    def loop(seeds: Set[URL], ref: Ref[CrawlState[E, A]]): IO[Nothing, Unit] =
      ref.update(_ |+| CrawlState.visited(seeds)) *>
        (IO.traverse(seeds)(seed =>
        getURL(seed).redeem(
          err  => IO.unit,
          html =>
            for {
              acc   <- ref.get
              seeds <- IO.now(extractURLs(seed, html).toSet.flatMap(router) -- acc.visited)
              crawl <-  processor(seed, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _))
              _     <-  ref.update(_ |+| CrawlState.crawled(crawl))
              _     <- loop(seeds, ref)
          } yield ()
        )
      ): IO[Nothing, List[Unit]]).void

    for {
      ref   <- Ref(mzero[CrawlState[E, A]])
      _     <- loop(seeds, ref)
      state <- ref.get
    } yield state.crawl
  }
  // sequential code, before we smashed everything together with Semigroup's combine
  // strong guarantees around atomicity

  //
  // EXERCISE 2
  //
  // Implement a version of the `crawlIO` function that works in parallel.
  //
  def crawlIOPar[E: Monoid, A: Monoid](
    seeds     : Set[URL],
    router    : URL => Set[URL],
    processor : (URL, String) => IO[E, A]): IO[Nothing, Crawl[E, A]] = {
    def loop(seeds: Set[URL], ref: Ref[CrawlState[E, A]]): IO[Nothing, Unit] =
      ref.update(_ |+| CrawlState.visited(seeds)) *>
        (IO.parTraverse(seeds)(seed => // <== now we're doing everything in parallel
          getURL(seed).redeem(
            err  => IO.unit,
            html =>
              for {
                visited <- ref.get.map(_.visited)
                seeds   <- IO.now(extractURLs(seed, html).toSet.flatMap(router) -- visited)
                crawl   <-  processor(seed, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _))
                _       <-  ref.update(_ |+| CrawlState.crawled(crawl))
                _       <- loop(seeds, ref)
              } yield ()
          )
        ): IO[Nothing, List[Unit]]).void

    for {
      ref   <- Ref(mzero[CrawlState[E, A]])
      _     <- loop(seeds, ref)
      state <- ref.get
    } yield state.crawl
  }
  // Instead of passing in the state, you need to pass a ref of state


  //
  // EXERCISE 3
  //
  // Implement a version of the `crawlIOPar` that can be tested without having
  // to interact with the real world.
  //
  def crawlIO2[E: Monoid, A: Monoid](
    seeds     : Set[URL],
    router    : URL => Set[URL],
    processor : (URL, String) => IO[E, A],
    getURL    : URL => IO[Exception, String] = getURL(_)): IO[Nothing, Crawl[E, A]] = {
    def loop(seeds: Set[URL], ref: Ref[CrawlState[E, A]]): IO[Nothing, Unit] =
      ref.update(_ |+| CrawlState.visited(seeds)) *>
        (IO.parTraverse(seeds)(seed => // <== now we're doing everything in parallel
          putStrLn("Getting content for: " + seed).attempt.void *>
          getURL(seed).redeem(
            err  => putStrLn("Failed getting content for " + seed).attempt.void *> IO.unit,
            html =>
              for {
                visited <- ref.get.map(_.visited)
                _       <- putStrLn("Visited: " + visited).attempt.void
                _       <- putStrLn("Seeds: " + seeds).attempt.void
                seeds   <- IO.now(extractURLs(seed, html).toSet.flatMap(router) -- visited)
                crawl   <-  processor(seed, html).redeemPure(Crawl(_, mzero[A]), Crawl(mzero[E], _))
                _       <-  ref.update(_ |+| CrawlState.crawled(crawl))
                _       <- loop(seeds, ref)
              } yield ()
          )
        ): IO[Nothing, List[Unit]]).void

    for {
      ref   <- Ref(mzero[CrawlState[E, A]])
      _     <- loop(seeds, ref)
      state <- ref.get
    } yield state.crawl
  }
  // To make things testable: extract interactions with the real world into functions (getURL)
  // could also do extractURLs

  // Tagless final
  //
  // Abstracting over effects - what else could we choose apart from IO effects, i.e. what would other instances be for example?
  // ^ yeah, Monix, cats Effect, even Monad Transformer stacks

  //
  // EXERCISE 4
  //
  // Create a type class to describe `printLine` and `readLine`.
  //
  trait Console[F[_]] { // abstraction over a set of console effects
    def printLine(line: String): F[Unit]
    def readLine: F[String]
  }
  object Console {
    def apply[F[_]](implicit F: Console[F]): Console[F] = F
  }

  //
  // EXERCISE 5
  //
  // Implement helper methods called `printLine` and `readLine` that work with
  // any `F[_]` that supports the `Console` effect.
  //
  def printLine[F[_]: Console](line: String): F[Unit] = Console[F].printLine(line)
  def readLine[F[_]: Console]: F[String] = Console[F].readLine

  //
  // EXERCISE 6
  //
  // Create an instance of the `Console` type class for `IO[E, ?]` for any `E`
  // by using `IO.syncTheHoff` and the Scala functions `println` and `scala.io.StdIn.readLine`.
  //
  implicit def ConsoleIO[E]: Console[IO[E, ?]] =
    new Console[IO[E, ?]] {
      def printLine(line: String): IO[E, Unit] = IO.sync(println(line))

      def readLine: IO[E, String] = IO.sync(scala.io.StdIn.readLine)
    }

  //
  // EXERCISE 7
  //
  // Create an instance of the `Random` type class for `IO[E, ?]` for any `E`
  // by using `IO.sync` and `scala.util.Random.nextInt`.
  //
  trait Random[F[_]] {
    def nextInt(max: Int): F[Int]
  }
  object Random {
    def apply[F[_]](implicit F: Random[F]): Random[F] = F
  }
  def nextInt[F[_]: Random](max: Int): F[Int] = Random[F].nextInt(max)
  implicit def RandomIO[E]: Random[IO[E, ?]] =
    (max: Int) => IO.sync(scala.util.Random.nextInt(max))

  //
  // EXERCISE 8
  //
  // Create a game that is polymorphic in the effect type `F[_]`, requiring only
  // the capability to perform `Console` and `Random` effects.
  //
  def myGame[F[_]: Console: Random: Monad]: F[Unit] = // fully polymorphic program in F
    for {
      _    <- printLine[F]("Welcome to Purely Functional Hangman!")
      name <- getName[F]
      word <- chooseWord[F]
      state = State(name, Set(), word)
      _    <- renderState[F](state)
      _    <- gameLoop[F](state)
    } yield ()

  case class State(name: String, guesses: Set[Char], word: String) {
    def failures: Int = (guesses -- word.toSet).size

    def playerLost: Boolean = guesses.size > (word.length * 2)

    def playerWon: Boolean = (word.toSet -- guesses).isEmpty

    def correctChoice(c: Char): Boolean = word.toSet.contains(c)
  }

  def gameLoop[F[_]: Console: Monad](state: State): F[State] =
    for {
      choice <- getChoice[F]
      state  <- state.copy(guesses = state.guesses + choice).point[F]
      state  <- renderState[F](state) *>
                (if (state.playerLost)
                  printLine[F](s"You have been HANGED! Sorry, ${state.name}, please try again!") *> state.point[F]
                else if (state.playerWon)
                  printLine[F](s"Congratulations, you are a winner, ${state.name}!") *> state.point[F]
                else (if (state.correctChoice(choice))
                    printLine[F](s"You guessed correctly! Keep at it, ${state.name}!")
                  else
                  printLine[F](s"Your guess was wrong, but keep trying, ${state.name}!")) *> gameLoop[F](state))
    } yield state

  def renderState[F[_]: Console](state: State): F[Unit] = {
    //
    // f     n  c  t  o
    // -  -  -  -  -  -  -
    //
    //  Guesses: a, z, y, x
    val word =
      state.word.toList.map(c =>
        if (state.guesses.contains(c)) s" $c " else s"   ").mkString("")

    val line = List.fill(state.word.length)(" - ").mkString("")

    val guesses = state.guesses.mkString(" Guesses: ", ", ", "")

    val text = word + "\n" + line + "\n\n" + guesses + "\n"

    printLine[F](text)
  }

  def getChoice[F[_]: Console: Monad]: F[Char] = // we need to retry (user doesn't enter just a char), so recursion, so Monad (depends on runtime value)
    for {
      _     <- printLine[F]("Please enter a guess: ")
      guess <- readLine[F].flatMap { line0 =>
                 val line = line0.trim

                 line.headOption match {
                   case Some(choice) if choice.isLetter && line.length == 1 =>
                     choice.point[F]
                   case _ =>
                     printLine[F]("Your choice is not valid, please try again") *> getChoice[F]
                 }
               }
    } yield guess


  def getName[F[_]: Console: Apply]: F[String] = // maybe does two things but they don't depend on each other; can't lift something so String must come from Console
    printLine[F]("Please enter your name: ") *> readLine[F]

  def chooseWord[F[_]: Random: Functor]: F[String] = // functors only map, just 1 operation
    nextInt[F](Dictionary.length).map(Dictionary.apply)

  val Dictionary = List("aaron", "abelian", "ability", "about", "abstract", "abstract", "abstraction", "accurately", "adamek", "add", "adjacent", "adjoint", "adjunction", "adjunctions", "after", "after", "again", "ahrens", "albeit", "algebra", "algebra", "algebraic", "all", "all", "allegories", "almost", "already", "also", "american", "among", "amount", "ams", "an", "an", "analysis", "analytic", "and", "and", "andre", "any", "anyone", "apart", "apologetic", "appears", "applicability", "applications", "applications", "applied", "apply", "applying", "applying", "approach", "archetypical", "archetypical", "are", "areas", "argument", "arising", "aristotle", "arrowsmorphism", "article", "arxiv13026946", "arxiv13030584", "as", "as", "aspect", "assumed", "at", "attempts", "audience", "august", "awodey", "axiom", "axiomatic", "axiomatized", "axioms", "back", "barr", "barry", "basic", "basic", "be", "beginners", "beginning", "behind", "being", "benedikt", "benjamin", "best", "better", "between", "bicategories", "binary", "bodo", "book", "borceux", "both", "both", "bourbaki", "bowdoin", "brash", "brendan", "build", "built", "but", "but", "by", "called", "cambridge", "can", "cardinal", "carlos", "carnap", "case", "cases", "categorial", "categorical", "categorical", "categories", "categories", "categorification", "categorize", "category", "category", "cats", "catsters", "central", "certain", "changes", "charles", "cheng", "chicago", "chiefly", "chopin", "chris", "cite", "clash", "classes", "classical", "closed", "coend", "coin", "colimit", "colin", "collection", "collections", "comparing", "completion", "composed", "composition", "computational", "computer", "computing", "concept", "concepts", "concepts", "conceptual", "concrete", "confronted", "consideration", "considers", "consistently", "construction", "constructions", "content", "contents", "context", "context", "contexts", "continues", "continuous", "contrast", "contributed", "contributions", "cooper", "correctness", "costas", "count", "course", "cover", "covering", "current", "currently", "david", "decategorification", "deducing", "define", "defined", "defining", "definition", "definitions", "der", "derives", "described", "describing", "description", "descriptions", "detailed", "development", "dictum", "did", "different", "dimensions", "directed", "discovered", "discovery", "discuss", "discussed", "discussion", "discussion", "disparage", "disservice", "do", "does", "driving", "drossos", "duality", "dvi", "each", "easy", "ed", "edges", "edit", "edition", "eilenberg", "eilenbergmaclane", "elementary", "elementary", "elements", "elementwise", "elephant", "ellis", "else", "embedding", "embodiment", "embryonic", "emily", "end", "enthusiastic", "equations", "equivalence", "equivalences", "equivalences", "etc", "etcs", "eugenia", "even", "eventually", "everything", "evident", "example", "examples", "examples", "except", "excused", "exist", "exists", "exposure", "expressed", "expressiveness", "extension", "extra", "f", "fact", "fair", "families", "far", "feeds", "feeling", "finds", "finite", "first", "flourished", "focuses", "folklore", "follows", "fong", "for", "for", "force", "forced", "foremost", "form", "formalizes", "formulated", "forthcoming", "found", "foundation", "foundations", "foundations", "francis", "free", "freyd", "freydmitchell", "from", "functions", "functor", "functor", "functors", "fundamental", "further", "gabrielulmer", "general", "general", "generalized", "generalizes", "geometry", "geometry", "george", "geroch", "get", "gift", "give", "given", "going", "goldblatt", "grandis", "graph", "gray", "grothendieck", "ground", "group", "groupoid", "grp", "guide", "göttingen", "had", "handbook", "handful", "handle", "harper", "has", "have", "he", "here", "here", "herrlich", "higher", "higher", "higherdimensional", "highlevel", "hilberts", "his", "historical", "historically", "history", "history", "holistic", "holland", "home", "homomorphisms", "homotopy", "homotopy", "horizontal", "horst", "however", "i", "idea", "ideas", "ieke", "if", "if", "illustrated", "important", "in", "in", "inaccessible", "inadmissible", "include", "includes", "including", "indeed", "indexes", "infinite", "informal", "initial", "innocent", "instance", "instead", "instiki", "interacting", "internal", "intersection", "into", "introduce", "introduced", "introduces", "introducing", "introduction", "introduction", "introductory", "intuitions", "invitation", "is", "isbell", "isbn", "isomorphisms", "it", "it", "its", "itself", "ive", "j", "jaap", "jacob", "jiri", "johnstone", "joy", "jstor", "just", "kan", "kant", "kapulkin", "kashiwara", "kind", "kinds", "kleins", "kmorphisms", "ktransfors", "kℕ", "la", "lagatta", "lane", "language", "large", "last", "later", "later", "latest", "lauda", "lawvere", "lawveres", "lead", "leads", "least", "lectures", "led", "leinster", "lemma", "lemmas", "level", "library", "lifting", "likewise", "limit", "limits", "link", "linked", "links", "list", "literally", "logic", "logic", "logically", "logische", "long", "lurie", "mac", "maclane", "made", "major", "make", "manifest", "many", "many", "mappings", "maps", "marco", "masaki", "material", "mathct0305049", "mathematical", "mathematical", "mathematician", "mathematician", "mathematics", "mathematics", "mathematicsbrit", "may", "mclarty", "mclartythe", "means", "meet", "membership", "methods", "michael", "misleading", "mitchell", "models", "models", "moerdijk", "monad", "monadicity", "monographs", "monoid", "more", "morphisms", "most", "mostly", "motivation", "motivations", "much", "much", "music", "must", "myriads", "named", "natural", "natural", "naturally", "navigation", "ncategory", "necessary", "need", "never", "new", "nlab", "no", "no", "nocturnes", "nonconcrete", "nonsense", "nontechnical", "norman", "north", "northholland", "not", "notes", "notes", "nothing", "notion", "now", "npov", "number", "object", "objects", "obliged", "observation", "observing", "of", "on", "one", "online", "oosten", "operads", "opposed", "or", "order", "originally", "other", "other", "others", "out", "outside", "outside", "over", "packing", "page", "page", "pages", "paper", "paradigm", "pareigis", "parlance", "part", "particularly", "pdf", "pedagogical", "people", "perfect", "perhaps", "perpetrated", "perspective", "peter", "phenomenon", "phil", "philosopher", "philosophers", "philosophical", "philosophy", "physics", "physics", "pierce", "pierre", "played", "pleasure", "pointed", "poset", "possession", "power", "powered", "powerful", "pp", "preface", "prerequisite", "present", "preserving", "presheaf", "presheaves", "press", "prevail", "print", "probability", "problem", "proceedings", "process", "progression", "project", "proof", "property", "provide", "provides", "ps", "publicly", "published", "pure", "purloining", "purpose", "quite", "quiver", "rails", "rather", "reader", "realizations", "reason", "recalled", "record", "references", "reflect", "reflects", "rejected", "related", "related", "relation", "relation", "relations", "representable", "reprints", "reproduce", "resistance", "rests", "results", "reveals", "reverse", "revised", "revisions", "revisions", "rezk", "riehl", "robert", "role", "row", "ruby", "running", "same", "samuel", "saunders", "say", "scedrov", "schanuel", "schapira", "school", "sci", "science", "scientists", "search", "see", "see", "sense", "sep", "sequence", "serious", "set", "set", "sets", "sets", "sheaf", "sheaves", "shortly", "show", "shulman", "similar", "simon", "simple", "simplified", "simply", "simpson", "since", "single", "site", "situations", "sketches", "skip", "small", "so", "society", "some", "some", "sometimes", "sophisticated", "sophistication", "source", "space", "speak", "special", "specific", "specifically", "speculative", "spivak", "sprache", "stage", "standard", "statements", "steenrod", "stephen", "steps", "steve", "still", "stop", "strecker", "structural", "structuralism", "structure", "structures", "students", "study", "studying", "subjects", "such", "suggest", "summer", "supported", "supports", "symposium", "syntax", "tac", "taken", "talk", "tannaka", "tautological", "technique", "tend", "tends", "term", "terminology", "ternary", "tex", "textbook", "textbooks", "texts", "than", "that", "the", "the", "their", "their", "them", "themselves", "then", "theorem", "theorems", "theorems", "theoretic", "theoretical", "theories", "theorist", "theory", "theory", "there", "there", "these", "these", "they", "thinking", "this", "this", "thought", "through", "throughout", "thus", "time", "to", "tom", "tone", "too", "toolset", "top", "topics", "topoi", "topological", "topology", "topologyhomotopy", "topos", "topos", "toposes", "toposes", "transactions", "transformation", "transformations", "trinitarianism", "trinity", "triple", "triples", "trivial", "trivially", "true", "turns", "two", "two", "type", "typically", "uncountable", "under", "under", "understood", "unification", "unify", "unions", "univalent", "universal", "universal", "universes", "university", "use", "used", "useful", "using", "usual", "van", "variants", "various", "vast", "vect", "versatile", "video", "videos", "viewpoint", "views", "vol", "vol", "vs", "was", "way", "we", "wealth", "web", "wells", "were", "what", "when", "when", "where", "which", "while", "whole", "whose", "will", "willerton", "william", "willingness", "with", "witticism", "words", "working", "working", "would", "writes", "xfy", "xfygzxgfz", "xy", "yoneda", "york1964", "youtube")

  //
  // EXERCISE 9
  //
  // Instantiate the polymorphic game to the `IO[Nothing, ?]` type.
  //
  val myGameIO: IO[Nothing, Unit] = myGame[IO[Nothing, ?]]

  //
  // EXERCISE 10
  //
  // Create a test data structure that can contain a buffer of lines (to be
  // read from the console), a log of output (that has been written to the
  // console), and a list of "random" numbers.
  //
  case class TestData(output: List[String], input: List[String], random: List[Int]) {
    def renderOutput = "OUTPUT\n" + output.reverse.mkString("\n")
  }

  //
  // EXERCISE 11
  //
  // Implement the following dynamically-created instance. Effects should be
  // implemented in terms of modifying the passed in `Ref` that contains
  // `TestData`.
  //
  type GameEffects[F[_]] = Console[F] with Random[F] with Monad[F]
  final case class TestIO[+E, +A](run: IO[E, A])
  def createTestInstance[E](ref: Ref[TestData]): GameEffects[TestIO[E, ?]] = // You can mock out type classes using Ref
    new Console[TestIO[E, ?]] with Random[TestIO[E, ?]] with Monad[TestIO[E, ?]] {
      def point[A](a: => A): TestIO[E, A] =
        TestIO(IO.point(a))

      def bind[A, B](fa: TestIO[E, A])(f: A => TestIO[E, B]): TestIO[E, B] =
        TestIO(fa.run.flatMap(f.andThen(_.run)))

      def printLine(line: String): TestIO[E, Unit] =
        TestIO(ref.update(s => s.copy(output = line :: s.output)).void)

      def readLine: TestIO[E, String] =
        TestIO(ref.modify(s => (s.input.head, s.copy(input = s.input.drop(1)))))

      def nextInt(max: Int): TestIO[E, Int] =
        TestIO(ref.modify(s => (s.random.head, s.copy(random = s.random.drop(1)))))
    }

  //
  // EXERCISE 12
  //
  // Implement the following runner function, which will run the game using
  // the provided set of input test data.
  //
  def testRunner(testData: TestData): IO[Nothing, TestData] =
    for {
      ref  <- Ref(testData)
      _    <- {
                implicit val inst = createTestInstance(ref)

                myGame[TestIO[Nothing, ?]]
              }.run
      data <- ref.get
    } yield data

  //
  // EXERCISE 13
  //
  // Create some test data for a trial run of the game.
  //
  val GameTest1 = testRunner(TestData(
    input = List("John", "a", "r", "o", "n"),
    output = List(),
    random = List(0)
  )).map(_.renderOutput)


  final case class Crawl[E, A](error: E, value: A) { // Just a fancy name for Tuple2 with named args
    def leftMap[E2](f: E => E2): Crawl[E2, A] = Crawl(f(error), value)
    def map[A2](f: A => A2): Crawl[E, A2] = Crawl(error, f(value))
  }
  object Crawl {
    implicit def CrawlMonoid[E: Monoid, A: Monoid]: Monoid[Crawl[E, A]] =
      new Monoid[Crawl[E, A]]{
        def zero: Crawl[E, A] = Crawl(mzero[E], mzero[A])
        def append(l: Crawl[E, A], r: => Crawl[E, A]): Crawl[E, A] =
          Crawl(l.error |+| r.error, l.value |+| r.value)
      }
  }

  // Private ctor: you don't want users to be able to construct URL directly, could be invalid
//  final case class URL private (url: String) {
//    final def relative(page: String): Option[URL] = URL(url + "/" + page)
//  }
  // "Smart ctor", one way to make illegal states unrepresentable
//  object URL {
//    def apply(url: String): Option[URL] =
//      scala.util.Try(new java.net.URI(url).parseServerAuthority()).toOption match {
//        case None => None
//        case Some(_) => Some(new URL(url))
//      }
//  }
  // better solution: Use ADT
  // or this (import x): "io.lemonlabs" %% "scala-uri" % "1.3.1"
  final case class URL private (parsed: io.lemonlabs.uri.Url) {
    import io.lemonlabs.uri._

    final def relative(page: String): Option[URL] =
      scala.util.Try(parsed.path match {
        case Path(parts) =>
          val whole = parts.dropRight(1) :+ page.dropWhile(_ == '/')

          parsed.withPath(UrlPath(whole))
      }).toOption.map(new URL(_))

    def url: String = parsed.toString

    override def equals(a: Any): Boolean = a match {
      case that : URL => this.url == that.url
      case _          => false
    }

    override def hashCode: Int = url.hashCode
  }
  object URL {
    import io.lemonlabs.uri._

    def apply(url: String): Option[URL] =
      scala.util.Try(AbsoluteUrl.parse(url)).toOption match {
        case None => None
        case Some(parsed) => Some(new URL(parsed))
      }
  }

  // this is doing blocking IO, we shouldn't be doing this
  def getURLOld(url: URL): IO[Exception, String] =
    IO.syncException(scala.io.Source.fromURL(url.url)(scala.io.Codec.UTF8).mkString)
  //                 ^ blocking IO (better to use netty async or something)

  // perform the work in a separate thread or threadpool
  private val blockingPool = java.util.concurrent.Executors.newCachedThreadPool()
  def getURL(url: URL): IO[Exception, String] =
    for {
      promise <- Promise.make[Exception, String]
      _       <- (for {
                    exitResult <- IO.async[Nothing, ExitResult[Exception, String]](k => blockingPool.submit(
                                    new Runnable() {
                                      def run: Unit =
                                        try {
                                          k(ExitResult.Completed(ExitResult.Completed(scala.io.Source.fromURL(url.url)(scala.io.Codec.UTF8).mkString)))
                                        } catch {
                                          case e: Exception => k(ExitResult.Completed(ExitResult.Failed(e)))
                                        }
                                    }
                                  )) : IO[Nothing, ExitResult[Exception, String]]
                    _          <- promise.done(exitResult)
                 } yield ()).fork
      html    <- promise.get
    } yield html
  // soon zio will have a solution for this
  // without this, you would clog up your ZIO threadpool with all these http requests
  // other q: if you just do async, you can get rid of double wrapping, but next computation that will run after this will run on the blocking pool
  //          and it will continue with the block pool because k is called from the runnable, which is in the blocking pool?

  def extractURLs(root: URL, html: String): List[URL] = {
    val pattern = "href=[\"\']([^\"\']+)[\"\']".r

    scala.util.Try({
      val matches = (for (m <- pattern.findAllMatchIn(html)) yield m.group(1)).toList

      for {
        m   <- matches
        url <- URL(m).toList ++ root.relative(m).toList
      } yield url
    }).getOrElse(Nil)
  }

  // Test data
  object test {
    val Home          = URL("http://scalaz.org").get
    val Index         = URL("http://scalaz.org/index.html").get
    val ScaladocIndex = URL("http://scalaz.org/scaladoc/index.html").get
    val About         = URL("http://scalaz.org/about").get

    val SiteIndex =
      Map(
        Home          -> """<html><body><a href="index.html">Home</a><a href="/scaladoc/index.html">Scaladocs</a></body></html>""",
        Index         -> """<html><body><a href="index.html">Home</a><a href="/scaladoc/index.html">Scaladocs</a></body></html>""",
        ScaladocIndex -> """<html><body><a href="index.html">Home</a><a href="/about">About</a></body></html>""",
        About         -> """<html><body><a href="home.html">Home</a><a href="http://google.com">Google</a></body></html>"""
      )

    val getURL: URL => IO[Exception, String] =
      (url: URL) => SiteIndex.get(url).fold[IO[Exception, String]](IO.fail(new Exception("Could not connect to: " + url)))(IO.now)

    val Router: URL => Set[URL] =
      url => if (url.parsed.apexDomain.contains("scalaz.org")) Set(url) else Set()

    val Processor: (URL, String) => IO[Unit, List[(URL, String)]] =
      (url, html) => IO.now(List(url -> html))
  }

  def run(args: List[String]): IO[Nothing, ExitStatus] =
    (for {
//      _     <- putStrLn("Hello World!")

//      crawl <- crawlIO2(Set(test.Home), test.Router, test.Processor, test.getURL)
//      _     <- putStrLn(crawl.value.mkString("\n"))

//      _     <- myGameIO

      _       <- GameTest1.flatMap(putStrLn)
    } yield ()).redeemPure(_ => ExitStatus.ExitNow(1), _ => ExitStatus.ExitNow(0))
}

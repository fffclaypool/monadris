package monadris.infrastructure.terminal

import zio.*
import zio.test.*

import monadris.domain.Input
import monadris.infrastructure.terminal.TerminalInput.ParseResult
import monadris.infrastructure.terminal.TestServices as Mocks

object TerminalInputSpec extends ZIOSpecDefault:

  private object Keys:
    val Escape: Int        = 27
    val Bracket: Int       = '['.toInt
    val Unmapped: Int      = 'x'.toInt
    val UnknownArrow: Char = 'X'
    val NotABracket: Int   = 'O'.toInt

  private val keyBindings: List[(Char, Input)] = List(
    'h' -> Input.MoveLeft,
    'H' -> Input.MoveLeft,
    'l' -> Input.MoveRight,
    'L' -> Input.MoveRight,
    'j' -> Input.MoveDown,
    'J' -> Input.MoveDown,
    'k' -> Input.RotateClockwise,
    'K' -> Input.RotateClockwise,
    'z' -> Input.RotateCounterClockwise,
    'Z' -> Input.RotateCounterClockwise,
    ' ' -> Input.HardDrop,
    'p' -> Input.Pause,
    'P' -> Input.Pause
  )

  private val arrowBindings: List[(Char, Input)] = List(
    'A' -> Input.RotateClockwise,
    'B' -> Input.MoveDown,
    'C' -> Input.MoveRight,
    'D' -> Input.MoveLeft
  )

  private val unboundKeys: List[Char] = List('x', '1', '@')
  private val quitKeys: List[Char]    = List('q', 'Q')
  private val nonQuitKeys: List[Int]  = List('x'.toInt, 'h'.toInt, ' '.toInt, Keys.Escape)

  private def arrowSequence(code: Char): Chunk[Int] =
    Chunk(Keys.Escape, Keys.Bracket, code.toInt)

  /** Reads `count` keys from a terminal that has `typed` waiting */
  private def readKeys(typed: Chunk[Int], count: Int): Task[List[ParseResult]] =
    ZIO.replicateZIO(count)(TerminalInput.readKeyZIO).map(_.toList).provide(Mocks.tty(typed), Mocks.config)

  private def readKey(typed: Chunk[Int]): Task[ParseResult] =
    TerminalInput.readKeyZIO.provide(Mocks.tty(typed), Mocks.config)

  private def readsAsUnknown(typed: Chunk[Int]): Task[TestResult] =
    readKey(typed).map(result => assertTrue(result == ParseResult.Unknown))

  /** Parses what follows an escape key that has already been consumed */
  private def parseAfterEscape(rest: Chunk[Int]): Task[Option[Input]] =
    TerminalInput.parseEscapeSequenceZIO.provide(Mocks.tty(rest), Mocks.config)

  private def keyMappingSuite = suite("key mapping")(
    test("keyToInput maps every bound key in both cases") {
      val mapped = keyBindings.map((key, _) => TerminalInput.keyToInput(key.toInt))
      assertTrue(mapped == keyBindings.map((_, input) => Some(input)))
    },
    test("keyToInput returns None for unbound keys") {
      assertTrue(unboundKeys.flatMap(key => TerminalInput.keyToInput(key.toInt)).isEmpty)
    },
    test("arrowToInput maps the four arrow codes") {
      val mapped = arrowBindings.map((code, _) => TerminalInput.arrowToInput(code.toInt))
      assertTrue(mapped == arrowBindings.map((_, input) => Some(input)))
    },
    test("arrowToInput returns None for an unknown code") {
      assertTrue(TerminalInput.arrowToInput(Keys.UnknownArrow.toInt).isEmpty)
    },
    test("isQuitKey accepts q in both cases only") {
      assertTrue(
        quitKeys.forall(key => TerminalInput.isQuitKey(key.toInt)),
        !nonQuitKeys.exists(TerminalInput.isQuitKey)
      )
    },
    test("EscapeKeyCode is the ASCII escape character") {
      assertTrue(TerminalInput.EscapeKeyCode == Keys.Escape)
    }
  )

  private def toInputSuite = suite("toInput")(
    test("extracts the input of an Arrow result") {
      assertTrue(TerminalInput.toInput(ParseResult.Arrow(Input.MoveLeft)) == Some(Input.MoveLeft))
    },
    test("maps a Regular result through the key bindings") {
      val mapped = keyBindings.map((key, _) => TerminalInput.toInput(ParseResult.Regular(key.toInt)))
      assertTrue(mapped == keyBindings.map((_, input) => Some(input)))
    },
    test("returns None for a Regular result with an unbound key") {
      assertTrue(TerminalInput.toInput(ParseResult.Regular(Keys.Unmapped)).isEmpty)
    },
    test("returns None for Timeout and Unknown") {
      assertTrue(
        TerminalInput.toInput(ParseResult.Timeout).isEmpty,
        TerminalInput.toInput(ParseResult.Unknown).isEmpty
      )
    }
  )

  private def readKeySuite = suite("readKeyZIO")(
    test("returns Timeout when no input is available") {
      readKey(Chunk.empty).map(result => assertTrue(result == ParseResult.Timeout))
    },
    test("returns Regular for a plain key, bound or not") {
      val typed = Chunk('h'.toInt, Keys.Unmapped)
      readKeys(typed, typed.size).map(results => assertTrue(results == typed.toList.map(ParseResult.Regular(_))))
    },
    test("returns Arrow for every arrow sequence") {
      ZIO
        .foreach(arrowBindings)((code, _) => readKey(arrowSequence(code)))
        .map(results => assertTrue(results == arrowBindings.map((_, input) => ParseResult.Arrow(input))))
    },
    test("returns Timeout once the typed keys are used up") {
      val typed = Chunk('a'.toInt, 'b'.toInt, 'c'.toInt)
      readKeys(typed, typed.size + 1).map { results =>
        assertTrue(results == typed.toList.map(ParseResult.Regular(_)) :+ ParseResult.Timeout)
      }
    },
    test("reads plain keys and escape sequences in the order they were typed") {
      val typed    = ('h'.toInt +: arrowSequence('A')) ++ arrowSequence('D') :+ 'q'.toInt
      val expected = List(
        ParseResult.Regular('h'.toInt),
        ParseResult.Arrow(Input.RotateClockwise),
        ParseResult.Arrow(Input.MoveLeft),
        ParseResult.Regular('q'.toInt)
      )
      readKeys(typed, expected.size).map(results => assertTrue(results == expected))
    }
  )

  private def brokenSequenceSuite = suite("readKeyZIO with a broken escape sequence")(
    test("a lone escape key is Unknown") {
      readsAsUnknown(Chunk(Keys.Escape))
    },
    test("an escape followed by something other than a bracket is Unknown") {
      readsAsUnknown(Chunk(Keys.Escape, Keys.NotABracket))
    },
    test("an escape and bracket with nothing after them is Unknown") {
      readsAsUnknown(Chunk(Keys.Escape, Keys.Bracket))
    },
    test("an unknown arrow code is Unknown") {
      readsAsUnknown(arrowSequence(Keys.UnknownArrow))
    }
  )

  private def parseEscapeSequenceSuite = suite("parseEscapeSequenceZIO")(
    test("returns None when nothing follows the escape key") {
      parseAfterEscape(Chunk.empty).map(result => assertTrue(result.isEmpty))
    },
    test("returns None when the next byte is not a bracket") {
      parseAfterEscape(Chunk(Keys.NotABracket)).map(result => assertTrue(result.isEmpty))
    },
    test("returns the input of a complete arrow sequence") {
      parseAfterEscape(Chunk(Keys.Bracket, 'D'.toInt)).map(result => assertTrue(result == Some(Input.MoveLeft)))
    }
  )

  def spec = suite("TerminalInput")(
    keyMappingSuite,
    toInputSuite,
    readKeySuite,
    brokenSequenceSuite,
    parseEscapeSequenceSuite
  )

package monadris.effect

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import monadris.domain.Input

class TerminalInputSpec extends AnyFlatSpec with Matchers:

  // ============================================================
  // keyToInput tests
  // ============================================================

  "TerminalInput.keyToInput" should "return MoveLeft for 'h'" in {
    TerminalInput.keyToInput('h'.toInt) shouldBe Some(Input.MoveLeft)
  }

  it should "return MoveLeft for 'H'" in {
    TerminalInput.keyToInput('H'.toInt) shouldBe Some(Input.MoveLeft)
  }

  it should "return MoveRight for 'l'" in {
    TerminalInput.keyToInput('l'.toInt) shouldBe Some(Input.MoveRight)
  }

  it should "return MoveRight for 'L'" in {
    TerminalInput.keyToInput('L'.toInt) shouldBe Some(Input.MoveRight)
  }

  it should "return MoveDown for 'j'" in {
    TerminalInput.keyToInput('j'.toInt) shouldBe Some(Input.MoveDown)
  }

  it should "return MoveDown for 'J'" in {
    TerminalInput.keyToInput('J'.toInt) shouldBe Some(Input.MoveDown)
  }

  it should "return RotateClockwise for 'k'" in {
    TerminalInput.keyToInput('k'.toInt) shouldBe Some(Input.RotateClockwise)
  }

  it should "return RotateClockwise for 'K'" in {
    TerminalInput.keyToInput('K'.toInt) shouldBe Some(Input.RotateClockwise)
  }

  it should "return RotateCounterClockwise for 'z'" in {
    TerminalInput.keyToInput('z'.toInt) shouldBe Some(Input.RotateCounterClockwise)
  }

  it should "return RotateCounterClockwise for 'Z'" in {
    TerminalInput.keyToInput('Z'.toInt) shouldBe Some(Input.RotateCounterClockwise)
  }

  it should "return HardDrop for space" in {
    TerminalInput.keyToInput(' '.toInt) shouldBe Some(Input.HardDrop)
  }

  it should "return Pause for 'p'" in {
    TerminalInput.keyToInput('p'.toInt) shouldBe Some(Input.Pause)
  }

  it should "return Pause for 'P'" in {
    TerminalInput.keyToInput('P'.toInt) shouldBe Some(Input.Pause)
  }

  it should "return None for unknown keys" in {
    TerminalInput.keyToInput('x'.toInt) shouldBe None
    TerminalInput.keyToInput('1'.toInt) shouldBe None
    TerminalInput.keyToInput('@'.toInt) shouldBe None
  }

  // ============================================================
  // isQuitKey tests
  // ============================================================

  "TerminalInput.isQuitKey" should "return true for 'q'" in {
    TerminalInput.isQuitKey('q'.toInt) shouldBe true
  }

  it should "return true for 'Q'" in {
    TerminalInput.isQuitKey('Q'.toInt) shouldBe true
  }

  it should "return false for other keys" in {
    TerminalInput.isQuitKey('x'.toInt) shouldBe false
    TerminalInput.isQuitKey('h'.toInt) shouldBe false
    TerminalInput.isQuitKey(' '.toInt) shouldBe false
    TerminalInput.isQuitKey(27) shouldBe false
  }

  // ============================================================
  // EscapeKeyCode constant tests
  // ============================================================

  "TerminalInput.EscapeKeyCode" should "be 27" in {
    TerminalInput.EscapeKeyCode shouldBe 27
  }

  // ============================================================
  // toInput tests
  // ============================================================

  "TerminalInput.toInput" should "extract input from Arrow result" in {
    val result = TerminalInput.ParseResult.Arrow(Input.MoveLeft)
    TerminalInput.toInput(result) shouldBe Some(Input.MoveLeft)
  }

  it should "convert Regular result using keyToInput" in {
    val result = TerminalInput.ParseResult.Regular('h'.toInt)
    TerminalInput.toInput(result) shouldBe Some(Input.MoveLeft)
  }

  it should "return None for Regular with unknown key" in {
    val result = TerminalInput.ParseResult.Regular('x'.toInt)
    TerminalInput.toInput(result) shouldBe None
  }

  it should "return None for Timeout" in {
    TerminalInput.toInput(TerminalInput.ParseResult.Timeout) shouldBe None
  }

  it should "return None for Unknown" in {
    TerminalInput.toInput(TerminalInput.ParseResult.Unknown) shouldBe None
  }

  // ============================================================
  // ParseResult enum coverage
  // ============================================================

  "TerminalInput.ParseResult" should "have all expected variants" in {
    // Just verify the enum variants exist
    val arrow = TerminalInput.ParseResult.Arrow(Input.MoveLeft)
    val regular = TerminalInput.ParseResult.Regular(65)
    val timeout = TerminalInput.ParseResult.Timeout
    val unknown = TerminalInput.ParseResult.Unknown

    arrow shouldBe a[TerminalInput.ParseResult.Arrow]
    regular shouldBe a[TerminalInput.ParseResult.Regular]
    timeout shouldBe TerminalInput.ParseResult.Timeout
    unknown shouldBe TerminalInput.ParseResult.Unknown
  }

  // ============================================================
  // Edge cases for key mappings
  // ============================================================

  "TerminalInput key mappings" should "handle all vim-style keys" in {
    // h, j, k, l for movement
    TerminalInput.keyToInput('h'.toInt).isDefined shouldBe true
    TerminalInput.keyToInput('j'.toInt).isDefined shouldBe true
    TerminalInput.keyToInput('k'.toInt).isDefined shouldBe true
    TerminalInput.keyToInput('l'.toInt).isDefined shouldBe true
  }

  it should "handle uppercase vim-style keys" in {
    TerminalInput.keyToInput('H'.toInt).isDefined shouldBe true
    TerminalInput.keyToInput('J'.toInt).isDefined shouldBe true
    TerminalInput.keyToInput('K'.toInt).isDefined shouldBe true
    TerminalInput.keyToInput('L'.toInt).isDefined shouldBe true
  }

  it should "return correct inputs for all mapped keys" in {
    val mappings = List(
      ('h', Input.MoveLeft),
      ('H', Input.MoveLeft),
      ('l', Input.MoveRight),
      ('L', Input.MoveRight),
      ('j', Input.MoveDown),
      ('J', Input.MoveDown),
      ('k', Input.RotateClockwise),
      ('K', Input.RotateClockwise),
      ('z', Input.RotateCounterClockwise),
      ('Z', Input.RotateCounterClockwise),
      (' ', Input.HardDrop),
      ('p', Input.Pause),
      ('P', Input.Pause)
    )

    mappings.foreach { case (key, expected) =>
      TerminalInput.keyToInput(key.toInt) shouldBe Some(expected)
    }
  }

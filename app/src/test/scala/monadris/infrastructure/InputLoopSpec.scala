package monadris.infrastructure

import monadris.domain.Input

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InputLoopSpec extends AnyFlatSpec with Matchers:

  // ============================================================
  // keyToInput tests
  // ============================================================

  "InputLoop.keyToInput" should "return MoveLeft for 'h'" in {
    InputLoop.keyToInput('h'.toInt) shouldBe Some(Input.MoveLeft)
  }

  it should "return MoveLeft for 'H'" in {
    InputLoop.keyToInput('H'.toInt) shouldBe Some(Input.MoveLeft)
  }

  it should "return MoveRight for 'l'" in {
    InputLoop.keyToInput('l'.toInt) shouldBe Some(Input.MoveRight)
  }

  it should "return MoveRight for 'L'" in {
    InputLoop.keyToInput('L'.toInt) shouldBe Some(Input.MoveRight)
  }

  it should "return MoveDown for 'j'" in {
    InputLoop.keyToInput('j'.toInt) shouldBe Some(Input.MoveDown)
  }

  it should "return MoveDown for 'J'" in {
    InputLoop.keyToInput('J'.toInt) shouldBe Some(Input.MoveDown)
  }

  it should "return RotateClockwise for 'k'" in {
    InputLoop.keyToInput('k'.toInt) shouldBe Some(Input.RotateClockwise)
  }

  it should "return RotateClockwise for 'K'" in {
    InputLoop.keyToInput('K'.toInt) shouldBe Some(Input.RotateClockwise)
  }

  it should "return RotateCounterClockwise for 'z'" in {
    InputLoop.keyToInput('z'.toInt) shouldBe Some(Input.RotateCounterClockwise)
  }

  it should "return RotateCounterClockwise for 'Z'" in {
    InputLoop.keyToInput('Z'.toInt) shouldBe Some(Input.RotateCounterClockwise)
  }

  it should "return HardDrop for space" in {
    InputLoop.keyToInput(' '.toInt) shouldBe Some(Input.HardDrop)
  }

  it should "return Pause for 'p'" in {
    InputLoop.keyToInput('p'.toInt) shouldBe Some(Input.Pause)
  }

  it should "return Pause for 'P'" in {
    InputLoop.keyToInput('P'.toInt) shouldBe Some(Input.Pause)
  }

  it should "return None for unknown keys" in {
    InputLoop.keyToInput('x'.toInt) shouldBe None
    InputLoop.keyToInput('1'.toInt) shouldBe None
    InputLoop.keyToInput('@'.toInt) shouldBe None
  }

  // ============================================================
  // isQuitKey tests
  // ============================================================

  "InputLoop.isQuitKey" should "return true for 'q'" in {
    InputLoop.isQuitKey('q'.toInt) shouldBe true
  }

  it should "return true for 'Q'" in {
    InputLoop.isQuitKey('Q'.toInt) shouldBe true
  }

  it should "return false for other keys" in {
    InputLoop.isQuitKey('x'.toInt) shouldBe false
    InputLoop.isQuitKey('h'.toInt) shouldBe false
    InputLoop.isQuitKey(' '.toInt) shouldBe false
    InputLoop.isQuitKey(27) shouldBe false
  }

  // ============================================================
  // EscapeKeyCode constant tests
  // ============================================================

  "InputLoop.EscapeKeyCode" should "be 27" in {
    InputLoop.EscapeKeyCode shouldBe 27
  }

  // ============================================================
  // toInput tests
  // ============================================================

  "InputLoop.toInput" should "extract input from Arrow result" in {
    val result = InputLoop.ParseResult.Arrow(Input.MoveLeft)
    InputLoop.toInput(result) shouldBe Some(Input.MoveLeft)
  }

  it should "convert Regular result using keyToInput" in {
    val result = InputLoop.ParseResult.Regular('h'.toInt)
    InputLoop.toInput(result) shouldBe Some(Input.MoveLeft)
  }

  it should "return None for Regular with unknown key" in {
    val result = InputLoop.ParseResult.Regular('x'.toInt)
    InputLoop.toInput(result) shouldBe None
  }

  it should "return None for Timeout" in {
    InputLoop.toInput(InputLoop.ParseResult.Timeout) shouldBe None
  }

  it should "return None for Unknown" in {
    InputLoop.toInput(InputLoop.ParseResult.Unknown) shouldBe None
  }

  // ============================================================
  // ParseResult enum coverage
  // ============================================================

  "InputLoop.ParseResult" should "have all expected variants" in {
    // Just verify the enum variants exist
    val arrow   = InputLoop.ParseResult.Arrow(Input.MoveLeft)
    val regular = InputLoop.ParseResult.Regular(65)
    val timeout = InputLoop.ParseResult.Timeout
    val unknown = InputLoop.ParseResult.Unknown

    arrow shouldBe a[InputLoop.ParseResult.Arrow]
    regular shouldBe a[InputLoop.ParseResult.Regular]
    timeout shouldBe InputLoop.ParseResult.Timeout
    unknown shouldBe InputLoop.ParseResult.Unknown
  }

  // ============================================================
  // Edge cases for key mappings
  // ============================================================

  "InputLoop key mappings" should "handle all vim-style keys" in {
    // h, j, k, l for movement
    InputLoop.keyToInput('h'.toInt).isDefined shouldBe true
    InputLoop.keyToInput('j'.toInt).isDefined shouldBe true
    InputLoop.keyToInput('k'.toInt).isDefined shouldBe true
    InputLoop.keyToInput('l'.toInt).isDefined shouldBe true
  }

  it should "handle uppercase vim-style keys" in {
    InputLoop.keyToInput('H'.toInt).isDefined shouldBe true
    InputLoop.keyToInput('J'.toInt).isDefined shouldBe true
    InputLoop.keyToInput('K'.toInt).isDefined shouldBe true
    InputLoop.keyToInput('L'.toInt).isDefined shouldBe true
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
      InputLoop.keyToInput(key.toInt) shouldBe Some(expected)
    }
  }

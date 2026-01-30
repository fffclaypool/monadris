package monadris.input

import monadris.domain.Input

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class KeyMappingSpec extends AnyFlatSpec with Matchers:

  private object Keys:
    val VimLeft: Char           = 'h'
    val VimRight: Char          = 'l'
    val VimDown: Char           = 'j'
    val VimRotate: Char         = 'k'
    val RotateCCW: Char         = 'z'
    val Space: Char             = ' '
    val Pause: Char             = 'p'
    val Quit: Char              = 'q'
    val ArrowUp: Char           = 'A'
    val ArrowDown: Char         = 'B'
    val ArrowRight: Char        = 'C'
    val ArrowLeft: Char         = 'D'
    val UnmappedKey: Char       = 'x'
    val ExpectedEscapeCode: Int = 27

  "KeyMapping.EscapeKeyCode" should "be 27" in:
    KeyMapping.EscapeKeyCode shouldBe Keys.ExpectedEscapeCode

  "KeyMapping.keyToInput" should "map vim-style movement keys" in:
    KeyMapping.keyToInput(Keys.VimLeft.toInt) shouldBe Some(Input.MoveLeft)
    KeyMapping.keyToInput(Keys.VimRight.toInt) shouldBe Some(Input.MoveRight)
    KeyMapping.keyToInput(Keys.VimDown.toInt) shouldBe Some(Input.MoveDown)
    KeyMapping.keyToInput(Keys.VimRotate.toInt) shouldBe Some(Input.RotateClockwise)

  it should "map uppercase vim-style keys" in:
    KeyMapping.keyToInput(Keys.VimLeft.toUpper.toInt) shouldBe Some(Input.MoveLeft)
    KeyMapping.keyToInput(Keys.VimRight.toUpper.toInt) shouldBe Some(Input.MoveRight)
    KeyMapping.keyToInput(Keys.VimDown.toUpper.toInt) shouldBe Some(Input.MoveDown)
    KeyMapping.keyToInput(Keys.VimRotate.toUpper.toInt) shouldBe Some(Input.RotateClockwise)

  it should "map rotation keys" in:
    KeyMapping.keyToInput(Keys.RotateCCW.toInt) shouldBe Some(Input.RotateCounterClockwise)
    KeyMapping.keyToInput(Keys.RotateCCW.toUpper.toInt) shouldBe Some(Input.RotateCounterClockwise)

  it should "map space to HardDrop" in:
    KeyMapping.keyToInput(Keys.Space.toInt) shouldBe Some(Input.HardDrop)

  it should "map pause key" in:
    KeyMapping.keyToInput(Keys.Pause.toInt) shouldBe Some(Input.Pause)
    KeyMapping.keyToInput(Keys.Pause.toUpper.toInt) shouldBe Some(Input.Pause)

  it should "return None for unmapped keys" in:
    KeyMapping.keyToInput(Keys.UnmappedKey.toInt) shouldBe None
    KeyMapping.keyToInput('1'.toInt) shouldBe None

  "KeyMapping.isQuitKey" should "return true for quit keys" in:
    KeyMapping.isQuitKey(Keys.Quit.toInt) shouldBe true
    KeyMapping.isQuitKey(Keys.Quit.toUpper.toInt) shouldBe true

  it should "return false for non-quit keys" in:
    KeyMapping.isQuitKey(Keys.VimLeft.toInt) shouldBe false
    KeyMapping.isQuitKey(Keys.Space.toInt) shouldBe false

  "KeyMapping.arrowToInput" should "map arrow key codes to inputs" in:
    KeyMapping.arrowToInput(Keys.ArrowUp.toInt) shouldBe Some(Input.RotateClockwise)
    KeyMapping.arrowToInput(Keys.ArrowDown.toInt) shouldBe Some(Input.MoveDown)
    KeyMapping.arrowToInput(Keys.ArrowRight.toInt) shouldBe Some(Input.MoveRight)
    KeyMapping.arrowToInput(Keys.ArrowLeft.toInt) shouldBe Some(Input.MoveLeft)

  it should "return None for non-arrow codes" in:
    KeyMapping.arrowToInput(Keys.VimLeft.toInt) shouldBe None

  "KeyMapping.ParseResult" should "have all expected variants" in:
    val arrow   = KeyMapping.ParseResult.Arrow(Input.MoveLeft)
    val regular = KeyMapping.ParseResult.Regular(65)
    val timeout = KeyMapping.ParseResult.Timeout
    val unknown = KeyMapping.ParseResult.Unknown

    arrow shouldBe a[KeyMapping.ParseResult.Arrow]
    regular shouldBe a[KeyMapping.ParseResult.Regular]
    timeout shouldBe KeyMapping.ParseResult.Timeout
    unknown shouldBe KeyMapping.ParseResult.Unknown

  "KeyMapping.toInput" should "extract input from Arrow result" in:
    val result = KeyMapping.ParseResult.Arrow(Input.MoveLeft)
    KeyMapping.toInput(result) shouldBe Some(Input.MoveLeft)

  it should "convert Regular result using keyToInput" in:
    val result = KeyMapping.ParseResult.Regular(Keys.VimLeft.toInt)
    KeyMapping.toInput(result) shouldBe Some(Input.MoveLeft)

  it should "return None for Regular with unknown key" in:
    val result = KeyMapping.ParseResult.Regular(Keys.UnmappedKey.toInt)
    KeyMapping.toInput(result) shouldBe None

  it should "return None for Timeout" in:
    KeyMapping.toInput(KeyMapping.ParseResult.Timeout) shouldBe None

  it should "return None for Unknown" in:
    KeyMapping.toInput(KeyMapping.ParseResult.Unknown) shouldBe None

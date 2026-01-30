package monadris.input

import monadris.domain.Input

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GameCommandSpec extends AnyFlatSpec with Matchers:

  "GameCommand" should "have all expected variants" in:
    val userAction = GameCommand.UserAction(Input.MoveLeft)
    val timeTick   = GameCommand.TimeTick
    val quit       = GameCommand.Quit

    userAction shouldBe a[GameCommand.UserAction]
    timeTick shouldBe GameCommand.TimeTick
    quit shouldBe GameCommand.Quit

  "GameCommand.UserAction" should "wrap any Input type" in:
    val inputs = List(
      Input.MoveLeft,
      Input.MoveRight,
      Input.MoveDown,
      Input.RotateClockwise,
      Input.RotateCounterClockwise,
      Input.HardDrop,
      Input.Pause,
      Input.Tick
    )

    inputs.foreach { expectedInput =>
      val command = GameCommand.UserAction(expectedInput)
      command match
        case GameCommand.UserAction(actualInput) => actualInput shouldBe expectedInput
        case _                                   => fail("Should be UserAction")
    }

  it should "extract input correctly" in:
    val command = GameCommand.UserAction(Input.HardDrop)
    command match
      case GameCommand.UserAction(input) => input shouldBe Input.HardDrop
      case _                             => fail("Should match UserAction")

  "GameCommand.TimeTick" should "be a singleton" in:
    GameCommand.TimeTick shouldBe GameCommand.TimeTick

  "GameCommand.Quit" should "be a singleton" in:
    GameCommand.Quit shouldBe GameCommand.Quit

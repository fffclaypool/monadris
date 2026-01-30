package monadris.view

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AnsiRendererSpec extends AnyFlatSpec with Matchers:

  "AnsiRenderer.Codes" should "have correct ANSI escape sequences" in {
    AnsiRenderer.Codes.Reset shouldBe "\u001b[0m"
    AnsiRenderer.Codes.Cyan shouldBe "\u001b[36m"
    AnsiRenderer.Codes.Yellow shouldBe "\u001b[33m"
    AnsiRenderer.Codes.Magenta shouldBe "\u001b[35m"
    AnsiRenderer.Codes.Green shouldBe "\u001b[32m"
    AnsiRenderer.Codes.Red shouldBe "\u001b[31m"
    AnsiRenderer.Codes.Blue shouldBe "\u001b[34m"
    AnsiRenderer.Codes.White shouldBe "\u001b[37m"
  }

  it should "have correct cursor control sequences" in {
    AnsiRenderer.Codes.HideCursor shouldBe "\u001b[?25l"
    AnsiRenderer.Codes.ShowCursor shouldBe "\u001b[?25h"
    AnsiRenderer.Codes.Home shouldBe "\u001b[H"
    AnsiRenderer.Codes.ClearScreen shouldBe "\u001b[2J\u001b[3J"
    AnsiRenderer.Codes.ClearLine shouldBe "\u001b[2K"
  }

  it should "have correct newline sequence" in {
    AnsiRenderer.Codes.Newline shouldBe "\r\n"
  }

  "AnsiRenderer.Codes.moveTo" should "format cursor position correctly" in {
    AnsiRenderer.Codes.moveTo(1, 1) shouldBe "\u001b[1;1H"
    AnsiRenderer.Codes.moveTo(10, 20) shouldBe "\u001b[10;20H"
    AnsiRenderer.Codes.moveTo(5, 15) shouldBe "\u001b[5;15H"
  }

  "AnsiRenderer.colorToAnsi" should "return Cyan for UiColor.Cyan" in {
    AnsiRenderer.colorToAnsi(UiColor.Cyan) shouldBe AnsiRenderer.Codes.Cyan
  }

  it should "return Yellow for UiColor.Yellow" in {
    AnsiRenderer.colorToAnsi(UiColor.Yellow) shouldBe AnsiRenderer.Codes.Yellow
  }

  it should "return Magenta for UiColor.Magenta" in {
    AnsiRenderer.colorToAnsi(UiColor.Magenta) shouldBe AnsiRenderer.Codes.Magenta
  }

  it should "return Green for UiColor.Green" in {
    AnsiRenderer.colorToAnsi(UiColor.Green) shouldBe AnsiRenderer.Codes.Green
  }

  it should "return Red for UiColor.Red" in {
    AnsiRenderer.colorToAnsi(UiColor.Red) shouldBe AnsiRenderer.Codes.Red
  }

  it should "return Blue for UiColor.Blue" in {
    AnsiRenderer.colorToAnsi(UiColor.Blue) shouldBe AnsiRenderer.Codes.Blue
  }

  it should "return White for UiColor.White" in {
    AnsiRenderer.colorToAnsi(UiColor.White) shouldBe AnsiRenderer.Codes.White
  }

  it should "return Reset for UiColor.Default" in {
    AnsiRenderer.colorToAnsi(UiColor.Default) shouldBe AnsiRenderer.Codes.Reset
  }

  it should "handle all UiColor values" in
    UiColor.values.foreach { color =>
      AnsiRenderer.colorToAnsi(color) should not be empty
    }

  "AnsiRenderer.rowToString" should "convert simple row without color changes" in {
    val row    = Vector(Pixel('a', UiColor.Default), Pixel('b', UiColor.Default))
    val result = AnsiRenderer.rowToString(row)
    result shouldBe "ab"
  }

  it should "include color codes when color changes" in {
    val row    = Vector(Pixel('a', UiColor.Cyan), Pixel('b', UiColor.Cyan))
    val result = AnsiRenderer.rowToString(row)
    result should include(AnsiRenderer.Codes.Cyan)
    result should include("ab")
  }

  it should "add reset code at end when last color is not Default" in {
    val row    = Vector(Pixel('a', UiColor.Red))
    val result = AnsiRenderer.rowToString(row)
    result should endWith(AnsiRenderer.Codes.Reset)
  }

  it should "not add reset code when last color is Default" in {
    val row    = Vector(Pixel('a', UiColor.Red), Pixel('b', UiColor.Default))
    val result = AnsiRenderer.rowToString(row)
    result should endWith("b")
  }

  it should "handle multiple color transitions" in {
    val row = Vector(
      Pixel('r', UiColor.Red),
      Pixel('g', UiColor.Green),
      Pixel('b', UiColor.Blue)
    )
    val result = AnsiRenderer.rowToString(row)
    result should include(AnsiRenderer.Codes.Red)
    result should include(AnsiRenderer.Codes.Green)
    result should include(AnsiRenderer.Codes.Blue)
  }

  it should "handle empty row" in {
    val result = AnsiRenderer.rowToString(Vector.empty)
    result shouldBe ""
  }

  "AnsiRenderer.bufferToString" should "join rows with newline" in {
    val pixels = Vector(
      Vector(Pixel('a', UiColor.Default)),
      Vector(Pixel('b', UiColor.Default))
    )
    val buffer = ScreenBuffer(1, 2, pixels)
    val result = AnsiRenderer.bufferToString(buffer)
    result should include(AnsiRenderer.Codes.Newline)
  }

  it should "handle single row buffer" in {
    val buffer = ScreenBuffer(1, 1, Vector(Vector(Pixel('x', UiColor.Default))))
    val result = AnsiRenderer.bufferToString(buffer)
    result shouldBe "x"
  }

  it should "handle empty buffer" in {
    val buffer = ScreenBuffer(0, 0, Vector.empty)
    val result = AnsiRenderer.bufferToString(buffer)
    result shouldBe ""
  }

  "AnsiRenderer.computeDiffString" should "return empty for identical buffers" in {
    val pixel  = Pixel('x', UiColor.Default)
    val buffer = ScreenBuffer(1, 1, Vector(Vector(pixel)))
    val result = AnsiRenderer.computeDiffString(buffer, buffer)
    result shouldBe ""
  }

  it should "include move command for changed pixel" in {
    val current  = ScreenBuffer(1, 1, Vector(Vector(Pixel('a', UiColor.Default))))
    val previous = ScreenBuffer(1, 1, Vector(Vector(Pixel('b', UiColor.Default))))
    val result   = AnsiRenderer.computeDiffString(current, previous)
    result should include(AnsiRenderer.Codes.moveTo(1, 1))
    result should include("a")
  }

  it should "handle color changes in diff" in {
    val current  = ScreenBuffer(1, 1, Vector(Vector(Pixel('a', UiColor.Red))))
    val previous = ScreenBuffer(1, 1, Vector(Vector(Pixel('a', UiColor.Blue))))
    val result   = AnsiRenderer.computeDiffString(current, previous)
    result should include(AnsiRenderer.Codes.Red)
  }

  it should "handle buffer size differences" in {
    val current = ScreenBuffer(
      2,
      1,
      Vector(
        Vector(Pixel('a', UiColor.Default), Pixel('b', UiColor.Default))
      )
    )
    val previous = ScreenBuffer(
      1,
      1,
      Vector(
        Vector(Pixel('a', UiColor.Default))
      )
    )
    val result = AnsiRenderer.computeDiffString(current, previous)
    result should include("b")
  }

  it should "handle multiple changed pixels" in {
    val current = ScreenBuffer(
      2,
      1,
      Vector(
        Vector(Pixel('x', UiColor.Default), Pixel('y', UiColor.Default))
      )
    )
    val previous = ScreenBuffer(
      2,
      1,
      Vector(
        Vector(Pixel('a', UiColor.Default), Pixel('b', UiColor.Default))
      )
    )
    val result = AnsiRenderer.computeDiffString(current, previous)
    result should include("x")
    result should include("y")
  }

  it should "add reset at end when last color is not Default" in {
    val current  = ScreenBuffer(1, 1, Vector(Vector(Pixel('a', UiColor.Cyan))))
    val previous = ScreenBuffer(1, 1, Vector(Vector(Pixel('b', UiColor.Default))))
    val result   = AnsiRenderer.computeDiffString(current, previous)
    result should endWith(AnsiRenderer.Codes.Reset)
  }

  it should "handle previous buffer being larger" in {
    val current  = ScreenBuffer(1, 1, Vector(Vector(Pixel('a', UiColor.Default))))
    val previous = ScreenBuffer(
      2,
      2,
      Vector(
        Vector(Pixel('a', UiColor.Default), Pixel('b', UiColor.Default)),
        Vector(Pixel('c', UiColor.Default), Pixel('d', UiColor.Default))
      )
    )
    val result = AnsiRenderer.computeDiffString(current, previous)
    result shouldBe ""
  }

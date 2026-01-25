package monadris.view

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ViewModelSpec extends AnyFlatSpec with Matchers:

  val smallBufferWidth: Int  = 3
  val smallBufferHeight: Int = 2
  val mediumBufferSize: Int  = 5
  val largeBufferWidth: Int  = 10
  val largeBufferHeight: Int = 5

  val outOfBoundsNegative: Int = -1
  val outOfBoundsLarge: Int    = 10

  "UiColor" should "have all expected variants" in {
    UiColor.values should contain allOf (
      UiColor.Cyan,
      UiColor.Yellow,
      UiColor.Magenta,
      UiColor.Green,
      UiColor.Red,
      UiColor.Blue,
      UiColor.White,
      UiColor.Default
    )
  }

  "Pixel" should "store char and color" in {
    val pixel = Pixel('X', UiColor.Red)
    pixel.char shouldBe 'X'
    pixel.color shouldBe UiColor.Red
  }

  it should "use Default color by default" in {
    val pixel = Pixel('A')
    pixel.color shouldBe UiColor.Default
  }

  "ScreenBuffer.empty" should "create buffer with correct dimensions" in {
    val buffer = ScreenBuffer.empty(largeBufferWidth, largeBufferHeight)
    buffer.width shouldBe largeBufferWidth
    buffer.height shouldBe largeBufferHeight
  }

  it should "fill with space characters" in {
    val buffer = ScreenBuffer.empty(smallBufferWidth, smallBufferHeight)
    buffer.pixels.foreach { row =>
      row.foreach { pixel =>
        pixel.char shouldBe ' '
        pixel.color shouldBe UiColor.Default
      }
    }
  }

  it should "have correct number of rows and columns" in {
    val buffer = ScreenBuffer.empty(mediumBufferSize, smallBufferWidth)
    buffer.pixels.size shouldBe smallBufferWidth
    buffer.pixels.foreach(_.size shouldBe mediumBufferSize)
  }

  "ScreenBuffer.update" should "update pixel at valid position" in {
    val buffer  = ScreenBuffer.empty(mediumBufferSize, mediumBufferSize)
    val updateX = 2
    val updateY = 3
    val updated = buffer.update(updateX, updateY, Pixel('X', UiColor.Cyan))

    updated.pixels(updateY)(updateX).char shouldBe 'X'
    updated.pixels(updateY)(updateX).color shouldBe UiColor.Cyan
  }

  it should "not change other pixels" in {
    val buffer    = ScreenBuffer.empty(mediumBufferSize, mediumBufferSize)
    val updated   = buffer.update(2, 3, Pixel('X', UiColor.Cyan))
    val lastIndex = mediumBufferSize - 1

    updated.pixels(0)(0).char shouldBe ' '
    updated.pixels(lastIndex)(lastIndex).char shouldBe ' '
  }

  it should "ignore updates outside bounds (negative x)" in {
    val buffer  = ScreenBuffer.empty(mediumBufferSize, mediumBufferSize)
    val updated = buffer.update(outOfBoundsNegative, 2, Pixel('X'))

    updated shouldBe buffer
  }

  it should "ignore updates outside bounds (x >= width)" in {
    val buffer  = ScreenBuffer.empty(mediumBufferSize, mediumBufferSize)
    val updated = buffer.update(mediumBufferSize, 2, Pixel('X'))

    updated shouldBe buffer
  }

  it should "ignore updates outside bounds (negative y)" in {
    val buffer  = ScreenBuffer.empty(mediumBufferSize, mediumBufferSize)
    val updated = buffer.update(2, outOfBoundsNegative, Pixel('X'))

    updated shouldBe buffer
  }

  it should "ignore updates outside bounds (y >= height)" in {
    val buffer  = ScreenBuffer.empty(mediumBufferSize, mediumBufferSize)
    val updated = buffer.update(2, mediumBufferSize, Pixel('X'))

    updated shouldBe buffer
  }

  "ScreenBuffer.drawChar" should "draw character at position" in {
    val buffer  = ScreenBuffer.empty(mediumBufferSize, mediumBufferSize)
    val drawX   = 1
    val drawY   = 2
    val updated = buffer.drawChar(drawX, drawY, 'A', UiColor.Red)

    updated.pixels(drawY)(drawX).char shouldBe 'A'
    updated.pixels(drawY)(drawX).color shouldBe UiColor.Red
  }

  it should "use Default color when not specified" in {
    val buffer  = ScreenBuffer.empty(mediumBufferSize, mediumBufferSize)
    val drawX   = 1
    val drawY   = 2
    val updated = buffer.drawChar(drawX, drawY, 'A')

    updated.pixels(drawY)(drawX).color shouldBe UiColor.Default
  }

  "ScreenBuffer.drawText" should "draw text horizontally" in {
    val buffer  = ScreenBuffer.empty(largeBufferWidth, largeBufferHeight)
    val startX  = 2
    val rowY    = 1
    val text    = "Hello"
    val updated = buffer.drawText(startX, rowY, text, UiColor.Green)

    text.zipWithIndex.foreach { case (char, i) =>
      updated.pixels(rowY)(startX + i).char shouldBe char
    }
  }

  it should "apply color to all characters" in {
    val buffer  = ScreenBuffer.empty(largeBufferWidth, largeBufferHeight)
    val text    = "Hi"
    val updated = buffer.drawText(0, 0, text, UiColor.Blue)

    (0 until text.length).foreach { i =>
      updated.pixels(0)(i).color shouldBe UiColor.Blue
    }
  }

  it should "handle empty string" in {
    val buffer  = ScreenBuffer.empty(mediumBufferSize, mediumBufferSize)
    val updated = buffer.drawText(0, 0, "")

    updated shouldBe buffer
  }

  it should "clip text that extends beyond bounds" in {
    val buffer       = ScreenBuffer.empty(mediumBufferSize, mediumBufferSize)
    val startX       = 3
    val updated      = buffer.drawText(startX, 0, "Hello")
    val visibleChars = mediumBufferSize - startX

    updated.pixels(0)(startX).char shouldBe 'H'
    updated.pixels(0)(startX + 1).char shouldBe 'e'
  }

  "ScreenBuffer.getRow" should "return row at valid index" in {
    val rowSize  = 3
    val rowIndex = 1
    val buffer   = ScreenBuffer.empty(rowSize, rowSize).drawText(0, rowIndex, "ABC")
    val row      = buffer.getRow(rowIndex)

    row.size shouldBe rowSize
    row(0).char shouldBe 'A'
    row(1).char shouldBe 'B'
    row(2).char shouldBe 'C'
  }

  it should "return empty vector for negative index" in {
    val buffer = ScreenBuffer.empty(mediumBufferSize, mediumBufferSize)
    buffer.getRow(outOfBoundsNegative) shouldBe Vector.empty
  }

  it should "return empty vector for index >= height" in {
    val buffer = ScreenBuffer.empty(mediumBufferSize, mediumBufferSize)
    buffer.getRow(mediumBufferSize) shouldBe Vector.empty
    buffer.getRow(outOfBoundsLarge) shouldBe Vector.empty
  }

  "ScreenBuffer.drawPixels" should "draw multiple pixels at once" in {
    val buffer = ScreenBuffer.empty(largeBufferWidth, largeBufferHeight)
    val pixels = Vector(
      Pixel('A', UiColor.Red),
      Pixel('B', UiColor.Green),
      Pixel('C', UiColor.Blue)
    )
    val startX  = 2
    val rowY    = 1
    val updated = buffer.drawPixels(startX, rowY, pixels)

    updated.pixels(rowY)(startX).char shouldBe 'A'
    updated.pixels(rowY)(startX).color shouldBe UiColor.Red
    updated.pixels(rowY)(startX + 1).char shouldBe 'B'
    updated.pixels(rowY)(startX + 1).color shouldBe UiColor.Green
    updated.pixels(rowY)(startX + 2).char shouldBe 'C'
    updated.pixels(rowY)(startX + 2).color shouldBe UiColor.Blue
  }

  it should "not change buffer when drawing empty pixels" in {
    val buffer  = ScreenBuffer.empty(mediumBufferSize, mediumBufferSize)
    val updated = buffer.drawPixels(0, 0, Vector.empty)

    updated shouldBe buffer
  }

  it should "ignore draws outside vertical bounds" in {
    val buffer = ScreenBuffer.empty(mediumBufferSize, mediumBufferSize)
    val pixels = Vector(Pixel('X', UiColor.Red))

    val updatedNegative = buffer.drawPixels(0, outOfBoundsNegative, pixels)
    val updatedBeyond   = buffer.drawPixels(0, mediumBufferSize, pixels)

    updatedNegative shouldBe buffer
    updatedBeyond shouldBe buffer
  }

  it should "clip pixels that extend beyond right edge" in {
    val buffer = ScreenBuffer.empty(mediumBufferSize, mediumBufferSize)
    val startX = mediumBufferSize - 2
    val pixels = Vector(
      Pixel('A', UiColor.Red),
      Pixel('B', UiColor.Green),
      Pixel('C', UiColor.Blue),
      Pixel('D', UiColor.Yellow)
    )
    val updated = buffer.drawPixels(startX, 0, pixels)

    updated.pixels(0)(startX).char shouldBe 'A'
    updated.pixels(0)(startX + 1).char shouldBe 'B'
    updated.width shouldBe mediumBufferSize
  }

  it should "preserve other rows when drawing" in {
    val buffer = ScreenBuffer
      .empty(mediumBufferSize, mediumBufferSize)
      .drawText(0, 0, "First")
    val pixels  = Vector(Pixel('X', UiColor.Red), Pixel('Y', UiColor.Green))
    val updated = buffer.drawPixels(0, 1, pixels)

    updated.pixels(0)(0).char shouldBe 'F'
    updated.pixels(0)(1).char shouldBe 'i'
    updated.pixels(1)(0).char shouldBe 'X'
    updated.pixels(1)(1).char shouldBe 'Y'
  }

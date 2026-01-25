package monadris.view

enum UiColor:
  case Cyan
  case Yellow
  case Magenta
  case Green
  case Red
  case Blue
  case White
  case Default

final case class Pixel(char: Char, color: UiColor = UiColor.Default)

final case class ScreenBuffer(
  width: Int,
  height: Int,
  pixels: Vector[Vector[Pixel]]
):
  def update(x: Int, y: Int, pixel: Pixel): ScreenBuffer =
    if x >= 0 && x < width && y >= 0 && y < height then copy(pixels = pixels.updated(y, pixels(y).updated(x, pixel)))
    else this

  def drawChar(x: Int, y: Int, char: Char, color: UiColor = UiColor.Default): ScreenBuffer =
    update(x, y, Pixel(char, color))

  def drawText(x: Int, y: Int, text: String, color: UiColor = UiColor.Default): ScreenBuffer =
    if text.isEmpty then this
    else
      val newPixels = text.map(c => Pixel(c, color)).toVector
      drawPixels(x, y, newPixels)

  def drawPixels(x: Int, y: Int, newPixels: Vector[Pixel]): ScreenBuffer =
    if y >= 0 && y < height && newPixels.nonEmpty then
      val row        = pixels(y)
      val updatedRow = row.patch(x, newPixels, newPixels.length)
      val safeRow    = if updatedRow.length > width then updatedRow.take(width) else updatedRow
      copy(pixels = pixels.updated(y, safeRow))
    else this

  def getRow(y: Int): Vector[Pixel] =
    if y >= 0 && y < height then pixels(y)
    else Vector.empty

object ScreenBuffer:
  def empty(width: Int, height: Int): ScreenBuffer =
    val emptyRow = Vector.fill(width)(Pixel(' ', UiColor.Default))
    ScreenBuffer(width, height, Vector.fill(height)(emptyRow))

package monadris.view

/**
 * UI色を表す列挙型（ANSI制御コードから分離）
 */
enum UiColor:
  case Cyan
  case Yellow
  case Magenta
  case Green
  case Red
  case Blue
  case White
  case Default

/**
 * 画面上の1ピクセル（文字 + 色）
 */
final case class Pixel(char: Char, color: UiColor = UiColor.Default)

/**
 * 画面バッファ（純粋なデータ構造）
 * 描画ロジックとANSI制御を分離するための中間表現
 */
final case class ScreenBuffer(
  width: Int,
  height: Int,
  pixels: Vector[Vector[Pixel]]
):
  /**
   * 指定位置のピクセルを更新
   */
  def update(x: Int, y: Int, pixel: Pixel): ScreenBuffer =
    if x >= 0 && x < width && y >= 0 && y < height then
      copy(pixels = pixels.updated(y, pixels(y).updated(x, pixel)))
    else this

  /**
   * 指定位置に文字を描画（色付き）
   */
  def drawChar(x: Int, y: Int, char: Char, color: UiColor = UiColor.Default): ScreenBuffer =
    update(x, y, Pixel(char, color))

  /**
   * 指定位置からテキストを描画
   */
  def drawText(x: Int, y: Int, text: String, color: UiColor = UiColor.Default): ScreenBuffer =
    if text.isEmpty then this
    else
      val newPixels = text.map(c => Pixel(c, color)).toVector
      drawPixels(x, y, newPixels)

  /**
   * 指定位置から複数のピクセルを一括描画（効率化用）
   * Vector.patchを使用して要素ごとの再生成を回避
   */
  def drawPixels(x: Int, y: Int, newPixels: Vector[Pixel]): ScreenBuffer =
    if y >= 0 && y < height && newPixels.nonEmpty then
      val row = pixels(y)
      val updatedRow = row.patch(x, newPixels, newPixels.length)
      // 行の長さが変わらないように調整（念のため）
      val safeRow = if updatedRow.length > width then updatedRow.take(width) else updatedRow
      copy(pixels = pixels.updated(y, safeRow))
    else
      this

  /**
   * 指定行全体を取得
   */
  def getRow(y: Int): Vector[Pixel] =
    if y >= 0 && y < height then pixels(y)
    else Vector.empty

object ScreenBuffer:
  /**
   * 空のバッファを生成
   */
  def empty(width: Int, height: Int): ScreenBuffer =
    val emptyRow = Vector.fill(width)(Pixel(' ', UiColor.Default))
    ScreenBuffer(width, height, Vector.fill(height)(emptyRow))

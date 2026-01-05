package monadris.infrastructure

import java.io.FileInputStream

import zio.*

/**
 * ターミナル操作のシンプルなサービス
 *
 * 責務:
 * - Rawモード切替 (stty raw / stty cooked)
 * - /dev/tty からの non-blocking 読み取り
 *
 * 設計方針:
 * - sleep は ZIO.sleep を直接使用
 * - print は ZIO Console を直接使用
 * - このサービスはTTY固有の操作のみを提供
 */
trait Terminal:
  def available: Task[Int]
  def read: Task[Int]

object Terminal:
  // アクセサメソッド
  def available: ZIO[Terminal, Throwable, Int] =
    ZIO.serviceWithZIO(_.available)

  def read: ZIO[Terminal, Throwable, Int] =
    ZIO.serviceWithZIO(_.read)

  // Rawモード切替（スコープ付きリソース）
  def withRawMode[R, E, A](effect: ZIO[R, E, A]): ZIO[R, E | Throwable, A] =
    ZIO.acquireReleaseWith(enableRawMode)(_ => disableRawMode.ignore)(_ => effect)

  private def enableRawMode: Task[Unit] =
    ZIO.attemptBlocking {
      java.lang.Runtime.getRuntime.exec(Array("/bin/sh", "-c", "stty raw -echo < /dev/tty")).waitFor()
    }.unit

  private def disableRawMode: Task[Unit] =
    ZIO.attemptBlocking {
      java.lang.Runtime.getRuntime.exec(Array("/bin/sh", "-c", "stty cooked echo < /dev/tty")).waitFor()
    }.unit

  // Live実装 - /dev/tty を使用
  val live: ZLayer[Any, Nothing, Terminal] = ZLayer.scoped {
    for tty <- ZIO.acquireRelease(
        ZIO.attempt(new FileInputStream("/dev/tty")).orDie
      )(fis => ZIO.succeed(fis.close()))
    yield new Terminal:
      def available: Task[Int] = ZIO.attemptBlocking(tty.available())
      def read: Task[Int]      = ZIO.attemptBlocking(tty.read())
  }

  // テスト用実装
  def test(inputs: Chunk[Int]): ZLayer[Any, Nothing, Terminal] =
    ZLayer.fromZIO {
      for
        queue <- Queue.unbounded[Int]
        _     <- queue.offerAll(inputs)
      yield new Terminal:
        def available: Task[Int] = queue.size
        def read: Task[Int]      = queue.take
    }

package monadris.infrastructure

import java.io.FileInputStream

import zio.*

import monadris.config.ConfigLayer
import monadris.domain.config.AppConfig

/**
 * システムIOを抽象化するサービス定義
 * 本番用(live)実装のみを含む。テスト用実装は src/test に分離。
 */

// ============================================================
// TtyService - ターミナル入力の抽象化
// ============================================================

trait TtyService:
  def available(): Task[Int]
  def read(): Task[Int]
  def sleep(ms: Long): Task[Unit]

object TtyService:
  // アクセサメソッド
  def available(): ZIO[TtyService, Throwable, Int] =
    ZIO.serviceWithZIO(_.available())

  def read(): ZIO[TtyService, Throwable, Int] =
    ZIO.serviceWithZIO(_.read())

  def sleep(ms: Long): ZIO[TtyService, Throwable, Unit] =
    ZIO.serviceWithZIO(_.sleep(ms))

  // Live実装 - 実際の/dev/ttyを使用
  val live: ZLayer[Any, Nothing, TtyService] = ZLayer.scoped {
    for tty <- ZIO.acquireRelease(
        ZIO.attempt(new FileInputStream("/dev/tty")).orDie
      )(fis => ZIO.succeed(fis.close()))
    yield new TtyService:
      def available(): Task[Int]      = ZIO.attemptBlocking(tty.available())
      def read(): Task[Int]           = ZIO.attemptBlocking(tty.read())
      def sleep(ms: Long): Task[Unit] = ZIO.sleep(ms.millis)
  }

// ============================================================
// ConsoleService - コンソール出力の抽象化
// ============================================================

trait ConsoleService:
  def print(text: String): Task[Unit]
  def flush(): Task[Unit]

object ConsoleService:
  def print(text: String): ZIO[ConsoleService, Throwable, Unit] =
    ZIO.serviceWithZIO(_.print(text))

  def flush(): ZIO[ConsoleService, Throwable, Unit] =
    ZIO.serviceWithZIO(_.flush())

  // Live実装 - 実際のコンソール出力
  val live: ZLayer[Any, Nothing, ConsoleService] = ZLayer.succeed {
    new ConsoleService:
      def print(text: String): Task[Unit] = ZIO.attempt(scala.Console.print(text))
      def flush(): Task[Unit]             = ZIO.attempt(java.lang.System.out.flush())
  }

// ============================================================
// CommandService - シェルコマンド実行の抽象化
// ============================================================

trait CommandService:
  def exec(cmd: String): Task[Unit]

object CommandService:
  def exec(cmd: String): ZIO[CommandService, Throwable, Unit] =
    ZIO.serviceWithZIO(_.exec(cmd))

  // Live実装 - 実際のシェルコマンド実行
  val live: ZLayer[Any, Nothing, CommandService] = ZLayer.succeed {
    new CommandService:
      def exec(cmd: String): Task[Unit] = ZIO.attempt {
        java.lang.Runtime.getRuntime.exec(Array("/bin/sh", "-c", cmd)).waitFor()
      }.unit
  }

// ============================================================
// 複合環境型
// ============================================================

type GameEnv = TtyService & ConsoleService & CommandService & AppConfig

object GameEnv:
  val live: ZLayer[Any, Config.Error, GameEnv] =
    TtyService.live ++ ConsoleService.live ++ CommandService.live ++ ConfigLayer.live

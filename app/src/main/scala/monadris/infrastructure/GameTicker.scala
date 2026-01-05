package monadris.infrastructure

import zio.*

import monadris.domain.model.game.GameCommand

/**
 * 一定間隔でTickコマンドをQueueにプッシュするコンポーネント
 * レベルに応じて落下速度が動的に変化する
 */
object GameTicker:

  /**
   * Tickを生成し続けるプロデューサー
   * intervalRefを参照して動的に間隔を調整
   *
   * @param queue コマンドを送信するQueue
   * @param intervalRef 現在の落下間隔を保持するRef（レベル変化に応じて外部から更新される）
   */
  def producer(
    queue: Queue[GameCommand],
    intervalRef: Ref[Long]
  ): UIO[Nothing] =
    val tickAndSleep = for
      interval <- intervalRef.get
      _        <- ZIO.sleep(interval.millis)
      _        <- queue.offer(GameCommand.Tick)
    yield ()

    tickAndSleep.forever

  /**
   * Tickerファイバーを起動
   * 返されたFiberはゲーム終了時にinterruptする必要がある
   */
  def start(
    queue: Queue[GameCommand],
    intervalRef: Ref[Long]
  ): UIO[Fiber.Runtime[Nothing, Nothing]] =
    producer(queue, intervalRef).fork

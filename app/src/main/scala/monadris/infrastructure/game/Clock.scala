package monadris.infrastructure.game

import zio.*

import monadris.infrastructure.terminal.TtyService
import monadris.input.GameCommand

object Clock:

  def run(
    queue: Queue[GameCommand],
    intervalRef: Ref[Long]
  ): ZIO[TtyService, Throwable, Unit] =
    val tickAndSleep = for
      interval <- intervalRef.get
      _        <- TtyService.sleep(interval.toInt)
      _        <- queue.offer(GameCommand.TimeTick)
    yield ()

    tickAndSleep.forever

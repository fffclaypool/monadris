package monadris.infrastructure.runtime

import zio.*

import monadris.infrastructure.io.TtyService

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

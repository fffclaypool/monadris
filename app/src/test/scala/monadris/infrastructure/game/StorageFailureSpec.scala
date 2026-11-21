package monadris.infrastructure.game

import zio.*
import zio.test.*

import monadris.domain.TetrominoShape
import monadris.infrastructure.persistence.ReplayError
import monadris.infrastructure.persistence.ReplayRepository
import monadris.infrastructure.terminal.TerminalSession
import monadris.infrastructure.terminal.TestServices as LocalTestServices
import monadris.replay.ReplayData
import monadris.replay.ReplayMetadata

/**
 * Replay storage problems must be reported to the player and return to the menu
 * instead of failing the whole application.
 */
object StorageFailureSpec extends ZIOSpecDefault:

  private object Names:
    val Valid    = "run"
    val Existing = "taken"
    val Invalid  = "../evil"

  private object Errors:
    val MultiLine = "connection refused\n  Detail: host unreachable"
    val Flattened = "connection refused Detail: host unreachable"

  private val AnyKey  = ' '.toInt
  private val QuitKey = 'q'.toInt

  /** Terminal session that answers prompts with a fixed line and records everything shown */
  final private case class RecordingSession(shown: Ref[Vector[String]], promptAnswer: String) extends TerminalSession:
    def showMessage(message: String): Task[Unit]        = shown.update(_ :+ message)
    def prompt(message: String): Task[String]           = shown.update(_ :+ message).as(promptAnswer)
    def waitForKeypress(): Task[Int]                    = ZIO.succeed(AnyKey)
    def showMessageAndWait(message: String): Task[Unit] = shown.update(_ :+ message)
    def enableRawMode: Task[Unit]                       = ZIO.unit
    def disableRawMode: Task[Unit]                      = ZIO.unit

  private def storageFailure(cause: Throwable): IO[ReplayError, Nothing] =
    ZIO.fail(ReplayError.StorageFailure(cause))

  final private case class StubRepository(
    saved: Ref[Vector[String]],
    existing: Set[String] = Set.empty,
    listed: IO[ReplayError, Vector[String]] = ZIO.succeed(Vector.empty),
    failExists: Option[Throwable] = None,
    failSave: Option[Throwable] = None,
    failLoad: Option[Throwable] = None,
    stored: Map[String, ReplayData] = Map.empty
  ) extends ReplayRepository:
    def save(name: String, replay: ReplayData): IO[ReplayError, Unit] =
      failSave.fold(saved.update(_ :+ name))(storageFailure)
    def load(name: String): IO[ReplayError, ReplayData] =
      failLoad match
        case Some(error) => storageFailure(error)
        case None        => ZIO.fromOption(stored.get(name)).orElseFail(ReplayError.NotFound(name))
    def list: IO[ReplayError, Vector[String]]          = listed
    def exists(name: String): IO[ReplayError, Boolean] =
      failExists.fold(ZIO.succeed(existing.contains(name)))(storageFailure)
    def delete(name: String): IO[ReplayError, Unit] = ZIO.unit

  private val emptyReplay: ReplayData =
    ReplayData(
      metadata = ReplayMetadata(
        version = ReplayMetadata.CurrentVersion,
        timestamp = 0L,
        gridWidth = LocalTestServices.testConfig.grid.width,
        gridHeight = LocalTestServices.testConfig.grid.height,
        initialPiece = TetrominoShape.T,
        nextPiece = TetrominoShape.I,
        finalScore = 0,
        finalLevel = 1,
        finalLinesCleared = 0,
        durationMs = 0L
      ),
      events = Vector.empty
    )

  final private case class Outcome(result: Either[Throwable, Unit], shown: String, saved: Vector[String])

  private def run(
    effect: ZIO[monadris.infrastructure.terminal.GameEnv & ReplayRepository, Throwable, Unit],
    promptAnswer: String,
    makeRepository: Ref[Vector[String]] => StubRepository
  ): UIO[Outcome] =
    for
      shown  <- Ref.make(Vector.empty[String])
      saved  <- Ref.make(Vector.empty[String])
      result <- effect
        .provide(
          LocalTestServices.tty(Chunk(QuitKey)),
          LocalTestServices.console,
          LocalTestServices.command,
          LocalTestServices.config,
          ZLayer.succeed[TerminalSession](RecordingSession(shown, promptAnswer)),
          ZLayer.succeed[ReplayRepository](makeRepository(saved))
        )
        .either
      shownText  <- shown.get.map(_.mkString)
      savedNames <- saved.get
    yield Outcome(result, shownText, savedNames)

  def spec = suite("Replay storage failures")(
    suite("GameSession.playAndRecord")(
      test("rejects an invalid replay name before the game starts") {
        for outcome <- run(GameSession.playAndRecord, Names.Invalid, StubRepository(_))
        yield assertTrue(
          outcome.result.isRight,
          outcome.shown.contains("may only contain"),
          outcome.saved.isEmpty
        )
      },
      test("rejects a replay name that is already taken") {
        for outcome <- run(
            GameSession.playAndRecord,
            Names.Existing,
            StubRepository(_, existing = Set(Names.Existing))
          )
        yield assertTrue(
          outcome.result.isRight,
          outcome.shown.contains("already exists"),
          outcome.saved.isEmpty
        )
      },
      test("reports a failure to check the replay name") {
        for outcome <- run(
            GameSession.playAndRecord,
            Names.Valid,
            StubRepository(_, failExists = Some(new RuntimeException(Errors.MultiLine)))
          )
        yield assertTrue(
          outcome.result.isRight,
          outcome.shown.contains(s"Failed to check the replay name: ${Errors.Flattened}"),
          outcome.saved.isEmpty
        )
      },
      test("saves the replay under the validated name") {
        for outcome <- run(GameSession.playAndRecord, s"  ${Names.Valid}  ", StubRepository(_))
        yield assertTrue(
          outcome.result.isRight,
          outcome.saved == Vector(Names.Valid),
          outcome.shown.contains(s"Replay saved as: ${Names.Valid}")
        )
      },
      test("reports a failure to save the replay after the game") {
        for outcome <- run(
            GameSession.playAndRecord,
            Names.Valid,
            StubRepository(_, failSave = Some(new RuntimeException(Errors.MultiLine)))
          )
        yield assertTrue(
          outcome.result.isRight,
          outcome.shown.contains(s"Failed to save the replay: ${Errors.Flattened}"),
          !outcome.shown.contains("Replay saved as")
        )
      }
    ),
    suite("ReplaySelector")(
      test("watchReplay reports a failure to list replays") {
        for outcome <- run(
            ReplaySelector.watchReplay,
            "",
            StubRepository(_, listed = storageFailure(new RuntimeException(Errors.MultiLine)))
          )
        yield assertTrue(
          outcome.result.isRight,
          outcome.shown.contains(s"Failed to list replays: ${Errors.Flattened}")
        )
      },
      test("listReplays reports a failure without an error message by its type") {
        for outcome <- run(
            ReplaySelector.listReplays,
            "",
            StubRepository(_, listed = storageFailure(new IllegalStateException()))
          )
        yield assertTrue(
          outcome.result.isRight,
          outcome.shown.contains("Failed to list replays: IllegalStateException")
        )
      },
      test("watchReplay plays the selected replay when it loads") {
        for outcome <- run(
            ReplaySelector.watchReplay,
            "1",
            StubRepository(
              _,
              listed = ZIO.succeed(Vector(Names.Existing)),
              stored = Map(Names.Existing -> emptyReplay)
            )
          )
        yield assertTrue(
          outcome.result.isRight,
          outcome.shown.contains(s"Loading replay: ${Names.Existing}"),
          !outcome.shown.contains("Failed to")
        )
      },
      test("watchReplay reports a replay that no longer exists") {
        for outcome <- run(
            ReplaySelector.watchReplay,
            "1",
            StubRepository(_, listed = ZIO.succeed(Vector(Names.Existing)))
          )
        yield assertTrue(
          outcome.result.isRight,
          outcome.shown.contains(s"Failed to load the replay: Replay not found: ${Names.Existing}")
        )
      },
      test("watchReplay reports a failure to load the selected replay") {
        for outcome <- run(
            ReplaySelector.watchReplay,
            "1",
            StubRepository(
              _,
              listed = ZIO.succeed(Vector(Names.Existing)),
              failLoad = Some(new RuntimeException(Errors.MultiLine))
            )
          )
        yield assertTrue(
          outcome.result.isRight,
          outcome.shown.contains(s"Loading replay: ${Names.Existing}"),
          outcome.shown.contains(s"Failed to load the replay: ${Errors.Flattened}")
        )
      }
    )
  )

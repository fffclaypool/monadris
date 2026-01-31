package monadris.infrastructure.persistence

import zio.test.*

object PostgresReplayRepositorySpec extends ZIOSpecDefault:

  def spec = suite("PostgresReplayRepository")(
    suite("JsonbString")(
      test("creates JsonbString from String") {
        val json   = """{"key": "value"}"""
        val jsonb  = JsonbString(json)
        val result = jsonb.value

        assertTrue(result == json)
      },
      test("preserves JSON content") {
        val complexJson = """{"events":[{"type":"input","frame":0},{"type":"spawn","frame":1}]}"""
        val jsonb       = JsonbString(complexJson)

        assertTrue(jsonb.value == complexJson)
      },
      test("handles empty JSON object") {
        val emptyJson = "{}"
        val jsonb     = JsonbString(emptyJson)

        assertTrue(jsonb.value == emptyJson)
      },
      test("handles JSON array") {
        val jsonArray = """[1,2,3]"""
        val jsonb     = JsonbString(jsonArray)

        assertTrue(jsonb.value == jsonArray)
      },
      test("handles nested JSON") {
        val nestedJson = """{"outer":{"inner":{"deep":"value"}}}"""
        val jsonb      = JsonbString(nestedJson)

        assertTrue(jsonb.value == nestedJson)
      },
      test("handles JSON with special characters") {
        val jsonWithSpecial = """{"message":"Hello \"World\""}"""
        val jsonb           = JsonbString(jsonWithSpecial)

        assertTrue(jsonb.value == jsonWithSpecial)
      }
    ),
    suite("GameResultRow")(
      test("creates GameResultRow with all fields") {
        import java.time.OffsetDateTime
        import java.time.ZoneOffset
        import java.util.UUID

        val id  = UUID.randomUUID()
        val now = OffsetDateTime.now(ZoneOffset.UTC)
        val row = GameResultRow(
          id = id,
          replayName = "test-replay",
          timestamp = now,
          finalScore = 1000,
          finalLevel = 5,
          linesCleared = 20,
          durationMs = 60000L,
          gridWidth = 10,
          gridHeight = 20,
          version = "1.0",
          createdAt = now
        )

        assertTrue(
          row.id == id,
          row.replayName == "test-replay",
          row.finalScore == 1000,
          row.finalLevel == 5,
          row.linesCleared == 20,
          row.durationMs == 60000L,
          row.gridWidth == 10,
          row.gridHeight == 20,
          row.version == "1.0"
        )
      }
    ),
    suite("ReplayRow")(
      test("creates ReplayRow with JsonbString events") {
        import java.time.OffsetDateTime
        import java.time.ZoneOffset
        import java.util.UUID

        val id           = UUID.randomUUID()
        val gameResultId = UUID.randomUUID()
        val now          = OffsetDateTime.now(ZoneOffset.UTC)
        val eventsJson   = """[{"PlayerInput":{"input":"MoveLeft","frameNumber":0}}]"""

        val row = ReplayRow(
          id = id,
          gameResultId = gameResultId,
          initialPiece = "T",
          nextPiece = "I",
          events = JsonbString(eventsJson),
          eventCount = 1,
          createdAt = now
        )

        assertTrue(
          row.id == id,
          row.gameResultId == gameResultId,
          row.initialPiece == "T",
          row.nextPiece == "I",
          row.events.value == eventsJson,
          row.eventCount == 1
        )
      }
    )
  )

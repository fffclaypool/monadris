-- One replay per game result. The index backing this UNIQUE constraint also
-- serves foreign key lookups and ON DELETE CASCADE.
ALTER TABLE replays
    ADD CONSTRAINT replays_game_result_id_key UNIQUE (game_result_id);

-- created_at is always set by the application, so NULL is not allowed.
UPDATE game_results SET created_at = NOW() WHERE created_at IS NULL;
UPDATE replays SET created_at = NOW() WHERE created_at IS NULL;

ALTER TABLE game_results ALTER COLUMN created_at SET NOT NULL;
ALTER TABLE replays ALTER COLUMN created_at SET NOT NULL;

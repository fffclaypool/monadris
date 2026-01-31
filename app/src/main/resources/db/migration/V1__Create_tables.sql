-- game_results: ゲーム結果サマリー（軽量、検索用）
CREATE TABLE game_results (
    id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    replay_name     VARCHAR(255) UNIQUE NOT NULL,
    timestamp       TIMESTAMP WITH TIME ZONE NOT NULL,
    final_score     INTEGER NOT NULL,
    final_level     INTEGER NOT NULL,
    lines_cleared   INTEGER NOT NULL,
    duration_ms     BIGINT NOT NULL,
    grid_width      INTEGER NOT NULL,
    grid_height     INTEGER NOT NULL,
    version         VARCHAR(20) NOT NULL,
    created_at      TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- replays: リプレイイベント（JSONB格納）
CREATE TABLE replays (
    id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    game_result_id  UUID NOT NULL REFERENCES game_results(id) ON DELETE CASCADE,
    initial_piece   VARCHAR(10) NOT NULL,
    next_piece      VARCHAR(10) NOT NULL,
    events          JSONB NOT NULL,
    event_count     INTEGER NOT NULL,
    created_at      TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

CREATE INDEX idx_game_results_timestamp ON game_results(timestamp DESC);
CREATE INDEX idx_game_results_score ON game_results(final_score DESC);

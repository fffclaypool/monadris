-- 1つのゲーム結果に対するリプレイは1件だけ（UNIQUE制約のインデックスが外部キー検索・CASCADE削除にも使われる）
ALTER TABLE replays
    ADD CONSTRAINT replays_game_result_id_key UNIQUE (game_result_id);

-- created_at はアプリ側で必ず設定するため NULL を許可しない
UPDATE game_results SET created_at = NOW() WHERE created_at IS NULL;
UPDATE replays SET created_at = NOW() WHERE created_at IS NULL;

ALTER TABLE game_results ALTER COLUMN created_at SET NOT NULL;
ALTER TABLE replays ALTER COLUMN created_at SET NOT NULL;

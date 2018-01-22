local P = {}

P.name = "Instant CTalk"

-- 発動モード
--   0 - ただの *.wav ファイルドロップとして処理する
--   1 - 口パク準備オブジェクトも生成する。字幕用テキストを出力した場合は字幕ファイルも作成する
P.firemode = 1

-- ファイル名フォーマット
--   0 - こんにちは.wav
--   1 - 180116_172059_こんにちは.wav
--   2 - キャラ名_こんにちは.wav
--   3 - 180116_172059_キャラ名_こんにちは.wav
P.format = 3

function P.oninitmenu()
  return "Instant CTalk"
end

function P.onselect(index, state)
  local ret = require("ICTalk").open({parent=state.parent, format=P.format})
  if (ret ~= nil)and(ret.files ~= nil)and(#ret.files > 0) then
    local wave
    for i, v in ipairs(ret.files) do
      -- 作成したすべてのファイルは処理完了後に削除するように登録
      GCMZDrops.deleteonfinish(v)
      if v:match("[^.]+$"):lower() == "wav" then
        wave = v
      end
    end
    if wave ~= nil then
      -- wav 挿入時の発動モードを常に 0 で上書きし、
      -- シフトキーを押しながらドロップしたものとして投げることで wav 挿入処理を発動させる
      -- また Instant CTalk から出力したテキストファイルは常に Shift_JIS なので、その設定も上書きする
      state.shift = P.firemode == 1
      return {{filepath=wave, overridefiremode=0, overridetextencoding="sjis"}}, state
    end
  end
  return nil
end

return P

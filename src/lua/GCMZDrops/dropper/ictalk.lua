local P = {}

P.name = "Instant CTalk"

local wavP = require("psdtoolkit_wav")

function P.oninitmenu()
  return "Instant CTalk"
end

function P.onselect(index, state)
  local setting = wavP.loadsetting()
  local ret = require("ICTalk").open({parent=state.parent, format=setting.ictalk_format})
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
      state.shift = setting.ictalk_firemode == 1
      return {{filepath=wave, overridefiremode=0, overridesubtitleencoding="sjis"}}, state
    end
  end
  return nil
end

return P

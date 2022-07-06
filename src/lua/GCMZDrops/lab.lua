local P = {}

P.name = "LAB ファイルをインポート"

P.priority = 0

local wavP = require("psdtoolkit_wav")

function P.ondragenter(files, state)
  for i, v in ipairs(files) do
    if v.filepath:match("[^.]+$"):lower() == "lab" then
      -- ファイルの拡張子が lab のファイルがあったら処理できそうなので true
      return true
    end
  end
  return false
end

function P.ondragover(files, state)
  -- ondragenter で処理できそうなものは ondragover でも処理できそうなので調べず true
  return true
end

function P.ondragleave()
end

function P.parse(filepath)
  local line
  local f = io.open(filepath, "r")
  local r = {}
  local maxendf = 0
  for line in f:lines() do
    local st, ed, p = string.match(line, "([0-9.]+) ([0-9.]+) (.+)")
    if st == nil then
      return nil -- unexpected format
    end
    -- 秒単位に変換
    maxendf = ed/10000000
    table.insert(r, {s=st/10000000, e=maxendf, p=p})
  end
  f:close()
  return r, maxendf
end

function P.ondrop(files, state)
  local setting = wavP.loadsetting()
  for i, v in ipairs(files) do
    -- ファイルの拡張子が lab なら
    if v.filepath:match("[^.]+$"):lower() == "lab" then
      -- プロジェクトの情報を取得する
      local proj = GCMZDrops.getexeditfileinfo()
      -- lab ファイルを解析
      local lab, len = P.parse(v.filepath)

      local oini = GCMZDrops.inistring("")
      oini:set("exedit", "width", proj.width)
      oini:set("exedit", "height", proj.height)
      oini:set("exedit", "rate", proj.rate)
      oini:set("exedit", "scale", proj.scale)
      oini:set("exedit", "length", math.floor(len * proj.rate / proj.scale))
      oini:set("exedit", "audio_rate", proj.audio_rate)
      oini:set("exedit", "audio_ch", proj.audio_ch)
      
      -- lab の内容に従ってテキストオブジェクトを挿入していく
      -- もし表示が被る場合は表示先のレイヤーも変える
      -- ただしそれでも結局正しく扱えないのであまり意味はないかも
      local textbase = tostring(wavP.exaread(wavP.resolvepath(v.filepath, setting.lab_exafinder, setting), "lab"))
      local values = {
        START = 0,
        END = 0,
        LIPSYNC = ""
      }
      local modifiers = {
        ENCODE_TEXT = function(v)
          return GCMZDrops.encodeexotext(v)
        end
      }
      local layers = {}
      local n = 0
      for i, t in ipairs(lab) do
        values.LIPSYNC = setting:lab_lipsync_scripter(t.p)
        values.START = math.ceil(t.s * proj.rate / proj.scale)
        values.END = math.ceil(t.e * proj.rate / proj.scale) - 1
        local found = nil
        for li, le in ipairs(layers) do
          if le < values.START then
            found = li
            break
          end
        end
        if found ~= nil then
          layers[found] = values.END
        else
          table.insert(layers, values.END)
          found = #layers
        end

        local aini = GCMZDrops.inistring(textbase)
        setting:lab_examodifler(aini, values, modifiers)
        wavP.insertexa(oini, aini, n, found)
        n = n + 1
      end

      local filepath = GCMZDrops.createtempfile("lab", ".exo")
      local exo, err = io.open(filepath, "wb")
      if exo == nil then
        error(err)
      end
      exo:write(tostring(oini))
      exo:close()
      debug_print("["..P.name.."] が " .. v.filepath .. " を exo ファイルに差し替えました。元のファイルは orgfilepath で取得できます。")
      files[i] = {filepath=filepath, orgfilepath=v.filepath}
    end
  end
  -- 他のイベントハンドラーにも処理をさせたいのでここは常に false
  return false
end

return P

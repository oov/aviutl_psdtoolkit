local P = {}

P.name = "SRT ファイルをインポート"

P.priority = 0

local wavP = require("psdtoolkit_wav")

function P.ondragenter(files, state)
  for i, v in ipairs(files) do
    if v.filepath:match("[^.]+$"):lower() == "srt" then
      -- ファイルの拡張子が srt のファイルがあったら処理できそうなので true
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
  local setting = wavP.loadsetting()
  local f, err = io.open(filepath, "rb")
  if f == nil then
    error(err)
  end
  local srt = f:read("*all")
  f:close()
  if setting.srt_encoding ~= "utf8" then
    srt = GCMZDrops.convertencoding(srt, setting.srt_encoding, "utf8")
  end
  if srt:sub(-1) ~= "\n" then
    srt = srt .. "\r\n"
  end

  local r = {}
  local id = nil
  local subtitle = ""
  local startf = nil
  local endf = nil
  local maxendf = 0
  for line in srt:gmatch("(.-)\r?\n") do
    if line ~= "" then
      local yet = true
      if yet and (id == nil) then
        local s = line:match("^%d+$")
        if s ~= nil then
          id = tonumber(s)
          yet = false
        end
      end
      if yet and (startf == nil)and(endf == nil) then
        local sh, sm, ss, sms, eh, em, es, ems = line:match("^(%d+):(%d%d):(%d%d),(%d%d%d) %-%-> (%d+):(%d%d):(%d%d),(%d%d%d)$")
        if sh ~= nil then
          startf = tonumber(sh)*60*60 + tonumber(sm)*60 + tonumber(ss) + tonumber(sms)/1000
          endf = tonumber(eh)*60*60 + tonumber(em)*60 + tonumber(es) + tonumber(ems)/1000
          yet = false
        end
      end
      if yet then
        subtitle = subtitle .. line .. "\r\n"
        yet = false
      end
    else
      if (id ~= nil)and(subtitle ~= "")and(startf ~= nil)and(endf ~= nil) then
        endf = endf + setting.srt_margin
        table.insert(r, {id=id, s=startf, e=endf, subtitle=subtitle})
        if maxendf < endf then
          maxendf = endf
        end
      end
      id = nil
      subtitle = ""
      startf = nil
      endf = nil
    end
  end
  -- 多分必要ないけど、時間軸を無視した配置もできるので一応対策
  table.sort(r, function(a, b)
    return a.s < b.s
  end)
  return r, maxendf
end

function P.ondrop(files, state)
  local setting = wavP.loadsetting()
  for i, v in ipairs(files) do
    -- ファイルの拡張子が srt なら
    if v.filepath:match("[^.]+$"):lower() == "srt" then
      -- プロジェクトの情報を取得する
      local proj = GCMZDrops.getexeditfileinfo()
      -- SRT ファイルを解析
      local srt, len = P.parse(v.filepath)

      local oini = GCMZDrops.inistring("")
      oini:set("exedit", "width", proj.width)
      oini:set("exedit", "height", proj.height)
      oini:set("exedit", "rate", proj.rate)
      oini:set("exedit", "scale", proj.scale)
      oini:set("exedit", "length", math.floor(len * proj.rate / proj.scale))
      oini:set("exedit", "audio_rate", proj.audio_rate)
      oini:set("exedit", "audio_ch", proj.audio_ch)
      
      -- SRT の内容に従ってテキストオブジェクトを挿入していく
      -- もし表示が被る場合は表示先のレイヤーも変える
      -- ただ、挿入モード1だと結局正しく扱えないのであまり意味はないかも
      local textbase = tostring(wavP.exaread(wavP.resolvepath(v.filepath, setting.srt_exafinder, setting), "srt"))
      local values = {
        START = 0,
        END = 0,
        SUBTITLE = ""
      }
      local modifiers = {
        ENCODE_TEXT = function(v)
          return GCMZDrops.encodeexotextutf8(v)
        end
      }
      local layers = {}
      local n = 0
      for i, t in ipairs(srt) do
        local subtitle = t.subtitle
        -- 置換用処理を呼び出す
        subtitle = setting:wav_subtitle_replacer(subtitle)
        -- 挿入モードが 1 の時はテキストをスクリプトとして整形する
        if setting.srt_insertmode == 1 then
          subtitle = setting:srt_subtitle_scripter(subtitle)
        end
        values.SUBTITLE = subtitle
        values.START = math.floor(t.s * proj.rate / proj.scale)
        values.END = math.floor(t.e * proj.rate / proj.scale)
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
        setting:srt_examodifler(aini, values, modifiers)
        wavP.insertexa(oini, aini, n, found)
        n = n + 1
      end

      local filepath = GCMZDrops.createtempfile("srt", ".exo")
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

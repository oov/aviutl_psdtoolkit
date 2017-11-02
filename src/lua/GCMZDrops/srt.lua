local P = {}

P.name = "SRT ファイルをインポート"

P.priority = 0

-- ===========================================================
-- 設定　ここから
-- ===========================================================

-- 挿入モード
--   0 - 字幕データをテキストオブジェクトとして挿入
--   1 - 字幕データをテキストオブジェクトにスクリプトとして挿入
--
-- [挿入モード 1 の使い方]
--   挿入モード 1 では、そのままではテキストは表示されません。
--   ドロップによって挿入されたテキストオブジェクトよりも下に
--   [メディアオブジェクトの追加]→[PSDToolKit]→[テキスト　字幕表示用]
--   で表示用のテキストオブジェクトを追加することで表示されます。
--   装飾などの設定は全て表示用のテキストオブジェクト側で行います。
P.insertmode = 1

-- SRTファイルの文字エンコーディング
-- "sjis" か "utf8" で指定
-- ※ただしどちらにしても挿入前に一旦 Shift_JIS に変換されます
P.encoding = "utf8"

-- 字幕表示を延長
-- 秒で指定
P.margin = 0

-- 字幕用のエイリアスファイル(*.exa)をどのように参照するか
-- この設定を使うと、ドロップされた *.srt ファイルの名前に応じて別のエイリアスファイルを使用できます。
-- エイリアスファイルは exa フォルダーの中に配置して下さい。
-- 該当するファイルが見つからない場合は exa\srt.exa が代わりに使用されます。
--   0 - 常に同じファイルを参照する
--     ドロップされたファイルに関わらず以下のエイリアスファイルが使用されます。
--       exa\srt.exa
--   1 - ファイルが入っているフォルダ名を元にする
--     例: ドロップされたファイルが C:\MyFolder\TKHS_Hello_World.srt の時
--       exa\MyFolder_srt.exa
--   2 - ファイル名を元にする
--     例: ドロップされたファイルが C:\MyFolder\TKHS_Hello_World.srt の時
--       exa\TKHS_Hello_World_srt.exa
--   3 - ファイル名の中で _ で区切られた最初の部分を元にする
--     例: ドロップされたファイルが C:\MyFolder\TKHS_Hello_World.srt の時
--       exa\TKHS_srt.exa
--   4 - ファイル名の中で _ で区切られた2つめの部分を元にする
--     例: ドロップされたファイルが C:\MyFolder\TKHS_Hello_World.srt の時
--       exa\Hello_srt.exa
--   5 - ファイル名の中で _ で区切られた3つめの部分を元にする
--     例: ドロップされたファイルが C:\MyFolder\TKHS_Hello_World.srt の時
--       exa\World_srt.exa
P.exa_finder = 0

-- エイリアスファイルの改変処理
-- 一般的な用途では変更する必要はありません。
P.exa_modifler_srt = function(exa, values, modifiers)
  exa:set("vo", "start", values.START + 1)
  exa:set("vo", "end", values.END + 1)
  exa:delete("vo", "length")
  exa:set("vo", "group", 1)
  exa:set("vo.0", "text", modifiers.ENCODE_TEXT(values.TEXT))
end

-- 挿入モード 1 の時に使用される接頭辞、接尾辞とエスケープ処理
P.text_prefix = '<?s=[==['
P.text_postfix = ']==];require("PSDToolKit\\\\PSDToolKitLib").settext(s, obj, true);s=nil?>'
P.text_escape = function(s)
  return s:gsub("]==]", ']==].."]==]"..[==[')
end

-- ===========================================================
-- 設定　ここまで
-- ===========================================================

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
  local f, err = io.open(filepath, "rb")
  if f == nil then
    error(err)
  end
  local srt = f:read("*all")
  f:close()
  if P.encoding ~= "sjis" then
    srt = GCMZDrops.convertencoding(srt, P.encoding, "sjis")
  end
  if srt:sub(-1) ~= "\n" then
    srt = srt .. "\r\n"
  end

  local r = {}
  local id = nil
  local text = ""
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
        text = text .. line .. "\r\n"
        yet = false
      end
    else
      if (id ~= nil)and(text ~= "")and(startf ~= nil)and(endf ~= nil) then
        endf = endf + P.margin
        table.insert(r, {id=id, s=startf, e=endf, text=text})
        if maxendf < endf then
          maxendf = endf
        end
      end
      id = nil
      text = ""
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
      local textbase = tostring(wavP.exaread(wavP.resolvepath(v.filepath, P.exa_finder), "srt"))
      local values = {
        START = 0,
        END = 0,
        TEXT = ""
      }
      local modifiers = {
        ENCODE_TEXT = function(v)
          return GCMZDrops.encodeexotext(v)
        end
      }
      local layers = {}
      local n = 0
      for i, t in ipairs(srt) do
        local text = t.text
        -- 挿入モードが 1 の時はテキストをスクリプトとして整形する
        if P.insertmode == 1 then
          if text:sub(-2) ~= "\r\n" then
            text = text .. "\r\n"
          end
          text = P.text_prefix .. "\r\n" .. P.text_escape(text) .. P.text_postfix
        end
        values.TEXT = text
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
        P.exa_modifler_srt(aini, values, modifiers)
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

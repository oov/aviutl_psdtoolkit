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

-- スクリプトが分かる人用
P.text_prefix = '<?s=[==['
P.text_postfix = ']==];require("PSDToolKit\\\\PSDToolKitLib").settext(s, obj, true);s=nil?>'

-- ===========================================================
-- 設定　ここまで
-- ===========================================================

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

function texobj(n, layer, startf, endf, group, text)
  if P.insertmode == 1 then
    text = text:gsub("]==]", ']==].."]==]"..[==[')
    text = P.text_prefix .. "\r\n" .. text
    if text:sub(-2) ~= "\r\n" then
      text = text .. "\r\n"
    end
    text = text .. P.text_postfix
  end
  return [[
[]] .. n .. [[]
start=]] .. (startf+1) .. "\r\n" .. [[
end=]] .. (endf+1) .. "\r\n" .. [[
layer=]] .. layer .. "\r\n" .. [[
group=]] .. group .. "\r\n" .. [[
overlay=1
camera=0
[]] .. n .. [[.0]
_name=テキスト
サイズ=34
表示速度=0.0
文字毎に個別オブジェクト=0
移動座標上に表示する=0
自動スクロール=0
B=0
I=0
type=0
autoadjust=0
soft=1
monospace=0
align=0
spacing_x=0
spacing_y=0
precision=1
color=ffffff
color2=000000
font=MS UI Gothic
text=]] .. GCMZDrops.encodeexotext(text) .. "\r\n" .. [[
[]] .. n .. [[.1]
_name=標準描画
X=0.0
Y=0.0
Z=0.0
拡大率=100.00
透明度=0.0
回転=0.00
blend=0
]]
end

function parse(filepath)
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
      local srt, len = parse(v.filepath)
      -- 時間を秒からフレームに書き換え
      len = math.floor(len * proj.rate / proj.scale)
      -- exo ファイルを組み立てる
      local exo = ""
      exo = [[
[exedit]
width=]] .. proj.width .. "\r\n" .. [[
height=]] .. proj.height .. "\r\n" .. [[
rate=]] .. proj.rate .. "\r\n" .. [[
scale=]] .. proj.scale .. "\r\n" .. [[
length=]] .. len .. "\r\n" .. [[
audio_rate=]] .. proj.audio_rate .. "\r\n" .. [[
audio_ch=]] .. proj.audio_ch .. "\r\n" .. [[
]]
      local filepath = GCMZDrops.createtempfile("srt", ".exo")
      f, err = io.open(filepath, "wb")
      if f == nil then
        error(err)
      end
      f:write(exo)

      -- SRT の内容に従ってテキストオブジェクトを挿入していく
      -- もし表示が被る場合は表示先のレイヤーも変える
      -- ただ、挿入モード1だと結局正しく扱えないのであまり意味はないかも
      local layers = {}
      local n = 0
      for i, t in ipairs(srt) do
        t.s = math.floor(t.s * proj.rate / proj.scale)
        t.e = math.floor(t.e * proj.rate / proj.scale)
        local found = nil
        for li, le in ipairs(layers) do
          if le < t.s then
            found = li
            break
          end
        end
        if found ~= nil then
          layers[found] = t.e
        else
          table.insert(layers, t.e)
          found = #layers
        end
        f:write(texobj(n, found, t.s, t.e, 1, t.text))
        n = n + 1
      end

      f:close()
      debug_print("["..P.name.."] が " .. v.filepath .. " を exo ファイルに差し替えました。元のファイルは orgfilepath で取得できます。")
      files[i] = {filepath=filepath, orgfilepath=v.filepath}
    end
  end
  -- 他のイベントハンドラーにも処理をさせたいのでここは常に false
  return false
end

return P

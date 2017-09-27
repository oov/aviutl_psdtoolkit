local P = {}

P.name = "Shift キーを押しながら *.wav をドロップで「口パク準備」を挿入"

P.priority = 0

-- ===========================================================
-- 設定　ここから
-- ===========================================================

-- 発動モード
--   0 - Shift キーを押しながら *.wav ファイルをドロップすると発動
--   1 - *.wav ファイルと同じ場所に *.txt があると発動、Shift キーで無効化
P.firemode = 0

-- 挿入モード
--   0 - 常に音声と口パク準備のみを挿入する
--   1 - テキストファイルを探してテキストオブジェクトとして挿入
--   2 - テキストファイルを探してテキストオブジェクトにスクリプトを挿入
--
-- [挿入モード 2 の使い方]
--   挿入モード 2 では、そのままではテキストは表示されません。
--   ドロップによって挿入されたテキストオブジェクトよりも下に
--   [メディアオブジェクトの追加]→[PSDToolKit]→[テキスト　字幕表示用]
--   で表示用のテキストオブジェクトを追加することで表示されます。
--   装飾などの設定は全て表示用のテキストオブジェクト側で行います。
P.insertmode = 2

-- *.lab ファイル（タイミング情報ファイル）が
-- *.wav と同じフォルダーに同じファイル名で存在する時は
-- 口パク準備で *.lab を優先的に使うかを true か false で指定
P.use_lab = true

-- テキストオブジェクトも一緒にグループ化するかを true か false で指定
P.text_group = true

-- 追加されるテキストオブジェクトを
-- 指定したフレーム数だけ音声よりも長くする
P.text_margin = 30

-- テキストファイルの文字エンコーディング
-- "sjis" か "utf8" で指定
-- ※ただしどちらにしても挿入前に一旦 Shift_JIS に変換されます
P.text_encoding = "sjis"

-- ===========================================================
-- 設定　ここまで
-- ===========================================================

function P.ondragenter(files, state)
  for i, v in ipairs(files) do
    if v.filepath:match("[^.]+$"):lower() == "wav" then
      -- ファイルの拡張子が wav のファイルがあったら処理できそうなので true
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

function fileexists(filepath)
  local f = io.open(filepath, "rb")
  if f ~= nil then
    f:close()
    return true
  end
  return false
end

function fire(state, v)
  if P.firemode == 0 then
    return state.shift
  elseif P.firemode == 1 then
    if state.shift then
      return false
    end
    local txtfilepath = v.orgfilepath or v.filepath
    txtfilepath = txtfilepath:sub(1, #txtfilepath - 3) .. "txt"
    return fileexists(txtfilepath)
  end
  return false
end

function P.encodelua(s)
  s = GCMZDrops.convertencoding(s, "sjis", "utf8")
  s = GCMZDrops.encodeluastring(s)
  s = GCMZDrops.convertencoding(s, "utf8", "sjis")
  return s
end

function P.ondrop(files, state)
  for i, v in ipairs(files) do
    -- ファイルの拡張子が wav で発動モードの条件を満たしていたら
    if (v.filepath:match("[^.]+$"):lower() == "wav") and fire(state, v) then
      -- プロジェクトとファイルの情報を取得する
      local proj = GCMZDrops.getexeditfileinfo()
      local fi = GCMZDrops.getfileinfo(v.filepath)
      -- 音声が現在のプロジェクトで何フレーム分あるのかを計算する
      local len = math.ceil((fi.audio_samples / proj.audio_rate) * proj.rate / proj.scale)

      local text = ""
      if P.insertmode > 0 then
        -- *.txt があるか探すために *.wav の拡張子部分を差し替える
        -- もし orgfilepath があるならそっちの名前を元に探さなければならない
        local txtfilepath = v.orgfilepath or v.filepath
        txtfilepath = txtfilepath:sub(1, #txtfilepath - 3) .. "txt"
        local f = io.open(txtfilepath, "rb")
        if f ~= nil then
          text = f:read("*all")
          f:close()
          -- 文字エンコーディングが Shift_JIS 以外の時は Shift_JIS へ変換する
          -- TODO: GCMZDrops.encodeexotext で UTF-8 の受け入れを可能に
          if P.text_encoding ~= "sjis" then
            text = GCMZDrops.convertencoding(text, P.text_encoding, "sjis")
          end
        end
      end

      local lipsync = v.filepath
      if P.use_lab then
        -- *.lab があるか探すために *.wav の拡張子部分を差し替える
        -- もし orgfilepath があるならそっちの名前を元に探さなければならない
        local labfilepath = v.orgfilepath or v.filepath
        labfilepath = labfilepath:sub(1, #labfilepath - 3) .. "lab"
        if fileexists(labfilepath) then
          if GCMZDrops.needcopy(labfilepath) then
            -- もし見つかった場所が恒久的に利用できる場所ではない場合は
            -- avoiddup.lua の機能で安全地帯にファイルをコピーする
            local newlabfilepath, created = require("avoiddup").getfile(labfilepath)
            lipsync = newlabfilepath
          else
            lipsync = labfilepath
          end
        end
      end

      -- ファイルを直接読み込む代わりに exo ファイルを組み立てる
      local exo = ""
      local textgroup = 0
      if P.text_group then
        textgroup = 1
      end
      if (P.insertmode == 0)or(text == "") then
        exo = [[
[exedit]
width=]] .. proj.width .. "\r\n" .. [[
height=]] .. proj.height .. "\r\n" .. [[
rate=]] .. proj.rate .. "\r\n" .. [[
scale=]] .. proj.scale .. "\r\n" .. [[
length=]] .. (len - 1) .. "\r\n" .. [[
audio_rate=]] .. proj.audio_rate .. "\r\n" .. [[
audio_ch=]] .. proj.audio_ch .. "\r\n" .. [[
[0]
start=1
end=]] .. (len - 1) .. "\r\n" .. [[
layer=1
group=1
overlay=1
audio=1
[0.0]
_name=音声ファイル
再生位置=0.00
再生速度=100.0
ループ再生=0
動画ファイルと連携=0
file=]] .. v.filepath .. "\r\n" .. [[
[0.1]
_name=標準再生
音量=100.0
左右=0.0
[1]
start=1
end=]] .. (len - 1) .. "\r\n" .. [[
layer=2
group=1
overlay=1
camera=0
[1.0]
_name=カスタムオブジェクト
track0=100.00
track1=1000.00
track2=20.00
track3=0.00
check0=0
type=0
filter=2
name=口パク準備@PSDToolKit
param=]] .. "file=" .. P.encodelua(lipsync) .. "\r\n" .. [[
[1.1]
_name=標準描画
X=0.0
Y=0.0
Z=0.0
拡大率=100.00
透明度=0.0
回転=0.00
blend=0
]]
      elseif (P.insertmode == 1) or (P.insertmode == 2) then
        if P.insertmode == 2 then
          text = text:gsub("]==]", ']==].."]==]"..[==[')
          text = "<?_s=[==[\r\n" .. text
          if text:sub(-2) ~= "\r\n" then
            text = text .. "\r\n"
          end
          text = text .. "]==]?>"
        end
        exo = [[
[exedit]
width=]] .. proj.width .. "\r\n" .. [[
height=]] .. proj.height .. "\r\n" .. [[
rate=]] .. proj.rate .. "\r\n" .. [[
scale=]] .. proj.scale .. "\r\n" .. [[
length=]] .. (len - 1 + P.text_margin) .. "\r\n" .. [[
audio_rate=]] .. proj.audio_rate .. "\r\n" .. [[
audio_ch=]] .. proj.audio_ch .. "\r\n" .. [[
[0]
start=1
end=]] .. (len - 1 + P.text_margin) .. "\r\n" .. [[
layer=1
group=]] .. textgroup .. "\r\n" .. [[
overlay=1
camera=0
[0.0]
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
[0.1]
_name=標準描画
X=0.0
Y=0.0
Z=0.0
拡大率=100.00
透明度=0.0
回転=0.00
blend=0
[1]
start=1
end=]] .. (len - 1) .. "\r\n" .. [[
layer=2
group=1
overlay=1
audio=1
[1.0]
_name=音声ファイル
再生位置=0.00
再生速度=100.0
ループ再生=0
動画ファイルと連携=0
file=]] .. v.filepath .. "\r\n" .. [[
[1.1]
_name=標準再生
音量=100.0
左右=0.0
[2]
start=1
end=]] .. (len - 1) .. "\r\n" .. [[
layer=3
group=1
overlay=1
camera=0
[2.0]
_name=カスタムオブジェクト
track0=100.00
track1=1000.00
track2=20.00
track3=0.00
check0=0
type=0
filter=2
name=口パク準備@PSDToolKit
param=]] .. "file=" .. P.encodelua(lipsync) .. "\r\n" .. [[
[2.1]
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
      local filepath = GCMZDrops.createtempfile("psd", ".exo")
      f, err = io.open(filepath, "wb")
      if f == nil then
        error(err)
      end
      f:write(exo)
      f:close()
      debug_print("["..P.name.."] が " .. v.filepath .. " を exo ファイルに差し替えました。元のファイルは orgfilepath で取得できます。")
      files[i] = {filepath=filepath, orgfilepath=v.filepath}
    end
  end
  -- 他のイベントハンドラーにも処理をさせたいのでここは常に false
  return false
end

return P

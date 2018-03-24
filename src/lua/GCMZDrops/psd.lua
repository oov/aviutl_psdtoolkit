local P = {}

P.name = "PSD ファイルの exo 化"

P.priority = 0

function P.ondragenter(files, state)
  for i, v in ipairs(files) do
    if v.filepath:match("[^.]+$"):lower() == "psd" then
      -- ファイルの拡張子が psd のファイルがあったら処理できそうなので true
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

function P.encodelua(s)
  s = GCMZDrops.convertencoding(s, "sjis", "utf8")
  s = GCMZDrops.encodeluastring(s)
  s = GCMZDrops.convertencoding(s, "utf8", "sjis")
  return s
end

function P.ondrop(files, state)
  for i, v in ipairs(files) do
    -- ファイルの拡張子が psd だったら
    if v.filepath:match("[^.]+$"):lower() == "psd" then
      local filepath = v.filepath
      local filename = filepath:match("[^/\\]+$")

      -- 一緒に pfv ファイルを掴んでいないか調べる
      local psddir = filepath:sub(1, #filepath-#filename)
      for i2, v2 in ipairs(files) do
        if v2.filepath:match("[^.]+$"):lower() == "pfv" then
          local pfv = v2.filepath:match("[^/\\]+$")
          local pfvdir = v2.filepath:sub(1, #v2.filepath-#pfv)
          if psddir == pfvdir then
            -- 同じフォルダー内の pfv ファイルを一緒に投げ込んでいたので連結
            filepath = filepath .. "|" .. pfv
            -- この pfv ファイルはドロップされるファイルからは取り除いておく
            table.remove(files, i2)
            break
          end
        end
      end

      -- ファイルを直接読み込む代わりに exo ファイルを組み立てる
      local proj = GCMZDrops.getexeditfileinfo()
      local exo = [[
[exedit]
width=]] .. proj.width .. "\r\n" .. [[
height=]] .. proj.height .. "\r\n" .. [[
rate=]] .. proj.rate .. "\r\n" .. [[
scale=]] .. proj.scale .. "\r\n" .. [[
length=64
audio_rate=]] .. proj.audio_rate .. "\r\n" .. [[
audio_ch=]] .. proj.audio_ch .. "\r\n" .. [[
[0]
start=1
end=64
layer=1
overlay=1
camera=0
[0.0]
_name=テキスト
サイズ=1
表示速度=0.0
文字毎に個別オブジェクト=0
移動座標上に表示する=0
自動スクロール=0
B=0
I=0
type=0
autoadjust=0
soft=0
monospace=0
align=4
spacing_x=0
spacing_y=0
precision=0
color=ffffff
color2=000000
font=MS UI Gothic
text=]] .. GCMZDrops.encodeexotext("<?-- " .. filename .. " \r\n\r\no={ -- オプション設定\r\nlipsync = 0    ,-- 口パク準備のレイヤー番号\r\nmpslider = 0    ,-- 多目的スライダーのレイヤー番号\r\nscene = 0    ,-- シーン番号\r\n\r\n-- 口パク設定\r\nls_locut = 100    ,-- ローカット\r\nls_hicut = 1000    ,-- ハイカット\r\nls_threshold = 20    ,-- しきい値\r\n\r\n-- 以下は書き換えないでください\r\nptkf=" .. P.encodelua(filepath) .. ",ptkl=\"\"}PSD,subobj=require(\"PSDToolKit\").PSDState.init(obj,o)?>") .. "\r\n" .. [[
[0.1]
_name=アニメーション効果
track0=-1.00
track1=100.00
track2=0.00
track3=0.00
check0=100
type=0
filter=2
name=描画@PSD
param=
[0.2]
_name=標準描画
X=0.0
Y=0.0
Z=0.0
拡大率=100.00
透明度=0.0
回転=0.00
blend=0
]]

      -- PSDToolKit ウィンドウにドロップされたファイルを追加する
      -- 一時的に package.cpath を書き換え PSDToolKitBridge.dll を読み込んで addfile を呼ぶ
      local origcpath = package.cpath
      package.cpath = GCMZDrops.scriptdir() .. "..\\script\\PSDToolKit\\?.dll"
      require('PSDToolKitBridge').addfile(GCMZDrops.convertencoding(filepath, "sjis", "utf8"))
      package.cpath = origcpath

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

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

-- レイヤー節約モードを有効にするかを true か false で指定
-- レイヤー節約モードでは「口パク準備」をテキストオブジェクトの
-- アニメーション効果として挿入することで、レイヤーを１つ分節約します。
-- テキストオブジェクトを音声より短くすると、当然正しく動かなくなります。
P.save_layer = false

-- 口パク準備のパラメータ設定はここにはありません。
-- exa フォルダにある lip.exa の中の track0, track1, track2 を書き換えてください。
-- それぞれ LoCut, HiCut, Threshold に対応しています。

-- テキストオブジェクトも一緒にグループ化するかを true か false で指定
P.text_group = true

-- 追加されるテキストオブジェクトを
-- 指定したフレーム数だけ音声よりも長くする
P.text_margin = 0

-- テキストファイルの文字エンコーディング
-- "sjis" か "utf8" で指定
-- ※ただしどちらにしても挿入前に一旦 Shift_JIS に変換されます
P.text_encoding = "sjis"

-- 字幕、音声、口パク準備用のエイリアスファイル(*.exa)をどのように参照するか
-- この設定を使うと、ドロップされた *.wav ファイルの名前に応じて別のエイリアスファイルを使用できます。
-- エイリアスファイルは exa フォルダーの中に配置して下さい。
-- 該当するファイルが見つからない場合は exa\text.exa / exa\wav.exa / exa\lip.exa が代わりに使用されます。
--   0 - 常に同じファイルを参照する
--     ドロップされたファイルに関わらず以下のエイリアスファイルが使用されます。
--       字幕: exa\text.exa
--       音声: exa\wav.exa
--       口パク準備: exa\lip.exa
--   1 - ファイルが入っているフォルダ名を元にする
--     例: ドロップされたファイルが C:\MyFolder\TKHS_Hello_World.wav の時
--       字幕: exa\MyFolder_text.exa
--       音声: exa\MyFolder_wav.exa
--       口パク準備: exa\MyFolder_lip.exa
--   2 - ファイル名を元にする
--     例: ドロップされたファイルが C:\MyFolder\TKHS_Hello_World.wav の時
--       字幕: exa\TKHS_Hello_World_text.exa
--       音声: exa\TKHS_Hello_World_wav.exa
--       口パク準備: exa\TKHS_Hello_World_lip.exa
--   3 - ファイル名の中で _ で区切られた最初の部分を元にする
--     例: ドロップされたファイルが C:\MyFolder\TKHS_Hello_World.wav の時
--       字幕: exa\TKHS_text.exa
--       音声: exa\TKHS_wav.exa
--       口パク準備: exa\TKHS_lip.exa
--   4 - ファイル名の中で _ で区切られた2つめの部分を元にする
--     例: ドロップされたファイルが C:\MyFolder\TKHS_Hello_World.wav の時
--       字幕: exa\Hello_text.exa
--       音声: exa\Hello_wav.exa
--       口パク準備: exa\Hello_lip.exa
--   5 - ファイル名の中で _ で区切られた3つめの部分を元にする
--     例: ドロップされたファイルが C:\MyFolder\TKHS_Hello_World.wav の時
--       字幕: exa\World_text.exa
--       音声: exa\World_wav.exa
--       口パク準備: exa\World_lip.exa
P.exa_finder = 0

-- エイリアスファイルの改変処理
-- 一般的な用途では変更する必要はありません。
P.exa_modifler_text = function(exa, values, modifiers)
  exa:set("vo", "start", 1)
  exa:set("vo", "end", values.TEXT_LEN)
  exa:delete("vo", "length")
  exa:set("vo", "group", P.text_group and 1 or 0)
  exa:set("vo.0", "text", modifiers.ENCODE_TEXT(values.TEXT))
end
P.exa_modifler_wav = function(exa, values, modifiers)
  exa:set("ao", "start", 1)
  exa:set("ao", "end", values.WAV_LEN)
  exa:delete("ao", "length")
  exa:set("ao", "group", 1)
  exa:set("ao.0", "file", values.WAV_PATH)
end
P.exa_modifler_lip = function(exa, values, modifiers)
  exa:set("vo", "start", 1)
  exa:set("vo", "end", values.WAV_LEN)
  exa:delete("vo", "length")
  exa:set("vo", "group", 1)
  exa:set("vo.0", "param", "file=" .. modifiers.ENCODE_LUA_STRING(values.LIP_PATH))
end

-- 挿入モード 2 の時に使用される接頭辞、接尾辞とエスケープ処理
P.text_prefix = '<?s=[==['
P.text_postfix = ']==];require("PSDToolKit\\\\PSDToolKitLib").settext(s, obj, true);s=nil?>'
P.text_escape = function(s)
  return s:gsub("]==]", ']==].."]==]"..[==[')
end

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

local function changefileext(filepath, ext)
  local old = filepath:match("[^.]+$")
  return filepath:sub(1, #filepath - #old) .. ext
end

local function fileexists(filepath)
  local f = io.open(filepath, "rb")
  if f ~= nil then
    f:close()
    return true
  end
  return false
end

local function fileread(filepath)
  local f = io.open(filepath, "rb")
  if f == nil then
    return nil
  end
  local text = f:read("*all")
  f:close()
  return text
end

function P.exaread(filepath, postfix)
  local inistr = nil
  if filepath ~= nil then
    filepath = GCMZDrops.scriptdir() .. "exa\\" .. filepath .. "_" .. postfix .. ".exa"
    inistr = fileread(filepath)
    if inistr == nil then
      debug_print("読み込み失敗: " .. filepath)
    end
  end
  if inistr == nil then
    filepath = GCMZDrops.scriptdir() .. "exa\\" .. postfix .. ".exa"
    inistr = fileread(filepath)
  end
  if inistr ~= nil then
    debug_print("使用するエイリアスファイル: " .. filepath)
  else
    error("cannot read: " .. filepath)
  end
  return GCMZDrops.inistring(inistr)
end

function P.findexatype(ini)
  if ini:sectionexists("vo") then
    return "vo"
  elseif ini:sectionexists("ao") then
    return "ao"
  end
  error("unexpected alias file format")
end

function P.numitemsections(ini)
  local prefix = P.findexatype(ini)  
  local n = 0
  while ini:sectionexists(prefix .. "." .. n) do
    n = n + 1
  end
  return n
end

function P.insertexa(destini, srcini, index, layer)
  local prefix = P.findexatype(srcini)
  destini:set(index, "layer", layer)
  destini:set(index, "overlay", 1)
  for _, key in ipairs(srcini:keys(prefix)) do
    destini:set(index, key, srcini:get(prefix, key, ""))
  end
  if prefix == "ao" then
    destini:set(index, "audio", 1)
  end

  for i = 0, P.numitemsections(srcini) - 1 do
    local exosection = index .. "." .. i
    local section = prefix .. "." .. i
    for _, key in ipairs(srcini:keys(section)) do
      destini:set(exosection, key, srcini:get(section, key, ""))
    end
  end
end

local function fire(state, v)
  local firemode = P.firemode
  if v.overridefiremode ~= nil then
    -- 他のスクリプトから overridefiremode 属性を追加されていた場合は
    -- 設定内容に関わらずそちらの発動モードを採用する
    firemode = v.overridefiremode
  end
  if firemode == 0 then
    return state.shift
  elseif firemode == 1 then
    if state.shift then
      return false
    end
    local txtfilepath = v.orgfilepath or v.filepath
    txtfilepath = txtfilepath:sub(1, #txtfilepath - 3) .. "txt"
    return fileexists(txtfilepath)
  end
  return false
end

function P.resolvepath(filepath, finder)
  if finder == 1 then
    return filepath:match("([^\\]+)[\\][^\\]+$")
  elseif finder == 2 then
    return filepath:match("([^\\]+)%.[^.]+$")
  elseif finder == 3 then
    return filepath:match("([^\\]+)%.[^.]+$"):match("^[^_]+")
  elseif finder == 4 then
    return filepath:match("([^\\]+)%.[^.]+$"):match("^[^_]+_([^_]+)")
  elseif finder == 5 then
    return filepath:match("([^\\]+)%.[^.]+$"):match("^[^_]+_[^_]+_([^_]+)")
  elseif type(finder) == "function" then
    return finder(filepath)
  end
  return nil
end

function P.ondrop(files, state)
  for i, v in ipairs(files) do
    -- ファイルの拡張子が wav で発動モードの条件を満たしていたら
    if (v.filepath:match("[^.]+$"):lower() == "wav") and fire(state, v) then
      -- プロジェクトとファイルの情報を取得する
      local proj = GCMZDrops.getexeditfileinfo()
      local fi = GCMZDrops.getfileinfo(v.filepath)

      -- テンプレート用変数を準備
      local values = {
        TEXT = "",
        TEXT_LEN = 0,
        WAV_LEN = 0,
        WAV_PATH = v.filepath,
        LIP_PATH = v.filepath
      }
      local modifiers = {
        ENCODE_TEXT = function(v)
          return GCMZDrops.encodeexotext(v)
        end,
        ENCODE_LUA_STRING = function(v)
          v = GCMZDrops.convertencoding(v, "sjis", "utf8")
          v = GCMZDrops.encodeluastring(v)
          v = GCMZDrops.convertencoding(v, "utf8", "sjis")
          return v
        end
      }

      -- 音声が現在のプロジェクトで何フレーム分あるのかを計算する
      values.WAV_LEN = math.ceil((fi.audio_samples / proj.audio_rate) * proj.rate / proj.scale)
      values.TEXT_LEN = values.WAV_LEN + P.text_margin

      if P.insertmode > 0 then
        -- *.txt があるか探すために *.wav の拡張子部分を差し替える
        -- もし orgfilepath があるならそっちの名前を元に探さなければならない
        local txtfilepath = changefileext(v.orgfilepath or v.filepath, "txt")
        local text = fileread(txtfilepath)
        if text ~= nil then
          -- 文字エンコーディングが Shift_JIS 以外の時は Shift_JIS へ変換する
          -- TODO: GCMZDrops.encodeexotext で UTF-8 の受け入れを可能に
          local enc = P.text_encoding
          if v.overridetextencoding ~= nil then
            -- 他のスクリプトから overridetextencoding 属性を追加されていた場合は
            -- 設定内容に関わらずそちらの文字エンコーディングを採用する
            enc = v.overridetextencoding
          end
          if enc ~= "sjis" then
            text = GCMZDrops.convertencoding(text, enc, "sjis")
          end
          -- 挿入モードが 2 の時はテキストをスクリプトとして整形する
          if P.insertmode == 2 then
            if text:sub(-2) ~= "\r\n" then
              text = text .. "\r\n"
            end
            text = P.text_prefix .. "\r\n" .. P.text_escape(text) .. P.text_postfix
          end
          values.TEXT = text
        end
      end
      
      if P.use_lab then
        -- *.lab があるか探すために *.wav の拡張子部分を差し替える
        -- もし orgfilepath があるならそっちの名前を元に探さなければならない
        local labfilepath = changefileext(v.orgfilepath or v.filepath, "lab")
        if fileexists(labfilepath) then
          if GCMZDrops.needcopy(labfilepath) then
            -- もし見つかった場所が恒久的に利用できる場所ではない場合は
            -- avoiddup.lua の機能で安全地帯にファイルをコピーする
            local newlabfilepath, created = require("avoiddup").getfile(labfilepath)
            values.LIP_PATH = newlabfilepath
          else
            values.LIP_PATH = labfilepath
          end
        end
      end

      -- exo ファイルのヘッダ部分を組み立て
      local oini = GCMZDrops.inistring("")
      oini:set("exedit", "width", proj.width)
      oini:set("exedit", "height", proj.height)
      oini:set("exedit", "rate", proj.rate)
      oini:set("exedit", "scale", proj.scale)
      oini:set("exedit", "length", (values.WAV_LEN < values.TEXT_LEN) and values.TEXT_LEN or values.WAV_LEN)
      oini:set("exedit", "audio_rate", proj.audio_rate)
      oini:set("exedit", "audio_ch", proj.audio_ch)

      -- オブジェクトの挿入
      local filepath = P.resolvepath(v.orgfilepath or v.filepath, P.exa_finder)
      local index = 0
      
      -- 字幕用エイリアスを組み立て
      if values.TEXT ~= "" then
        local aini = P.exaread(filepath, "text")
        P.exa_modifler_text(aini, values, modifiers)
        if P.save_layer then
          -- レイヤー節約モードの場合は最後に口パク準備のアニメーション効果を挿入
          local prefix = P.findexatype(aini)
          local n = P.numitemsections(aini)
          local lastsection = prefix .. "." .. (n-1)
          local newsection = prefix .. "." .. n
          -- 一番最後の効果をひとつ後ろにずらし、コピー元は一旦削除
          for _, key in ipairs(aini:keys(lastsection)) do
            aini:set(newsection, key, aini:get(lastsection, key, ""))
            aini:delete(lastsection, key)
          end
          -- 口パク準備のカスタムオブジェクトをエイリアスファイルから読み込んで
          -- アニメーション効果に書き換えて挿入する
          local lini = P.exaread(filepath, "lip")
          P.exa_modifler_lip(lini, values, modifiers)
          local lsection = P.findexatype(lini) .. ".0" -- TODO: 決め打ちじゃない方がいい？
          for _, key in ipairs(lini:keys(lsection)) do
            aini:set(lastsection, key, lini:get(lsection, key, ""))
          end
          aini:set(lastsection, "_name", "アニメーション効果")
        end
        P.insertexa(oini, aini, index, index + 1)
        index = index + 1
      end

      -- 音声用エイリアスを組み立て
      local aini = P.exaread(filepath, "wav")
      P.exa_modifler_wav(aini, values, modifiers)
      P.insertexa(oini, aini, index, index + 1)
      index = index + 1

      -- 口パク準備用エイリアスを組み立て
      if not P.save_layer then
        local aini = P.exaread(filepath, "lip")
        P.exa_modifler_lip(aini, values, modifiers)
        P.insertexa(oini, aini, index, index + 1)
        index = index + 1
      end

      local filepath = GCMZDrops.createtempfile("wav", ".exo")
      f, err = io.open(filepath, "wb")
      if f == nil then
        error(err)
      end
      f:write(tostring(oini))
      f:close()
      debug_print("["..P.name.."] が " .. v.filepath .. " を exo ファイルに差し替えました。元のファイルは orgfilepath で取得できます。")
      files[i] = {filepath=filepath, orgfilepath=v.filepath}
    end
  end
  -- 他のイベントハンドラーにも処理をさせたいのでここは常に false
  return false
end

return P

-- ============================================================
-- ■注意■
-- ============================================================
--
-- このファイルには設定のデフォルト値が書いてありますが、
-- これはユーザーが書き換えるためのファイルではありません。
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 設定を変更したい時はこのファイルを書き換えるのではなく、
-- `setting.lua-template` のファイル名を `setting.lua` に変更し、
-- `setting.lua` 内に必要な設定のみを書き込むことで設定してください。
--
-- 上記の手順の通りに行うことで、PSDToolKit をバージョンアップする時に
-- 設定が上書きされるのを防ぐことができます。
--
-- 使い方の詳しい解説は付属のマニュアルを参照してください。
--
-- ============================================================

local P = {}

local function slidervalue(v)
  if type(v) == "number" then
    return string.format("%0.2f,%0.2f,3", v, v)
  end
  return v
end

P.wav_firemode = 0
P.wav_firemode_wavtxt = 0
P.wav_firemode_exo = 0
P.wav_lipsync = 0
P.wav_lipsync_group = 1
P.wav_lipsync_offset = 0
P.wav_mpslider = 0
P.wav_mpslider_group = 1
P.wav_mpslider_margin_left = 0
P.wav_mpslider_margin_right = 0
P.wav_subtitle = 0
P.wav_subtitle_group = 1
P.wav_subtitle_margin_left = 0
P.wav_subtitle_margin_right = 0
P.wav_subtitle_encoding = "sjis"
function P:wav_subtitle_replacer(s) return s end
function P:wav_subtitle_scripter(s)
  if s:sub(-2) ~= "\r\n" then
    s = s .. "\r\n"
  end
  return "<?s=[==[\r\n" .. s:gsub(']==]', ']==].."]==]"..[==[') .. ']==];require("PSDToolKit").subtitle:set(s,obj,true);s=nil?>'
end
P.wav_mergedprep = 0
function P:wav_mergedprep_scripter(s, o)
  if s:sub(-2) ~= "\r\n" then
    s = s .. "\r\n"
  end
  return "<?s=[==[\r\n" .. s:gsub(']==]', ']==].."]==]"..[==[') .. ']==];require("PSDToolKit").prep.init({' ..
    "ls_mgl=" .. o.ls_mgl .. ",ls_mgr=" .. o.ls_mgr .. "," ..
    "st_mgl=" .. o.st_mgl .. ",st_mgr=" .. o.st_mgr .. "," ..
    "sl_mgl=" .. o.sl_mgl .. ",sl_mgr=" .. o.sl_mgr .. "," ..
    '},obj,s)?>'
end
P.wav_exafinder = 0
function P:wav_examodifler_wav(exa, values, modifiers)
  exa:set("ao", "start", values.WAV_START)
  exa:set("ao", "end", values.WAV_END)
  exa:set("ao", "group", 1)
  exa:set("ao.0", "file", values.WAV_PATH)
end
function P:wav_examodifler_lipsync(exa, values, modifiers)
  exa:set("vo", "start", values.LIPSYNC_START)
  exa:set("vo", "end", values.LIPSYNC_END)
  exa:set("vo", "group", self.wav_lipsync_group)
  exa:set("vo.0", "param", "file=" .. modifiers.ENCODE_LUA_STRING(values.LIPSYNC_PATH))
end
function P:wav_examodifler_mpslider(exa, values, modifiers)
  exa:set("vo", "start", values.MPSLIDER_START)
  exa:set("vo", "end", values.MPSLIDER_END)
  exa:set("vo", "group", self.wav_mpslider_group)
  local mpslidervalues = values.USER and values.USER.mpslider or {}
  local mpsidx = 0
  for i = 0, self.wav_mpslider-1 do
    local key = "vo." .. i
    exa:set(key, "_name", i == 0 and "カスタムオブジェクト" or "アニメーション効果")
    exa:set(key, "track0", slidervalue(mpslidervalues[mpsidx+1]) or "0.00,0.00,3")
    exa:set(key, "track1", slidervalue(mpslidervalues[mpsidx+2]) or "0.00,0.00,3")
    exa:set(key, "track2", slidervalue(mpslidervalues[mpsidx+3]) or "0.00,0.00,3")
    exa:set(key, "track3", slidervalue(mpslidervalues[mpsidx+4]) or "0.00,0.00,3")
    mpsidx = mpsidx + 4
    exa:set(key, "check0", "0")
    exa:set(key, "type", "0")
    exa:set(key, "filter", "2")
    exa:set(key, "name", "多目的スライダー@PSDToolKit")
    exa:set(key, "param", "")
  end
  local key = "vo." .. self.wav_mpslider
  exa:set(key, "_name", "標準描画")
  exa:set(key, "X", "0.0")
  exa:set(key, "Y", "0.0")
  exa:set(key, "Z", "0.0")
  exa:set(key, "拡大率", "100.00")
  exa:set(key, "透明度", "100.0")
  exa:set(key, "回転", "0.00")
  exa:set(key, "blend", "0")
end
function P:wav_examodifler_subtitle(exa, values, modifiers)
  exa:set("vo", "start", values.SUBTITLE_START)
  exa:set("vo", "end", values.SUBTITLE_END)
  exa:set("vo", "group", self.wav_subtitle_group)
  local text = values.SUBTITLE_TEXT
  -- wav_subtitle が 2 の時はテキストをスクリプトとして整形する
  if self.wav_subtitle == 2 then
    text = self:wav_subtitle_scripter(text)
  end
  exa:set("vo.0", "text", modifiers.ENCODE_TEXT(text))
end
function P:wav_examodifler_mergedprep(exa, values, modifiers)
  local endlen = math.max(values.SUBTITLE_END, values.LIPSYNC_END, values.MPSLIDER_END)
  exa:set("vo", "start", math.min(values.SUBTITLE_START, values.LIPSYNC_START, values.MPSLIDER_START))
  exa:set("vo", "end", endlen)
  exa:set("vo", "group", "1")
  local idx = 0
  local key = "vo." .. idx
  exa:set(key, "_name", "テキスト")
  exa:set(key, "サイズ", "1")
  exa:set(key, "表\示速度", "0.0")
  exa:set(key, "文字毎に個別オブジェクト", "0")
  exa:set(key, "移動座標上に表\示する", "0")
  exa:set(key, "自動スクロール", "0")
  exa:set(key, "B", "0")
  exa:set(key, "I", "0")
  exa:set(key, "type", "0")
  exa:set(key, "autoadjust", "0")
  exa:set(key, "soft", "0")
  exa:set(key, "monospace", "0")
  exa:set(key, "align", "4")
  exa:set(key, "spacing_x", "0")
  exa:set(key, "spacing_y", "0")
  exa:set(key, "precision", "0")
  exa:set(key, "color", "ffffff")
  exa:set(key, "color2", "000000")
  exa:set(key, "font", "MS UI Gothic")
  exa:set(key, "text", modifiers.ENCODE_TEXT(self:wav_mergedprep_scripter(self.wav_subtitle == 2 and values.SUBTITLE_TEXT or "", {
    st_mgl = values.SUBTITLE_START - 1,
    st_mgr = endlen - values.SUBTITLE_END,
    ls_mgl = values.LIPSYNC_START - 1,
    ls_mgr = endlen - values.LIPSYNC_END,
    sl_mgl = values.MPSLIDER_START - 1,
    sl_mgr = endlen - values.MPSLIDER_END,
  })))
  idx = idx + 1

  if self.wav_lipsync == 1 and values.LIPSYNC_PATH ~= nil then
    key = "vo." .. idx
    exa:set(key, "_name", "アニメーション効果")
    exa:set(key, "track0", "0.00")
    exa:set(key, "track1", "0.00")
    exa:set(key, "track2", "0.00")
    exa:set(key, "track3", "0.00")
    exa:set(key, "check0", "0")
    exa:set(key, "type", "0")
    exa:set(key, "filter", "2")
    exa:set(key, "name", "口パク準備@PSDToolKit")
    exa:set(key, "param", "file=" .. modifiers.ENCODE_LUA_STRING(values.LIPSYNC_PATH))
    idx = idx + 1
  end

  local mpslidervalues = values.USER and values.USER.mpslider or {}
  local mpsidx = 0
  for i = 0, self.wav_mpslider-1 do
    key = "vo." .. idx
    exa:set(key, "_name", "アニメーション効果")
    exa:set(key, "track0", slidervalue(mpslidervalues[mpsidx+1]) or "0.00,0.00,3")
    exa:set(key, "track1", slidervalue(mpslidervalues[mpsidx+2]) or "0.00,0.00,3")
    exa:set(key, "track2", slidervalue(mpslidervalues[mpsidx+3]) or "0.00,0.00,3")
    exa:set(key, "track3", slidervalue(mpslidervalues[mpsidx+4]) or "0.00,0.00,3")
    mpsidx = mpsidx + 4
    exa:set(key, "check0", "0")
    exa:set(key, "type", "0")
    exa:set(key, "filter", "2")
    exa:set(key, "name", "多目的スライダー@PSDToolKit")
    exa:set(key, "param", "")
    idx = idx + 1
  end
  key = "vo." .. idx
  exa:set(key, "_name", "標準描画")
  exa:set(key, "X", "0.0")
  exa:set(key, "Y", "0.0")
  exa:set(key, "Z", "0.0")
  exa:set(key, "拡大率", "100.00")
  exa:set(key, "透明度", "100.0")
  exa:set(key, "回転", "0.00")
  exa:set(key, "blend", "0")
end

P.lab_exafinder = 0
function P:lab_examodifler(exa, values, modifiers)
  exa:set("vo", "start", values.START + 1)
  exa:set("vo", "end", values.END + 1)
  exa:set("vo", "group", 1)
  exa:set("vo.0", "text", modifiers.ENCODE_TEXT(values.LIPSYNC))
end
function P:lab_lipsync_scripter(s)
  return '<?l=' .. GCMZDrops.encodeluastring(s) .. ';require("PSDToolKit").talk:setphoneme(obj,l);l=nil?>'
end

P.srt_insertmode = 1
P.srt_encoding = "utf8"
P.srt_margin = 0
P.srt_exafinder = 0
function P:srt_examodifler(exa, values, modifiers)
  exa:set("vo", "start", values.START + 1)
  exa:set("vo", "end", values.END + 1)
  exa:set("vo", "group", 1)
  exa:set("vo.0", "text", modifiers.ENCODE_TEXT(values.SUBTITLE))
end
function P:srt_subtitle_replacer(s) return s end
function P:srt_subtitle_scripter(s)
  if s:sub(-2) ~= "\r\n" then
    s = s .. "\r\n"
  end
  return "<?s=[==[\r\n" .. s:gsub(']==]', ']==].."]==]"..[==[') .. ']==];require("PSDToolKit").subtitle:set(s,obj,true);s=nil?>'
end

P.ictalk_firemode = 1
P.ictalk_format = 3

return P
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

P.wav_firemode = 0
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
P.wav_subtitle_prefix = '<?s=[==['
function P:wav_subtitle_escape(s) return s:gsub(']==]', ']==].."]==]"..[==[') end
P.wav_subtitle_postfix = ']==];require("PSDToolKit").subtitle:set(s,obj,true);s=nil?>'
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
  for i = 0, self.wav_mpslider-1 do
    local key = "vo." .. i
    exa:set(key, "_name", i == 0 and "カスタムオブジェクト" or "アニメーション効果")
    exa:set(key, "track0", "0.00")
    exa:set(key, "track1", "0.00")
    exa:set(key, "track2", "0.00")
    exa:set(key, "track3", "0.00")
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
  exa:set(key, "透明度", "0.0")
  exa:set(key, "回転", "0.00")
  exa:set(key, "blend", "0")
end
function P:wav_examodifler_subtitle(exa, values, modifiers)
  exa:set("vo", "start", values.SUBTITLE_START)
  exa:set("vo", "end", values.SUBTITLE_END)
  exa:set("vo", "group", self.wav_subtitle_group)
  exa:set("vo.0", "text", modifiers.ENCODE_TEXT(values.SUBTITLE_TEXT))
end

P.lab_exafinder = 0
function P:lab_examodifler(exa, values, modifiers)
  exa:set("vo", "start", values.START + 1)
  exa:set("vo", "end", values.END + 1)
  exa:set("vo", "group", 1)
  exa:set("vo.0", "text", modifiers.ENCODE_TEXT(values.LIPSYNC))
end
P.lab_lipsync_prefix = '<?l='
function P:lab_lipsync_escape(s) return GCMZDrops.encodeluastring(s) end
P.lab_lipsync_postfix = ';require("PSDToolKit").talk:setphoneme(obj,l);l=nil?>'

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
P.srt_subtitle_prefix = '<?s=[==['
function P:srt_subtitle_escape(s) return s:gsub(']==]', ']==].."]==]"..[==[') end
P.srt_subtitle_postfix = ']==];require("PSDToolKit").subtitle:set(s,obj,true);s=nil?>'

P.ictalk_firemode = 1
P.ictalk_format = 3

return P
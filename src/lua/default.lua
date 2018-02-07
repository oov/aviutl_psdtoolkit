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
-- 使い方の詳しい解説はこちらを参照してください。
-- https://github.com/oov/aviutl_psdtoolkit/wiki/Setting
--
-- ============================================================

local P = {}

P.wav_firemode = 0
P.wav_insertmode = 2
P.wav_uselab = true
P.wav_groupsubtitle = true
P.wav_subtitlemargin = 0
P.wav_subtitleencoding = "sjis"
P.wav_exafinder = 0
function P:wav_examodifler_wav(exa, values, modifiers)
  exa:set("ao", "start", 1)
  exa:set("ao", "end", values.WAV_LEN)
  exa:set("ao", "group", 1)
  exa:set("ao.0", "file", values.WAV_PATH)
end
function P:wav_examodifler_lipsync(exa, values, modifiers)
  exa:set("vo", "start", 1)
  exa:set("vo", "end", values.WAV_LEN)
  exa:set("vo", "group", 1)
  exa:set("vo.0", "param", "file=" .. modifiers.ENCODE_LUA_STRING(values.LIPSYNC_PATH))
end
function P:wav_examodifler_subtitle(exa, values, modifiers)
  exa:set("vo", "start", 1)
  exa:set("vo", "end", values.SUBTITLE_LEN)
  exa:set("vo", "group", self.wav_groupsubtitle and 1 or 0)
  exa:set("vo.0", "text", modifiers.ENCODE_TEXT(values.SUBTITLE))
end
function P:wav_subtitle_replacer(s) return s end
P.wav_subtitle_prefix = '<?s=[==['
function P:wav_subtitle_escape(s) return s:gsub(']==]', ']==].."]==]"..[==[') end
P.wav_subtitle_postfix = ']==];require("PSDToolKit").subtitle:set(s, obj, true);s=nil?>'

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
P.srt_subtitle_postfix = ']==];require("PSDToolKit").subtitle:set(s, obj, true);s=nil?>'

P.ictalk_firemode = 1
P.ictalk_format = 3

return P
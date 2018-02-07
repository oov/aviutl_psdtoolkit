-- ============================================================
-- ■注意■
-- ============================================================
--
-- このファイルには設定のデフォルト値が書いてありますが、
-- これはユーザーが書き換えるためのファイルではありません。
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 設定を変更したい時はこのファイルを書き換えるのではなく、
-- `setting.lua-template` のファイル名を `setting.lua` に変更し、
-- `setting.lua` 内に書き込むことで設定してください。
--
-- 上記の手順の通りに行うことで、PSDToolKit をバージョンアップする時に
-- 設定が上書きされるのを防ぐことができます。
--
-- 使い方の詳しい解説はこちらを参照してください。
--
-- URL
--
-- ============================================================

local P = {}

-- ============================================================
-- *.wav ファイルを拡張編集に投げ込んだ時に「口パク準備」や「字幕準備」を作成する動作に関する設定
-- ============================================================

-- 発動モード - どのように投げ込むと「口パク準備」を作成するか
--   0 - *.wav ファイルをエクスプローラーで掴んだあと、
--       Shift キーを押しながら拡張編集に投げ込むと「口パク準備」を自動作成
--   1 - 拡張編集に投げ込んだ *.wav ファイルと同じ場所に同じ名前の *.txt があると「口パク準備」を自動作成
--       ただし Shift キーを押しながら拡張編集に投げ込んだ場合は「口パク準備」を作成しない
P.wav_firemode = 0

-- 挿入モード - 同じ名前の *.txt がある時にテキストなどを作成するか
--   0 - 常に「音声」と「口パク準備」のみを挿入する
--   1 - 同じ名前の *.txt がある場合はテキストとして挿入
--   2 - 同じ名前の *.txt がある場合は「字幕準備」として挿入
--
-- 「字幕準備」の使い方
--   「字幕準備」は、そのままではテキストは画面に表示されません。「字幕準備」よりも下に
--   [メディアオブジェクトの追加]→[PSDToolKit]→[テキスト　字幕表示用]
--   で「テキスト　字幕表示用」を追加し、「準備レイヤ」にレイヤー番号を指定することで表示されます。
--   装飾などは「テキスト　字幕表示用」で行うことになるため、位置や文字サイズなどを一括で変更できます。
P.wav_insertmode = 2

-- *.lab ファイル（タイミング情報ファイル）を使うか
-- *.wav と同じフォルダーに同じファイル名で存在する時は
-- 「口パク準備」で *.lab を使うかを true か false で指定
-- 有効にしても *.lab が存在しない場合は動作に影響はありません
P.wav_uselab = true

-- テキストまたは「字幕準備」も一緒にグループ化するか
-- true か false で指定
P.wav_groupsubtitle = true

-- テキストまたは「字幕準備」を指定したフレーム数だけ音声よりも長くする
P.wav_subtitlemargin = 0

-- 同じ名前の *.txt の文字エンコーディング
-- "sjis" か "utf8" で指定
-- ※ただしどちらにしても挿入前に一旦 Shift_JIS に変換されます
P.wav_subtitleencoding = "sjis"

-- 「音声」、「口パク準備」、そしてテキストまたは「字幕準備」のエイリアスファイル(*.exa)をどのように参照するか
-- この設定を使うと、ドロップされた *.wav ファイルの名前に応じて別のエイリアスファイルを使用できます。
-- エイリアスファイルは `exa` フォルダーの中に配置して下さい。
-- 以下のルールで該当するファイルが見つからない場合は wav.exa / lipsync.exa / subtitle.exa が代わりに使用されます。
--   0 - 常に同じファイルを参照する
--     ドロップされたファイルに関わらず以下のエイリアスファイルが使用されます。
--       音声: wav.exa
--       口パク準備: lipsync.exa
--       字幕: subtitle.exa
--   1 - ファイルが入っているフォルダ名を元にする
--     例: ドロップされたファイルが C:\MyFolder\TKHS_Hello_World.wav の時
--       音声: MyFolder_wav.exa
--       口パク準備: MyFolder_lipsync.exa
--       字幕: MyFolder_subtitle.exa
--   2 - ファイル名を元にする
--     例: ドロップされたファイルが C:\MyFolder\TKHS_Hello_World.wav の時
--       音声: TKHS_Hello_World_wav.exa
--       口パク準備: TKHS_Hello_World_lipsync.exa
--       字幕: TKHS_Hello_World_subtitle.exa
--   3 - ファイル名の中で _ で区切られた最初の部分を元にする
--     例: ドロップされたファイルが C:\MyFolder\TKHS_Hello_World.wav の時
--       音声: TKHS_wav.exa
--       口パク準備: TKHS_lipsync.exa
--       字幕: TKHS_subtitle.exa
--   4 - ファイル名の中で _ で区切られた2つめの部分を元にする
--     例: ドロップされたファイルが C:\MyFolder\TKHS_Hello_World.wav の時
--       音声: Hello_wav.exa
--       口パク準備: Hello_lipsync.exa
--       字幕: Hello_subtitle.exa
--   5 - ファイル名の中で _ で区切られた3つめの部分を元にする
--     例: ドロップされたファイルが C:\MyFolder\TKHS_Hello_World.wav の時
--       音声: World_wav.exa
--       口パク準備: World_lipsync.exa
--       字幕: World_subtitle.exa
P.wav_exafinder = 0

-- エイリアスファイルの改変処理
-- 一般的な用途では変更する必要はありません
P.wav_examodifler_wav = function(exa, values, modifiers)
  exa:set("ao", "start", 1)
  exa:set("ao", "end", values.WAV_LEN)
  exa:delete("ao", "length")
  exa:set("ao", "group", 1)
  exa:set("ao.0", "file", values.WAV_PATH)
end
P.wav_examodifler_lipsync = function(exa, values, modifiers)
  exa:set("vo", "start", 1)
  exa:set("vo", "end", values.WAV_LEN)
  exa:delete("vo", "length")
  exa:set("vo", "group", 1)
  exa:set("vo.0", "param", "file=" .. modifiers.ENCODE_LUA_STRING(values.LIPSYNC_PATH))
end
P.wav_examodifler_subtitle = function(exa, values, modifiers)
  exa:set("vo", "start", 1)
  exa:set("vo", "end", values.SUBTITLE_LEN)
  exa:delete("vo", "length")
  exa:set("vo", "group", P.wav_groupsubtitle and 1 or 0)
  exa:set("vo.0", "text", modifiers.ENCODE_TEXT(values.SUBTITLE))
end
  
-- wav_insertmode = 2 の時に使用される「字幕準備」用の接頭辞、接尾辞、エスケープ処理
-- 一般的な用途では変更する必要はありません
P.wav_subtitle_prefix = '<?s=[==['
P.wav_subtitle_postfix = ']==];require("PSDToolKit").subtitle:set(s, obj, true);s=nil?>'
P.wav_subtitle_escape = function(s)
  return s:gsub("]==]", ']==].."]==]"..[==[')
end

-- ============================================================
-- *.lab ファイルを拡張編集に投げ込んだ時に「口パク準備」を作成する動作に関する設定
-- ============================================================

-- 「口パク準備」のエイリアスファイル(*.exa)をどのように参照するか
-- この設定を使うと、ドロップされた *.lab ファイルの名前に応じて別のエイリアスファイルを使用できます。
-- エイリアスファイルは `exa` フォルダーの中に配置して下さい。
-- 以下のルールで該当するファイルが見つからない場合は lab.exa が代わりに使用されます。
--   0 - 常に同じファイルを参照する
--     ドロップされたファイルに関わらず以下のエイリアスファイルが使用されます。
--       lab.exa
--   1 - ファイルが入っているフォルダ名を元にする
--     例: ドロップされたファイルが C:\MyFolder\TKHS_Hello_World.lab の時
--       MyFolder_lab.exa
--   2 - ファイル名を元にする
--     例: ドロップされたファイルが C:\MyFolder\TKHS_Hello_World.lab の時
--       TKHS_Hello_World_lab.exa
--   3 - ファイル名の中で _ で区切られた最初の部分を元にする
--     例: ドロップされたファイルが C:\MyFolder\TKHS_Hello_World.lab の時
--       TKHS_lab.exa
--   4 - ファイル名の中で _ で区切られた2つめの部分を元にする
--     例: ドロップされたファイルが C:\MyFolder\TKHS_Hello_World.lab の時
--       Hello_lab.exa
--   5 - ファイル名の中で _ で区切られた3つめの部分を元にする
--     例: ドロップされたファイルが C:\MyFolder\TKHS_Hello_World.lab の時
--       World_lab.exa
P.lab_exafinder = 0

-- エイリアスファイルの改変処理
-- 一般的な用途では変更する必要はありません
P.lab_examodifler = function(exa, values, modifiers)
  exa:set("vo", "start", values.START + 1)
  exa:set("vo", "end", values.END + 1)
  exa:delete("vo", "length")
  exa:set("vo", "group", 1)
  exa:set("vo.0", "text", modifiers.ENCODE_TEXT(values.LIPSYNC))
end

P.lab_lipsync_prefix = '<?l='
P.lab_lipsync_postfix = ';require("PSDToolKit").talk:setphoneme(obj,l);l=nil?>'
P.lab_lipsync_escape = function(s)
  return GCMZDrops.encodeluastring(s)
end

-- ============================================================
-- *.srt ファイルを拡張編集に投げ込んだ時にテキストまたは「字幕準備」を作成する動作に関する設定
-- ============================================================

-- 挿入モード
--   0 - テキストとして挿入
--   1 - 「字幕準備」として挿入
--
-- 「字幕準備」の使い方
--   「字幕準備」は、そのままではテキストは画面に表示されません。「字幕準備」よりも下に
--   [メディアオブジェクトの追加]→[PSDToolKit]→[テキスト　字幕表示用]
--   で「テキスト　字幕表示用」を追加し、「準備レイヤ」にレイヤー番号を指定することで表示されます。
--   装飾などは「テキスト　字幕表示用」で行うことになるため、位置や文字サイズなどを一括で変更できます。
P.srt_insertmode = 1

-- SRTファイルの文字エンコーディング
-- "sjis" か "utf8" で指定
-- ※ただしどちらにしても挿入前に一旦 Shift_JIS に変換されます
P.srt_encoding = "utf8"

-- 字幕表示を延長
-- 秒で指定
P.srt_margin = 0

-- 「字幕準備」のエイリアスファイル(*.exa)をどのように参照するか
-- この設定を使うと、ドロップされた *.srt ファイルの名前に応じて別のエイリアスファイルを使用できます。
-- エイリアスファイルは `exa` フォルダーの中に配置して下さい。
-- 以下のルールで該当するファイルが見つからない場合は srt.exa が代わりに使用されます。
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
P.srt_exafinder = 0

-- エイリアスファイルの改変処理
-- 一般的な用途では変更する必要はありません
P.srt_examodifler = function(exa, values, modifiers)
  exa:set("vo", "start", values.START + 1)
  exa:set("vo", "end", values.END + 1)
  exa:delete("vo", "length")
  exa:set("vo", "group", 1)
  exa:set("vo.0", "text", modifiers.ENCODE_TEXT(values.SUBTITLE))
end

-- srt_insertmode = 1 の時に使用される「字幕準備」用の接頭辞、接尾辞、エスケープ処理
-- 一般的な用途では変更する必要はありません
P.srt_subtitle_prefix = '<?s=[==['
P.srt_subtitle_postfix = ']==];require("PSDToolKit").subtitle:set(s, obj, true);s=nil?>'
P.srt_subtitle_escape = function(s)
  return s:gsub("]==]", ']==].."]==]"..[==[')
end

-- ============================================================
-- Instant CTalk に関する設定
-- ※Instant CTalk は実験的な機能なので、将来的には大きく変更されるかもしれません
-- ============================================================

-- 発動モード
--   0 - ただの *.wav ファイルドロップとして処理する
--   1 - 口パク準備オブジェクトも生成する。字幕用テキストを出力した場合は字幕ファイルも作成する
P.ictalk_firemode = 1

-- ファイル名フォーマット
--   0 - こんにちは.wav
--   1 - 180116_172059_こんにちは.wav
--   2 - キャラ名_こんにちは.wav
--   3 - 180116_172059_キャラ名_こんにちは.wav
P.ictalk_format = 3

return P
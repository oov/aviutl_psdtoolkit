# `かんしくん` とは

`かんしくん` は、指定されたフォルダーを監視して、同じ名前を持つ `*.wav` ファイルと `*.txt` が新規作成または上書き保存されたときに、自動的に拡張編集へドロップするための補助プログラムです。

# 使い方

1. PSDToolKit の [環境設定ダイアログ](setting.md#環境設定ダイアログ) で、`1フレーム目に音声とテキストがある *.exo をドロップした時` にチェックを入れます。  
※このチェックを入れないと [`口パク準備`](prep.md#口パク準備) や [`字幕準備`](prep.md#字幕準備) などのオブジェクトが自動生成されません。
2. `かんしくん` フォルダーにある `setting.txt-template` のファイル名を `setting.txt` に変更します。
3. `setting.txt` をメモ帳などのテキストエディタで開き、自分の環境に合わせて設定を書き換えて上書き保存します。  
具体的な記述例については[ツール連携入門](tutorial2.md)で解説しています。
4. `forcepser.exe` を起動すると黒いウィンドウが現れ、フォルダーの監視が始まります。

監視中の対象フォルダーに名前のルールを満たす `*.wav` と `*.txt` が作成されると、拡張編集の現在のカーソル位置へファイルがドロップされます。

## basedir の考え方

`basedir` は、複数の動画制作を行う場合に切り替えを簡略化するためのものです。  
`[[rule]]` の `dir` で `%BASEDIR%` を使わないようにすれば、仕組みについて理解する必要はありません。

```txt
C:\
└ 編集データ
　　├ 動画プロジェクト1
　　│　├ キャラ1
　　│　│　├ 20201230_235959_キャラ1_こんにちは.wav
　　│　│　└ 20201230_235959_キャラ1_こんにちは.txt
　　│　└ キャラ2
　　│　　　├ 20201230_235959_キャラ2_こんにちは.wav
　　│　　　└ 20201230_235959_キャラ2_こんにちは.txt
　　└ 動画プロジェクト2
　　　　├ キャラ1
　　　　│　├ 20201231_235959_キャラ1_こんにちは.wav
　　　　│　└ 20201231_235959_キャラ1_こんにちは.txt
　　　　└ キャラ2
　　　　　　├ 20201231_235959_キャラ2_こんにちは.wav
　　　　　　└ 20201231_235959_キャラ2_こんにちは.txt
```

上記の例のように、`動画プロジェクト1` と `動画プロジェクト2` の音声ファイルをそれぞれ

- `C:\編集データ\動画プロジェクト1\キャラ1\20201230_235959_キャラ1_こんにちは.wav`
- `C:\編集データ\動画プロジェクト2\キャラ1\20201231_235959_キャラ1_こんにちは.wav`

のように管理している場合、どちらも `キャラ1\20201230_235959_キャラ1_こんにちは.wav` の部分は概ね同じような形になります。

例えば

```txt
basedir = 'C:\編集データ\動画プロジェクト1'
delta = 15.0
freshness = 5.0

[[rule]]
dir = '%BASEDIR%\キャラ1'
file = '*_キャラ1_*.wav'
encoding = 'sjis'
layer = 1

[[rule]]
dir = '%BASEDIR%\キャラ2'
file = '*_キャラ2_*.wav'
encoding = 'sjis'
layer = 6
```

上記のように  
`basedir = 'C:\編集データ\動画プロジェクト1'`  
という設定にした場合、下にある  
`dir = '%BASEDIR%\キャラ1'`  
などの指定は、`%BASEDIR%` が `basedir` に指定した内容に置き換えられるため実際には  
`dir = 'C:\編集データ\動画プロジェクト1\キャラ1'`  
になります。

`動画プロジェクト2` を編集するときには `basedir` だけを書き換えれば、後ろのルール設定は一切書き換えなくても対応できるようになります。
動画プロジェクトごとにキレイにフォルダー管理をされている場合に有用です。

# 困ったときは

## 黒いウィンドウがすぐに消えてしまう

監視対象のフォルダーが存在しないなど、setting.txt の内容に問題があると監視が開始できません。

コマンドプロンプトから `forcepser.exe` を呼び出すと、エラーメッセージが読めるかもしれません。

## `*.wav` ファイルを作っても反応しない

字幕が必要なくても、ファイルは必ず `*.txt` とセットで作成しなければいけません。

PSDToolKit の [環境設定ダイアログ](setting.md#環境設定ダイアログ) で `「字幕準備」を生成` にチェックを入れなければ、テキストファイルがあっても字幕準備は作成されません。

## 文字化けする

`setting.txt` で正しい文字コードを指定してください。
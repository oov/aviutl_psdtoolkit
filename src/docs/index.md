# PSDToolKit とは

PSDToolKit は AviUtl の拡張編集プラグイン上で PSD ファイルを扱えるようにするためのツールキットです。  
また、それに付随して必要になるであろう機能なども含んでいます。

PSDToolKit は[こちらからダウンロード](https://github.com/oov/aviutl_psdtoolkit/releases)できます。  
各バージョンの下にある `▶ Assets` をクリックすると実際のファイルへのリンクが現れます。

# 注意事項

PSDToolKit は無保証で提供されます。  
PSDToolKit を使用したこと及び使用しなかったことによるいかなる損害について、開発者は何も保証しません。

これに同意できない場合、あなたは PSDToolKit を使用することができません。

# 動作環境

動作には 64bit Windows が、快適な動作には十分な CPU 速度とメモリが必要です。  
動作確認は AviUtl version 1.10 / 拡張編集 version 0.92 で行っています。

# 同梱されるファイルについて

このツールキットは以下のコンポーネントで構成されています。

- PSDToolKit.txt
  - README
- PSDToolKit.auf
  - メニューに「ウィンドウを表示（デフォルトショートカットキー `Ctrl+W`）」を追加
  - メニューに「環境設定」を追加
  - 開いている PSD ファイル情報をプロジェクトファイルに記録
- AudioMixer.auf
  - 拡張編集で使える「[チャンネルストリップ](audio.md#チャンネルストリップ)」オーディオフィルタを追加
  - 拡張編集で使える「[Aux1 チャンネルストリップ](audio.md#Aux1_チャンネルストリップ)」オーディオフィルタを追加
  - 内部的に使用する「[マスターチャンネルストリップ](audio.md#マスターチャンネルストリップ)」を追加（常時有効）
- GCMZDrops.auf  
GCMZDrops.txt  
GCMZDrops/_entrypoint.lua  
GCMZDrops/avoiddup.lua  
GCMZDrops/example.lua  
GCMZDrops/generic.lua  
GCMZDrops/textsjis.lua  
GCMZDrops/wmvmask.lua  
GCMZDrops/dropper/example.lua  
GCMZDrops/dropper/clipboard.lua  
  - プラグイン「[ごちゃまぜドロップス](https://github.com/oov/aviutl_GCMZDrops)」
  - `GCMZDrops` フォルダーには上記以外に PSDToolKit で追加したファイルもあります
- GCMZDrops/psdtoolkit_psd.lua  
GCMZDrops/psdtoolkit_wav.lua  
GCMZDrops/psdtoolkit_srt.lua  
GCMZDrops/psdtoolkit_lab.lua  
  - PSDToolKit が使用するごちゃまぜドロップス用スクリプト
  - 拡張編集への PSD ファイルのドラッグ＆ドロップ対応や、Wave ファイルの拡張処理を行います
- ZRamPreview.auf  
ZRamPreview.auo  
ZRamPreview.exe  
ZRamPreview.txt  
script/Extram.dll  
  - プラグイン「[拡張編集RAMプレビュー](https://github.com/oov/aviutl_rampreview)」
  - 「キャッシュテキスト」スクリプトの動作に必要です
- script/CacheText.anm  
script/CacheText.lua  
キャッシュテキスト.exa  
キャッシュテキスト.txt  
  - スクリプト「[キャッシュテキスト](https://github.com/oov/aviutl_cachetext)」
  - 「[字幕表示（キャッシュ）](obj.md#字幕表示（キャッシュ）)」の動作に必要です
- かんしくん/forcepser.exe  
かんしくん/_entrypoint.lua  
かんしくん/forcepser.txt  
かんしくん/setting.txt-template  
かんしくん/setting.txt-template-old  
かんしくん/asas/asas.exe  
かんしくん/asas/asas.txt  
かんしくん/asas/changelog.txt  
かんしくん/asas/asas32.dll  
かんしくん/asas/asas64.dll
  - 音声ファイル保存時に拡張編集に投げ込む補助プログラム「[かんしくん](https://github.com/oov/forcepser)」
- script/PSDToolKit/PSDToolKitBridge.dll  
script/PSDToolKit/PSDToolKit.exe
  - PSD ファイルの描画など
  - exe を手動で起動しても基本的にはなにもできません
- script/PSDToolKit/@PSDToolKit.anm  
script/PSDToolKit/@PSDToolKit.obj  
script/PSDToolKit/@PSD.anm  
script/PSDToolKit/@subobj.anm  
script/PSDToolKit/PSDToolKit.lua  
script/PSDToolKit/default.lua
  - 拡張編集用のスクリプト
- script/PSDToolKit/setting.lua-template
  - 設定カスタマイズ用のテンプレート
- PSDToolKit/口パク準備 設定上書き.exa  
PSDToolKit/口パク準備.exa  
PSDToolKit/口パク準備（音素のみ）.exa  
PSDToolKit/多目的スライダー.exa  
PSDToolKit/字幕準備.exa  
PSDToolKit/字幕表示.exa  
PSDToolKit/字幕表示（キャッシュ）.exa  
  - 各種オブジェクトを作成するためのエイリアスファイル
- PSDToolKit説明書.html  
PSDToolKitDocs/*
  - ドキュメント

# このドキュメント内で使用されている画像について

このドキュメント内で使用されているさとうささらの画像は水梟るさんにより制作されたものです（[一次配布元](http://seiga.nicovideo.jp/seiga/im5467479)）。

また、[さとうささら](http://satosasara.com/)は音声合成ソフトウェア [CeVIO Creative Studio](http://cevio.jp/) のキャラクターです。

# 更新履歴

更新履歴は CHANGELOG を参照してください。

https://github.com/oov/aviutl_psdtoolkit/blob/master/CHANGELOG.md


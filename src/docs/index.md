# PSDToolKit とは

PSDToolKit は AviUtl の拡張編集プラグイン上で PSD ファイルを扱えるようにするためのツールキットです。  
また、それに付随して必要になるであろう機能なども含んでいます。

# 注意事項

PSDToolKit は無保証で提供されます。  
PSDToolKit を使用したこと及び使用しなかったことによるいかなる損害について、開発者は何も保証しません。

これに同意できない場合、あなたは PSDToolKit を使用することができません。

# 動作環境

動作には 64bit Windows が、快適な動作には十分な CPU 速度とメモリが必要です。  
動作確認は AviUtl version 1.00 / 拡張編集 version 0.92 のみで行っています。

# 同梱されるファイルについて

このツールキットは以下のコンポーネントで構成されています。

- PSDToolKit.txt
  - README
- PSDToolKit.auf
  - メニューに「ウィンドウを表示（デフォルトショートカットキー `Ctrl+W`）」を追加
  - プロジェクトファイルへの編集中 PSD データの保存／読み込み
- AudioMixer.auf
  - 拡張編集で使える「チャンネルストリップ」オーディオフィルタを追加
  - 拡張編集で使える「Aux1 チャンネルストリップ」オーディオフィルタを追加
  - 内部的に使用する「マスターチャンネルストリップ」を追加（常時有効）
- GCMZDrops.auf
  - 拡張編集ウィンドウへのファイルドロップを拡張する「ごちゃまぜドロップス」プラグイン
  - [別の場所で単独配布しているもの](https://github.com/oov/aviutl_GCMZDrops)を同梱しています
- GCMZDrops.txt
  - 同上
- ZRamPreview.auf
  - RAMプレビューを実現する「拡張編集RAMプレビュー」プラグイン
  - [別の場所で単独配布しているもの](https://github.com/oov/aviutl_rampreview)を同梱しています
- ZRamPreview.auo
  - 同上
- ZRamPreview.exe
  - 同上
- ZRamPreview.txt
  - 同上
- script/Extram.dll
  - 同上
- script/PSDToolKit/PSDToolKitBridge.dll
  - PSDToolKit.exe を使用した PSD ファイルの描画
  - PSDToolKit.exe からの要求に応じたクリップボード操作、エクスポートなど
  - 画像キャッシュ機構
- script/PSDToolKit/PSDToolKit.exe
  - PSDToolKitBridge.dll が使用します
  - 手動で起動しても基本的にはなにもできません
- script/PSDToolKit/@PSDToolKit.anm
  - アニメーション効果スクリプト
  - Assign
  - オブジェクト描画
  - パーツ差し替え
  - 口パク あいうえお
  - 口パク 開閉のみ
  - 目パチ
  - 字幕表示フェード
- script/PSDToolKit/@PSDToolKit.obj
  - カスタムオブジェクトスクリプト
  - 口パク準備
- script/PSDToolKit/PSDToolKit.lua
  - PSDToolKit のメイン処理が書かれたファイル
- script/PSDToolKit/setting.lua-template
  - 設定カスタマイズ用のテンプレート
- script/PSDToolKit/default.lua
  - 設定のデフォルト値
- PSDToolKit/*.exa
  - 機能を探しやすくするための AviUtl 用エイリアスファイル
  - PSDToolKitオブジェクト
  - オブジェクト描画
  - テキスト　字幕表示用
  - パーツ差し替え
  - 口パク　あいうえお
  - 口パク　開閉のみ
  - 口パク準備
  - 目パチ
- GCMZDrops/*.lua
  - ごちゃまぜドロップス用スクリプトファイル
  - ごちゃまぜドロップスに同梱されているファイル
    - _entrypoint.lua
    - avoiddup.lua
    - example.lua
    - generic.lua
    - textsjis.lua
    - dropper/example.lua
  - PSDToolKit 用のスクリプトファイル
    - psdtoolkit_psd.lua
      - 拡張編集ウィンドウに PSD ファイルをドロップ可能にする
    - psdtoolkit_wav.lua
      - *.wav ファイルをドロップで口パク準備や字幕準備を自動生成
    - psdtoolkit_srt.lua
      - *.srt ファイルをドロップで字幕準備を自動生成
    - psdtoolkit_lab.lua
      - *.lab ファイルをドロップで口パク準備を自動生成
    - dropper/psdtoolkit_ictalk.lua
      - Instant CTalk（CeVIO API による簡易的な音声作成ツール）
    - dropper/ICTalk.dll
      - Instant CTalk のダイアログ表示を行うための Lua プラグイン
- PSDToolKitDocs/*
  - ドキュメント

# 更新履歴

更新履歴は CHANGELOG を参照してください。

https://github.com/oov/aviutl_psdtoolkit/blob/master/CHANGELOG.md


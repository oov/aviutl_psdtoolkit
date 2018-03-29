# PSD アニメーション効果について

PSDToolKit で予め用意されているアニメーション効果のうち、名前の接尾辞が `@PSD` になっているものは **PSD ファイルを描画するまでの間に使用するアニメーション効果**です。

[PSD ファイルオブジェクト](obj.md#PSD_ファイルオブジェクト)には任意の個数の PSD アニメーション効果や PSD ファイルからエクスポートしたアニメーション効果を並べることができ、最後に [`描画`](#描画@PSD) を置くことで画面に描画されます。

ひとつのオブジェクトに足せるアニメーション効果は10個までですが、必要であれば下のレイヤーにアニメーション効果を単独で配置することで更に増やすこともできます。

# 描画@PSD

`描画` は準備されたデータを元に画面への描画を行うアニメーション効果です。

描画を行った時点でその PSD ファイルオブジェクトは描画完了状態になり、そのフレーム内でそれ以上のレイヤー変更などはできなくなります。

プロパティ名|説明
---|---
`反転`|画像を反転させます。<br>`-1` - PSDToolKit ウィンドウでの設定に従う<br>`0` - 反転なし<br>`1` - 左右反転<br>`2` - 上下反転<br>`3` - 上下左右反転
`縮小率`|元のサイズより画像を縮小して表示します。<br>ここで縮小しても描画負荷はほとんど下がらないため、動作が重いのを回避したい場合は PSD ファイル自体を縮小しておく必要があります。
`オフセットX`|オブジェクト内での描画位置をX方向にずらします。
`オフセットY`|オブジェクト内での描画位置をY方向にずらします。

# 目パチ@PSD

`目パチ` は目パチを実現するための `アニメーション効果` です。

少なくとも開いた目と閉じた目の２種類のレイヤーが必要です。

プロパティ名|説明
---|---
`間隔(秒)`|次に目パチのアニメーションを始めるまでの秒数です。
`速さ`|目パチの速さをフレーム単位で指定します。
`オフセット`|目パチを始めるタイミングをずらします。<br>複数キャラクターに `目パチ` を設定するとそのままでは同時に目パチしてしまうので、必要に応じて `オフセット` で調整してください。

`設定` ボタンを押すと以下のプロパティがあります。

プロパティ名|説明
---|---
`開き`<br>`ほぼ開き`<br>`半開き`<br>`ほぼ閉じ`<br>`閉じ`|表示するレイヤーを割り当てます。

## `目パチ` を Lua スクリプトから使う

以下のフォームを使うと、スクリプト内で使用可能な目パチオブジェクトを作成できます。

<form id="blinker-builder" class="ptk-script-builder">
<dl>
<dt>開き</dt><dd><input type="text" name="m4" autocomplete="off" placeholder="例: v1.!目/*開き"></dd>
<dt>ほぼ開き</dt><dd><input type="text" name="m3" autocomplete="off" placeholder="例: v1.!目/*ほぼ開き"></dd>
<dt>半開き</dt><dd><input type="text" name="m2" autocomplete="off" placeholder="例: v1.!目/*半開き"></dd>
<dt>ほぼ閉じ</dt><dd><input type="text" name="m1" autocomplete="off" placeholder="例: v1.!目/*ほぼ閉じ"></dd>
<dt>閉じ</dt><dd><input type="text" name="m0" autocomplete="off" placeholder="例: v1.!目/*閉じ"></dd>
</dl>
<dl>
<dt>間隔(秒)</dt><dd><input type="text" name="interval" autocomplete="off" value="4" placeholder="例: 4"></dd>
<dt>速さ</dt><dd><input type="text" name="speed" autocomplete="off" value="1" placeholder="例: 1"></dd>
<dt>オフセット</dt><dd><input type="text" name="offset" autocomplete="off" value="0" placeholder="例: 0"></dd>
</dl>
<dl>
<dt>出力</dt><dd><input type="text" name="output" value="" readonly="readonly"></dd><dd><button type="button" name="copy">クリップボードにコピー</button></dd>
</dl>
</form>

上記フォームで作成されるものは以下のコードと同等です。

```lua
-- 上から順に 閉じ から 開き へのパターンを入れておく
local patterns = {
  "v1.!目/*閉じ",
  "v1.!目/*ほぼ閉じ",
  "v1.!目/*半目",
  "v1.!目/*ほぼ開き",
  "v1.!目/*開き"
}
local interval = 4 -- 間隔(秒)
local speed    = 1 -- 速さ
local offset   = 0 -- オフセット
local blinker = require("PSDToolKit").Blinker.new(patterns, interval, speed, offset) -- 目パチオブジェクトを生成

-- 実際に使用する場合は以下のように PSD:addstate に与えます
-- PSD:addstate(blinker)
```

# 口パク 開閉のみ@PSD

`口パク 開閉のみ` は開閉のみのシンプルなアニメーションで、[`目パチ`](#目パチ@PSD) と同じ要領で設定できます。

プロパティ名|説明
---|---
`速さ`|口パクのアニメーション速度をフレーム単位で指定します。
`感度`|音声に対する反応の機敏さを指定します。<br>値を大きくすると細かい開閉が減りますが、反応速度が遅くなっていきます。
`口パク準備がなくても有効`|チェックを入れると、`口パク準備` のオブジェクトがない区間でも `閉じ` のパターンが適用されるようになります。

`設定` ボタンを押すと以下のプロパティがあります。

プロパティ名|説明
---|---
`開き`<br>`ほぼ開き`<br>`半開き`<br>`ほぼ閉じ`<br>`閉じ`|表示するレイヤーを割り当てます。

## `口パク 開閉のみ` を Lua スクリプトから使う

以下のフォームを使うと、スクリプト内で使用可能な口パクオブジェクトを作成できます。

<form id="lipsyncsimple-builder" class="ptk-script-builder">
<dl>
<dt>開き</dt><dd><input type="text" name="m4" autocomplete="off" placeholder="例: v1.!口/*開き"></dd>
<dt>ほぼ開き</dt><dd><input type="text" name="m3" autocomplete="off" placeholder="例: v1.!口/*ほぼ開き"></dd>
<dt>半開き</dt><dd><input type="text" name="m2" autocomplete="off" placeholder="例: v1.!口/*半開き"></dd>
<dt>ほぼ閉じ</dt><dd><input type="text" name="m1" autocomplete="off" placeholder="例: v1.!口/*ほぼ閉じ"></dd>
<dt>閉じ</dt><dd><input type="text" name="m0" autocomplete="off" placeholder="例: v1.!口/*閉じ"></dd>
</dl>
<dl>
<dt>速さ</dt><dd><input type="text" name="speed" autocomplete="off" value="1" placeholder="例: 1"></dd>
<dt>感度</dt><dd><input type="text" name="sensitivity" autocomplete="off" value="1" placeholder="例: 1"></dd>
<dt>オプション</dt><dd><label><input type="checkbox" name="alwaysapply" autocomplete="off" value="1" checked="checked">口パク準備がなくても有効</label></dd>
</dl>
<dl>
<dt>出力</dt><dd><input type="text" name="output" value="" readonly="readonly"></dd><dd><button type="button" name="copy">クリップボードにコピー</button></dd>
</dl>
</form>

上記フォームで作成されるものは以下のコードと同等です。

```lua
-- 上から順に 閉じ から 開き へのパターンを入れておく
local patterns = {
  "v1.!口/*閉じ",
  "v1.!口/*ほぼ閉じ",
  "v1.!口/*半開き",
  "v1.!口/*ほぼ開き",
  "v1.!口/*開き"
}
local speed = 1 -- 速さ
local sensitivity = 1 -- 感度
local alwaysapply = true -- 口パク準備がなくても有効
local lipsync = require("PSDToolKit").LipSyncSimple.new(patterns, speed, alwaysapply, sensitivity) -- 口パクオブジェクトを生成

-- 実際に使用する場合は以下のように PSD:addstate に与えます
-- PSD:addstate(lipsync)
```

# 口パク あいうえお@PSD

`口パク あいうえお` は母音の形に合わせてアニメーションを行うタイプのアニメーションで、`あ` / `い` / `う` / `え` / `お` / `ん` に対応した表情パターンが必要になります。

また、音声ファイルから自動で母音を認識するような機能は PSDToolKit にはないため、`口パク準備` を使う際には `*.wav` ファイルと[同名の `*.lab` ファイルを用意する](prep.md#口パク準備@PSDToolKit)か、拡張編集の[タイムラインへ直接 `*.lab` ファイルをドラッグ＆ドロップ](plugins.md#*.lab_ファイル)して母音のタイミング情報を認識できる状態にする必要があります。

プロパティ名|説明
---|---
`子音処理`|子音の処理方法を指定します。<br>`0` - すべて「ん」として処理する<br>`1` - 口を閉じる子音以外は前の母音を引き継ぐ<br>`2` - 口を閉じる子音以外は前後の母音の形をより小さいもので補間
`口パク準備がなくても有効`|チェックを入れると、`口パク準備` のオブジェクトがない区間でも `閉じ` のパターンが適用されるようになります。

`設定` ボタンを押すと以下のプロパティがあります。

プロパティ名|説明
---|---
`あ`<br>`い`<br>`う`<br>`え`<br>`お`<br>`ん`|表示するレイヤーを割り当てます。

## `口パク あいうえお` を Lua スクリプトから使う

以下のフォームを使うと、スクリプト内で使用可能な口パクオブジェクトを作成できます。

<form id="lipsynclab-builder" class="ptk-script-builder">
<dl>
<dt>あ</dt><dd><input type="text" name="a" autocomplete="off" placeholder="例: v1.!口/*あ"></dd>
<dt>い</dt><dd><input type="text" name="i" autocomplete="off" placeholder="例: v1.!口/*い"></dd>
<dt>う</dt><dd><input type="text" name="u" autocomplete="off" placeholder="例: v1.!口/*う"></dd>
<dt>え</dt><dd><input type="text" name="e" autocomplete="off" placeholder="例: v1.!口/*え"></dd>
<dt>お</dt><dd><input type="text" name="o" autocomplete="off" placeholder="例: v1.!口/*お"></dd>
<dt>ん</dt><dd><input type="text" name="N" autocomplete="off" placeholder="例: v1.!口/*ん"></dd>
</dl>
<dl>
<dt>子音処理</dt><dd><input type="text" name="mode" autocomplete="off" value="1" placeholder="例: 1"></dd>
<dt>オプション</dt><dd><label><input type="checkbox" name="alwaysapply" autocomplete="off" value="1" checked="checked">口パク準備がなくても有効</label></dd>
</dl>
<dl>
<dt>出力</dt><dd><input type="text" name="output" value="" readonly="readonly"></dd><dd><button type="button" name="copy">クリップボードにコピー</button></dd>
</dl>
</form>

上記フォームで作成されるものは以下のコードと同等です。

```lua
-- あ/い/う/え/お/ん に相当するパターンを割り当てる
local patterns = {
  a = "v1.!口/*あ",
  i = "v1.!口/*い",
  u = "v1.!口/*う",
  e = "v1.!口/*え",
  o = "v1.!口/*お",
  N = "v1.!口/*ん"
}
local mode = 1 -- 子音処理
local alwaysapply = true -- 口パク準備がなくても有効
local lipsync = require("PSDToolKit").LipSyncLab.new(patterns, mode, alwaysapply) -- 口パクオブジェクトを生成

-- 実際に使用する場合は以下のように PSD:addstate に与えます
-- PSD:addstate(lipsync)
```

## もっと柔軟に Lua スクリプトから制御する

`口パク あいうえお` を使わずに自分でスクリプトを書くこともできます。  
このアプローチなら思いのままに動かせるでしょう。

以下はスクリプトの記述例です。

```lua
-- このスクリプトを「口パク　あいうえお」の代わりに挿入することで、
-- 自前の口パク処理を実装できます
-- （例えば「スクリプト制御」を追加して dofile('script\\lipsync_example.lua') など）

-- それぞれの音素に対応した口の形
-- 必要に応じて好きな子音のパターンの追加もできます
local patterns = {
  a = "v1.!口/*あ",
  i = "v1.!口/*い",
  u = "v1.!口/*う",
  e = "v1.!口/*え",
  o = "v1.!口/*お",
  N = "v1.!口/*ん"
}

-- ちなみに CeVIO で使われる音素は以下のようなものですが、
-- lab ファイルを書き出すソフトによっては方言があるかもしれません
--   pau
--     無音部分
--   cl
--     「っ」
--   a/i/u/e/o
--     母音
--   A/I/U/E/O
--     無声化された母音
--     ※例「洗濯機」→「s/e/N/t/a/k/U/k/i」
--   N
--     「ん」
--   k/s/t/n/h/m/y/r/w/g/z/d/b/p
--   ky/sh/ch/ny/hy/my/ry/gy/j/by/py
--     子音

-- 無声化された母音は通常の母音を使いまわす
--（やらない方が自然にみえることもあります）
patterns.A = patterns.A or patterns.a or nil
patterns.I = patterns.I or patterns.i or nil
patterns.U = patterns.U or patterns.u or nil
patterns.E = patterns.E or patterns.e or nil
patterns.O = patterns.O or patterns.o or nil
-- pau は N を割り当て
patterns.pau = patterns.pau or patterns.N or nil

-- 現在のPSDオブジェクトから音量と音素情報を取得
local ts = PSD.talkstate
-- ts には以下のプロパティがあります
--   number  ts.frame        「口パク準備」基準でのフレーム数
--   number  ts.time         「口パク準備」基準での時間（秒）
--   number  ts.totalframe   「口パク準備」基準での総フレーム数
--   number  ts.totaltime    「口パク準備」基準での総時間（秒）
--   table   ts.buf          「口パク準備」で収集した音量情報（生データ）
--   number  ts.samplerate   「口パク準備」で収集した音量情報の周波数
--   number  ts.locut        「口パク準備」で設定されたローカット周波数
--   number  ts.hicut        「口パク準備」で設定されたハイカット周波数
--   number  ts.threshold    「口パク準備」で設定されたしきい値
--   number  ts.deflocut     「PSD ファイルオブジェクト」で設定されたローカット周波数
--   number  ts.defhicut     「PSD ファイルオブジェクト」で設定されたハイカット周波数
--   number  ts.defthreshold 「PSD ファイルオブジェクト」で設定されたしきい値
--   number  ts.progress     現在の音素の開始地点を0、終了地点を1とした時の現在位置
--   string  ts.cur          発音中の音素（ない時は空文字列）
--   number  ts.cur_start    音素が始まる時間（ない時は0）
--   number  ts.cur_end      音素が終わる時間（ない時は0）
--   string  ts.prev         前の音素（ない時は空文字列）
--   number  ts.prev_start   音素が始まる時間（ない時は0）
--   number  ts.prev_end     音素が終わる時間（ない時は0）
--   string  ts.next         次の音素（ない時は空文字列）
--   number  ts.next_start   音素が始まる時間（ない時は0）
--   number  ts.next_end     音素が終わる時間（ない時は0）
-- ts には以下のメソッドがあります
--   number  ts:getvolume()   現在の音量を取得します
--   boolean ts:curisvowel()  現在発音中の音素が母音の時に 1、無声化された母音の時に -1、それ以外の時に 0
--   boolean ts:previsvowel() 前の音素が母音の時に 1、無声化された母音の時に -1、それ以外の時に 0
--   boolean ts:nextisvowel() 次の音素が母音の時に 1、無声化された母音の時に -1、それ以外の時に 0
-- ただし未設定の場合など、ts が正しく取得できない場合は nil です
-- また *.lab ファイルのタイムラインへのドラッグ＆ドロップにより音素情報を直接タイムライン上に持っている場合は
-- ts.prev や ts.next などのプロパティにはデータが入りません

if ts == nil then
  -- 音素情報が取得できなかった時は「ん」のパターンを割り当て
  -- PSD:addstate を使うと現在のPSDファイルのレイヤー表示に対して状態を追記できます
  PSD:addstate(patterns.N)
else
  -- この音素用のパターンが設定されている場合はそれを使う
  if patterns[ts.cur] ~= nil and patterns[ts.cur] ~= "" then
    PSD:addstate(patterns[ts.cur])
  else
    -- 音素用のパターンが見つからなかった場合は
    -- 前の母音をそのまま引き継いでみる
    if ts.prev_end == ts.cur_start and ts:previsvowel() ~= 0 then
      PSD:addstate(patterns[ts.prev])
    else
      -- 上手く引き継げない時は「ん」でお茶を濁す
      PSD:addstate(patterns.N)
    end
  end
end
```

# パーツ差し替え@PSD

`パーツ差し替え` は拡張編集上のスライダーでレイヤーを切り替えできるようにする `アニメーション効果` で、言わば [同じ階層のレイヤーをエクスポート](tutorial.md#*.anm_ファイルを作成する) の簡易版です。

拡張編集の `パラメータ設定` ダイアログには 255 バイトの容量上限がありパーツ数が多いと上手く保存されないので、たくさんの項目を登録する場合は同じ階層のレイヤーをエクスポートを利用してください。

プロパティ名|説明
---|---
`パーツ`|設定したパーツのどれを使うかを選択するスライダーです。<br>`0` - パーツを使用しない<br>`1`～`8` - 選択したパーツを使用する

`設定` ボタンを押すと以下のプロパティがあります。

プロパティ名|説明
---|---
`1`～`8`|表示するレイヤーを割り当てます。

#!/bin/bash

OUTPUT_DIR="../../docs"
DATE=`date "+%Y-%m-%d %H:%M:%S"`
TARGET=`cat VERSION`
pushd src/docs > /dev/null
mkdir -p $OUTPUT_DIR/assets
cp assets/* $OUTPUT_DIR/assets/
pandoc.exe index.md -o $OUTPUT_DIR/index.html -f gfm -M curpage="index" -M title="はじめに" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe tutorial.md -o $OUTPUT_DIR/tutorial.html -f gfm -M curpage="tutorial" -M title="チュートリアル" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe obj.md -o $OUTPUT_DIR/obj.html -f gfm -M curpage="obj" -M title="独自のオブジェクト" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe psd.md -o $OUTPUT_DIR/psd.html -f gfm -M curpage="psd" -M title="PSD アニメーション効果" -M author="oov" -M date="$DATE" -M target="$TARGET" -M script="assets/script-builder.js" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe prep.md -o $OUTPUT_DIR/prep.html -f gfm -M curpage="prep" -M title="準備オブジェクト" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe audio.md -o $OUTPUT_DIR/audio.html -f gfm -M curpage="audio" -M title="音声フィルタ" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe pfv.md -o $OUTPUT_DIR/pfv.html -f gfm -M curpage="pfv" -M title="PSDTool のお気に入りを使う" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe subobj.md -o $OUTPUT_DIR/subobj.html -f gfm -M curpage="subobj" -M title="subobj アニメーション効果" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe gcmzdrops.md -o $OUTPUT_DIR/gcmzdrops.html -f gfm -M curpage="gcmzdrops" -M title="ドラッグ＆ドロップ拡張" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe preview.md -o $OUTPUT_DIR/preview.html -f gfm -M curpage="preview" -M title="プレビューとコマ落ち" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe faq.md -o $OUTPUT_DIR/faq.html -f gfm -M curpage="faq" -M title="よくある質問" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe setting.md -o $OUTPUT_DIR/setting.html -f gfm -M curpage="setting" -M title="設定" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
popd > /dev/null

cp src/docs/rootindex.html bin/PSDToolKit説明書.html
rm -rf bin/PSDToolKitDocs
cp -r docs bin/PSDToolKitDocs

#!/bin/bash

mkdir -p bin/PSDToolKitDocs/assets
cp src/docs/assets/* bin/PSDToolKitDocs/assets/

DATE=`date "+%Y-%m-%d %H:%M:%S"`
TARGET=`cat VERSION`
pushd src/docs > /dev/null
pandoc.exe index.md -o ../../bin/PSDToolKitDocs/index.html -f gfm -M curpage="index" -M title="はじめに" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe tutorial.md -o ../../bin/PSDToolKitDocs/tutorial.html -f gfm -M curpage="tutorial" -M title="チュートリアル" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe obj.md -o ../../bin/PSDToolKitDocs/obj.html -f gfm -M curpage="obj" -M title="独自のオブジェクト" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe psd.md -o ../../bin/PSDToolKitDocs/psd.html -f gfm -M curpage="psd" -M title="PSD アニメーション効果" -M author="oov" -M date="$DATE" -M target="$TARGET" -M script="assets/script-builder.js" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe prep.md -o ../../bin/PSDToolKitDocs/prep.html -f gfm -M curpage="prep" -M title="準備オブジェクト" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe audio.md -o ../../bin/PSDToolKitDocs/audio.html -f gfm -M curpage="audio" -M title="音声フィルタ" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe pfv.md -o ../../bin/PSDToolKitDocs/pfv.html -f gfm -M curpage="pfv" -M title="PSDTool のお気に入りを使う" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe subobj.md -o ../../bin/PSDToolKitDocs/subobj.html -f gfm -M curpage="subobj" -M title="subobj アニメーション効果" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe gcmzdrops.md -o ../../bin/PSDToolKitDocs/gcmzdrops.html -f gfm -M curpage="gcmzdrops" -M title="ドラッグ＆ドロップ拡張" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe preview.md -o ../../bin/PSDToolKitDocs/preview.html -f gfm -M curpage="preview" -M title="プレビューとコマ落ち" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe faq.md -o ../../bin/PSDToolKitDocs/faq.html -f gfm -M curpage="faq" -M title="よくある質問" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
pandoc.exe setting.md -o ../../bin/PSDToolKitDocs/setting.html -f gfm -M curpage="setting" -M title="設定" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --base-header-level=2 --toc-depth=5
cp rootindex.html ../../bin/PSDToolKit説明書.html
popd > /dev/null
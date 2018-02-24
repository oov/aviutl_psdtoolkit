#!/bin/bash

mkdir -p bin/PSDToolKitDocs/assets
cp src/docs/assets/* bin/PSDToolKitDocs/assets/

DATE=`date "+%Y-%m-%d %H:%M:%S"`
TARGET=`cat VERSION`
pushd src/docs > /dev/null
pandoc.exe menu.md -o ../../bin/PSDToolKitDocs/menu.html -M title="メニュー" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua
pandoc.exe index.md -o ../../bin/PSDToolKitDocs/index.html -f gfm -M title="はじめに" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --include-before-body=../../bin/PSDToolKitDocs/menu.html --base-header-level=2 --toc-depth=5
pandoc.exe install.md -o ../../bin/PSDToolKitDocs/install.html -f gfm -M title="インストール／アンインストール" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --include-before-body=../../bin/PSDToolKitDocs/menu.html --base-header-level=2 --toc-depth=5
pandoc.exe howtouse.md -o ../../bin/PSDToolKitDocs/howtouse.html -f gfm -M title="使い方" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --include-before-body=../../bin/PSDToolKitDocs/menu.html --base-header-level=2 --toc-depth=5
pandoc.exe psd.md -o ../../bin/PSDToolKitDocs/psd.html -f gfm -M title="PSD ファイルの基本" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --include-before-body=../../bin/PSDToolKitDocs/menu.html --base-header-level=2 --toc-depth=5
pandoc.exe pfv.md -o ../../bin/PSDToolKitDocs/pfv.html -f gfm -M title="PSDTool のお気に入りを使う" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --include-before-body=../../bin/PSDToolKitDocs/menu.html --base-header-level=2 --toc-depth=5
pandoc.exe faview.md -o ../../bin/PSDToolKitDocs/faview.html -f gfm -M title="シンプルビューの活用" -M author="oov" -M date="$DATE" -M target="$TARGET" -M script="assets/script-builder.js" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --include-before-body=../../bin/PSDToolKitDocs/menu.html --base-header-level=2 --toc-depth=5
pandoc.exe audio.md -o ../../bin/PSDToolKitDocs/audio.html -f gfm -M title="音声と口パク準備と字幕" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --include-before-body=../../bin/PSDToolKitDocs/menu.html --base-header-level=2 --toc-depth=5
pandoc.exe preview.md -o ../../bin/PSDToolKitDocs/preview.html -f gfm -M title="プレビューとコマ落ち" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --include-before-body=../../bin/PSDToolKitDocs/menu.html --base-header-level=2 --toc-depth=5
pandoc.exe setting.md -o ../../bin/PSDToolKitDocs/setting.html -f gfm -M title="設定について" -M author="oov" -M date="$DATE" -M target="$TARGET" --lua-filter filter/remove-colgroup.lua --lua-filter filter/link-md2html.lua --template=template.html -s --toc --include-before-body=../../bin/PSDToolKitDocs/menu.html --base-header-level=2 --toc-depth=5
rm ../../bin/PSDToolKitDocs/menu.html
popd > /dev/null
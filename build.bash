#!/bin/bash

mkdir bin bin/PSDToolKit bin/script bin/script/PSDToolKit

# copy readme
sed 's/\r$//' README.md | sed 's/$/\r/' > bin/README.txt

# copy alias files
sed 's/\r$//' 'src/exa/NewObject.exa' | sed 's/$/\r/' > 'bin/PSDToolKit/PSDToolKitオブジェクト.exa'
sed 's/\r$//' 'src/exa/Render.exa' | sed 's/$/\r/' > 'bin/PSDToolKit/オブジェクト描画.exa'
sed 's/\r$//' 'src/exa/Override.exa' | sed 's/$/\r/' > 'bin/PSDToolKit/パーツ差し替え.exa'
sed 's/\r$//' 'src/exa/Blink.exa' | sed 's/$/\r/' > 'bin/PSDToolKit/目パチ.exa'
sed 's/\r$//' 'src/exa/TalkDetector.exa' | sed 's/$/\r/' > 'bin/PSDToolKit/口パク準備.exa'
sed 's/\r$//' 'src/exa/LipSync.exa' | sed 's/$/\r/' > 'bin/PSDToolKit/口パク　開閉のみ.exa'
sed 's/\r$//' 'src/exa/LipSyncVowels.exa' | sed 's/$/\r/' > 'bin/PSDToolKit/口パク　あいうえお.exa'

# copy script files
sed 's/\r$//' 'src/lua/PSDToolKitLib.lua' | sed 's/$/\r/' > 'bin/script/PSDToolKit/PSDToolKitLib.lua'
sed 's/\r$//' 'src/lua/@PSDToolKit.anm' | sed 's/$/\r/' > 'bin/script/PSDToolKit/@PSDToolKit.anm'
sed 's/\r$//' 'src/lua/@PSDToolKit.obj' | sed 's/$/\r/' > 'bin/script/PSDToolKit/@PSDToolKit.obj'

# build src/go/assets/bindata.go
pushd src/go/assets
go.exe generate
popd

# build PSDToolKit.exe
pushd src/go
go.exe build -ldflags="-s" -o ../../bin/script/PSDToolKit/PSDToolKit.exe
popd

# build lazarus projects
cmd.exe /c C:/lazarus/lazbuild.exe --build-all src/lazarus/auf.lpi src/lazarus/luadll.lpi

# install
# mkdir aviutl/PSDToolKit aviutl/script aviutl/script/PSDToolKit
# cp bin/*.auf aviutl/
# cp bin/PSDToolKit/* aviutl/PSDToolKit/
# cp bin/script/PSDToolKit/* aviutl/script/PSDToolKit/

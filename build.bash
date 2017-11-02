#!/bin/bash

mkdir bin bin/PSDToolKit bin/script bin/script/PSDToolKit bin/GCMZDrops bin/GCMZDrops/exa

# copy readme
sed 's/\r$//' README.md | sed 's/$/\r/' > bin/README.txt

# update version string
VERSION='v0.1.2rc5'
GITHASH=`git rev-parse --short HEAD`
cat << EOS | sed 's/\r$//' | sed 's/$/\r/' > 'src/lazarus/ver.pas'
unit Ver;

{\$mode objfpc}{\$H+}
{\$CODEPAGE UTF-8}

interface

const
  Version = '$VERSION ( $GITHASH )';

implementation

end.
EOS
cat << EOS | sed 's/\r$//' | sed 's/$/\r/' > 'src/go/ver.go'
package main

const version = "$VERSION ( $GITHASH )"
EOS

# copy alias files
sed 's/\r$//' 'src/exa/NewObject.exa' | sed 's/$/\r/' > 'bin/PSDToolKit/PSDToolKitオブジェクト.exa'
sed 's/\r$//' 'src/exa/Render.exa' | sed 's/$/\r/' > 'bin/PSDToolKit/オブジェクト描画.exa'
sed 's/\r$//' 'src/exa/Override.exa' | sed 's/$/\r/' > 'bin/PSDToolKit/パーツ差し替え.exa'
sed 's/\r$//' 'src/exa/Blink.exa' | sed 's/$/\r/' > 'bin/PSDToolKit/目パチ.exa'
sed 's/\r$//' 'src/exa/TalkDetector.exa' | sed 's/$/\r/' > 'bin/PSDToolKit/口パク準備.exa'
sed 's/\r$//' 'src/exa/LipSync.exa' | sed 's/$/\r/' > 'bin/PSDToolKit/口パク　開閉のみ.exa'
sed 's/\r$//' 'src/exa/LipSyncVowels.exa' | sed 's/$/\r/' > 'bin/PSDToolKit/口パク　あいうえお.exa'
sed 's/\r$//' 'src/exa/Subtitle.exa' | sed 's/$/\r/' > 'bin/PSDToolKit/テキスト　字幕表示用.exa'
sed 's/\r$//' 'src/exa/SubtitleStep.exa' | sed 's/$/\r/' > 'bin/PSDToolKit/テキスト　字幕表示用・文字送り.exa'

# copy script files
sed 's/\r$//' 'src/lua/PSDToolKitLib.lua' | sed 's/$/\r/' > 'bin/script/PSDToolKit/PSDToolKitLib.lua'
sed 's/\r$//' 'src/lua/@PSDToolKit.anm' | sed 's/$/\r/' > 'bin/script/PSDToolKit/@PSDToolKit.anm'
sed 's/\r$//' 'src/lua/@PSDToolKit.obj' | sed 's/$/\r/' > 'bin/script/PSDToolKit/@PSDToolKit.obj'

# copy GCMZDrops script files
sed 's/\r$//' 'src/lua/GCMZDrops/psd.lua' | sed 's/$/\r/' > 'bin/GCMZDrops/psdtoolkit_psd.lua'
sed 's/\r$//' 'src/lua/GCMZDrops/wav.lua' | sed 's/$/\r/' > 'bin/GCMZDrops/psdtoolkit_wav.lua'
sed 's/\r$//' 'src/lua/GCMZDrops/srt.lua' | sed 's/$/\r/' > 'bin/GCMZDrops/psdtoolkit_srt.lua'
sed 's/\r$//' 'src/lua/GCMZDrops/exa/lip.exa' | sed 's/$/\r/' > 'bin/GCMZDrops/exa/lip.exa'
sed 's/\r$//' 'src/lua/GCMZDrops/exa/text.exa' | sed 's/$/\r/' > 'bin/GCMZDrops/exa/text.exa'
sed 's/\r$//' 'src/lua/GCMZDrops/exa/wav.exa' | sed 's/$/\r/' > 'bin/GCMZDrops/exa/wav.exa'
sed 's/\r$//' 'src/lua/GCMZDrops/exa/srt.exa' | sed 's/$/\r/' > 'bin/GCMZDrops/exa/srt.exa'

# build src/go/assets/bindata.go
pushd src/go/assets
go.exe generate
popd

# build PSDToolKit.exe
pushd src/go
go.exe build -x -tags gdip -ldflags="-s" -o ../../bin/script/PSDToolKit/PSDToolKit.exe
popd

# build lazarus projects
cmd.exe /c C:/lazarus/lazbuild.exe --build-all src/lazarus/luadll.lpi
cmd.exe /c C:/lazarus/lazbuild.exe --build-all src/lazarus/AssistPlugin.lpi
cmd.exe /c C:/lazarus/lazbuild.exe --build-all src/lazarus/AudioMixerPlugin.lpi

# copy GCMZDrops release version
# cp ../aviutl_gcmzdrops/bin/*.auf bin/
# cp ../aviutl_gcmzdrops/bin/GCMZDrops/*.lua bin/GCMZDrops/

# install
# mkdir aviutl/PSDToolKit aviutl/script aviutl/script/PSDToolKit aviutl/GCMZDrops aviutl/GCMZDrops/exa
# cp bin/*.auf aviutl/
# cp bin/PSDToolKit/* aviutl/PSDToolKit/
# cp bin/script/PSDToolKit/* aviutl/script/PSDToolKit/
# cp bin/GCMZDrops/* aviutl/GCMZDrops/
# cp bin/GCMZDrops/exa/* aviutl/GCMZDrops/exa/

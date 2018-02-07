#!/bin/bash

mkdir bin bin/PSDToolKit bin/script bin/script/PSDToolKit bin/script/PSDToolKit/exa bin/GCMZDrops bin/GCMZDrops/dropper

# copy readme
sed 's/\r$//' README.md | sed 's/$/\r/' > bin/README.txt

# update version string
VERSION='v0.1.3'
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
sed 's/\r$//' 'src/lua/PSDToolKit.lua' | sed 's/$/\r/' > 'bin/script/PSDToolKit/PSDToolKit.lua'
sed 's/\r$//' 'src/lua/PSDToolKitIndex.lua' | sed 's/$/\r/' > 'bin/script/PSDToolKit.lua'
sed 's/\r$//' 'src/lua/@PSDToolKit.anm' | sed 's/$/\r/' > 'bin/script/PSDToolKit/@PSDToolKit.anm'
sed 's/\r$//' 'src/lua/@PSDToolKit.obj' | sed 's/$/\r/' > 'bin/script/PSDToolKit/@PSDToolKit.obj'
sed 's/\r$//' 'src/lua/setting-default.lua' | sed 's/$/\r/' > 'bin/script/PSDToolKit/setting-default.lua'
sed 's/\r$//' 'src/lua/setting.lua-template' | sed 's/$/\r/' > 'bin/script/PSDToolKit/setting.lua-template'

# copy GCMZDrops script files
sed 's/\r$//' 'src/lua/GCMZDrops/psd.lua' | sed 's/$/\r/' > 'bin/GCMZDrops/psdtoolkit_psd.lua'
sed 's/\r$//' 'src/lua/GCMZDrops/wav.lua' | sed 's/$/\r/' > 'bin/GCMZDrops/psdtoolkit_wav.lua'
sed 's/\r$//' 'src/lua/GCMZDrops/srt.lua' | sed 's/$/\r/' > 'bin/GCMZDrops/psdtoolkit_srt.lua'
sed 's/\r$//' 'src/lua/GCMZDrops/lab.lua' | sed 's/$/\r/' > 'bin/GCMZDrops/psdtoolkit_lab.lua'
sed 's/\r$//' 'src/lua/GCMZDrops/dropper/ictalk.lua' | sed 's/$/\r/' > 'bin/GCMZDrops/dropper/psdtoolkit_ictalk.lua'
sed 's/\r$//' 'src/lua/exa/lipsync.exa' | sed 's/$/\r/' > 'bin/script/PSDToolKit/exa/lipsync.exa'
sed 's/\r$//' 'src/lua/exa/subtitle.exa' | sed 's/$/\r/' > 'bin/script/PSDToolKit/exa/subtitle.exa'
sed 's/\r$//' 'src/lua/exa/wav.exa' | sed 's/$/\r/' > 'bin/script/PSDToolKit/exa/wav.exa'
sed 's/\r$//' 'src/lua/exa/srt.exa' | sed 's/$/\r/' > 'bin/script/PSDToolKit/exa/srt.exa'
sed 's/\r$//' 'src/lua/exa/lab.exa' | sed 's/$/\r/' > 'bin/script/PSDToolKit/exa/lab.exa'

# build src/go/assets/bindata.go
pushd src/go/assets
go.exe generate
popd

# build PSDToolKit.exe
pushd src/go
rsrc.exe -ico assets/data/icon.ico -arch=amd64 -o PSDToolKit.syso
go.exe build -x -tags gdip -ldflags="-s" -o ../../bin/script/PSDToolKit/PSDToolKit.exe
popd

# build lazarus projects
cmd.exe /c C:/lazarus/lazbuild.exe --build-all src/lazarus/PSDToolKitBridge.lpi
cmd.exe /c C:/lazarus/lazbuild.exe --build-all src/lazarus/AssistPlugin.lpi
cmd.exe /c C:/lazarus/lazbuild.exe --build-all src/lazarus/AudioMixerPlugin.lpi
cmd.exe /c C:/lazarus/lazbuild.exe --build-all src/lazarus/ictalk/ictalk.lpi

# copy GCMZDrops release version
# cp ../aviutl_gcmzdrops/bin/*.auf bin/
# cp ../aviutl_gcmzdrops/bin/GCMZDrops/*.lua bin/GCMZDrops/

# install
# mkdir aviutl/PSDToolKit aviutl/script aviutl/script/PSDToolKit aviutl/script/PSDToolKit/exa aviutl/GCMZDrops aviutl/GCMZDrops/dropper
# cp bin/*.auf aviutl/
# cp bin/PSDToolKit/* aviutl/PSDToolKit/
# cp bin/script/* aviutl/script/
# cp bin/script/PSDToolKit/* aviutl/script/PSDToolKit/
# cp bin/script/PSDToolKit/exa/* aviutl/script/PSDToolKit/exa/
# cp bin/GCMZDrops/* aviutl/GCMZDrops/
# cp bin/GCMZDrops/dropper/* aviutl/GCMZDrops/dropper/

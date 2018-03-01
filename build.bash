#!/bin/bash

mkdir -p bin/PSDToolKit bin/script/PSDToolKit/exa bin/GCMZDrops/dropper

# copy readme
sed 's/\r$//' README.md | sed 's/$/\r/' > bin/PSDToolKit.txt

# update version string
VERSION='v0.2beta7'
GITHASH=`git rev-parse --short HEAD`
echo -n "$VERSION ( $GITHASH )" > "VERSION"
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
sed 's/\r$//' 'src/exa/PrepSubtitle.exa' | sed 's/$/\r/' > 'bin/PSDToolKit/字幕準備.exa'
sed 's/\r$//' 'src/exa/MultiPurposeSlider.exa' | sed 's/$/\r/' > 'bin/PSDToolKit/多目的スライダー.exa'

# copy script files
sed 's/\r$//' 'src/lua/PSDToolKit.lua' | sed 's/$/\r/' > 'bin/script/PSDToolKit/PSDToolKit.lua'
sed 's/\r$//' 'src/lua/PSDToolKitIndex.lua' | sed 's/$/\r/' > 'bin/script/PSDToolKit.lua'
sed 's/\r$//' 'src/lua/@PSDToolKit.anm' | sed 's/$/\r/' > 'bin/script/PSDToolKit/@PSDToolKit.anm'
sed 's/\r$//' 'src/lua/@PSDToolKit.obj' | sed 's/$/\r/' > 'bin/script/PSDToolKit/@PSDToolKit.obj'
sed 's/\r$//' 'src/lua/@subobj.anm' | sed 's/$/\r/' > 'bin/script/PSDToolKit/@subobj.anm'
sed 's/\r$//' 'src/lua/default.lua' | sed 's/$/\r/' > 'bin/script/PSDToolKit/default.lua'
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
pushd src/go/assets > /dev/null
go.exe generate
popd > /dev/null

# build PSDToolKit.exe
pushd src/go > /dev/null
rsrc.exe -ico assets/datasrc/icon.ico -arch=amd64 -o PSDToolKit.syso
env.exe CGO_LDFLAGS_ALLOW="-Wl,--allow-multiple-definition" go.exe build -x -tags gdip -ldflags="-s" -o ../../bin/script/PSDToolKit/PSDToolKit.exe
popd > /dev/null

# build lazarus projects
cmd.exe /c C:/lazarus/lazbuild.exe --build-all src/lazarus/PSDToolKitBridge.lpi
cmd.exe /c C:/lazarus/lazbuild.exe --build-all src/lazarus/AssistPlugin.lpi
cmd.exe /c C:/lazarus/lazbuild.exe --build-all src/lazarus/AudioMixerPlugin.lpi
cmd.exe /c C:/lazarus/lazbuild.exe --build-all src/lazarus/ictalk/ictalk.lpi

# copy GCMZDrops release version
# cp ../aviutl_gcmzdrops/bin/GCMZDrops.* bin/
# cp ../aviutl_gcmzdrops/bin/GCMZDrops/*.lua bin/GCMZDrops/
# cp ../aviutl_gcmzdrops/bin/GCMZDrops/dropper/*.lua bin/GCMZDrops/dropper/

# copy RamPreview release version
# cp ../aviutl_rampreview/bin/ZRamPreview.* bin/
# cp ../aviutl_rampreview/bin/script/Extram.dll bin/script/

# install
# mkdir -p aviutl/PSDToolKit aviutl/script/PSDToolKit/exa aviutl/GCMZDrops/dropper
# cp bin/*.auf aviutl/
# cp bin/*.auo aviutl/
# cp bin/*.exe aviutl/
# cp bin/PSDToolKit/*.* aviutl/PSDToolKit/
# cp bin/script/*.* aviutl/script/
# cp bin/script/PSDToolKit/*.* aviutl/script/PSDToolKit/
# cp bin/script/PSDToolKit/exa/*.* aviutl/script/PSDToolKit/exa/
# cp bin/GCMZDrops/*.* aviutl/GCMZDrops/
# cp bin/GCMZDrops/dropper/*.* aviutl/GCMZDrops/dropper/

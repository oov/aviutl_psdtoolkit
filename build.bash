#!/bin/bash

make () {
  TARGET=$1
  TARGET_LANG=$2
  mkdir -p $TARGET/PSDToolKit $TARGET/script/PSDToolKit/exa $TARGET/GCMZDrops/dropper $TARGET/かんしくん/asas

  # copy readme
  sed 's/\r$//' README.md | sed 's/$/\r/' > $TARGET/PSDToolKit.txt

  # update version string
  VERSION='v0.2beta53'
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

  # copy GCMZDrops script files
  if [ "$TARGET_LANG" = "EN" ]; then
    SUFFIX=_en
  fi

  # copy alias files
  sed 's/\r$//' 'src/exa/TalkDetector'$SUFFIX'.exa' | sed 's/$/\r/' > $TARGET'/PSDToolKit/口パク準備.exa'
  sed 's/\r$//' 'src/exa/Phoneme'$SUFFIX'.exa' | sed 's/$/\r/' > $TARGET'/PSDToolKit/口パク準備（音素のみ）.exa'
  sed 's/\r$//' 'src/exa/Subtitle'$SUFFIX'.exa' | sed 's/$/\r/' > $TARGET'/PSDToolKit/字幕表示.exa'
  sed 's/\r$//' 'src/exa/SubtitleFast'$SUFFIX'.exa' | sed 's/$/\r/' > $TARGET'/PSDToolKit/字幕表示（キャッシュ）.exa'
  sed 's/\r$//' 'src/exa/PrepSubtitle'$SUFFIX'.exa' | sed 's/$/\r/' > $TARGET'/PSDToolKit/字幕準備.exa'
  sed 's/\r$//' 'src/exa/MultiPurposeSlider'$SUFFIX'.exa' | sed 's/$/\r/' > $TARGET'/PSDToolKit/多目的スライダー.exa'

  # copy script files
  sed 's/\r$//' 'src/lua/PSDToolKit.lua' | sed 's/$/\r/' > $TARGET'/script/PSDToolKit/PSDToolKit.lua'
  sed 's/\r$//' 'src/lua/PSDToolKitIndex.lua' | sed 's/$/\r/' > $TARGET'/script/PSDToolKit.lua'
  sed 's/\r$//' 'src/lua/json.lua' | sed 's/$/\r/' > $TARGET'/script/PSDToolKit/json.lua'
  sed 's/\r$//' 'src/lua/@PSD.anm' | sed 's/$/\r/' > $TARGET'/script/PSDToolKit/@PSD.anm'
  sed 's/\r$//' 'src/lua/@PSDToolKit.anm' | sed 's/$/\r/' > $TARGET'/script/PSDToolKit/@PSDToolKit.anm'
  sed 's/\r$//' 'src/lua/@PSDToolKit.obj' | sed 's/$/\r/' > $TARGET'/script/PSDToolKit/@PSDToolKit.obj'
  sed 's/\r$//' 'src/lua/@subobj.anm' | sed 's/$/\r/' > $TARGET'/script/PSDToolKit/@subobj.anm'
  sed 's/\r$//' 'src/lua/default.lua' | sed 's/$/\r/' > $TARGET'/script/PSDToolKit/default.lua'
  sed 's/\r$//' 'src/lua/setting.lua-template' | sed 's/$/\r/' > $TARGET'/script/PSDToolKit/setting.lua-template'

  sed 's/\r$//' 'src/lua/GCMZDrops/psd.lua' | sed 's/$/\r/' > $TARGET'/GCMZDrops/psdtoolkit_psd.lua'
  sed 's/\r$//' 'src/lua/GCMZDrops/wav.lua' | sed 's/$/\r/' > $TARGET'/GCMZDrops/psdtoolkit_wav.lua'
  sed 's/\r$//' 'src/lua/GCMZDrops/srt.lua' | sed 's/$/\r/' > $TARGET'/GCMZDrops/psdtoolkit_srt.lua'
  sed 's/\r$//' 'src/lua/GCMZDrops/lab.lua' | sed 's/$/\r/' > $TARGET'/GCMZDrops/psdtoolkit_lab.lua'
  sed 's/\r$//' 'src/lua/exa/lipsync'$SUFFIX'.exa' | sed 's/$/\r/' > $TARGET'/script/PSDToolKit/exa/lipsync.exa'
  sed 's/\r$//' 'src/lua/exa/subtitle'$SUFFIX'.exa' | sed 's/$/\r/' > $TARGET'/script/PSDToolKit/exa/subtitle.exa'
  sed 's/\r$//' 'src/lua/exa/wav'$SUFFIX'.exa' | sed 's/$/\r/' > $TARGET'/script/PSDToolKit/exa/wav.exa'
  sed 's/\r$//' 'src/lua/exa/srt'$SUFFIX'.exa' | sed 's/$/\r/' > $TARGET'/script/PSDToolKit/exa/srt.exa'
  sed 's/\r$//' 'src/lua/exa/lab'$SUFFIX'.exa' | sed 's/$/\r/' > $TARGET'/script/PSDToolKit/exa/lab.exa'

  # build src/go/assets/bindata.go
  pushd src/go/assets > /dev/null
  go.exe generate
  popd > /dev/null

  # build PSDToolKit.exe
  pushd src/go > /dev/null
  rsrc.exe -ico assets/datasrc/icon.ico -arch=amd64 -o PSDToolKit.syso
  env.exe go.exe build -x -tags gdip -ldflags="-s" -o ../../$TARGET/script/PSDToolKit/PSDToolKit.exe
  popd > /dev/null

  # build lazarus projects
  cmd.exe /c C:/lazarus/lazbuild.exe --build-all src/lazarus/PSDToolKitBridge.lpi
  cmd.exe /c C:/lazarus/lazbuild.exe --build-all src/lazarus/AssistPlugin.lpi
  cmd.exe /c C:/lazarus/lazbuild.exe --build-all src/lazarus/AudioMixerPlugin.lpi

  # copy GCMZDrops release version
  cp ../aviutl_gcmzdrops/bin/GCMZDrops.* $TARGET/
  cp ../aviutl_gcmzdrops/bin/GCMZDrops/*.lua $TARGET/GCMZDrops/
  cp ../aviutl_gcmzdrops/bin/GCMZDrops/dropper/*.lua $TARGET/GCMZDrops/dropper/

  # copy RamPreview release version
  cp ../aviutl_rampreview/bin/ZRamPreview.* $TARGET/
  cp ../aviutl_rampreview/bin/script/Extram.dll $TARGET/script/

  # copy forcepser release version
  cp ../forcepser/bin/forcepser.* $TARGET/かんしくん/
  cp ../forcepser/bin/setting.txt-template $TARGET/かんしくん/
  cp ../forcepser/bin/setting.txt-template-old $TARGET/かんしくん/
  cp ../forcepser/bin/_entrypoint.lua $TARGET/かんしくん/
  cp ../forcepser/bin/asas/* $TARGET/かんしくん/asas/

  # copy CacheText release version
  cp ../aviutl_cachetext/bin/script/CacheText.* $TARGET/script/
  cp ../aviutl_cachetext/bin/キャッシュテキスト.* $TARGET/
}

make bin JP
make bin_en EN
cp bin/PSDToolKit.auf bin_en/PSDToolKit.auf
cp bin/script/PSDToolKit/PSDToolKitBridge.dll bin_en/script/PSDToolKit/PSDToolKitBridge.dll
cp bin/AudioMixer.auf bin_en/AudioMixer.auf

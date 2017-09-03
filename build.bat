@rem 雀の往来
mkdir bin

@rem copy readme
copy /B /Y README.md bin\README.txt

@rem copy alias files
mkdir bin\PSDToolKit
copy /B /Y src\exa\NewObject.exa bin\PSDToolKit\PSDToolKitオブジェクト.exa
copy /B /Y src\exa\Render.exa bin\PSDToolKit\オブジェクト描画.exa
copy /B /Y src\exa\SimpleView.exa bin\PSDToolKit\シンプルビュー.exa
copy /B /Y src\exa\Blink.exa bin\PSDToolKit\目パチ.exa
copy /B /Y src\exa\TalkDetector.exa bin\PSDToolKit\口パク準備.exa
copy /B /Y src\exa\LipSync.exa "bin\PSDToolKit\口パク　開閉のみ.exa"
copy /B /Y src\exa\LipSyncVowels.exa "bin\PSDToolKit\口パク　あいうえお.exa"

@rem copy script files
mkdir bin\script
mkdir bin\script\PSDToolKit
copy /B /Y src\lua\PSDToolKitLib.lua bin\script\PSDToolKit\
copy /B /Y src\lua\@PSDToolKit.anm bin\script\PSDToolKit\
copy /B /Y src\lua\@PSDToolKit.obj bin\script\PSDToolKit\

@rem build src/go/assets/bindata.go
cd src/go/assets
go generate
cd ../../..

@rem build PSDToolKit.exe
cd src/go
go build -ldflags="-s" -o ../../bin/script/PSDToolKit/PSDToolKit.exe
cd ../../

@rem build lazarus projects
C:\lazarus\lazbuild.exe --build-all src/lazarus/auf.lpi src/lazarus/luadll.lpi

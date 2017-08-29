@rem copy readme
copy /B /Y README.md bin\README.md

@rem copy script files
copy /B /Y src\lua\PSDToolKitLib.lua bin\script\PSDToolKit\
copy /B /Y src\lua\@PSDToolKit.anm bin\script\PSDToolKit\

@rem build src/go/assets/bindata.go
cd src/go/assets
go generate
cd ../../..

@rem build PSDToolKit.exe
cd src/go
go build -ldflags="-s -H=windowsgui" -o ../../bin/script/PSDToolKit/PSDToolKit.exe
cd ../../

@rem build lazarus projects
C:\lazarus\lazbuild.exe --build-all src/lazarus/auf.lpi src/lazarus/luadll.lpi

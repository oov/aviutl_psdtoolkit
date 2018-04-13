library PSDToolKitBridge;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

uses
  Windows,
  SysUtils,
  BridgeMain,
  Find,
  Remote,
  lua,
  Util,
  LuaFuncs,
  Cache,
  Execute, fftsg;

exports
  luaopen_PSDToolKitBridge;

initialization
  SetMultiByteConversionCodePage(CP_UTF8);
  Randomize();
  LoadLua(ExtractFileDir(GetDLLName()) + '\..\..\lua51.dll');

finalization
  FreeLua();

end.

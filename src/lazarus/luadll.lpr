library luadll;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

uses
  Windows,
  SysUtils,
  Main,
  Find,
  Remote,
  lua,
  Util,
  LuaFuncs,
  Cache,
  Execute;

  function luaopen_PSDToolKit(L: Plua_State): integer; cdecl;
  var
    i: integer;
  begin
    lua_newtable(L);
    for i := Low(Functions) to High(Functions) do
    begin
      lua_pushcfunction(L, Functions[i].Func);
      lua_setfield(L, 2, Functions[i].Name);
    end;
    lua_pushvalue(L, 2);
    lua_setglobal(L, 'PSDToolKit');
    Result := 1;
  end;

function GetLuaDLLName(): WideString;
begin
  SetLength(Result, MAX_PATH);
  GetModuleFileNameW(hInstance, @Result[1], MAX_PATH);
  Result := ExtractFileDir(PWideChar(Result)) + '\..\..\lua51.dll';
end;

exports
  luaopen_PSDToolKit;

initialization
  SetMultiByteConversionCodePage(CP_UTF8);
  Randomize();
  LoadLua(GetLuaDLLName());

finalization
  FreeLua();

end.

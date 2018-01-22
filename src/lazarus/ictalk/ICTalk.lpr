library ICTalk;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

uses
  Windows, SysUtils, Classes, CeVIOControl, Util, lua, Funcs, ICTalkDialog;

function luaopen_ICTalk(L: Plua_State): integer; cdecl;
var
  i: integer;
begin
  lua_newtable(L);
  for i := Low(Functions) to High(Functions) do
  begin
    lua_pushcfunction(L, Functions[i].Func);
    lua_setfield(L, -2, Functions[i].Name);
  end;
  lua_pushvalue(L, -1);
  lua_setglobal(L, 'ICTalk');
  Result := 1;
end;

function GetLuaDLLName(): WideString;
begin
  SetLength(Result, MAX_PATH);
  GetModuleFileNameW(hInstance, @Result[1], MAX_PATH);
  Result := ExtractFileDir(PWideChar(Result)) + '\..\..\lua51.dll';
end;

exports
  luaopen_ICTalk;

initialization
  SetMultiByteConversionCodePage(CP_UTF8);
  Randomize();
  LoadLua(GetLuaDLLName());

finalization
  FreeLua();

end.


library luadll;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

uses
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

exports
  luaopen_PSDToolKit;

end.

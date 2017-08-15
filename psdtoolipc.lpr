library psdtoolipc;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

uses
  main,
  find,
  remote,
  lua,
  util,
  luafuncs,
  hook,
  cache;

  function luaopen_PSDToolIPC(L: Plua_State): integer; cdecl;
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
    lua_setglobal(L, 'PSDToolIPC');
    Result := 1;
  end;

exports
  luaopen_PSDToolIPC;

end.

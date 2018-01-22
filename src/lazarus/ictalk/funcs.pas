unit Funcs;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  SysUtils, Lua;

function LuaOpen(L: Plua_State): integer; cdecl;

type
  TEntry = record
    Name: PChar;
    Func: lua_CFunction;
  end;

const
  Functions: array[0..0] of TEntry = (
    (Name: 'open'; Func: @LuaOpen));

implementation

uses
  ICTalkDialog, Util;

function LuaReturn(L: Plua_State; const Ret: integer): integer;
begin
  if Ret = -1 then
    // *** IMPORTANT ***
    // Both Lua and FreePascal are using setjmp/longjmp for error/exception handling.
    // So while using lua_error(luaL_error), we cannot use try statement, any string type, etc.
    // If we do not follow this rule the program crashes in random places.
    Result := luaL_error(L, '%s', lua_tostring(L, -1))
  else
    Result := Ret;
end;

function LuaPushError(L: Plua_State; E: Exception): integer;
var
  SJIS: ShiftJISString;
begin
  SJIS := E.Message;
  lua_pushlstring(L, @SJIS[1], Length(SJIS));
  Result := -1;
end;

function LuaOpen(L: Plua_State): integer; cdecl;

  function Main(): integer;
  begin
    try
      Result := ShowICTalkDialog(L);
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

end.


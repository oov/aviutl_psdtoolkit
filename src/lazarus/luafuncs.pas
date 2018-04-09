unit LuaFuncs;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  lua;

function luaopen_PSDToolKitBridge(L: Plua_State): integer; cdecl;

implementation

uses
  SysUtils, BridgeMain, Cache, Util;

var
  bridge: TPSDToolKitBridge;
  CacheMgr: TCacheManager;

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
  SJIS := ShiftJISString(E.Message);
  lua_pushlstring(L, @SJIS[1], Length(SJIS));
  Result := -1;
end;

function LuaAddFile(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    FilePath: UTF8String;
  begin
    try
      FilePath := lua_tostring(L, 1);
      lua_pop(L, 1);
      bridge.AddFile(FilePath);
      Result := 0;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaClearFiles(L: Plua_State): integer; cdecl;

  function Main(): integer;
  begin
    try
      bridge.ClearFiles();
      Result := 0;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaDraw(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    id: integer;
    filename: ShiftJISString;
    w, h: integer;
    p: Pointer;
  begin
    try
      id := lua_tointeger(L, 1);
      filename := lua_tostring(L, 2);
      p := lua_topointer(L, 3);
      w := lua_tointeger(L, 4);
      h := lua_tointeger(L, 5);
      lua_pop(L, 5);
      bridge.Draw(id, filename, p, w, h);
      Result := 0;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaGetLayerNames(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    id, i, pos, di: integer;
    filename: ShiftJISString;
    s: UTF8String;
  begin
    try
      id := lua_tointeger(L, 1);
      filename := lua_tostring(L, 2);
      lua_pop(L, 2);
      s := bridge.GetLayerNames(id, filename);
      lua_newtable(L);
      pos := 1;
      di := 1;
      for i := 1 to Length(s) do
        if s[i] = #$0a then
        begin
          lua_pushlstring(L, @s[pos], i - pos);
          lua_rawseti(L, -2, di);
          pos := i + 1;
          Inc(di);
        end;
      lua_pushlstring(L, @s[pos], Length(s) - pos + 1);
      lua_rawseti(L, -2, di);
      Result := 1;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaSetProperties(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    id: integer;
    filename, tmp: ShiftJISString;

    Layer: UTF8String;
    pLayer: PUTF8String;
    Scale: single;
    pScale: PSingle;
    OffsetX, OffsetY: integer;
    pOffsetX, pOffsetY: System.PInteger;
    Modified: boolean;
    Width, Height: integer;
    CacheKey: DWord;
  begin
    try
      id := lua_tointeger(L, 1);
      filename := lua_tostring(L, 2);

      lua_getfield(L, 3, 'layer');
      if lua_isstring(L, -1) then
      begin
        tmp := lua_tostring(L, -1);
        Layer := tmp;
        pLayer := @Layer;
      end
      else
        pLayer := nil;
      lua_pop(L, 1);

      lua_getfield(L, 3, 'scale');
      if lua_isnumber(L, -1) then
      begin
        Scale := lua_tonumber(L, -1);
        pScale := @Scale;
      end
      else
        pScale := nil;
      lua_pop(L, 1);

      lua_getfield(L, 3, 'offsetx');
      if lua_isnumber(L, -1) then
      begin
        OffsetX := lua_tointeger(L, -1);
        pOffsetX := @OffsetX;
      end
      else
        pOffsetX := nil;
      lua_pop(L, 1);

      lua_getfield(L, 3, 'offsety');
      if lua_isnumber(L, -1) then
      begin
        OffsetY := lua_tointeger(L, -1);
        pOffsetY := @OffsetY;
      end
      else
        pOffsetY := nil;
      lua_pop(L, 1);

      lua_pop(L, 3);
      bridge.SetProperties(id, filename, pLayer, pScale,
        pOffsetX, pOffsetY, Modified, CacheKey, Width, Height);
      lua_pushboolean(L, Modified);
      lua_pushinteger(L, integer(CacheKey));
      lua_pushinteger(L, Width);
      lua_pushinteger(L, Height);
      Result := 4;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaShowGUI(L: Plua_State): integer; cdecl;

  function Main(): integer;
  begin
    try
      bridge.ShowGUI();
      Result := 0;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaSerialize(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    S: RawByteString;
  begin
    try
      S := bridge.Serialize();
      lua_pushlstring(L, @S[1], Length(S));
      Result := 1;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaDeserialize(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    S: RawByteString;
  begin
    try
      S := lua_tostring(L, 1);
      lua_pop(L, 1);
      bridge.Deserialize(S);
      Result := 0;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaPutCache(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    Id: ShiftJISString;
    P: Pointer;
    Len: integer;
    F: boolean;
  begin
    try
      Id := lua_tostring(L, 1);
      P := lua_topointer(L, 2);
      Len := lua_tointeger(L, 3);
      F := lua_toboolean(L, 4);
      lua_pop(L, 4);
      if F then
        CacheMgr.PutToFile(Id, P, Len)
      else
        CacheMgr.PutToMemory(Id, P, Len);
      Result := 0;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaGetCache(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    Id: ShiftJISString;
    P: Pointer;
    Len: integer;
  begin
    try
      Id := lua_tostring(L, 1);
      P := lua_topointer(L, 2);
      Len := lua_tointeger(L, 3);
      lua_pop(L, 3);
      if not CacheMgr.Get(Id, P, Len) then
        raise Exception.Create('failed to get image cache');
      Result := 0;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaType(L: Plua_State): integer; cdecl;
begin
  lua_pushstring(L, lua_typename(L, lua_type(L, -1)));
  Result := 1;
end;

function luaopen_PSDToolKitBridge(L: Plua_State): integer; cdecl;
type
  TEntry = record
    Name: PChar;
    Func: lua_CFunction;
  end;
const
  Functions: array[0..10] of TEntry = (
    (Name: 'addfile'; Func: @LuaAddFile),
    (Name: 'clearfiles'; Func: @LuaClearFiles),
    (Name: 'draw'; Func: @LuaDraw),
    (Name: 'getlayernames'; Func: @LuaGetLayerNames),
    (Name: 'setprops'; Func: @LuaSetProperties),
    (Name: 'showgui'; Func: @LuaShowGUI),
    (Name: 'serialize'; Func: @LuaSerialize),
    (Name: 'deserialize'; Func: @LuaDeserialize),
    (Name: 'putcache'; Func: @LuaPutCache),
    (Name: 'getcache'; Func: @LuaGetCache),
    (Name: 'type'; Func: @LuaType));
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
  lua_setglobal(L, 'PSDToolKitBridge');
  Result := 1;
end;

initialization
  bridge := TPSDToolKitBridge.Create();
  cacheMgr := TCacheManager.Create();


finalization
  bridge.Free();
  cacheMgr.Free();

end.

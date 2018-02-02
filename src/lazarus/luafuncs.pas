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

function LuaAddFile(L: Plua_State): integer; cdecl;
var
  filename: ShiftJISString;
begin
  try
    filename := lua_tostring(L, 1);
    bridge.AddFile(filename);
  except
    on e: Exception do
    begin
      lua_pushboolean(L, False);
      lua_pushstring(L, PChar(e.Message));
      Result := 2;
      Exit;
    end;
  end;
  lua_pushboolean(L, True);
  Result := 1;
end;

function LuaDraw(L: Plua_State): integer; cdecl;
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
    bridge.Draw(id, filename, p, w, h);
  except
    on e: Exception do
    begin
      lua_pushboolean(L, False);
      lua_pushstring(L, PChar(e.Message));
      Result := 2;
      Exit;
    end;
  end;
  lua_pushboolean(L, True);
  Result := 1;
end;

function LuaGetLayerNames(L: Plua_State): integer; cdecl;
var
  id, i, pos, di: integer;
  filename: ShiftJISString;
  s: UTF8String;
begin
  try
    id := lua_tointeger(L, 1);
    filename := lua_tostring(L, 2);
    lua_pushboolean(L, True);
    s := bridge.GetLayerNames(id, filename);
    lua_newtable(L);
    pos := 1;
    di := 1;
    for i := 1 to Length(s) do
      if s[i] = #$0a then
      begin
        lua_pushlstring(L, @s[pos], i - pos);
        lua_rawseti(L, 4, di);
        pos := i + 1;
        Inc(di);
      end;
    lua_pushlstring(L, @s[pos], Length(s) - pos + 1);
    lua_rawseti(L, 4, di);
  except
    on e: Exception do
    begin
      lua_pushboolean(L, False);
      lua_pushstring(L, PChar(e.Message));
      Result := 2;
      Exit;
    end;
  end;
  Result := 2;
end;

function LuaSetProperties(L: Plua_State): integer; cdecl;
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
begin
  try
    id := lua_tointeger(L, 1);
    filename := lua_tostring(L, 2);
    lua_getfield(L, 3, 'layer');
    lua_getfield(L, 3, 'scale');
    lua_getfield(L, 3, 'offsetx');
    lua_getfield(L, 3, 'offsety');

    if lua_isstring(L, 4) then
    begin
      tmp := lua_tostring(L, 4);
      Layer := tmp;
      pLayer := @Layer;
    end
    else
      pLayer := nil;

    if lua_isnumber(L, 5) then
    begin
      Scale := lua_tonumber(L, 5);
      pScale := @Scale;
    end
    else
      pScale := nil;

    if lua_isnumber(L, 6) then
    begin
      OffsetX := lua_tointeger(L, 6);
      pOffsetX := @OffsetX;
    end
    else
      pOffsetX := nil;

    if lua_isnumber(L, 7) then
    begin
      OffsetY := lua_tointeger(L, 7);
      pOffsetY := @OffsetY;
    end
    else
      pOffsetY := nil;

    bridge.SetProperties(id, filename, pLayer, pScale,
      pOffsetX, pOffsetY, Modified, Width, Height);
  except
    on e: Exception do
    begin
      lua_pushboolean(L, False);
      lua_pushstring(L, PChar(e.Message));
      Result := 2;
      Exit;
    end;
  end;
  lua_pushboolean(L, True);
  lua_pushboolean(L, Modified);
  lua_pushinteger(L, Width);
  lua_pushinteger(L, Height);
  Result := 4;
end;

function LuaShowGUI(L: Plua_State): integer; cdecl;
begin
  try
    bridge.ShowGUI();
  except
    on e: Exception do
    begin
      lua_pushboolean(L, False);
      lua_pushstring(L, PChar(e.Message));
      Result := 2;
      Exit;
    end;
  end;
  lua_pushboolean(L, True);
  Result := 1;
end;

function LuaSerialize(L: Plua_State): integer; cdecl;
var
  S: string;
begin
  try
    S := bridge.Serialize();
  except
    on e: Exception do
    begin
      lua_pushboolean(L, False);
      lua_pushstring(L, PChar(e.Message));
      Result := 2;
      Exit;
    end;
  end;
  lua_pushboolean(L, True);
  lua_pushlstring(L, @S[1], Length(S));
  Result := 2;
end;

function LuaDeserialize(L: Plua_State): integer; cdecl;
begin
  try
    bridge.Deserialize(lua_tostring(L, 1));
  except
    on e: Exception do
    begin
      lua_pushboolean(L, False);
      lua_pushstring(L, PChar(e.Message));
      Result := 2;
      Exit;
    end;
  end;
  lua_pushboolean(L, True);
  lua_pushboolean(L, True);
  Result := 2;
end;

function LuaPutCache(L: Plua_State): integer; cdecl;
var
  id: ShiftJISString;
  p: Pointer;
  len: integer;
  f: boolean;
begin
  try
    id := lua_tostring(L, 1);
    p := lua_topointer(L, 2);
    len := lua_tointeger(L, 3);
    f := lua_toboolean(L, 4);
    if f then
      CacheMgr.PutToFile(id, p, len)
    else
      CacheMgr.PutToMemory(id, p, len);
  except
    on e: Exception do
    begin
      lua_pushboolean(L, False);
      lua_pushstring(L, PChar(e.Message));
      Result := 2;
      Exit;
    end;
  end;
  lua_pushboolean(L, True);
  Result := 1;
end;

function LuaGetCache(L: Plua_State): integer; cdecl;
var
  id: ShiftJISString;
  p: Pointer;
  len: integer;
  r: boolean;
begin
  try
    id := lua_tostring(L, 1);
    p := lua_topointer(L, 2);
    len := lua_tointeger(L, 3);
    r := CacheMgr.Get(id, p, len);
  except
    on e: Exception do
    begin
      lua_pushboolean(L, False);
      lua_pushstring(L, PChar(e.Message));
      Result := 2;
      Exit;
    end;
  end;
  lua_pushboolean(L, r);
  Result := 1;
end;

function luaopen_PSDToolKitBridge(L: Plua_State): integer; cdecl;
type
  TEntry = record
    Name: PChar;
    Func: lua_CFunction;
  end;
const
  Functions: array[0..8] of TEntry = (
    (Name: 'addfile'; Func: @LuaAddFile),
    (Name: 'draw'; Func: @LuaDraw),
    (Name: 'getlayernames'; Func: @LuaGetLayerNames),
    (Name: 'setprops'; Func: @LuaSetProperties),
    (Name: 'showgui'; Func: @LuaShowGUI),
    (Name: 'serialize'; Func: @LuaSerialize),
    (Name: 'deserialize'; Func: @LuaDeserialize),
    (Name: 'putcache'; Func: @LuaPutCache),
    (Name: 'getcache'; Func: @LuaGetCache));
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

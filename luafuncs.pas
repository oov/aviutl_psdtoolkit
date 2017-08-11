unit luafuncs;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, lua;

function LuaDraw(L: Plua_State): integer; cdecl;
function LuaGetLayerNames(L: Plua_State): integer; cdecl;
function LuaSetProperties(L: Plua_State): integer; cdecl;
function LuaShowGUI(L: Plua_State): integer; cdecl;

type
  TEntry = record
    Name: PChar;
    Func: lua_CFunction;
  end;

const
  Functions: array[0..3] of TEntry = (
    (Name: 'draw'; Func: @LuaDraw),
    (Name: 'get_layer_names'; Func: @LuaGetLayerNames),
    (Name: 'set_properties'; Func: @LuaSetProperties),
    (Name: 'show_gui'; Func: @LuaShowGUI));

implementation

uses
  main, util;

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
    psdtool.Draw(id, filename, p, w, h);
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
    s := psdtool.GetLayerNames(id, filename);
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

  Layers: UTF8String;
  pLayers: PUTF8String;
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
      Layers := tmp;
      pLayers := @Layers;
    end
    else
      pLayers := nil;

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

    psdtool.SetProperties(id, filename, pLayers, pScale,
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
    psdtool.ShowGUI();
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

end.

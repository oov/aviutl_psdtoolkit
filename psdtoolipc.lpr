library psdtoolipc;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

uses
  SysUtils,
  lua,
  lauxlib,
  main,
  util, callback, find;

  function PSDToolIPC_Draw(L: Plua_State): integer; cdecl;
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
        lua_pushstring(L, e.Message);
        Result := 2;
        Exit;
      end;
    end;
    lua_pushboolean(L, True);
    Result := 1;
  end;

  function PSDToolIPC_GetLayerNames(L: Plua_State): integer; cdecl;
  var
    id: integer;
    filename: ShiftJISString;
    s: UTF8String;
  begin
    try
      id := lua_tointeger(L, 1);
      filename := lua_tostring(L, 2);
      s := psdtool.GetLayerNames(id, filename);
    except
      on e: Exception do
      begin
        lua_pushboolean(L, False);
        lua_pushstring(L, e.Message);
        Result := 2;
        Exit;
      end;
    end;
    lua_pushboolean(L, True);
    lua_pushstring(L, s);
    Result := 2;
  end;

  function PSDToolIPC_SetProperties(L: Plua_State): integer; cdecl;
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
        lua_pushstring(L, e.Message);
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

  function PSDToolIPC_ShowGUI(L: Plua_State): integer; cdecl;
  begin
    try
      psdtool.ShowGUI();
    except
      on e: Exception do
      begin
        lua_pushboolean(L, False);
        lua_pushstring(L, e.Message);
        Result := 2;
        Exit;
      end;
    end;
    lua_pushboolean(L, True);
    Result := 1;
  end;

function PSDToolIPC_GetEditingImageState(L: Plua_State): integer; cdecl;
var
  FileHash: DWORD;
  Scale: Single;
  OffsetX, OffsetY: Integer;
  FilePath, State: UTF8String;
begin
  try
    psdtool.GetEditingImageState(FilePath, FileHash, Scale, OffsetX, OffsetY, State);
  except
    on e: Exception do
    begin
      lua_pushboolean(L, False);
      lua_pushstring(L, e.Message);
      Result := 2;
      Exit;
    end;
  end;
  lua_pushboolean(L, True);
  lua_pushstring(L, FilePath);
  lua_pushinteger(L, FileHash);
  lua_pushnumber(L, Scale);
  lua_pushinteger(L, OffsetX);
  lua_pushinteger(L, OffsetY);
  lua_pushstring(L, State);
  Result := 7;
end;
const
  luaFunctions: array[0..5] of luaL_reg = (
    (Name: 'draw'; func: @PSDToolIPC_Draw),
    (Name: 'get_layer_names'; func: @PSDToolIPC_GetLayerNames),
    (Name: 'set_properties'; func: @PSDToolIPC_SetProperties),
    (Name: 'show_gui'; func: @PSDToolIPC_ShowGUI),
    (Name: 'get_editing_image_state'; func: @PSDToolIPC_GetEditingImageState),
    (Name: nil; func: nil));

  function luaopen_PSDToolIPC(L: Plua_State): integer; cdecl;
  begin
    luaL_register(L, 'PSDToolIPC', luaFunctions);
    Result := 0;
  end;

exports
  luaopen_PSDToolIPC;

end.

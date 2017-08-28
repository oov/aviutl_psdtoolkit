unit PSDToolIPCAssist;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Windows, AviUtl;

type
  { TPSDToolIPCAssist }

  TPSDToolIPCAssist = class
  private
    FEntry: TFilterDLL;
    FWindow: THandle;
    function GetEntry: PFilterDLL;
  public
    constructor Create();
    destructor Destroy(); override;
    function InitProc(fp: PFilter): boolean;
    function ExitProc(fp: PFilter): boolean;
    property Entry: PFilterDLL read GetEntry;
  end;

implementation

uses
  lua, SysUtils;

var
  MainDLLInstance: THandle;

function LuaAllocator(ud, ptr: Pointer; osize, nsize: size_t): Pointer; cdecl;
begin
  if nsize = 0 then
  begin
    if ptr <> nil then
      FreeMem(ptr);
    Result := nil;
    Exit;
  end;
  if ptr <> nil then
    Result := ReallocMem(ptr, nsize)
  else
    Result := GetMem(nsize);
end;

procedure ShowGUI();
var
  L: Plua_state;
  Proc: lua_CFunction;
  n: integer;
begin
  if MainDLLInstance = 0 then
    Exit;
  Proc := lua_CFunction(GetProcAddress(MainDLLInstance, 'luaopen_PSDToolIPC'));
  if Proc = nil then
    Exit;
  L := lua_newstate(@LuaAllocator, nil);
  if L = nil then
    Exit;
  try
    lua_pushstring(L, 'PSDToolIPC');
    n := Proc(L);
    if n <> 1 then
      Exit;
    lua_getfield(L, 2, 'showgui');
    if not lua_iscfunction(L, 3) then
      Exit;
    lua_call(L, 0, 0);
  finally
    lua_close(L);
  end;
end;

function WndProc(hwnd: HWND; Msg: UINT; WP: WPARAM; LP: LPARAM): LRESULT; stdcall;
begin
  if (Msg = WM_FILTER_COMMAND) and (WP = 1) then
    ShowGUI();
  Result := DefWindowProc(hwnd, Msg, WP, LP);
end;

{ TPSDToolIPCAssist }

function TPSDToolIPCAssist.GetEntry: PFilterDLL;
begin
  Result := @FEntry;
end;

constructor TPSDToolIPCAssist.Create;
const
  PluginName = 'PSDToolIPC';
  PluginInfo = 'PSDToolIPCAssist v0.1';
begin
  inherited Create();
  FillChar(FEntry, SizeOf(FEntry), 0);
  FEntry.Flag := FILTER_FLAG_ALWAYS_ACTIVE or FILTER_FLAG_EX_INFORMATION or FILTER_FLAG_NO_CONFIG;
  FEntry.Name := PluginName;
  FEntry.Information := PluginInfo;
end;

destructor TPSDToolIPCAssist.Destroy;
begin
  inherited Destroy();
end;

function TPSDToolIPCAssist.InitProc(fp: PFilter): boolean;
type
  ShiftJISString = type ansistring(932);
const
  ShowGUICaption: WideString = 'ウィンドウを表示';
var
  wc: WNDCLASS;
begin
  Result := True;
  if MainDLLInstance = 0 then
    Exit;

  wc.style := 0;
  wc.lpfnWndProc := @WndProc;
  wc.cbClsExtra := 0;
  wc.cbWndExtra := 0;
  wc.hInstance := fp^.DLLHInst;
  wc.hIcon := 0;
  wc.hCursor := 0;
  wc.hbrBackground := 0;
  wc.lpszMenuName := nil;
  wc.lpszClassName := 'PSDToolIPCAssist';
  RegisterClass(wc);
  FWindow := CreateWindow('PSDToolIPCAssist', nil, 0, 0, 0, 0, 0, 0,
    0, fp^.DLLHInst, nil);
  fp^.ExFunc^.AddMenuItem(fp, PChar(ShiftJISString(ShowGUICaption)),
    FWindow, 1, VK_W, ADD_MENU_ITEM_FLAG_KEY_CTRL);
end;

function TPSDToolIPCAssist.ExitProc(fp: PFilter): boolean;
begin
  Result := True;
  if MainDLLInstance = 0 then
    Exit;
  DestroyWindow(FWindow);
end;

initialization
  MainDLLInstance := LoadLibrary('script\PSDToolIPC\PSDToolIPC.dll');

finalization
  if MainDLLInstance <> 0 then
    FreeLibrary(MainDLLInstance);

end.

unit PSDToolKitAssist;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Windows, AviUtl;

type
  { TPSDToolKitAssist }

  TPSDToolKitAssist = class
  private
    FEntry: TFilterDLL;
    FWindow: THandle;
    function GetEntry: PFilterDLL;
  public
    constructor Create();
    destructor Destroy(); override;
    function InitProc(fp: PFilter): boolean;
    function ExitProc({%H-}fp: PFilter): boolean;
    property Entry: PFilterDLL read GetEntry;
  end;

implementation

uses
  lua, SysUtils;

var
  MainDLLInstance: THandle;

function LuaAllocator({%H-}ud, ptr: Pointer; {%H-}osize, nsize: size_t): Pointer; cdecl;
begin
  if nsize = 0 then
  begin
    if ptr <> nil then
      FreeMem(ptr);
    Result := nil;
    Exit;
  end;
  if ptr <> nil then
    Result := ReallocMem({%H-}ptr, nsize)
  else
    Result := GetMem(nsize);
end;

function FindExEditWindow(): THandle;
var
  h: THandle;
  pid, mypid: DWORD;
begin
  Result := 0;
  mypid := GetCurrentProcessId();
  h := 0;
  while Result = 0 do
  begin
    h := FindWindowExA(0, h, 'ExtendedFilterClass', nil);
    if h = 0 then
      Exit;
    GetWindowThreadProcessId(h, @pid);
    if pid = mypid then
       Result := h;
  end;
end;

procedure ShowGUI();
var
  h: THandle;
  s: WideString;
  L: Plua_state;
  Proc: lua_CFunction;
  n: integer;
begin
  if MainDLLInstance = 0 then
    Exit;
  Proc := lua_CFunction(GetProcAddress(MainDLLInstance, 'luaopen_PSDToolKit'));
  if Proc = nil then
    Exit;
  L := lua_newstate(@LuaAllocator, nil);
  if L = nil then
    Exit;
  try
    lua_pushstring(L, 'PSDToolKit');
    n := Proc(L);
    if n <> 1 then
      Exit;
    lua_getfield(L, 2, 'showgui');
    if not lua_iscfunction(L, 3) then
      Exit;
    lua_call(L, 0, 2);
    if not lua_toboolean(L, -2) then begin
      h := FindExEditWindow();
      s := 'ウィンドウの表示中にエラーが発生しました。'#13#10#13#10+WideString(string(lua_tostring(L, -1)));
      MessageBoxW(h, PWideChar(s), 'PSDToolKit', MB_ICONERROR);
    end;
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

{ TPSDToolKitAssist }

function TPSDToolKitAssist.GetEntry: PFilterDLL;
begin
  Result := @FEntry;
end;

constructor TPSDToolKitAssist.Create;
const
  PluginName = 'PSDToolKit';
  PluginInfo = 'PSDToolKitAssist v0.1';
begin
  inherited Create();
  FillChar(FEntry, SizeOf(FEntry), 0);
  FEntry.Flag := FILTER_FLAG_ALWAYS_ACTIVE or FILTER_FLAG_EX_INFORMATION or FILTER_FLAG_NO_CONFIG;
  FEntry.Name := PluginName;
  FEntry.Information := PluginInfo;
end;

destructor TPSDToolKitAssist.Destroy;
begin
  inherited Destroy();
end;

function TPSDToolKitAssist.InitProc(fp: PFilter): boolean;
type
  ShiftJISString = type ansistring(932);
const
  ShowGUICaption: WideString = 'ウィンドウを表示';
  HWND_MESSAGE = HWND(-3);
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
  wc.lpszClassName := 'PSDToolKitAssist';
  RegisterClass(wc);
  FWindow := CreateWindow('PSDToolKitAssist', nil, WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, HWND_MESSAGE,
    0, fp^.DLLHInst, nil);
  fp^.ExFunc^.AddMenuItem(fp, PChar(ShiftJISString(ShowGUICaption)),
    FWindow, 1, VK_W, ADD_MENU_ITEM_FLAG_KEY_CTRL);
end;

function TPSDToolKitAssist.ExitProc(fp: PFilter): boolean;
begin
  Result := True;
  if MainDLLInstance = 0 then
    Exit;
  DestroyWindow(FWindow);
end;

initialization
  MainDLLInstance := LoadLibrary('script\PSDToolKit\PSDToolKit.dll');

finalization
  if MainDLLInstance <> 0 then
    FreeLibrary(MainDLLInstance);

end.

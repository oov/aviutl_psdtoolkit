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
    function WndProc(Window: HWND; Message: UINT; WP: WPARAM;
      LP: LPARAM; Edit: Pointer; Filter: PFilter): LRESULT;
    function ProjectLoadProc(Filter: PFilter; Edit: Pointer; Data: Pointer;
      Size: integer): boolean;
    function ProjectSaveProc(Filter: PFilter; Edit: Pointer; Data: Pointer;
      var Size: integer): boolean;
    property Entry: PFilterDLL read GetEntry;
  end;

implementation

uses
  lua, SysUtils, Ver;

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
    if not lua_toboolean(L, -2) then
    begin
      h := FindExEditWindow();
      s := 'ウィンドウの表示中にエラーが発生しました。'#13#10#13#10 + WideString(string(lua_tostring(L, -1)));
      MessageBoxW(h, PWideChar(s), 'PSDToolKit', MB_ICONERROR);
    end;
  finally
    lua_close(L);
  end;
end;

function Serialize(): string;
var
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
      raise Exception.Create('PSDToolKit が見つかりません');
    lua_getfield(L, 2, 'serialize');
    if not lua_iscfunction(L, 3) then
      raise Exception.Create('PSDToolKit.serialize が見つかりません');
    lua_call(L, 0, 2);
    Result := lua_tostring(L, -1);
    if not lua_toboolean(L, -2) then
      raise Exception.Create(Result);
  finally
    lua_close(L);
  end;
end;

procedure Deserialize(s: string);
var
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
      raise Exception.Create('PSDToolKit が見つかりません');
    lua_getfield(L, 2, 'deserialize');
    if not lua_iscfunction(L, 3) then
      raise Exception.Create('PSDToolKit.deserialize が見つかりません');
    lua_pushlstring(L, @s[1], Length(s));
    lua_call(L, 1, 2);
    if not lua_toboolean(L, -2) then
      raise Exception.Create(lua_tostring(L, -1));
  finally
    lua_close(L);
  end;
end;

function MenuWndProc(hwnd: HWND; Msg: UINT; WP: WPARAM; LP: LPARAM): LRESULT; stdcall;
begin
  case Msg of
    WM_FILTER_COMMAND:
    begin
      if WP = 1 then
        ShowGUI();
    end;
    WM_FILTER_FILE_OPEN:
    begin
      OutputDebugString('Open');
    end;
    WM_FILTER_FILE_CLOSE:
    begin
      OutputDebugString('Close');
    end;
  end;

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
  PluginInfo = 'PSDToolKitAssist ' + Version;
begin
  inherited Create();
  FillChar(FEntry, SizeOf(FEntry), 0);
  FEntry.Flag := FILTER_FLAG_ALWAYS_ACTIVE or FILTER_FLAG_EX_INFORMATION or
    FILTER_FLAG_NO_CONFIG;
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
  if (MainDLLInstance = 0) or not LuaLoaded() then
    Exit;

  wc.style := 0;
  wc.lpfnWndProc := @MenuWndProc;
  wc.cbClsExtra := 0;
  wc.cbWndExtra := 0;
  wc.hInstance := fp^.DLLHInst;
  wc.hIcon := 0;
  wc.hCursor := 0;
  wc.hbrBackground := 0;
  wc.lpszMenuName := nil;
  wc.lpszClassName := 'PSDToolKitAssist';
  RegisterClass(wc);
  FWindow := CreateWindow('PSDToolKitAssist', nil, WS_OVERLAPPEDWINDOW,
    CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
    HWND_MESSAGE, 0, fp^.DLLHInst, nil);
  fp^.ExFunc^.AddMenuItem(fp, PChar(ShiftJISString(ShowGUICaption)),
    FWindow, 1, VK_W, ADD_MENU_ITEM_FLAG_KEY_CTRL);
  fp^.Hwnd := FWindow;
end;

function TPSDToolKitAssist.ExitProc(fp: PFilter): boolean;
begin
  Result := True;
  if MainDLLInstance = 0 then
    Exit;
  DestroyWindow(FWindow);
end;

function TPSDToolKitAssist.WndProc(Window: HWND; Message: UINT; WP: WPARAM;
  LP: LPARAM; Edit: Pointer; Filter: PFilter): LRESULT;
begin
  Result := AVIUTL_TRUE;
end;

function TPSDToolKitAssist.ProjectLoadProc(Filter: PFilter; Edit: Pointer;
  Data: Pointer; Size: integer): boolean;
var
  S: string;
begin
  try
    SetLength(S, Size);
    Move(Data^, S[1], Size);
    Deserialize(S);
  except
    on E: Exception do MessageBoxW(FindExEditWindow, PWideChar('読み込み中にエラーが発生しました。'#13#10#13#10 + WideString(E.Message)), 'PSDToolKit', MB_ICONERROR);
  end;
  Result := True;
end;

function TPSDToolKitAssist.ProjectSaveProc(Filter: PFilter; Edit: Pointer;
  Data: Pointer; var Size: integer): boolean;
var
  S: string;
begin
  try
    S := Serialize();
    Size := Length(S);
    if Assigned(Data) then Move(s[1], Data^, Length(S));
  except
    on E: Exception do MessageBoxW(FindExEditWindow, PWideChar('保存中にエラーが発生しました。'#13#10#13#10 + WideString(E.Message)), 'PSDToolKit', MB_ICONERROR);
  end;
  Result := True;
end;

function GetMainDLLName(): WideString;
begin
  SetLength(Result, MAX_PATH);
  GetModuleFileNameW(hInstance, @Result[1], MAX_PATH);
  Result := ExtractFileDir(PWideChar(Result)) + '\script\PSDToolKit\PSDToolKit.dll';
end;

function GetLuaDLLName(): WideString;
begin
  SetLength(Result, MAX_PATH);
  GetModuleFileNameW(hInstance, @Result[1], MAX_PATH);
  Result := ExtractFileDir(PWideChar(Result)) + '\lua51.dll';
end;

initialization
  MainDLLInstance := LoadLibraryW(PWideChar(GetMainDLLName()));
  LoadLua(GetLuaDLLName());

finalization
  if MainDLLInstance <> 0 then
    FreeLibrary(MainDLLInstance);
  if LuaLoaded() then
    FreeLua();

end.

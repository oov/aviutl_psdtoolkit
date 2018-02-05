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

type
  ShiftJISString = type ansistring(932);

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
  s: WideString;
  err: ShiftJISString;
  L: Plua_state;
  Proc: lua_CFunction;
  n: integer;
begin
  if MainDLLInstance = 0 then
    Exit;
  try
    Proc := lua_CFunction(GetProcAddress(MainDLLInstance, 'luaopen_PSDToolKitBridge'));
    if Proc = nil then
      raise Exception.Create('luaopen_PSDToolKitBridge not found');
    L := lua_newstate(@LuaAllocator, nil);
    if L = nil then
      raise Exception.Create('failed to execute lua_newstate');
    try
      lua_pushstring(L, 'PSDToolKitBridge');
      n := Proc(L);
      if n <> 1 then
        raise Exception.Create('luaopen_PSDToolKitBridge returned unexpected value');
      lua_getfield(L, 2, 'showgui');
      if not lua_iscfunction(L, 3) then
        raise Exception.Create('PSDToolKitBridge.showgui is not a function');

      if lua_pcall(L, 0, 0, 0) <> 0 then
      begin
        err := lua_tostring(L, -1);
        raise Exception.Create(err);
      end;
    finally
      lua_close(L);
    end;
  except
    on E: Exception do
    begin
      s := 'ウィンドウの表示中にエラーが発生しました。'#13#10#13#10 + E.Message;
      MessageBoxW(FindExEditWindow(), PWideChar(s), 'PSDToolKit', MB_ICONERROR);
    end;
  end;
end;

function Serialize(): RawByteString;
var
  L: Plua_state;
  Proc: lua_CFunction;
  err: ShiftJISString;
  n: integer;
begin
  if MainDLLInstance = 0 then
    Exit;
  Proc := lua_CFunction(GetProcAddress(MainDLLInstance, 'luaopen_PSDToolKitBridge'));
  if Proc = nil then
    raise Exception.Create('luaopen_PSDToolKitBridge not found');
  L := lua_newstate(@LuaAllocator, nil);
  if L = nil then
    raise Exception.Create('failed to execute lua_newstate');
  try
    lua_pushstring(L, 'PSDToolKitBridge');
    n := Proc(L);
    if n <> 1 then
      raise Exception.Create('luaopen_PSDToolKitBridge returned unexpected value');
    lua_getfield(L, 2, 'serialize');
    if not lua_iscfunction(L, 3) then
      raise Exception.Create('PSDToolKitBridge.serialize is not a function');
    if lua_pcall(L, 0, 1, 0) <> 0 then begin
      err := lua_tostring(L, -1);
      raise Exception.Create(err);
    end;
    Result := lua_tostring(L, -1);
  finally
    lua_close(L);
  end;
end;

procedure Deserialize(s: RawByteString);
var
  L: Plua_state;
  Proc: lua_CFunction;
  err: ShiftJISString;
  n: integer;
begin
  if MainDLLInstance = 0 then
    Exit;
  Proc := lua_CFunction(GetProcAddress(MainDLLInstance, 'luaopen_PSDToolKitBridge'));
  if Proc = nil then
    raise Exception.Create('luaopen_PSDToolKitBridge not found');
  L := lua_newstate(@LuaAllocator, nil);
  if L = nil then
    raise Exception.Create('failed to execute lua_newstate');
  try
    lua_pushstring(L, 'PSDToolKitBridge');
    n := Proc(L);
    if n <> 1 then
      raise Exception.Create('luaopen_PSDToolKitBridge returned unexpected value');
    lua_getfield(L, 2, 'deserialize');
    if not lua_iscfunction(L, 3) then
      raise Exception.Create('PSDToolKitBridge.deserialize is not a function');
    lua_pushlstring(L, @s[1], Length(s));
    if lua_pcall(L, 1, 0, 0) <> 0 then begin
      err := lua_tostring(L, -1);
      raise Exception.Create(err);
    end;
  finally
    lua_close(L);
  end;
end;

procedure ClearFiles();
var
  s: WideString;
  err: ShiftJISString;
  L: Plua_state;
  Proc: lua_CFunction;
  n: integer;
begin
  if MainDLLInstance = 0 then
    Exit;
  try
    Proc := lua_CFunction(GetProcAddress(MainDLLInstance, 'luaopen_PSDToolKitBridge'));
    if Proc = nil then
      raise Exception.Create('luaopen_PSDToolKitBridge not found');
    L := lua_newstate(@LuaAllocator, nil);
    if L = nil then
      raise Exception.Create('failed to execute lua_newstate');
    try
      lua_pushstring(L, 'PSDToolKitBridge');
      n := Proc(L);
      if n <> 1 then
        raise Exception.Create('luaopen_PSDToolKitBridge returned unexpected value');
      lua_getfield(L, 2, 'clearfiles');
      if not lua_iscfunction(L, 3) then
        raise Exception.Create('PSDToolKitBridge.clearfiles is not a function');

      if lua_pcall(L, 0, 0, 0) <> 0 then
      begin
        err := lua_tostring(L, -1);
        raise Exception.Create(err);
      end;
    finally
      lua_close(L);
    end;
  except
    on E: Exception do
    begin
      s := '編集中ファイルのクリア中にエラーが発生しました。'#13#10#13#10 + E.Message;
      MessageBoxW(FindExEditWindow(), PWideChar(s), 'PSDToolKit', MB_ICONERROR);
    end;
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
    WM_FILTER_FILE_OPEN: ClearFiles();
    WM_FILTER_FILE_CLOSE: ClearFiles();
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
  ExEditNameANSI = #$8a#$67#$92#$a3#$95#$d2#$8f#$57; // '拡張編集'
  ExEditVersion = ' version 0.92 ';
  HWND_MESSAGE = HWND(-3);
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
var
  i: integer;
  wc: WNDCLASS;
  si: SYSTEM_INFO;
  asi: TSysInfo;
  exedit: PFilter;
begin
  Result := True;
  FWindow := 0;

  try
    GetNativeSystemInfo(@si);
    if si.wProcessorArchitecture <> PROCESSOR_ARCHITECTURE_AMD64 then
      raise Exception.Create(
        'PSDToolKit を使用するには64bit 版の Windows が必要です。');
    if MainDLLInstance = 0 then
      raise Exception.Create(
        'script\PSDToolKit\PSDToolKitBridge.dll の読み込みに失敗しました。');
    if not LuaLoaded() then
      raise Exception.Create('lua51.dll の読み込みに失敗しました。');
    if fp^.ExFunc^.GetSysInfo(nil, @asi) = AVIUTL_FALSE then
      raise Exception.Create(
        'AviUtl のバージョン情報取得に失敗しました。');
    if asi.Build < 10000 then
      raise Exception.Create(
        'PSDToolKit を使うには AviUtl version 1.00 以降が必要です。');
    for i := 0 to asi.FilterN - 1 do
    begin
      exedit := fp^.ExFunc^.GetFilterP(i);
      if (exedit = nil) or (exedit^.Name <> ExEditNameANSI) then
        continue;
      if StrPos(exedit^.Information, ExEditVersion) = nil then
        raise Exception.Create('PSDToolKit を使うには拡張編集' +
          ExEditVersion + 'が必要です。');
      break;
    end;
  except
    on E: Exception do
    begin
      MessageBoxW(0, PWideChar(
        'PSDToolKit の初期化に失敗しました。'#13#10#13#10 + WideString(E.Message)),
        'PSDToolKit', MB_ICONERROR);
      Exit;
    end;
  end;

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
  if FWindow <> 0 then
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
    on E: Exception do
      MessageBoxW(FindExEditWindow, PWideChar(
        '読み込み中にエラーが発生しました。'#13#10#13#10 +
        WideString(E.Message)), 'PSDToolKit', MB_ICONERROR);
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
    if Assigned(Data) then
      Move(s[1], Data^, Length(S));
  except
    on E: Exception do
      MessageBoxW(FindExEditWindow, PWideChar(
        '保存中にエラーが発生しました。'#13#10#13#10 + WideString(E.Message)),
        'PSDToolKit', MB_ICONERROR);
  end;
  Result := True;
end;

function GetMainDLLName(): WideString;
begin
  SetLength(Result, MAX_PATH);
  GetModuleFileNameW(hInstance, @Result[1], MAX_PATH);
  Result := ExtractFileDir(PWideChar(Result)) +
    '\script\PSDToolKit\PSDToolKitBridge.dll';
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

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
    function GetEntry: PFilterDLL;
  public
    constructor Create();
    destructor Destroy(); override;
    function InitProc(fp: PFilter): boolean;
    function ExitProc({%H-}fp: PFilter): boolean;
    function Proc({%H-}fp: PFilter; fpip: PFilterProcInfo): boolean;
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
  lua, SysUtils, SettingDialog, Ver;

type
  ShiftJISString = type ansistring(932);

var
  MainDLLInstance: THandle;
  CurrentFrame, CurrentFrameN, CurrentRenderIndex: integer;

procedure SetCurrentFramePtr();
type
  TSetCurrentFramePtrFunc = procedure(CurrentFrame, CurrentFrameN, CurrentRenderIndex: PInteger);
var
  F: TSetCurrentFramePtrFunc;
begin
  if MainDLLInstance = 0 then
    Exit;
  F := TSetCurrentFramePtrFunc(GetProcAddress(MainDLLInstance, 'SetCurrentFramePtr'));
  if F = nil then
    Exit;
  F(@CurrentFrame, @CurrentFrameN, @CurrentRenderIndex);
end;

procedure SetExFuncPtr(const Filter: PFilter; const Edit: Pointer);
type
  TSetExFuncPtrFunc = procedure(const ExFunc: PExFunc);
var
  F: TSetExFuncPtrFunc;
begin
  if MainDLLInstance = 0 then
    Exit;
  F := TSetExFuncPtrFunc(GetProcAddress(MainDLLInstance, 'SetExFuncPtr'));
  if F = nil then
    Exit;

  if Filter = nil then
    F(nil)
  else
    F(Filter^.ExFunc);
end;

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

function ParseExEditVersion(S: string): integer;
  function NumDot(S: string): integer;
  var
    i: integer;
  begin
    for i := Low(S) to High(S) do
      case S[i] of
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.':;
        else begin
          Result := i - 1;
          Exit;
        end;
      end;
    Result := Length(S);
  end;
const
  ExEditVersionString = ' version ';
var
  I: integer;
  e: Extended;
begin
  Result := 0;
  I := Pos(ExEditVersionString, S);
  if I = 0 then
     Exit;
  Delete(S, 1, I + Length(ExEditVersionString) - 1);
  I := NumDot(S);
  if I = 0 then
     Exit;
  Delete(S, I + 1, Length(S));
  E := StrToFloatDef(S, 0);
  if E = 0 then
     Exit;
  Result := Trunc(E * 10000);
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

procedure ShowSetting();
  function GetBasePath(): WideString;
  begin
    SetLength(Result, MAX_PATH);
    GetModuleFileNameW(hInstance, @Result[1], MAX_PATH);
    Result := ExtractFileDir(PWideChar(Result)) + '\script\PSDToolKit\';
  end;
var
  BasePath, S: WideString;
  SJIS: ShiftJISString;
  L: Plua_state;
begin
  if MainDLLInstance = 0 then
    Exit;
  try
    L := lua_newstate(@LuaAllocator, nil);
    if L = nil then
      raise Exception.Create('failed to execute lua_newstate');
    try
      luaL_openlibs(L);
      BasePath := GetBasePath();
      lua_getglobal(L, 'package');
      SJIS := ShiftJISString(BasePath + '?.lua');
      lua_pushlstring(L, @SJIS[1], Length(SJIS));
      lua_setfield(L, -2, 'path');
      SJIS := ShiftJISString(BasePath + '?.dll');
      lua_pushlstring(L, @SJIS[1], Length(SJIS));
      lua_setfield(L, -2, 'cpath');
      lua_pop(L, 1);
      lua_getglobal(L, 'require');
      lua_pushstring(L, 'setting-gui');
      if lua_pcall(L, 1, 1, 0) <> 0 then
      begin
        lua_pop(L, 2);
        lua_newtable(L);
      end;
      lua_pushinteger(L, FindExEditWindow());
      lua_setfield(L, -2, 'parent');
      lua_pushstring(L, PChar(UTF8String(BasePath + 'setting-gui.lua')));
      lua_setfield(L, -2, 'filepath');
      ShowSettingDialog(L);
    finally
      lua_close(L);
    end;
  except
    on E: Exception do
    begin
      S := '環境設定ウィンドウの表示中にエラーが発生しました。'#13#10#13#10 + E.Message;
      MessageBoxW(FindExEditWindow(), PWideChar(S), 'PSDToolKit', MB_ICONERROR);
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

procedure UpdateCurrentProjectPath(Edit: Pointer; Filter: PFilter);
var
  SI: TSysInfo;
  FI: TFileInfo;
  err: ShiftJISString;
  U: UTF8String;
  L: Plua_state;
  Proc: lua_CFunction;
  n: integer;
begin
  if MainDLLInstance = 0 then
    Exit;

  FillChar(SI, SizeOf(SI), 0);
  FillChar(FI, SizeOf(FI), 0);
  if (Edit = nil) or (Filter = nil)
    or (Filter^.ExFunc^.GetSysInfo(Edit, @SI) = AVIUTL_FALSE)
    or (SI.ProjectName = nil) or (SI.ProjectName = '')
    or (Filter^.ExFunc^.GetFileInfo(Edit, @FI) = AVIUTL_FALSE)
    or (FI.FrameN = 0) or (FI.AudioRate = 0) or (FI.AudioCh = 0) then
    U := ''
  else
    U := ShiftJISString(SI.ProjectName);

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
    lua_getfield(L, 2, 'updatecurrentprojectpath');
    if not lua_iscfunction(L, 3) then
      raise Exception.Create('PSDToolKitBridge.updatecurrentprojectpath is not a function');

    lua_pushlstring(L, @U[1], Length(U));
    if lua_pcall(L, 1, 0, 0) <> 0 then begin
      err := lua_tostring(L, -1);
      raise Exception.Create(err);
    end;
  finally
    lua_close(L);
  end;
end;

{ TPSDToolKitAssist }

function TPSDToolKitAssist.GetEntry: PFilterDLL;
begin
  Result := @FEntry;
end;

constructor TPSDToolKitAssist.Create();
const
  PluginName = 'PSDToolKit';
  PluginInfo = 'PSDToolKitAssist ' + Version;
begin
  inherited Create();
  FillChar(FEntry, SizeOf(FEntry), 0);
  FEntry.Flag := FILTER_FLAG_ALWAYS_ACTIVE or FILTER_FLAG_EX_INFORMATION or FILTER_FLAG_PRIORITY_HIGHEST;
  FEntry.Name := PluginName;
  FEntry.Information := PluginInfo;
end;

destructor TPSDToolKitAssist.Destroy();
begin
  inherited Destroy();
end;

function TPSDToolKitAssist.InitProc(fp: PFilter): boolean;
type
  ShiftJISString = type ansistring(932);
const
  ShowGUICaption: WideString = 'ウィンドウを表示';
  SettingCaption: WideString = '環境設定';
  ExEditNameANSI = #$8a#$67#$92#$a3#$95#$d2#$8f#$57; // '拡張編集'
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
var
  i, v: integer;
  si: SYSTEM_INFO;
  asi: TSysInfo;
  exedit: PFilter;
begin
  Result := True;

  try
    GetNativeSystemInfo(@si);
    if si.wProcessorArchitecture <> PROCESSOR_ARCHITECTURE_AMD64 then
      raise Exception.Create(
        'PSDToolKit を使用するには64bit 版の Windows が必要です。');
    if MainDLLInstance = 0 then
      raise Exception.Create(
        'script\PSDToolKit\PSDToolKitBridge.dll の読み込みに失敗しました。');
    if not LuaLoaded() then
      raise Exception.Create('lua51.dll の読み込みに失敗しました。'#13#10'インストール方法が間違っている可能性があります。'#13#10'詳しくは付属マニュアルのチュートリアルを参照してください。');
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
      v := ParseExEditVersion(exedit^.Information);
      if v = 0 then
        raise Exception.Create('拡張編集のバージョンナンバー解析に失敗しました。');
      if v < 9200 then
        raise Exception.Create('PSDToolKit を使うには拡張編集 version 0.92 以降が必要です。');
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

  fp^.ExFunc^.AddMenuItem(fp, PChar(ShiftJISString(ShowGUICaption)),
    fp^.Hwnd, 1, VK_W, ADD_MENU_ITEM_FLAG_KEY_CTRL);
  fp^.ExFunc^.AddMenuItem(fp, PChar(ShiftJISString(SettingCaption)),
    fp^.Hwnd, 2, 0, 0);
  SetCurrentFramePtr();
end;

function TPSDToolKitAssist.ExitProc(fp: PFilter): boolean;
begin
  Result := True;
  SetExFuncPtr(nil, nil);
  if MainDLLInstance = 0 then
    Exit;
end;

function TPSDToolKitAssist.Proc(fp: PFilter; fpip: PFilterProcInfo): boolean;
begin
  InterlockedExchange(CurrentFrame, fpip^.Frame);
  InterlockedExchange(CurrentFrameN, fpip^.FrameN);
  InterlockedExchangeAdd(CurrentRenderIndex, 1);
  Result := True;
end;

function TPSDToolKitAssist.WndProc(Window: HWND; Message: UINT; WP: WPARAM;
  LP: LPARAM; Edit: Pointer; Filter: PFilter): LRESULT;
begin
  Result := AVIUTL_FALSE;
  case Message of
    WM_FILTER_INIT:
      CreateWindowW('STATIC', PWideChar('まずは説明書を読みましょう。'),
        WS_CHILD or WS_VISIBLE or ES_LEFT, 0, 0, 400, 32,
        Window, 0, Filter^.DLLHInst, nil);
    WM_FILTER_COMMAND:
    begin
      case WP of
        1: ShowGUI();
        2: ShowSetting();
      end;
    end;
    WM_FILTER_FILE_OPEN: begin
      SetExFuncPtr(Filter, Edit);
      ClearFiles();
      UpdateCurrentProjectPath(Edit, Filter);
    end;
    WM_FILTER_FILE_CLOSE: begin
      SetExFuncPtr(nil, nil);
      ClearFiles();
      UpdateCurrentProjectPath(nil, nil);
    end;
    WM_FILTER_SAVE_END: begin
      UpdateCurrentProjectPath(Edit, Filter);
    end;
  end;
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

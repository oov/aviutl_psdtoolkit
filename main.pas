unit main;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  SysUtils, Classes, Process, callback, util;

type

  { TPSDToolIPC }

  TPSDToolIPC = class
  private
    FStateMap: TStateMap;
    FRemoteProcess: TProcess;
    FWaitEventThread: TWaitEventThread;
    FCS: TRTLCriticalSection;
    procedure PrepareIPC();
    procedure Notify(Sender: TObject);
    procedure EnterCS(CommandName: string);
    procedure LeaveCS(CommandName: string);
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Draw(id: integer; filename: UTF8String; p: PByteArray;
      Width: integer; Height: integer);
    function GetLayerNames(id: integer; filename: UTF8String): UTF8String;
    procedure SetProperties(id: integer; filename: UTF8String;
      Layers: PUTF8String; Scale: PSingle; OffsetX: System.PInteger; OffsetY: System.PInteger;
      out Modified: boolean; out Width: integer; out Height: integer);
    procedure ShowGUI();
    procedure GetEditingImageState(out FilePath: UTF8String; out FileHash: DWORD; out Scale: single; out OffsetX: integer; out OffsetY: integer; out State: UTF8String);
  end;

var
  psdtool: TPSDToolIPC;

implementation

uses
  Windows, find;

var
  hKeyboardHook: THandle;
  ShiftOn, CtrlOn, AltOn: boolean;

function IsConfigButton(h: THandle; wID: integer): boolean;
const
  TARGET_BUTTON_CLASS = 'Button';
  TARGET_BUTTON_ID = 8001;
  ROOT_CLASS = 'ExtendedFilterClass';
var
  s: WideString;
  i: integer;
  hAncestor: THandle;
begin
  Result := False;
  if h = 0 then
    Exit;
  if wID <> TARGET_BUTTON_ID then
    Exit;
  SetLength(s, 256);
  i := GetClassNameW(h, @s[1], Length(s));
  if i = 0 then
    Exit;
  SetLength(s, i);
  if s <> TARGET_BUTTON_CLASS then
    Exit;
  if GetWindowLong(h, GWL_STYLE) <> (WS_CHILDWINDOW or WS_VISIBLE or
    BS_CENTER or BS_VCENTER) then
    Exit;
  if GetWindowLong(h, GWL_EXSTYLE) <> 0 then
    Exit;

  hAncestor := GetAncestor(h, GA_ROOT);
  SetLength(s, 256);
  i := GetClassNameW(hAncestor, @s[1], Length(s));
  if i = 0 then
    Exit;
  SetLength(s, i);
  if s <> ROOT_CLASS then
    Exit;

  SetLength(s, 256);
  i := GetWindowTextW(hAncestor, @s[1], Length(s));
  if i = 0 then
    Exit;
  SetLength(s, i);
  ods('TopWindow: %s', [s]);

end;

function KeyboardHookProc(code: longint; wp: WPARAM; lp: LPARAM): LRESULT; stdcall;
var
  processed: boolean;
begin
  if code < 0 then
    Exit;

  processed := False;
  try
    if code <> HC_ACTION then
      Exit;
    case wp of
      VK_A:
        if ((lp and $80000000) <> 0) and ShiftOn and CtrlOn and AltOn then begin
          psdtool.ShowGUI();
          ShiftOn := false;
          CtrlOn := false;
          AltOn := false;
        end;
      VK_SHIFT:
        ShiftOn := (lp and $80000000) = 0;
      VK_CONTROL:
        CtrlOn := (lp and $80000000) = 0;
      VK_MENU:
        AltOn := (lp and $80000000) = 0;
    end;
  finally
    if not processed then
      Result := CallNextHookEx(hKeyboardHook, code, wp, lp);
  end;
end;

procedure InstallHook();
begin
  if IsExEditWindowExists() then
  begin
    hKeyboardHook := SetWindowsHookEx(WH_KEYBOARD, @KeyboardHookProc,
      0, GetCurrentThreadId());
    if hKeyboardHook <> 0 then
      ods('install keyboard hook success', [])
    else
      ods('install keyboard hook failed', []);
  end
  else
  begin
    hKeyboardHook := 0;
  end;
end;

procedure UninstallHook();
begin
  if hKeyboardHook <> 0 then
  begin
    if UnhookWindowsHookEx(hKeyboardHook) then
      ods('uninstall keyboard hook success', [])
    else
      ods('uninstall keyboard hook failed', []);
  end;
end;

{ TPSDToolIPC }

constructor TPSDToolIPC.Create();
var
  ws: WideString;
begin
  inherited Create;
  InitCriticalSection(FCS);
  FRemoteProcess := TProcess.Create(nil);
  SetLength(ws, MAX_PATH);
  GetModuleFileNameW(hInstance, @ws[1], MAX_PATH);
  ws := PWideChar(ws);
  ws[Length(ws) - 2] := 'e';
  ws[Length(ws) - 1] := 'x';
  ws[Length(ws) - 0] := 'e';

  FRemoteProcess.Executable := ws;
  FRemoteProcess.Options := [poUsePipes, poNoConsole];
  FStateMap := TStateMap.Create();

  FWaitEventThread := TWaitEventThread.Create();
  FWaitEventThread.OnNotify := @Notify;
end;

destructor TPSDToolIPC.Destroy;
begin
  FWaitEventThread.Terminate();
  SetEvent(FWaitEventThread.NotifyEvent);

  FStateMap.Free;
  FRemoteProcess.Free;
  DoneCriticalSection(FCS);
  inherited Destroy;
end;

procedure TPSDToolIPC.Draw(id: integer; filename: UTF8String;
  p: PByteArray; Width: integer; Height: integer);
var
  l: integer;
begin
  EnterCS('DRAW');
  try
    PrepareIPC();
    FRemoteProcess.Input.WriteBuffer('DRAW', 4);
    WriteIdAndFileName(FRemoteProcess.Input, id, filename);
    WriteInt32(FRemoteProcess.Input, Width);
    WriteInt32(FRemoteProcess.Input, Height);
    ods('  Width: %d / Height: %d', [Width, Height]);

    ReadResult(FRemoteProcess.Output);
    l := ReadBinary(FRemoteProcess.Output, p, Width * 4 * Height);
    ods('  -> Binary(Len: %d)', [l]);
  finally
    LeaveCS('DRAW');
  end;
end;

function TPSDToolIPC.GetLayerNames(id: integer; filename: UTF8String): UTF8String;
begin
  EnterCS('LNAM');
  try
    PrepareIPC();
    FRemoteProcess.Input.WriteBuffer('LNAM', 4);
    WriteIdAndFileName(FRemoteProcess.Input, id, filename);

    ReadResult(FRemoteProcess.Output);
    Result := ReadString(FRemoteProcess.Output);
    ods('  -> String(Len: %d)', [Length(Result)]);
  finally
    LeaveCS('LNAM');
  end;
end;

procedure TPSDToolIPC.SetProperties(id: integer; filename: UTF8String;
  Layers: PUTF8String; Scale: PSingle; OffsetX: System.PInteger; OffsetY: System.PInteger;
  out Modified: boolean; out Width: integer; out Height: integer);
const
  PROPID_END = 0;
  PROPID_LAYERS = 1;
  PROPID_SCALE = 2;
  PROPID_OFFSETX = 3;
  PROPID_OFFSETY = 4;
begin
  EnterCS('PROP');
  try
    PrepareIPC();
    FRemoteProcess.Input.WriteBuffer('PROP', 4);
    WriteIdAndFileName(FRemoteProcess.Input, id, filename);
    if Layers <> nil then
    begin
      WriteInt32(FRemoteProcess.Input, PROPID_LAYERS);
      WriteString(FRemoteProcess.Input, Layers^);
      ODS('  Layers: %s', [Layers^]);
    end;
    if Scale <> nil then
    begin
      WriteInt32(FRemoteProcess.Input, PROPID_SCALE);
      WriteSingle(FRemoteProcess.Input, Scale^);
      ODS('  Scale: %f', [Scale^]);
    end;
    if OffsetX <> nil then
    begin
      WriteInt32(FRemoteProcess.Input, PROPID_OFFSETX);
      WriteInt32(FRemoteProcess.Input, OffsetX^);
      ODS('  OffsetX: %d', [OffsetX^]);
    end;
    if OffsetY <> nil then
    begin
      WriteInt32(FRemoteProcess.Input, PROPID_OFFSETY);
      WriteInt32(FRemoteProcess.Input, OffsetY^);
      ODS('  OffsetY: %d', [OffsetY^]);
    end;
    WriteInt32(FRemoteProcess.Input, PROPID_END);

    ReadResult(FRemoteProcess.Output);
    Modified := ReadInt32(FRemoteProcess.Output) <> 0;
    Width := ReadInt32(FRemoteProcess.Output);
    Height := ReadInt32(FRemoteProcess.Output);
    ods('  -> Modified:%d / Width:%d / Height:%d', [Ord(Modified), Width, Height]);
  finally
    LeaveCS('PROP');
  end;
end;

procedure TPSDToolIPC.ShowGUI();
var
  h: THandle;
begin
  EnterCS('SGUI');
  try
    PrepareIPC();
    FRemoteProcess.Input.WriteBuffer('SGUI', 4);

    if not DuplicateHandle(GetCurrentProcess(), FWaitEventThread.NotifyEvent,
      FRemoteProcess.Handle, @h, SYNCHRONIZE or EVENT_MODIFY_STATE, False, 0) then
      raise Exception.Create('DuplicateHandle Failed');
    WriteUInt64(FRemoteProcess.Input, QWord(h));
    ODS('Event Handle: %d', [h]);

    ReadResult(FRemoteProcess.Output);
    h := THandle(ReadUInt64(FRemoteProcess.Output));
    SetForegroundWindow(h);
    ods('  -> Window Handle(%d)', [h]);
  finally
    LeaveCS('SGUI');
  end;
end;

procedure TPSDToolIPC.GetEditingImageState(out FilePath: UTF8String; out FileHash: DWORD; out
  Scale: single; out OffsetX: integer;
  out OffsetY: integer; out State: UTF8String);
begin
  EnterCS('EDIS');
  try
    PrepareIPC();
    FRemoteProcess.Input.WriteBuffer('EDIS', 4);

    ReadResult(FRemoteProcess.Output);
    FilePath := ReadString(FRemoteProcess.Output);
    FileHash := DWORD(ReadInt32(FRemoteProcess.Output));
    Scale := ReadSingle(FRemoteProcess.Output);
    OffsetX := ReadInt32(FRemoteProcess.Output);
    OffsetY := ReadInt32(FRemoteProcess.Output);
    State := ReadString(FRemoteProcess.Output);
    ods('  -> FilePath: %s / FileHash: %08x / Scale: %f / OffsetX: %d / OffsetY: %d / State: %s',
      [FilePath, FileHash, Scale, OffsetX, OffsetY, State]);
  finally
    LeaveCS('EDIS');
  end;
end;

procedure TPSDToolIPC.PrepareIPC;
var
  s: UTF8String;
begin
  if FRemoteProcess.Running then
    Exit;
  SetLength(s, 4);
  FRemoteProcess.Execute;
  FRemoteProcess.CloseStderr;
  FRemoteProcess.Input.WriteBuffer('HELO', 4);
  FRemoteProcess.Output.ReadBuffer(s[1], 4);
  if s <> 'HELO' then
  begin
    FRemoteProcess.Terminate(1);
  end;
end;

procedure TPSDToolIPC.Notify(Sender: TObject);
var
  w: TExEditWindow;
  pw: TExEditParameterDialog;
  n: DWORD;
  FileHash: DWORD;
  Scale: single;
  OffsetX, OffsetY: integer;
  FilePath, State: UTF8String;
  ws: WideString;
begin
  GetEditingImageState(FilePath,FileHash, Scale, OffsetX, OffsetY, State);
  ws := WideString(UTF8String(Format('f="%s";l="%s";',
    [StringifyForLua(FilePath), State])));
  if FindExEditParameterDialog(pw) then
  begin
    SendMessageW(pw.Edit, WM_SETTEXT, 0, LPARAM(PWideChar(ws)));
    Exit;
  end;
  if FindExEditWindow(w) then
  begin
    PostMessage(w.Config, BM_CLICK, 0, 0);
    n := GetTickCount() + 3000;
    while n > GetTickCount() do
    begin
      Sleep(0);
      if FindExEditParameterDialog(pw) then
      begin
        SendMessageW(pw.Edit, WM_SETTEXT, 0, LPARAM(PWideChar(ws)));
        PostMessage(pw.OK, BM_CLICK, 0, 0);
        Exit;
      end;
    end;
  end;
end;

procedure TPSDToolIPC.EnterCS(CommandName: string);
begin
  EnterCriticalSection(FCS);
  ods('%s BEGIN', [CommandName]);
end;

procedure TPSDToolIPC.LeaveCS(CommandName: string);
begin
  ods('%s END', [CommandName]);
  LeaveCriticalSection(FCS);
end;

initialization
  Randomize();
  psdtool := TPSDToolIPC.Create();
  InstallHook();

finalization
  UninstallHook();
  psdtool.Free();
end.

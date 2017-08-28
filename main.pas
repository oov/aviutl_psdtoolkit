unit Main;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  SysUtils, Classes, Process, Remote, Util;

type

  { TPSDToolIPC }

  TPSDToolIPC = class
  private
    FRemoteProcess: TProcess;
    FReceiver: TReceiver;
    FCS: TRTLCriticalSection;
    FEditingImageState: WideString;
    FPSDToolWindow: THandle;
    procedure SendEditingImageStateToExEdit;
    procedure PrepareIPC();
    procedure OnRequest(Sender: TObject; const Command: UTF8String);
    procedure OnShiftCtrlAltA(Sender: TObject);
    procedure EnterCS(CommandName: string);
    procedure LeaveCS(CommandName: string);
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Draw(id: integer; filename: UTF8String; p: PByteArray;
      Width: integer; Height: integer);
    function GetLayerNames(id: integer; filename: UTF8String): UTF8String;
    procedure SetProperties(id: integer; filename: UTF8String;
      Layer: PUTF8String; Scale: PSingle; OffsetX: System.PInteger;
      OffsetY: System.PInteger; out Modified: boolean; out Width: integer;
      out Height: integer);
    procedure ShowGUI();
  end;

implementation

uses
  Windows, Find;

{ TPSDToolIPC }

constructor TPSDToolIPC.Create();
var
  ws: WideString;
begin
  inherited Create;
  InitCriticalSection(FCS);
  FRemoteProcess := TProcess.Create(nil);
  ws := GetDLLName();
  ws[Length(ws) - 2] := 'e';
  ws[Length(ws) - 1] := 'x';
  ws[Length(ws) - 0] := 'e';
  FRemoteProcess.Executable := ws;
  FRemoteProcess.Options := [poUsePipes, poNoConsole];
  FReceiver := nil;
end;

destructor TPSDToolIPC.Destroy;
begin
  if FReceiver <> nil then
    FReceiver.Terminate;

  if FRemoteProcess.Running then
  begin
    FRemoteProcess.CloseInput;
    FRemoteProcess.CloseOutput;
  end;
  FreeAndNil(FRemoteProcess);

  if FReceiver <> nil then
  begin
    while not FReceiver.Finished do
      ThreadSwitch();
    FreeAndNil(FReceiver);
  end;

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
    ODS('  Width: %d / Height: %d', [Width, Height]);
  finally
    LeaveCS('DRAW');
  end;
  FReceiver.WaitResult();
  try
    l := FReceiver.ReadBinary(p, Width * 4 * Height);
    ODS('  -> Binary(Len: %d)', [l]);
  finally
    FReceiver.Done();
  end;
end;

function TPSDToolIPC.GetLayerNames(id: integer; filename: UTF8String): UTF8String;
begin
  EnterCS('LNAM');
  try
    PrepareIPC();
    FRemoteProcess.Input.WriteBuffer('LNAM', 4);
    WriteIdAndFileName(FRemoteProcess.Input, id, filename);
  finally
    LeaveCS('LNAM');
  end;
  FReceiver.WaitResult();
  try
    Result := FReceiver.ReadString();
    ODS('  -> String(Len: %d)', [Length(Result)]);
  finally
    FReceiver.Done();
  end;
end;

procedure TPSDToolIPC.SetProperties(id: integer; filename: UTF8String;
  Layer: PUTF8String; Scale: PSingle; OffsetX: System.PInteger;
  OffsetY: System.PInteger; out Modified: boolean; out Width: integer;
  out Height: integer);
const
  PROPID_END = 0;
  PROPID_LAYER = 1;
  PROPID_SCALE = 2;
  PROPID_OFFSETX = 3;
  PROPID_OFFSETY = 4;
begin
  EnterCS('PROP');
  try
    PrepareIPC();
    FRemoteProcess.Input.WriteBuffer('PROP', 4);
    WriteIdAndFileName(FRemoteProcess.Input, id, filename);
    if Layer <> nil then
    begin
      WriteInt32(FRemoteProcess.Input, PROPID_LAYER);
      WriteString(FRemoteProcess.Input, Layer^);
      ODS('  Layer: %s', [Layer^]);
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
  finally
    LeaveCS('PROP');
  end;
  FReceiver.WaitResult();
  try
    Modified := FReceiver.ReadInt32() <> 0;
    Width := FReceiver.ReadInt32();
    Height := FReceiver.ReadInt32();
    ODS('  -> Modified:%d / Width:%d / Height:%d', [Ord(Modified), Width, Height]);
  finally
    FReceiver.Done();
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
  finally
    LeaveCS('SGUI');
  end;
  FReceiver.WaitResult();
  try
    h := THandle(FReceiver.ReadUInt64());
    ODS('  -> Window Handle(%d)', [h]);
  finally
    FReceiver.Done();
  end;
  if h <> 0 then begin
    FPSDToolWindow := h;
    SetForegroundWindow(h);
  end;
end;

procedure TPSDToolIPC.SendEditingImageStateToExEdit;
var
  h: THandle;
  ws: WideString;
  w: TExEditWindow;
  pw: TExEditParameterDialog;
  n: DWORD;
begin
  EnterCriticalSection(FCS);
  try
    ws := FEditingImageState;
    h := FPSDToolWindow;
  finally
    LeaveCriticalSection(FCS);
  end;

  if FindExEditParameterDialog(pw) then
  begin
    SendMessageW(pw.Edit, WM_SETTEXT, 0, {%H-}LPARAM(PWideChar(ws)));
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
        SendMessageW(pw.Edit, WM_SETTEXT, 0, {%H-}LPARAM(PWideChar(ws)));
        PostMessage(pw.OK, BM_CLICK, 0, 0);
        Exit;
      end;
    end;
  end;
  MessageBoxW(h, 'Target not found.', 'PSDToolIPC', MB_ICONERROR);
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
    raise Exception.Create('unexpected reply: ' + s);
  end;

  if FReceiver <> nil then
    FreeAndNil(FReceiver);
  FReceiver := TReceiver.Create(FRemoteProcess.Output);
  FReceiver.OnRequest := @OnRequest;
end;

procedure TPSDToolIPC.OnRequest(Sender: TObject; const Command: UTF8String);
var
  FileHash: DWORD;
  Scale: single;
  OffsetX, OffsetY: integer;
  FilePath, State: UTF8String;
begin
  case Command of
    'EDIS':
    begin
      FilePath := FReceiver.ReadString();
      FileHash := DWORD(FReceiver.ReadInt32());
      Scale := FReceiver.ReadSingle();
      OffsetX := FReceiver.ReadInt32();
      OffsetY := FReceiver.ReadInt32();
      State := FReceiver.ReadString();
      ODS('  -> FilePath: %s / FileHash: %08x / Scale: %f / OffsetX: %d / OffsetY: %d / State: %s',
        [FilePath, FileHash, Scale, OffsetX, OffsetY, State]);
      EnterCS('EDIS');
      try
        FRemoteProcess.Input.WriteDWord($80000000);
        FEditingImageState :=
          WideString(UTF8String(Format('f="%s";l="%s";',
          [StringifyForLua(FilePath), State])));
      finally
        LeaveCS('EDIS');
      end;
      TThread.ExecuteInThread(@SendEditingImageStateToExEdit).FreeOnTerminate := True;
    end;
  end;
end;

procedure TPSDToolIPC.OnShiftCtrlAltA(Sender: TObject);
begin
  ShowGUI();
end;

procedure TPSDToolIPC.EnterCS(CommandName: string);
begin
  EnterCriticalSection(FCS);
  ODS('%s BEGIN', [CommandName]);
end;

procedure TPSDToolIPC.LeaveCS(CommandName: string);
begin
  ODS('%s END', [CommandName]);
  LeaveCriticalSection(FCS);
end;

end.

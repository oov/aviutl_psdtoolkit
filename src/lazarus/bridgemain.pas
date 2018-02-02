unit BridgeMain;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  SysUtils, Process, Remote;

type

  { TPSDToolKitBridge }

  TPSDToolKitBridge = class
  private
    FRemoteProcess: TProcess;
    FReceiver: TReceiver;
    FCS: TRTLCriticalSection;
    FPSDToolWindow: THandle;
    procedure PrepareIPC();
    procedure OnRequest(Sender: TObject; const Command: UTF8String);
    procedure OnReceiveEditingImageState();
    procedure OnReceiveCopyFaviewValue();
    procedure OnReceiveExportFaviewSlider();
    procedure EnterCS(CommandName: string);
    procedure LeaveCS(CommandName: string);
  public
    constructor Create();
    destructor Destroy(); override;
    procedure AddFile(FilePath: UTF8String);
    procedure Draw(id: integer; filename: UTF8String; p: PByteArray;
      Width: integer; Height: integer);
    function GetLayerNames(id: integer; filename: UTF8String): UTF8String;
    procedure SetProperties(id: integer; filename: UTF8String;
      Layer: PUTF8String; Scale: PSingle; OffsetX: System.PInteger;
      OffsetY: System.PInteger; out Modified: boolean; out Width: integer;
      out Height: integer);
    procedure ShowGUI();
    function Serialize(): RawByteString;
    procedure Deserialize(s: RawByteString);
  end;

implementation

uses
  Windows, Classes, Execute, Util;

{ TPSDToolKitBridge }

constructor TPSDToolKitBridge.Create();
begin
  inherited Create;
  InitCriticalSection(FCS);
  FRemoteProcess := TProcess.Create(nil);
  FRemoteProcess.Executable := ExtractFileDir(GetDLLName()) + '\PSDToolKit.exe';
  FRemoteProcess.Options := [poUsePipes, poNoConsole];
  FReceiver := nil;
end;

destructor TPSDToolKitBridge.Destroy();
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

procedure TPSDToolKitBridge.AddFile(FilePath: UTF8String);
begin
  EnterCS('ADDF');
  try
    PrepareIPC();
    FRemoteProcess.Input.WriteBuffer('ADDF', 4);
    WriteString(FRemoteProcess.Input, FilePath);
    ODS('  FilePath: %s', [FilePath]);
  finally
    LeaveCS('ADDF');
  end;
  FReceiver.WaitResult();
  FReceiver.Done();
end;

procedure TPSDToolKitBridge.Draw(id: integer; filename: UTF8String;
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

function TPSDToolKitBridge.GetLayerNames(id: integer; filename: UTF8String): UTF8String;
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

procedure TPSDToolKitBridge.SetProperties(id: integer; filename: UTF8String;
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

procedure TPSDToolKitBridge.ShowGUI();
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
  if h <> 0 then
  begin
    FPSDToolWindow := h;
    SetForegroundWindow(h);
  end;
end;

function TPSDToolKitBridge.Serialize(): RawByteString;
begin
  EnterCS('SRLZ');
  try
    PrepareIPC();
    FRemoteProcess.Input.WriteBuffer('SRLZ', 4);
  finally
    LeaveCS('SRLZ');
  end;
  FReceiver.WaitResult();
  try
    Result := FReceiver.ReadString();
    ODS('  -> String(Len: %d)', [Length(Result)]);
  finally
    FReceiver.Done();
  end;
end;

procedure TPSDToolKitBridge.Deserialize(s: RawByteString);
var
  r: boolean;
begin
  EnterCS('DSLZ');
  try
    PrepareIPC();
    FRemoteProcess.Input.WriteBuffer('DSLZ', 4);
    WriteString(FRemoteProcess.Input, s);
  finally
    LeaveCS('DSLZ');
  end;
  FReceiver.WaitResult();
  try
    r := FReceiver.ReadInt32() <> 0;
    ODS('  -> Result:%d', [Ord(r)]);
  finally
    FReceiver.Done();
  end;
end;

procedure TPSDToolKitBridge.PrepareIPC();
begin
  if FRemoteProcess.Running then
    Exit;
  try
    FRemoteProcess.Execute;
  except
    on E: EProcess do begin
      raise Exception.Create(
        'failed to execute: script\PSDToolKit\PSDToolKit.exe'#13#10+
        E.Message+#13#10#13#10+
        'Please check whether the antivirus software is blocking program execution.'#13#10+
        'アンチウィルスソフトがプログラム実行を阻害していないか確認してください。');
    end;
  end;
  FRemoteProcess.CloseStderr;
  FRemoteProcess.Input.WriteBuffer('HELO', 4);

  if FReceiver <> nil then
    FreeAndNil(FReceiver);
  FReceiver := TReceiver.Create(FRemoteProcess.Output);
  FReceiver.OnRequest := @OnRequest;
  FReceiver.WaitResult();
  FReceiver.Done();
end;

procedure TPSDToolKitBridge.OnRequest(Sender: TObject; const Command: UTF8String);
const
  UnknownCommandErr = 'Unknown Command';
begin
  case Command of
    'EDIS': OnReceiveEditingImageState();
    'CPFV': OnReceiveCopyFaviewValue();
    'EXFS': OnReceiveExportFaviewSlider();
    else
    begin
      WriteUInt32(FRemoteProcess.Input, Length(UnknownCommandErr) or $80000000);
      WriteString(FRemoteProcess.Input, UnknownCommandErr);
    end;
  end;
end;

procedure TPSDToolKitBridge.OnReceiveEditingImageState();
var
  FilePath, State: UTF8String;
begin
  FilePath := FReceiver.ReadString();
  State := FReceiver.ReadString();
  ODS('  -> FilePath: %s / State: %s', [FilePath, State]);
  EnterCS('EDIS');
  try
    WriteUInt32(FRemoteProcess.Input, $80000000);
    TSendEditingImageStateToExEdit.Create(FPSDToolWindow,
      UTF8String(Format('f=%s;l=%s;', [StringifyForLua(FilePath),
      StringifyForLua(State)])));
  finally
    LeaveCS('EDIS');
  end;
end;

procedure TPSDToolKitBridge.OnReceiveCopyFaviewValue();
var
  FilePath, SliderName, Name, Value: UTF8String;
begin
  FilePath := FReceiver.ReadString();
  SliderName := FReceiver.ReadString();
  Name := FReceiver.ReadString();
  Value := FReceiver.ReadString();
  ODS('  -> SliderName: %s / Name: %s / Value: %s', [SliderName, Name, Value]);
  EnterCS('CPFV');
  try
    WriteUInt32(FRemoteProcess.Input, $80000000);
  finally
    LeaveCS('CPFV');
  end;
  if not CopyToClipboard(FPSDToolWindow, Value) then
    MessageBox(FPSDToolWindow, 'could not open clipboard', 'PSDToolKit', MB_ICONERROR);
end;

procedure TPSDToolKitBridge.OnReceiveExportFaviewSlider();
var
  FilePath, SliderName, Names, Values: UTF8String;
begin
  FilePath := FReceiver.ReadString();

  SliderName := FReceiver.ReadString();
  Names := FReceiver.ReadString();
  Values := FReceiver.ReadString();
  ODS('  -> SliderName: %s / Names: %s / Values: %s', [SliderName, Names, Values]);
  EnterCS('EXFS');
  try
    WriteUInt32(FRemoteProcess.Input, $80000000);
    TExportFaviewSlider.Create(FPSDToolWindow, FilePath, SliderName, Names, Values);
  finally
    LeaveCS('EXFS');
  end;
end;

procedure TPSDToolKitBridge.EnterCS(CommandName: string);
begin
  EnterCriticalSection(FCS);
  ODS('%s BEGIN', [CommandName]);
end;

procedure TPSDToolKitBridge.LeaveCS(CommandName: string);
begin
  ODS('%s END', [CommandName]);
  LeaveCriticalSection(FCS);
end;

end.

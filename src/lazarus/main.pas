unit Main;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  SysUtils, Classes, Process, Remote, Util;

type

  { TPSDToolKit }

  TPSDToolKit = class
  private
    FRemoteProcess: TProcess;
    FReceiver: TReceiver;
    FCS: TRTLCriticalSection;
    FEditingImageState: WideString;
    FSliderName, FSliderItemNames, FSliderItemValues: UTF8String;
    FPSDToolWindow: THandle;
    procedure SendEditingImageStateToExEdit;
    procedure ExportFaviewSlider;
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

{ TPSDToolKit }

constructor TPSDToolKit.Create();
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

destructor TPSDToolKit.Destroy;
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

procedure TPSDToolKit.Draw(id: integer; filename: UTF8String;
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

function TPSDToolKit.GetLayerNames(id: integer; filename: UTF8String): UTF8String;
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

procedure TPSDToolKit.SetProperties(id: integer; filename: UTF8String;
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

procedure TPSDToolKit.ShowGUI();
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

procedure TPSDToolKit.SendEditingImageStateToExEdit;
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
  if FindExEditWindow(w) and (w.Config <> 0) then
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
  MessageBoxW(h, 'Target not found.', 'PSDToolKit', MB_ICONERROR);
end;

procedure TPSDToolKit.ExportFaviewSlider;
var
  h: THandle;
  N: integer;
  SliderName, Names, Values, S: UTF8String;
  SS: ShiftJISString;
  FileName: WideString;
  f: TFileStreamW;
begin
  EnterCriticalSection(FCS);
  try
    SliderName := FSliderName;
    Names := FSliderItemNames;
    Values := FSliderItemValues;
    h := FPSDToolWindow;
  finally
    LeaveCriticalSection(FCS);
  end;
  FileName := SaveDialog(h, 'Save SimpleView slider as file',
    'AviUtl animation effect script(*.anm)'#0'*.anm'#0'CSV file(*.csv)'#0'*.csv'#0#0,
    1, WideString(SliderName)+'.anm', 'anm', ExtractFilePath(GetDLLName())+'..');
  case LowerCase(ExtractFileExt(FileName)) of
    '.anm':
    begin
      f := TFileStreamW.Create(FileName);
      try
        N := 0;
        S := Values;
        while True do
        begin
          Token(#0, S);
          Inc(N);
          if S = '' then
            break;
        end;
        SS := SliderName;
        WriteRawString(f, Format('--track0:%s,1,%d,1,1'#13#10, [SS, N]));
        WriteRawString(f, 'local names = {');
        while True do
        begin
          S := StringifyForLua(Token(#0, Names)) + ', ';
          SS := S;
          WriteRawString(f, SS);
          if Names = '' then
            break;
        end;
        WriteRawString(f, 'nil}'#13#10);
        WriteRawString(f, 'local values = {');
        while True do
        begin
          S := StringifyForLua(Token(#0, Values)) + ', ';
          SS := S;
          WriteRawString(f, SS);
          if Values = '' then
            break;
        end;
        WriteRawString(f, 'nil}'#13#10);
        WriteRawString(f, 'PSDToolKitLib.psd:addstate(values[obj.track0])'#13#10);
      finally
        f.Free;
      end;
    end;
    '.csv':
    begin
      f := TFileStreamW.Create(FileName);
      try
        while True do
        begin
          WriteRawString(f, StringifyForCSV(Token(#0, Names)));
          WriteRawString(f, ',');
          WriteRawString(f, StringifyForCSV(Token(#0, Values)));
          WriteRawString(f, #13#10);
          if (Names = '')and(Values = '') then
            break;
        end;
      finally
        f.Free;
      end;
    end;
  end;
end;

procedure TPSDToolKit.PrepareIPC;
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

procedure TPSDToolKit.OnRequest(Sender: TObject; const Command: UTF8String);
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

procedure TPSDToolKit.OnReceiveEditingImageState;
var
  FileHash: DWORD;
  Scale: single;
  OffsetX, OffsetY: integer;
  FilePath, State: UTF8String;
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
    WriteUInt32(FRemoteProcess.Input, $80000000);
    FEditingImageState :=
      WideString(UTF8String(Format('f=%s;l=%s;',
      [StringifyForLua(FilePath), StringifyForLua(State)])));
  finally
    LeaveCS('EDIS');
  end;
  TThread.ExecuteInThread(@SendEditingImageStateToExEdit).FreeOnTerminate := True;
end;

procedure TPSDToolKit.OnReceiveCopyFaviewValue;
var
  SliderName, Name, Value: UTF8String;
begin
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

procedure TPSDToolKit.OnReceiveExportFaviewSlider;
var
  SliderName, Names, Values: UTF8String;
begin
  SliderName := FReceiver.ReadString();
  Names := FReceiver.ReadString();
  Values := FReceiver.ReadString();
  ODS('  -> SliderName: %s / Names: %s / Values: %s', [SliderName, Names, Values]);
  EnterCS('EXFS');
  try
    WriteUInt32(FRemoteProcess.Input, $80000000);
    FSliderName := SliderName;
    FSliderItemNames := Names;
    FSliderItemValues := Values;
  finally
    LeaveCS('EXFS');
  end;
  TThread.ExecuteInThread(@ExportFaviewSlider).FreeOnTerminate := True;
end;

procedure TPSDToolKit.EnterCS(CommandName: string);
begin
  EnterCriticalSection(FCS);
  ODS('%s BEGIN', [CommandName]);
end;

procedure TPSDToolKit.LeaveCS(CommandName: string);
begin
  ODS('%s END', [CommandName]);
  LeaveCriticalSection(FCS);
end;

end.

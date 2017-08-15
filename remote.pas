unit remote;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Classes, SysUtils;

type
  TRequestEvent = procedure(Sender: TObject; const Command: UTF8String) of object;

  { TReceiver }

  TReceiver = class(TThread)
  private
    FBuf: UTF8String;
    FOnRequest: TRequestEvent;
    FReply: PRTLEvent;
    FDone: PRTLEvent;
    FStream: TStream;
  protected
    procedure Execute(); override;
  public
    constructor Create(Stream: TStream);
    destructor Destroy(); override;

    procedure WaitResult();
    procedure Done();

    function ReadUInt64(): QWord;
    function ReadInt32(): longint;
    function ReadSingle(): single;
    function ReadString(): UTF8String;
    function ReadBinary(const P: Pointer; const Len: integer): integer;
    property OnRequest: TRequestEvent read FOnRequest write FOnRequest;
  end;

implementation

uses
  util;

{ TReceiver }

procedure TReceiver.Execute;
begin
  SetLength(FBuf, 4);
  try
    while not Terminated do
    begin
      FStream.ReadBuffer(FBuf[1], 4);
      if Terminated then Exit;
      if PDWORD(@FBuf[1])^ and $80000000 = 0 then
      begin
        FOnRequest(Self, FBuf);
        continue;
      end;
      RTLEventSetEvent(FReply);
      RTLEventWaitFor(FDone);
      RTLEventResetEvent(FDone);
    end;
  except
    on E: Exception do begin
      if Terminated and (E.ClassType=EReadError) then Exit;
      ODS('Exception in TReceiver: %s', [E.Message]);
      Terminate();
      RTLEventSetEvent(FReply);
    end;
  end;
end;

constructor TReceiver.Create(Stream: TStream);
begin
  inherited Create(False);
  FStream := Stream;
  FReply := RTLEventCreate();
  FDone := RTLEventCreate();
end;

destructor TReceiver.Destroy;
begin
  RTLEventDestroy(FReply);
  RTLEventDestroy(FDone);
  inherited Destroy;
end;

procedure TReceiver.WaitResult;
var
  l: integer;
  msg: UTF8String;
begin
  RTLEventWaitFor(FReply);
  RTLEventResetEvent(FReply);
  if Terminated then raise Exception.Create('could not read result');
  try
    l := PDWORD(@FBuf[1])^ and $7fffffff;
    if l = 0 then
    begin
      ODS('  -> SUCCESS', []);
      Exit;
    end;
    SetLength(msg, l);
    FStream.ReadBuffer(msg[1], l);
    ODS('  -> ERROR: %s', [msg]);
    raise Exception.Create(msg);
  except
    Done();
    raise;
  end;
end;

procedure TReceiver.Done;
begin
  RTLEventSetEvent(FDone);
end;

function TReceiver.ReadUInt64: QWord;
begin
  Result := FStream.ReadQWord();
end;

function TReceiver.ReadInt32: longint;
begin
  Result := longint(FStream.ReadDWord());
end;

function TReceiver.ReadSingle: single;
var
  d: DWORD;
begin
  d := FStream.ReadDWord();
  Result := PSingle(@d)^;
end;

function TReceiver.ReadString: UTF8String;
var
  l: integer;
begin
  l := ReadInt32();
  if l = 0 then
  begin
    Result := '';
    Exit;
  end;
  SetLength(Result, l);
  FStream.ReadBuffer(Result[1], l);
end;

function TReceiver.ReadBinary(const P: Pointer; const Len: integer): integer;
var
  l: integer;
begin
  l := ReadInt32();
  if l > Len then
    raise Exception.Create('could not read binary: given buffer too short');
  FStream.ReadBuffer(P^, l);
  Result := l;
end;

end.

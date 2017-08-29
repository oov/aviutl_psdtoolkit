unit Lag;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  CircularBuffer;

type
  { TLagFilter }

  TLagFilter = class
  private
    FChannels: integer;
    FDuration: single;
    FSampleRate: single;
    FBuffer: TCircularBuffer;
    function GetDurationDisp: string;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure UpdateParameter();
    procedure Clear();
    procedure ProcessReplacing(Input: PSingle; sampleframes: integer);
    property SampleRate: single read FSampleRate write FSampleRate;
    property Channels: integer read FChannels write FChannels;
    property Duration: single read FDuration write FDuration;
    property DurationDisp: string read GetDurationDisp;
  end;

implementation

uses
  SysUtils;

{ TLagFilter }

function TLagFilter.GetDurationDisp: string;
begin
  Result := Format('%0.0f', [FDuration*1000.0]);
end;

constructor TLagFilter.Create;
begin
  inherited Create();
  FBuffer := TCircularBuffer.Create();
  FChannels:=0;
  FDuration:=0;
  FSampleRate:=0;
end;

destructor TLagFilter.Destroy;
begin
  FBuffer.Free;
  inherited Destroy;
end;

procedure TLagFilter.UpdateParameter;
begin
  Clear();
end;

procedure TLagFilter.Clear;
var
  l: integer;
  buf: array of Byte;
begin
  FBuffer.Clear();
  l := Trunc(FDuration * FSampleRate) * FChannels * SizeOf(Single);
  SetLength(buf, l);
  FBuffer.Write(buf[0], l);
end;

procedure TLagFilter.ProcessReplacing(Input: PSingle; sampleframes: integer);
var
  l: integer;
begin
  l := sampleframes * FChannels * SizeOf(Single);
  FBuffer.Write(Input^, l);
  FBuffer.Read(Input^, l);
end;

end.


unit Vibrato;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

type
  { TVibratoFilter }

  TVibratoFilter = class
  private
    FDepth: single;
    FFrequency: single;
    FSampleRate: single;
    FBuffer: array of single;
    FBufPos: PSingle;
    FGlobalPos: single;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure UpdateParameter();
    procedure Clear();
    procedure Process(Input: PSingle; Output: PSingle; sampleframes: integer);
    property SampleRate: single read FSampleRate write FSampleRate;
    property Frequency: single read FFrequency write FFrequency;
    property Depth: single read FDepth write FDepth;
  end;

implementation

uses
  Math;

var
  CosTable: array[0..4095] of single;

procedure InitCosTable();
var
  I, H: integer;
  S: single;
begin
  H := High(CosTable);
  S := PI / (H * 0.5);
  for I := Low(CosTable) to H do
    CosTable[I] := cos(I * S);
end;

function Interpolate(const S0, S1: single; const T: single): single; inline;
begin
  Result := S0 * (1 - T) + S1 * T;
end;

{ TVibratoFilter }

constructor TVibratoFilter.Create;
begin
  inherited Create();
  FFrequency := 1;
  FDepth := 1;
  FSampleRate := 0;
end;

destructor TVibratoFilter.Destroy;
begin
  inherited Destroy;
end;

procedure TVibratoFilter.UpdateParameter;
var
  L: integer;
begin
  L := Ceil(FDepth * FSampleRate) + 1;
  if Length(FBuffer) <> L then begin
    SetLength(FBuffer, L);
    Clear();
  end;
end;

procedure TVibratoFilter.Clear;
begin
  FillChar(FBuffer[0], Length(FBuffer) * SizeOf(single), 0);
  FBufPos := @FBuffer[0];
  FGlobalPos := 0;
end;

procedure TVibratoFilter.Process(Input: PSingle; Output: PSingle; sampleframes: integer); inline;
var
  I, BufLen: integer;
  Buf, BufStart, BufEnd, S0, S1: PSingle;
  GPos, LPos, HRate, DHRate: single;
begin
  HRate := FSampleRate * FDepth * 0.5;
  DHRate := FFrequency * $0fff / FSampleRate;
  BufLen := Length(FBuffer);
  BufStart := @FBuffer[0];
  BufEnd := BufStart;
  Inc(BufEnd, BufLen);
  Buf := FBufPos;
  GPos := FGlobalPos;
  for I := 0 to sampleframes - 1 do
  begin
    Buf^ := Input^;

    LPos := (CosTable[Trunc(GPos * DHRate) and $0fff] - 1) * HRate - 1;

    S0 := Buf;
    Inc(S0, Ceil(LPos));
    if S0 < BufStart then
      Inc(S0, BufLen);

    S1 := S0;
    Inc(S1);
    if S1 = BufEnd then
      S1 := BufStart;

    Output^ := Interpolate(S1^, S0^, Frac(-LPos));

    Inc(Buf);
    if Buf = BufEnd then
      Buf := BufStart;
    Inc(Input);
    Inc(Output);
    GPos := GPos + 1;
  end;
  FBufPos := Buf;
  if GPos > FSampleRate then
    GPos := GPos - FSampleRate;
  FGlobalPos := GPos;
end;

initialization
  InitCosTable();

end.


unit Vibrato;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

type
  { TVibratoFilter }

  TVibratoFilter = class
  private
    FChannels: integer;
    FCurrentDepth: single;
    FMaxDepth: single;
    FFrequency: single;
    FSampleRate: single;
    FBuffer: array of single;
    FBufPos: PSingle;
    FGlobalPos: single;
    procedure ProcessMono(Input: PSingle; Output: PSingle; sampleframes: integer);
    procedure ProcessMulti(Input: PSingle; Output: PSingle; sampleframes: integer);
  public
    constructor Create();
    destructor Destroy(); override;
    procedure UpdateParameter();
    procedure Clear();
    procedure Process(Input: PSingle; Output: PSingle; sampleframes: integer);
    property SampleRate: single read FSampleRate write FSampleRate;
    property Channels: integer read FChannels write FChannels;
    property Frequency: single read FFrequency write FFrequency;
    property MaxDepth: single read FMaxDepth write FMaxDepth;
    property CurrentDepth: single read FCurrentDepth write FCurrentDepth;
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

procedure TVibratoFilter.ProcessMono(Input: PSingle; Output: PSingle;
  sampleframes: integer); inline;
var
  I, BufLen: integer;
  Buf, BufStart, BufEnd, S0, S1: PSingle;
  GPos, LPos, T, HRate, DHRate: single;
begin
  HRate := FSampleRate * FMaxDepth * FCurrentDepth * 0.5;
  DHRate := FFrequency * $0fff / FSampleRate;
  BufLen := Length(FBuffer);
  BufStart := @FBuffer[0];
  BufEnd := BufStart;
  Inc(BufEnd, BufLen);
  Buf := FBufPos;
  GPos := FGlobalPos;
  for I := 0 to sampleframes - 1 do
  begin
    LPos := (CosTable[Trunc(GPos * DHRate) and $0fff] - 1) * HRate - 1;
    T := Frac(-LPos);

    S0 := Buf;
    Inc(S0, Ceil(LPos));
    if S0 < BufStart then
      Inc(S0, BufLen);

    S1 := S0;
    Inc(S1);
    if S1 = BufEnd then
      S1 := BufStart;

    Buf^ := Input^;
    Output^ := Interpolate(S1^, S0^, T);

    Inc(Buf);
    if Buf = BufEnd then
      Buf := BufStart;
    Inc(Input);
    Inc(Output);
    GPos := GPos + 1;
  end;
  FBufPos := Buf;
  if GPos * DHRate > $0fff then
    GPos := (GPos * DHRate - $0fff) / DHRate;
  FGlobalPos := GPos;
end;

procedure TVibratoFilter.ProcessMulti(Input: PSingle; Output: PSingle;
  sampleframes: integer); inline;
var
  I, J, BufLen, Stride: integer;
  Buf, BufStart, BufEnd, S0, S1: PSingle;
  GPos, LPos, HRate, DHRate, T: single;
begin
  Stride := FChannels;
  HRate := FSampleRate * FMaxDepth * FCurrentDepth * 0.5;
  DHRate := FFrequency * $0fff / FSampleRate;
  BufLen := Length(FBuffer);
  BufStart := @FBuffer[0];
  BufEnd := BufStart;
  Inc(BufEnd, BufLen);
  Buf := FBufPos;
  GPos := FGlobalPos;
  for I := 0 to sampleframes - 1 do
  begin
    LPos := (CosTable[Trunc(GPos * DHRate) and $0fff] - 1) * HRate - 1;
    T := Frac(-LPos);

    S0 := Buf;
    Inc(S0, Ceil(LPos) * Stride);
    if S0 < BufStart then
      Inc(S0, BufLen);

    S1 := S0;
    Inc(S1, Stride);
    if S1 = BufEnd then
      S1 := BufStart;

    for J := 0 to Stride - 1 do begin
      Buf^ := Input^;
      Output^ := Interpolate(S1^, S0^, T);
      Inc(Buf);
      Inc(Input);
      Inc(Output);
      Inc(S0);
      Inc(S1);
    end;
    if Buf = BufEnd then
      Buf := BufStart;
    GPos := GPos + 1;
  end;
  FBufPos := Buf;
  if GPos * DHRate > $0fff then
    GPos := (GPos * DHRate - $0fff) / DHRate;
  FGlobalPos := GPos;
end;

constructor TVibratoFilter.Create;
begin
  inherited Create();
  FFrequency := 1;
  FMaxDepth := 1;
  FCurrentDepth := 1;
  FSampleRate := 0;
  Channels := 0;
end;

destructor TVibratoFilter.Destroy;
begin
  inherited Destroy;
end;

procedure TVibratoFilter.UpdateParameter;
var
  L: integer;
begin
  L := (Ceil(FMaxDepth * FSampleRate) + 1) * FChannels;
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
begin
  if FChannels = 1 then
    ProcessMono(Input, Output, sampleframes)
  else
    ProcessMulti(Input, Output, sampleframes);
end;

initialization
  InitCosTable();

end.


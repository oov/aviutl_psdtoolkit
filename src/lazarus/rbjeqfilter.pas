{
based on http://www.musicdsp.org/showone.php?id=225

RBJ Audio EQ Cookbook Filters
A pascal conversion of arguru[AT]smartelectronix[DOT]com's
c++ implementation.

WARNING:This code is not FPU undernormalization safe.

Filter Types
0-LowPass
1-HiPass
2-BandPass CSG
3-BandPass CZPG
4-Notch
5-AllPass
6-Peaking
7-LowShelf
8-HiShelf
}
unit RbjEQFilter;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses Math;

type
  TBuffer = record
    in1, in2, out1, out2: single;
  end;
  PBuffer = ^TBuffer;

  TRbjEqFilterType = (ftLowPass, ftHiPass, ftBandPassCSG, ftBandPassCZPG, ftNotch, ftAllPass, ftPeaking, ftLowShelf, ftHiShelf);
  { TRbjEQFilter }

  TRbjEQFilter = class
  private
    b0a0, b1a0, b2a0, a1a0, a2a0: single;
    FBuf: array of TBuffer;
    FSampleRate: single;
    FFilterType: integer;
    FFreq, FQ, FDBGain: single;
    FQIsBandWidth: boolean;
    function GetChannels: integer;
    procedure SetChannels(AValue: integer);
  public
    constructor Create();
    procedure UpdateParameter();
    procedure Clear();
    procedure ProcessReplacing(Input: psingle; sampleframes: integer);
    property FilterType: integer read FFilterType write FFilterType;
    property SampleRate: single read FSampleRate write FSampleRate;
    property Freq: single read FFreq write FFreq;
    property Q: single read FQ write FQ;
    property DBGain: single read FDBGain write FDBGain;
    property QIsBandWidth: boolean read FQIsBandWidth write FQIsBandWidth;
    property Channels: integer read GetChannels write SetChannels;
  end;

implementation

constructor TRbjEQFilter.Create();
begin
  FSampleRate := 0;
  FFilterType := 0;
  FFreq := 500;
  FQ := 0.3;
  FDBGain := 0;
  FQIsBandWidth := True;
end;

function TRbjEQFilter.GetChannels: integer;
begin
  Result := Length(FBuf);
end;

procedure TRbjEQFilter.SetChannels(AValue: integer);
begin
  SetLength(FBuf, AValue);
end;

procedure TRbjEQFilter.UpdateParameter;
var
  alpha, a0, a1, a2, b0, b1, b2, q_: single;
  A, beta, omega, tsin, tcos: single;
begin
  q_ := (1 - FQ) * 0.98;
  //peaking, LowShelf or HiShelf
  if FFilterType >= 6 then
  begin
    A := power(10.0, (DBGain / 40.0));
    omega := 2 * pi * FFreq / FSampleRate;
    tsin := sin(omega);
    tcos := cos(omega);

    if FQIsBandWidth then
      alpha := tsin * sinh(log2(2.0) / 2.0 * q_ * omega / tsin)
    else
      alpha := tsin / (2.0 * q_);

    beta := sqrt(A) / q_;

    // peaking
    if FFilterType = 6 then
    begin
      b0 := 1.0 + alpha * A;
      b1 := -2.0 * tcos;
      b2 := 1.0 - alpha * A;
      a0 := 1.0 + alpha / A;
      a1 := -2.0 * tcos;
      a2 := 1.0 - alpha / A;
    end
    else
    // lowshelf
    if FFilterType = 7 then
    begin
      b0 := (A * ((A + 1.0) - (A - 1.0) * tcos + beta * tsin));
      b1 := (2.0 * A * ((A - 1.0) - (A + 1.0) * tcos));
      b2 := (A * ((A + 1.0) - (A - 1.0) * tcos - beta * tsin));
      a0 := ((A + 1.0) + (A - 1.0) * tcos + beta * tsin);
      a1 := (-2.0 * ((A - 1.0) + (A + 1.0) * tcos));
      a2 := ((A + 1.0) + (A - 1.0) * tcos - beta * tsin);
    end;
    // hishelf
    if FFilterType = 8 then
    begin
      b0 := (A * ((A + 1.0) + (A - 1.0) * tcos + beta * tsin));
      b1 := (-2.0 * A * ((A - 1.0) + (A + 1.0) * tcos));
      b2 := (A * ((A + 1.0) + (A - 1.0) * tcos - beta * tsin));
      a0 := ((A + 1.0) - (A - 1.0) * tcos + beta * tsin);
      a1 := (2.0 * ((A - 1.0) - (A + 1.0) * tcos));
      a2 := ((A + 1.0) - (A - 1.0) * tcos - beta * tsin);
    end;
  end
  else  //other filter types
  begin
    omega := 2 * pi * FFreq / FSampleRate;
    tsin := sin(omega);
    tcos := cos(omega);
    if FQIsBandWidth then
      alpha := tsin * sinh(log2(2) / 2 * q_ * omega / tsin)
    else
      alpha := tsin / (2 * q_);
    //lowpass
    if FFilterType = 0 then
    begin
      b0 := (1 - tcos) / 2;
      b1 := 1 - tcos;
      b2 := (1 - tcos) / 2;
      a0 := 1 + alpha;
      a1 := -2 * tcos;
      a2 := 1 - alpha;
    end
    else //hipass
    if FFilterType = 1 then
    begin
      b0 := (1 + tcos) / 2;
      b1 := -(1 + tcos);
      b2 := (1 + tcos) / 2;
      a0 := 1 + alpha;
      a1 := -2 * tcos;
      a2 := 1 - alpha;
    end
    else //bandpass CSG
    if FFilterType = 2 then
    begin
      b0 := tsin / 2;
      b1 := 0;
      b2 := -tsin / 2;
      a0 := 1 + alpha;
      a1 := -1 * tcos;
      a2 := 1 - alpha;
    end
    else //bandpass CZPG
    if FFilterType = 3 then
    begin
      b0 := alpha;
      b1 := 0.0;
      b2 := -alpha;
      a0 := 1.0 + alpha;
      a1 := -2.0 * tcos;
      a2 := 1.0 - alpha;
    end
    else  //notch
    if FFilterType = 4 then
    begin
      b0 := 1.0;
      b1 := -2.0 * tcos;
      b2 := 1.0;
      a0 := 1.0 + alpha;
      a1 := -2.0 * tcos;
      a2 := 1.0 - alpha;
    end
    else   //allpass
    if FFilterType = 5 then
    begin
      b0 := 1.0 - alpha;
      b1 := -2.0 * tcos;
      b2 := 1.0 + alpha;
      a0 := 1.0 + alpha;
      a1 := -2.0 * tcos;
      a2 := 1.0 - alpha;
    end;
  end;

  b0a0 := {%H-}b0 / {%H-}a0;
  b1a0 := {%H-}b1 / a0;
  b2a0 := {%H-}b2 / a0;
  a1a0 := {%H-}a1 / a0;
  a2a0 := {%H-}a2 / a0;
end;

procedure TRbjEQFilter.Clear();
var
  ch: integer;
  p: PBuffer;
begin
  for ch := Low(FBuf) to High(FBuf) do
  begin
    p := @FBuf[ch];
    p^.in1 := 0;
    p^.in2 := 0;
    p^.out1 := 0;
    p^.out2 := 0;
  end;
end;

procedure TRbjEQFilter.ProcessReplacing(Input: psingle; sampleframes: integer);
var
  i, ch, stride: integer;
  LastOut, i1, i2, o1, o2: single;
  inp: PSingle;
  chBuf: PBuffer;
begin
  stride := Length(FBuf);
  for ch := Low(FBuf) to High(FBuf) do
  begin
    inp := Input;
    chBuf := @FBuf[ch];
    i1 := chBuf^.in1;
    i2 := chBuf^.in2;
    o1 := chBuf^.out1;
    o2 := chBuf^.out2;
    for i := 0 to SampleFrames - 1 do
    begin
      LastOut := b0a0 * (inp^) + b1a0 * i1 + b2a0 * i2 - a1a0 * o1 - a2a0 * o2;
      i2 := i1;
      i1 := inp^;
      o2 := o1;
      o1 := LastOut;
      inp^ := LastOut;
      Inc(inp, stride);
    end;
    chBuf^.in1 := i1;
    chBuf^.in2 := i2;
    chBuf^.out1 := o1;
    chBuf^.out2 := o2;
    Inc(Input);
  end;
end;

end.

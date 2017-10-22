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
    FFilterType: TRbjEqFilterType;
    FFreq, FQ, FDBGain: single;
    FQIsBandWidth: boolean;
    function GetChannels: integer;
    procedure SetChannels(AValue: integer);
  public
    constructor Create();
    procedure UpdateParameter();
    procedure Clear();
    procedure ProcessReplacing(Input: psingle; sampleframes: integer);
    property FilterType: TRbjEqFilterType read FFilterType write FFilterType;
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
  FFilterType := ftLowPass;
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
  if FFilterType in [ftPeaking, ftLowShelf, ftHiShelf] then
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

    case FFilterType of
      ftPeaking: begin
        b0 := 1.0 + alpha * A;
        b1 := -2.0 * tcos;
        b2 := 1.0 - alpha * A;
        a0 := 1.0 + alpha / A;
        a1 := -2.0 * tcos;
        a2 := 1.0 - alpha / A;
      end;
      ftLowShelf: begin
        b0 := (A * ((A + 1.0) - (A - 1.0) * tcos + beta * tsin));
        b1 := (2.0 * A * ((A - 1.0) - (A + 1.0) * tcos));
        b2 := (A * ((A + 1.0) - (A - 1.0) * tcos - beta * tsin));
        a0 := ((A + 1.0) + (A - 1.0) * tcos + beta * tsin);
        a1 := (-2.0 * ((A - 1.0) + (A + 1.0) * tcos));
        a2 := ((A + 1.0) + (A - 1.0) * tcos - beta * tsin);
      end;
      ftHiShelf: begin
        b0 := (A * ((A + 1.0) + (A - 1.0) * tcos + beta * tsin));
        b1 := (-2.0 * A * ((A - 1.0) + (A + 1.0) * tcos));
        b2 := (A * ((A + 1.0) + (A - 1.0) * tcos - beta * tsin));
        a0 := ((A + 1.0) - (A - 1.0) * tcos + beta * tsin);
        a1 := (2.0 * ((A - 1.0) - (A + 1.0) * tcos));
        a2 := ((A + 1.0) - (A - 1.0) * tcos - beta * tsin);
      end;
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
    case FFilterType of
      ftLowPass: begin
        b0 := (1 - tcos) / 2;
        b1 := 1 - tcos;
        b2 := (1 - tcos) / 2;
        a0 := 1 + alpha;
        a1 := -2 * tcos;
        a2 := 1 - alpha;
      end;
      ftHiPass: begin
        b0 := (1 + tcos) / 2;
        b1 := -(1 + tcos);
        b2 := (1 + tcos) / 2;
        a0 := 1 + alpha;
        a1 := -2 * tcos;
        a2 := 1 - alpha;
      end;
      ftBandPassCSG: begin
        b0 := tsin / 2;
        b1 := 0;
        b2 := -tsin / 2;
        a0 := 1 + alpha;
        a1 := -1 * tcos;
        a2 := 1 - alpha;
      end;
      ftBandPassCZPG: begin
        b0 := alpha;
        b1 := 0.0;
        b2 := -alpha;
        a0 := 1.0 + alpha;
        a1 := -2.0 * tcos;
        a2 := 1.0 - alpha;
      end;
      ftNotch: begin
        b0 := 1.0;
        b1 := -2.0 * tcos;
        b2 := 1.0;
        a0 := 1.0 + alpha;
        a1 := -2.0 * tcos;
        a2 := 1.0 - alpha;
      end;
      ftAllPass: begin
        b0 := 1.0 - alpha;
        b1 := -2.0 * tcos;
        b2 := 1.0 + alpha;
        a0 := 1.0 + alpha;
        a1 := -2.0 * tcos;
        a2 := 1.0 - alpha;
      end;
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
  Ch: integer;
  P: PBuffer;
begin
  for Ch := Low(FBuf) to High(FBuf) do
  begin
    P := @FBuf[Ch];
    P^.in1 := 0;
    P^.in2 := 0;
    P^.out1 := 0;
    P^.out2 := 0;
  end;
end;

procedure TRbjEQFilter.ProcessReplacing(Input: psingle; sampleframes: integer);
var
  I, Ch, Stride: integer;
  LastOut, i1, i2, o1, o2: single;
  Inp: PSingle;
  ChBuf: PBuffer;
begin
  Stride := Length(FBuf);
  for Ch := Low(FBuf) to High(FBuf) do
  begin
    Inp := Input;
    ChBuf := @FBuf[Ch];
    i1 := ChBuf^.in1;
    i2 := ChBuf^.in2;
    o1 := ChBuf^.out1;
    o2 := ChBuf^.out2;
    for I := 0 to SampleFrames - 1 do
    begin
      LastOut := b0a0 * (Inp^) + b1a0 * i1 + b2a0 * i2 - a1a0 * o1 - a2a0 * o2;
      i2 := i1;
      i1 := Inp^;
      o2 := o1;
      o1 := LastOut;
      Inp^ := LastOut;
      Inc(Inp, Stride);
    end;
    ChBuf^.in1 := i1;
    ChBuf^.in2 := i2;
    ChBuf^.out1 := o1;
    ChBuf^.out2 := o2;
    Inc(Input);
  end;
end;

end.

unit Reverb;

{$mode objfpc}{$H+}

interface

uses
  MultiTapDelay, AllPass, FixedCircularBuffer, Vibrato, OnePole;

type
  { TReverb }

  TReverb = class
  private
    FHPFrequency: single;
    FLPFrequency: single;
    FPreDelay: single;

    FEarlyMTDelay: array of TMultiTapDelay;
    FEarlyVib: array of TVibratoFilter;
    FEarlyAPF: array of TAllPassFilter;

    FLateAPF: array of TAllPassFilter;

    FTailGain: array of single;
    FTailAPF: array of TAllPassFilter;
    FTailLPF: array of TOnePole;
    FTailHPF: array of TOnePole;
    FCBuffer: array of TFixedCircularBuffer;

    FChannels: integer;
    FDecay: single;
    FSampleRate: single;
    FBuffer: array of single;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure UpdateParameter();
    procedure Clear();
    procedure ProcessReplacing(Input: PSingle; sampleframes: integer);
    property SampleRate: single read FSampleRate write FSampleRate;
    property Channels: integer read FChannels write FChannels;
    property PreDelay: single read FPreDelay write FPreDelay;
    property Decay: single read FDecay write FDecay;
    property LPFrequency: single read FLPFrequency write FLPFrequency;
    property HPFrequency: single read FHPFrequency write FHPFrequency;
  end;

implementation

uses
  Math;

type
  TParam = record
    Duration: single;
    Gain: single;
  end;

  TVibParam = record
    Frequency: single;
    Depth: single;
  end;

const
  EarlyMTDelayParams: array[0..1] of array[0..3] of TParam =
    (
    (
    (Duration: 449 / 44100; Gain: 1.50),
    (Duration: 4919 / 44100; Gain: 0.46),
    (Duration: 6911 / 44100; Gain: 0.21),
    (Duration: 9623 / 44100; Gain: 0.15)
    ),
    (
    (Duration: 523 / 44100; Gain: 1.50),
    (Duration: 4523 / 44100; Gain: 0.45),
    (Duration: 7237 / 44100; Gain: 0.22),
    (Duration: 9743 / 44100; Gain: 0.15)
    )
    );
  EarlyVibParams: array[0..3] of TVibParam =
    (
    (Frequency: 5.0; Depth: 0.00012),
    (Frequency: 3.9; Depth: 0.00019),
    (Frequency: 3.3; Depth: 0.00013),
    (Frequency: 4.1; Depth: 0.00011)
    );
  EarlyAPFParams: array[0..3] of TParam =
    (
    (Duration: 317 / 44100; Gain: 0.61),
    (Duration: 269 / 44100; Gain: 0.49),
    (Duration: 557 / 44100; Gain: 0.48),
    (Duration: 613 / 44100; Gain: 0.49)
    );
  LateAPFParams: array[0..5] of TParam =
    (
    (Duration: 2741 / 44100; Gain: 0.70),
    (Duration: 2851 / 44100; Gain: 0.70),
    (Duration: 3643 / 44100; Gain: 0.66),
    (Duration: 3719 / 44100; Gain: 0.66),
    (Duration: 5171 / 44100; Gain: 0.50),
    (Duration: 5119 / 44100; Gain: 0.50)
    );
  TailAPFParams: array[0..3] of TParam =
    (
    (Duration: 1753 / 44100; Gain: 0.74),
    (Duration: 1459 / 44100; Gain: 0.74),
    (Duration: 2741 / 44100; Gain: 0.54),
    (Duration: 2851 / 44100; Gain: 0.50)
    );
  TailCombParams: array[0..1] of TParam =
    (
    (Duration: 9623 / 44100; Gain: 0),
    (Duration: 9743 / 44100; Gain: 0)
    );

{ TReverb }

constructor TReverb.Create;
var
  I: integer;
begin
  inherited Create();
  PreDelay := 0;
  SetLength(FEarlyMTDelay, Length(EarlyMTDelayParams));
  for I := Low(EarlyMTDelayParams) to High(EarlyMTDelayParams) do
    FEarlyMTDelay[I] := TMultiTapDelay.Create(Length(EarlyMTDelayParams[I]));
  SetLength(FEarlyVib, Length(EarlyVibParams));
  for I := Low(EarlyVibParams) to High(EarlyVibParams) do
    FEarlyVib[I] := TVibratoFilter.Create();
  SetLength(FEarlyAPF, Length(EarlyAPFParams));
  for I := Low(EarlyAPFParams) to High(EarlyAPFParams) do
    FEarlyAPF[I] := TAllPassFilter.Create();
  SetLength(FLateAPF, Length(LateAPFParams));
  for I := Low(LateAPFParams) to High(LateAPFParams) do
    FLateAPF[I] := TAllPassFilter.Create();
  SetLength(FTailAPF, Length(TailAPFParams));
  for I := Low(TailAPFParams) to High(TailAPFParams) do
    FTailAPF[I] := TAllPassFilter.Create();
  SetLength(FTailLPF, Length(TailCombParams));
  for I := Low(TailCombParams) to High(TailCombParams) do
    FTailLPF[I] := TOnePole.Create();
  SetLength(FTailHPF, Length(TailCombParams));
  for I := Low(TailCombParams) to High(TailCombParams) do
    FTailHPF[I] := TOnePole.Create();
  SetLength(FCBuffer, Length(TailCombParams));
  for I := Low(TailCombParams) to High(TailCombParams) do
    FCBuffer[I] := TFixedCircularBuffer.Create();
  SetLength(FTailGain, Length(TailCombParams));
  FDecay := 0.2;
  FLPFrequency := 24000;
  FHPFrequency := 0;
end;

destructor TReverb.Destroy;
var
  I: integer;
begin
  for I := Low(EarlyMTDelayParams) to High(EarlyMTDelayParams) do
    FEarlyMTDelay[I].Free;
  for I := Low(EarlyVibParams) to High(EarlyVibParams) do
    FEarlyVib[I].Free;
  for I := Low(EarlyAPFParams) to High(EarlyAPFParams) do
    FEarlyAPF[I].Free;
  for I := Low(LateAPFParams) to High(LateAPFParams) do
    FLateAPF[I].Free;
  for I := Low(TailAPFParams) to High(TailAPFParams) do
    FTailAPF[I].Free;
  for I := Low(TailCombParams) to High(TailCombParams) do
    FTailLPF[I].Free;
  for I := Low(TailCombParams) to High(TailCombParams) do
    FTailHPF[I].Free;
  for I := Low(TailCombParams) to High(TailCombParams) do
    FCBuffer[I].Free;
  inherited Destroy;
end;

procedure TReverb.UpdateParameter;
var
  I, J: integer;
begin
  for I := Low(EarlyMTDelayParams) to High(EarlyMTDelayParams) do
  begin
    FEarlyMTDelay[I].SampleRate := FSampleRate;
    for J := Low(EarlyMTDelayParams[I]) to High(EarlyMTDelayParams[I]) do
    begin
      FEarlyMTDelay[I].Duration[J] := FPreDelay + EarlyMTDelayParams[I][J].Duration;
      FEarlyMTDelay[I].Gain[J] := EarlyMTDelayParams[I][J].Gain;
    end;
    FEarlyMTDelay[I].UpdateParameter();
  end;

  for I := Low(EarlyVibParams) to High(EarlyVibParams) do
  begin
    FEarlyVib[I].SampleRate := FSampleRate;
    FEarlyVib[I].Channels := 1;
    FEarlyVib[I].Frequency := EarlyVibParams[I].Frequency;
    FEarlyVib[I].MaxDepth := EarlyVibParams[I].Depth;
    FEarlyVib[I].UpdateParameter();
  end;

  for I := Low(EarlyAPFParams) to High(EarlyAPFParams) do
  begin
    FEarlyAPF[I].SampleRate := FSampleRate;
    FEarlyAPF[I].Duration := EarlyAPFParams[I].Duration;
    FEarlyAPF[I].FeedbackGain := EarlyAPFParams[I].Gain;
    FEarlyAPF[I].UpdateParameter();
  end;

  for I := Low(LateAPFParams) to High(LateAPFParams) do
  begin
    FLateAPF[I].SampleRate := FSampleRate;
    FLateAPF[I].Duration := LateAPFParams[I].Duration;
    FLateAPF[I].FeedbackGain := LateAPFParams[I].Gain;
    FLateAPF[I].UpdateParameter();
  end;

  for I := Low(TailAPFParams) to High(TailAPFParams) do
  begin
    FTailAPF[I].SampleRate := FSampleRate;
    FTailAPF[I].Duration := TailAPFParams[I].Duration;
    FTailAPF[I].FeedbackGain := TailAPFParams[I].Gain;
    FTailAPF[I].UpdateParameter();
  end;

  for I := Low(TailCombParams) to High(TailCombParams) do
  begin
    FTailLPF[I].SetLPF(FSampleRate, Max(0, Min(FSampleRate * 0.5, FLPFrequency)));
  end;

  for I := Low(TailCombParams) to High(TailCombParams) do
  begin
    FTailHPF[I].SetHPF(FSampleRate, Max(0, Min(FSampleRate * 0.5, FHPFrequency)));
  end;

  for I := Low(TailCombParams) to High(TailCombParams) do
  begin
    FCBuffer[I].Samples := Trunc(TailCombParams[I].Duration * FSampleRate);
    FTailGain[I] := Power(0.001, TailCombParams[I].Duration / FDecay);
  end;
end;

procedure TReverb.Clear;
var
  I: integer;
begin
  for I := Low(EarlyMTDelayParams) to High(EarlyMTDelayParams) do
    FEarlyMTDelay[I].Clear();
  for I := Low(EarlyVibParams) to High(EarlyVibParams) do
    FEarlyVib[I].Clear();
  for I := Low(EarlyAPFParams) to High(EarlyAPFParams) do
    FEarlyAPF[I].Clear();
  for I := Low(LateAPFParams) to High(LateAPFParams) do
    FLateAPF[I].Clear();
  for I := Low(TailAPFParams) to High(TailAPFParams) do
    FTailAPF[I].Clear();
  for I := Low(TailCombParams) to High(TailCombParams) do
    FTailLPF[I].Clear();
  for I := Low(TailCombParams) to High(TailCombParams) do
    FTailHPF[I].Clear();
  for I := Low(TailCombParams) to High(TailCombParams) do
    FCBuffer[I].Clear();
end;

procedure TReverb.ProcessReplacing(Input: PSingle; sampleframes: integer);
const
  Div2 = 1.0 / 2.0;
  Div3 = 1.0 / 3.0;
  Denom = 1e-24;
var
  I, Ch, Chs: integer;
  Inp, P0, P1, P2, P3, P4, P5, P6, P7: PSingle;
  Divider, G0, G1: single;
begin
  if Length(FBuffer) < sampleframes * 8 then
    SetLength(FBuffer, sampleframes * 8);

  // convert to mono
  Chs := FChannels;
  Inp := Input;
  P0 := @FBuffer[0];
  for I := 0 to sampleframes - 1 do
  begin
    P0^ := Inp^;
    Inc(Inp);
    for Ch := 1 to Chs - 1 do
    begin
      P0^ := P0^ + Inp^;
      Inc(Inp);
    end;
    Inc(P0);
  end;
  if Chs > 1 then
  begin
    P0 := @FBuffer[0];
    Divider := 1 / Chs;
    for I := 0 to sampleframes - 1 do
    begin
      P0^ := P0^ * Divider;
      Inc(P0);
    end;
  end;

  P0 := @FBuffer[sampleframes * 0];
  P1 := @FBuffer[sampleframes * 1];
  P2 := @FBuffer[sampleframes * 2];
  P3 := @FBuffer[sampleframes * 3];
  P4 := @FBuffer[sampleframes * 4];
  P5 := @FBuffer[sampleframes * 5];
  P6 := @FBuffer[sampleframes * 6];
  P7 := @FBuffer[sampleframes * 7];

  // make early reflection
  FEarlyMTDelay[0].Process(P0, P6, sampleframes);
  FEarlyMTDelay[1].Process(P0, P7, sampleframes);
  FEarlyVib[0].Process(P6, P6, sampleframes);
  FEarlyVib[1].Process(P7, P7, sampleframes);
  FEarlyAPF[0].Process(P6, P6, sampleframes);
  FEarlyAPF[1].Process(P7, P7, sampleframes);
  FEarlyAPF[2].Process(P6, P6, sampleframes);
  FEarlyAPF[3].Process(P7, P7, sampleframes);

  // make late reflection
  FLateAPF[0].Process(P6, P0, sampleframes);
  FLateAPF[1].Process(P7, P1, sampleframes);
  FLateAPF[2].Process(P6, P2, sampleframes);
  FLateAPF[3].Process(P7, P3, sampleframes);
  FLateAPF[4].Process(P6, P4, sampleframes);
  FLateAPF[5].Process(P7, P5, sampleframes);

  //FillChar(P0^, sampleframes * sizeof(single), 0);
  //FillChar(P1^, sampleframes * sizeof(single), 0);
  //FillChar(P2^, sampleframes * sizeof(single), 0);
  //FillChar(P3^, sampleframes * sizeof(single), 0);
  //FillChar(P4^, sampleframes * sizeof(single), 0);
  //FillChar(P5^, sampleframes * sizeof(single), 0);
  //FillChar(P6^, sampleframes * sizeof(single), 0);
  //FillChar(P7^, sampleframes * sizeof(single), 0);

  // make tail
  for I := 0 to sampleframes - 1 do
  begin
    P0^ := ((P0^ + P2^ + P4^) * Div3 + P6^) * Div2;
    P1^ := ((P1^ + P3^ + P5^) * Div3 + P7^) * Div2;
    Inc(P0);
    Inc(P1);
    Inc(P2);
    Inc(P3);
    Inc(P4);
    Inc(P5);
    Inc(P6);
    Inc(P7);
  end;

  P0 := @FBuffer[sampleframes * 0];
  P1 := @FBuffer[sampleframes * 1];
  FCBuffer[0].Read(P2, sampleframes);
  FCBuffer[1].Read(P3, sampleframes);
  FEarlyVib[2].Process(P2, P2, sampleframes);
  FEarlyVib[3].Process(P3, P3, sampleframes);
  FTailAPF[0].Process(P2, P2, sampleframes);
  FTailAPF[1].Process(P3, P3, sampleframes);
  FTailAPF[2].Process(P2, P2, sampleframes);
  FTailAPF[3].Process(P3, P3, sampleframes);

  G0 := FTailGain[0];
  G1 := FTailGain[1];
  for I := 0 to sampleframes - 1 do
  begin
    P0^ := P0^ + P2^ * G0 + Denom;
    P1^ := P1^ + P3^ * G1 + Denom;
    Inc(P0);
    Inc(P1);
    Inc(P2);
    Inc(P3);
  end;

  P0 := @FBuffer[sampleframes * 0];
  P1 := @FBuffer[sampleframes * 1];
  FTailLPF[0].Process(P0, P0, sampleframes);
  FTailHPF[0].Process(P0, P0, sampleframes);
  FTailLPF[1].Process(P1, P1, sampleframes);
  FTailHPF[1].Process(P1, P1, sampleframes);
  FCBuffer[0].Write(P0, sampleframes);
  FCBuffer[1].Write(P1, sampleframes);

  Inp := Input;
  case Chs of
    1:
    begin
      for I := 0 to sampleframes - 1 do
      begin
        Inp^ := (P0^ + P1^) * Div2;
        Inc(Inp);
        Inc(P0);
        Inc(P1);
      end;
    end;
    2:
    begin
      for I := 0 to sampleframes - 1 do
      begin
        Inp^ := P0^;
        Inc(Inp);
        Inp^ := P1^;
        Inc(Inp);
        Inc(P0);
        Inc(P1);
      end;
    end;
  end;
end;

end.

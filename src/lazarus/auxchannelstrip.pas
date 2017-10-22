unit AuxChannelStrip;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Reverb;

type
  { TAuxChannelStrip }

  TAuxChannelStrip = class
  private
    FDecay: single;
    FHPFreq: single;
    FLPFreq: single;
    FPreDelay: single;
    FReverb: TReverb;

    FCurrentPostGain: single;
    FPostGain: single;
  public
    constructor Create();
    destructor Destroy(); override;
    function UpdateEffects(SampleRate: single; Channels: integer): boolean;
    procedure ProcessEffects(Buffer: PSingle; Len: integer);
    procedure ClearEffects();

    property Reverb: TReverb read FReverb;
    property PreDelay: single read FPreDelay write FPreDelay;
    property Decay: single read FDecay write FDecay;
    property LPFreq: single read FLPFreq write FLPFreq;
    property HPFreq: single read FHPFreq write FHPFreq;

    property CurrentPostGain: single read FCurrentPostGain write FCurrentPostGain;
    property PostGain: single read FPostGain write FPostGain;
  end;

implementation

{ TAuxChannelStrip }

constructor TAuxChannelStrip.Create;
begin
  inherited Create();
  FReverb := TReverb.Create();

  FPostGain := 0;
  FCurrentPostGain := 0;
end;

destructor TAuxChannelStrip.Destroy;
begin
  FReverb.Free;
  inherited Destroy;
end;

function TAuxChannelStrip.UpdateEffects(SampleRate: single; Channels: integer): boolean;
begin
  Result := False;

  if (Channels <> FReverb.Channels) or (SampleRate <> FReverb.SampleRate) or
    (FLPFreq <> FReverb.LPFrequency) or (FHPFreq <> FReverb.HPFrequency) or (FPreDelay <> FReverb.PreDelay) or (FDecay <> FReverb.Decay) then
  begin
    FReverb.Channels := Channels;
    FReverb.SampleRate := SampleRate;
    FReverb.PreDelay := FPreDelay;
    FReverb.Decay := FDecay;
    FReverb.LPFrequency := FLPFreq;
    FReverb.HPFrequency := FHPFreq;
    FReverb.UpdateParameter();
    Result := True;
  end;

  if FCurrentPostGain <> FPostGain then
  begin
    FCurrentPostGain := FPostGain;
    Result := True;
  end;
end;

procedure TAuxChannelStrip.ProcessEffects(Buffer: PSingle; Len: integer);
begin
  FReverb.ProcessReplacing(Buffer, Len);
end;

procedure TAuxChannelStrip.ClearEffects;
begin
  FReverb.Clear();
end;

end.

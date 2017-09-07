unit ChannelStrip;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  GHashMap, CircularBuffer, Lag, RbjEQFilter, MDADynamics;

type
  { TChannelStrip }

  TChannelStrip = class
  private
    FBuffer: TCircularBuffer;
    FFresh: boolean;

    FCurrentPreGain: single;
    FPreGain: single;

    FLag: TLagFilter;
    FLagDuration: single;

    FLoShelf: TRbjEQFilter;
    FLoShelfFreq: single;
    FLoShelfGain: single;

    FHiShelf: TRbjEQFilter;
    FHiShelfFreq: single;
    FHiShelfGain: single;

    FDynamics: TMDADynamics;
    FDynThreshold: single;
    FDynRatio: single;
    FDynAttack: single;
    FDynRelease: single;

    FCurrentPostGain: single;
    FPostGain: single;
  public
    constructor Create();
    destructor Destroy(); override;
    function UpdateEffects(SampleRate: single; Channels: integer): boolean;
    procedure ProcessEffects(Buffer: PSingle; Len: integer);
    procedure ClearEffects();
    function CalcDependentDuration(): single;
    property Fresh: boolean read FFresh write FFresh;
    property Buffer: TCircularBuffer read FBuffer;

    property CurrentPreGain: single read FCurrentPreGain write FCurrentPreGain;
    property PreGain: single read FPreGain write FPreGain;

    property Lag: TLagFilter read FLag;
    property LagDuration: single read FLagDuration write FLagDuration;

    property LoShelf: TRbjEQFilter read FLoShelf;
    property LoShelfFreq: single read FLoShelfFreq write FLoShelfFreq;
    property LoShelfGain: single read FLoShelfGain write FLoShelfGain;

    property HiShelf: TRbjEQFilter read FHiShelf;
    property HiShelfFreq: single read FHiShelfFreq write FHiShelfFreq;
    property HiShelfGain: single read FHiShelfGain write FHiShelfGain;

    property Dynamics: TMDADynamics read FDynamics;
    property DynThreshold: single read FDynThreshold write FDynThreshold;
    property DynRatio: single read FDynRatio write FDynRatio;
    property DynAttack: single read FDynAttack write FDynAttack;
    property DynRelease: single read FDynRelease write FDynRelease;

    property CurrentPostGain: single read FCurrentPostGain write FCurrentPostGain;
    property PostGain: single read FPostGain write FPostGain;
  end;

  { TIntegerHash }

  TIntegerHash = class
    class function hash(k: integer; n: SizeUInt): SizeUInt;
    class function equal(a: integer; b: integer): boolean;
  end;

  TChannelStripMap = specialize THashmap<integer, TChannelStrip, TIntegerHash>;

implementation

{ TChannelStrip }

constructor TChannelStrip.Create;
const
  r2 = 1.4142135623730951;
begin
  inherited Create();
  FBuffer := TCircularBuffer.Create();
  FFresh := False;

  FPreGain := 0;
  FCurrentPreGain := 0;

  FLag := TLagFilter.Create();

  FLoShelfFreq := 200;
  FLoShelfGain := 0;
  FLoShelf := TRbjEQFilter.Create();
  FLoShelf.FilterType := 7;
  FLoShelf.Q := 1.0 / r2;

  FHiShelfFreq := 3000;
  FHiShelfGain := 0;
  FHiShelf := TRbjEQFilter.Create();
  FHiShelf.FilterType := 8;
  FHiShelf.Q := 1.0 / r2;

  FDynamics := TMDADynamics.Create();
  FDynamics.Output := 0;
  FDynThreshold := 0.40;
  FDynRatio := 0.60;
  FDynAttack := 0.18;
  FDynRelease := 0.55;

  FPostGain := 0;
  FCurrentPostGain := 0;
end;

destructor TChannelStrip.Destroy;
begin
  FBuffer.Free;
  FDynamics.Free;
  FHiShelf.Free;
  FLoShelf.Free;
  FLag.Free;
  inherited Destroy;
end;

function TChannelStrip.UpdateEffects(SampleRate: single; Channels: integer): boolean;
begin
  Result := False;
  if FCurrentPreGain <> FPreGain then
  begin
    FCurrentPreGain := FPreGain;
    Result := True;
  end;

  if (Channels <> FLag.Channels) or (SampleRate <> FLag.SampleRate) or
    (FLagDuration <> FLag.Duration) then
  begin
    FLag.Channels := Channels;
    FLag.SampleRate := SampleRate;
    FLag.Duration := FLagDuration;
    FLag.UpdateParameter();
    Result := True;
  end;

  if (Channels <> FLoShelf.Channels) or (SampleRate <> FLoShelf.SampleRate) or
    (FLoShelfFreq <> FLoShelf.Freq) or (FLoShelfGain <> FLoShelf.DBGain) then
  begin
    FLoShelf.Channels := Channels;
    FLoShelf.SampleRate := SampleRate;
    FLoShelf.Freq := FLoShelfFreq;
    FLoShelf.DBGain := FLoShelfGain;
    FLoShelf.UpdateParameter();
    Result := True;
  end;

  if (Channels <> FHiShelf.Channels) or (SampleRate <> FHiShelf.SampleRate) or
    (FHiShelfFreq <> FHiShelf.Freq) or (FHiShelfGain <> FHiShelf.DBGain) then
  begin
    FHiShelf.Channels := Channels;
    FHiShelf.SampleRate := SampleRate;
    FHiShelf.Freq := FHiShelfFreq;
    FHiShelf.DBGain := FHiShelfGain;
    FHiShelf.UpdateParameter();
    Result := True;
  end;

  if (Channels <> FDynamics.Channels) or (SampleRate <> FDynamics.SampleRate) or
    (FDynThreshold <> FDynamics.Threshold) or (FDynRatio <> FDynamics.Ratio) or
    (FDynAttack <> FDynamics.Attack) or (FDynRelease <> FDynamics.Release) then
  begin
    FDynamics.Channels := Channels;
    FDynamics.SampleRate := SampleRate;
    FDynamics.Threshold := FDynThreshold;
    FDynamics.Ratio := FDynRatio;
    FDynamics.Attack := FDynAttack;
    FDynamics.Release := FDynRelease;
    FDynamics.UpdateParameter();
    Result := True;
  end;

  if FCurrentPostGain <> FPostGain then
  begin
    FCurrentPostGain := FPostGain;
    Result := True;
  end;
end;

procedure TChannelStrip.ProcessEffects(Buffer: PSingle; Len: integer);
begin
  if FLagDuration > 0 then
    FLag.ProcessReplacing(Buffer, Len);
  if FLoShelfGain <> 0 then
    FLoShelf.ProcessReplacing(Buffer, Len);
  if FHiShelfGain <> 0 then
    FHiShelf.ProcessReplacing(Buffer, Len);
  if FDynRatio <> 0.2 then
    FDynamics.ProcessReplacing(Buffer, Len);
end;

procedure TChannelStrip.ClearEffects;
begin
  FLag.Clear();
  FLoShelf.Clear();
  FHiShelf.Clear();
  FDynamics.Clear();
end;

function TChannelStrip.CalcDependentDuration: single;
var
  v: single;
begin
  Result := 0;
  if FLag.SampleRate = 0 then
    Exit;

  if Result < FLagDuration then
    Result := FLagDuration;

  v := 2 / FLoShelf.SampleRate;
  if Result < v then
    Result := v;

  v := FDynamics.ReleaseDuration + FDynamics.AttackDuration;
  if Result < v then
    Result := v;
end;

{ TIntegerHash }

class function TIntegerHash.hash(k: integer; n: SizeUInt): SizeUInt;
begin
  Result := k {%H-}mod n;
end;

class function TIntegerHash.equal(a: integer; b: integer): boolean;
begin
  Result := a = b;
end;

end.

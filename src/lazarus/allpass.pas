unit AllPass;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

type
  { TAllPassFilter }

  TAllPassFilter = class
  private
    FDuration: single;
    FFeedbackGain: single;
    FSampleRate: single;
    FBuffer: array of single;
    FBufPos: PSingle;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure UpdateParameter();
    procedure Clear();
    procedure Process(Input: PSingle; Output: PSingle; sampleframes: integer);
    property SampleRate: single read FSampleRate write FSampleRate;
    property Duration: single read FDuration write FDuration;
    property FeedbackGain: single read FFeedbackGain write FFeedbackGain;
  end;

implementation

{ TAllPassFilter }

constructor TAllPassFilter.Create;
begin
  inherited Create();
  FDuration := 0;
  FSampleRate := 0;
end;

destructor TAllPassFilter.Destroy;
begin
  inherited Destroy;
end;

procedure TAllPassFilter.UpdateParameter;
var
  L: integer;
begin
  L := Trunc(FDuration * FSampleRate);
  if Length(FBuffer) <> L then begin
    SetLength(FBuffer, L);
    Clear();
  end;
end;

procedure TAllPassFilter.Clear;
begin
  FillChar(FBuffer[0], Length(FBuffer) * SizeOf(single), 0);
  FBufPos := @FBuffer[0];
end;

procedure TAllPassFilter.Process(Input: PSingle; Output: PSingle;
  sampleframes: integer); inline;
const
  Denom = 1e-24;
var
  I: integer;
  Buf, BufStart, BufEnd: PSingle;
  S, Gain, Gain2: single;
begin
  Gain := FFeedbackGain;
  Gain2 := Gain * Gain;
  BufStart := @FBuffer[0];
  BufEnd := BufStart;
  Inc(BufEnd, Length(FBuffer));
  Buf := FBufPos;
  for I := 0 to sampleframes - 1 do
  begin
    S := Input^ * -Gain + Buf^;
    Buf^ := Input^ * (1 - Gain2) + Buf^ * Gain + Denom;
    Output^ := S;
    Inc(Buf);
    if Buf = BufEnd then
      Buf := BufStart;
    Inc(Input);
    Inc(Output);
  end;
  FBufPos := Buf;
end;

end.

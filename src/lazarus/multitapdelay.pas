unit MultiTapDelay;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

type
  { TMultiTapDelay }

  TMultiTapDelay = class
  private
    FSampleRate: single;
    FDuration: array of single;
    FGain: array of single;
    FBuffer: array of array of single;
    FBufPos: array of PSingle;
    FTemp: array of single;
    function GetDuration(Index: integer): single;
    function GetGain(Index: integer): single;
    procedure SetDuration(Index: integer; AValue: single);
    procedure SetGain(Index: integer; AValue: single);
  public
    constructor Create(const N: integer);
    destructor Destroy(); override;
    procedure UpdateParameter();
    procedure Clear();
    procedure Process(Input: PSingle; Output: PSingle; sampleframes: integer);
    procedure ProcessReplacing(Input: PSingle; sampleframes: integer);
    property SampleRate: single read FSampleRate write FSampleRate;
    property Duration[Index: integer]: single read GetDuration write SetDuration;
    property Gain[Index: integer]: single read GetGain write SetGain;
  end;

implementation

{ TMultiTapDelay }

function TMultiTapDelay.GetDuration(Index: integer): single;
begin
  Result := FDuration[Index];
end;

function TMultiTapDelay.GetGain(Index: integer): single;
begin
  Result := FGain[Index];
end;

procedure TMultiTapDelay.SetDuration(Index: integer; AValue: single);
begin
  FDuration[Index] := AValue;
end;

procedure TMultiTapDelay.SetGain(Index: integer; AValue: single);
begin
  FGain[Index] := AValue;
end;

constructor TMultiTapDelay.Create(const N: integer);
begin
  inherited Create();
  FSampleRate := 0;
  SetLength(FDuration, N);
  SetLength(FGain, N);
  SetLength(FBuffer, N);
  SetLength(FBufPos, N);
end;

destructor TMultiTapDelay.Destroy;
begin
  inherited Destroy;
end;

procedure TMultiTapDelay.UpdateParameter;
var
  I, L: integer;
  Modified: Boolean;
begin
  Modified := False;
  for I := Low(FGain) to High(FGain) do
  begin
    L := Trunc(FDuration[I] * FSampleRate);
    if Length(FBuffer[I]) <> L then begin
      SetLength(FBuffer[I], L);
      Modified := True;
    end;
  end;
  if Modified then Clear();
end;

procedure TMultiTapDelay.Clear;
var
  I: integer;
begin
  for I := Low(FGain) to High(FGain) do
  begin
    FillChar(FBuffer[I][0], Length(FBuffer[I]) * SizeOf(single), 0);
    FBufPos[I] := @FBuffer[I][0];
  end;
end;

procedure TMultiTapDelay.Process(Input: PSingle; Output: PSingle;
  sampleframes: integer);
var
  I, J: integer;
  Buf, BufStart, BufEnd, Inp, Outp: PSingle;
  G: single;
begin
  for I := Low(FGain) to High(FGain) do
  begin
    G := FGain[I];
    BufStart := @FBuffer[I][0];
    BufEnd := BufStart;
    Inc(BufEnd, Length(FBuffer[I]));
    Buf := FBufPos[I];
    Inp := Input;
    Outp := Output;
    if I = 0 then
      for J := 0 to sampleframes - 1 do
      begin
        Outp^ := Buf^ * G;
        Buf^ := Inp^;
        Inc(Buf);
        if Buf = BufEnd then
          Buf := BufStart;
        Inc(Inp);
        Inc(Outp);
      end
    else
      for J := 0 to sampleframes - 1 do
      begin
        Outp^ := Outp^ + Buf^ * G;
        Buf^ := Inp^;
        Inc(Buf);
        if Buf = BufEnd then
          Buf := BufStart;
        Inc(Inp);
        Inc(Outp);
      end;
    FBufPos[I] := Buf;
  end;
end;

procedure TMultiTapDelay.ProcessReplacing(Input: PSingle; sampleframes: integer); inline;
begin
  if Length(FTemp) < sampleframes then
    SetLength(FTemp, sampleframes);
  Process(Input, @FTemp[0], sampleframes);
  Move(FTemp[0], Input^, sampleframes * sizeof(single));
end;

end.

unit FixedCircularBuffer;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

type
  { TFixedCircularBuffer }

  TFixedCircularBuffer = class
  private
    FBufPos: integer;
    FBuffer: array of single;
    function GetSamples: integer;
    procedure SetSamples(AValue: integer);
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Clear();
    procedure Read(Output: PSingle; sampleframes: integer);
    procedure Write(Input: PSingle; sampleframes: integer);
    property Samples: integer read GetSamples write SetSamples;
  end;

implementation

{ TFixedCircularBuffer }

function TFixedCircularBuffer.GetSamples: integer;
begin
  Result := Length(FBuffer);
end;

procedure TFixedCircularBuffer.SetSamples(AValue: integer);
begin
  SetLength(FBuffer, AValue);
  Clear();
end;

constructor TFixedCircularBuffer.Create();
begin
  inherited Create();
end;

destructor TFixedCircularBuffer.Destroy;
begin
  inherited Destroy;
end;

procedure TFixedCircularBuffer.Clear;
begin
  FillChar(FBuffer[0], Length(FBuffer) * SizeOf(single), 0);
  FBufPos := 0;
end;

procedure TFixedCircularBuffer.Read(Output: PSingle; sampleframes: integer);
var
  I: integer;
  Buf, BufStart, BufEnd: PSingle;
begin
  BufStart := @FBuffer[0];
  BufEnd := BufStart;
  Inc(BufEnd, Length(FBuffer));
  Buf := BufStart + FBufPos;
  for I := 0 to sampleframes - 1 do
  begin
    Output^ := Buf^;
    Inc(Buf);
    if Buf = BufEnd then
      Buf := BufStart;
    Inc(Output);
  end;
end;

procedure TFixedCircularBuffer.Write(Input: PSingle; sampleframes: integer);
var
  I: integer;
  Buf, BufStart, BufEnd: PSingle;
begin
  BufStart := @FBuffer[0];
  BufEnd := BufStart;
  Inc(BufEnd, Length(FBuffer));
  Buf := BufStart + FBufPos;
  for I := 0 to sampleframes - 1 do
  begin
    Buf^ := Input^;
    Inc(Buf);
    if Buf = BufEnd then
      Buf := BufStart;
    Inc(Input);
  end;
  FBufPos := Buf - BufStart;
end;

end.


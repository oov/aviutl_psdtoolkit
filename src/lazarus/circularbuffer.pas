unit CircularBuffer;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

type
  { TCircularBuffer }

  TCircularBuffer = class
  private
    FBuffer: array of byte;
    FRemain, FReadPos, FWritePos: integer;
    procedure Expand(NewSize: integer);
  public
    constructor Create();
    destructor Destroy(); override;
    function Write(const Buffer; Len: integer): integer;
    function Read(const Buffer; Len: integer): integer;
    function Discard(Len: integer): integer;
    procedure Clear();
    property Remain: integer read FRemain;
    property ReadPos: integer read FReadPos;
    property WritePos: integer read FWritePos;
  end;

implementation

{ TCircularBuffer }

procedure TCircularBuffer.Expand(NewSize: integer);
var
  OldLen: integer;
begin
  OldLen := Length(FBuffer);
  SetLength(FBuffer, NewSize);
  if OldLen - FReadPos < FRemain then
  begin
    Move(FBuffer[FReadPos], FBuffer[NewSize - (OldLen - FReadPos)], OldLen - FReadPos);
    FReadPos := NewSize - (OldLen - FReadPos);
  end;
end;

constructor TCircularBuffer.Create;
begin
  inherited Create();
  FRemain := 0;
  FWritePos := 0;
  FReadPos := 0;
end;

destructor TCircularBuffer.Destroy;
begin
  inherited Destroy;
end;

function TCircularBuffer.Write(const Buffer; Len: integer): integer;
var
  L: integer;
  P: PByte;
begin
  if Length(FBuffer) - FRemain < Len then
    Expand(FRemain + Len);
  Result := Len;
  if Len = 0 then
    Exit;
  P := @Buffer;
  L := Len;
  if FWritePos = Length(FBuffer) then
    FWritePos := 0;
  if L > Length(FBuffer) - FWritePos then
    L := Length(FBuffer) - FWritePos;
  Move(P^, FBuffer[FWritePos], L);
  Dec(Len, L);
  Inc(FRemain, L);
  Inc(P, L);
  Inc(FWritePos, L);
  if Len = 0 then
    Exit;
  if FWritePos = Length(FBuffer) then
    FWritePos := 0;
  Move(P^, FBuffer[FWritePos], Len);
  Inc(FWritePos, Len);
  Inc(FRemain, Len);
end;

function TCircularBuffer.Read(const Buffer; Len: integer): integer;
var
  L: integer;
  P: PByte;
begin
  if Len > FRemain then
    Len := FRemain;
  Result := Len;
  if Len = 0 then
    Exit;
  P := @Buffer;
  L := Len;
  if FReadPos = Length(FBuffer) then
    FReadPos := 0;
  if L > Length(FBuffer) - FReadPos then
    L := Length(FBuffer) - FReadPos;
  Move(FBuffer[FReadPos], P^, L);
  Dec(Len, L);
  Dec(FRemain, L);
  Inc(P, L);
  Inc(FReadPos, L);
  if Len = 0 then
    Exit;
  if FReadPos = Length(FBuffer) then
    FReadPos := 0;
  Move(FBuffer[FReadPos], P^, Len);
  Inc(FReadPos, Len);
  Dec(FRemain, Len);
end;

function TCircularBuffer.Discard(Len: integer): integer;
var
  L: integer;
begin
  if Len > FRemain then
    Len := FRemain;
  Result := Len;
  if Len = 0 then
    Exit;
  L := Len;
  if L > Length(FBuffer) - FReadPos then
    L := Length(FBuffer) - FReadPos;
  Dec(Len, L);
  Dec(FRemain, L);
  Inc(FReadPos, L);
  if FReadPos = Length(FBuffer) then
    FReadPos := 0;
  if Len = 0 then
    Exit;
  Inc(FReadPos, Len);
  Dec(FRemain, Len);
end;

procedure TCircularBuffer.Clear;
begin
  FRemain := 0;
  FReadPos := 0;
  FWritePos := 0;
end;


end.


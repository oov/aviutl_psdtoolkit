unit fftsg;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

type

  { TRDFT }

  TRDFT = class
  private
    FIP: PInteger;
    FW, FBuf, FWindow: PDouble;
    FN: integer;
    procedure ToPower();
  public
    constructor Create(const N: integer);
    destructor Destroy(); override;
    function Execute(const a: PSmallint; const Channels: integer): PDouble;
  end;

implementation

uses
  Math;

{$LINK fftsg.o}// gcc -O3 -march=pentium4 -c fftsg.c

procedure rdft(n: integer; isgn: integer; a: PDouble; ip: PInteger; w: PDouble);
  cdecl; external Name 'rdft';

function _sin(x: double): double; cdecl; export;
begin
  Result := Sin(x);
end;

function _cos(x: double): double; cdecl; export;
begin
  Result := Cos(x);
end;

{ TRDFT }

constructor TRDFT.Create(const N: integer);
var
  I: integer;
begin
  inherited Create();

  FN := N;
  FBuf := GetMem(N * SizeOf(double) * 2);
  FWindow := FBuf + N;
  FIP := GetMem((2 + Trunc(Sqrt(double(N) * 0.5) + 1)) * SizeOf(integer));
  FW := GetMem((N * 5) div 4 * SizeOf(double));
  FIP^ := 0;

  for I := 0 to N - 1 do
    (FWindow + I)^ := 1.0 / 32768.0 * (0.54 - 0.46 * cos(2 * PI * I / N));
end;

destructor TRDFT.Destroy();
begin
  FreeMem(FIP);
  FreeMem(FW);
  FreeMem(FBuf);
  inherited Destroy();
end;

function TRDFT.Execute(const a: PSmallint; const Channels: integer): PDouble;
var
  I, J: integer;
  S, Divider: double;
  Src: PSmallint;
  Buf, Wnd: PDouble;
begin
  Buf := FBuf;
  Wnd := FWindow;
  Src := a;
  case Channels of
    1:
      for I := 0 to FN - 1 do
        (Buf + I)^ := double((Src + I)^) * (Wnd + I)^;
    2:
      for I := 0 to FN - 1 do
      begin
        (Buf + I)^ := double(integer(Src^) + integer((Src + 1)^)) * (Wnd + I)^ * 0.5;
        Inc(Src, 2);
      end;
    else
    begin
      Divider := double(Channels);
      for I := 0 to FN - 1 do
      begin
        S := 0;
        for J := 0 to Channels - 1 do
          S := S + double((Src + J)^);
        (Buf + I)^ := S * (Wnd + I)^ * Divider;
        Inc(Src, Channels);
      end;
    end;
  end;
  rdft(FN, 1, Buf, FIP, FW);
  ToPower();
  Result := Buf;
end;

procedure TRDFT.ToPower();
var
  I: integer;
  x, y: double;
  Buf: PDouble;
begin
  Buf := FBuf;
  for I := 0 to FN div 2 - 1 do
  begin
    x := (Buf + I * 2)^;
    y := (Buf + I * 2 + 1)^;
    (Buf + I)^ := sqrt(x * x + y * y);
  end;
end;

end.

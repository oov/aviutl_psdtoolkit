unit OnePole;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

type
  { TOnePole }

  TOnePole = class
  private
    FA0, FA1, FB1, FI1, FO1: single;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Clear();
    procedure Process(Input: PSingle; Output: PSingle; sampleframes: integer);
    procedure SetLPF(const Freq, Cut: single);
    procedure SetHPF(const Freq, Cut: single);
  end;

implementation

{ TOnePole }

constructor TOnePole.Create;
begin
  inherited Create();
end;

destructor TOnePole.Destroy;
begin
  inherited Destroy;
end;

procedure TOnePole.Clear;
begin
  FI1 := 0;
  FO1 := 0;
end;

procedure TOnePole.Process(Input: PSingle; Output: PSingle; sampleframes: integer); inline;
const
  Denom = 1e-24;
var
  i: integer;
  a0_, a1_, b1_, i1_, o1_: single;
begin
  a0_ := FA0;
  a1_ := FA1;
  b1_ := FB1;
  i1_ := FI1;
  o1_ := FO1;
  for i := 0 to SampleFrames - 1 do
  begin
    o1_ := Input^*a0_ + i1_*a1_ + o1_*b1_ + Denom;
    i1_ := Input^;
    Output^ := o1_;
    Inc(Input);
    Inc(Output);
  end;
  FI1 := i1_;
  FO1 := o1_;
end;

procedure TOnePole.SetLPF(const Freq, Cut: single);
var
  W, Norm, C: single;
begin
  W := 2.0 * Freq;
  C := Cut * 2.0 * PI;
  Norm := 1.0 / (C + W);
  FA0 := C * Norm;
  FA1 := FA0;
  FB1 := (W - C) * Norm;
end;

procedure TOnePole.SetHPF(const Freq, Cut: single);
var
  W, Norm, C: single;
begin
  W := 2.0 * Freq;
  C := Cut * 2.0 * PI;
  Norm := 1.0 / (C + W);
  FA0 := W * Norm;
  FA1 := -FA0;
  FB1 := (W - C) * Norm;
end;

end.


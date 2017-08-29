// This code is based on mdaDynamics.

// mda VST plug-ins

// Copyright (c) 2008 Paul Kellett

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
// IN THE SOFTWARE.
unit MDADynamics;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

type

  { TMDADynamics }

  TMDADynamics = class
  private
    FSampleRate: single;
    FChannels: integer;
    FThresh: single;
    FRatio: single;
    FOutput: single;
    FAttack: single;
    FRelease: single;
    FLimiter: single;
    FGateThresh: single;
    FGateAttack: single;
    FGateDecay: single;
    FFxMix: single;

    thr, rat, env, env2, att, rel, trim, lthr, xthr, xrat, dry: single;
    genv, gatt, irel: single;
    FMode: integer;
    function GetAttackDisp: string;
    function GetAttackDuration: single;
    function GetFxMixDisp: string;
    function GetGateAttackDisp: string;
    function GetGateDecayDisp: string;
    function GetGateThresholdDisp: string;
    function GetOutputDisp: string;
    function GetLimiterDisp: string;
    function GetRatioDisp: string;
    function GetReleaseDisp: string;
    function GetReleaseDuration: single;
    function GetThresholdDisp: string;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure UpdateParameter();
    procedure Clear();
    procedure ProcessReplacing(Input: PSingle; sampleframes: integer);
    property SampleRate: single read FSampleRate write FSampleRate;
    property Channels: integer read FChannels write FChannels;
    property Threshold: single read FThresh write FThresh;
    property ThresholdDisp: string read GetThresholdDisp;
    property Ratio: single read FRatio write FRatio;
    property RatioDisp: string read GetRatioDisp;
    property Output: single read FOutput write FOutput;
    property OutputDisp: string read GetOutputDisp;
    property Attack: single read FAttack write FAttack;
    property AttackDuration: single read GetAttackDuration;
    property AttackDisp: string read GetAttackDisp;
    property Release: single read FRelease write FRelease;
    property ReleaseDuration: single read GetReleaseDuration;
    property ReleaseDisp: string read GetReleaseDisp;
    property Limiter: single read FLimiter write FLimiter;
    property LimiterDisp: string read GetLimiterDisp;
    property GateThreshold: single read FGateThresh write FGateThresh;
    property GateThresholdDisp: string read GetGateThresholdDisp;
    property GateAttack: single read FGateAttack write FGateAttack;
    property GateAttackDisp: string read GetGateAttackDisp;
    property GateDecay: single read FGateDecay write FGateDecay;
    property GateDecayDisp: string read GetGateDecayDisp;
    property FxMix: single read FFxMix write FFxMix;
    property FxMixDisp: string read GetFxMixDisp;
  end;

implementation

uses
  SysUtils, Math;

{ TMDADynamics }

function TMDADynamics.GetAttackDisp: string;
begin
  Result := Format('%d', [Trunc(-301030.1 / (FSampleRate * log10(1.0 - att)))]);
end;

function TMDADynamics.GetAttackDuration: single;
begin
  Result := Trunc(-301030.1 / (FSampleRate * log10(1.0 - att))) / 1000000;
end;

function TMDADynamics.GetFxMixDisp: string;
begin
  Result := Format('%d', [Trunc(100.0 * FFxMix)]);
end;

function TMDADynamics.GetGateAttackDisp: string;
begin
  Result := Format('%d', [Trunc(-301030.1 / (FSampleRate * log10(1.0 - gatt)))]);
end;

function TMDADynamics.GetGateDecayDisp: string;
begin
  Result := Format('%d', [Trunc(-1806.0 / (FSampleRate * log10(xrat)))]);
end;

function TMDADynamics.GetGateThresholdDisp: string;
begin
  if xthr = 0.0 then
    Result := 'OFF'
  else
    Result := Format('%d', [Trunc(60.0 * FGateThresh - 60.0)]);
end;

function TMDADynamics.GetOutputDisp: string;
begin
  Result := Format('%d', [Trunc(40.0 * FOutput)]);
end;

function TMDADynamics.GetLimiterDisp: string;
begin
  if lthr = 0.0 then
    Result := 'OFF'
  else
    Result := Format('%d', [Trunc(30.0 * FLimiter - 20.0)]);
end;

function TMDADynamics.GetRatioDisp: string;
begin
  if FRatio > 0.58 then
  begin
    if FRatio < 0.62 then
      Result := 'oo'
    else
      Result := Format('%0.2f', [-rat]);
  end
  else
  begin
    if FRatio < 0.2 then
      Result := Format('%0.2f', [0.5 + 2.5 * FRatio])
    else
      Result := Format('%0.2f', [1.0 / (1.0 - rat)]);
  end;
end;

function TMDADynamics.GetReleaseDisp: string;
begin
  Result := Format('%d', [Trunc(-301.0301 / (FSampleRate * log10(1.0 - rel)))]);
end;

function TMDADynamics.GetReleaseDuration: single;
begin
  Result := Trunc(-301.0301 / (FSampleRate * log10(1.0 - rel))) / 1000.0;
end;

function TMDADynamics.GetThresholdDisp: string;
begin
  Result := Format('%d', [Trunc(40.0 * FThresh - 40.0)]);
end;

constructor TMDADynamics.Create;
begin
  inherited Create();
  FThresh := 0.60;
  FRatio := 0.40;
  FOutput := 0.10;
  FAttack := 0.18;
  FRelease := 0.55;
  FLimiter := 1.00;
  FGateThresh := 0.00;
  FGateAttack := 0.10;
  FGateDecay := 0.50;
  FFxMix := 1.00;
  env := 0;
  env2 := 0;
  genv := 0;
end;

destructor TMDADynamics.Destroy;
begin
  inherited Destroy;
end;

procedure TMDADynamics.UpdateParameter;
begin
  FMode := 0;
  thr := Power(10.0, 2.0 * FThresh - 2.0);
  rat := 2.5 * FRatio - 0.5;
  if rat > 1.0 then
  begin
    rat := 1.0 + 16.0 * (rat - 1.0) * (rat - 1.0);
    FMode := 1;
  end;
  if rat < 0.0 then
  begin
    rat := 0.6 * rat;
    FMode := 1;
  end;
  trim := Power(10.0, 2.0 * FOutput);
  att := Power(10.0, -0.002 - 2.0 * FAttack);
  rel := Power(10.0, -2.0 - 3.0 * FRelease);

  if FLimiter > 0.98 then
    lthr := 0.0
  else
  begin
    lthr := 0.99 * Power(10.0, Trunc(30.0 * FLimiter - 20.0) / 20.0);
    FMode := 1;
  end;

  if FGateThresh < 0.02 then
    xthr := 0.0 //expander
  else
  begin
    xthr := power(10.0, 3.0 * FGateThresh - 3.0);
    FMode := 1;
  end;
  xrat := 1.0 - power(10.0, -2.0 - 3.3 * FGateDecay);
  irel := power(10.0, -2.0 / FSampleRate);
  gatt := power(10.0, -0.002 - 3.0 * FGateAttack);

  if (rat < 0.0) and (thr < 0.1) then
    rat := rat * thr * 15.0;

  dry := 1.0 - FFxMix;
  trim := trim * FFxMix;
end;

procedure TMDADynamics.Clear;
begin
  env := 0;
  env2 := 0;
  genv := 0;
end;

procedure TMDADynamics.ProcessReplacing(Input: PSingle; sampleframes: integer);
var
  inp, pEnd: PSingle;
  ch: integer;
  i, g, e, e2, ra, re, at, ga, tr, th, lth, xth, ge, y: single;
begin
  e := env;
  e2 := env2;
  ra := rat;
  re := 1.0 - rel;
  at := att;
  ga := gatt;
  tr := trim;
  th := thr;
  lth := lthr;
  xth := xthr;
  ge := genv;
  y := dry;
  pEnd := Input;
  Inc(pEnd, sampleframes * FChannels);

  if FMode <> 0 then
  begin //comp/gate/lim
    if lth = 0.0 then
      lth := 1000.0;
    while Input <> pEnd do
    begin
      inp := Input;
      i := 0;
      for ch := 0 to FChannels - 1 do
      begin
        i := Max(Abs(inp^), i);
        Inc(inp);
      end;

      if i > e then
        e := e + at * (i - e)
      else
        e := e * re;
      if i > e then
        e2 := i
      else
        e2 := e2 * re;

      if e > th then
        g := tr / (1.0 + ra * ((e / th) - 1.0))
      else
        g := tr;

      if g < 0.0 then
        g := 0.0;
      if g * e2 > lth then
        g := lth / e2; // limit

      if e > xth then
        ge := ge + ga - ga * ge
      else
        ge := ge * xrat; // gate

      for ch := 0 to FChannels - 1 do
      begin
        Input^ := Input^ * (g * ge + y);
        Inc(Input);
      end;
    end;
  end
  else
  begin
    while Input <> pEnd do
    begin
      inp := Input;
      i := 0;
      for ch := 0 to FChannels - 1 do
      begin
        i := Max(Abs(inp^), i);
        Inc(inp);
      end;

      if i > e then
        e := e + at * (i - e)
      else
        e := e * re;
      if e > th then
        g := tr / (1.0 + ra * ((e / th) - 1.0))
      else
        g := tr;

      for ch := 0 to FChannels - 1 do
      begin
        Input^ := Input^ * (g + y);
        Inc(Input);
      end;
    end;
  end;
  if e < 1.0e-10 then
    env := 0
  else
    env := e;
  if e2 < 1.0e-10 then
    env2 := 0
  else
    env2 := e2;
  if ge < 1.0e-10 then
    genv := 0
  else
    genv := ge;
end;

end.

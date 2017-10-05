unit AudioMixer;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Windows, AviUtl, ChannelStrip, MDADynamics;

type

  { TAudioMixer }

  TAudioMixer = class
  private
    FChannelStripEntry, FMasterChannelStripEntry: TFilterDLL;
    FLastFrame: integer;
    FStrips: TChannelStripMap;
    FLimiter: TMDADynamics;
    FMixBuf: array of single;
    FTempBuf: array of single;
    FRepairBuf: array of byte;

    FFont, FParamsLabel, FIDCombo: THandle;
    FTimerID: PtrUInt;
    FSaving, FChanged: boolean;
    FSelectedID: integer;

    function GetChannelStripEntry: PFilterDLL;
    function GetMasterChannelStripEntry: PFilterDLL;
  public
    constructor Create();
    destructor Destroy(); override;
    function ChannelStripWndProc(Window: HWND; Message: UINT; WP: WPARAM;
      {%H-}LP: LPARAM; {%H-}Edit: Pointer; Filter: PFilter): integer;
    function ChannelStripProc(fp: PFilter; fpip: PFilterProcInfo): boolean;
    function MasterChannelStripProc(fp: PFilter; fpip: PFilterProcInfo): boolean;
    procedure UpdateParamsView();
    property ChannelStripEntry: PFilterDLL read GetChannelStripEntry;
    property MasterChannelStripEntry: PFilterDLL read GetMasterChannelStripEntry;
  end;

implementation

uses
  SysUtils, Classes, Math, Ver;

type
  { TParams }

  TParams = record
    ID: integer;
    PreGain: single;
    LagDuration: single;
    LoShelfFreq: single;
    LoShelfGain: single;
    HiShelfFreq: single;
    HiShelfGain: single;
    DynThreshold: single;
    DynRatio: single;
    DynAttack: single;
    DynRelease: single;
    PostGain: single;
  end;

function sliderToDB(const v: single): single;
begin
  if v < 0 then
    if 1 + v < 0.00001 then
      Result := -96
    else
      Result := 20 * Log10(1 + v)
  else
    Result := v * 24;//Result := -20*Log10(0.0625+(1-v)*0.9375);
end;

function Params(const values: PInteger): TParams;
begin
  Result.ID := values[0];

  Result.PreGain := sliderToDB(values[1] / 10000.0);

  Result.LagDuration := values[2] / 1000.0;
  Result.LoShelfFreq := single(values[3]);
  Result.LoShelfGain := sliderToDB(values[4] / 10000.0);

  Result.HiShelfFreq := single(values[5]);
  Result.HiShelfGain := sliderToDB(values[6] / 10000.0);

  Result.DynThreshold := single(values[7]) / 10000.0;
  Result.DynRatio := single(values[8]) / 10000.0 * 0.4 + 0.2;
  Result.DynAttack := single(values[9]) / 10000.0;
  Result.DynRelease := single(values[10]) / 10000.0 * 0.82;

  Result.PostGain := sliderToDB(values[12] / 10000.0);
end;

function ParamsToString(const Strip: TChannelStrip): string;
begin
  Result := '';
  Result := Result + '[Pre Gain]'#13#10;
  Result := Result + Format('  Gain      %0.2f dB'#13#10, [Strip.CurrentPreGain]);
  Result := Result + #13#10;
  Result := Result + '[Lag]'#13#10;
  Result := Result + Format('  Duration  %s ms'#13#10, [Strip.Lag.DurationDisp]);
  Result := Result + #13#10;
  Result := Result + '[Low-Shelf EQ]'#13#10;
  Result := Result + Format('  Frequency %0.0f Hz'#13#10, [Strip.LoShelf.Freq]);
  Result := Result + Format('  Gain      %0.2f dB'#13#10, [Strip.LoShelf.DBGain]);
  //Result := Result + Format('    Q:         %0.2f'#13#10, [Strip.LoShelf.Q]);
  Result := Result + #13#10;
  Result := Result + '[High-Shelf EQ]'#13#10;
  Result := Result + Format('  Frequency %0.0f Hz'#13#10, [Strip.HiShelf.Freq]);
  Result := Result + Format('  Gain      %0.2f dB'#13#10, [Strip.HiShelf.DBGain]);
  //Result := Result + Format('    Q:         %0.2f'#13#10, [Strip.HiShelf.Q]);
  Result := Result + #13#10;
  Result := Result + '[Compressor]'#13#10;
  Result := Result + Format('  Threshold %s dB'#13#10, [Strip.Dynamics.ThresholdDisp]);
  Result := Result + Format('  Ratio     %s:1'#13#10, [Strip.Dynamics.RatioDisp]);
  Result := Result + Format('  Attack    %s μs'#13#10, [Strip.Dynamics.AttackDisp]);
  Result := Result + Format('  Release   %s ms'#13#10, [Strip.Dynamics.ReleaseDisp]);
  Result := Result + #13#10;
  Result := Result + '[Post Gain]'#13#10;
  Result := Result + Format('  Gain      %0.2f dB'#13#10, [Strip.CurrentPostGain]);
  Result := Result + #13#10;
end;

procedure ODS(const Fmt: string; const Args: array of const);
begin
  OutputDebugStringW(PWideChar(WideString(Format('psdtoolkit auf: ' + Fmt, Args))));
end;

procedure HideAll(Parent: THandle);
var
  h: THandle;
begin
  h := 0;
  while True do
  begin
    h := FindWindowExW(Parent, h, nil, nil);
    if h = 0 then
      Exit;
    ShowWindow(h, SW_HIDE);
  end;
end;

{ TAudioMixer }

function TAudioMixer.ChannelStripWndProc(Window: HWND; Message: UINT;
  WP: WPARAM; LP: LPARAM; Edit: Pointer; Filter: PFilter): integer;
var
  r: TRect;
  i: integer;
begin
  Result := 0;
  case Message of
    WM_FILTER_INIT:
    begin
      FFont := CreateFont(-14, 0, 0, 0, FW_NORMAL, 0, 0, 0, DEFAULT_CHARSET,
        OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
        DEFAULT_PITCH or FF_DONTCARE, 'Courier New');
      HideAll(Window);

      GetWindowRect(Window, {%H-}r);
      FIDCombo := CreateWindowW('COMBOBOX', nil, WS_CHILD or
        WS_TABSTOP or WS_VISIBLE or CBS_DROPDOWNLIST or WS_VSCROLL,
        8, 8, r.Width - 16 - GetSystemMetrics(SM_CXFIXEDFRAME) * 2, 400,
        Window, 1, Filter^.DLLHInst, nil);
      SendMessageW(FIDCombo, WM_SETFONT, WPARAM(FFont), 0);
      for i := 0 to 100 do
        SendMessage(FIDCombo, CB_ADDSTRING, 0,
          {%H-}LPARAM(PChar(Format('ID: %03d', [i]))));
      SendMessageW(FIDCombo, CB_SETCURSEL, 0, 0);

      FParamsLabel := CreateWindowW('STATIC', '', WS_CHILD or
        WS_VISIBLE or ES_LEFT, 8, 8 + 40, r.Width - 16, r.Height -
        16 - 40, Window, 0, Filter^.DLLHInst, nil);

      SendMessageW(FParamsLabel, WM_SETFONT, WPARAM(FFont), 0);

      FTimerID := SetTimer(Window, 1, 40, nil);
    end;
    WM_FILTER_EXIT:
    begin
      SendMessageW(FIDCombo, WM_SETFONT, 0, 0);
      SendMessageW(FParamsLabel, WM_SETFONT, 0, 0);
      KillTimer(Window, FTimerID);
      DeleteObject(FFont);
      FFont := 0;
    end;
    WM_FILTER_SAVE_START: FSaving := True;
    WM_FILTER_SAVE_END: FSaving := False;
    WM_COMMAND:
    begin
      if (LOWORD(WP) = 1) and (HIWORD(WP) = CBN_SELCHANGE) then
      begin
        FSelectedID := SendMessageW(FIDCombo, CB_GETCURSEL, 0, 0);
        FChanged := True;
        UpdateParamsView();
      end;
    end;
    WM_TIMER:
    begin
      if WP = 1 then
        UpdateParamsView();
    end;
  end;
end;

function TAudioMixer.ChannelStripProc(fp: PFilter; fpip: PFilterProcInfo): boolean;
var
  len, ID: integer;
  Strip: TChannelStrip;
  prms: TParams;
begin
  Result := True;
  ID := fp^.Track[0];
  if ID = -1 then
    Exit;

  len := fpip^.AudioCh * fpip^.AudioN;
  if FStrips.GetValue(ID, Strip) then
  begin
    // is ID used duplicately?
    if Strip.Fresh then
    begin
      FillChar(fpip^.AudioP^, len * SizeOf(smallint), 0);
      Exit;
    end;
  end
  else
    Strip := TChannelStrip.Create();

  prms := Params(fp^.Track);
  Strip.PreGain := prms.PreGain;
  Strip.LagDuration := prms.LagDuration;
  Strip.LoShelfFreq := prms.LoShelfFreq;
  Strip.LoShelfGain := prms.LoShelfGain;
  Strip.HiShelfFreq := prms.HiShelfFreq;
  Strip.HiShelfGain := prms.HiShelfGain;
  Strip.DynThreshold := prms.DynThreshold;
  Strip.DynRatio := prms.DynRatio;
  Strip.DynAttack := prms.DynAttack;
  Strip.DynRelease := prms.DynRelease;
  Strip.PostGain := prms.PostGain;
  Strip.Fresh := True;
  Strip.Buffer.Write(fpip^.AudioP^, len * SizeOf(smallint));
  FillChar(fpip^.AudioP^, len * SizeOf(smallint), 0);
  FStrips.Items[ID] := Strip;
end;

function TAudioMixer.MasterChannelStripProc(fp: PFilter; fpip: PFilterProcInfo): boolean;
const
  ToSingle = 1.0 / 32768.0;
  ToInt = 32768.0;
var
  i, j, len: integer;
  p: PSmallint;
  pMixBuf, pTempBuf: PSingle;
  Duration, SampleRate, Gain, v: single;
  finfo: TFileInfo;
  IT: TChannelStripMap.TIterator;
  Strip: TChannelStrip;
  Processed: boolean;
begin
  Result := True;

  len := fpip^.AudioCh * fpip^.AudioN;
  if fp^.ExFunc^.GetFileInfo(fpip^.EditP, @finfo) = AVIUTL_FALSE then
    SampleRate := 48000
  else
    SampleRate := finfo.AudioRate;

  if FLastFrame + 1 <> fpip^.Frame then
  begin
    // the audio stream was jumped for some reason.
    // load previous frame to avoid audio glitch.

    // 1. discard current state and find longest lag duration
    FLimiter.Clear();
    Duration := FLimiter.AttackDuration + FLimiter.ReleaseDuration;
    IT := FStrips.Iterator();
    if IT <> nil then
    begin
      try
        repeat
          Strip := IT.Value;
          v := Strip.CalcDependentDuration();
          if Duration < v then
            Duration := v;
          Strip.Buffer.Clear();
          Strip.ClearEffects();
          Strip.Fresh := False;
          FStrips[IT.Key] := Strip;
        until not IT.Next;
      finally
        IT.Free;
      end;
    end;

    // 2. load recent frames
    j := (Trunc(SampleRate * Duration)) div fpip^.AudioN + 1;
    for i := fpip^.Frame - j to fpip^.Frame do
    begin
      if i < 0 then
      begin
        continue;
      end;
      FLastFrame := i - 1;
      j := fp^.ExFunc^.GetAudioFiltered(fpip^.EditP, i, nil);
      j := j * fpip^.AudioCh * SizeOf(smallint);
      if j > Length(FRepairBuf) then
        SetLength(FRepairBuf, j);
      fp^.ExFunc^.GetAudioFiltered(fpip^.EditP, i, @FRepairBuf[0]);
    end;
    Move(FRepairBuf[0], fpip^.AudioP^, len * SizeOf(smallint));
    Exit;
  end;

  FLastFrame := fpip^.Frame;

  // for mix
  // 1. write to the mix buffer from the original stream
  if len > Length(FMixBuf) then
  begin
    SetLength(FMixBuf, len);
    SetLength(FTempBuf, len);
  end;
  pMixBuf := @FMixBuf[0];
  p := fpip^.AudioP;
  for i := 0 to len - 1 do
  begin
    pMixBuf^ := single(p^) * ToSingle;
    Inc(p);
    Inc(pMixBuf);
  end;

  // 2. mix all track buffers
  Processed := False;
  IT := FStrips.Iterator();
  if IT <> nil then
  begin
    try
      repeat
        Strip := IT.Value;
        p := fpip^.AudioP;
        j := Strip.Buffer.Read(p^, SizeOf(smallint) * len);
        if (not Strip.Fresh) and (j = 0) then
        begin
          Strip.Free;
          FStrips.erase(IT);
          continue;
        end;
        Processed := True;
        Strip.Fresh := False;
        if j < SizeOf(smallint) * len then
          FillChar(PByte(fpip^.AudioP)[j], SizeOf(smallint) * len - j, 0);

        if Strip.UpdateEffects(SampleRate, fpip^.AudioCh) and
          (IT.Key = FSelectedID) then
        begin
          FChanged := True;
        end;

        Gain := Power(10, Strip.CurrentPreGain / 20);
        pTempBuf := @FTempBuf[0];
        j := j shr 1;
        for i := 0 to j - 1 do
        begin
          pTempBuf^ := single(p^) * ToSingle * Gain;
          Inc(p);
          Inc(pTempBuf);
        end;

        pMixBuf := @FMixBuf[0];
        pTempBuf := @FTempBuf[0];
        Strip.ProcessEffects(pTempBuf, fpip^.AudioN);
        Gain := Power(10, Strip.CurrentPostGain / 20);
        for i := 0 to j - 1 do
        begin
          pMixBuf^ := pMixBuf^ + pTempBuf^ * Gain;
          Inc(pMixBuf);
          Inc(pTempBuf);
        end;
        FStrips.Items[IT.Data.Key] := Strip;
      until not IT.Next;
    finally
      IT.Free;
    end;
  end;
  if not Processed then Exit;

  // 3. apply limitter
  if (fpip^.AudioCh <> FLimiter.Channels) or (SampleRate <> FLimiter.SampleRate) then
  begin
    FLimiter.SampleRate := SampleRate;
    FLimiter.Channels := fpip^.AudioCh;
    FLimiter.UpdateParameter();
  end;
  FLimiter.ProcessReplacing(@FMixBuf[0], fpip^.AudioN);

  // 4. write back to the original buffer
  pMixBuf := @FMixBuf[0];
  p := fpip^.AudioP;
  for i := 0 to len - 1 do
  begin
    p^ := Trunc(pMixBuf^ * ToInt);
    Inc(p);
    Inc(pMixBuf);
  end;
end;

procedure TAudioMixer.UpdateParamsView;
var
  Strip: TChannelStrip;
begin
  if (not FChanged)or FSaving then
    Exit;

  FChanged := False;
  if not FStrips.GetValue(FSelectedID, Strip) then
  begin
    SetWindowText(FParamsLabel, nil);
    Exit;
  end;
  SetWindowText(FParamsLabel, PChar(ParamsToString(Strip)));
end;

function TAudioMixer.GetChannelStripEntry: PFilterDLL;
begin
  Result := @FChannelStripEntry;
end;

function TAudioMixer.GetMasterChannelStripEntry: PFilterDLL;
begin
  Result := @FMasterChannelStripEntry;
end;

constructor TAudioMixer.Create;
const
  ChannelStripPluginName = #$83#$60#$83#$83#$83#$93#$83#$6C#$83#$8B#$83#$58#$83#$67#$83#$8A#$83#$62#$83#$76; // チャンネルストリップ
  ChannelStripPluginInfo = ChannelStripPluginName + ' ' + Version;
  ChannelStripTrackN = 13;
  ChannelStripTrackName: array[0..ChannelStripTrackN - 1] of PChar =
    (
    'ID',
    #$93#$FC#$97#$CD#$89#$B9#$97#$CA, // 入力音量
    #$92#$78#$89#$84, // 遅延
    'Lo Freq',
    'Lo Gain',
    'Hi Freq',
    'Hi Gain',
    'Threshold',
    'Ratio',
    'Attack',
    'Release',
    nil,
    #$8F#$6F#$97#$CD#$89#$B9#$97#$CA // 出力音量
    );
  ChannelStripTrackDefault: array[0..ChannelStripTrackN - 1] of integer =
    (-1, 0, 0, 200, 0, 3000, 0, 6000, 0, 1800, 5500, 0, 0);
  ChannelStripTrackS: array[0..ChannelStripTrackN - 1] of integer =
    (-1, -10000, 0, 1, -10000, 1, -10000, 0, 0, 0, 0, 0, -10000);
  ChannelStripTrackE: array[0..ChannelStripTrackN - 1] of integer =
    (100, 10000, 500, 24000, 10000, 24000, 10000, 10000, 10000, 10000, 10000, 0, 10000);
  MasterChannelStripPluginName =
    #$83#$7D#$83#$58#$83#$5E#$81#$5B#$83#$60#$83#$83#$83#$93#$83#$6C#$83#$8B#$83#$58#$83#$67#$83#$8A#$83#$62#$83#$76; // マスターチャンネルストリップ
  MasterChannelStripPluginInfo = MasterChannelStripPluginName + ' ' + Version;
begin
  inherited Create();
  FillChar(FChannelStripEntry, SizeOf(FChannelStripEntry), 0);
  FChannelStripEntry.Flag := FILTER_FLAG_PRIORITY_LOWEST or FILTER_FLAG_ALWAYS_ACTIVE or
    FILTER_FLAG_AUDIO_FILTER or FILTER_FLAG_WINDOW_SIZE or FILTER_FLAG_EX_INFORMATION;
  FChannelStripEntry.X := 240 or FILTER_WINDOW_SIZE_CLIENT;
  FChannelStripEntry.Y := 500 or FILTER_WINDOW_SIZE_CLIENT;
  FChannelStripEntry.Name := ChannelStripPluginName;
  FChannelStripEntry.Information:= ChannelStripPluginInfo;
  FChannelStripEntry.TrackN := ChannelStripTrackN;
  FChannelStripEntry.TrackName := @ChannelStripTrackName[0];
  FChannelStripEntry.TrackDefault := @ChannelStripTrackDefault[0];
  FChannelStripEntry.TrackS := @ChannelStripTrackS[0];
  FChannelStripEntry.TrackE := @ChannelStripTrackE[0];

  FillChar(FMasterChannelStripEntry, SizeOf(FMasterChannelStripEntry), 0);
  // FILTER_FLAG_RADIO_BUTTON is not necessary for the operation,
  // but it is necessary to hide the item from ExEdit's menu.
  FMasterChannelStripEntry.Flag := FILTER_FLAG_PRIORITY_LOWEST or FILTER_FLAG_ALWAYS_ACTIVE or
    FILTER_FLAG_AUDIO_FILTER or FILTER_FLAG_WINDOW_SIZE or FILTER_FLAG_NO_CONFIG or
    FILTER_FLAG_RADIO_BUTTON or FILTER_FLAG_EX_INFORMATION;
  FMasterChannelStripEntry.Name := MasterChannelStripPluginName;
  FMasterChannelStripEntry.Information := MasterChannelStripPluginInfo;

  FLastFrame := 0;
  FSaving := False;
  FChanged := False;
  FSelectedID := 0;
  FStrips := TChannelStripMap.Create();
  SetLength(FMixBuf, 0);
  SetLength(FTempBuf, 0);
  SetLength(FRepairBuf, 0);
  FLimiter := TMDADynamics.Create();
  FLimiter.Threshold := 1;
  FLimiter.Ratio := 0.6;
  FLimiter.Attack := 0.9209; // 1 msec
  FLimiter.Release := 0.614; // 100 msec
  FLimiter.Output := 0;
  FLimiter.Limiter := 0.7;
  FLimiter.SampleRate := 48000;
  FLimiter.Channels := 2;
  FLimiter.UpdateParameter();
{
  ODS('    Threshold: %s dB', [FLimiter.ThresholdDisp]);
  ODS('    Ratio:     %s:1', [FLimiter.RatioDisp]);
  ODS('    Attack:    %s us', [FLimiter.AttackDisp]);
  ODS('    Release:   %s ms', [FLimiter.ReleaseDisp]);
  ODS('    Limiter:   %s dB', [FLimiter.LimiterDisp]);
}
end;

destructor TAudioMixer.Destroy;
var
  IT: TChannelStripMap.TIterator;
begin
  IT := FStrips.Iterator();
  if IT <> nil then
  begin
    try
      repeat
        IT.Value.Free;
      until not IT.Next;
    finally
      IT.Free;
    end;
  end;
  FreeAndNil(FStrips);
  FreeAndNil(FLimiter);
  inherited Destroy;
end;

end.

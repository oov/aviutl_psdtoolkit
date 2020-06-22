unit Cache;

{$mode objfpc}{$H+}{$Q-}
{$CODEPAGE UTF-8}

interface

uses
  Classes, SysUtils, GHashMap, fftsg, AviUtl, Util;

type
  TCacheEntry = record
    S: TStream;
    LastUsed: QWORD;
  end;

  TMovieEntry = record
    H: PAVIFileHandle;
    FI: TFileInfo;
    FileName: ShiftJISString;
    CurSampleRate, CurChannels, CurSamples: integer;
    LastUsed: QWORD;
  end;
  PMovieEntry = ^TMovieEntry;

  { TUTF8StringHash }

  TUTF8StringHash = class
    class function hash(k: UTF8String; n: SizeUInt): SizeUInt;
    class function equal(a: UTF8String; b: UTF8String): boolean;
  end;

  TCacheMap = specialize THashmap<UTF8String, TCacheEntry, TUTF8StringHash>;

  { TCacheManager }

  TCacheManager = class(TThread)
  private
    FBuffer: array of Smallint;
    FCacheMap: TCacheMap;
    FRDFT: TRDFT;
    FMovieMap: array[0..31] of TMovieEntry;
    FExFunc: PExFunc;
    FCS: TRTLCriticalSection;
    FNotify: PRTLEvent;
    function FindMovie(const FileName: ShiftJISString): PMovieEntry;
  protected
    procedure Execute(); override;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure SetExFuncPtr(const P: PExFunc);
    procedure PutToMemory(Name: UTF8String; P: Pointer; Len: integer);
    procedure PutToFile(Name: UTF8String; P: Pointer; Len: integer);
    function Get(Name: UTF8String; P: Pointer; Len: integer): boolean;
    function GetSpeakLevel(const FileName: ShiftJISString; const Pos: double;
      const LoCut, HiCut: double): double;
  end;

  { TTempFileStream }

  TTempFileStream = class(THandleStream)
  public
    constructor Create(const prefix: WideString);
    destructor Destroy; override;
  end;

implementation

uses
  Windows;

const
  RDFTSize = 256;
  SampleRate = 24000;

function FNV1a32(S: UTF8String): DWORD;
var
  I: integer;
begin
  Result := $811c9dc5;
  for I := 1 to Length(S) do
  begin
    Result := (Result xor DWORD(Ord(S[I]))) * $01000193;
  end;
end;

{ TTempFileStream }

constructor TTempFileStream.Create(const prefix: WideString);
var
  FileHandle: THandle;
  BasePath, FileName: WideString;
  I: integer;
begin
  SetLength(BasePath, MAX_PATH);
  GetTempPathW(MAX_PATH, @BasePath[1]);
  BasePath := PWideChar(BasePath);
  for I := 0 to 10 do
  begin
    FileName := BasePath + prefix + WideString(IntToStr(Random($ffffff)));
    FileHandle := CreateFileW(PWideChar(FileName), GENERIC_READ or
      GENERIC_WRITE, 0, nil, CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY or
      FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_NOT_CONTENT_INDEXED or
      FILE_FLAG_SEQUENTIAL_SCAN or FILE_FLAG_DELETE_ON_CLOSE, 0);
    if FileHandle = INVALID_HANDLE_VALUE then
      continue;
    inherited Create(FileHandle);
    Exit;
  end;
  raise Exception.Create('cannot create a temporary file');
end;

destructor TTempFileStream.Destroy;
begin
  FileClose(Handle);
  inherited Destroy;
end;

{$Q+}

{ TUTF8StringHash }

class function TUTF8StringHash.hash(k: UTF8String; n: SizeUInt): SizeUInt;
begin
  Result := SizeUInt(FNV1a32(k) mod n);
end;

class function TUTF8StringHash.equal(a: UTF8String; b: UTF8String): boolean;
begin
  Result := a = b;
end;

{ TCacheManager }

function TCacheManager.FindMovie(const FileName: ShiftJISString): PMovieEntry;
var
  I: integer;
  Unused, Oldest: PMovieEntry;
begin
  Result := nil;
  Unused := nil;
  Oldest := nil;
  for I := Low(FMovieMap) to High(FMovieMap) do begin
    if FMovieMap[I].H = nil then begin
      if Unused = nil then
        Unused := @FMovieMap[I];
      continue;
    end;
    if FMovieMap[I].FileName <> FileName then begin
      if (Oldest = nil)or(Oldest^.LastUsed > FMovieMap[I].LastUsed) then
        Oldest := @FMovieMap[I];
      continue;
    end;
    Result := @FMovieMap[I];
    break;
  end;
  if Result = nil then begin
    if Unused <> nil then
      Result := Unused
    else begin
      FExFunc^.AVIFileClose(Oldest^.H);
      Result := Oldest;
    end;
    Result^.H := FExFunc^.AVIFileOpen(PChar(FileName), Result^.FI,
      AVI_FILE_OPEN_FLAG_AUDIO_ONLY);
    if Result^.H = nil then
      raise Exception.Create('failed to open file');
    Result^.FileName := FileName;
    Result^.CurSampleRate := Result^.FI.AudioRate;
    Result^.CurChannels := Result^.FI.AudioCh;
    Result^.CurSamples := Result^.FI.AudioN;
  end;
  if (SampleRate <> Result^.CurSampleRate) or (1 <> Result^.CurChannels) then
  begin
    Result^.CurSamples := FExFunc^.AVIFileSetAudioSampleRate(Result^.H, SampleRate, 1);
    Result^.CurSampleRate := SampleRate;
    Result^.CurChannels := 1;
  end;
end;

procedure TCacheManager.Execute();
const
  CLEANUP_INTERVAL = 1 * 1000;
  CACHEMAP_CLEANUP_INTERVAL = 30 * 1000;
  CACHEMAP_LIFETIME = 60 * 1000;
  MOVIEMAP_LIFETIME = 2 * 1000;
var
  Now, CacheMapLast: QWORD;
  CIT: TCacheMap.TIterator;
  I: integer;
begin
  CacheMapLast := 0;
  while not Terminated do
  begin
    RTLEventWaitFor(FNotify, CLEANUP_INTERVAL);
    RTLEventResetEvent(FNotify);
    Now := GetTickCount64();

    EnterCriticalSection(FCS);
    try
      if Now - CacheMapLast > CACHEMAP_CLEANUP_INTERVAL then
      begin
        CIT := FCacheMap.Iterator();
        if CIT <> nil then
        begin
          try
            repeat
              if Now - CIT.Value.LastUsed > CACHEMAP_LIFETIME then
              begin
                CIT.Value.S.Free();
                FCacheMap.erase(CIT);
              end;
            until not CIT.Next;
          finally
            CIT.Free;
          end;
        end;
        CacheMapLast := Now;
      end;

      for I := Low(FMovieMap) to High(FMovieMap) do begin
        if (FMovieMap[I].H = nil)or(Now - FMovieMap[I].LastUsed < MOVIEMAP_LIFETIME) then
          continue;
        FExFunc^.AVIFileClose(FMovieMap[I].H);
        FMovieMap[I].H := nil;
      end;
    finally
      LeaveCriticalSection(FCS);
    end;
  end;
end;

constructor TCacheManager.Create();
begin
  inherited Create(False);
  FExFunc := nil;
  InitCriticalSection(FCS);
  FNotify := RTLEventCreate();
  FCacheMap := TCacheMap.Create();
  FillChar(FMovieMap[0], Length(FMovieMap) * SizeOf(TMovieEntry), 0);
  SetLength(FBuffer, SampleRate div 10); // 100ms
  FRDFT := TRDFT.Create(RDFTSize);
end;

destructor TCacheManager.Destroy();
var
  CIT: TCacheMap.TIterator;
  I: integer;
begin
  Terminate();
  RTLEventSetEvent(FNotify);
  while not Finished do
    ThreadSwitch();

  CIT := FCacheMap.Iterator();
  if CIT <> nil then
  begin
    try
      repeat
        CIT.Value.S.Free();
      until not CIT.Next;
    finally
      CIT.Free;
    end;
  end;
  FreeAndNil(FCacheMap);

  if FExFunc <> nil then begin
    for I := Low(FMovieMap) to High(FMovieMap) do begin
      if FMovieMap[I].H = nil then
        continue;
      FExFunc^.AVIFileClose(FMovieMap[I].H);
    end;
  end;
  FreeAndNil(FRDFT);

  RTLEventDestroy(FNotify);
  DoneCriticalSection(FCS);
  inherited Destroy;
end;

procedure TCacheManager.PutToMemory(Name: UTF8String; P: Pointer; Len: integer);
var
  V: TCacheEntry;
  Found: boolean;
begin
  EnterCriticalSection(FCS);
  try
    V.S := nil;
    Found := FCacheMap.GetValue(Name, V);
    if not Found or (V.S.ClassType <> TMemoryStream) then
    begin
      FreeAndNil(V.S);
      V.S := TMemoryStream.Create;
    end;
    V.S.Position := 0;
    V.S.WriteBuffer(P^, Len);
    V.S.Size := Len;
    V.LastUsed := GetTickCount64();
    FCacheMap.Items[Name] := V;
  finally
    LeaveCriticalSection(FCS);
  end;
end;

procedure TCacheManager.PutToFile(Name: UTF8String; P: Pointer; Len: integer);
var
  V: TCacheEntry;
  Found: boolean;
begin
  EnterCriticalSection(FCS);
  try
    V.S := nil;
    Found := FCacheMap.GetValue(Name, V);
    if not Found or (V.S.ClassType <> TTempFileStream) then
    begin
      FreeAndNil(V.S);
      V.S := TTempFileStream.Create('psdtkit');
    end;
    V.S.Position := 0;
    V.S.WriteBuffer(P^, Len);
    V.S.Size := Len;
    V.LastUsed := GetTickCount64();
    FCacheMap.Items[Name] := V;
  finally
    LeaveCriticalSection(FCS);
  end;
end;

function TCacheManager.Get(Name: UTF8String; P: Pointer; Len: integer): boolean;
var
  V: TCacheEntry;
begin
  EnterCriticalSection(FCS);
  try
    Result := FCacheMap.GetValue(Name, V);
    if Result then
    begin
      V.S.Position := 0;
      V.S.ReadBuffer(P^, Len);
      V.LastUsed := GetTickCount64();
      FCacheMap.Items[Name] := V;
    end;
  finally
    LeaveCriticalSection(FCS);
  end;
end;

function TCacheManager.GetSpeakLevel(const FileName: ShiftJISString;
  const Pos: double; const LoCut, HiCut: double): double;
var
  V: PMovieEntry;
  A: PSmallint;
  R: PDouble;
  HzDivider, Hz, N: double;
  Read, I: integer;
begin
  if FExFunc = nil then
    raise Exception.Create('not initialized yet');

  Result := 0;
  EnterCriticalSection(FCS);
  try
    A := @FBuffer[0];
    V := FindMovie(FileName);
    // It seems avi_file_read_audio_sample writes more samples than requested.
    // Therefore we have to reserve large buffer enough for that.
    Read := FExFunc^.AVIFileReadAudioSample(V^.H, Trunc(Pos * double(V^.CurSampleRate)),
      RDFTSize, A);
    V^.LastUsed := GetTickCount64();
    if Read = 0 then
      Exit;

    for I := Read to RDFTSize - 1 do
      (A + I)^ := 0;

    R := FRDFT.Execute(A, 1);
    HzDivider := double(V^.CurSampleRate) / RDFTSize;
    N := 0;
    for I := 0 to RDFTSize div 2 - 1 do
    begin
      Hz := double(I) * HzDivider;
      if Hz < LoCut then
        continue;
      if HiCut < Hz then
        break;
      Result := Result + (R + I)^;
      N := N + 1;
    end;
    if N <> 0 then
      Result := Result / N;
  finally
    LeaveCriticalSection(FCS);
  end;
end;

procedure TCacheManager.SetExFuncPtr(const P: PExFunc);
var
  I: integer;
begin
  if FExFunc = P then Exit;
  if FExFunc <> nil then
  begin
    for I := Low(FMovieMap) to High(FMovieMap) do begin
      if FMovieMap[I].H = nil then
        continue;
      FExFunc^.AVIFileClose(FMovieMap[I].H);
      FMovieMap[I].H := nil;
    end;
  end;
  FExFunc := P;
end;

end.

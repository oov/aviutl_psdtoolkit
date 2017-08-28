unit Cache;

{$mode objfpc}{$H+}{$Q-}
{$CODEPAGE UTF-8}

interface

uses
  Classes, SysUtils, GHashMap;

type
  TCacheEntry = record
    S: TStream;
    LastUsed: QWORD;
  end;

  { TUTF8StringHash }

  TUTF8StringHash = class
    class function hash(k: UTF8String; n: SizeUInt): SizeUInt;
    class function equal(a: UTF8String; b: UTF8String): boolean;
  end;

  TCacheMap = specialize THashmap<UTF8String, TCacheEntry, TUTF8StringHash>;

  { TCacheManager }

  TCacheManager = class(TThread)
  private
    FCacheMap: TCacheMap;
    FCS: TRTLCriticalSection;
    FNotify: PRTLEvent;
  protected
    procedure Execute(); override;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure PutToMemory(Name: UTF8String; P: Pointer; Len: integer);
    procedure PutToFile(Name: UTF8String; P: Pointer; Len: integer);
    function Get(Name: UTF8String; P: Pointer; Len: integer): boolean;
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

procedure TCacheManager.Execute;
const
  CLEANUP_INTERVAL = 10 * 1000;
  LIFETIME = 60 * 1000;
var
  LastRun, Now: QWORD;
  IT: TCacheMap.TIterator;
begin
  LastRun := GetTickCount64();
  while not Terminated do
  begin
    RTLEventWaitFor(FNotify);
    RTLEventResetEvent(FNotify);
    Now := GetTickCount64();
    if Now - LastRun < CLEANUP_INTERVAL then
      continue;

    IT := FCacheMap.Iterator();
    if IT <> nil then
    begin
      try
        repeat
          if Now - IT.Value.LastUsed > LIFETIME then
          begin
            it.Value.S.Free();
            FCacheMap.erase(IT);
          end;
        until not IT.Next;
      finally
        IT.Free;
      end;
    end;
    LastRun := Now;
  end;
end;

constructor TCacheManager.Create;
begin
  inherited Create(False);
  InitCriticalSection(FCS);
  FNotify := RTLEventCreate();
  FCacheMap := TCacheMap.Create();
end;

destructor TCacheManager.Destroy;
var
  IT: TCacheMap.TIterator;
begin
  Terminate();
  RTLEventSetEvent(FNotify);
  while not Finished do
    ThreadSwitch();

  IT := FCacheMap.Iterator();
  if IT <> nil then
  begin
    try
      repeat
        IT.Value.S.Free();
      until not IT.Next;
    finally
      IT.Free;
    end;
  end;
  FreeAndNil(FCacheMap);
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
  RTLEventSetEvent(FNotify);
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
      V.S := TTempFileStream.Create('psdtipc');
    end;
    V.S.Position := 0;
    V.S.WriteBuffer(P^, Len);
    V.S.Size := Len;
    V.LastUsed := GetTickCount64();
    FCacheMap.Items[Name] := V;
  finally
    LeaveCriticalSection(FCS);
  end;
  RTLEventSetEvent(FNotify);
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
  RTLEventSetEvent(FNotify);
end;

end.

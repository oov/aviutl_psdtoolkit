unit Util;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Classes;

type
  ShiftJISString = type ansistring(932);
  TFileNameFormatType = (ftMsg, ftDTMsg, ftCastMsg, ftDTCastMsg);
  { TWideFileStream }

  TWideFileStream = class(THandleStream)
  public
    constructor Create(const FilePath: WideString; Mode: word);
    destructor Destroy(); override;
  end;

function SuggestNextFileName(const FormatType: TFileNameFormatType;
  const DateTime: TDateTime; const TalkMessage, Cast: WideString): WideString;

implementation

uses
  Windows, SysUtils;

function WideStringToShiftJISString(const S: WideString): ShiftJISString;
var
  Len: integer;
begin
  Len := WideCharToMultiByte(932, WC_NO_BEST_FIT_CHARS, @S[1], Length(S),
    nil, 0, 'x', nil);
  if Len = 0 then
    RaiseLastOSError();
  SetLength(Result, Len);
  if WideCharToMultiByte(932, WC_NO_BEST_FIT_CHARS, @S[1], Length(S),
    @Result[1], Len, 'x', nil) = 0 then
    RaiseLastOSError();
end;

function FilterForFileName(const S: WideString; const Limit: integer): WideString;
var
  I, N, SLen: integer;
  WS: WideString;
begin
  if Length(S) = 0 then
    Exit;
  N := 0;
  WS := WideString(WideStringToShiftJISString(S));
  SetLength(Result, Length(WS));
  SLen := Length(WS);
  if SLen > Limit then
    SLen := Limit - 2;
  for I := 1 to SLen do
  begin
    case WS[I] of
      #$09, #$0a, #$0c, #$20:
      begin
        Inc(N);
        Result[N] := '_';
      end;
      #$0d:
      begin
        if (I < SLen) and (WS[I + 1] = #$0a) then
          continue;
        Inc(N);
        Result[N] := '_';
      end;
      #$00, #$01, #$02, #$03, #$04, #$05, #$06, #$07,
      #$08, #$0b, #$0e, #$0f,
      #$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17,
      #$18, #$19, #$1a, #$1b, #$1c, #$1d, #$1e, #$1f,
      #$22, #$2a, #$2f, #$3a, #$3c, #$3e, #$3f, #$7c, #$7f:
      begin
        Inc(N);
        Result[N] := 'x';
      end;
      else
      begin
        Inc(N);
        Result[N] := WS[I];
      end;
    end;
  end;
  SetLength(Result, N);
  if SLen <> Length(WS) then
    Result := Result + '+' + WideString(IntToStr(Length(S) - SLen));

end;

function TestCreateBrandNewFile(const FileName: WideString): boolean;
var
  h: THandle;
begin
  Result := False;
  h := CreateFileW(PWideChar(FileName), GENERIC_READ or GENERIC_WRITE,
    0, nil, CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY or FILE_ATTRIBUTE_HIDDEN or
    FILE_ATTRIBUTE_NOT_CONTENT_INDEXED or FILE_FLAG_SEQUENTIAL_SCAN or
    FILE_FLAG_DELETE_ON_CLOSE, 0);
  if h = INVALID_HANDLE_VALUE then
    Exit;
  CloseHandle(h);
  Result := True;
  Exit;
end;

function FormatDateTimeForFileName(const DT: TDateTime): WideString;
begin
  Result := WideString(FormatDateTime('yymmdd"_"hhnnss"_"', DT));
end;

function BuildBaseFileName(const FormatType: TFileNameFormatType;
  const DateTime: TDateTime; const TalkMessage, Cast: WideString): WideString;
begin
  case FormatType of
    ftMsg:
      Result := FilterForFileName(TalkMessage, 13);
    ftDTMsg:
      Result := FormatDateTimeForFileName(DateTime) + FilterForFileName(TalkMessage, 13);
    ftCastMsg:
      Result := FilterForFileName(Cast, 16) + '_' + FilterForFileName(TalkMessage, 13);
    ftDTCastMsg:
      Result := FormatDateTimeForFileName(DateTime) + FilterForFileName(Cast, 16) +
        '_' + FilterForFileName(TalkMessage, 13);
    else
      raise Exception.Create('unexpected format type');
  end;
end;

function SuggestNextFileName(const FormatType: TFileNameFormatType;
  const DateTime: TDateTime; const TalkMessage, Cast: WideString): WideString;
var
  BasePath, FileName: WideString;
  I: integer;
begin
  SetLength(BasePath, MAX_PATH);
  GetTempPathW(MAX_PATH, @BasePath[1]);
  BasePath := IncludeTrailingPathDelimiter(PWideChar(BasePath)) +
    BuildBaseFileName(FormatType, DateTime, TalkMessage, Cast);
  for I := 0 to 1000 do
  begin
    if I = 0 then
      FileName := BasePath
    else
      FileName := BasePath + WideString('[' + IntToStr(I + 1) + ']');
    if not TestCreateBrandNewFile(FileName + '.txt') then
      continue;
    if not TestCreateBrandNewFile(FileName + '.wav') then
      continue;
    Result := FileName;
    Exit;
  end;
  raise Exception.Create('cannot create a file');
end;

{ TWideFileStream }

constructor TWideFileStream.Create(const FilePath: WideString; Mode: word);
var
  H: THandle;
begin
  if (Mode and fmCreate) = fmCreate then
    H := CreateFileW(PWideChar(FilePath), GENERIC_WRITE, FILE_SHARE_READ,
      nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0)
  else
    H := CreateFileW(PWideChar(FilePath), GENERIC_READ, FILE_SHARE_READ,
      nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if H = INVALID_HANDLE_VALUE then
    raise Exception.Create('cannot open file');
  inherited Create(H);
end;

destructor TWideFileStream.Destroy;
begin
  CloseHandle(Handle);
  inherited Destroy;
end;

end.

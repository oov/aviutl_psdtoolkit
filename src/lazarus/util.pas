unit Util;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Classes, SysUtils;

type
  ShiftJISString = type ansistring(932);

  { TFileStreamW }

  TFileStreamW = class(THandleStream)
  public
    constructor Create(const FileName: WideString);
    destructor Destroy; override;
  end;

procedure ODS(const Fmt: string; const Args: array of const);
function SaveDialog(const Window: THandle; const Title: WideString;
  const Filter: WideString; const nFilterIndex: cardinal;
  DefFileName: WideString = ''; DefExt: WideString = '';
  InitialDir: WideString = ''): WideString;
function CopyToClipboard(const hwnd: THandle; const S: WideString): boolean;
function DecodePercentEncoding(const S: RawByteString): RawByteString;
function StrCount(const S, Delimiter: PChar): integer;

function Token(const Delimiter: UTF8String; var s: UTF8String): UTF8String;
function GetDLLName(): WideString;
function Sanitize(S: UTF8String): UTF8String;
function StringifyForLua(s: UTF8String): UTF8String;
function StringifyForCSV(S: UTF8String): UTF8String;

procedure WriteUInt64(const S: TStream; const V: QWord);
procedure WriteUInt32(const S: TStream; const V: DWORD);
procedure WriteInt32(const S: TStream; const V: integer);
procedure WriteSingle(const S: TStream; const V: single);
procedure WriteString(const S: TStream; const V: UTF8String);
procedure WriteRawString(const S: TStream; const V: RawByteString);
procedure WriteIdAndFileName(const S: TStream; const ID: integer;
  const FileName: UTF8String);

implementation

uses
  Windows;

type
  TOpenFilenameW = record
    lStructSize: DWORD;
    hWndOwner: HWND;
    hInstance: HINST;
    lpstrFilter: PWideChar;
    lpstrCustomFilter: PWideChar;
    nMaxCustFilter: DWORD;
    nFilterIndex: DWORD;
    lpstrFile: PWideChar;
    nMaxFile: DWORD;
    lpstrFileTitle: PWideChar;
    nMaxFileTitle: DWORD;
    lpstrInitialDir: PWideChar;
    lpstrTitle: PWideChar;
    Flags: DWORD;
    nFileOffset: word;
    nFileExtension: word;
    lpstrDefExt: PWideChar;
    lCustData: LPARAM;
    lpfnHook: function(Wnd: HWND; Msg: UINT; wParam: WPARAM;
        lParam: LPARAM): UINT_PTR stdcall;
    lpTemplateName: PWideChar;
    pvReserved: Pointer;
    dwReserved: DWORD;
    FlagsEx: DWORD;
  end;

function GetSaveFileNameW(var OpenFile: TOpenFilenameW): Bool;
  stdcall; external 'comdlg32.dll' Name 'GetSaveFileNameW';

var
  debugging: boolean;

procedure ODS(const Fmt: string; const Args: array of const);
begin
  if not debugging then
    Exit;
  OutputDebugStringW(PWideChar(WideString(Format('psdtoolkit cli: ' + Fmt, Args))));
end;

function SaveDialog(const Window: THandle; const Title: WideString;
  const Filter: WideString; const nFilterIndex: cardinal;
  DefFileName: WideString = ''; DefExt: WideString = '';
  InitialDir: WideString = ''): WideString;
var
  OFN: TOpenFilenameW;
begin
  FillChar(OFN, SizeOf(TOpenFilenameW), 0);
  OFN.lStructSize := SizeOf(TOpenFileNameW);
  OFN.hInstance := hInstance;
  OFN.hWndOwner := Window;
  OFN.nFilterIndex := nFilterIndex;
  OFN.lpstrTitle := PWideChar(Title);
  OFN.lpstrDefExt := PWideChar(DefExt);
  if InitialDir <> '' then
    OFN.lpStrInitialDir := PWideChar(InitialDir);
  OFN.lpStrFilter := PWideChar(Filter);
  SetLength(Result, MAX_PATH + Length(DefFileName));
  FillChar(Result[1], SizeOf(WideChar)*Length(Result), 0);
  Move(DefFileName[1], Result[1], SizeOf(widechar) * Length(DefFileName));
  OFN.lpstrFile := @Result[1];
  OFN.nMaxFile := Length(Result);
  OFN.Flags := OFN_EXPLORER or OFN_PATHMUSTEXIST or OFN_FILEMUSTEXIST or
    OFN_HIDEREADONLY or OFN_ENABLESIZING or OFN_OVERWRITEPROMPT;
  if GetSaveFileNameW(OFN) then
    Result := PWideChar(OFN.lpStrFile)
  else
    Result := '';
end;

function CopyToClipboard(const hwnd: THandle; const S: WideString): boolean;
var
  h: HGLOBAL;
  p: PWideChar;
begin
  Result := False;
  if not OpenClipboard(hwnd) then
    Exit;
  h := GlobalAlloc(GHND, Length(S)*2 + 2);
  try
    p := GlobalLock(h);
    try
      if p = nil then
        Exit;
      Move(S[1], p^, Length(S)*2);
      p[Length(S)] := #0;
    finally
      GlobalUnlock(h);
    end;
    EmptyClipboard();
    SetClipboardData(CF_UNICODETEXT, h);
  except
    GlobalFree(h);
    raise;
  end;
  CloseClipboard();
  Result := True;
end;

function DecodePercentEncoding(const S: RawByteString): RawByteString;
const
  Table: array[byte] of byte = (
    $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
    $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
    $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
    $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $0, $0, $0, $0, $0, $0,
    $0, $a, $b, $c, $d, $e, $f, $0, $0, $0, $0, $0, $0, $0, $0, $0,
    $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
    $0, $a, $b, $c, $d, $e, $f, $0, $0, $0, $0, $0, $0, $0, $0, $0,
    $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
    $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
    $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
    $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
    $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
    $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
    $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
    $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
    $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0);
var
  Src, EndPos, Dest: PByte;
begin
  SetLength(Result, Length(S));
  Dest := @Result[1];
  Src := @S[1];
  EndPos := Src + Length(S);
  while (Src < EndPos) do
  begin
    if (Src^ = $25) and (Src + 2 < EndPos) then
    begin
      Dest^ := (Table[(Src + 1)^] shl 4) or Table[(Src + 2)^];
      Inc(Src, 3);
    end
    else
    begin
      Dest^ := Src^;
      Inc(Src);
    end;
    Inc(Dest);
  end;
  SetLength(Result, Dest - @Result[1]);
end;

function StrCount(const S, Delimiter: PChar): integer;
var
  SS: PChar;
begin
  SS := S;
  Result := 0;
  while True do
  begin
    SS := StrPos(SS, Delimiter);
    if SS = nil then
      break;
    Inc(Result);
    Inc(SS);
  end;
end;

function Token(const Delimiter: UTF8String; var S: UTF8String): UTF8String;
var
  P: integer;
begin
  P := Pos(Delimiter, S);
  if P = 0 then
  begin
    Result := S;
    S := '';
  end
  else
  begin
    Result := Copy(S, 1, P - 1);
    Delete(S, 1, P + Length(Delimiter) - 1);
  end;
end;

function GetDLLName(): WideString;
begin
  SetLength(Result, MAX_PATH);
  GetModuleFileNameW(hInstance, @Result[1], MAX_PATH);
  Result := PWideChar(Result);
end;

function Sanitize(S: UTF8String): UTF8String;
var
  si, di: integer;
  c: char;
begin
  SetLength(Result, Length(s));
  di := 1;
  for si := 1 to Length(s) do
  begin
    case s[si] of
      #$00, #$01, #$02, #$03, #$04, #$05, #$06, #$07,
      #$08, #$09, #$0a, #$0b, #$0c, #$0d, #$0e, #$0f,
      #$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17,
      #$18, #$19, #$1a, #$1b, #$1c, #$1d, #$1e, #$1f,
      #$22, #$2a, #$2f, #$3a, #$3c, #$3e, #$3f, #$7c, #$7f: c := '_';
      else
        c := s[si];
    end;
    Result[di] := c;
    Inc(di, 1);
  end;
  SetLength(Result, di - 1);
end;

function StringifyForLua(s: UTF8String): UTF8String;
var
  si, di: integer;
  c: char;
begin
  SetLength(Result, Length(s) * 2 + 2);
  Result[1] := '"';
  di := 2;
  for si := 1 to Length(s) do
  begin
    case s[si] of
      #$07: c := 'a';
      #$08: c := 'b';
      #$09: c := 't';
      #$0a: c := 'n';
      #$0b: c := 'v';
      #$0c: c := 'f';
      #$0d: c := 'r';
      #$22: c := #$22;
      #$27: c := #$27;
      #$5b: c := #$5b;
      #$5c: c := #$5c;
      #$5d: c := #$5d;
      else
      begin
        Result[di] := s[si];
        Inc(di);
        continue;
      end;
    end;
    Result[di + 0] := '\';
    Result[di + 1] := c;
    Inc(di, 2);
  end;
  Result[di] := '"';
  SetLength(Result, di);
end;

function StringifyForCSV(S: UTF8String): UTF8String;
begin
  if Pos(',', S) = 0 then begin
    Result := S;
    Exit;
  end;
  Result := '"' + StringReplace(S, '"', '""', [rfReplaceAll]) + '"';
end;

procedure WriteUInt64(const S: TStream; const V: QWord);
begin
  S.WriteQWord(V);
end;

procedure WriteUInt32(const S: TStream; const V: DWORD);
begin
  S.WriteDWord(V);
end;

procedure WriteInt32(const S: TStream; const V: integer);
begin
  S.WriteDWord(V);
end;

procedure WriteSingle(const S: TStream; const V: single);
begin
  S.WriteDWord(DWORD(V));
end;

procedure WriteString(const S: TStream; const V: UTF8String);
begin
  WriteInt32(S, Length(V));
  S.WriteBuffer(V[1], Length(V));
end;

procedure WriteRawString(const S: TStream; const V: RawByteString);
begin
  S.WriteBuffer(V[1], Length(V));
end;

procedure WriteIdAndFileName(const S: TStream; const ID: integer;
  const FileName: UTF8String);
begin
  WriteInt32(S, ID);
  WriteString(S, FileName);
  ods('  ID: %d / Filename: %s', [ID, FileName]);
end;

{ TFileStreamW }

constructor TFileStreamW.Create(const FileName: WideString);
var
  h: THandle;
begin
  h := CreateFileW(PWideChar(FileName), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, 0, 0);
  if h = INVALID_HANDLE_VALUE then
    raise Exception.Create('cannot create file: '+FileName);
  inherited Create(h);
end;

destructor TFileStreamW.Destroy;
begin
  CloseHandle(Handle);
  inherited Destroy;
end;

initialization
  debugging := SysUtils.GetEnvironmentVariable('PSDTOOLKITDEBUG') <> '';
end.

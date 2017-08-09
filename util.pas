unit util;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Classes, SysUtils, GHashMap;

type
  ShiftJISString = type ansistring(932);

  { TImageState }

  TImageState = record
    State: WideString;
  end;

  { TWideStringHash }

  TWideStringHash = class
    class function hash(k: WideString; n: SizeUInt): SizeUInt;
    class function equal(a: WideString; b: WideString): boolean;
  end;

  { TStateMap }

  TStateMap = specialize THashmap<WideString, TImageState, TWideStringHash>;

procedure ODS(const Fmt: string; const Args: array of const);
function fnv1a32(ws: WideString): cardinal;
function StringifyForLua(s: UTF8String): UTF8String;

procedure WriteUInt64(const S: TStream; const V: QWord);
procedure WriteInt32(const S: TStream; const V: integer);
procedure WriteSingle(const S: TStream; const V: single);
procedure WriteString(const S: TStream; const V: UTF8String);
procedure WriteIdAndFileName(const S: TStream; const ID: integer;
  const FileName: UTF8String);

procedure ReadResult(const S: TStream);
function ReadUInt64(const S: TStream): QWord;
function ReadInt32(const S: TStream): integer;
function ReadSingle(const S: TStream): single;
function ReadString(const S: TStream): UTF8String;
function ReadBinary(const S: TStream; const P: Pointer; const Len: integer): integer;

implementation

uses
  Windows;

var
  debugging: boolean;

procedure ODS(const Fmt: string; const Args: array of const);
begin
  if not debugging then
    Exit;
  OutputDebugStringW(PWideChar(WideString(Format('psdtool-ipc cli: ' + Fmt, Args))));
end;

function fnv1a32(ws: WideString): cardinal;
const
  fnv1a32base = $811c9dc5;
  fnv1a32prime = $01000193;
var
  i: integer;
begin
  Result := fnv1a32base;
  for i := 1 to Length(ws) do
  begin
    Result := (Result xor cardinal(ws[i])) * fnv1a32prime;
  end;
end;

function StringifyForLua(s: UTF8String): UTF8String;
var
  si, di: integer;
  c: char;
begin
  SetLength(Result, Length(s) * 2);
  di := 1;
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
  SetLength(Result, di - 1);
end;

procedure WriteUInt64(const S: TStream; const V: QWord);
begin
  S.WriteQWord(V);
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

procedure WriteIdAndFileName(const S: TStream; const ID: integer;
  const FileName: UTF8String);
begin
  WriteInt32(S, ID);
  WriteString(S, FileName);
  ods('  ID: %d / Filename: %s', [ID, FileName]);
end;

procedure ReadResult(const S: TStream);
var
  l: integer;
  msg: UTF8String;
begin
  l := ReadInt32(S);
  if l = 0 then
  begin
    ods('  -> SUCCESS', []);
    Exit;
  end;
  SetLength(msg, l);
  S.ReadBuffer(msg[1], l);
  ods('  -> ERROR: %s', [msg]);
  raise Exception.Create(msg);
end;

function ReadUInt64(const S: TStream): QWord;
begin
  Result := S.ReadQWord();
end;

function ReadInt32(const S: TStream): integer;
begin
  Result := S.ReadDWord();
end;

function ReadSingle(const S: TStream): single;
var
  d: DWORD;
begin
  d := S.ReadDWord();
  Result := PSingle(@d)^;
end;

function ReadString(const S: TStream): UTF8String;
var
  l: integer;
begin
  l := ReadInt32(S);
  if l = 0 then
  begin
    Result := '';
    Exit;
  end;
  SetLength(Result, l);
  S.ReadBuffer(Result[1], l);
end;

function ReadBinary(const S: TStream; const P: Pointer; const Len: integer): integer;
var
  l: integer;
begin
  l := ReadInt32(S);
  if l > Len then
    raise Exception.Create('could not read binary: given buffer too short');
  S.ReadBuffer(P^, l);
  Result := l;
end;

{ TWideStringHash }

class function TWideStringHash.hash(k: WideString; n: SizeUInt): SizeUInt;
begin
  Result := SizeUInt(fnv1a32(k)) mod n;
end;

class function TWideStringHash.equal(a: WideString; b: WideString): boolean;
begin
  Result := a = b;
end;

initialization
  debugging := SysUtils.GetEnvironmentVariable('PSDTOOLIPCDEBUG') <> '';
end.

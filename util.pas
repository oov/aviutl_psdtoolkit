unit Util;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Classes, SysUtils;

type
  ShiftJISString = type ansistring(932);

  { TImageState }

  TImageState = record
    State: WideString;
  end;


procedure ODS(const Fmt: string; const Args: array of const);
function GetDLLName(): WideString;
function StringifyForLua(s: UTF8String): UTF8String;

procedure WriteUInt64(const S: TStream; const V: QWord);
procedure WriteInt32(const S: TStream; const V: integer);
procedure WriteSingle(const S: TStream; const V: single);
procedure WriteString(const S: TStream; const V: UTF8String);
procedure WriteIdAndFileName(const S: TStream; const ID: integer;
  const FileName: UTF8String);

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

function GetDLLName(): WideString;
begin
  SetLength(Result, MAX_PATH);
  GetModuleFileNameW(hInstance, @Result[1], MAX_PATH);
  Result := PWideChar(Result);
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

initialization
  debugging := SysUtils.GetEnvironmentVariable('PSDTOOLIPCDEBUG') <> '';
end.

unit AssistMain;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  AviUtl;

function GetFilterTableList(): PPFilterDLL; stdcall;

implementation

uses
  PSDToolKitAssist;

const
  BoolConv: array[boolean] of AviUtlBool = (AVIUTL_FALSE, AVIUTL_TRUE);

var
  Assist: TPSDToolKitAssist;
  FilterDLLList: array of PFilterDLL;

function AssistFuncInit(fp: PFilter): AviUtlBool; cdecl;
begin
  Result := BoolConv[Assist.InitProc(fp)];
end;

function AssistFuncExit(fp: PFilter): AviUtlBool; cdecl;
begin
  Result := BoolConv[Assist.ExitProc(fp)];
end;

function GetFilterTableList(): PPFilterDLL; stdcall;
begin
  Result := @FilterDLLList[0];
end;

initialization
  Assist := TPSDToolKitAssist.Create();
  Assist.Entry^.FuncInit := @AssistFuncInit;
  Assist.Entry^.FuncExit := @AssistFuncExit;

  SetLength(FilterDLLList, 2);
  FilterDLLList[0] := Assist.Entry;
  FilterDLLList[1] := nil;

finalization
  Assist.Free();

end.


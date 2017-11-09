unit AssistMain;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  AviUtl;

function GetFilterTableList(): PPFilterDLL; stdcall;

implementation

uses
  Windows, PSDToolKitAssist;

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

function AssistFuncWndProc(Window: HWND; Message: UINT; WP: WPARAM;
  LP: LPARAM; Edit: Pointer; Filter: PFilter): LRESULT; cdecl;
begin
  Result := Assist.WndProc(Window, Message, WP, LP, Edit, Filter);
end;

function AssistFuncProjectLoad(Filter: PFilter; Edit: Pointer;
  Data: Pointer; Size: integer): AviUtlBool; cdecl;
begin
  Result := BoolConv[Assist.ProjectLoadProc(Filter, Edit, Data, Size)];
end;

function AssistFuncProjectSave(Filter: PFilter; Edit: Pointer;
  Data: Pointer; var Size: integer): AviUtlBool; cdecl;
begin
  Result := BoolConv[Assist.ProjectSaveProc(Filter, Edit, Data, Size)];
end;

function GetFilterTableList(): PPFilterDLL; stdcall;
begin
  Result := @FilterDLLList[0];
end;

initialization
  Assist := TPSDToolKitAssist.Create();
  Assist.Entry^.FuncInit := @AssistFuncInit;
  Assist.Entry^.FuncExit := @AssistFuncExit;
  Assist.Entry^.FuncWndProc := @AssistFuncWndProc;
  Assist.Entry^.FuncProjectLoad := @AssistFuncProjectLoad;
  Assist.Entry^.FuncProjectSave := @AssistFuncProjectSave;

  SetLength(FilterDLLList, 2);
  FilterDLLList[0] := Assist.Entry;
  FilterDLLList[1] := nil;

finalization
  Assist.Free();

end.


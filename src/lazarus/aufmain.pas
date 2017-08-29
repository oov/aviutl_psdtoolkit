unit AUFMain;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  AviUtl;

function GetFilterTableList(): PPFilterDLL; stdcall;

implementation

uses
  Windows, AudioEffector, PSDToolKitAssist;

const
  BoolConv: array[boolean] of AviUtlBool = (AVIUTL_FALSE, AVIUTL_TRUE);

var
  Assist: TPSDToolKitAssist;
  AudioEff: TAudioEffector;
  FilterDLLList: array of PFilterDLL;

function AssistFuncInit(fp: PFilter): AviUtlBool; cdecl;
begin
  Result := BoolConv[Assist.InitProc(fp)];
end;

function AssistFuncExit(fp: PFilter): AviUtlBool; cdecl;
begin
  Result := BoolConv[Assist.ExitProc(fp)];
end;

function EffectFuncProc(fp: PFilter; fpip: PFilterProcInfo): AviUtlBool; cdecl;
begin
  Result := BoolConv[AudioEff.EffectProc(fp, fpip)];
end;

function EffectFuncWndProc(Window: HWND; Message: UINT; WP: WPARAM;
  LP: LPARAM; Edit: Pointer; Filter: PFilter): LRESULT; cdecl;
begin
  Result := AudioEff.EffectWndProc(Window, Message, WP, LP, Edit, Filter);
end;

function MasterFuncProc(fp: PFilter; fpip: PFilterProcInfo): AviUtlBool; cdecl;
begin
  Result := BoolConv[AudioEff.MasterProc(fp, fpip)];
end;

function GetFilterTableList(): PPFilterDLL; stdcall;
begin
  Result := @FilterDLLList[0];
end;

initialization
  Assist := TPSDToolKitAssist.Create();
  Assist.Entry^.FuncInit := @AssistFuncInit;
  Assist.Entry^.FuncExit := @AssistFuncExit;

  AudioEff := TAudioEffector.Create();
  AudioEff.EffectEntry^.FuncProc := @EffectFuncProc;
  AudioEff.EffectEntry^.FuncWndProc := @EffectFuncWndProc;
  AudioEff.MasterEntry^.FuncProc := @MasterFuncProc;

  SetLength(FilterDLLList, 4);
  FilterDLLList[0] := Assist.Entry;
  FilterDLLList[1] := AudioEff.EffectEntry;
  FilterDLLList[2] := AudioEff.MasterEntry;
  FilterDLLList[3] := nil;

finalization
  AudioEff.Free();
  Assist.Free();

end.


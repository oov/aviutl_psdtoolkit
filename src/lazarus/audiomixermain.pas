unit AudioMixerMain;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  AviUtl;

function GetFilterTableList(): PPFilterDLL; stdcall;

implementation

uses
  Windows, AudioMixer;

const
  BoolConv: array[boolean] of AviUtlBool = (AVIUTL_FALSE, AVIUTL_TRUE);

var
  Mixer: TAudioMixer;
  FilterDLLList: array of PFilterDLL;

function ChannelStripFuncProc(fp: PFilter; fpip: PFilterProcInfo): AviUtlBool; cdecl;
begin
  Result := BoolConv[Mixer.ChannelStripProc(fp, fpip)];
end;

function ChannelStripFuncWndProc(Window: HWND; Message: UINT; WP: WPARAM;
  LP: LPARAM; Edit: Pointer; Filter: PFilter): LRESULT; cdecl;
begin
  Result := Mixer.ChannelStripWndProc(Window, Message, WP, LP, Edit, Filter);
end;

function MasterChannelStripFuncProc(fp: PFilter; fpip: PFilterProcInfo): AviUtlBool; cdecl;
begin
  Result := BoolConv[Mixer.MasterChannelStripProc(fp, fpip)];
end;

function GetFilterTableList(): PPFilterDLL; stdcall;
begin
  Result := @FilterDLLList[0];
end;

initialization
  Mixer := TAudioMixer.Create();
  Mixer.ChannelStripEntry^.FuncProc := @ChannelStripFuncProc;
  Mixer.ChannelStripEntry^.FuncWndProc := @ChannelStripFuncWndProc;
  Mixer.MasterChannelStripEntry^.FuncProc := @MasterChannelStripFuncProc;

  SetLength(FilterDLLList, 3);
  FilterDLLList[0] := Mixer.ChannelStripEntry;
  FilterDLLList[1] := Mixer.MasterChannelStripEntry;
  FilterDLLList[2] := nil;

finalization
  Mixer.Free();

end.


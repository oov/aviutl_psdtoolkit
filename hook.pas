unit hook;

{$mode objfpc}{$H+}

interface

type
  THookEvent = procedure(Sender: TObject) of object;

  { THooker }

  THooker = class
  private
    FHook: THandle;
    FOnPressShiftCtrlAltA: THookEvent;
    function GetHooked: boolean;
  public
    constructor Create();
    destructor Destroy(); override;
    property Hooked: boolean read GetHooked;
    property OnPressShiftCtrlAltA: THookEvent
      read FOnPressShiftCtrlAltA write FOnPressShiftCtrlAltA;
  end;

var
  Hooker: THooker;

implementation

uses
  Windows, find, util;

function KeyboardHookProc(code: longint; wp: WPARAM; lp: LPARAM): LRESULT; stdcall;
var
  processed: boolean;
begin
  if code < 0 then
    Exit;

  processed := False;
  try
    if code <> HC_ACTION then
      Exit;
    if wp <> VK_A then
      Exit;
    if ((lp and $80000000) <> 0) and ((GetAsyncKeyState(VK_SHIFT) and $8000) <> 0) and
      ((GetAsyncKeyState(VK_CONTROL) and $8000) <> 0) and
      ((GetAsyncKeyState(VK_MENU) and $8000) <> 0) and
      (Hooker.OnPressShiftCtrlAltA <> nil) then
      Hooker.OnPressShiftCtrlAltA(Hooker);
  finally
    if not processed then
      Result := CallNextHookEx(Hooker.FHook, code, wp, lp);
  end;
end;

{ THooker }

function THooker.GetHooked: boolean;
begin
  Result := FHook <> 0;
end;

constructor THooker.Create;
begin
  inherited Create();
  FHook := SetWindowsHookEx(WH_KEYBOARD, @KeyboardHookProc, 0, GetCurrentThreadId());
  if Hooked then
    ODS('install keyboard hook success', [])
  else
    ODS('install keyboard hook failed', []);
end;

destructor THooker.Destroy;
begin
  if Hooked then
  begin
    if UnhookWindowsHookEx(FHook) then
      ODS('uninstall keyboard hook success', [])
    else
      ODS('uninstall keyboard hook failed', []);
  end;

  inherited Destroy;
end;

initialization
  if IsExEditWindowExists() then
    Hooker := THooker.Create()
  else
    Hooker := nil;

finalization
  if Hooker <> nil then
    Hooker.Free();

end.

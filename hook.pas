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

implementation

uses
  Windows;

var
  GHooker: THooker;

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
      (GHooker.OnPressShiftCtrlAltA <> nil) then
      GHooker.OnPressShiftCtrlAltA(GHooker);
  finally
    if not processed then
      Result := CallNextHookEx(GHooker.FHook, code, wp, lp);
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
  GHooker := Self;
end;

destructor THooker.Destroy;
begin
  if Hooked then
    UnhookWindowsHookEx(FHook);
  inherited Destroy;
end;

end.

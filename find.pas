unit find;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Windows;

type
  TExEditWindow = record
    Window: THandle;
    Config: THandle;
    Static_: THandle;
    StaticText: WideString;
  end;

  TExEditParameterDialog = record
    Window: THandle;
    Edit: THandle;
    EditText: WideString;
    OK: THandle;
    Cancel: THandle;
  end;

function IsExEditWindowExists(): Boolean;
function FindExEditWindow(out w: TExEditWindow): Boolean;
function FindExEditParameterDialog(out pw: TExEditParameterDialog): Boolean;

implementation

uses
  util;

function FindControl(Parent, Prev: THandle; ControlClass: WideString; Rect: PRect): THandle;
var
  h: THandle;
  r: TRect;
  pt: TPoint;
begin
  Result := 0;
  h := Prev;
  while Result = 0 do begin
    h := FindWindowExW(Parent, h, PWideChar(ControlClass), nil);
    if h = 0 then Exit;
    if Rect <> nil then begin
      if not GetWindowRect(h, @r) then
        continue;
      pt := r.CenterPoint();
      if not ScreenToClient(Parent, pt) then
        continue;
      if not Rect^.Contains(pt) then
        continue;
    end;
    Result := h;
  end;
end;

function GetComboBoxSelectedItem(h: THandle): WideString;
var
  idx, len: WPARAM;
begin
  idx := SendMessageW(h, CB_GETCURSEL, 0, 0);
  len := SendMessageW(h, CB_GETLBTEXTLEN, idx, 0);
  SetLength(Result, len);
  SendMessageW(h, CB_GETLBTEXT, idx, LPARAM(@Result[1]));
end;

function GetControlText(h: THandle): WideString;
var
  len: WPARAM;
begin
  len := SendMessageW(h, WM_GETTEXTLENGTH, 0, 0);
  SetLength(Result, len);
  SendMessageW(h, WM_GETTEXT, len, LPARAM(@Result[1]));
end;

function FindPSDToolIPCSelectedComboBox(Parent: THandle): THandle;
var
  h: THandle;
  s: WideString;
begin
  h := 0;
  Result := 0;
  while Result = 0 do begin
    h := FindControl(Parent, h, 'ComboBox', nil);
    if h = 0 then Exit;
    s := GetComboBoxSelectedItem(h);
    if Pos(WideString('Render@PSDToolIPC'), s) = Length(s)-16 then
      Result := h;
  end;
end;

function FindSiblingControl(Parent, ComboBox: THandle; ControlClass: WideString): THandle;
var
  h: THandle;
  WindowRect, ComboBoxRect, TargetRect: TRect;
begin
  Result := 0;
  if not GetClientRect(Parent, @WindowRect) then Exit;
  if not GetWindowRect(ComboBox, @ComboBoxRect) then Exit;
  if not ScreenToClient(Parent, ComboBoxRect.TopLeft) then Exit;
  if not ScreenToClient(Parent, ComboBoxRect.BottomRight) then Exit;
  TargetRect.TopLeft := ComboBoxRect.TopLeft;
  TargetRect.BottomRight := TPoint.Create(WindowRect.Right - ComboBoxRect.Left, ComboBOxRect.Bottom);
  h := 0;
  while Result = 0 do begin
    h := FindControl(Parent, h, ControlClass, @TargetRect);
    if h = 0 then Exit;
    if not IsWindowVisible(h) then continue;
    Result := h;
  end;
end;

function IsExEditWindowExists(): Boolean;
var
  h: THandle;
  pid, mypid: DWORD;
begin
  Result := False;
  mypid := GetCurrentProcessId();
  h := 0;
  while not Result do begin
    h := FindWindowExA(0, h, 'ExtendedFilterClass', nil);
    if h = 0 then Exit;
    GetWindowThreadProcessId(h, @pid);
    Result := pid = mypid;
  end;
end;

function FindExEditWindow(out w: TExEditWindow): Boolean;
var
  h, Window, ComboBox, Config, Static: THandle;
  pid, mypid: DWORD;
begin
  Result := False;
  mypid := GetCurrentProcessId();
  h := 0;
  Window := 0;
  while Window = 0 do begin
    h := FindWindowExA(0, h, 'ExtendedFilterClass', nil);
    if h = 0 then Exit;
    GetWindowThreadProcessId(h, @pid);
    if not IsWindowVisible(h) then continue;
    if pid = mypid then
      Window := h;
  end;

  ComboBox := FindPSDToolIPCSelectedComboBox(Window);
  if ComboBox = 0 then Exit;
  Config := FindSiblingControl(Window, ComboBox, 'Button');
  if Config = 0 then Exit;
  Static := FindSiblingControl(Window, ComboBox, 'Static');
  if Static = 0 then Exit;
  w.Window:=Window;
  w.Config:=Config;
  w.Static_:=Static;
  w.StaticText:=GetControlText(Static);
  Result := True;
end;

function FindExEditParameterDialog(out pw: TExEditParameterDialog): Boolean;
var
  h, Dialog, Edit, OK, Cancel: THandle;
  pid, mypid, id: DWORD;
  EditText: WideString;
begin
  Result := False;

  mypid := GetCurrentProcessId();
  h := 0;
  Dialog := 0;
  while Dialog = 0 do begin
    h := FindWindowExA(0, h, '#32770', nil);
    if h = 0 then Exit;
    GetWindowThreadProcessId(h, @pid);
    if not IsWindowVisible(h) then continue;
    if pid = mypid then
      Dialog := h;
  end;

  h := 0;
  Edit := 0;
  while Edit = 0 do begin
    h := FindControl(Dialog, h, 'Edit', nil);
    if h = 0 then Exit;
    if not IsWindowVisible(h) then continue;
    EditText := GetControlText(h);
    //if (Pos(WideString('fx=0;'), EditText) = 0)and(Pos(WideString('fx=1;'), EditText) = 0) then Exit;
    //if (Pos(WideString('fy=0;'), EditText) = 0)and(Pos(WideString('fy=1;'), EditText) = 0) then Exit;
    Edit := h;
  end;

  h := 0;
  OK := 0;
  while OK = 0 do begin
    h := FindControl(Dialog, h, 'Button', nil);
    if h = 0 then Exit;
    if not IsWindowVisible(h) then continue;
    id := GetWindowLong(h, GWL_ID);
    if id = IDOK then
      OK := h;
  end;

  h := 0;
  Cancel := 0;
  while Cancel = 0 do begin
    h := FindControl(Dialog, h, 'Button', nil);
    if h = 0 then Exit;
    if not IsWindowVisible(h) then continue;
    id := GetWindowLong(h, GWL_ID);
    if id = IDCANCEL then
      Cancel := h;
  end;
  Result := True;
  pw.Window:=Dialog;
  pw.Edit:=Edit;
  pw.EditText := EditText;
  pw.OK:=OK;
  pw.Cancel := Cancel;
end;

end.


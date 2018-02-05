unit Find;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Windows;

type
  TExEditWindow = record
    Window: THandle;
    Config: THandle;
  end;

  TExEditParameterDialog = record
    Window: THandle;
    Edit: THandle;
    EditText: WideString;
    OK: THandle;
    Cancel: THandle;
  end;

  TExEditMultiParameterDialog = record
    Window: THandle;
    Caption: array of WideString;
    Edit: array of THandle;
    OK: THandle;
    Cancel: THandle;
  end;

function IsExEditWindowExists(): boolean;
function FindExEditWindow(out w: TExEditWindow): boolean;
function FindExEditParameterDialog(out pw: TExEditParameterDialog): boolean;
function FindExEditMultiParameterDialog(out pw: TExEditMultiParameterDialog): boolean;

implementation

uses
  Classes, Util;

type
  TEditCaption = record
    Edit: THandle;
    EditRect: TRect;
    Caption: THandle;
  end;
  PEditCaption = ^TEditCaption;

function EditListCompare(Item1, Item2: Pointer): integer;
var
  P1: PEditCaption absolute Item1;
  P2: PEditCaption absolute Item2;
begin
  Result := P1^.EditRect.CenterPoint.y - P2^.EditRect.CenterPoint.y;
end;

function FindControl(Parent, Prev: THandle; ControlClass: WideString;
  Rect: PRect): THandle;
var
  h: THandle;
  r: TRect;
  pt: TPoint;
  cls: PWideChar;
begin
  Result := 0;
  h := Prev;
  cls := PWideChar(ControlClass);
  while Result = 0 do
  begin
    h := FindWindowExW(Parent, h, cls, nil);
    if h = 0 then
      Exit;
    if Rect <> nil then
    begin
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
  SetLength(Result, len+1);
  SendMessageW(h, CB_GETLBTEXT, idx, {%H-}LPARAM(@Result[1]));
  Result := PWideChar(Result);
end;

function GetControlText(h: THandle): WideString;
var
  len: WPARAM;
begin
  len := SendMessageW(h, WM_GETTEXTLENGTH, 0, 0);
  SetLength(Result, len+1);
  SendMessageW(h, WM_GETTEXT, len+1, {%H-}LPARAM(@Result[1]));
  Result := PWideChar(Result);
end;

function FindPSDToolKitSelectedComboBox(Parent: THandle): THandle;
var
  h: THandle;
  s: WideString;
begin
  h := 0;
  Result := 0;
  while Result = 0 do
  begin
    h := FindControl(Parent, h, 'ComboBox', nil);
    if h = 0 then
      Exit;
    if not IsWindowVisible(h) then
      continue;
    s := GetComboBoxSelectedItem(h);
    if Pos(WideString('Assign@PSDToolKit'), s) = Length(s) - 16 then
      Result := h;
  end;
end;

function FindSiblingControl(Parent, ComboBox: THandle;
  ControlClass: WideString): THandle;
var
  h: THandle;
  WindowRect, ComboBoxRect, TargetRect: TRect;
begin
  Result := 0;
  if not GetClientRect(Parent, @WindowRect) then
    Exit;
  if not GetWindowRect(ComboBox, @ComboBoxRect) then
    Exit;
  if not ScreenToClient(Parent, ComboBoxRect.TopLeft) then
    Exit;
  if not ScreenToClient(Parent, ComboBoxRect.BottomRight) then
    Exit;
  TargetRect.TopLeft := ComboBoxRect.TopLeft;
  TargetRect.BottomRight := TPoint.Create(WindowRect.Right -
    ComboBoxRect.Left, ComboBOxRect.Bottom);
  h := 0;
  while Result = 0 do
  begin
    h := FindControl(Parent, h, ControlClass, @TargetRect);
    if h = 0 then
      Exit;
    if not IsWindowVisible(h) then
      continue;
    if (ControlClass = 'Button') and ((GetWindowLong(h, GWL_STYLE) and BS_CHECKBOX) =
      BS_CHECKBOX) then
      continue;
    Result := h;
  end;
end;

function IsExEditWindowExists(): boolean;
var
  h: THandle;
  pid, mypid: DWORD;
begin
  Result := False;
  mypid := GetCurrentProcessId();
  h := 0;
  while not Result do
  begin
    h := FindWindowExA(0, h, 'ExtendedFilterClass', nil);
    if h = 0 then
      Exit;
    GetWindowThreadProcessId(h, @pid);
    Result := pid = mypid;
  end;
end;

function FindExEditWindow(out w: TExEditWindow): boolean;
var
  h, Window, ComboBox: THandle;
  pid, mypid: DWORD;
begin
  Result := False;
  FillChar(w, SizeOf(w), 0);
  mypid := GetCurrentProcessId();
  h := 0;
  Window := 0;
  while Window = 0 do
  begin
    h := FindWindowExA(0, h, 'ExtendedFilterClass', nil);
    if h = 0 then
      Exit;
    GetWindowThreadProcessId(h, @pid);
    if not IsWindowVisible(h) then
      continue;
    if pid = mypid then
      Window := h;
  end;

  ComboBox := FindPSDToolKitSelectedComboBox(Window);
  if ComboBox <> 0 then
    w.Config := FindSiblingControl(Window, ComboBox, 'Button');
  w.Window := Window;
  Result := True;
end;

function FindExEditParameterDialog(out pw: TExEditParameterDialog): boolean;
var
  h, Dialog, Edit, OK, Cancel: THandle;
  pid, mypid, id: DWORD;
  EditText: WideString;
begin
  Result := False;

  mypid := GetCurrentProcessId();
  h := 0;
  Dialog := 0;
  while Dialog = 0 do
  begin
    h := FindWindowExA(0, h, '#32770', nil);
    if h = 0 then
      Exit;
    GetWindowThreadProcessId(h, @pid);
    if not IsWindowVisible(h) then
      continue;
    if pid = mypid then
      Dialog := h;
  end;

  h := 0;
  Edit := 0;
  while Edit = 0 do
  begin
    h := FindControl(Dialog, h, 'Edit', nil);
    if h = 0 then
      Exit;
    if not IsWindowVisible(h) then
      continue;
    EditText := GetControlText(h);
    Edit := h;
  end;

  h := 0;
  OK := 0;
  while OK = 0 do
  begin
    h := FindControl(Dialog, h, 'Button', nil);
    if h = 0 then
      Exit;
    if not IsWindowVisible(h) then
      continue;
    id := GetWindowLong(h, GWL_ID);
    if id = idOk then
      OK := h;
  end;

  h := 0;
  Cancel := 0;
  while Cancel = 0 do
  begin
    h := FindControl(Dialog, h, 'Button', nil);
    if h = 0 then
      Exit;
    if not IsWindowVisible(h) then
      continue;
    id := GetWindowLong(h, GWL_ID);
    if id = idCancel then
      Cancel := h;
  end;
  Result := True;
  pw.Window := Dialog;
  pw.Edit := Edit;
  pw.EditText := EditText;
  pw.OK := OK;
  pw.Cancel := Cancel;
end;

function FindExEditMultiParameterDialog(out pw: TExEditMultiParameterDialog): boolean;
var
  Caption: array of THandle;
  CaptionRect: array of TRect;
  EditCaption: array of TEditCaption;
  I, J: integer;
  P: PEditCaption;
  h, Dialog, OK, Cancel: THandle;
  pid, mypid, id: DWORD;
  L: TList;
begin
  Result := False;

  mypid := GetCurrentProcessId();
  h := 0;
  Dialog := 0;
  while Dialog = 0 do
  begin
    h := FindWindowExA(0, h, '#32770', nil);
    if h = 0 then
      Exit;
    GetWindowThreadProcessId(h, @pid);
    if not IsWindowVisible(h) then
      continue;
    if pid = mypid then
      Dialog := h;
  end;

  h := 0;
  I := 0;
  while True do
  begin
    h := FindControl(Dialog, h, 'Static', nil);
    if h = 0 then
      break;
    if not IsWindowVisible(h) then
      continue;
    SetLength(Caption, I + 1);
    SetLength(CaptionRect, I + 1);
    Caption[I] := h;
    GetWindowRect(h, CaptionRect[I]);
    Inc(I);
  end;

  h := 0;
  I := 0;
  while True do
  begin
    h := FindControl(Dialog, h, 'Edit', nil);
    if h = 0 then
      break;
    if not IsWindowVisible(h) then
      continue;
    SetLength(EditCaption, I + 1);
    EditCaption[I].Edit := h;
    GetWindowRect(h, EditCaption[I].EditRect);
    EditCaption[I].Caption := 0;
    for J := Low(Caption) to High(Caption) do
    begin
      if (Caption[J] <> 0) and (EditCaption[I].EditRect.Top <
        CaptionRect[J].CenterPoint.y) and (CaptionRect[J].CenterPoint.y <
        EditCaption[I].EditRect.Bottom) and (CaptionRect[J].CenterPoint.x <
        EditCaption[I].EditRect.Left) then
      begin
        EditCaption[I].Caption := Caption[J];
        Caption[J] := 0;
        break;
      end;
    end;
    if EditCaption[I].Caption = 0 then
      SetLength(EditCaption, I)
    else
      Inc(I);
  end;

  h := 0;
  OK := 0;
  while OK = 0 do
  begin
    h := FindControl(Dialog, h, 'Button', nil);
    if h = 0 then
      Exit;
    if not IsWindowVisible(h) then
      continue;
    id := GetWindowLong(h, GWL_ID);
    if id = idOk then
      OK := h;
  end;

  h := 0;
  Cancel := 0;
  while Cancel = 0 do
  begin
    h := FindControl(Dialog, h, 'Button', nil);
    if h = 0 then
      Exit;
    if not IsWindowVisible(h) then
      continue;
    id := GetWindowLong(h, GWL_ID);
    if id = idCancel then
      Cancel := h;
  end;

  L := TList.Create();
  try
    for I := Low(EditCaption) to High(EditCaption) do
      L.Add(@EditCaption[i]);
    L.Sort(@EditListCompare);
    SetLength(pw.Edit, Length(EditCaption));
    SetLength(pw.Caption, Length(EditCaption));
    for I := Low(EditCaption) to High(EditCaption) do begin
      P := L.Items[I];
      pw.Edit[I] := P^.Edit;
      pw.Caption[I] := GetControlText(P^.Caption);
    end;
  finally
    L.Free;
  end;
  Result := True;
  pw.Window := Dialog;
  pw.OK := OK;
  pw.Cancel := Cancel;
end;

end.

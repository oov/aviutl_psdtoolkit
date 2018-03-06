unit SettingDialog;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Lua;

function ShowSettingDialog(L: Plua_State): integer;

implementation

{$R *.res}

uses
  Windows, SysUtils, Classes, Util;

type
  THandleDynArray = array of THandle;

  TDisableFamilyWindowsData = record
    PID: DWORD;
    Exclude: THandle;
    Handles: THandleDynArray;
  end;
  PDisableFamilyWindowsData = ^TDisableFamilyWindowsData;

function DisableFamilyWindowsCallback(Window: HWND; LParam: LPARAM): WINBOOL; stdcall;
var
  P: PDisableFamilyWindowsData absolute LParam;
  PID: DWORD;
begin
  if IsWindowVisible(Window) and IsWindowEnabled(Window) and
    (P^.Exclude <> Window) then
  begin
    GetWindowThreadProcessId(Window, PID);
    if PID = P^.PID then
    begin
      SetLength(P^.Handles, Length(P^.Handles) + 1);
      P^.Handles[Length(P^.Handles) - 1] := Window;
      EnableWindow(Window, False);
    end;
  end;
  Result := True;
end;

function DisableFamilyWindows(const Exclude: THandle): THandleDynArray;
var
  Data: TDisableFamilyWindowsData;
begin
  Data.PID := GetCurrentProcessId();
  Data.Exclude := Exclude;
  EnumWindows(@DisableFamilyWindowsCallback, LPARAM(@Data));
  Result := Data.Handles;
end;

procedure EnableFamilyWindows(const Handles: THandleDynArray);
var
  I: integer;
begin
  for I := Low(Handles) to High(Handles) do
    EnableWindow(Handles[I], True);
end;

const
  ID_CHECKBOX_GENERATE_LIPSYNCPREP = 100;
  ID_CHECKBOX_GENERATE_MPSLIDER = 101;
  ID_CHECKBOX_GENERATE_SUBTITLEPREP = 102;
  ID_COMBOBOX_NUMOFMPSLIDER = 1001;
  ID_CHECKBOX_SUBTITLEGROUP = 1002;
  ID_COMBOBOX_SUBTITLEENCODING = 1003;

type
  { TSettingDialog }

  TSettingDialog = class
  protected
    function WndProc(Hwnd: HWND; Message: UINT; WP: WPARAM; LP: LPARAM): LRESULT;
  private
    FGenerateLipSyncPrep: boolean;
    FGenerateSubtitlePrep: boolean;
    FNumOfMPSlider: integer;
    FSubtitleEncoding: integer;
    FSubtitleGrouping: boolean;
    FWindow: THandle;
    FSuspendedWindows: THandleDynArray;
    function GetControl(const ControlId: integer): THandle;
    function GetCheck(const ControlId: integer): boolean;
    procedure SetCheck(const ControlId: integer; const Value: boolean);

    procedure InitDialog();
    procedure FinalDialog();
  public
    constructor Create();
    destructor Destroy(); override;
    property GenerateLipSyncPrep: boolean read FGenerateLipSyncPrep
      write FGenerateLipSyncPrep;
    property NumOfMPSlider: integer read FNumOfMPSlider write FNumOfMPSlider;
    property GenerateSubtitlePrep: boolean read FGenerateSubtitlePrep
      write FGenerateSubtitlePrep;
    property SubtitleGrouping: boolean read FSubtitleGrouping write FSubtitleGrouping;
    property SubtitleEncoding: integer read FSubtitleEncoding write FSubtitleEncoding;
  end;

var
  Dialog: TSettingDialog;

function WndProcTrampoline(Hwnd: HWND; Message: UINT; WP: WPARAM;
  LP: LPARAM): LRESULT; stdcall;
begin
  try
    Result := Dialog.WndProc(Hwnd, Message, WP, LP);
  except
    on E: Exception do
      MessageBoxW(Hwnd, PWideChar(
        '予期しない例外が発生しました。'#13#10#13#10 +
        WideString(E.Message)),
        '環境設定', MB_ICONERROR);
  end;
end;

{ TICTalkDialog }

function TSettingDialog.GetControl(const ControlId: integer): THandle;
begin
  Result := GetDlgItem(FWindow, ControlId);
end;

function TSettingDialog.GetCheck(const ControlId: integer): boolean;
begin
  Result := SendMessageW(GetControl(ControlId), BM_GETCHECK, 0, 0) = BST_CHECKED;
end;

procedure TSettingDialog.SetCheck(const ControlId: integer; const Value: boolean);
const
  state: array[boolean] of WPARAM = (BST_UNCHECKED, BST_CHECKED);
begin
  SendMessageW(GetControl(ControlId), BM_SETCHECK, state[Value], 0);
end;

function TSettingDialog.WndProc(Hwnd: HWND; Message: UINT; WP: WPARAM;
  LP: LPARAM): LRESULT;
begin
  Result := 0;
  case Message of
    WM_INITDIALOG:
    begin
      FWindow := Hwnd;
      try
        InitDialog();
        Result := 1;
      except
        EndDialog(Hwnd, idAbort);
        raise;
      end;
    end;
    WM_COMMAND:
    begin
      case LOWORD(WP) of
        idOk: EndDialog(Hwnd, idOk);
        idCancel: EndDialog(Hwnd, idCancel);
      end;
      Result := 1;
    end;
    WM_DESTROY:
    begin
      FinalDialog();
      Result := 1;
    end;
    else;
  end;
end;

procedure TSettingDialog.InitDialog();
var
  I: integer;
  H: THandle;
  WS: WideString;
  Rect: TRect;
begin
  FSuspendedWindows := DisableFamilyWindows(FWindow);

  SetWindowTextW(FWindow, 'PSDToolKit 環境設定');

  SetCheck(ID_CHECKBOX_GENERATE_LIPSYNCPREP, FGenerateLipSyncPrep);
  SetCheck(ID_CHECKBOX_GENERATE_MPSLIDER, FNumOfMPSlider > 0);
  SetCheck(ID_CHECKBOX_GENERATE_SUBTITLEPREP, FGenerateSubtitlePrep);

  H := GetControl(ID_COMBOBOX_NUMOFMPSLIDER);
  for I := 1 to 11 do
  begin
    WS := WideString(IntToStr(I * 4));
    SendMessageW(H, CB_ADDSTRING, 0, {%H-}LPARAM(PWideChar(WS)));
  end;
  GetWindowRect(H, Rect);
  SetWindowPos(H, 0, 0, 0, Rect.Width, 300, SWP_NOMOVE or SWP_NOZORDER);
  if FNumOfMPSlider = 0 then
    SendMessageW(H, CB_SETCURSEL, 1, 0)
  else
    SendMessageW(H, CB_SETCURSEL, FNumOfMPSlider - 1, 0);

  SetCheck(ID_CHECKBOX_SUBTITLEGROUP, FSubtitleGrouping);
  H := GetControl(ID_COMBOBOX_SUBTITLEENCODING);
  SendMessageW(H, CB_ADDSTRING, 0, {%H-}LPARAM(PWideChar('Shift_JIS')));
  SendMessageW(H, CB_ADDSTRING, 0, {%H-}LPARAM(PWideChar('UTF-8')));
  GetWindowRect(H, Rect);
  SetWindowPos(H, 0, 0, 0, Rect.Width, 300, SWP_NOMOVE or SWP_NOZORDER);
  SendMessageW(H, CB_SETCURSEL, FSubtitleEncoding, 0);
end;

procedure TSettingDialog.FinalDialog();
begin
  EnableFamilyWindows(FSuspendedWindows);

  FGenerateLipSyncPrep := GetCheck(ID_CHECKBOX_GENERATE_LIPSYNCPREP);
  if GetCheck(ID_CHECKBOX_GENERATE_MPSLIDER) = False then
    FNumOfMPSlider := 0
  else
    FNumOfMPSlider := SendMessageW(GetControl(ID_COMBOBOX_NUMOFMPSLIDER),
      CB_GETCURSEL, 0, 0) + 1;
  FGenerateSubtitlePrep := GetCheck(ID_CHECKBOX_GENERATE_SUBTITLEPREP);
  FSubtitleGrouping := GetCheck(ID_CHECKBOX_SUBTITLEGROUP);
  FSubtitleEncoding := SendMessageW(GetControl(ID_COMBOBOX_SUBTITLEENCODING),
    CB_GETCURSEL, 0, 0);
end;

constructor TSettingDialog.Create();
begin
  inherited Create();
end;

destructor TSettingDialog.Destroy();
begin
  inherited Destroy();
end;

function ShowSettingDialog(L: Plua_State): integer;
var
  FilePath: UTF8String;
  Parent: THandle;
  SL: TStringList;
  FS: TFileStreamW;
begin
  lua_getfield(L, -1, 'parent');
  Parent := THandle(lua_tointeger(L, -1));
  lua_pop(L, 1);

  lua_getfield(L, -1, 'filepath');
  FilePath := UTF8String(lua_tostring(L, -1));
  lua_pop(L, 1);

  Result := 1;
  Dialog := TSettingDialog.Create();
  try
    lua_getfield(L, -1, 'wav_lipsync');
    if lua_isnil(L, -1) then
      Dialog.GenerateLipSyncPrep := False
    else
      Dialog.GenerateLipSyncPrep := lua_toboolean(L, -1);
    lua_pop(L, 1);

    lua_getfield(L, -1, 'wav_mpslider');
    if lua_isnil(L, -1) then
      Dialog.NumOfMPSlider := 0
    else
      Dialog.NumOfMPSlider := lua_tointeger(L, -1);
    lua_pop(L, 1);

    lua_getfield(L, -1, 'wav_insertmode');
    if lua_isnil(L, -1) then
      Dialog.GenerateSubtitlePrep := False
    else
      Dialog.GenerateSubtitlePrep := lua_tointeger(L, -1) <> 0;
    lua_pop(L, 1);

    lua_getfield(L, -1, 'wav_groupsubtitle');
    if lua_isnil(L, -1) then
      Dialog.SubtitleGrouping := True
    else
      Dialog.SubtitleGrouping := lua_toboolean(L, -1);
    lua_pop(L, 1);

    lua_getfield(L, -1, 'wav_subtitleencoding');
    if lua_isnil(L, -1) then
      Dialog.SubtitleEncoding := 0
    else
    begin
      if lua_tostring(L, -1) = 'utf8' then
        Dialog.SubtitleEncoding := 1
      else
        Dialog.SubtitleEncoding := 0;
    end;
    lua_pop(L, 1);

    if DialogBoxParamW(hInstance, 'SETTINGDIALOG', Parent, @WndProcTrampoline,
      LPARAM(Dialog)) = idOk then
    begin
      SL := TStringList.Create;
      try
        SL.Add('local P = {}');
        if Dialog.GenerateLipSyncPrep then
          SL.Add('P.wav_lipsync = true');
        if Dialog.NumOfMPSlider > 0 then
          SL.Add('P.wav_mpslider = ' + IntToStr(Dialog.NumOfMPSlider));
        if Dialog.GenerateSubtitlePrep then
          SL.Add('P.wav_insertmode = 2');
        if not Dialog.SubtitleGrouping then
          SL.Add('P.wav_groupsubtitle = false');
        if Dialog.SubtitleEncoding <> 0 then
          SL.Add('P.wav_subtitleencoding = "utf8"');
        SL.Add('return P');

        FS := TFileStreamW.Create(WideString(FilePath));
        try
          SL.SaveToStream(FS);
        finally
          FS.Free;
        end;
      finally
        SL.Free;
      end;
    end;
  finally
    FreeAndNil(Dialog);
  end;
end;

end.

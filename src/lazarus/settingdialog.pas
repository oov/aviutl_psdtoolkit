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
  ID_LIPSYNC_GENERATE = 100;
  ID_MPSLIDER_GENERATE = 101;
  ID_SUBTITLE_GENERATE = 102;
  ID_LIPSYNC_GROUP_ID = 1000;
  ID_LIPSYNC_OFFSET = 1001;
  ID_MPSLIDER_NUMBER = 2000;
  ID_MPSLIDER_GROUP_ID = 2001;
  ID_MPSLIDER_MARGIN_LEFT = 2002;
  ID_MPSLIDER_MARGIN_RIGHT = 2003;
  ID_SUBTITLE_ENCODING = 3000;
  ID_SUBTITLE_GROUP_ID = 3001;
  ID_SUBTITLE_MARGIN_LEFT = 3002;
  ID_SUBTITLE_MARGIN_RIGHT = 3003;

type
  { TSettingDialog }

  TSettingDialog = class
  protected
    function WndProc(Hwnd: HWND; Message: UINT; WP: WPARAM; LP: LPARAM): LRESULT;
  private
    FLipsyncGenerate: boolean;
    FLipsyncGroupId: integer;
    FLipsyncOffset: integer;
    FMPSliderGenerate: boolean;
    FMPSliderGroupId: integer;
    FMPSliderMarginLeft: integer;
    FMPSliderMarginRight: integer;
    FMPSliderNumber: integer;
    FSubtitleGenerate: boolean;
    FSubtitleEncoding: integer;
    FSubtitleGroupId: integer;
    FSubtitleMarginLeft: integer;
    FSubtitleMarginRight: integer;
    FWindow: THandle;
    FSuspendedWindows: THandleDynArray;
    function GetControl(const ControlId: integer): THandle;
    function GetCheck(const ControlId: integer): boolean;
    procedure SetCheck(const ControlId: integer; const Value: boolean);
    function GetText(const ControlId: integer): WideString;
    procedure SetText(const ControlId: integer; const Value: WideString);

    procedure InitDialog();
    procedure FinalDialog();
  public
    constructor Create();
    destructor Destroy(); override;
    property LipsyncGenerate: boolean read FLipsyncGenerate write FLipsyncGenerate;
    property LipsyncGroupId: integer read FLipsyncGroupId write FLipsyncGroupId;
    property LipsyncOffset: integer read FLipsyncOffset write FLipsyncOffset;
    property MPSliderGenerate: boolean read FMPSliderGenerate write FMPSliderGenerate;
    property MPSliderNumber: integer read FMPSliderNumber write FMPSliderNumber;
    property MPSliderGroupId: integer read FMPSliderGroupId write FMPSliderGroupId;
    property MPSliderMarginLeft: integer read FMPSliderMarginLeft
      write FMPSliderMarginLeft;
    property MPSliderMarginRight: integer read FMPSliderMarginRight
      write FMPSliderMarginRight;
    property SubtitleGenerate: boolean read FSubtitleGenerate write FSubtitleGenerate;
    property SubtitleEncoding: integer read FSubtitleEncoding write FSubtitleEncoding;
    property SubtitleGroupId: integer read FSubtitleGroupId write FSubtitleGroupId;
    property SubtitleMarginLeft: integer read FSubtitleMarginLeft
      write FSubtitleMarginLeft;
    property SubtitleMarginRight: integer read FSubtitleMarginRight
      write FSubtitleMarginRight;
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

function TSettingDialog.GetText(const ControlId: integer): WideString;
var
  h: THandle;
begin
  h := GetControl(ControlId);
  SetLength(Result, GetWindowTextLengthW(h) + 1);
  GetWindowTextW(h, @Result[1], Length(Result));
  SetLength(Result, Length(Result) - 1);
end;

procedure TSettingDialog.SetText(const ControlId: integer; const Value: WideString);
begin
  SetWindowTextW(GetControl(ControlId), PWideChar(Value));
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

  SetCheck(ID_LIPSYNC_GENERATE, FLipsyncGenerate);
  SetCheck(ID_MPSLIDER_GENERATE, FMPSliderGenerate);
  SetCheck(ID_SUBTITLE_GENERATE, FSubtitleGenerate);

  SetText(ID_LIPSYNC_GROUP_ID, WideString(IntToStr(FLipsyncGroupId)));
  SetText(ID_LIPSYNC_OFFSET, WideString(IntToStr(FLipsyncOffset)));

  H := GetControl(ID_MPSLIDER_NUMBER);
  for I := 1 to 11 do
  begin
    WS := WideString(IntToStr(I * 4));
    SendMessageW(H, CB_ADDSTRING, 0, {%H-}LPARAM(PWideChar(WS)));
  end;
  GetWindowRect(H, Rect);
  SetWindowPos(H, 0, 0, 0, Rect.Width, 300, SWP_NOMOVE or SWP_NOZORDER);
  SendMessageW(H, CB_SETCURSEL, FMPSliderNumber, 0);
  SetText(ID_MPSLIDER_GROUP_ID, WideString(IntToStr(FMPSliderGroupId)));
  SetText(ID_MPSLIDER_MARGIN_LEFT, WideString(IntToStr(FMPSliderMarginLeft)));
  SetText(ID_MPSLIDER_MARGIN_RIGHT, WideString(IntToStr(FMPSliderMarginRight)));

  H := GetControl(ID_SUBTITLE_ENCODING);
  SendMessageW(H, CB_ADDSTRING, 0, {%H-}LPARAM(PWideChar('Shift_JIS')));
  SendMessageW(H, CB_ADDSTRING, 0, {%H-}LPARAM(PWideChar('UTF-8')));
  GetWindowRect(H, Rect);
  SetWindowPos(H, 0, 0, 0, Rect.Width, 300, SWP_NOMOVE or SWP_NOZORDER);
  SendMessageW(H, CB_SETCURSEL, FSubtitleEncoding, 0);
  SetText(ID_SUBTITLE_GROUP_ID, WideString(IntToStr(FSubtitleGroupId)));
  SetText(ID_SUBTITLE_MARGIN_LEFT, WideString(IntToStr(FSubtitleMarginLeft)));
  SetText(ID_SUBTITLE_MARGIN_RIGHT, WideString(IntToStr(FSubtitleMarginRight)));
end;

procedure TSettingDialog.FinalDialog();
begin
  EnableFamilyWindows(FSuspendedWindows);

  FLipsyncGenerate := GetCheck(ID_LIPSYNC_GENERATE);
  FMPSliderGenerate := GetCheck(ID_MPSLIDER_GENERATE);
  FSubtitleGenerate := GetCheck(ID_SUBTITLE_GENERATE);

  FLipsyncGroupId := StrToIntDef(string(GetText(ID_LIPSYNC_GROUP_ID)), 1);
  FLipsyncOffset := StrToIntDef(string(GetText(ID_LIPSYNC_OFFSET)), 0);

  FMPSliderNumber := SendMessageW(GetControl(ID_MPSLIDER_NUMBER), CB_GETCURSEL, 0, 0);
  FMPSliderGroupId := StrToIntDef(string(GetText(ID_MPSLIDER_GROUP_ID)), 1);
  FMPSliderMarginLeft := StrToIntDef(string(GetText(ID_MPSLIDER_MARGIN_LEFT)), 0);
  FMPSliderMarginRight := StrToIntDef(string(GetText(ID_MPSLIDER_MARGIN_RIGHT)), 0);

  FSubtitleEncoding := SendMessageW(GetControl(ID_SUBTITLE_ENCODING),
    CB_GETCURSEL, 0, 0);
  FSubtitleGroupId := StrToIntDef(string(GetText(ID_SUBTITLE_GROUP_ID)), 1);
  FSubtitleMarginLeft := StrToIntDef(string(GetText(ID_SUBTITLE_MARGIN_LEFT)), 0);
  FSubtitleMarginRight := StrToIntDef(string(GetText(ID_SUBTITLE_MARGIN_RIGHT)), 0);
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
      Dialog.LipsyncGenerate := False
    else
      Dialog.LipsyncGenerate := lua_tointeger(L, -1) <> 0;
    lua_pop(L, 1);

    lua_getfield(L, -1, 'wav_mpslider');
    if lua_isnil(L, -1) then
      Dialog.MPSliderGenerate := False
    else
      Dialog.MPSliderGenerate := lua_tointeger(L, -1) > 0;
    lua_pop(L, 1);

    lua_getfield(L, -1, 'wav_subtitle');
    if lua_isnil(L, -1) then
      Dialog.SubtitleGenerate := False
    else
      Dialog.SubtitleGenerate := lua_tointeger(L, -1) <> 0;
    lua_pop(L, 1);

    lua_getfield(L, -1, 'wav_lipsync_group');
    if lua_isnil(L, -1) then
      Dialog.LipsyncGroupId := 1
    else
      Dialog.LipsyncGroupId := lua_tointeger(L, -1);
    lua_pop(L, 1);

    lua_getfield(L, -1, 'wav_lipsync_offset');
    if lua_isnil(L, -1) then
      Dialog.LipsyncOffset := 0
    else
      Dialog.LipsyncOffset := lua_tointeger(L, -1);
    lua_pop(L, 1);

    lua_getfield(L, -1, 'wav_mpslider');
    if lua_isnil(L, -1) then
      Dialog.MPSliderNumber := 1
    else
      Dialog.MPSliderNumber := lua_tointeger(L, -1) - 1;
    lua_pop(L, 1);

    lua_getfield(L, -1, 'wav_mpslider_group');
    if lua_isnil(L, -1) then
      Dialog.MPSliderGroupId := 1
    else
      Dialog.MPSliderGroupId := lua_tointeger(L, -1);
    lua_pop(L, 1);

    lua_getfield(L, -1, 'wav_mpslider_margin_left');
    if lua_isnil(L, -1) then
      Dialog.MPSliderMarginLeft := 0
    else
      Dialog.MPSliderMarginLeft := lua_tointeger(L, -1);
    lua_pop(L, 1);

    lua_getfield(L, -1, 'wav_mpslider_margin_right');
    if lua_isnil(L, -1) then
      Dialog.MPSliderMarginRight := 0
    else
      Dialog.MPSliderMarginRight := lua_tointeger(L, -1);
    lua_pop(L, 1);

    lua_getfield(L, -1, 'wav_subtitle_encoding');
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

    lua_getfield(L, -1, 'wav_subtitle_group');
    if lua_isnil(L, -1) then
      Dialog.SubtitleGroupId := 1
    else
      Dialog.SubtitleGroupId := lua_tointeger(L, -1);
    lua_pop(L, 1);

    lua_getfield(L, -1, 'wav_subtitle_margin_left');
    if lua_isnil(L, -1) then
      Dialog.SubtitleMarginLeft := 0
    else
      Dialog.SubtitleMarginLeft := lua_tointeger(L, -1);
    lua_pop(L, 1);

    lua_getfield(L, -1, 'wav_subtitle_margin_right');
    if lua_isnil(L, -1) then
      Dialog.SubtitleMarginRight := 0
    else
      Dialog.SubtitleMarginRight := lua_tointeger(L, -1);
    lua_pop(L, 1);

    if DialogBoxParamW(hInstance, 'SETTINGDIALOG', Parent, @WndProcTrampoline,
      LPARAM(Dialog)) = idOk then
    begin
      SL := TStringList.Create;
      try
        SL.Add('local P = {}');
        if Dialog.LipsyncGenerate then
          SL.Add('P.wav_lipsync = 1');
        if Dialog.LipsyncGroupId <> 1 then
          SL.Add('P.wav_lipsync_group = ' + IntToStr(Dialog.LipsyncGroupId));
        if Dialog.LipsyncOffset <> 0 then
          SL.Add('P.wav_lipsync_offset = ' + IntToStr(Dialog.LipsyncOffset));

        if Dialog.MPSliderGenerate then
          SL.Add('P.wav_mpslider = ' + IntToStr(Dialog.MPSliderNumber + 1));
        if Dialog.MPSliderGroupId <> 1 then
          SL.Add('P.wav_mpslider_group = ' + IntToStr(Dialog.MPSliderGroupId));
        if Dialog.MPSliderMarginLeft <> 0 then
          SL.Add('P.wav_mpslider_margin_left = ' + IntToStr(Dialog.MPSliderMarginLeft));
        if Dialog.MPSliderMarginRight <> 0 then
          SL.Add('P.wav_mpslider_margin_right = ' +
            IntToStr(Dialog.MPSliderMarginRight));


        if Dialog.SubtitleGenerate then
          SL.Add('P.wav_subtitle = 2');
        if Dialog.SubtitleEncoding <> 0 then
          SL.Add('P.wav_subtitle_encoding = "utf8"');
        if Dialog.SubtitleGroupId <> 1 then
          SL.Add('P.wav_subtitle_group = ' + IntToStr(Dialog.SubtitleGroupId));
        if Dialog.SubtitleMarginLeft <> 0 then
          SL.Add('P.wav_subtitle_margin_left = ' + IntToStr(Dialog.SubtitleMarginLeft));
        if Dialog.SubtitleMarginRight <> 0 then
          SL.Add('P.wav_subtitle_margin_right = ' +
            IntToStr(Dialog.SubtitleMarginRight));

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

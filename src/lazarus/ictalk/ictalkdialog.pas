unit ICTalkDialog;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Lua;

function ShowICTalkDialog(L: Plua_State): integer;

implementation

{$R *.res}

uses
  Windows, SysUtils, Classes, IniFiles, CeVIOControl, Util;

const
  ID_EDIT_TALK_MESSAGE = 0;
  ID_LISTBOX_CASTS = 3;
  ID_TRACKBAR_SPEED = 4;
  ID_TRACKBAR_TONE = 5;
  ID_TRACKBAR_TONESCALE = 6;
  ID_TRACKBAR_ALPHA = 7;
  ID_LABEL_SPEED = 8;
  ID_LABEL_TONE = 9;
  ID_LABEL_TONESCALE = 10;
  ID_LABEL_ALPHA = 11;
  ID_CHECKBOX_EXPORTSUBTITLE = 100;
  ID_BUTTON_LISTEN = 1000;

type
  TFileNames = array of WideString;

  { TICTalkDialog }

  TICTalkDialog = class
  protected
    function WndProc(Hwnd: HWND; Message: UINT; WP: WPARAM; LP: LPARAM): LRESULT;
    function EditSubClassProc(Hwnd: HWND; Message: UINT; WP: WPARAM;
      LP: LPARAM): LRESULT;
  private
    FExportedFileNames: TFileNames;
    FFFileNameFormatType: TFileNameFormatType;
    FFileNameFormatType: TFileNameFormatType;
    FWindow: THandle;
    FIni: TMemIniFile;
    FEditOriginalProc: WNDPROC;

    function GetControl(ControlId: integer): THandle;

    function GetAlpha: integer;
    function GetCastName: WideString;
    function GetCastIndex: integer;
    function GetExportSubtitleText: boolean;
    function GetIniFileName: WideString;
    function GetSpeed: integer;
    function GetTone: integer;
    function GetToneScale: integer;
    function GetTalkMessage: WideString;
    procedure InitDialog();
    procedure FinalDialog();
    procedure ClickListen();
    procedure ChangeCast();
    procedure ChangeTrackBar();
    procedure SetAlpha(AValue: integer);
    procedure SetCastIndex(AValue: integer);
    procedure SetExportSubtitleText(AValue: boolean);
    procedure SetSpeed(AValue: integer);
    procedure SetTalkMessage(AValue: WideString);
    procedure SetTone(AValue: integer);
    procedure SetToneScale(AValue: integer);
    procedure UpdateTrackBarLabels();
    procedure ExportWave();
  public
    constructor Create();
    destructor Destroy(); override;
    property TalkMessage: WideString read GetTalkMessage write SetTalkMessage;
    property CastIndex: integer read GetCastIndex write SetCastIndex;
    property CastName: WideString read GetCastName;
    property Speed: integer read GetSpeed write SetSpeed;
    property Tone: integer read GetTone write SetTone;
    property ToneScale: integer read GetToneScale write SetToneScale;
    property Alpha: integer read GetAlpha write SetAlpha;
    property IniFileName: WideString read GetIniFileName;
    property ExportSubtitleText: boolean read GetExportSubtitleText
      write SetExportSubtitleText;
    property FileNameFormatType: TFileNameFormatType
      read FFileNameFormatType write FFFileNameFormatType;
    property ExportedFileNames: TFileNames read FExportedFileNames;
  end;

var
  Dialog: TICTalkDialog;

function WndProcTrampoline(Hwnd: HWND; Message: UINT; WP: WPARAM;
  LP: LPARAM): LRESULT; stdcall;
begin
  try
    Result := Dialog.WndProc(Hwnd, Message, WP, LP);
  except
    on E: Exception do
      MessageBoxW(Hwnd, PWideChar(
        '予期しない例外が発生しました。'#13#10#13#10 + WideString(E.Message)),
        'Instant CTalk', MB_ICONERROR);
  end;
end;

function EditSubClassTrampoline(Hwnd: HWND; Message: UINT; WP: WPARAM;
  LP: LPARAM): LRESULT; stdcall;
begin
  try
    Result := Dialog.EditSubClassProc(Hwnd, Message, WP, LP);
  except
    on E: Exception do
      MessageBoxW(Hwnd, PWideChar(
        '予期しない例外が発生しました。'#13#10#13#10 + WideString(E.Message)),
        'Instant CTalk', MB_ICONERROR);
  end;
end;

{ TICTalkDialog }

function TICTalkDialog.GetControl(ControlId: integer): THandle;
begin
  Result := GetDlgItem(FWindow, ControlId);
end;

function TICTalkDialog.GetCastName: WideString;
var
  h: THandle;
  Idx: integer;
begin
  h := GetControl(ID_LISTBOX_CASTS);
  Idx := CastIndex;
  SetLength(Result, SendMessageW(h, LB_GETTEXTLEN, Idx, 0) + 1);
  SendMessageW(h, LB_GETTEXT, Idx, {%H-}LPARAM(@Result[1]));
  SetLength(Result, Length(Result) - 1);
end;

function TICTalkDialog.GetAlpha: integer;
begin
  Result := 100 - SendMessageW(GetControl(ID_TRACKBAR_ALPHA), TBM_GETPOS, 0, 0);
end;

function TICTalkDialog.WndProc(Hwnd: HWND; Message: UINT; WP: WPARAM;
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
    WM_KEYDOWN:
    begin
      OutputDebugString('EditSubClassProc');
      if (WP = VK_RETURN) and (GetAsyncKeyState(VK_CONTROL) < 0) then
      begin
        PostMessage(FWindow, WM_COMMAND, idOk, 0);
        Exit;
      end;
    end;
    WM_COMMAND:
    begin
      case LOWORD(WP) of
        idOk:
        begin
          ExportWave();
          EndDialog(Hwnd, idOk);
        end;
        idCancel: EndDialog(Hwnd, idCancel);
        ID_BUTTON_LISTEN: ClickListen();
        ID_LISTBOX_CASTS: if HIWORD(wp) = CBN_SELCHANGE then
            ChangeCast();
      end;
      Result := 1;
    end;
    WM_VSCROLL:
      if (GetControl(ID_TRACKBAR_SPEED) = THandle(LP)) or
        (GetControl(ID_TRACKBAR_TONE) = THandle(LP)) or
        (GetControl(ID_TRACKBAR_TONESCALE) = THandle(LP)) or
        (GetControl(ID_TRACKBAR_ALPHA) = THandle(LP)) then
        ChangeTrackBar();
    WM_DESTROY:
    begin
      FinalDialog();
      Result := 1;
    end;
    else;
  end;
end;

function TICTalkDialog.EditSubClassProc(Hwnd: HWND; Message: UINT;
  WP: WPARAM; LP: LPARAM): LRESULT;
begin
  case Message of
    WM_GETDLGCODE:
    begin
      if (LP <> 0) and (WP = VK_RETURN) then
      begin
        Result := DLGC_WANTALLKEYS;
        Exit;
      end;
    end;
    WM_KEYDOWN:
    begin
      if WP = VK_RETURN then
      begin
        if GetAsyncKeyState(VK_CONTROL) < 0 then
          PostMessage(FWindow, WM_COMMAND, idOk, 0)
        else
          PostMessage(FWindow, WM_COMMAND, ID_BUTTON_LISTEN, 0);
        Result := 0;
        Exit;
      end;
    end;
  end;
  Result := CallWindowProc(Windows.WNDPROC(FEditOriginalProc), Hwnd, Message, WP, LP);
end;

function TICTalkDialog.GetCastIndex: integer;
begin
  Result := SendMessageW(GetControl(ID_LISTBOX_CASTS), LB_GETCURSEL, 0, 0);
end;

function TICTalkDialog.GetExportSubtitleText: boolean;
begin
  Result := SendMessageW(GetControl(ID_CHECKBOX_EXPORTSUBTITLE), BM_GETCHECK, 0, 0) =
    BST_CHECKED;
end;

function TICTalkDialog.GetSpeed: integer;
begin
  Result := 100 - SendMessageW(GetControl(ID_TRACKBAR_SPEED), TBM_GETPOS, 0, 0);
end;

function TICTalkDialog.GetTone: integer;
begin
  Result := 100 - SendMessageW(GetControl(ID_TRACKBAR_TONE), TBM_GETPOS, 0, 0);
end;

function TICTalkDialog.GetToneScale: integer;
begin
  Result := 100 - SendMessageW(GetControl(ID_TRACKBAR_TONESCALE), TBM_GETPOS, 0, 0);
end;

procedure TICTalkDialog.InitDialog();
var
  h: THandle;
  SL: TStringList;
  I, Idx: integer;
  started: boolean;
  SelectedCast, WS: WideString;
begin
  h := GetControl(ID_EDIT_TALK_MESSAGE);
  FEditOriginalProc := Windows.WNDPROC(GetWindowLongPtr(h, GWL_WNDPROC));
  SetWindowLong(h, GWL_WNDPROC, LONG(@EditSubClassTrampoline));

  try
    started := IsHostStarted();
  except
    raise Exception.Create('CeVIO が正しくインストールされていません。');
  end;
  if not started then
    raise Exception.Create('CeVIO が起動されていません。'#13#10'Instant CTalk を使用するには先に CeVIO を起動させてください。');

  SetWindowTextW(FWindow, 'Instant CTalk');
  h := GetControl(ID_LISTBOX_CASTS);
  SL := GetAvailableCasts();
  try
    Idx := 0;
    SelectedCast := WideString(FIni.ReadString('Generic', 'SelectedCast', ''));
    for I := 0 to SL.Count - 1 do begin
      WS := WideString(SL[I]);
      SendMessageW(h, LB_ADDSTRING, 0, {%H-}LPARAM(PWideChar(WS)));
      if WS = SelectedCast then
        Idx := I;
    end;
    if SL.Count > 0 then
      CastIndex := Idx;
  finally
    SL.Free;
  end;

  SendMessageW(GetControl(ID_TRACKBAR_SPEED), TBM_SETRANGE, 1, MAKELPARAM(0, 100));
  SendMessageW(GetControl(ID_TRACKBAR_TONE), TBM_SETRANGE, 1, MAKELPARAM(0, 100));
  SendMessageW(GetControl(ID_TRACKBAR_TONESCALE), TBM_SETRANGE, 1, MAKELPARAM(0, 100));
  SendMessageW(GetControl(ID_TRACKBAR_ALPHA), TBM_SETRANGE, 1, MAKELPARAM(0, 100));

  ExportSubtitleText := FIni.ReadBool('Generic', 'ExportSubtitle', True);
  ChangeCast();
end;

procedure TICTalkDialog.FinalDialog();
begin
  FIni.WriteBool('Generic', 'ExportSubtitle', ExportSubtitleText);
  FIni.WriteString('Generic', 'SelectedCast', String(CastName));
  SetWindowLong(GetControl(ID_EDIT_TALK_MESSAGE), GWL_WNDPROC, LONG(FEditOriginalProc));
end;

procedure TICTalkDialog.ClickListen();
var
  s: WideString;
begin
  s := TalkMessage;
  if s <> '' then
    Speak(TalkMessage, CastName, 50, Speed, Tone, ToneScale, Alpha);
end;

procedure TICTalkDialog.ChangeCast();
var
  s: string;
begin
  s := string(CastName);
  Speed := FIni.ReadInteger(s, 'Speed', 50);
  Tone := FIni.ReadInteger(s, 'Tone', 50);
  ToneScale := FIni.ReadInteger(s, 'ToneScale', 50);
  Alpha := FIni.ReadInteger(s, 'Alpha', 50);
  UpdateTrackBarLabels();
  ClickListen();
end;

procedure TICTalkDialog.ChangeTrackBar();
begin
  UpdateTrackBarLabels();
  ClickListen();
end;

procedure TICTalkDialog.SetTone(AValue: integer);
begin
  SendMessageW(GetControl(ID_TRACKBAR_TONE), TBM_SETPOS, 1, 100 -
    Max(0, Min(100, AValue)));
end;

procedure TICTalkDialog.SetToneScale(AValue: integer);
begin
  SendMessageW(GetControl(ID_TRACKBAR_TONESCALE), TBM_SETPOS, 1, 100 -
    Max(0, Min(100, AValue)));
end;

procedure TICTalkDialog.UpdateTrackBarLabels();
var
  s: string;
begin
  s := string(CastName);
  FIni.WriteInteger(s, 'Speed', Speed);
  FIni.WriteInteger(s, 'Tone', Tone);
  FIni.WriteInteger(s, 'ToneScale', ToneScale);
  FIni.WriteInteger(s, 'Alpha', Alpha);
  SetWindowText(GetControl(ID_LABEL_SPEED), PChar(IntToStr(Speed)));
  SetWindowText(GetControl(ID_LABEL_TONE), PChar(IntToStr(Tone)));
  SetWindowText(GetControl(ID_LABEL_TONESCALE), PChar(IntToStr(ToneScale)));
  SetWindowText(GetControl(ID_LABEL_ALPHA), PChar(IntToStr(Alpha)));
end;

procedure TICTalkDialog.ExportWave();
var
  S: WideString;
  SJIS: ShiftJISString;
  WS: TWideFileStream;
begin
  S := SuggestNextFileName(FileNameFormatType, Now(), TalkMessage, CastName);
  if not ExportWaveFile(S + '.wav', TalkMessage, CastName, 50, Speed,
    Tone, ToneScale, Alpha) then
    raise Exception.Create('failed to export a wave file');
  SetLength(FExportedFileNames, 1);
  FExportedFileNames[0] := S + '.wav';
  try
    if ExportSubtitleText then
    begin
      SJIS := ShiftJISString(TalkMessage);
      WS := TWideFileStream.Create(S + '.txt', fmCreate);
      try
        WS.WriteBuffer(SJIS[1], Length(SJIS));
      finally
        WS.Free;
      end;
      SetLength(FExportedFileNames, 2);
      FExportedFileNames[1] := S + '.txt';
    end;
  except
    DeleteFile(S + '.wav');
    raise;
  end;
end;

function TICTalkDialog.GetIniFileName: WideString;
begin
  SetLength(Result, MAX_PATH + 1);
  GetModuleFileNameW(hInstance, @Result[1], Length(Result));
  Result := ChangeFileExt(PWideChar(Result), WideString('.ini'));
end;

function TICTalkDialog.GetTalkMessage: WideString;
var
  h: THandle;
begin
  h := GetControl(ID_EDIT_TALK_MESSAGE);
  SetLength(Result, GetWindowTextLengthW(h) + 1);
  GetWindowTextW(h, @Result[1], Length(Result));
  SetLength(Result, Length(Result) - 1);
end;

procedure TICTalkDialog.SetAlpha(AValue: integer);
begin
  SendMessageW(GetControl(ID_TRACKBAR_ALPHA), TBM_SETPOS, 1, 100 -
    Max(0, Min(100, AValue)));
end;

procedure TICTalkDialog.SetCastIndex(AValue: integer);
begin
  if SendMessageW(GetControl(ID_LISTBOX_CASTS), LB_SETCURSEL, AValue, 0) = LB_ERR then
    raise Exception.Create('キャスト選択に失敗しました');
end;

procedure TICTalkDialog.SetExportSubtitleText(AValue: boolean);
begin
  if AValue then
    SendMessageW(GetControl(ID_CHECKBOX_EXPORTSUBTITLE), BM_SETCHECK, BST_CHECKED, 0)
  else
    SendMessageW(GetControl(ID_CHECKBOX_EXPORTSUBTITLE), BM_SETCHECK, BST_UNCHECKED, 0);
end;

procedure TICTalkDialog.SetSpeed(AValue: integer);
begin
  SendMessageW(GetControl(ID_TRACKBAR_SPEED), TBM_SETPOS, 1, 100 -
    Max(0, Min(100, AValue)));
end;

procedure TICTalkDialog.SetTalkMessage(AValue: WideString);
begin
  SetWindowTextW(GetControl(ID_EDIT_TALK_MESSAGE), PWideChar(AValue));
end;

constructor TICTalkDialog.Create();
var
  ws: TWideFileStream;
  sl: TStringList;
begin
  inherited Create();

  FIni := TMemIniFile.Create('');
  try
    sl := TStringList.Create;
    try
      ws := TWideFileStream.Create(IniFileName, fmOpenRead);
      try
        sl.LoadFromStream(ws);
      finally
        ws.Free;
      end;
      FIni.SetStrings(sl);
    finally
      sl.Free;
    end;
  except
  end;

  SetLength(FExportedFileNames, 0);
end;

destructor TICTalkDialog.Destroy();
var
  ws: TWideFileStream;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    FIni.GetStrings(sl);
    ws := TWideFileStream.Create(IniFileName, fmCreate);
    try
      sl.SaveToStream(ws);
    finally
      ws.Free;
    end;
  finally
    sl.Free;
  end;
  FIni.Free;

  inherited Destroy();
end;

function ShowICTalkDialog(L: Plua_State): integer;
var
  Parent: THandle;
  I, FileNameFormatType: integer;
  fn: TFileNames;
  SJIS: ShiftJISString;
begin
  lua_getfield(L, -1, 'parent');
  Parent := THandle(lua_tointeger(L, -1));
  lua_getfield(L, -2, 'format');
  FileNameFormatType := lua_tointeger(L, -1);
  lua_pop(L, 2);

  lua_newtable(L);
  Result := 1;
  Dialog := TICTalkDialog.Create();
  try
    case FileNameFormatType of
      0: Dialog.FileNameFormatType := ftMsg;
      1: Dialog.FileNameFormatType := ftDTMsg;
      2: Dialog.FileNameFormatType := ftCastMsg;
      3: Dialog.FileNameFormatType := ftDTCastMsg;
      else
        Dialog.FileNameFormatType := ftDTCastMsg;
    end;
    lua_newtable(L);
    if DialogBoxParamW(hInstance, 'ICTALKDIALOG', Parent, @WndProcTrampoline,
      LPARAM(Dialog)) = idOk then
    begin
      fn := Dialog.ExportedFileNames;
      for I := 0 to Length(fn) - 1 do
      begin
        SJIS := ShiftJISString(fn[I]);
        lua_pushlstring(L, @SJIS[1], Length(SJIS));
        lua_rawseti(L, -2, I + 1);
      end;
    end;
    lua_setfield(L, -2, 'files');
  finally
    FreeAndNil(Dialog);
  end;
end;

end.

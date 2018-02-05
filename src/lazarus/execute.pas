unit Execute;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Classes;

type
  { TSendEditingImageStateToExEdit }

  TSendEditingImageStateToExEdit = class(TThread)
  private
    FWindow: THandle;
    FState: UTF8String;
  protected
    procedure Execute; override;
  public
    constructor Create(Window: THandle; S: UTF8String);
  end;

  { TExportFaviewSlider }

  TExportFaviewSlider = class(TThread)
  private
    FWindow: THandle;
    FFilePath: UTF8String;
    FSliderName: UTF8String;
    FNames: array of UTF8String;
    FValues: array of UTF8String;
    FSelectedIndex: integer;
    procedure Copy();
    procedure CopySlider();
    function SliderToLuaScript(): UTF8String;
    procedure ExportByCSV(FileName: WideString);
    procedure ExportByANM(FileName: WideString);
    procedure ExportSlider();
    function GetReadableSliderName(): UTF8String;
  protected
    procedure Execute; override;
  public
    constructor Create(Window: THandle; FilePath: UTF8String;
      SliderName: UTF8String; Names: UTF8String; Values: UTF8String;
      SelectedIndex: integer);
  end;

implementation

uses
  Windows, SysUtils, Find, Util;

{ TSendEditingImageStateToExEdit }

procedure TSendEditingImageStateToExEdit.Execute;
var
  w: TExEditWindow;
  pw: TExEditParameterDialog;
  ws: WideString;
  n: DWORD;
begin
  ws := WideString(FState);
  if FindExEditParameterDialog(pw) then
  begin
    SendMessageW(pw.Edit, WM_SETTEXT, 0, {%H-}LPARAM(PWideChar(ws)));
    Exit;
  end;
  if FindExEditWindow(w) and (w.Config <> 0) then
  begin
    PostMessage(w.Config, BM_CLICK, 0, 0);
    n := GetTickCount() + 3000;
    while n > GetTickCount() do
    begin
      Sleep(0);
      if FindExEditParameterDialog(pw) then
      begin
        SendMessageW(pw.Edit, WM_SETTEXT, 0, {%H-}LPARAM(PWideChar(ws)));
        PostMessage(pw.OK, BM_CLICK, 0, 0);
        Exit;
      end;
    end;
  end;
  MessageBoxW(FWindow, 'Target not found.', 'PSDToolKit', MB_ICONERROR);
end;

constructor TSendEditingImageStateToExEdit.Create(Window: THandle; S: UTF8String);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FWindow := Window;
  FState := S;
end;

{ TExportFaviewSlider }

procedure TExportFaviewSlider.Copy();
begin
  if not CopyToClipboard(FWindow, WideString(FValues[FSelectedIndex])) then
    MessageBox(FWindow, 'could not open clipboard', 'PSDToolKit', MB_ICONERROR);
end;

procedure TExportFaviewSlider.CopySlider();
begin
  if not CopyToClipboard(FWindow, WideString(SliderToLuaScript())) then
    MessageBox(FWindow, 'could not open clipboard', 'PSDToolKit', MB_ICONERROR);
end;

function TExportFaviewSlider.SliderToLuaScript(): UTF8String;
var
  MinIndex, MaxIndex, I: integer;
begin
  MinIndex := Max(Low(FNames), Low(FValues));
  MaxIndex := Min(High(FNames), High(FValues));
  Result := Format('--track0:%s,1,%d,1,1'#13#10,
    [GetReadableSliderName(), MaxIndex - MinIndex + 1]);
  Result := Result + 'local values = {'#13#10;
  for I := MinIndex to MaxIndex do
    if (Length(FValues[I]) > 2) and (FValues[I][2] = '.') then
      Result := Result + '  ' + StringifyForLua(FValues[I]) + ',' + #13#10
    else
      Result := Result + '  ' + StringifyForLua(FValues[I]) + ', -- ' +
        StringifyForLua(FNames[I]) + #13#10;
  Result := Result + '  nil'#13#10'}'#13#10;
  Result := Result + 'PSD:addstate(values[obj.track0])'#13#10;
end;

procedure TExportFaviewSlider.ExportByCSV(FileName: WideString);
var
  I: integer;
  F: TFileStreamW;
begin
  F := TFileStreamW.Create(FileName);
  try
    for I := Max(Low(FNames), Low(FValues)) to Min(High(FNames), High(FValues)) do
    begin
      WriteRawString(F, StringifyForCSV(FNames[I]));
      WriteRawString(F, ',');
      WriteRawString(F, StringifyForCSV(FValues[I]));
      WriteRawString(F, #13#10);
    end;
  finally
    F.Free;
  end;
end;

procedure TExportFaviewSlider.ExportByANM(FileName: WideString);
var
  S: UTF8String;
  SS: ShiftJISString;
  F: TFileStreamW;
begin
  F := TFileStreamW.Create(FileName);
  try
    S := SliderToLuaScript();
    SS := S;
    WriteRawString(F, SS);
  finally
    F.Free;
  end;
end;

procedure TExportFaviewSlider.ExportSlider();
var
  S: UTF8String;
  FileName: WideString;
begin
  S := Sanitize(ChangeFileExt(ExtractFileName(Token('|', FFilePath)), '') +
    '-' + FSliderName + '.anm');
  S := StringReplace(S, '\', '_', [rfReplaceAll]);
  S := StringReplace(S, '@', '_', [rfReplaceAll]);
  FileName := SaveDialog(FWindow, 'Save SimpleView slider as file',
    'AviUtl animation effect script(*.anm)'#0'*.anm'#0'CSV file(*.csv)'#0'*.csv'#0#0,
    1, WideString(S), 'anm', ExtractFilePath(GetDLLName()) + '..');
  case LowerCase(ExtractFileExt(FileName)) of
    '.anm': ExportByANM(FileName);
    '.csv': ExportByCSV(FileName);
  end;
end;

function TExportFaviewSlider.GetReadableSliderName(): UTF8String;
var
  PC: PChar;
begin
  PC := StrRScan(PChar(FSliderName), '\');
  if PC <> nil then
  begin
    Inc(PC);
    Result := PC;
  end
  else
    Result := FSliderName;
end;

procedure TExportFaviewSlider.Execute;
const
  HWND_MESSAGE = HWND(-3);
var
  I: integer;
  PW: TExEditMultiParameterDialog;
  FoundDialog: boolean;
  Menu, DummyWindow: THandle;
  WS: WideString;
  Pt: TPoint;
  wc: WNDCLASS;
  ForeignThreadId: DWORD;
begin
  wc.style := 0;
  wc.lpfnWndProc := @DefWindowProc;
  wc.cbClsExtra := 0;
  wc.cbWndExtra := 0;
  wc.hInstance := hInstance;
  wc.hIcon := 0;
  wc.hCursor := 0;
  wc.hbrBackground := 0;
  wc.lpszMenuName := nil;
  wc.lpszClassName := 'DummyWindow';
  RegisterClass(wc);
  DummyWindow := CreateWindow('DummyWindow', nil, WS_OVERLAPPEDWINDOW,
    CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
    HWND_MESSAGE, 0, hInstance, nil);
  try
    FoundDialog := FindExEditMultiParameterDialog(PW);
    Menu := CreatePopupMenu();
    try
      WS := '"' + WideString(FNames[FSelectedIndex]) +
        '" をクリップボードにコピー';
      AppendMenuW(Menu, MF_ENABLED or MF_STRING, 1000, PWideChar(WS));
      AppendMenuW(Menu, MF_ENABLED or MF_SEPARATOR, 0, nil);

      if FoundDialog and (Length(PW.Caption) > 0) then
      begin
        for I := Low(PW.Caption) to High(PW.Caption) do
        begin
          WS := '"' + WideString(FNames[FSelectedIndex]) + '" を「' +
            PW.Caption[I] + '」に割り当て';
          AppendMenuW(Menu, MF_ENABLED or MF_STRING, 2000 + I, PWideChar(WS));
        end;
        AppendMenuW(Menu, MF_ENABLED or MF_SEPARATOR, 0, nil);
      end;

      WS := 'スライダー "' + WideString(GetReadableSliderName()) +
        '" 全体をクリップボードにコピー';
      AppendMenuW(Menu, MF_ENABLED or MF_STRING, 1001, PWideChar(WS));
      WS := 'スライダー "' + WideString(GetReadableSliderName()) +
        '" 全体をファイルにエクスポート';
      AppendMenuW(Menu, MF_ENABLED or MF_STRING, 1002, PWideChar(WS));

      GetCursorPos(Pt);

      ForeignThreadId := GetWindowThreadProcessId(FWindow, nil);
      AttachThreadInput(GetCurrentThreadId(), ForeignThreadId, True);
      try
        SetForegroundWindow(DummyWindow);
      finally
        AttachThreadInput(GetCurrentThreadId(), ForeignThreadId, False);
      end;
      I := integer(TrackPopupMenu(Menu, TPM_TOPALIGN or TPM_LEFTALIGN or
        TPM_RETURNCMD or TPM_RIGHTBUTTON, Pt.x, Pt.y, 0, DummyWindow, nil));
      case I of
        0:
        begin
          OutputDebugString('Return 0');
          Exit;
        end;
        1000: Copy();
        1001: CopySlider();
        1002: ExportSlider();
        else
        begin
          if (0 <= I - 2000) and (I - 2000 < Length(PW.Caption)) then
          begin
            WS := WideString(FValues[FSelectedIndex]);
            SendMessageW(pw.Edit[I - 2000], WM_SETTEXT, 0, {%H-}LPARAM(PWideChar(WS)));
          end;
        end;
      end;
    finally
      DestroyMenu(Menu);
    end;
  finally
    DestroyWindow(DummyWindow);
  end;
end;

constructor TExportFaviewSlider.Create(Window: THandle; FilePath: UTF8String;
  SliderName: UTF8String; Names: UTF8String; Values: UTF8String; SelectedIndex: integer);
var
  I: integer;
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FWindow := Window;
  FFilePath := FilePath;
  FSliderName := SliderName;
  I := 0;
  while Names <> '' do
  begin
    SetLength(FNames, I + 1);
    FNames[I] := Token(#0, Names);
    Inc(I);
  end;
  I := 0;
  while Values <> '' do
  begin
    SetLength(FValues, I + 1);
    FValues[I] := Token(#0, Values);
    Inc(I);
  end;
  FSelectedIndex := SelectedIndex;
end;

end.

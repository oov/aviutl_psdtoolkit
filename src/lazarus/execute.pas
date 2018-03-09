unit Execute;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Classes;

type
  TSiblingIndices = array of integer;
  { TSendEditingImageStateToExEdit }

  TSendEditingImageStateToExEdit = class(TThread)
  private
    FWindow: THandle;
    FFilePath: UTF8String;
    FState: UTF8String;
  protected
    procedure Execute; override;
  public
    constructor Create(const Window: THandle; const FilePath, State: UTF8String);
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

  { TExportLayerNames }

  TExportLayerNames = class(TThread)
  private
    FWindow: THandle;
    FFilePath: UTF8String;
    FNames: array of UTF8String;
    FValues: array of UTF8String;
    FSelectedIndex: integer;
    function FindSiblings(const S: UTF8String): TSiblingIndices;
    function GetParentLayerName(const S: UTF8String): UTF8String;
    function ToReadable(const S: UTF8String): UTF8String;
    procedure Copy();
    procedure CopySiblings();
    function SiblingsToLuaScript(): UTF8String;
    procedure ExportByCSV(FileName: WideString);
    procedure ExportByANM(FileName: WideString);
    procedure ExportSiblings();
  protected
    procedure Execute; override;
  public
    constructor Create(Window: THandle; FilePath: UTF8String;
      Names, Values: UTF8String; SelectedIndex: integer);
  end;

implementation

uses
  Windows, SysUtils, Find, Util;

const
  HWND_MESSAGE = HWND(-3);

{ TExportLayerNames }

function TExportLayerNames.FindSiblings(const S: UTF8String): TSiblingIndices;
var
  Parent: UTF8String;
  I, N, ParentLen: integer;
begin
  Parent := GetParentLayerName(S) + '/';
  ParentLen := Length(Parent);
  N := StrCount(PChar(Parent), '/');
  for I := Low(FNames) to High(FNames) do
  begin
    if Length(FNames[I]) <= ParentLen then
      continue;
    if not CompareMem(@Parent[1], @FNames[I][1], ParentLen) then
      continue;
    if StrCount(PChar(FNames[I]), '/') <> N then
      continue;
    SetLength(Result, Length(Result) + 1);
    Result[Length(Result) - 1] := I;
  end;
end;

function TExportLayerNames.GetParentLayerName(const S: UTF8String): UTF8String;
var
  P, PC: PChar;
begin
  P := PChar(S);
  PC := StrRScan(P, '/');
  if PC = nil then
  begin
    Result := '';
    Exit;
  end;
  Result := System.Copy(S, 1, PC - P);
end;

function TExportLayerNames.ToReadable(const S: UTF8String): UTF8String;
var
  P, PC: PChar;
begin
  P := PChar(S);
  PC := StrRScan(P, '/');
  if PC = nil then
  begin
    Result := DecodePercentEncoding(S);
    Exit;
  end;
  Result := DecodePercentEncoding(UTF8String(PC + 1));
end;

procedure TExportLayerNames.Copy();
begin
  if not CopyToClipboard(FWindow, WideString(FValues[FSelectedIndex])) then
    MessageBox(FWindow, 'could not open clipboard', 'PSDToolKit', MB_ICONERROR);
end;

procedure TExportLayerNames.CopySiblings();
begin
  if not CopyToClipboard(FWindow, WideString(SiblingsToLuaScript())) then
    MessageBox(FWindow, 'could not open clipboard', 'PSDToolKit', MB_ICONERROR);
end;

function TExportLayerNames.SiblingsToLuaScript(): UTF8String;
var
  Siblings: TSiblingIndices;
  I: integer;
begin
  Siblings := FindSiblings(FNames[FSelectedIndex]);
  Result := Format('--track0:%s,0,%d,0,1'#13#10,
    [StringReplace(ToReadable(GetParentLayerName(FNames[FSelectedIndex])),
    ',', '_', [rfReplaceAll]), Length(Siblings)]);
  Result := Result + 'local values = {'#13#10;
  for I := Low(Siblings) to High(Siblings) do
    Result := Result + '  ' + StringifyForLua(FValues[Siblings[I]]) + ','#13#10;
  Result := Result + '}'#13#10;
  Result := Result + 'PSD:addstate(values, obj.track0)'#13#10;
end;

procedure TExportLayerNames.ExportByCSV(FileName: WideString);
var
  I: integer;
  Siblings: TSiblingIndices;
  F: TFileStreamW;
begin
  Siblings := FindSiblings(FNames[FSelectedIndex]);
  F := TFileStreamW.Create(FileName);
  try
    for I := Low(Siblings) to High(Siblings) do
    begin
      WriteRawString(F, StringifyForCSV(ToReadable(FNames[Siblings[I]])));
      WriteRawString(F, ',');
      WriteRawString(F, StringifyForCSV(FValues[Siblings[I]]));
      WriteRawString(F, #13#10);
    end;
  finally
    F.Free;
  end;
end;

procedure TExportLayerNames.ExportByANM(FileName: WideString);
var
  SS: ShiftJISString;
  F: TFileStreamW;
begin
  F := TFileStreamW.Create(FileName);
  try
    SS := SiblingsToLuaScript();
    WriteRawString(F, SS);
  finally
    F.Free;
  end;
end;

procedure TExportLayerNames.ExportSiblings();
var
  S: UTF8String;
  FileName: WideString;
begin
  S := Sanitize(ChangeFileExt(ExtractFileName(Token('|', FFilePath)), '') +
    '-' + ToReadable(GetParentLayerName(FNames[FSelectedIndex])) + '.anm');
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

procedure TExportLayerNames.Execute;
var
  I: integer;
  PW: TExEditMultiParameterDialog;
  FoundDialog: boolean;
  Menu, DummyWindow: THandle;
  Name, WS: WideString;
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
      Name := WideString(ToReadable(FNames[FSelectedIndex]));
      WS := '"' + Name + '" をクリップボードにコピー';
      AppendMenuW(Menu, MF_ENABLED or MF_STRING, 1000, PWideChar(WS));
      AppendMenuW(Menu, MF_ENABLED or MF_SEPARATOR, 0, nil);

      if FoundDialog and (Length(PW.Caption) > 0) then
      begin
        for I := Low(PW.Caption) to High(PW.Caption) do
        begin
          WS := '"' + Name + '" を「' + PW.Caption[I] + '」に割り当て';
          AppendMenuW(Menu, MF_ENABLED or MF_STRING, 2000 + I, PWideChar(WS));
        end;
        AppendMenuW(Menu, MF_ENABLED or MF_SEPARATOR, 0, nil);
      end;

      WS := '"' + Name +
        '" と同じ階層のレイヤー全てをクリップボードにコピー';
      AppendMenuW(Menu, MF_ENABLED or MF_STRING, 1001, PWideChar(WS));
      WS := '"' + Name +
        '" と同じ階層のレイヤー全てをファイルにエクスポート';
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
        0: Exit;
        1000: Copy();
        1001: CopySiblings();
        1002: ExportSiblings();
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

constructor TExportLayerNames.Create(Window: THandle; FilePath: UTF8String;
  Names, Values: UTF8String; SelectedIndex: integer);
var
  I: integer;
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FWindow := Window;
  FFilePath := FilePath;
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

function ModifyLuaString(var S: WideString; const Name: WideString;
  const Value: UTF8String): boolean;
var
  P, StartPos, EndPos: PWideChar;
begin
  Result := False;
  P := PWideChar(S);
  StartPos := StrPos(P, PWideChar(Name + '="'));
  if not Assigned(StartPos) then
    Exit;
  EndPos := StartPos + Length(Name) + 2;
  while True do
  begin
    EndPos := StrScan(EndPos, '"');
    if not Assigned(EndPos) then
      Exit;
    if (EndPos - 1)^ <> '\' then
      break;
    Inc(EndPos);
  end;
  S := Copy(S, 1, StartPos - P) + Name + '=' + WideString(StringifyForLua(Value)) +
    Copy(S, 1 + (EndPos - P) + 1, Length(S) - (EndPos - P) - 1);
  Result := True;
end;

{ TSendEditingImageStateToExEdit }

procedure TSendEditingImageStateToExEdit.Execute;
var
  W: TExEditMultiLineText;
  Src: WideString;
begin
  try
    if not FindExEditMultiLineText(W, 'ptkf="') then
      raise Exception.Create(
        '設定の送信先になるテキスト入力欄が見つかりませんでした。'#13#10#13#10'「送る」ボタンを使用するためにはPSDファイルオブジェクトのプロパティを表示し、テキスト入力欄を見える状態にしておく必要があります。');

    Src := W.EditText;
    if not ModifyLuaString(Src, 'ptkf', FFilePath) then
      raise Exception.Create(
        'テキスト入力欄から書き換え対象テキスト「ptkf="～"」が見つかりませんでした。');
    if not ModifyLuaString(Src, 'ptkl', FState) then
      raise Exception.Create(
        'テキスト入力欄から書き換え対象テキスト「ptkl="～"」が見つかりませんでした。');
    SendMessageW(W.Edit, WM_SETTEXT, 0, {%H-}LPARAM(PWideChar(Src)));
    SendMessageW(W.Window, WM_COMMAND, MAKELONG(GetDlgCtrlID(w.Edit),
      EN_CHANGE), W.Edit);
  except
    on E: Exception do
      MessageBoxW(FWindow, PWideChar(WideString(E.Message)), 'PSDToolKit', MB_ICONERROR);
  end;
end;

constructor TSendEditingImageStateToExEdit.Create(const Window: THandle;
  const FilePath, State: UTF8String);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FWindow := Window;
  FFilePath := FilePath;
  FState := State;
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
  Result := Format('--track0:%s,0,%d,0,1'#13#10,
    [StringReplace(GetReadableSliderName(), ',', '_', [rfReplaceAll]),
    MaxIndex - MinIndex + 1]);
  Result := Result + 'local values = {'#13#10;
  for I := MinIndex to MaxIndex do
    Result := Result + '  ' + StringifyForLua(FValues[I]) + ','#13#10;
  Result := Result + '}'#13#10;
  Result := Result + 'PSD:addstate(values, obj.track0)'#13#10;
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
  SS: ShiftJISString;
  F: TFileStreamW;
begin
  F := TFileStreamW.Create(FileName);
  try
    SS := SliderToLuaScript();
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
        0: Exit;
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

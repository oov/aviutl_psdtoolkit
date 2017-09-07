unit Execute;

{$mode objfpc}{$H+}

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
    FNames: UTF8String;
    FValues: UTF8String;
  protected
    procedure Execute; override;
  public
    constructor Create(Window: THandle; FilePath: UTF8String;
      SliderName: UTF8String; Names: UTF8String; Values: UTF8String);
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

procedure TExportFaviewSlider.Execute;
var
  N: integer;
  PC: PChar;
  S: UTF8String;
  SS: ShiftJISString;
  FileName: WideString;
  f: TFileStreamW;
begin
  S := Sanitize(ChangeFileExt(ExtractFileName(Token('|', FFilePath)), '') +
    '-' + FSliderName + '.anm');
  S := StringReplace(S, '\', '_', [rfReplaceAll]);
  S := StringReplace(S, '@', '_', [rfReplaceAll]);
  FileName := SaveDialog(FWindow, 'Save SimpleView slider as file',
    'AviUtl animation effect script(*.anm)'#0'*.anm'#0'CSV file(*.csv)'#0'*.csv'#0#0,
    1, WideString(S), 'anm', ExtractFilePath(GetDLLName()) + '..');
  case LowerCase(ExtractFileExt(FileName)) of
    '.anm':
    begin
      f := TFileStreamW.Create(FileName);
      try
        N := 0;
        S := FValues;
        while True do
        begin
          Token(#0, S);
          Inc(N);
          if S = '' then
            break;
        end;
        PC := StrRScan(PChar(FSliderName), '\');
        if PC <> nil then
        begin
          Inc(PC);
          S := PC;
          SS := S;
        end
        else
          SS := FSliderName;
        WriteRawString(f, Format('--track0:%s,1,%d,1,1'#13#10, [SS, N]));
        WriteRawString(f, 'local names = {');
        while True do
        begin
          S := StringifyForLua(Token(#0, FNames)) + ', ';
          SS := S;
          WriteRawString(f, SS);
          if FNames = '' then
            break;
        end;
        WriteRawString(f, 'nil}'#13#10);
        WriteRawString(f, 'local values = {');
        while True do
        begin
          S := StringifyForLua(Token(#0, FValues)) + ', ';
          SS := S;
          WriteRawString(f, SS);
          if FValues = '' then
            break;
        end;
        WriteRawString(f, 'nil}'#13#10);
        WriteRawString(f, 'PSDToolKitLib.psd:addstate(values[obj.track0])'#13#10);
      finally
        f.Free;
      end;
    end;
    '.csv':
    begin
      f := TFileStreamW.Create(FileName);
      try
        while True do
        begin
          WriteRawString(f, StringifyForCSV(Token(#0, FNames)));
          WriteRawString(f, ',');
          WriteRawString(f, StringifyForCSV(Token(#0, FValues)));
          WriteRawString(f, #13#10);
          if (FNames = '') and (FValues = '') then
            break;
        end;
      finally
        f.Free;
      end;
    end;
  end;
end;

constructor TExportFaviewSlider.Create(Window: THandle; FilePath: UTF8String;
  SliderName: UTF8String; Names: UTF8String; Values: UTF8String);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FWindow := Window;
  FFilePath := FilePath;
  FSliderName := SliderName;
  FNames := Names;
  FValues := Values;
end;

end.

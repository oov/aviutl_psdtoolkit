unit CeVIOControl;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Classes;

function IsHostStarted(): boolean;
function GetAvailableCasts(): TStringList;
procedure Speak(const Text, Cast: WideString; const Volume, Speed, Tone, ToneScale, Alpha: integer);
function ExportWaveFile(const FilePath, Text, Cast: WideString; const Volume, Speed, Tone, ToneScale, Alpha: integer): boolean;

implementation

uses
  SysUtils, ComObj;

const
  CLSID_ServiceControl: TGUID = '{C2DC23A5-A574-426E-9105-E6CFD0E1C69D}';
  IID_IServiceControlV40: TGUID = '{F13F6DA2-A97F-4F16-BF08-9AB069B5DC37}';
  CLSID_Talker: TGUID = '{901D047D-2CAD-4461-AA3C-DB8596649255}';
  IID_ITalkerV40 : TGUID = '{3C219B24-2F87-4DFE-BB20-F9211F61EB23}';

type
  IServiceControlV40 = interface(IDispatch)
    ['{F13F6DA2-A97F-4F16-BF08-9AB069B5DC37}']
    function Get_HostVersion : WideString; safecall;
    function Get_InterfaceVersion : WideString; safecall;
    function Get_IsHostStarted : WordBool; safecall;
     // StartHost :
    function StartHost(noWait:WordBool):Integer;safecall;
     // CloseHost :
    procedure CloseHost(mode:Integer);safecall;
     // HostVersion :
    property HostVersion:WideString read Get_HostVersion;
     // InterfaceVersion :
    property InterfaceVersion:WideString read Get_InterfaceVersion;
     // IsHostStarted :
    property IsHostStarted:WordBool read Get_IsHostStarted;
   end;

  ITalkerComponent = interface(IDispatch)
    ['{2B1DE3AF-B3A6-4BA6-AB41-17501FA4D7DA}']
    function Get_Id : WideString; safecall;
    function Get_Name : WideString; safecall;
    function Get_Value : LongWord; safecall;
    procedure Set_Value(const pRetVal:LongWord); safecall;
     // Id :
    property Id:WideString read Get_Id;
     // Name :
    property Name:WideString read Get_Name;
     // Value :
    property Value:LongWord read Get_Value write Set_Value;
   end;

  ITalkerComponentArray = interface(IDispatch)
    ['{3432764C-A3FE-432B-8A12-56A28FE1E281}']
    function Get_Length : Integer; safecall;
     // At :
    function At(index:Integer):ITalkerComponent;safecall;
     // ByName :
    function ByName(Name:WideString):ITalkerComponent;safecall;
     // Duplicate :
    function Duplicate:ITalkerComponentArray;safecall;
     // Length :
    property Length:Integer read Get_Length;
   end;

  IStringArray = interface(IDispatch)
    ['{1F96250E-7EA8-4FFE-8C36-C010D0921428}']
    function Get_Length : Integer; safecall;
     // At :
    function At(index:Integer):WideString;safecall;
     // Duplicate :
    function Duplicate:IStringArray;safecall;
     // Length :
    property Length:Integer read Get_Length;
   end;

  ISpeakingState = interface(IDispatch)
    ['{0091179B-02F6-4EB9-BFF5-CF727F28B2FE}']
    function Get_IsSucceeded : WordBool; safecall;
    function Get_IsCompleted : WordBool; safecall;
     // Wait :
    procedure Wait;safecall;
     // Wait_2 :
    procedure Wait_2(timeoutSeconds:Double);safecall;
     // IsSucceeded :
    property IsSucceeded:WordBool read Get_IsSucceeded;
     // IsCompleted :
    property IsCompleted:WordBool read Get_IsCompleted;
   end;

  IPhonemeData = interface(IDispatch)
    ['{987319B8-6940-39C5-9D91-B33AC5DDBFAB}']
    function Get_Phoneme : WideString; safecall;
    function Get_StartTime : Double; safecall;
    function Get_EndTime : Double; safecall;
     // Phoneme :
    property Phoneme:WideString read Get_Phoneme;
     // StartTime :
    property StartTime:Double read Get_StartTime;
     // EndTime :
    property EndTime:Double read Get_EndTime;
   end;

  IPhonemeDataArray = interface(IDispatch)
    ['{50DEA452-98D9-49B8-A103-13671BC43CAF}']
    function Get_Length : Integer; safecall;
     // At :
    function At(index:Integer):IPhonemeData;safecall;
     // Duplicate :
    function Duplicate:IPhonemeDataArray;safecall;
     // Length :
    property Length:Integer read Get_Length;
   end;

  ITalkerV40 = interface(IDispatch)
    ['{3C219B24-2F87-4DFE-BB20-F9211F61EB23}']
    function Get_Volume : LongWord; safecall;
    procedure Set_Volume(const pRetVal:LongWord); safecall;
    function Get_Speed : LongWord; safecall;
    procedure Set_Speed(const pRetVal:LongWord); safecall;
    function Get_Tone : LongWord; safecall;
    procedure Set_Tone(const pRetVal:LongWord); safecall;
    function Get_Alpha : LongWord; safecall;
    procedure Set_Alpha(const pRetVal:LongWord); safecall;
    function Get_ToneScale : LongWord; safecall;
    procedure Set_ToneScale(const pRetVal:LongWord); safecall;
    function Get_Components : ITalkerComponentArray; safecall;
    function Get_Cast : WideString; safecall;
    procedure Set_Cast(const pRetVal:WideString); safecall;
    function Get_AvailableCasts : IStringArray; safecall;
     // Speak :
    function Speak(text_:WideString):ISpeakingState;safecall;
     // Stop :
    function Stop:WordBool;safecall;
     // GetTextDuration :
    function GetTextDuration(text_:WideString):Double;safecall;
     // GetPhonemes :
    function GetPhonemes(text_:WideString):IPhonemeDataArray;safecall;
     // OutputWaveToFile :
    function OutputWaveToFile(text_:WideString;path:WideString):WordBool;safecall;
     // Volume :
    property Volume:LongWord read Get_Volume write Set_Volume;
     // Speed :
    property Speed:LongWord read Get_Speed write Set_Speed;
     // Tone :
    property Tone:LongWord read Get_Tone write Set_Tone;
     // Alpha :
    property Alpha:LongWord read Get_Alpha write Set_Alpha;
     // ToneScale :
    property ToneScale:LongWord read Get_ToneScale write Set_ToneScale;
     // Components :
    property Components:ITalkerComponentArray read Get_Components;
     // Cast :
    property Cast:WideString read Get_Cast write Set_Cast;
     // AvailableCasts :
    property AvailableCasts:IStringArray read Get_AvailableCasts;
   end;

function GetIServiceControlV40(): IServiceControlV40;
begin
  OleCheck(CreateComObject(CLSID_ServiceControl).QueryInterface(IID_IServiceControlV40, Result));
end;

function GetITalkerV40(): ITalkerV40;
begin
  OleCheck(CreateComObject(CLSID_Talker).QueryInterface(IID_ITalkerV40, Result));
end;

function IsHostStarted(): boolean;
begin
  Result := GetIServiceControlV40().IsHostStarted;
end;

function GetAvailableCasts(): TStringList;
var
  sa: IStringArray;
  i: integer;
begin
  Result := TStringList.Create();
  try
    sa := GetITalkerV40().AvailableCasts;
    for i := 0 to sa.Length - 1 do begin
      Result.Add(String(sa.At(i)));
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure Speak(const Text, Cast: WideString; const Volume, Speed, Tone, ToneScale, Alpha: integer);
var
  tk: ITalkerV40;
begin
  tk := GetITalkerV40();
  tk.Cast := Cast;
  tk.Volume := Volume;
  tk.Speed := Speed;
  tk.Tone := Tone;
  tk.ToneScale:= ToneScale;
  tk.Alpha := Alpha;
  tk.Speak(Text);
end;

function ExportWaveFile(const FilePath, Text, Cast: WideString; const Volume, Speed, Tone, ToneScale, Alpha: integer): boolean;
var
  tk: ITalkerV40;
begin
  tk := GetITalkerV40();
  tk.Cast := Cast;
  tk.Volume := Volume;
  tk.Speed := Speed;
  tk.Tone := Tone;
  tk.ToneScale:= ToneScale;
  tk.Alpha := Alpha;
  Result := tk.OutputWaveToFile(Text, FilePath);
end;

end.


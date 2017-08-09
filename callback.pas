unit callback;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Windows, Classes, SysUtils;

type

  { TWaitEventThread }

  TWaitEventThread = class(TThread)
  private
    FNotify: TNotifyEvent;
    FNotifyEvent: THandle;
  protected
    procedure Execute(); override;
  public
    constructor Create();
    destructor Destroy(); override;
    property NotifyEvent: THandle read FNotifyEvent;
    property OnNotify: TNotifyEvent read FNotify write FNotify;
  end;

implementation

uses
  util;

{ TWaitEventThread }

procedure TWaitEventThread.Execute;
begin
  while not Terminated do
    case WaitForSingleObject(FNotifyEvent, INFINITE) of
      WAIT_ABANDONED:
        Exit;
      WAIT_OBJECT_0:
        if not Terminated then FNotify(Self);
      WAIT_TIMEOUT:
        Exit;
      else
        Exit;
    end;
end;

constructor TWaitEventThread.Create();
begin
  inherited Create(False);
  FNotifyEvent := CreateEvent(nil, False, False, nil);
  FreeOnTerminate := True;
end;

destructor TWaitEventThread.Destroy;
begin
  CloseHandle(FNotifyEvent);
  inherited Destroy;
end;

end.

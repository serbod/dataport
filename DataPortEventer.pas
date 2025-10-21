unit DataPortEventer;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$DEFINE NO_LIBC}
{$ENDIF}

interface

uses
  SysUtils, Classes, DataPort;

const
  DP_NOTIFY_NONE    = 'N';
  DP_NOTIFY_OPEN    = 'O';
  DP_NOTIFY_CLOSE   = 'C';
  DP_NOTIFY_ERROR   = 'E';
  DP_NOTIFY_DATA    = 'D';

type
  { TDataPortNotifyThread
    Call events of TDataPort objects from application main thread.
    Windows PostMessage() is not situable for cross-platform non-visual controls.
    In FreePascal AllocateHWnd() implemented in LCLIntf unit, and LM_USER instead of WM_USER
  }

  TDataPortNotifyThread = class(TThread)
  protected
    FList: TStringList;
    FLock: TSimpleRWSync;
    procedure SyncProc();
    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function GetCount: Integer;
    procedure RegisterDataport(ADataPort: TDataPort);
    procedure UnRegisterDataport(ADataPort: TDataPort);
    procedure NotifyDataport(ADataPort: TDataPort; AEventType: AnsiChar;
      const AErrorStr: string = '');
  end;

{ Thread-safe functions }
procedure RegisterDataportNotify(AValue: TDataPort);
procedure UnRegisterDataportNotify(AValue: TDataPort);
procedure NotifyDataport(ADataPort: TDataPort; AEventType: AnsiChar;
  const AErrorStr: string = '');

implementation

var
  DataPortNotifyThread: TDataPortNotifyThread;

{ TDataPortNotifyThread }

procedure TDataPortNotifyThread.SyncProc();
var
  cEventType: Char;
  Item: TDataPort;
  sError: string;
begin
  cEventType := DP_NOTIFY_NONE;
  sError := '';
  FLock.BeginWrite;
  try
    if FList.Count > 0 then
    begin
      Item := (FList.Objects[0] as TDataPort);
      cEventType := FList.Strings[0][1];
      sError := Copy(FList.Strings[0], 2, MaxInt);
      FList.Delete(0);
    end;
  finally
    FLock.EndWrite;
  end;

  case cEventType of
    DP_NOTIFY_OPEN:
    begin
      if Assigned(Item.OnOpen) then Item.OnOpen(Item);
    end;
    DP_NOTIFY_CLOSE:
    begin
      if Assigned(Item.OnClose) then Item.OnClose(Item);
    end;
    DP_NOTIFY_ERROR:
    begin
      if Assigned(Item.OnError) then Item.OnError(Item, sError);
    end;
    DP_NOTIFY_DATA:
    begin
      if Assigned(Item.OnDataAppear) then Item.OnDataAppear(Item);
    end;
  end;
end;

procedure TDataPortNotifyThread.Execute;
//var
//  i: Integer;
begin
  while not Terminated do
  begin
    if GetCount > 0 then
    begin
      Synchronize(SyncProc);
    end
    else
      Sleep(1);

    {for i := 0 to FList.Count-1 do
    begin
      FItem := TDataPortUART(FList.Items[i]);
      if Assigned(FItem.OnDataAppear) and (FItem.PeekSize() > 0) then
      begin
        FEventType := DP_NOTIFY_DATA;
        Synchronize(SyncProc);
      end;

      if Assigned(FItem.OnError) and (FErrorStr <> '') then
      begin
        FEventType := DP_NOTIFY_ERROR;
        Synchronize(SyncProc);
      end;
    end; }
  end;
end;

constructor TDataPortNotifyThread.Create;
begin
  FLock := TSimpleRWSync.Create();
  FList := TStringList.Create();
  inherited Create(False);
end;

destructor TDataPortNotifyThread.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FList);
  FreeAndNil(FLock);
end;

function TDataPortNotifyThread.GetCount: Integer;
begin
  FLock.BeginRead;
  try
    Result := FList.Count;
  finally
    FLock.EndRead;
  end;
end;

procedure TDataPortNotifyThread.RegisterDataport(ADataPort: TDataPort);
begin
end;

procedure TDataPortNotifyThread.UnRegisterDataport(ADataPort: TDataPort);
var
  i: Integer;
begin
  FLock.BeginWrite;
  try
    for i := FList.Count-1 downto 0 do
    begin
      if FList.Objects[i] = ADataPort then
        FList.Delete(i);
    end;
  finally
    FLock.EndWrite;
  end;
end;

procedure TDataPortNotifyThread.NotifyDataport(ADataPort: TDataPort;
  AEventType: AnsiChar; const AErrorStr: string);
begin
  FLock.BeginWrite;
  try
    FList.AddObject(AEventType + AErrorStr, ADataPort);
  finally
    FLock.EndWrite;
  end;
end;

procedure RegisterDataportNotify(AValue: TDataPort);
begin
  if not Assigned(DataPortNotifyThread) then
    DataPortNotifyThread := TDataPortNotifyThread.Create();
  DataPortNotifyThread.RegisterDataport(AValue);
end;

procedure UnRegisterDataportNotify(AValue: TDataPort);
begin
  if Assigned(DataPortNotifyThread) then
  begin
    DataPortNotifyThread.UnRegisterDataport(AValue);
    if DataPortNotifyThread.GetCount = 0 then
      FreeAndNil(DataPortNotifyThread);
  end;
end;

procedure NotifyDataport(ADataPort: TDataPort; AEventType: AnsiChar;
  const AErrorStr: string);
begin
  if Assigned(DataPortNotifyThread) then
    DataPortNotifyThread.NotifyDataport(ADataPort, AEventType, AErrorStr);
end;

initialization
  DataPortNotifyThread := nil;

finalization
  FreeAndNil(DataPortNotifyThread);

end.


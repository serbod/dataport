{
Cross-platform async events from read/write threads to main thread.

(C) Sergey Bodrov, 2012-2025
}
unit DataPortEventer;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$DEFINE NO_LIBC}
{$ENDIF}

interface

uses
  {$ifndef FPC}Windows, Messages, {$endif}
  SysUtils, Classes, DataPort;

const
  DP_NOTIFY_NONE    = 'N';
  DP_NOTIFY_OPEN    = 'O';
  DP_NOTIFY_CLOSE   = 'C';
  DP_NOTIFY_ERROR   = 'E';
  DP_NOTIFY_DATA    = 'D';

{ Thread-safe functions }
procedure RegisterDataportNotify(AValue: TDataPort);
procedure UnRegisterDataportNotify(AValue: TDataPort);
procedure NotifyDataport(ADataPort: TDataPort; AEventType: AnsiChar;
  const AErrorStr: string = '');

implementation

{$ifndef FPC}
const WM_DATAPORT_NOTIFY = WM_USER + 101;
{$endif}

type
  { TDataPortNotifyThread
    Call events of TDataPort objects from application main thread.
    Windows PostMessage() is not situable for cross-platform non-visual controls.
    In FreePascal AllocateHWnd() implemented in LCLIntf unit, and LM_USER instead of WM_USER
  }

  TDataPortNotifyThread = class(TThread)
  protected
    procedure SyncProc();
    procedure Execute; override;
  end;

  TDataPortEventer = class(TComponent)
  protected
    FList: TStringList;
    FLock: TSimpleRWSync;
    FNotifyThread: TDataPortNotifyThread;

    {$ifndef FPC}
    FHWnd: HWND;
    procedure WndMsgProc(var AMessage: TMessage);
    {$endif}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ExecEvent();
    function GetCount: Integer;
    procedure RegisterDataport(ADataPort: TDataPort);
    procedure UnRegisterDataport(ADataPort: TDataPort);
    procedure NotifyDataport(ADataPort: TDataPort; AEventType: AnsiChar;
      const AErrorStr: string = '');

    {$ifndef FPC}
    property HWnd: HWND read FHWnd;
    {$endif}
  end;

var
  DataPortEventerObject: TDataPortEventer;

{ TDataPortNotifyThread }

procedure TDataPortNotifyThread.SyncProc();
begin
  if Assigned(DataPortEventerObject) then
    DataPortEventerObject.ExecEvent();
end;

procedure TDataPortNotifyThread.Execute;
//var
//  i: Integer;
begin
  while (not Terminated) and Assigned(DataPortEventerObject) do
  begin
    if DataPortEventerObject.GetCount > 0 then
    begin
      try
        Synchronize(SyncProc);
      except
        // show must go on
      end;
    end
    else
      Sleep(1);
  end;
end;

procedure RegisterDataportNotify(AValue: TDataPort);
begin
  if not Assigned(DataPortEventerObject) then
    DataPortEventerObject := TDataPortEventer.Create(nil);
  DataPortEventerObject.RegisterDataport(AValue);
end;

procedure UnRegisterDataportNotify(AValue: TDataPort);
begin
  if Assigned(DataPortEventerObject) then
  begin
    DataPortEventerObject.UnRegisterDataport(AValue);
    if DataPortEventerObject.GetCount = 0 then
      FreeAndNil(DataPortEventerObject);
  end;
end;

procedure NotifyDataport(ADataPort: TDataPort; AEventType: AnsiChar;
  const AErrorStr: string);
begin
  if Assigned(DataPortEventerObject) then
    DataPortEventerObject.NotifyDataport(ADataPort, AEventType, AErrorStr);
end;

{ TDataPortEventer }

{$ifndef FPC}
procedure TDataPortEventer.WndMsgProc(var AMessage: TMessage);
begin
  case AMessage.Msg of
    WM_DATAPORT_NOTIFY:
    begin
      if Assigned(DataPortEventerObject) then
        DataPortEventerObject.ExecEvent();
      AMessage.Result := 0;
    end;
  else
    AMessage.Result := DefWindowProc(FHWnd, AMessage.Msg, AMessage.WParam, AMessage.LParam);
  end;
end;
{$endif}

constructor TDataPortEventer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TSimpleRWSync.Create();
  FList := TStringList.Create();
{$ifndef FPC}
  FHWnd := AllocateHWnd(WndMsgProc);
{$else}
  FNotifyThread := TDataPortNotifyThread.Create(False);
{$endif}
end;

destructor TDataPortEventer.Destroy;
begin
{$ifndef FPC}
  DeallocateHWnd(FHWnd);
{$else}
  FreeAndNil(FNotifyThread);
{$endif}
  FreeAndNil(FList);
  FreeAndNil(FLock);
  inherited;
end;

procedure TDataPortEventer.ExecEvent;
var
  cEventType: Char;
  Item: TDataPort;
  sError: string;
begin
  cEventType := DP_NOTIFY_NONE;
  sError := '';
  Item := nil;

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
      //if Assigned(Item.OnClose) then Item.OnClose(Item);
      Item.Close();
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

function TDataPortEventer.GetCount: Integer;
begin
  FLock.BeginRead;
  try
    Result := FList.Count;
  finally
    FLock.EndRead;
  end;
end;

procedure TDataPortEventer.RegisterDataport(ADataPort: TDataPort);
begin
  //
end;

procedure TDataPortEventer.UnRegisterDataport(ADataPort: TDataPort);
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

procedure TDataPortEventer.NotifyDataport(ADataPort: TDataPort;
  AEventType: AnsiChar; const AErrorStr: string);
begin
  FLock.BeginWrite;
  try
    FList.AddObject(AEventType + AErrorStr, ADataPort);
  finally
    FLock.EndWrite;
  end;
{$ifndef FPC}
  PostMessage(HWnd, WM_DATAPORT_NOTIFY, 0, 0);
{$endif}
end;

initialization
  DataPortEventerObject := nil;

finalization
  FreeAndNil(DataPortEventerObject);

end.


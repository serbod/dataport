{
Asynchronous wrapper around Synapse TBlockSocket.

(C) Sergey Bodrov, 2012-2018

When using UDP, remember, that it not session protocol, data delivery and correct
order not guaranteed. To start receive data, you must send empty packet to
remote side, it tell remote side return address.

From version 1.0.3 multiple DataPortIP instances uses common socket reader with single thread.

Properties:
  RemoteHost - IP-address or name of remote host
  RemotePort - remote UPD or TCP port number
  LocalHost  - IP-address or name of local host
  LocalPort  - local UPD or TCP port number

Methods:
  Open() - Connect to remote port. Session establiched for TCP and just port initialised for UDP. Init string format:
    InitStr = 'RemoteHost:RemotePort'
    RemoteHost - IP-address or name of remote host
    RemotePort - remote UPD or TCP port number

Events:
  OnOpen - Triggered after UDP port init or TCP session establiched.
}
unit DataPortIP;

interface

uses {$ifndef FPC}Windows,{$endif} SysUtils, Classes,
   syncobjs, DataPort, synsock, blcksock, synautil;

type
  TIpProtocolEnum = (ippUDP, ippTCP);
  TIpSocketItem = class;

  { TDataPortIP }

  TDataPortIP = class(TDataPort)
  private
    //slReadData: TStringList; // for storing every incoming data packet separately
    FLock: TMultiReadExclusiveWriteSynchronizer;
    procedure SetIpProtocol(AValue: TIpProtocolEnum);
  protected
    FIpSocketItem: TIpSocketItem;
    FRemoteHost: string;
    FRemotePort: string;
    FIpProtocol: TIpProtocolEnum;
    function GetLocalHost: string; virtual;
    function GetLocalPort: string; virtual;
    procedure OnIncomingMsgHandler(Sender: TObject; const AMsg: string);
    procedure OnErrorHandler(Sender: TObject; const AMsg: string);
    procedure OnConnectHandler(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    { Open() - Connect to remote port. Session establiched for TCP and just port initialised for UDP. Init string format:
      InitStr = 'RemoteHost:RemotePort'
      RemoteHost - IP-address or name of remote host
      RemotePort - remote UPD or TCP port number }
    procedure Open(const AInitStr: string = ''); override;
    procedure Close(); override;
    function Push(const AData: AnsiString): boolean; override;
    function Pull(ASize: Integer = MaxInt): AnsiString; override;
    function Peek(ASize: Integer = MaxInt): AnsiString; override;
    function PeekSize(): Cardinal; override;
    { IP protocol type }
    property IpProtocol: TIpProtocolEnum read FIpProtocol write SetIpProtocol;
  published
    { IP-address or name of remote host }
    property RemoteHost: string read FRemoteHost write FRemoteHost;
    { remote UPD or TCP port number }
    property RemotePort: string read FRemotePort write FRemotePort;
    { IP-address or name of local host }
    property LocalHost: string read GetLocalHost;
    { local UPD or TCP port number }
    property LocalPort: string read GetLocalPort;
    property Active;
    property OnDataAppear;
    property OnError;
    { Triggered after UDP port init or TCP session establiched }
    property OnOpen;
    property OnClose;
  end;

  TDataPortTCP = class(TDataPortIP)
  public
    procedure Open(const AInitStr: string = ''); override;
  end;

  { TDataPortUDP }

  TDataPortUDP = class(TDataPortIP)
  public
    procedure Open(const AInitStr: string = ''); override;
    { Send data to destination address ADestAddr as 'host:port' }
    function PushTo(const AData: AnsiString; ADestAddr: string): Boolean;
  end;

  { TIpSocketItem }
  { Item for sockets list, created on Open(), used by reader thread }
  TIpSocketItem = class(TObject)
  private
    FOnIncomingMsg: TMsgEvent;
    FOnError: TMsgEvent;
    FOnConnect: TNotifyEvent;
    FLock: TCriticalSection;
  public
    // Only socket reader can manage Socket
    Socket: TBlockSocket;
    DataPortIP: TDataPortIP;
    Protocol: TIpProtocolEnum;
    LockCount: Integer;
    RxDataStr: AnsiString;
    //TxDataStr: AnsiString;
    ErrorStr: string;
    Active: Boolean;
    Connected: Boolean;

    function GetLocalHost(): string;
    function GetLocalPort(): string;
    function SendString(const ADataStr: AnsiString): Boolean;
    function SendStream(st: TStream): Boolean;

    property Lock: TCriticalSection read FLock;
    property OnIncomingMsg: TMsgEvent read FOnIncomingMsg write FOnIncomingMsg;
    property OnError: TMsgEvent read FOnError write FOnError;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
  end;

procedure Register;

implementation

type
  { TIpSocketPool }
  { For better portability to DLL and stability, reader thread and critical section
    automaticaly created after unit initialisation, when first DataPortIP opened.
    And destroyed when last DataPortIP closed, before unit finalisation. }
  TIpSocketPool = class(TList)
  private
    FIpReadThread: TThread;
    FLock: TCriticalSection;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    procedure BeforeDestruction(); override;
    function DataPortOpen(ADataPortIP: TDataPortIP; AIncomingMsgHandler: TMsgEvent; AErrorHandler: TMsgEvent; AConnectHandler: TNotifyEvent): TIpSocketItem;
    procedure DataPortClose(ADataPortIP: TDataPortIP);
    function GetItem(AIndex: Integer): TIpSocketItem;
    { Lock for modifing items list. NOTE! Can be nil if no items in list! }
    property Lock: TCriticalSection read FLock;
  end;

  { TIpReadThread }

  TIpReadThread = class(TThread)
  protected
    FItem: TIpSocketItem;
    FEventType: Byte; // 0 - none, 1-connect, 2-data, 3-error
    procedure CloseSocket();
    procedure Execute(); override;
    procedure SyncProc();
  public
    IpSocketPool: TIpSocketPool;
  end;

var
  GlobalIpSocketPool: TIpSocketPool;

procedure Register;
begin
  RegisterComponents('DataPort', [TDataPortTCP]);
  RegisterComponents('DataPort', [TDataPortUDP]);
end;

{ TIpReadThread }

procedure TIpReadThread.CloseSocket();
begin
  FItem.Active := False;
  if Assigned(FItem.Socket) then
  begin
    try
      FItem.Socket.CloseSocket();
    finally
      FreeAndNil(FItem.Socket);
    end;
  end;
end;

procedure TIpReadThread.Execute();
var
  n, ItemLockCount: Integer;
  IsNeedSleep: Boolean;
begin
  n := 0;
  while not Terminated do
  begin
    IsNeedSleep := True;
    if n < IpSocketPool.Count then
    begin
      FItem := IpSocketPool.GetItem(n);
      if Assigned(FItem.DataPortIP) then
      begin
        // acquire lock
        ItemLockCount := InterLockedIncrement(FItem.LockCount);
        try
          if (ItemLockCount = 1) then
          begin
            // connect
            if FItem.Active and (not Assigned(FItem.Socket)) then
            begin
              FItem.Connected := False;
              FItem.ErrorStr := '';
              case FItem.DataPortIP.IpProtocol of
                ippUDP: FItem.Socket := TUDPBlockSocket.Create();
                ippTCP: FItem.Socket := TTCPBlockSocket.Create();
              end;

              FItem.Socket.Connect(FItem.DataPortIP.RemoteHost, FItem.DataPortIP.RemotePort);
              if FItem.Socket.LastError <> 0 then
              begin
                // Error event
                FItem.ErrorStr := IntToStr(FItem.Socket.LastError) + ' ' + FItem.Socket.LastErrorDesc;
                FItem.Active := False;
              end
              else
              begin
                // Connected event
                FItem.Connected := True;
                if Assigned(FItem.OnConnect) then
                begin
                  FEventType := 1;
                  Synchronize(SyncProc);
                end;
              end;
              //IsNeedSleep := False;
            end;

            // read
            //if FItem.Active and Assigned(FItem.Socket) and (FItem.Socket.WaitingData > 0) then
            //if FItem.Active and Assigned(FItem.Socket) and (FItem.Socket.WaitingDataEx() > 0) then
            if FItem.Active and Assigned(FItem.Socket) then
            begin
              try
                FItem.RxDataStr := FItem.RxDataStr + FItem.Socket.RecvPacket(0);
                if FItem.Socket.LastError = 0 then
                begin
                  // DataRead event
                  if Assigned(FItem.OnIncomingMsg) then
                  begin
                    FEventType := 2;
                    Synchronize(SyncProc);
                  end;
                end
                else if FItem.Socket.LastError = WSAETIMEDOUT then
                begin
                  // nothing
                end
                else
                begin
                  // Error event
                  FItem.ErrorStr := IntToStr(FItem.Socket.LastError) + ' ' + FItem.Socket.LastErrorDesc;
                  FItem.Active := False;
                end;
                IsNeedSleep := False;
              except on E: Exception do
                begin
                  FItem.ErrorStr := E.Message;
                  CloseSocket();
                end;
              end;
            end;

            // disconnect
            if (not FItem.Active) and Assigned(FItem.Socket) then
            begin
              CloseSocket();
              IsNeedSleep := False;
            end;
          end;
        finally
          // release lock
          InterLockedDecrement(FItem.LockCount);
        end;
      end
      else
      begin
        // delete item
        CloseSocket();
        if Assigned(IpSocketPool.Lock) then
        begin
          IpSocketPool.Lock.Acquire();
          try
            IpSocketPool.Delete(n);
            Dec(n);
          finally
            IpSocketPool.Lock.Release();
          end;
        end;
      end;

      // Error event
      if FItem.ErrorStr <> '' then
      begin
        if Assigned(FItem.OnError) then
        begin
          FEventType := 3;
          Synchronize(SyncProc);
        end;
        FItem.ErrorStr := '';
      end;

      Inc(n);
    end
    else
    begin
      n := 0;
      if IsNeedSleep then
        Sleep(1);
    end;
  end;

  if Terminated then
  begin
    // cleanup sockets
    for n := IpSocketPool.Count-1 downto 0 do
    begin
      FItem := IpSocketPool.GetItem(n);
      CloseSocket();
      IpSocketPool.Delete(n);
    end;
  end;
end;

procedure TIpReadThread.SyncProc();
begin
  case FEventType of
    1: FItem.OnConnect(FItem);
    2: FItem.OnIncomingMsg(FItem, FItem.RxDataStr);
    3: FItem.OnError(FItem, FItem.ErrorStr);
  end;
  FEventType := 0;
end;

{ TIpSocketPool }

procedure TIpSocketPool.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if Action = lnDeleted then
    TIpSocketItem(Ptr).Free();
end;

procedure TIpSocketPool.BeforeDestruction();
begin
  if Assigned(FIpReadThread) then
    FreeAndNil(FIpReadThread);

  if Assigned(FLock) then
    FreeAndNil(FLock);

  inherited BeforeDestruction;
end;

function TIpSocketPool.DataPortOpen(ADataPortIP: TDataPortIP;
  AIncomingMsgHandler: TMsgEvent; AErrorHandler: TMsgEvent;
  AConnectHandler: TNotifyEvent): TIpSocketItem;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
  begin
    Result := GetItem(i);
    if Result.DataPortIP = ADataPortIP then
      Exit;
  end;

  Result := TIpSocketItem.Create();
  Result.DataPortIP := ADataPortIP;
  Result.OnIncomingMsg := AIncomingMsgHandler;
  Result.OnError := AErrorHandler;
  Result.OnConnect := AConnectHandler;
  Result.Active := True;

  if not Assigned(FLock) then
    FLock := TCriticalSection.Create();

  FLock.Acquire();
  try
    Add(Result);
  finally
    FLock.Release();
  end;

  if (not Assigned(FIpReadThread)) then
  begin
    // create socket reader
    FIpReadThread := TIpReadThread.Create(True);
    (FIpReadThread as TIpReadThread).IpSocketPool := Self;
    FIpReadThread.Suspended := False;
  end;
end;

procedure TIpSocketPool.DataPortClose(ADataPortIP: TDataPortIP);
var
  i, ActiveCount: Integer;
  Item: TIpSocketItem;
begin
  ActiveCount := 0;
  if Assigned(FLock) then
  begin
    FLock.Acquire();
    try
      for i := Count-1 downto 0 do
      begin
        Item := GetItem(i);
        if Item.DataPortIP = ADataPortIP then
        begin
          Item.DataPortIP := nil;
          Item.OnIncomingMsg := nil;
          Item.OnError := nil;
          Item.OnConnect := nil;
          Break;
        end
        else if Assigned(Item.DataPortIP) then
          Inc(ActiveCount);
      end;

    finally
      FLock.Release();
    end;
  end;

  if (ActiveCount = 0) then
  begin
    if Assigned(FIpReadThread) then
      FreeAndNil(FIpReadThread);
    if Assigned(FLock) then
      FreeAndNil(FLock);
  end;
end;

function TIpSocketPool.GetItem(AIndex: Integer): TIpSocketItem;
begin
  Result := TIpSocketItem(Get(AIndex));
end;

{ TIpSocketItem }

function TIpSocketItem.GetLocalHost(): string;
begin
  if Assigned(Socket) then
  begin
    Socket.GetSinLocal();
    Result := Socket.GetLocalSinIP;
  end
  else
    Result := '';
end;

function TIpSocketItem.GetLocalPort(): string;
begin
  if Assigned(Socket) then
  begin
    Socket.GetSinLocal();
    Result := IntToStr(Socket.GetLocalSinPort);
  end
  else
    Result := '';
end;

function TIpSocketItem.SendString(const ADataStr: AnsiString): Boolean;
var
  LockTryCount: Integer;
begin
  //TxDataStr := TxDataStr + ADataStr;
  Result := False;
  if Assigned(Socket) then
  begin
    // try to acquire exclusive lock
    LockTryCount := 10;
    while (InterLockedIncrement(LockCount) > 1) and (LockTryCount > 0) do
    begin
      InterLockedDecrement(LockCount);
      Dec(LockTryCount);
      if (LockTryCount = 0) then
        Exit;
      Sleep(1);
    end;

    try
      try
        Result := Socket.CanWrite(0);
        if Result then
        begin
          Socket.SendString(ADataStr);
          Result := (Socket.LastError = 0);
          if not Result then
          begin
            ErrorStr := IntToStr(Socket.LastError) + ' ' + Socket.LastErrorDesc;
          end;
        end;
      except
        ErrorStr := 'Socket write exception';
      end;

    finally
      // release exclusive lock
      InterLockedDecrement(LockCount);
    end;
  end;
end;

function TIpSocketItem.SendStream(st: TStream): Boolean;
begin
  Result := False;
  if Assigned(st) and (st.Size <> 0) and Assigned(Socket) then
  begin
    st.Position := 0;
    try
      Result := Socket.CanWrite(0);
      if Result then
      begin
        Socket.SendStream(st);
        Result := (Socket.LastError = 0);
        if not Result then
        begin
          ErrorStr := IntToStr(Socket.LastError) + ' ' + Socket.LastErrorDesc;
        end;
      end;
    except
      ErrorStr := 'Socket write exception';
    end;
  end;
end;

{ TDataPortIP }

constructor TDataPortIP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.FLock := TMultiReadExclusiveWriteSynchronizer.Create();
  Self.FRemoteHost := '';
  Self.FRemotePort := '';
  Self.FActive := False;
end;

procedure TDataPortIP.Open(const AInitStr: string = '');
var
  n: integer;
begin
  // Set host and port from init string
  if AInitStr <> '' then
  begin
    n := Pos(':', AInitStr);
    if n > 0 then
    begin
      Self.FRemoteHost := Copy(AInitStr, 1, n - 1);
      Self.FRemotePort := Copy(AInitStr, n + 1, MaxInt);
    end
    else
      Self.FRemoteHost := AInitStr;
  end;

  if Assigned(GlobalIpSocketPool) then
  begin
    FIpSocketItem := GlobalIpSocketPool.DataPortOpen(Self, OnIncomingMsgHandler, OnErrorHandler, OnConnectHandler);
  end;

  // don't inherits Open() - OnOpen event will be after successfull connection
end;

procedure TDataPortIP.Close();
begin
  FIpSocketItem := nil;
  if Active and Assigned(GlobalIpSocketPool) then
    GlobalIpSocketPool.DataPortClose(Self);
  inherited Close();
end;

destructor TDataPortIP.Destroy();
begin
  FIpSocketItem := nil;
  if Assigned(GlobalIpSocketPool) then
    GlobalIpSocketPool.DataPortClose(Self);
  FreeAndNil(FLock);
  inherited Destroy();
end;

procedure TDataPortIP.OnConnectHandler(Sender: TObject);
begin
  FActive := True;
  if Assigned(OnOpen) then
    OnOpen(Self);
end;

procedure TDataPortIP.SetIpProtocol(AValue: TIpProtocolEnum);
begin
  if FIpProtocol = AValue then Exit;
  Close();
  FIpProtocol := AValue;
end;

function TDataPortIP.GetLocalHost: string;
begin
  if Assigned(FIpSocketItem) then
    Result := FIpSocketItem.GetLocalHost()
  else
    Result := '';
end;

function TDataPortIP.GetLocalPort: string;
begin
  if Assigned(FIpSocketItem) then
    Result := FIpSocketItem.GetLocalPort()
  else
    Result := '';
end;

procedure TDataPortIP.OnIncomingMsgHandler(Sender: TObject; const AMsg: string);
begin
  if AMsg <> '' then
  begin
    if Assigned(FOnDataAppear) then
      FOnDataAppear(self);
  end;
end;

procedure TDataPortIP.OnErrorHandler(Sender: TObject; const AMsg: string);
begin
  FIpSocketItem := nil;
  if Assigned(Self.FOnError) then
    Self.FOnError(Self, AMsg);
  FActive := False;
end;

function TDataPortIP.Peek(ASize: Integer): AnsiString;
begin
  if Assigned(FIpSocketItem) then
  begin
    FLock.BeginRead();
    try
      Result := Copy(FIpSocketItem.RxDataStr, 1, ASize);
    finally
      FLock.EndRead();
    end;
  end;
end;

function TDataPortIP.PeekSize(): Cardinal;
begin
  if Assigned(FIpSocketItem) then
  begin
    FLock.BeginRead();
    try
      Result := Cardinal(Length(FIpSocketItem.RxDataStr));
    finally
      FLock.EndRead();
    end;
  end
  else
    Result := 0;
end;

function TDataPortIP.Pull(ASize: Integer): AnsiString;
begin
  Result := '';
  if Assigned(FIpSocketItem) then
  begin
    FLock.BeginRead();
    try
      Result := Copy(FIpSocketItem.RxDataStr, 1, ASize);
      Delete(FIpSocketItem.RxDataStr, 1, ASize);
    finally
      FLock.EndRead();
    end;
  end;
end;

function TDataPortIP.Push(const AData: AnsiString): boolean;
begin
  Result := False;
  if Assigned(FIpSocketItem) and FLock.BeginWrite() then
  begin
    try
      FIpSocketItem.SendString(AData);
      Result := True;
    finally
      FLock.EndWrite();
    end;
  end;
end;

procedure TDataPortTCP.Open(const AInitStr: string = '');
begin
  FIpProtocol := ippTCP;
  inherited Open(AInitStr);
  FActive := True;
end;

procedure TDataPortUDP.Open(const AInitStr: string = '');
begin
  FIpProtocol := ippUDP;
  inherited Open(AInitStr);
  FActive := True;
end;

function TDataPortUDP.PushTo(const AData: AnsiString; ADestAddr: string
  ): Boolean;
var
  n: integer;
  ss, sh, sp: string;
begin
  Result := False;
  if Assigned(FIpSocketItem) and Assigned(FIpSocketItem.Socket) then
  begin
    if ADestAddr = '' then
    begin
      //UdpSocket.SetRemoteSin(remoteHost, remotePort);
    end
    else
    begin
      ss := ADestAddr;
      n := Pos(':', ss);
      sh := Copy(ss, 1, n - 1);
      sp := Copy(ss, n + 1, MaxInt);
      FIpSocketItem.Socket.SetRemoteSin(sh, sp);
    end;
    FIpSocketItem.Socket.SendString(AData);
    Result := (FIpSocketItem.Socket.LastError = 0);
    if not Result then
    begin
      FIpSocketItem.ErrorStr := IntToStr(FIpSocketItem.Socket.LastError) + ' ' + FIpSocketItem.Socket.LastErrorDesc;
    end;
  end;
end;


initialization
  GlobalIpSocketPool := TIpSocketPool.Create();

finalization
  FreeAndNil(GlobalIpSocketPool);

end.

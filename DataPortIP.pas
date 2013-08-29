unit DataPortIP;

interface
uses SysUtils, Classes, DataPort, synsock, blcksock, synautil;

type
  TIpProtocolEnum = (ippTCP, ippUDP);

  TIpClient = class(TThread)
  private
    Socket: TBlockSocket;
    s: string;
    bConnect: boolean;
    FOnIncomingMsgEvent: TMsgEvent;
    FOnErrorEvent: TMsgEvent;
    FOnConnect: TNotifyEvent;
    procedure SyncProc();
  protected
    procedure Execute(); override;
  public
    remoteHost: string;
    remotePort: string;
    protocol: TIpProtocolEnum;
    bLock: Boolean;
    property OnIncomingMsgEvent: TMsgEvent read FOnIncomingMsgEvent write FOnIncomingMsgEvent;
    property OnErrorEvent: TMsgEvent read FOnErrorEvent write FOnErrorEvent;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    function SendString(s: string): Boolean;
    procedure SendStream(st: TStream; Dest: string);
  end;

  { TDataPortIP }

  TDataPortIP = class(TDataPort)
  private
    //slReadData: TStringList; // for storing every incoming data packet separately
    sReadData: AnsiString;
    lock: TMultiReadExclusiveWriteSynchronizer;
    IpClient: TIpClient;
    FRemoteHost: string;
    FRemotePort: string;
    FOnConnect: TNotifyEvent;
    procedure IncomingMsgHandler(Sender: TObject; AMsg: string);
    procedure ErrorEventHandler(Sender: TObject; AMsg: string);
    procedure OnConnectHandler(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure Open(InitStr: string = ''); override;
    procedure Close(); override;
    function Push(sMsg: AnsiString): Boolean; override;
    function Pull(size: Integer = MaxInt): AnsiString; override;
    function Peek(size: Integer = MaxInt): AnsiString; override;
    function PeekSize(): Cardinal; override;
  published
    property RemoteHost: string read FRemoteHost write FRemoteHost;
    property RemotePort: string read FRemotePort write FRemotePort;
    property Active;
    property OnDataAppear;
    property OnError;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
  end;

  TDataPortTCP = class(TDataPortIP)
  public
    procedure Open(InitStr: string = ''); override;
  end;

  TDataPortUDP = class(TDataPortIP)
  public
    procedure Open(InitStr: string = ''); override;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('DataPort', [TDataPortTCP]);
  RegisterComponents('DataPort', [TDataPortUDP]);
end;

// === TIpClient ===
procedure TIpClient.SyncProc();
begin
  //if s:='' then Exit;
  if bConnect then
  begin
    if Assigned(self.FOnConnect) then FOnConnect(self);
    bConnect:=False;
    Exit;
  end;

  bLock:=True;
  if Socket.LastError=0 then
  begin
    if Assigned(self.FOnIncomingMsgEvent) then FOnIncomingMsgEvent(self, s);
  end
  else
  begin
    if Assigned(self.FOnErrorEvent) then FOnErrorEvent(self, s);
    self.Terminate();
  end;
  s:='';
  bLock:=False;
end;

procedure TIpClient.Execute();
begin
  if self.protocol=ippUDP then Socket:=TUDPBlockSocket.Create()
  else if self.protocol=ippTCP then Socket:=TTCPBlockSocket.Create();

  try
    bConnect:=False;
    Socket.Connect(remoteHost, remotePort);
    {s:='Connect '+remoteHost+':'+remotePort+' '+UdpSocket.LastErrorDesc;
    Synchronize(SyncProc);
    Sleep(100);}
    if Socket.LastError<>0 then
    begin
      s:=IntToStr(Socket.LastError)+' '+Socket.LastErrorDesc;
      Synchronize(SyncProc);
      //Exit;
      Self.Terminate();
    end
    else
    begin
      // Connected event
      bConnect:=True;
      Synchronize(SyncProc);
    end;

    while not Terminated do
    begin
      s:=Socket.RecvPacket(100);
      if Socket.LastError=0 then
      begin
        Synchronize(SyncProc);
      end
      else if Socket.LastError=WSAETIMEDOUT then
      begin
        s:='';
      end
      else
      begin
        s:=IntToStr(Socket.LastError)+' '+Socket.LastErrorDesc;
        Synchronize(SyncProc);
      end;
      Sleep(1);
    end;
    Socket.CloseSocket();
  finally
    FreeAndNil(Socket);
  end;
end;

function TIpClient.SendString(s: string): Boolean;
begin
  Result:=False;
  if Assigned(Socket) then
  begin
    Socket.SendString(s);
    if Socket.LastError<>0 then
    begin
      s:=IntToStr(Socket.LastError)+' '+Socket.LastErrorDesc;
      //Synchronize(SyncProc);
      SyncProc();
      Exit;
    end;
    Result:=True;
  end;
end;

procedure TIpClient.SendStream(st: TStream; Dest: string);
var
  n: Integer;
  ss, sh, sp: string;
begin
  if not Assigned(Socket) then Exit;
  if Dest='' then
  begin
    //UdpSocket.SetRemoteSin(remoteHost, remotePort);
  end
  else
  begin
    ss:=Dest;
    n:=Pos(':', ss);
    sh:=Copy(ss, 1, n-1);
    sp:=Copy(ss, n+1, MaxInt);
    Socket.SetRemoteSin(sh, sp);
  end;
  st.Position:=0;
  Socket.SendStreamRaw(st);
  if Socket.LastError<>0 then
  begin
    s:=IntToStr(Socket.LastError)+' '+Socket.LastErrorDesc;
    Synchronize(SyncProc);
  end;
end;


{ TDataPortIP }

constructor TDataPortIP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.lock:=TMultiReadExclusiveWriteSynchronizer.Create();
  //Self.slReadData:=TStringList.Create();
  Self.FRemoteHost:='';
  Self.FRemotePort:='';
  Self.FActive:=False;
  Self.sReadData:='';
  Self.IpClient:=nil;
end;

procedure TDataPortIP.Open(InitStr: string = '');
var
  n: Integer;
begin
  // Set host and port from init string
  if InitStr<>'' then
  begin
    n:=Pos(':', InitStr);
    if n>0 then
    begin
      Self.FRemoteHost:=Copy(InitStr, 1, n-1);
      Self.FRemotePort:=Copy(InitStr, n+1, MaxInt);
    end
    else
      Self.FRemoteHost:=InitStr;
  end;

  if Assigned(self.IpClient) then FreeAndNil(self.IpClient);
  Self.IpClient:=TIpClient.Create(true);
  Self.IpClient.OnIncomingMsgEvent:=self.IncomingMsgHandler;
  Self.IpClient.OnErrorEvent:=Self.ErrorEventHandler;
  Self.IpClient.OnConnect:=Self.OnConnectHandler;
  Self.IpClient.remoteHost:=Self.FRemoteHost;
  Self.IpClient.remotePort:=Self.FRemotePort;
  Self.IpClient.bLock:=False;
  // thread resumed in inherited classes
  //Self.IpClient.Resume();
  //Self.FActive:=True;

  // don't inherits Open() - OnOpen event will be after successfull connection
end;

procedure TDataPortIP.Close();
begin
  if Active then
  begin
    if Assigned(self.IpClient) then self.IpClient.Terminate();
  end;
  inherited Close();
end;

destructor TDataPortIP.Destroy();
begin
  if Assigned(self.IpClient) then FreeAndNil(self.IpClient);
  //FreeAndNil(self.slReadData);
  FreeAndNil(self.lock);
  inherited Destroy();
end;

procedure TDataPortIP.IncomingMsgHandler(Sender: TObject; AMsg: string);
begin
  if AMsg<>'' then
  begin
    if lock.BeginWrite then
    begin
      //slReadData.Add(AMsg);
      sReadData:=sReadData+AMsg;
      lock.EndWrite;

      if Assigned(FOnDataAppear) then FOnDataAppear(self);
    end;

  end;
end;

procedure TDataPortIP.ErrorEventHandler(Sender: TObject; AMsg: string);
begin
  if Assigned(Self.FOnError) then Self.FOnError(Self, AMsg);
  self.FActive:=False;
end;

{
function TDataPortIP.Peek(size: Integer = MaxInt): AnsiString;
var
  i, num, remain: Integer;
begin
  Result:='';
  remain:=size;
  lock.BeginRead();
  for i:=0 to slReadData.Count do
  begin
    num:=Length(slReadData[i]);
    if num>remain then num:=remain;
    Result:=Result+Copy(slReadData[i], 1, num);
    remain:=remain-num;
    if remain<=0 then Break;
  end;
  lock.EndRead();
end;
}

function TDataPortIP.Peek(size: Integer = MaxInt): AnsiString;
begin
  lock.BeginRead();
  Result:=Copy(sReadData, 1, size);
  lock.EndRead();
end;

function TDataPortIP.PeekSize(): Cardinal;
//var i: Integer;
begin
  //Result:=0;
  lock.BeginRead();
  //// Length of all strings
  //for i:=0 to slReadData.Count-1 do Result:=Result+Cardinal(Length(slReadData[i]));
  Result:=Cardinal(Length(sReadData));
  lock.EndRead();
end;

{
function TDataPortIP.Pull(size: Integer = MaxInt): AnsiString;
var
  num, len, remain: Integer;
begin
  Result:='';
  remain:=size;
  if not lock.BeginWrite() then Exit;
  while slReadData.Count>0 do
  begin
    // we read every string to exclude line delimiters
    len:=Length(slReadData[0]);
    num:=len;
    if num>remain then num:=remain;
    Result:=Result+Copy(slReadData[0], 1, num);
    remain:=remain-num;
    if num>=len then slReadData.Delete(0)
    else
    begin
      Delete(slReadData[0], 1, num);
      Break;
    end;
    if remain<=0 then Break;
  end;
  lock.EndWrite();
end;
}

function TDataPortIP.Pull(size: Integer = MaxInt): AnsiString;
begin
  Result:='';
  if not lock.BeginWrite() then Exit;
  Result:=Copy(sReadData, 1, size);
  Delete(sReadData, 1, size);
  //sReadData:='';
  lock.EndWrite();
end;

function TDataPortIP.Push(sMsg: AnsiString): Boolean;
begin
  Result:=False;
  if not lock.BeginWrite() then Exit;
  if Assigned(self.IpClient) then
  begin
    self.IpClient.SendString(sMsg);
    Result:=True;
  end;
  lock.EndWrite();
end;

procedure TDataPortTCP.Open(InitStr: string = '');
begin
  inherited Open(InitStr);
  Self.IpClient.protocol:=ippTCP;
  Self.IpClient.Start();
  Self.FActive:=True;
end;

procedure TDataPortUDP.Open(InitStr: string = '');
begin
  inherited Open(InitStr);
  Self.IpClient.protocol:=ippUDP;
  Self.IpClient.Start();
  Self.FActive:=True;
end;

procedure TDataPortIP.OnConnectHandler(Sender: TObject);
begin
  Self.FActive:=True;
  if Assigned(FOnConnect) then FOnConnect(Self);
  if Assigned(OnOpen) then OnOpen(Self);
end;



end.

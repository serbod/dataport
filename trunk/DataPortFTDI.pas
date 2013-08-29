unit DataPortFTDI;

interface
uses SysUtils, Classes, DataPort, DataPortIP;

type

  { TDataPortUdpFtdi }

  TDataPortUdpFtdi = class(TDataPort)
  private
    sReadData: AnsiString;
    lock: TMultiReadExclusiveWriteSynchronizer;
    IpClient: TIpClient;
    FRemoteHost: string;
    FRemotePort: string;
    FMinDataBytes: Integer;
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
    property Active;
    property RemoteHost: string read FRemoteHost write FRemoteHost;
    property RemotePort: string read FRemotePort write FRemotePort;
    property MinDataBytes: integer read FMinDataBytes write FMinDataBytes;
    property OnDataAppear;
    property OnError;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
  end;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('DataPort', [TDataPortUdpFtdi]);
end;


{ TDataPortUdpFtdi }

constructor TDataPortUdpFtdi.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.lock:=TMultiReadExclusiveWriteSynchronizer.Create();
  sReadData:='';
  FRemoteHost:='';
  FRemotePort:='';
  FActive:=False;
  Self.IpClient:=nil;
end;

procedure TDataPortUdpFtdi.Open(InitStr: string = '');
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
  Self.IpClient.protocol:=ippUDP;
  Self.IpClient.remoteHost:=Self.FRemoteHost;
  Self.IpClient.remotePort:=Self.FRemotePort;
  Self.IpClient.OnIncomingMsgEvent:=self.IncomingMsgHandler;
  Self.IpClient.OnErrorEvent:=Self.ErrorEventHandler;
  Self.IpClient.OnConnect:=Self.OnConnectHandler;
  Self.IpClient.Start();
  Self.FActive:=True;

  // don't inherits Open() - OnOpen event will be after successfull connection
end;

procedure TDataPortUdpFtdi.Close();
begin
  if Assigned(self.IpClient) then FreeAndNil(self.IpClient);
  inherited Close();
end;

destructor TDataPortUdpFtdi.Destroy();
begin
  if Assigned(self.IpClient) then FreeAndNil(self.IpClient);
  FreeAndNil(self.lock);
  inherited Destroy();
end;

procedure TDataPortUdpFtdi.IncomingMsgHandler(Sender: TObject; AMsg: string);
begin
  if AMsg<>'' then
  begin
    // Remove 2 status bytes
    Delete(AMsg, 1, 2);

    if lock.BeginWrite then
    begin
      sReadData:=sReadData+AMsg;
      lock.EndWrite;

      if Length(sReadData) >= FMinDataBytes then
      begin
        if Assigned(FOnDataAppear) then FOnDataAppear(self);
      end;
    end;

  end;
end;

procedure TDataPortUdpFtdi.ErrorEventHandler(Sender: TObject; AMsg: string);
begin
  if Assigned(Self.FOnError) then Self.FOnError(Self, AMsg);
end;

function TDataPortUdpFtdi.Peek(size: Integer = MaxInt): AnsiString;
begin
  lock.BeginRead();
  Result:=Copy(sReadData, 1, size);
  lock.EndRead();
end;

function TDataPortUdpFtdi.PeekSize(): Cardinal;
begin
  lock.BeginRead();
  // Length of all strings
  Result:=Cardinal(Length(sReadData));
  lock.EndRead();
end;

function TDataPortUdpFtdi.Pull(size: Integer = MaxInt): AnsiString;
begin
  Result:='';
  if not lock.BeginWrite() then Exit;
  Result:=Copy(sReadData, 1, size);
  Delete(sReadData, 1, size);
  //sReadData:='';
  lock.EndWrite();
end;

function TDataPortUdpFtdi.Push(sMsg: AnsiString): Boolean;
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

procedure TDataPortUdpFtdi.OnConnectHandler(Sender: TObject);
begin
  if Assigned(FOnConnect) then FOnConnect(Self);
  if Assigned(OnOpen) then OnOpen(Self);
end;

end.

{
Serial communication port (UART). In Windows it COM-port, real or virtual.
In Linux it /dev/ttyS or /dev/ttyUSB. Also, Linux use file /var/FLock/LCK..ttyS for port FLocking

Sergey Bodrov, 2012-2017

Properties:
  Port - port name (COM1, /dev/ttyS01)
  BaudRate - data excange speed
  MinDataBytes - minimal bytes count in buffer for triggering event OnDataAppear

Methods:
  Open() - Opens port. As parameter it use port initialization string:
    InitStr = 'Port,BaudRate,DataBits,Parity,StopBits,SoftFlow,HardFlow'

    Port - COM port name (COM1, /dev/ttyS01)
    BaudRate - connection speed (50..4000000 bits per second), default 9600
    DataBits - default 8
    Parity - (N - None, O - Odd, E - Even, M - Mark or S - Space) default N
    StopBits - (1, 1.5, 2)
    SoftFlow - Enable XON/XOFF handshake, default 1
    HardFlow - Enable CTS/RTS handshake, default 0

Events:
  OnOpen - Triggered after sucсessful connection.
  OnClose - Triggered after disconnection.
}
unit DataPortSerial;

interface

uses
  {$IFNDEF MSWINDOWS}
    {$IFNDEF NO_LIBC}
    Libc,
    KernelIoctl,
    {$ELSE}
    termio, baseunix, unix,
    {$ENDIF}
    {$IFNDEF FPC}
    Types,
    {$ENDIF}
  {$ELSE}
    Windows,
  {$ENDIF}
  SysUtils, Classes, DataPort, DataPortUART, synaser, synautil;

type
  { TSerialClient - serial port reader/writer, based on Ararat Synapse }
  TSerialClient = class(TThread)
  private
    FSerial: TBlockSerial;
    sFromPort: AnsiString;
    sLastError: string;
    FSafeMode: Boolean;
    FOnIncomingMsgEvent: TMsgEvent;
    FOnErrorEvent: TMsgEvent;
    FOnConnectEvent: TNotifyEvent;
    FDoConfig: Boolean;
    procedure SyncProc();
    procedure SyncProcOnConnect();
  protected
    procedure Execute(); override;
  public
    sPort: string;
    iBaudRate: Integer;
    DataBits: Integer;
    Parity: char;
    StopBits: Integer;
    SoftFlow: Boolean;
    HardFlow: Boolean;
    CalledFromThread: Boolean;
    sToSend: AnsiString;
    property SafeMode: Boolean read FSafeMode write FSafeMode;
    property Serial: TBlockSerial read FSerial;
    property OnIncomingMsgEvent: TMsgEvent read FOnIncomingMsgEvent
      write FOnIncomingMsgEvent;
    property OnErrorEvent: TMsgEvent read FOnErrorEvent write FOnErrorEvent;
    property OnConnectEvent: TNotifyEvent read FOnConnectEvent write FOnConnectEvent;
    { Set port parameters (baud rate, data bits, etc..) }
    procedure Config();
    function SendString(const AData: AnsiString): Boolean;
    procedure SendStream(st: TStream);
  end;

  { TDataPortSerial - serial DataPort }
  TDataPortSerial = class(TDataPortUART)
  private
    FSerialClient: TSerialClient;
  protected
    procedure SetBaudRate(AValue: Integer); override;
    procedure SetDataBits(AValue: Integer); override;
    procedure SetParity(AValue: AnsiChar); override;
    procedure SetStopBits(AValue: TSerialStopBits); override;
    procedure SetSoftFlow(AValue: Boolean); override;
    procedure SetHardFlow(AValue: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    { Open serial DataPort
         InitStr = 'Port,BaudRate,DataBits,Parity,StopBits,SoftFlow,HardFlow'

         Port - COM port name (COM1, /dev/tty01)
         BaudRate - connection speed (50..4000000 bits per second), default 9600
         DataBits - default 8
         Parity - (N - None, O - Odd, E - Even, M - Mark or S - Space) default N
         StopBits - (1, 1.5, 2)
         SoftFlow - Enable XON/XOFF handshake, default 1
         HardFlow - Enable CTS/RTS handshake, default 0 }
    procedure Open(const AInitStr: string = ''); override;
    procedure Close(); override;
    function Push(const AData: AnsiString): Boolean; override;
    class function GetSerialPortNames(): string;

    { Get modem wires status (DSR,CTS,Ring,Carrier) }
    function GetModemStatus(): TModemStatus; override;
    { Set DTR (Data Terminal Ready) signal }
    procedure SetDTR(AValue: Boolean); override;
    { Set RTS (Request to send) signal }
    procedure SetRTS(AValue: Boolean); override;

    property SerialClient: TSerialClient read FSerialClient;

  published
    { Serial port name (COM1, /dev/ttyS01) }
    property Port: string read FPort write FPort;
    { BaudRate - connection speed (50..4000000 bits per second), default 9600 }
    property BaudRate: Integer read FBaudRate write SetBaudRate;
    { DataBits - default 8  (5 for Baudot code, 7 for true ASCII) }
    property DataBits: Integer read FDataBits write SetDataBits;
    { Parity - (N - None, O - Odd, E - Even, M - Mark or S - Space) default N }
    property Parity: AnsiChar read FParity write SetParity;
    { StopBits - (stb1, stb15, stb2), default stb1 }
    property StopBits: TSerialStopBits read FStopBits write SetStopBits;
    { Software flow control, enable XON/XOFF handshake, default 1 }
    property SoftFlow: Boolean read FSoftFlow write SetSoftFlow;
    { Hardware flow control, not supported by many USB adapters
      Enable CTS/RTS handshake, default 0 }
    property HardFlow: Boolean read FHardFlow write SetHardFlow;
    { Minimum bytes in incoming buffer to trigger OnDataAppear }
    property MinDataBytes: Integer read FMinDataBytes write FMinDataBytes;
    property Active;
    property OnDataAppear;
    property OnError;
    property OnOpen;
    property OnClose;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('DataPort', [TDataPortSerial]);
end;

// === TSerialClient ===
procedure TSerialClient.Config();
begin
  FDoConfig := True;
end;

procedure TSerialClient.SyncProc();
begin
  if CalledFromThread then
    Exit;
  //if s:='' then Exit;
  CalledFromThread := True;
  if sFromPort <> '' then
  begin
    if Assigned(Self.FOnIncomingMsgEvent) then
      FOnIncomingMsgEvent(Self, sFromPort);
    sFromPort := '';
  end;
  if sLastError <> '' then
  begin
    if Assigned(Self.FOnErrorEvent) then
      FOnErrorEvent(Self, sLastError);
    Self.Terminate();
  end;
  CalledFromThread := False;
end;

procedure TSerialClient.SyncProcOnConnect();
begin
  if CalledFromThread then
    Exit;
  CalledFromThread := True;
  if Assigned(Self.FOnConnectEvent) then
    Self.FOnConnectEvent(Self);
  CalledFromThread := False;
end;

procedure TSerialClient.Execute();
begin
  sLastError := '';

  FSerial := TBlockSerial.Create();
  try
    Serial.DeadlockTimeout := 3000;
    Serial.Connect(sPort);
    Sleep(1);
    if Serial.LastError = 0 then
    begin
      Serial.Config(iBaudRate, DataBits, Parity, StopBits, SoftFlow, HardFlow);
      FDoConfig := False;
      Sleep(1);
    end;

    if Serial.LastError <> 0 then
    begin
      sLastError := Serial.LastErrorDesc;
      Synchronize(SyncProc);
    end
    else
    begin
      Synchronize(SyncProcOnConnect);
    end;

    while not Terminated do
    begin
      sLastError := '';

      if FDoConfig then
      begin
        Serial.Config(iBaudRate, DataBits, Parity, StopBits, SoftFlow, HardFlow);
        FDoConfig := False;
      end
      else
      begin
        sFromPort := Serial.RecvPacket(100);
      end;

      if (Serial.LastError <> 0) and (Serial.LastError <> ErrTimeout) then
        sLastError := IntToStr(Serial.LastError) + '=' + Serial.LastErrorDesc;
      if (Length(sFromPort) > 0) or (Length(sLastError) > 0) then
        Synchronize(SyncProc);

      Sleep(1);

      if sToSend <> '' then
      begin
        if Serial.CanWrite(10) then
        begin
          Serial.SendString(sToSend);
          if (Serial.LastError <> 0) and (Serial.LastError <> ErrTimeout) then
          begin
            sLastError := Serial.LastErrorDesc;
            Synchronize(SyncProc);
          end
          else
            sToSend := '';
        end;
      end;

    end;
  finally
    FreeAndNil(FSerial);
  end;
end;

function TSerialClient.SendString(const AData: AnsiString): Boolean;
begin
  Result := False;
  if not Assigned(Self.Serial) then
    Exit;
  if SafeMode then
    Self.sToSend := Self.sToSend + AData
  else
  begin
    if Serial.CanWrite(100) then
      Serial.SendString(AData);
    if (Serial.LastError <> 0) and (Serial.LastError <> ErrTimeout) then
    begin
      sLastError := Serial.LastErrorDesc;
      Synchronize(SyncProc);
      Exit;
    end;
  end;
  Result := True;
end;

procedure TSerialClient.SendStream(st: TStream);
var
  ss: TStringStream;
begin
  if not Assigned(Self.Serial) then
    Exit;
  if SafeMode then
  begin
    ss := TStringStream.Create('');
    try
      ss.CopyFrom(st, st.Size);
      Self.sToSend := Self.sToSend + ss.DataString;
    finally
      ss.Free();
    end;
  end
  else
  begin
    Serial.SendStreamRaw(st);
    if Serial.LastError <> 0 then
    begin
      sLastError := Serial.LastErrorDesc;
      Synchronize(SyncProc);
    end;
  end;
end;


{ TDataPortSerial }

constructor TDataPortSerial.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSerialClient := nil;
end;

procedure TDataPortSerial.Open(const AInitStr: string = '');
begin
  inherited Open(AInitStr);

  if Assigned(FSerialClient) then
  begin
    FreeAndNil(FSerialClient);
  end;
  FSerialClient := TSerialClient.Create(True);
  FSerialClient.OnIncomingMsgEvent := Self.OnIncomingMsgHandler;
  FSerialClient.OnErrorEvent := Self.OnErrorHandler;
  FSerialClient.OnConnectEvent := Self.OnConnectHandler;
  FSerialClient.SafeMode := True;

  FSerialClient.sPort := FPort;
  FSerialClient.iBaudRate := FBaudRate;
  FSerialClient.DataBits := FDataBits;
  FSerialClient.Parity := FParity;
  FSerialClient.StopBits := Ord(FStopBits);
  FSerialClient.SoftFlow := FSoftFlow;
  FSerialClient.HardFlow := FHardFlow;

  // Check serial port
  //if Pos(Port, synaser.GetSerialPortNames())=0 then Exit;
  {$IFDEF UNIX}
  // detect FLock file name
  if Pos('tty', Port) > 0 then
  begin
    s := '/var/FLock/LCK..' + Copy(Port, Pos('tty', Port), maxint);
    if FileExists(s) then
    begin
      // try to remove FLock file (if any)
      DeleteFile(s);
    end;
  end;
  {$ENDIF}
  FSerialClient.Suspended := False;
end;

procedure TDataPortSerial.Close();
begin
  if Assigned(FSerialClient) then
  begin
    if FSerialClient.CalledFromThread then
      FSerialClient.Terminate()
    else
      FreeAndNil(FSerialClient);
  end;
  inherited Close();
end;

destructor TDataPortSerial.Destroy();
begin
  if Assigned(FSerialClient) then
  begin
    FSerialClient.OnIncomingMsgEvent := nil;
    FSerialClient.OnErrorEvent := nil;
    FSerialClient.OnConnectEvent := nil;
    FreeAndNil(FSerialClient);
  end;
  inherited Destroy();
end;

class function TDataPortSerial.GetSerialPortNames: string;
begin
  Result := synaser.GetSerialPortNames();
end;

function TDataPortSerial.GetModemStatus(): TModemStatus;
var
  ModemWord: Integer;
begin
  if Assigned(SerialClient) and Assigned(SerialClient.Serial) then
  begin
    ModemWord := SerialClient.Serial.ModemStatus();
    {$IFNDEF MSWINDOWS}
    FModemStatus.DSR := (ModemWord and TIOCM_DSR) > 0;
    FModemStatus.CTS := (ModemWord and TIOCM_CTS) > 0;
    FModemStatus.Carrier := (ModemWord and TIOCM_CAR) > 0;
    FModemStatus.Ring := (ModemWord and TIOCM_RNG) > 0;
    {$ELSE}
    FModemStatus.DSR := (ModemWord and MS_DSR_ON) > 0;
    FModemStatus.CTS := (ModemWord and MS_CTS_ON) > 0;
    FModemStatus.Carrier := (ModemWord and MS_RLSD_ON) > 0;
    FModemStatus.Ring := (ModemWord and MS_RING_ON) > 0;
    {$ENDIF}
  end;
  Result := inherited GetModemStatus;
end;

procedure TDataPortSerial.SetDTR(AValue: Boolean);
begin
  if Assigned(SerialClient) and Assigned(SerialClient.Serial) then
  begin
    SerialClient.Serial.DTR := AValue;
  end;
  inherited SetDTR(AValue);
end;

procedure TDataPortSerial.SetRTS(AValue: Boolean);
begin
  if Assigned(SerialClient) and Assigned(SerialClient.Serial) then
  begin
    SerialClient.Serial.RTS := AValue;
  end;
  inherited SetRTS(AValue);
end;

function TDataPortSerial.Push(const AData: AnsiString): Boolean;
begin
  Result := False;
  if Assigned(SerialClient) and FLock.BeginWrite() then
  begin
    try
      SerialClient.SendString(AData);
    finally
      FLock.EndWrite();
    end;
  end;
end;

procedure TDataPortSerial.SetBaudRate(AValue: Integer);
begin
  inherited SetBaudRate(AValue);
  if Active then
  begin
    SerialClient.iBaudRate := FBaudRate;
    SerialClient.Config();
  end;
end;

procedure TDataPortSerial.SetDataBits(AValue: Integer);
begin
  inherited SetDataBits(AValue);
  if Active then
  begin
    SerialClient.DataBits := FDataBits;
    SerialClient.Config();
  end;
end;

procedure TDataPortSerial.SetParity(AValue: AnsiChar);
begin
  inherited SetParity(AValue);
  if Active then
  begin
    SerialClient.Parity := FParity;
    SerialClient.Config();
  end;
end;

procedure TDataPortSerial.SetHardFlow(AValue: Boolean);
begin
  inherited SetHardFlow(AValue);
  if Active then
  begin
    SerialClient.HardFlow := FHardFlow;
    SerialClient.Config();
  end;
end;

procedure TDataPortSerial.SetSoftFlow(AValue: Boolean);
begin
  inherited SetSoftFlow(AValue);
  if Active then
  begin
    SerialClient.SoftFlow := FSoftFlow;
    SerialClient.Config();
  end;
end;

procedure TDataPortSerial.SetStopBits(AValue: TSerialStopBits);
begin
  inherited SetStopBits(AValue);
  if Active then
  begin
    case FStopBits of
      stb1:  SerialClient.StopBits := SB1;
      stb15: SerialClient.StopBits := SB1andHalf;
      stb2:  SerialClient.StopBits := SB2;
    end;
    SerialClient.Config();
  end;
end;

end.

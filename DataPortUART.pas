{
Serial communication port (UART). In Windows it COM-port, real or virtual.
In Linux it /dev/ttyS or /dev/ttyUSB. Also, Linux use file /var/FLock/LCK..ttyS for port FLocking

(C) Sergey Bodrov, 2012-2025

Properties:
  Port - port name (COM1, /dev/ttyS01)
  BaudRate - data excange speed
  DataBits - default 8  (5 for Baudot code, 7 for true ASCII)
  Parity - (N - None, O - Odd, E - Even, M - Mark or S - Space) default N
  StopBits - (stb1, stb15, stb2), default stb1
  FlowControl - (sfcNone, sfcSend, sfcReady, sfcSoft) default sfcNone
    sfcSend - SEND signal pair CTS/RTS, used for hardware flow control
    sfcReady - READY signal pair DTR/DSR, used for modem control
    sfcSoft - software flow control XON/XOFF byte ($11 for resume and $13 for pause transmission)

  MinDataBytes - minimal bytes count in buffer for triggering event OnDataAppear

Methods:
  Open() - Opens port. As parameter it use port initialization string:
    InitStr = 'Port,BaudRate,DataBits,Parity,StopBits,SoftFlow,HardFlow'

    Port - COM port name (COM1, /dev/ttyS01)
    BaudRate - connection speed (50..4000000 bits per second), default 9600
    DataBits - default 8
    Parity - (N - None, O - Odd, E - Even, M - Mark or S - Space) default N
    StopBits - (1, 1.5, 2) default 0
    SoftFlow - Enable XON/XOFF handshake, default 0
    HardFlow - Enable CTS/RTS handshake, default 0

Events:
  OnOpen - Triggered after sucсessful connection.
  OnClose - Triggered after disconnection.

Roles:
  Data Terminal Equipment (DTE) - computer terminal
  Data Circuit-terminating Equipment (DCE) - modem, peripreral device
}
unit DataPortUART;

interface

uses
  SysUtils, Classes, DataPort, DataPortEventer;

type
  TSerialStopBits = (stb1, stb15, stb2);

  TSerialFlowControl = (sfcNone, sfcSend, sfcReady, sfcSoft);

  TModemStatus = record
    { RTS (Request to send) signal (w) - DTE requests the DCE prepare to transmit data. }
    { RTR (Ready To Receive) (w) - DTE is ready to receive data from DCE. If in use, RTS is assumed to be always asserted. }
    RTS: Boolean;
    { CTS (Clear to send) signal (r) - DCE is ready to accept data from the DTE. }
    CTS: boolean;
    { DTR (Data Terminal Ready) signal (w) - DTE is ready to receive, initiate, or continue a call. }
    DTR: Boolean;
    { DSR (Data Set Ready) signal (r) - DCE is ready to receive and send data. }
    DSR: Boolean;
    { Data Carrier Detect (r) - DCE is receiving a carrier from a remote DCE. }
    Carrier: Boolean;
    { Ring Indicator (r) - DCE has detected an incoming ring signal on the telephone line. }
    Ring: Boolean;
  end;

  { TDataPortUART - serial DataPort }
  TDataPortUART = class(TDataPort)
  private
    FOnModemStatus: TNotifyEvent;
    FOnDataAppearUnsafe: TNotifyEvent;
    procedure SetHardFlow(AValue: Boolean);
    procedure SetSoftFlow(AValue: Boolean);
  protected
    FReadDataStr: AnsiString;
    FLock: TSimpleRWSync;
    FPort: string;
    FBaudRate: Integer;
    FDataBits: Integer;
    FParity: AnsiChar;
    FStopBits: TSerialStopBits;
    FFlowControl: TSerialFlowControl;
    FSoftFlow: Boolean;
    FHardFlow: Boolean;
    FMinDataBytes: Integer;
    FHalfDuplex: Boolean;
    FModemStatus: TModemStatus;
    procedure SetBaudRate(AValue: Integer); virtual;
    procedure SetDataBits(AValue: Integer); virtual;
    procedure SetParity(AValue: AnsiChar); virtual;
    procedure SetStopBits(AValue: TSerialStopBits); virtual;
    procedure SetFlowControl(AValue: TSerialFlowControl); virtual;
    // called from inner thread!
    procedure OnIncomingMsgHandler(Sender: TObject; const AMsg: AnsiString); virtual;
    procedure OnErrorHandler(Sender: TObject; const AMsg: AnsiString); virtual;
    procedure OnConnectHandler(Sender: TObject); virtual;
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
         SoftFlow - Enable XON/XOFF handshake, default 0
         HardFlow - Enable CTS/RTS handshake, default 0 }
    procedure Open(const AInitStr: string = ''); override;
    function Pull(ASize: Integer = MaxInt): AnsiString; override;
    function Peek(ASize: Integer = MaxInt): AnsiString; override;
    function PeekSize(): Cardinal; override;

    { Get modem wires status (DSR,CTS,Ring,Carrier) }
    function GetModemStatus(): TModemStatus; virtual;
    { Set DTR (Data Terminal Ready) signal }
    procedure SetDTR(AValue: Boolean); virtual;
    { Set RTS (Request to send) signal }
    procedure SetRTS(AValue: Boolean); virtual;
    { Modem wires status }
    property ModemStatus: TModemStatus read FModemStatus;
  published
    { Serial port name (COM1, /dev/ttyS01) }
    property Port: string read FPort write FPort nodefault;
    { BaudRate - connection speed (50..4000000 bits per second), default 9600 }
    property BaudRate: Integer read FBaudRate write SetBaudRate default 9600;
    { DataBits - default 8  (5 for Baudot code, 7 for true ASCII) }
    property DataBits: Integer read FDataBits write SetDataBits default 8;
    { Parity - (N - None, O - Odd, E - Even, M - Mark or S - Space) default N }
    property Parity: AnsiChar read FParity write SetParity default 'N';
    { StopBits - (stb1, stb15, stb2), default stb1 }
    property StopBits: TSerialStopBits read FStopBits write SetStopBits default stb1;
    { Flow control - (sfcNone, sfcRTS, sfcDTR, sfcXON) default sfcNone
      sfcSend - SEND signal pair CTS/RTS, used for hardware flow control
      sfcReady - READY signal pair DTR/DSR, used for modem control
      sfcSoft - software flow control XON/XOFF byte ($11 for resume and $13 for pause transmission) }
    property FlowControl: TSerialFlowControl read FFlowControl write SetFlowControl default sfcNone;
    { deprecated, set to False and use FlowControl }
    property SoftFlow: Boolean read FSoftFlow write SetSoftFlow; {$ifdef FPC}deprecated;{$endif}
    { deprecated, set to False and use FlowControl }
    property HardFlow: Boolean read FHardFlow write SetHardFlow; {$ifdef FPC}deprecated;{$endif}
    { Minimum bytes in incoming buffer to trigger OnDataAppear }
    property MinDataBytes: Integer read FMinDataBytes write FMinDataBytes default 1;
    { Use half-duplex for send and receive data }
    property HalfDuplex: Boolean read FHalfDuplex write FHalfDuplex default False;
    property Active;
    property OnDataAppear;
    property OnError;
    property OnOpen;
    property OnClose;
    { Triggered when modem status changed }
    property OnModemStatus: TNotifyEvent read FOnModemStatus write FOnModemStatus;
    { Triggered when data appeared (not thread-safe, called from inner thread!) }
    property OnDataAppearUnsafe: TNotifyEvent read FOnDataAppearUnsafe write FOnDataAppearUnsafe;
  end;

  function ExtractFirstWord(var s: string; const delimiter: string = ' '): string;

implementation

function ExtractFirstWord(var s: string; const delimiter: string = ' '): string;
var
  i: Integer;
begin
  Result := '';
  i := Pos(delimiter, s);
  if i > 0 then
  begin
    Result := Copy(s, 1, i - 1);
    s := Copy(s, i + 1, maxint);
  end
  else
  begin
    Result := s;
    s := '';
  end;
end;

{ TDataPortUART }

constructor TDataPortUART.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TSimpleRWSync.Create();
  FPort := 'COM1';
  FBaudRate := 9600;
  FDataBits := 8;
  FParity := 'N';
  FStopBits := stb1;
  FFlowControl := sfcNone;
  FMinDataBytes := 1;
  FActive := False;
  //Self.slReadData := TStringList.Create();
  FReadDataStr := '';
  RegisterDataportNotify(Self);
end;

procedure TDataPortUART.Open(const AInitStr: string = '');
var
  s, ss: string;
begin
  ss := AInitStr;

  // Port
  s := ExtractFirstWord(ss, ',');
  if s <> '' then
    FPort := s;

  // BaudRate
  s := ExtractFirstWord(ss, ',');
  FBaudRate := StrToIntDef(s, FBaudRate);

  // DataBits
  s := ExtractFirstWord(ss, ',');
  FDataBits := StrToIntDef(s, FDataBits);

  // Parity
  s := ExtractFirstWord(ss, ',');
  if s <> '' then
    FParity := AnsiChar(s[1]);
  if Pos(FParity, 'NOEMSnoems') = 0 then
    FParity := 'N';

  // StopBits
  s := ExtractFirstWord(ss, ',');
  if s = '1' then
    FStopBits := stb1
  else if s = '1.5' then
    FStopBits := stb15
  else if s = '2' then
    FStopBits := stb2;

  FFlowControl := sfcNone;
  // SoftFlow
  s := ExtractFirstWord(ss, ',');
  if s = '1' then
    FFlowControl := sfcSoft;

  // HardFlow
  s := ExtractFirstWord(ss, ',');
  if s = '1' then
    FFlowControl := sfcSend;

  // don't inherits Open() - OnOpen event will be after successfull connection
end;

destructor TDataPortUART.Destroy();
begin
  UnRegisterDataportNotify(Self);
  FreeAndNil(FLock);
  inherited Destroy();
end;

procedure TDataPortUART.OnIncomingMsgHandler(Sender: TObject; const AMsg: AnsiString);
begin
  if AMsg <> '' then
  begin
    if FLock.BeginWrite then
    begin
      try
        FReadDataStr := FReadDataStr + AMsg;
      finally
        FLock.EndWrite;
      end;

      NotifyDataport(Self, DP_NOTIFY_DATA);

      if Assigned(OnDataAppearUnsafe) then
        OnDataAppearUnsafe(Self);
    end;
  end
  else
  begin
    FModemStatus := GetModemStatus();
    if Assigned(OnModemStatus) then
      OnModemStatus(Self);
  end;
end;

procedure TDataPortUART.OnErrorHandler(Sender: TObject; const AMsg: AnsiString);
begin
  FActive := False;
  if (AMsg <> '') and Assigned(OnError) then
    //OnError(Self, AMsg)
    NotifyDataport(Self, DP_NOTIFY_ERROR, AMsg)
  else if Assigned(OnClose) then
    NotifyDataport(Self, DP_NOTIFY_CLOSE)
    //OnClose(Self);
end;

procedure TDataPortUART.OnConnectHandler(Sender: TObject);
begin
  FActive := True;
  if Assigned(OnOpen) then
    //OnOpen(Self);
    NotifyDataport(Self, DP_NOTIFY_OPEN);
end;

function TDataPortUART.Peek(ASize: Integer): AnsiString;
begin
  FLock.BeginRead();
  try
    Result := Copy(FReadDataStr, 1, ASize);
  finally
    FLock.EndRead();
  end;
end;

function TDataPortUART.PeekSize(): Cardinal;
begin
  FLock.BeginRead();
  try
    Result := Cardinal(Length(FReadDataStr));
  finally
    FLock.EndRead();
  end;
end;

function TDataPortUART.GetModemStatus(): TModemStatus;
begin
  Result := FModemStatus;
end;

procedure TDataPortUART.SetDTR(AValue: Boolean);
begin
  FModemStatus.DTR := AValue;
end;

procedure TDataPortUART.SetRTS(AValue: Boolean);
begin
  FModemStatus.RTS := AValue;
end;

function TDataPortUART.Pull(ASize: Integer): AnsiString;
begin
  Result := '';
  if FLock.BeginWrite() then
  begin
    try
      Result := Copy(FReadDataStr, 1, ASize);
      Delete(FReadDataStr, 1, ASize);
    finally
      FLock.EndWrite();
    end;
  end;
end;

procedure TDataPortUART.SetHardFlow(AValue: Boolean);
begin
  FHardFlow := AValue;
  if FHardFlow then
    FFlowControl := sfcSend;
end;

procedure TDataPortUART.SetSoftFlow(AValue: Boolean);
begin
  FSoftFlow := AValue;
  if FSoftFlow then
    FFlowControl := sfcSoft;
end;

procedure TDataPortUART.SetBaudRate(AValue: Integer);
begin
  FBaudRate := AValue;
end;

procedure TDataPortUART.SetDataBits(AValue: Integer);
begin
  if (AValue < 5) or (AValue > 9) then
    Exit;
  FDataBits := AValue;
end;

procedure TDataPortUART.SetFlowControl(AValue: TSerialFlowControl);
begin
  FFlowControl := AValue;
end;

procedure TDataPortUART.SetParity(AValue: AnsiChar);
begin
  if Pos(AValue, 'NOEMSnoems') > 0 then
    FParity := AValue;
end;

procedure TDataPortUART.SetStopBits(AValue: TSerialStopBits);
begin
  FStopBits := AValue;
end;

end.

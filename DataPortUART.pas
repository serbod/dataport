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
unit DataPortUART;

interface

uses
  SysUtils, Classes, DataPort;

type
  TSerialStopBits = (stb1, stb15, stb2);

  { TDataPortUART - serial DataPort }
  TDataPortUART = class(TDataPort)
  protected
    //slReadData: TStringList; // for storing every incoming data packet separately
    FReadDataStr: AnsiString;
    FLock: TMultiReadExclusiveWriteSynchronizer;
    FPort: string;
    FBaudRate: Integer;
    FDataBits: Integer;
    FParity: AnsiChar;
    FStopBits: TSerialStopBits;
    FSoftFlow: Boolean;
    FHardFlow: Boolean;
    FMinDataBytes: Integer;
    procedure SetBaudRate(AValue: Integer); virtual;
    procedure SetDataBits(AValue: Integer); virtual;
    procedure SetParity(AValue: AnsiChar); virtual;
    procedure SetStopBits(AValue: TSerialStopBits); virtual;
    procedure SetSoftFlow(AValue: Boolean); virtual;
    procedure SetHardFlow(AValue: Boolean); virtual;
    procedure OnIncomingMsgHandler(Sender: TObject; const AMsg: string); virtual;
    procedure OnErrorHandler(Sender: TObject; const AMsg: string); virtual;
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
         SoftFlow - Enable XON/XOFF handshake, default 1
         HardFlow - Enable CTS/RTS handshake, default 0 }
    procedure Open(const InitStr: string = ''); override;
    function Pull(size: Integer = MaxInt): AnsiString; override;
    function Peek(size: Integer = MaxInt): AnsiString; override;
    function PeekSize(): Cardinal; override;
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

  function GetFirstWord(var s: string; const delimiter: string = ' '): string;

implementation

function GetFirstWord(var s: string; const delimiter: string = ' '): string;
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
  FLock := TMultiReadExclusiveWriteSynchronizer.Create();
  FPort := 'COM1';
  FBaudRate := 9600;
  FDataBits := 8;
  FParity := 'N';
  FStopBits := stb1;
  FSoftFlow := True;
  FHardFlow := False;
  FMinDataBytes := 1;
  FActive := False;
  //Self.slReadData := TStringList.Create();
  FReadDataStr := '';
end;

procedure TDataPortUART.Open(const InitStr: string = '');
var
  s, ss: string;
begin
  ss := InitStr;

  // Port
  s := GetFirstWord(ss, ',');
  if s <> '' then
    FPort := s;

  // BaudRate
  s := GetFirstWord(ss, ',');
  FBaudRate := StrToIntDef(s, FBaudRate);

  // DataBits
  s := GetFirstWord(ss, ',');
  FDataBits := StrToIntDef(s, FDataBits);

  // Parity
  s := GetFirstWord(ss, ',');
  if s <> '' then
    FParity := s[1];
  if Pos(FParity, 'NOEMSnoems') = 0 then
    FParity := 'N';

  // StopBits
  s := GetFirstWord(ss, ',');
  if s = '1' then
    FStopBits := stb1
  else if s = '1.5' then
    FStopBits := stb15
  else if s = '2' then
    FStopBits := stb2;

  // SoftFlow
  s := GetFirstWord(ss, ',');
  if s = '1' then
    FSoftFlow := True
  else if s = '0' then
    FSoftFlow := False;

  // HardFlow
  s := GetFirstWord(ss, ',');
  if s = '1' then
    FHardFlow := True
  else if s = '0' then
    FHardFlow := False;

  // don't inherits Open() - OnOpen event will be after successfull connection
end;

destructor TDataPortUART.Destroy();
begin
  //FreeAndNil(Self.slReadData);
  FreeAndNil(Self.FLock);
  inherited Destroy();
end;

procedure TDataPortUART.OnIncomingMsgHandler(Sender: TObject; const AMsg: string);
begin
  if AMsg <> '' then
  begin
    if FLock.BeginWrite then
    begin
      //slReadData.Add(AMsg);
      FReadDataStr := FReadDataStr + AMsg;
      FLock.EndWrite;

      if Assigned(FOnDataAppear) then
        FOnDataAppear(Self);
    end;

  end;
end;

procedure TDataPortUART.OnErrorHandler(Sender: TObject; const AMsg: string);
begin
  if Assigned(Self.FOnError) then
    Self.FOnError(Self, AMsg);
  Self.FActive := False;
end;

procedure TDataPortUART.OnConnectHandler(Sender: TObject);
begin
  Self.FActive := True;
  if Assigned(OnOpen) then
    OnOpen(Self);
end;

{
function TDataPortIP.Peek(size: Integer = MaxInt): AnsiString;
var
  i, num, remain: Integer;
begin
  Result := '';
  remain := size;
  FLock.BeginRead();
  try
    for i:=0 to slReadData.Count do
    begin
      num := Length(slReadData[i]);
      if num > remain then
        num := remain;
      Result := Result + Copy(slReadData[i], 1, num);
      remain := remain - num;
      if remain <= 0 then
        Break;
    end;
  finally
    FLock.EndRead();
  end;
end;
}

function TDataPortUART.Peek(size: Integer = MaxInt): AnsiString;
begin
  FLock.BeginRead();
  try
    Result := Copy(FReadDataStr, 1, size);
  finally
    FLock.EndRead();
  end;
end;

function TDataPortUART.PeekSize(): Cardinal;
  //var i: Integer;
begin
  //Result:=0;
  FLock.BeginRead();
  try
    //// Length of all strings
    //for i := 0 to slReadData.Count-1 do
    //  Result := Result + Cardinal(Length(slReadData[i]));
    Result := Cardinal(Length(FReadDataStr));
  finally
    FLock.EndRead();
  end;
end;

{
function TDataPortIP.Pull(size: Integer = MaxInt): AnsiString;
var
  num, len, remain: Integer;
begin
  Result := '';
  remain := size;
  if not FLock.BeginWrite() then
    Exit;
  try
    while slReadData.Count > 0 do
    begin
      // we read every string to exclude line delimiters
      len := Length(slReadData[0]);
      num := len;
      if num > remain then
        num := remain;
      Result := Result + Copy(slReadData[0], 1, num);
      remain := remain - num;
      if num >= len then
        slReadData.Delete(0)
      else
      begin
        Delete(slReadData[0], 1, num);
        Break;
      end;
      if remain <= 0 then Break;
    end;
  finally
    FLock.EndWrite();
  end;
end;
}

function TDataPortUART.Pull(size: Integer = MaxInt): AnsiString;
begin
  Result := '';
  if FLock.BeginWrite() then
  begin
    try
      Result := Copy(FReadDataStr, 1, size);
      Delete(FReadDataStr, 1, size);
    finally
      FLock.EndWrite();
    end;
  end;
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

procedure TDataPortUART.SetHardFlow(AValue: Boolean);
begin
  FHardFlow := AValue;
end;

procedure TDataPortUART.SetParity(AValue: AnsiChar);
begin
  if Pos(AValue, 'NOEMSnoems') > 0 then
    FParity := AValue;
end;

procedure TDataPortUART.SetSoftFlow(AValue: Boolean);
begin
  FSoftFlow := AValue;
end;

procedure TDataPortUART.SetStopBits(AValue: TSerialStopBits);
begin
  FStopBits := AValue;
end;

end.

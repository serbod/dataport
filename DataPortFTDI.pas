unit DataPortFTDI;

{
Serial communication port based on FTD2XX library.

Sergey Bodrov, 2012-2016

Properties:
  SerialNumber      - FTDI device serial number
  DeviceDescription - FTDI device description string
  BaudRate          - data exchange speed (300, 1200, 9600, 115384, 230769, 923076)
  MinDataBytes      - minimal bytes count in buffer for triggering event OnDataAppear

Methods:
  Open() - Opens port. As parameter it use port initialization string:
    InitStr = '<DeviceDescription>:<SerialNumber>'
      Examples:
        'USB Serial:' - first device of 'USB Serial' type
        ':FT425622'   - device with s/n FT425622
  GetFtdiDeviceList() - list of available devices in format <DeviceDescription>:<SerialNumber><LineFeed>

Events:
  OnModemStatus - Triggered when modem status changes (CTS, DTR, RI, DCD)
}

interface

uses SysUtils, Classes, DataPort, D2XXUnit, Types;

type
  TSerialFlowControl = (sfcNone, sfcRTS, sfcDTR, sfcXON);
  TSerialStopBits = (stb1, stb15, stb2);

  { TFtdiClient - FTDI device reader/writer thread }
  TFtdiClient = class(TThread)
  private
    sFromPort: ansistring;
    sLastError: string;
    FSafeMode: boolean;
    FOnIncomingMsgEvent: TMsgEvent;
    FOnErrorEvent: TMsgEvent;
    FOnConnectEvent: TNotifyEvent;
    FDoConfig: boolean;
    { how many bytes read for once, default 64k }
    FReadCount: integer;

    FFtHandle: DWord;
    FFtIOStatus: FT_Result;
    { bytes in receive queue }
    FFtRxQBytes: DWord;
    { bytes in transmit queue }
    FFtTxQBytes: DWord;
    { wakeup event status }
    FFtEventStatus: DWord;
    { input buffer }
    FFtInBuffer: array[0..FT_In_Buffer_Index] of byte;
    { output buffer }
    FFtOutBuffer: array[0..FT_Out_Buffer_Index] of byte;
    procedure SyncProc();
    procedure SyncProcOnConnect();
    function SendStringInternal(AData: string): integer;
    procedure ClosePort();
    function CheckFtError(APortStatus: FT_Result; AFunctionName: string = ''): boolean;
    function GetFtErrorDescription(APortStatus: FT_Result): string;
  protected
    procedure Execute(); override;
  public
    //Serial: TBlockSerial;
    InitStr: string;
    CalledFromThread: boolean;
    sToSend: ansistring;
    // port properties
    FFtBaudRate: DWord;
    FFtDataBits: byte;
    FFtStopBits: byte;
    FFtParity: byte;
    FFtFlowControl: word;
    property SafeMode: boolean read FSafeMode write FSafeMode;
    property OnIncomingMsgEvent: TMsgEvent read FOnIncomingMsgEvent
      write FOnIncomingMsgEvent;
    property OnError: TMsgEvent read FOnErrorEvent write FOnErrorEvent;
    property OnConnect: TNotifyEvent read FOnConnectEvent write FOnConnectEvent;
    { Set port parameters (baud rate, data bits, etc..) }
    procedure Config();
    function SendString(AData: ansistring): boolean;
  end;

  { TDataPortFtdi }

  TDataPortFtdi = class(TDataPort)
  private
    sReadData: ansistring;
    lock: TMultiReadExclusiveWriteSynchronizer;
    FFtSerialNumber: string;
    FFtDeviceDescription: string;
    FMinDataBytes: integer;
    FBaudRate: integer;
    FFlowControl: TSerialFlowControl;
    FParity: AnsiChar;
    FStopBits: TSerialStopBits;
    FOnModemStatus: TNotifyEvent;
    procedure FSetBaudRate(AValue: integer);
    procedure FSetDataBits(AValue: integer);
    procedure FSetFlowControl(AValue: TSerialFlowControl);
    procedure FSetParity(AValue: AnsiChar);
    procedure FSetStopBits(AValue: TSerialStopBits);
    procedure IncomingMsgHandler(Sender: TObject; AMsg: string);
    procedure OnErrorHandler(Sender: TObject; AMsg: string);
    procedure OnConnectHandler(Sender: TObject);
  protected
    FtdiClient: TFtdiClient;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    { Open FTDI device for data transmission
      InitStr = '<DeviceDescription>:<SerialNumber>'
      Examples:
        'USB Serial:' - first device of 'USB Serial' type
        ':FT425622'   - device with s/n FT425622
      List of available devices can be acquired in GetFtdiDeviceList()
    }
    procedure Open(InitStr: string = ''); override;
    procedure Close(); override;
    function Push(sMsg: ansistring): boolean; override;
    function Pull(size: integer = MaxInt): ansistring; override;
    function Peek(size: integer = MaxInt): ansistring; override;
    function PeekSize(): cardinal; override;
    class function GetFtdiDeviceList(): string;
    //class function GetFtdiDriverVersion(): string;
  published
    property Active;
    { FTDI device serial number }
    property SerialNumber: string read FFtSerialNumber write FFtSerialNumber;
    { FTDI device description string }
    property DeviceDescription: string read FFtDeviceDescription
      write FFtDeviceDescription;
    { data exchange speed (300, 1200, 9600, 115384, 230769, 923076) }
    property BaudRate: integer read FBaudRate write FSetBaudRate;
    { minimal bytes count in buffer for triggering event OnDataAppear }
    property MinDataBytes: integer read FMinDataBytes write FMinDataBytes;
    { Triggered when modem status changes (CTS, DTR, RI, DCD) }
    property OnModemStatus: TNotifyEvent read FOnModemStatus write FOnModemStatus;
    property OnDataAppear;
    property OnError;
    property OnOpen;
    property OnClose;
  end;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('DataPort', [TDataPortFtdi]);
end;

function GetFirstWord(var s: string; delimiter: string = ' '): string;
var
  i: integer;
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


{ TFtdiClient }

procedure TFtdiClient.Config;
begin
  FDoConfig := True;
end;

function TFtdiClient.CheckFtError(APortStatus: FT_Result;
  AFunctionName: string): boolean;
begin
  Result := True;
  if APortStatus <> FT_OK then
  begin
    sLastError := AFunctionName + ': ' + GetFtErrorDescription(APortStatus);
    Synchronize(SyncProc);
    Result := False;
  end;
end;

function TFtdiClient.SendStringInternal(AData: string): integer;
var
  WriteCount, WriteResult: integer;
begin
  Result := 0;
  FFtIOStatus := FT_GetStatus(FFtHandle, @FFtRxQBytes, @FFtTxQBytes, @FFtEventStatus);
  if FFtIOStatus = FT_OK then
  begin
    WriteCount := Length(AData);
    if WriteCount > Length(FFtOutBuffer) then
      WriteCount := Length(FFtOutBuffer);
    Move(AData, FFtOutBuffer, WriteCount);
    // Writes Write_Count Bytes from FT_Out_Buffer to the USB device
    // Function returns the number of bytes actually sent
    // In this example, Write_Count should be 64k bytes max
    FFtIOStatus := FT_Write(FFtHandle, @FFtOutBuffer, WriteCount, @WriteResult);
    if FFtIOStatus = FT_OK then
    begin
      Result := WriteResult;
    end;
  end;
  if FFtIOStatus <> FT_OK then
  begin
    sLastError := 'FT_Write: ' + GetFtErrorDescription(FFtIOStatus);
    Synchronize(SyncProc);
    Exit;
  end;
end;

procedure TFtdiClient.Execute;
var
  PortStatus: FT_Result;
  DeviceString: ansistring;
  s, ss: string;
  FtDeviceStringBuffer: array [1..50] of AnsiChar;
  ReadCount, ReadResult, WriteResult: integer;
  ReadTimeout, WriteTimeout: DWORD;

  procedure ClosePortAndExit();
  begin
    PortStatus := FT_Close(FFtHandle);
    Exit;
  end;

begin
  // Default settings
  sLastError := '';
  DeviceString := '';
  FDoConfig := False;
  FFtHandle := FT_INVALID_HANDLE;
  FFtBaudRate := FT_BAUD_9600;
  FFtDataBits := FT_DATA_BITS_8;
  FFtStopBits := FT_STOP_BITS_1;
  FFtParity := FT_PARITY_NONE;
  FFtFlowControl := FT_FLOW_NONE;
  FReadCount := SizeOf(FFtInBuffer);

  // parse InitStr, open port
  ss := InitStr;
  s := GetFirstWord(ss, ':');
  if Length(s) > 0 then
  begin
    DeviceString := s;
    FillChar(FtDeviceStringBuffer, SizeOf(FtDeviceStringBuffer), 0);
    Move(DeviceString[1], FtDeviceStringBuffer, Length(DeviceString));
    PortStatus := FT_OpenEx(@FtDeviceStringBuffer, FT_OPEN_BY_DESCRIPTION, @FFtHandle);
    CheckFtError(PortStatus, 'FT_OpenEx');
  end
  else
  begin
    s := GetFirstWord(ss, ':');
    if Length(s) > 0 then
    begin
      DeviceString := s;
      FillChar(FtDeviceStringBuffer, SizeOf(FtDeviceStringBuffer), 0);
      Move(DeviceString[1], FtDeviceStringBuffer, Length(DeviceString));
      PortStatus := FT_OpenEx(@FtDeviceStringBuffer, FT_OPEN_BY_SERIAL_NUMBER,
        @FFtHandle);
      CheckFtError(PortStatus, 'FT_OpenEx');
    end
    else
      Exit;
  end;

  // Device handle acquired, we must release it in any case
  try
    // This function sends a reset command to the device.
    PortStatus := FT_ResetDevice(FFtHandle);
    if not CheckFtError(PortStatus, 'FT_ResetDevice') then
      Exit;

    if FDoConfig then
    begin
      // set BaudRate
      PortStatus := FT_SetBaudRate(FFtHandle, FFtBaudRate);
      if not CheckFtError(PortStatus, 'FT_SetBaudRate') then
        Exit;

      // set data characteristics
      PortStatus := FT_SetDataCharacteristics(FFtHandle, FFtDataBits,
        FFtStopBits, FFtParity);
      if not CheckFtError(PortStatus, 'FT_SetDataCharacteristics') then
        Exit;

      // set flow control
      PortStatus := FT_SetFlowControl(FFtHandle, FFtFlowControl,
        FT_XON_Value, FT_XOFF_Value);
      if not CheckFtError(PortStatus, 'FT_SetFlowControl') then
        Exit;
    end;

    ReadTimeout := 10;
    WriteTimeout := 10;
    // This function sets the read and write timeouts (in milliseconds) for the device
    PortStatus := FT_SetTimeouts(FFtHandle, ReadTimeout, WriteTimeout);
    if not CheckFtError(PortStatus, 'FT_SetTimeouts') then
      Exit;

    // This function purges receive and transmit buffers in the device.
    PortStatus := FT_Purge(FFtHandle, (FT_PURGE_RX + FT_PURGE_TX));
    if not CheckFtError(PortStatus, 'FT_Purge') then
      Exit;

    Synchronize(SyncProcOnConnect);

    while not Terminated do
    begin
      // Reads Read_Count Bytes (or less) from the USB device to the FT_In_Buffer
      // Function returns the number of bytes actually received  which may range from zero
      // to the actual number of bytes requested, depending on how many have been received
      // at the time of the request + the read timeout value.
      sFromPort := '';
      ReadCount := FReadCount;
      if (ReadCount = 1) then
      begin
        ReadResult := ReadCount;
      end;

      FFtIOStatus := FT_GetStatus(FFtHandle, @FFtRxQBytes, @FFtTxQBytes,
        @FFtEventStatus);
      if CheckFtError(FFtIOStatus, 'FT_GetStatus') and (FFtRxQBytes > 0) then
      begin
        FFtIOStatus := FT_Read(FFtHandle, @FFtInBuffer, ReadCount, @ReadResult);
        if CheckFtError(FFtIOStatus, 'FT_Read') and (ReadResult > 0) then
        begin
          // copy input buffer to string
          SetLength(sFromPort, ReadResult);
          Move(FFtInBuffer, sFromPort[1], ReadResult);
          Synchronize(SyncProc);
        end;
      end;

      // safe mode write
      if Length(sToSend) > 0 then
      begin
        WriteResult := SendStringInternal(sToSend);
        Delete(sToSend, 1, WriteResult);
      end;
      Sleep(1);
    end;

  finally
    PortStatus := FT_Close(FFtHandle);
  end;
end;

function TFtdiClient.GetFtErrorDescription(APortStatus: FT_Result): string;
begin
  Result := '';
  if APortStatus = FT_OK then
    Exit;
  case APortStatus of
    FT_INVALID_HANDLE: Result := 'Invalid handle';
    FT_DEVICE_NOT_FOUND: Result := 'Device not found';
    FT_DEVICE_NOT_OPENED: Result := 'Device not opened';
    FT_IO_ERROR: Result := 'General IO error';
    FT_INSUFFICIENT_RESOURCES: Result := 'Insufficient resources';
    FT_INVALID_PARAMETER: Result := 'Invalid parameter';
    FT_INVALID_BAUD_RATE: Result := 'Invalid baud rate';
    FT_DEVICE_NOT_OPENED_FOR_ERASE: Result := 'Device not opened for erase';
    FT_DEVICE_NOT_OPENED_FOR_WRITE: Result := 'Device not opened for write';
    FT_FAILED_TO_WRITE_DEVICE: Result := 'Failed to write';
    FT_EEPROM_READ_FAILED: Result := 'EEPROM read failed';
    FT_EEPROM_WRITE_FAILED: Result := 'EEPROM write failed';
    FT_EEPROM_ERASE_FAILED: Result := 'EEPROM erase failed';
    FT_EEPROM_NOT_PRESENT: Result := 'EEPROM not present';
    FT_EEPROM_NOT_PROGRAMMED: Result := 'EEPROM not programmed';
    FT_INVALID_ARGS: Result := 'Invalid arguments';
    FT_OTHER_ERROR: Result := 'Other error';
    else
      Result := 'Unknown error';
  end;
end;

function TFtdiClient.SendString(AData: ansistring): boolean;
var
  WriteResult: integer;
begin
  Result := False;
  if SafeMode then
  begin
    self.sToSend := AData;
    Result := True;
  end
  else
  begin
    WriteResult := SendStringInternal(AData);
    Result := (WriteResult = Length(AData));
  end;
end;

procedure TFtdiClient.SyncProc;
begin
  if CalledFromThread then
    Exit;
  //if s:='' then Exit;
  CalledFromThread := True;
  if sFromPort <> '' then
  begin
    if Assigned(self.FOnIncomingMsgEvent) then
      FOnIncomingMsgEvent(self, sFromPort);
    sFromPort := '';
  end;
  if sLastError <> '' then
  begin
    if Assigned(self.FOnErrorEvent) then
      FOnErrorEvent(self, sLastError);
    self.Terminate();
  end;
  CalledFromThread := False;
end;

procedure TFtdiClient.SyncProcOnConnect;
begin
  if CalledFromThread then
    Exit;
  CalledFromThread := True;
  if Assigned(self.FOnConnectEvent) then
    self.FOnConnectEvent(self);
  CalledFromThread := False;
end;


procedure TFtdiClient.ClosePort();
begin
  FT_Close(FFtHandle);
end;

{ TDataPortFtdi }

constructor TDataPortFtdi.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.lock := TMultiReadExclusiveWriteSynchronizer.Create();
  sReadData := '';
  FActive := False;
  Self.FtdiClient := nil;
end;

procedure TDataPortFtdi.Open(InitStr: string = '');
var
  ss: string;
begin
  // Set host and port from init string
  if InitStr <> '' then
  begin
    ss := InitStr;
    Self.FFtDeviceDescription := GetFirstWord(ss, ':');

    Self.FFtSerialNumber := GetFirstWord(ss, ':');
  end;

  if Assigned(self.FtdiClient) then
    FreeAndNil(self.FtdiClient);
  Self.FtdiClient := TFtdiClient.Create(True);
  Self.FtdiClient.InitStr := FFtDeviceDescription + ':' + FFtSerialNumber;
  Self.FtdiClient.SafeMode := True;
  Self.FtdiClient.OnIncomingMsgEvent := self.IncomingMsgHandler;
  Self.FtdiClient.OnError := Self.OnErrorHandler;
  Self.FtdiClient.OnConnect := Self.OnConnectHandler;
  Self.FtdiClient.Suspended := False;
  Self.FActive := True;

  // don't inherits Open() - OnOpen event will be after successfull connection
end;

procedure TDataPortFtdi.Close();
begin
  if Assigned(self.FtdiClient) then
    FreeAndNil(self.FtdiClient);
  inherited Close();
end;

destructor TDataPortFtdi.Destroy();
begin
  if Assigned(self.FtdiClient) then
    FreeAndNil(self.FtdiClient);
  FreeAndNil(self.lock);
  inherited Destroy();
end;

procedure TDataPortFtdi.IncomingMsgHandler(Sender: TObject; AMsg: string);
begin
  if AMsg <> '' then
  begin
    // Remove 2 status bytes
    Delete(AMsg, 1, 2);

    if lock.BeginWrite then
    begin
      sReadData := sReadData + AMsg;
      lock.EndWrite;

      if Length(sReadData) >= FMinDataBytes then
      begin
        if Assigned(FOnDataAppear) then
          FOnDataAppear(self);
      end;
    end;

  end;
end;

procedure TDataPortFtdi.OnErrorHandler(Sender: TObject; AMsg: string);
begin
  if Assigned(Self.FOnError) then
    Self.FOnError(Self, AMsg);
end;

function TDataPortFtdi.Peek(size: integer = MaxInt): ansistring;
begin
  lock.BeginRead();
  Result := Copy(sReadData, 1, size);
  lock.EndRead();
end;

function TDataPortFtdi.PeekSize(): cardinal;
begin
  lock.BeginRead();
  Result := cardinal(Length(sReadData));
  lock.EndRead();
end;

function TDataPortFtdi.Pull(size: integer = MaxInt): ansistring;
begin
  Result := '';
  if not lock.BeginWrite() then
    Exit;
  Result := Copy(sReadData, 1, size);
  Delete(sReadData, 1, size);
  //sReadData:='';
  lock.EndWrite();
end;

function TDataPortFtdi.Push(sMsg: ansistring): boolean;
begin
  Result := False;
  if not lock.BeginWrite() then
    Exit;
  if Assigned(self.FtdiClient) then
  begin
    self.FtdiClient.SendString(sMsg);
    Result := True;
  end;
  lock.EndWrite();
end;

procedure TDataPortFtdi.OnConnectHandler(Sender: TObject);
begin
  if Assigned(OnOpen) then
    OnOpen(Self);
end;

class function TDataPortFtdi.GetFtdiDeviceList(): string;
const
  FT_DEVICE_232BM = 0;
  FT_DEVICE_232AM = 1;
  FT_DEVICE_100AX = 2;
  FT_DEVICE_UNKNOWN = 3;
  FT_DEVICE_2232C = 4;
  FT_DEVICE_232R = 5;
  FT_DEVICE_2232H = 6;
  FT_DEVICE_4232H = 7;
  FT_DEVICE_232H = 8;
  FT_DEVICE_X_SERIES = 9;
var
  FtDeviceCount, DeviceIndex: DWord;
  PortStatus: FT_Result;
  DeviceInfo: FT_Device_Info_Node;
  //FtDeviceInfoList: array of FT_Device_Info_Node;
  i: integer;
  sDevType: string;
begin
  Result := '';
  //PortStatus := FT_GetNumDevices(@FtDeviceCount, nil, FT_LIST_NUMBER_ONLY);
  //if PortStatus <> FT_OK then Exit;

  PortStatus := FT_CreateDeviceInfoList(@FtDeviceCount);
  if PortStatus <> FT_OK then
    Exit;

  //SetLength(FT_DeviceInfoList, FtDeviceCount);
  //PortStatus :=  FT_GetDeviceInfoList(FtDeviceInfoList, @FtDeviceCount);
  //if PortStatus <> FT_OK then Exit;

  for i := 0 to FtDeviceCount - 1 do
  begin
    DeviceIndex := i;
    DeviceInfo.Flags := 0;
    DeviceInfo.DeviceType := 0;
    DeviceInfo.ID := 0;
    DeviceInfo.LocID := 0;
    DeviceInfo.SerialNumber := '';
    DeviceInfo.Description := '';
    DeviceInfo.DeviceHandle := 0;

    PortStatus := FT_GetDeviceInfoDetail(DeviceIndex,
      @DeviceInfo.Flags, @DeviceInfo.DeviceType, @DeviceInfo.ID,
      @DeviceInfo.LocID, @DeviceInfo.SerialNumber, @DeviceInfo.Description,
      @DeviceInfo.DeviceHandle);
    if PortStatus = FT_OK then
    begin
      if (DeviceInfo.Flags and $1) > 0 then
        Continue; // device busy
      //if (DeviceInfo.Flags and $2) > 0 then; // HighSpeed device
      case DeviceInfo.DeviceType of
        FT_DEVICE_232BM: sDevType := '232BM';
        FT_DEVICE_232AM: sDevType := '232AM';
        FT_DEVICE_100AX: sDevType := '100AX';
        FT_DEVICE_UNKNOWN: sDevType := 'Unknown';
        FT_DEVICE_2232C: sDevType := '2232C';
        FT_DEVICE_232R: sDevType := '232R';
        FT_DEVICE_2232H: sDevType := '2232H';
        FT_DEVICE_4232H: sDevType := '4232H';
        FT_DEVICE_232H: sDevType := '232H';
        FT_DEVICE_X_SERIES: sDevType := 'X Series';
        else
          sDevType := 'Unknown';
      end;

      if Length(Result) > 0 then
        Result := Result + #13#10;
      Result := Result + Trim(DeviceInfo.Description) + ':' + Trim(DeviceInfo.SerialNumber);
    end;
  end;

end;

(*
class function TDataPortFtdi.GetFtdiDriverVersion(): string;
var
  DrVersion: DWORD;
begin
  Result := '';
  {$ifdef WINDOWS}
  if not Active then Exit;
  if FT_GetDriverVersion(FtdiClient.FFtHandle, @DrVersion) = FT_OK then
  begin
    Result := IntToStr((DrVersion shr 16) and $FF)
         +'.'+IntToStr((DrVersion shr 8) and $FF)
         +'.'+IntToStr(DrVersion and $FF);
 end;
  {$endif}
end;
*)

procedure TDataPortFtdi.FSetBaudRate(AValue: integer);
begin
  FBaudRate := AValue;
  if Active then
  begin
    FtdiClient.FFtBaudRate := FBaudRate;
    FtdiClient.Config();
  end;
end;

procedure TDataPortFtdi.FSetDataBits(AValue: integer);
begin

end;

procedure TDataPortFtdi.FSetFlowControl(AValue: TSerialFlowControl);
begin
  FFlowControl := AValue;
  if Active then
  begin
    case FFlowControl of
      sfcNone: FtdiClient.FFtFlowControl := FT_FLOW_NONE;
      sfcRTS: FtdiClient.FFtFlowControl := FT_FLOW_RTS_CTS;
      sfcDTR: FtdiClient.FFtFlowControl := FT_FLOW_DTR_DSR;
      sfcXON: FtdiClient.FFtFlowControl := FT_FLOW_XON_XOFF;
    end;
    FtdiClient.Config();
  end;
end;

procedure TDataPortFtdi.FSetParity(AValue: AnsiChar);
begin
  if Pos(AValue, 'NOEMSnoems') = 0 then
    Exit;
  FParity := AValue;
  if Active then
  begin
    case FParity of
      'N', 'n': FtdiClient.FFtParity := FT_PARITY_NONE;
      'O', 'o': FtdiClient.FFtParity := FT_PARITY_ODD;
      'E', 'e': FtdiClient.FFtParity := FT_PARITY_EVEN;
      'M', 'm': FtdiClient.FFtParity := FT_PARITY_MARK;
      'S', 's': FtdiClient.FFtParity := FT_PARITY_SPACE;
    end;
    FtdiClient.Config();
  end;

end;

procedure TDataPortFtdi.FSetStopBits(AValue: TSerialStopBits);
begin

end;

end.

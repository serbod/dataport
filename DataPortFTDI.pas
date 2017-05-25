unit DataPortFTDI;

{
Serial communication port based on FTD2XX library.

Sergey Bodrov, 2012-2017

Properties:
  SerialNumber      - FTDI device serial number
  DeviceDescription - FTDI device description string
  BaudRate          - data exchange speed (300, 1200, 9600, 115384, 230769, 923076)
  DataBits          - default 8
  Parity            - (N - None, O - Odd, E - Even, M - Mark or S - Space) default N
  StopBits          - (1, 1.5, 2) default 1
  MinDataBytes      - minimal bytes count in buffer for triggering event OnDataAppear

Methods:
  Open() - Opens port. As parameter it use port initialization string:
    InitStr = '<DeviceDescription>:<SerialNumber>:<PortInitStr>'
      Examples:
        'USB Serial::' - first device of 'USB Serial' type
        ':FT425622:'   - device with s/n FT425622
  GetFtdiDeviceList() - list of available devices in format <DeviceDescription>:<SerialNumber><LineFeed>

Events:
  OnModemStatus - Triggered when modem status changes (CTS, DTR, RI, DCD)
}

interface

uses SysUtils, Classes, DataPort, DataPortUART, D2XXUnit;

type
  TSerialFlowControl = (sfcNone, sfcRTS, sfcDTR, sfcXON);

  { TFtdiClient - FTDI device reader/writer thread }
  TFtdiClient = class(TThread)
  private
    sFromPort: AnsiString;
    sLastError: string;
    FSafeMode: Boolean;
    FOnIncomingMsgEvent: TMsgEvent;
    FOnErrorEvent: TMsgEvent;
    FOnConnectEvent: TNotifyEvent;
    FDoConfig: Boolean;
    { how many bytes read for once, default 64k }
    FReadCount: Integer;

    FFtHandle: LongWord;
    FFtIOStatus: FT_Result;
    { bytes in receive queue }
    FFtRxQBytes: LongWord;
    { bytes in transmit queue }
    FFtTxQBytes: LongWord;
    { wakeup event status }
    FFtEventStatus: LongWord;
    { input buffer }
    FFtInBuffer: array[0..FT_In_Buffer_Index] of byte;
    { output buffer }
    FFtOutBuffer: array[0..FT_Out_Buffer_Index] of byte;
    procedure SyncProc();
    procedure SyncProcOnConnect();
    function SendStringInternal(AData: string): Integer;
    function CheckFtError(APortStatus: FT_Result; AFunctionName: string = ''): Boolean;
    function GetFtErrorDescription(APortStatus: FT_Result): string;
  protected
    procedure Execute(); override;
  public
    //Serial: TBlockSerial;
    InitStr: string;
    CalledFromThread: Boolean;
    sToSend: AnsiString;
    // port properties
    FtBaudRate: LongWord;
    FtDataBits: byte;
    FtStopBits: byte;
    FtParity: byte;
    FtFlowControl: word;
    procedure AfterConstruction; override;
    property SafeMode: Boolean read FSafeMode write FSafeMode;
    property OnIncomingMsgEvent: TMsgEvent read FOnIncomingMsgEvent
      write FOnIncomingMsgEvent;
    property OnError: TMsgEvent read FOnErrorEvent write FOnErrorEvent;
    property OnConnect: TNotifyEvent read FOnConnectEvent write FOnConnectEvent;
    { Set port parameters (baud rate, data bits, etc..) }
    procedure Config();
    function SendString(const AData: AnsiString): Boolean;
    { Get modem wires status (DSR,CTS,Ring,Carrier) }
    function ReadModemStatus(var AModemStatus: TModemStatus): Boolean;
    { Set DTR (Data Terminal Ready) signal }
    procedure SetDTR(AValue: Boolean);
    { Set RTS (Request to send) signal }
    procedure SetRTS(AValue: Boolean);
  end;

  { TDataPortFtdi }

  TDataPortFtdi = class(TDataPortUART)
  private
    FtdiClient: TFtdiClient;
    FFtSerialNumber: string;
    FFtDeviceDescription: string;
    FFlowControl: TSerialFlowControl;
    FOnModemStatus: TNotifyEvent;
  protected
    procedure SetBaudRate(AValue: Integer); override;
    procedure SetDataBits(AValue: Integer); override;
    procedure SetParity(AValue: AnsiChar); override;
    procedure SetStopBits(AValue: TSerialStopBits); override;
    procedure SetFlowControl(AValue: TSerialFlowControl);
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
    procedure Open(const AInitStr: string = ''); override;
    procedure Close(); override;
    function Push(const AData: AnsiString): Boolean; override;

    class function GetFtdiDeviceList(): string;
    //class function GetFtdiDriverVersion(): string;

    { Get modem wires status (DSR,CTS,Ring,Carrier) }
    function GetModemStatus(): TModemStatus; override;
    { Set DTR (Data Terminal Ready) signal }
    procedure SetDTR(AValue: Boolean); override;
    { Set RTS (Request to send) signal }
    procedure SetRTS(AValue: Boolean); override;
  published
    property Active;
    { FTDI device serial number }
    property SerialNumber: string read FFtSerialNumber write FFtSerialNumber;
    { FTDI device description string }
    property DeviceDescription: string read FFtDeviceDescription write FFtDeviceDescription;
    { FTDI flow control }
    property FlowControl: TSerialFlowControl read FFlowControl write SetFlowControl;
    property BaudRate;
    property DataBits;
    property MinDataBytes;
    property OnDataAppear;
    property OnError;
    property OnOpen;
    property OnClose;
    { Triggered when modem status changes (CTS, DTR, RI, DCD) }
    property OnModemStatus: TNotifyEvent read FOnModemStatus write FOnModemStatus;
  end;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('DataPort', [TDataPortFtdi]);
end;

{ TFtdiClient }

procedure TFtdiClient.Config();
begin
  FDoConfig := True;
end;

function TFtdiClient.CheckFtError(APortStatus: FT_Result;
  AFunctionName: string): Boolean;
begin
  Result := True;
  if APortStatus <> FT_OK then
  begin
    sLastError := AFunctionName + ': ' + GetFtErrorDescription(APortStatus);
    Synchronize(SyncProc);
    Result := False;
  end;
end;

function TFtdiClient.SendStringInternal(AData: string): Integer;
var
  WriteCount, WriteResult: Integer;
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

procedure TFtdiClient.Execute();
var
  PortStatus: FT_Result;
  DeviceString: AnsiString;
  s, ss: string;
  FtDeviceStringBuffer: array [1..50] of AnsiChar;
  ReadCount, ReadResult, WriteResult: Integer;
  ReadTimeout, WriteTimeout: LongWord;

begin
  // Default settings
  sLastError := '';
  DeviceString := '';
  FDoConfig := True;
  FFtHandle := FT_INVALID_HANDLE;
  FReadCount := SizeOf(FFtInBuffer);
  ReadTimeout := 10;
  WriteTimeout := 10;

  // parse InitStr, open port
  FtDeviceStringBuffer[1] := #0; // remove warning
  ss := InitStr;
  s := GetFirstWord(ss, ':');
  if Length(s) > 0 then
  begin
    DeviceString := s;
    FillChar(FtDeviceStringBuffer, SizeOf(FtDeviceStringBuffer), 0);
    Move(DeviceString[1], FtDeviceStringBuffer, Length(DeviceString));
    PortStatus := FT_OpenEx(@FtDeviceStringBuffer, FT_OPEN_BY_DESCRIPTION, @FFtHandle);
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
    end
    else
      PortStatus := FT_DEVICE_NOT_FOUND;
  end;

  if CheckFtError(PortStatus, 'FT_OpenEx') then
  begin
    Terminate();
    Exit;
  end;

  // Device handle acquired, we must release it in any case
  try
    while not Terminated do
    begin
      // configure port
      if FDoConfig then
      begin
        FDoConfig := False;
        // This function sends a reset command to the device.
        PortStatus := FT_ResetDevice(FFtHandle);
        if CheckFtError(PortStatus, 'FT_ResetDevice') then
        begin
          // set BaudRate
          PortStatus := FT_SetBaudRate(FFtHandle, FtBaudRate);
          if CheckFtError(PortStatus, 'FT_SetBaudRate') then
          begin
            // set data characteristics
            PortStatus := FT_SetDataCharacteristics(FFtHandle, FtDataBits, FtStopBits, FtParity);
            if CheckFtError(PortStatus, 'FT_SetDataCharacteristics') then
            begin
              // set flow control
              PortStatus := FT_SetFlowControl(FFtHandle, FtFlowControl, FT_XON_Value, FT_XOFF_Value);
              if CheckFtError(PortStatus, 'FT_SetFlowControl') then
              begin
                // This function sets the read and write timeouts (in milliseconds) for the device
                PortStatus := FT_SetTimeouts(FFtHandle, ReadTimeout, WriteTimeout);
                if CheckFtError(PortStatus, 'FT_SetTimeouts') then
                begin
                  // This function purges receive and transmit buffers in the device.
                  PortStatus := FT_Purge(FFtHandle, (FT_PURGE_RX + FT_PURGE_TX));
                  CheckFtError(PortStatus, 'FT_Purge');
                end;
              end;
            end;
          end;
        end;

        if PortStatus = FT_OK then
          Synchronize(SyncProcOnConnect)
        else
        begin
          Terminate();
          Continue;
        end;
      end;

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

procedure TFtdiClient.AfterConstruction();
begin
  inherited AfterConstruction;
  FtBaudRate    := FT_BAUD_9600;
  FtDataBits    := FT_DATA_BITS_8;
  FtStopBits    := FT_STOP_BITS_1;
  FtParity      := FT_PARITY_NONE;
  FtFlowControl := FT_FLOW_NONE;
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

function TFtdiClient.SendString(const AData: AnsiString): Boolean;
var
  WriteResult: Integer;
begin
  if SafeMode then
  begin
    self.sToSend := self.sToSend + AData;
    Result := True;
  end
  else
  begin
    WriteResult := SendStringInternal(AData);
    Result := (WriteResult = Length(AData));
  end;
end;

function TFtdiClient.ReadModemStatus(var AModemStatus: TModemStatus): Boolean;
var
  ModemStat: LongWord;
begin
  if FFtHandle <> FT_INVALID_HANDLE then
  begin
    Result := (FT_GetModemStatus(FFtHandle, @ModemStat) = FT_OK);
    if Result then
    begin
      AModemStatus.CTS := (ModemStat and FT_CTS) <> 0;
      AModemStatus.DSR := (ModemStat and FT_DSR) <> 0;
      AModemStatus.Ring := (ModemStat and FT_RI) <> 0;
      AModemStatus.Carrier := (ModemStat and FT_DCD) <> 0;
    end;
  end
  else
    Result := False;
end;

procedure TFtdiClient.SetDTR(AValue: Boolean);
begin
  if FFtHandle <> FT_INVALID_HANDLE then
  begin
    if AValue then
      FT_SetDtr(FFtHandle)
    else
      FT_ClrDtr(FFtHandle);
  end;
end;

procedure TFtdiClient.SetRTS(AValue: Boolean);
begin
  if FFtHandle <> FT_INVALID_HANDLE then
  begin
    if AValue then
      FT_SetRts(FFtHandle)
    else
      FT_ClrRts(FFtHandle);
  end;
end;

procedure TFtdiClient.SyncProc();
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

procedure TFtdiClient.SyncProcOnConnect();
begin
  if CalledFromThread then
    Exit;
  CalledFromThread := True;
  if Assigned(self.FOnConnectEvent) then
    self.FOnConnectEvent(self);
  CalledFromThread := False;
end;


{ TDataPortFtdi }

constructor TDataPortFtdi.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFlowControl := sfcNone;
  Self.FtdiClient := nil;
end;

procedure TDataPortFtdi.Open(const AInitStr: string);
var
  ss: string;
begin
  ss := AInitStr;
  // Set device description and serial number
  if ss <> '' then
  begin
    Self.FFtDeviceDescription := GetFirstWord(ss, ':');

    Self.FFtSerialNumber := GetFirstWord(ss, ':');
  end;

  inherited Open(ss);

  if Assigned(self.FtdiClient) then
    FreeAndNil(self.FtdiClient);
  Self.FtdiClient := TFtdiClient.Create(True);
  Self.FtdiClient.InitStr := FFtDeviceDescription + ':' + FFtSerialNumber;
  Self.FtdiClient.SafeMode := True;
  Self.FtdiClient.OnIncomingMsgEvent := Self.OnIncomingMsgHandler;
  Self.FtdiClient.OnError := Self.OnErrorHandler;
  Self.FtdiClient.OnConnect := Self.OnConnectHandler;
  Self.FtdiClient.Suspended := False;
  Self.FActive := True;
end;

procedure TDataPortFtdi.Close();
begin
  if Assigned(FtdiClient) then
    FreeAndNil(FtdiClient);
  inherited Close();
end;

destructor TDataPortFtdi.Destroy();
begin
  if Assigned(FtdiClient) then
    FreeAndNil(FtdiClient);
  inherited Destroy();
end;

function TDataPortFtdi.Push(const AData: AnsiString): Boolean;
begin
  Result := False;
  if Assigned(FtdiClient) and FLock.BeginWrite() then
  begin
    try
      Result := FtdiClient.SendString(AData);
    finally
      FLock.EndWrite();
    end;
  end;
end;

class function TDataPortFtdi.GetFtdiDeviceList(): string;
var
  FtDeviceCount, DeviceIndex: LongWord;
  PortStatus: FT_Result;
  DeviceInfo: FT_Device_Info_Node;
  //FtDeviceInfoList: array of FT_Device_Info_Node;
  i: Integer;
  //sDevType: string;
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
      {
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
      }

      if Length(Result) > 0 then
        Result := Result + #13#10;
      Result := Result + Trim(DeviceInfo.Description) + ':' + Trim(DeviceInfo.SerialNumber);
    end;
  end;

end;

function TDataPortFtdi.GetModemStatus(): TModemStatus;
begin
  if Assigned(FtdiClient) then
  begin
    FtdiClient.ReadModemStatus(FModemStatus);
  end;
  Result := inherited GetModemStatus;
end;

procedure TDataPortFtdi.SetDTR(AValue: Boolean);
begin
  if Assigned(FtdiClient) then
    FtdiClient.SetDTR(AValue);
  inherited SetDTR(AValue);
end;

procedure TDataPortFtdi.SetRTS(AValue: Boolean);
begin
  if Assigned(FtdiClient) then
    FtdiClient.SetRTS(AValue);
  inherited SetRTS(AValue);
end;

(*
class function TDataPortFtdi.GetFtdiDriverVersion(): string;
var
  DrVersion: LongWord;
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

procedure TDataPortFtdi.SetBaudRate(AValue: Integer);
begin
  inherited SetBaudRate(AValue);
  if Active then
  begin
    FtdiClient.FtBaudRate := FBaudRate;
    FtdiClient.Config();
  end;
end;

procedure TDataPortFtdi.SetDataBits(AValue: Integer);
begin
  inherited SetDataBits(AValue);
  if Active then
  begin
    FtdiClient.FtDataBits := Byte(FDataBits);
    FtdiClient.Config();
  end;
end;

procedure TDataPortFtdi.SetFlowControl(AValue: TSerialFlowControl);
begin
  FFlowControl := AValue;
  if Active then
  begin
    case FFlowControl of
      sfcNone: FtdiClient.FtFlowControl := FT_FLOW_NONE;
      sfcRTS:  FtdiClient.FtFlowControl := FT_FLOW_RTS_CTS;
      sfcDTR:  FtdiClient.FtFlowControl := FT_FLOW_DTR_DSR;
      sfcXON:  FtdiClient.FtFlowControl := FT_FLOW_XON_XOFF;
    end;
    FtdiClient.Config();
  end;
end;

procedure TDataPortFtdi.SetParity(AValue: AnsiChar);
begin
  inherited SetParity(AValue);
  if Active then
  begin
    case FParity of
      'N', 'n': FtdiClient.FtParity := FT_PARITY_NONE;
      'O', 'o': FtdiClient.FtParity := FT_PARITY_ODD;
      'E', 'e': FtdiClient.FtParity := FT_PARITY_EVEN;
      'M', 'm': FtdiClient.FtParity := FT_PARITY_MARK;
      'S', 's': FtdiClient.FtParity := FT_PARITY_SPACE;
    end;
    FtdiClient.Config();
  end;
end;

procedure TDataPortFtdi.SetStopBits(AValue: TSerialStopBits);
begin
  inherited SetStopBits(AValue);
  if Active then
  begin
    case FStopBits of
      stb1:  FtdiClient.FtStopBits := FT_STOP_BITS_1;
      stb15: FtdiClient.FtStopBits := FT_STOP_BITS_15;
      stb2:  FtdiClient.FtStopBits := FT_STOP_BITS_2;
    end;
    FtdiClient.Config();
  end;
end;

end.

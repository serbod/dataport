{
Serial communication port based on FTD2XX library.

(C) Sergey Bodrov, 2012-2025

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
unit DataPortFTDI;

interface

uses {$ifndef FPC}Windows,{$endif} SysUtils, Classes, DataPort, DataPortUART, D2XXUnit;

type
  { TFtdiClient - FTDI device reader/writer thread }
  TFtdiClient = class(TThread)
  private
    FLock: TSimpleRWSync;
    FRxData: AnsiString;
    FTxData: AnsiString;

    FLastErrorStr: string;
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
    //FFtOutBuffer: array[0..FT_Out_Buffer_Index] of byte;

    function SendStringInternal(const AData: AnsiString): Integer;
    function CheckFtError(APortStatus: FT_Result; AFunctionName: string = ''): Boolean;
    function GetFtErrorDescription(APortStatus: FT_Result): string;
  protected
    FParentDataPort: TDataPortUART;
    procedure Execute(); override;
  public
    InitStr: string;
    // port properties
    BaudRate: Integer;
    DataBits: Integer;
    Parity: AnsiChar;
    StopBits: TSerialStopBits;
    FlowControl: TSerialFlowControl;
    MinDataBytes: Integer;

    constructor Create(AParent: TDataPortUART); reintroduce;
    destructor Destroy; override;
    // thread-unsafe events
    property OnIncomingMsgEvent: TMsgEvent read FOnIncomingMsgEvent
      write FOnIncomingMsgEvent;
    property OnError: TMsgEvent read FOnErrorEvent write FOnErrorEvent;
    property OnConnect: TNotifyEvent read FOnConnectEvent write FOnConnectEvent;
    { Set port parameters (baud rate, data bits, etc..) }
    procedure Config();
    function SendAnsiString(const AData: AnsiString): Boolean;
    { Get modem wires status (DSR,CTS,Ring,Carrier) }
    function ReadModemStatus(var AModemStatus: TModemStatus): Boolean;
    { Set DTR (Data Terminal Ready) signal }
    procedure SetDTR(AValue: Boolean);
    { Set RTS (Request to send) signal }
    procedure SetRTS(AValue: Boolean);
    { Get COM port name }
    function GetPortName(): string;

    property TxData: AnsiString read FTxData;
  end;

  { TDataPortFtdi }

  TDataPortFtdi = class(TDataPortUART)
  private
    FFtdiClient: TFtdiClient;
    FFtSerialNumber: string;
    FFtDeviceDescription: string;
    FFlowControl: TSerialFlowControl;
    FOnModemStatus: TNotifyEvent;

    function CloseClient(): Boolean;
  protected
    procedure SetBaudRate(AValue: Integer); override;
    procedure SetDataBits(AValue: Integer); override;
    procedure SetParity(AValue: AnsiChar); override;
    procedure SetStopBits(AValue: TSerialStopBits); override;
    procedure SetFlowControl(AValue: TSerialFlowControl); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    { Open FTDI device for data transmission
      InitStr = '<DeviceDescription>:<SerialNumber>:<PortInitStr>'
      Examples:
        'USB Serial:' - first device of 'USB Serial' type
        ':FT425622'   - device with s/n FT425622
      List of available devices can be acquired in GetFtdiDeviceList()
    }
    procedure Open(const AInitStr: string = ''); override;
    procedure Close(); override;
    function Push(const AData: AnsiString): Boolean; override;

    { list of available devices in format <DeviceDescription>:<SerialNumber><LineFeed> }
    class function GetFtdiDeviceList(): AnsiString;
    //class function GetFtdiDriverVersion(): string;

    { Get modem wires status (DSR,CTS,Ring,Carrier) }
    function GetModemStatus(): TModemStatus; override;
    { Set DTR (Data Terminal Ready) signal }
    procedure SetDTR(AValue: Boolean); override;
    { Set RTS (Request to send) signal }
    procedure SetRTS(AValue: Boolean); override;

    property FtdiClient: TFtdiClient read FFtdiClient;
  published
    property Active;
    { FTDI device serial number }
    property SerialNumber: string read FFtSerialNumber write FFtSerialNumber;
    { FTDI device description string }
    property DeviceDescription: string read FFtDeviceDescription write FFtDeviceDescription;
    { FlowControl - (sfcNone, sfcSend, sfcReady, sfcSoft) default sfcNone }
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

const
  //TX_BUF_SIZE = 128;  // safe size
  TX_BUF_SIZE = 512;  // optimal size

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
    Terminate();
    FLastErrorStr := AFunctionName + ': ' + GetFtErrorDescription(APortStatus);
    Result := False;
  end;
end;

function TFtdiClient.SendStringInternal(const AData: AnsiString): Integer;
var
  WriteResult: Integer;
  WritePos, WriteSize, TotalSize: Integer;
begin
  Result := 0;
  TotalSize := Length(AData);
  if (TotalSize = 0) or Terminated then
    Exit;
  WritePos := 0;
  while (FFtIOStatus = FT_OK) and (Result < TotalSize) do
  begin
    // some FTDI chips or drivers can't receive many bytes at once
    WriteSize := TX_BUF_SIZE;
    //WriteSize := $FF;
    if (WritePos + WriteSize) > TotalSize then
      WriteSize := TotalSize - WritePos;

    FFtIOStatus := FT_GetStatus(FFtHandle, @FFtRxQBytes, @FFtTxQBytes, @FFtEventStatus);
    if FFtIOStatus = FT_OK then
    begin
      if FFtTxQBytes > TX_BUF_SIZE then
        Break;

      // Writes Write_Count Bytes from FT_Out_Buffer to the USB device
      // Function returns the number of bytes actually sent
      // In this example, Write_Count should be 64k bytes max
      if WriteSize > 0 then
      begin
        //Move(AData[WritePos+1], FFtOutBuffer, WriteSize);
        //FFtIOStatus := FT_Write(FFtHandle, @FFtOutBuffer, WriteSize, @WriteResult);
        if WritePos + WriteSize > Length(AData) then
          Break;
        WriteResult := 0;
        FFtIOStatus := FT_Write(FFtHandle, @AData[WritePos+1], WriteSize, @WriteResult);
        if FFtIOStatus = FT_OK then
        begin
          if WriteResult = 0 then
            WriteResult := WriteSize;
          Result := Result + WriteResult;
          if WriteResult <> WriteSize then
            Break;
        end
        else
          Break;
        WritePos := WritePos + WriteSize;
      end;
    end;

  end;

  CheckFtError(FFtIOStatus, 'FT_Write');
end;

procedure TFtdiClient.Execute();
var
  FtBaudRate: LongWord;
  FtDataBits: byte;
  FtStopBits: byte;
  FtParity: byte;
  FtFlowControl: word;
  FtModemStatus: LongWord;
  FtModemStatusPrev: LongWord;

  PortStatus: FT_Result;
  SerialString, DeviceString: AnsiString;
  s, ss: string;
  FtDeviceStringBuffer: array [1..50] of AnsiChar;
  ReadCount, ReadResult, WriteSize, WriteResult: Integer;
  ReadTimeout, WriteTimeout: LongWord;
  NeedSleep: Boolean;

begin
  // Default settings
  FLastErrorStr := '';
  DeviceString := '';
  FDoConfig := True;
  FFtHandle := FT_INVALID_HANDLE;
  FReadCount := SizeOf(FFtInBuffer);
  ReadTimeout := 100;
  WriteTimeout := 100;
  FtBaudRate := FT_BAUD_9600;
  FtDataBits := FT_DATA_BITS_8;
  FtParity := FT_PARITY_NONE;
  FtStopBits := FT_STOP_BITS_1;
  FtFlowControl := FT_FLOW_NONE;
  FtModemStatus := 0;
  FtModemStatusPrev := 0;

  if Terminated then Exit;

  // parse InitStr, open port
  FtDeviceStringBuffer[1] := #0; // remove warning
  ss := InitStr;
  DeviceString := ExtractFirstWord(ss, ':');
  SerialString := ExtractFirstWord(ss, ':'); // Serial num
  if Length(SerialString) > 0 then
  begin
    FillChar(FtDeviceStringBuffer, SizeOf(FtDeviceStringBuffer), 0);
    Move(SerialString[1], FtDeviceStringBuffer, Length(SerialString));
    PortStatus := FT_OpenEx(@FtDeviceStringBuffer, FT_OPEN_BY_SERIAL_NUMBER, @FFtHandle);
  end
  else if Length(DeviceString) > 0 then
  begin
    FillChar(FtDeviceStringBuffer, SizeOf(FtDeviceStringBuffer), 0);
    Move(DeviceString[1], FtDeviceStringBuffer, Length(DeviceString));
    PortStatus := FT_OpenEx(@FtDeviceStringBuffer, FT_OPEN_BY_DESCRIPTION, @FFtHandle);
  end
  else
    PortStatus := FT_DEVICE_NOT_FOUND;

  if not CheckFtError(PortStatus, 'FT_OpenEx') then
    FFtHandle := FT_INVALID_HANDLE;

  // Device handle acquired, we must release it in any case
  try
    while not Terminated do
    begin
      NeedSleep := True;
      // configure port
      if FDoConfig then
      begin
        FDoConfig := False;
        if BaudRate <> 0 then
          FtBaudRate := BaudRate;
        if DataBits <> 0 then
          FtDataBits := Byte(DataBits);
        case Parity of
          'N', 'n': FtParity := FT_PARITY_NONE;
          'O', 'o': FtParity := FT_PARITY_ODD;
          'E', 'e': FtParity := FT_PARITY_EVEN;
          'M', 'm': FtParity := FT_PARITY_MARK;
          'S', 's': FtParity := FT_PARITY_SPACE;
        end;
        case StopBits of
          stb1:  FtStopBits := FT_STOP_BITS_1;
          stb15: FtStopBits := FT_STOP_BITS_15;
          stb2:  FtStopBits := FT_STOP_BITS_2;
        end;
        case FlowControl of
          sfcNone:   FtFlowControl := FT_FLOW_NONE;
          sfcSend:   FtFlowControl := FT_FLOW_RTS_CTS;
          sfcReady:  FtFlowControl := FT_FLOW_DTR_DSR;
          sfcSoft:   FtFlowControl := FT_FLOW_XON_XOFF;
        end;

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
                  PortStatus := FT_ResetDevice(FFtHandle);
                  CheckFtError(PortStatus, 'FT_ResetDevice_2');
                end;
              end;
            end;
          end;
        end;

        if PortStatus = FT_OK then
        begin
          if Assigned(OnConnect) then
            OnConnect(Self);
        end
        else
        begin
          Terminate();
          Continue;
        end;
      end;

      // update modem status
      if Assigned(FParentDataPort.OnModemStatus) then
      begin
        FFtIOStatus := FT_GetModemStatus(FFtHandle, @FtModemStatus);
        if CheckFtError(FFtIOStatus, 'FT_GetModemStatus') then
        begin
          if FtModemStatusPrev <> FtModemStatus then
          begin
            FtModemStatusPrev := FtModemStatus;
            if Assigned(OnIncomingMsgEvent) then
              OnIncomingMsgEvent(Self, '');
          end;
        end;
      end;

      // Reads Read_Count Bytes (or less) from the USB device to the FT_In_Buffer
      // Function returns the number of bytes actually received  which may range from zero
      // to the actual number of bytes requested, depending on how many have been received
      // at the time of the request + the read timeout value.
      FRxData := '';
      ReadCount := FReadCount;
      ReadResult := 0;

      FFtIOStatus := FT_GetStatus(FFtHandle, @FFtRxQBytes, @FFtTxQBytes,
        @FFtEventStatus);
      if CheckFtError(FFtIOStatus, 'FT_GetStatus') and (FFtRxQBytes > 0) then
      begin
        if ReadCount > FFtRxQBytes then
          ReadCount := FFtRxQBytes;
        // This function does not return until dwBytesToRead bytes have been read into the buffer
        FFtIOStatus := FT_Read(FFtHandle, @FFtInBuffer, ReadCount, @ReadResult);
        if CheckFtError(FFtIOStatus, 'FT_Read') and (ReadResult > 0) then
        begin
          // copy input buffer to string
          SetLength(FRxData, ReadResult);
          Move(FFtInBuffer, FRxData[1], ReadResult);
          if Assigned(OnIncomingMsgEvent) then
            OnIncomingMsgEvent(Self, FRxData);
          FRxData := '';
          NeedSleep := False;
        end;
      end;

      FLock.BeginWrite;
      try
        if (Length(FTxData) > 0) then
        begin
          DeviceString := Copy(FTxData, 1, TX_BUF_SIZE);
        end;
      finally
        FLock.EndWrite;
      end;

      if DeviceString <> '' then
      begin
        WriteSize := Length(DeviceString);
        WriteResult := 0;
        FFtIOStatus := FT_Write(FFtHandle, @DeviceString[1], WriteSize, @WriteResult);
        if (FFtIOStatus = FT_OK) then
        begin
          if WriteResult = 0 then
            WriteResult := WriteSize;

          FLock.BeginWrite;
          try
            Delete(FTxData, 1, WriteResult);
          finally
            FLock.EndWrite;
          end;
          NeedSleep := False;
        end;
        DeviceString := '';
      end;

      if NeedSleep then
        Sleep(1);
    end;

  finally
    if FFtHandle <> FT_INVALID_HANDLE then
      FT_Close(FFtHandle);
    FFtHandle := FT_INVALID_HANDLE;
    FFtIOStatus := FT_INVALID_HANDLE;
  end;

  if Assigned(OnError) then
    OnError(Self, FLastErrorStr);

  OnIncomingMsgEvent := nil;
  OnError := nil;
  OnConnect := nil;
end;

constructor TFtdiClient.Create(AParent: TDataPortUART);
begin
  FLock := TSimpleRWSync.Create();
  inherited Create(True);
  FParentDataPort := AParent;
  BaudRate := 9600;
end;

destructor TFtdiClient.Destroy;
begin
  inherited Destroy;  // terminate thread, if running
  FreeAndNil(FLock);
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

function TFtdiClient.SendAnsiString(const AData: AnsiString): Boolean;
begin
  Result := FLock.BeginWrite;
  if Result then
  try
    FTxData := FTxData + AData;
  finally
    FLock.EndWrite;
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

function TFtdiClient.GetPortName(): string;
var
  ComPortNum: Longint;
begin
  Result := '';
  if (FFtHandle <> FT_INVALID_HANDLE) then
  begin
    FT_GetComPortNumber(FFtHandle, @ComPortNum);
    if ComPortNum <> -1 then
      Result := 'COM' + IntToStr(ComPortNum);
  end;
end;

{ TDataPortFtdi }

constructor TDataPortFtdi.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFlowControl := sfcNone;
  FFtdiClient := nil;
end;

destructor TDataPortFtdi.Destroy();
begin
  CloseClient();
  inherited Destroy();
end;

function TDataPortFtdi.CloseClient(): Boolean;
begin
  Result := True;
  FreeAndNil(FFtdiClient);
end;

procedure TDataPortFtdi.Open(const AInitStr: string);
var
  ss: string;
begin
  ss := AInitStr;
  // Set device description and serial number
  if ss <> '' then
  begin
    Self.FFtDeviceDescription := ExtractFirstWord(ss, ':');

    Self.FFtSerialNumber := ExtractFirstWord(ss, ':');
  end;

  inherited Open(ss);

  if CloseClient() then
  begin
    FFtdiClient := TFtdiClient.Create(Self);
    FFtdiClient.InitStr := FFtDeviceDescription + ':' + FFtSerialNumber;
    FFtdiClient.BaudRate := FBaudRate;
    FFtdiClient.DataBits := FDataBits;
    FFtdiClient.Parity := FParity;
    FFtdiClient.StopBits := FStopBits;
    FFtdiClient.FlowControl := FFlowControl;

    FFtdiClient.OnIncomingMsgEvent := OnIncomingMsgHandler;
    FFtdiClient.OnError := OnErrorHandler;
    FFtdiClient.OnConnect := OnConnectHandler;
    FFtdiClient.Suspended := False;
    // don't set FActive - will be set in OnConnect event after successfull connection
  end;
end;

procedure TDataPortFtdi.Close();
begin
  CloseClient();
  inherited Close();
end;

function TDataPortFtdi.Push(const AData: AnsiString): Boolean;
begin
  Result := False;
  if Active and Assigned(FtdiClient) then
  begin
    Result := FtdiClient.SendAnsiString(AData);
  end;
end;

class function TDataPortFtdi.GetFtdiDeviceList(): AnsiString;
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

  for i := 1 to FtDeviceCount do
  begin
    DeviceIndex := i-1;
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
        Result := Result + sLineBreak;
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
    FtdiClient.BaudRate := FBaudRate;
    FtdiClient.Config();
  end;
end;

procedure TDataPortFtdi.SetDataBits(AValue: Integer);
begin
  inherited SetDataBits(AValue);
  if Active then
  begin
    FtdiClient.DataBits := FDataBits;
    FtdiClient.Config();
  end;
end;

procedure TDataPortFtdi.SetFlowControl(AValue: TSerialFlowControl);
begin
  FFlowControl := AValue;
  if Active then
  begin
    FtdiClient.FlowControl := FFlowControl;
    FtdiClient.Config();
  end;
end;

procedure TDataPortFtdi.SetParity(AValue: AnsiChar);
begin
  inherited SetParity(AValue);
  if Active then
  begin
    FtdiClient.Parity := FParity;
    FtdiClient.Config();
  end;
end;

procedure TDataPortFtdi.SetStopBits(AValue: TSerialStopBits);
begin
  inherited SetStopBits(AValue);
  if Active then
  begin
    FtdiClient.StopBits := FStopBits;
    FtdiClient.Config();
  end;
end;

end.

unit D2XXUnit;
{
FTDI D2XX library interface
}
interface
{$IFDEF FPC}
  {$mode DELPHI}
{$endif}

uses SysUtils, Classes,

{$IFDEF FPC}
dynlibs;
{$ELSE}
Windows;
{$ENDIF}

// Uncomment next line for statically link external library
//{$DEFINE S}


type
  FT_Result = longint;



// Device Info Node structure for info list functions
type
  FT_Device_Info_Node = record
    Flags: DWord;
    DeviceType: Dword;
    ID: DWord;
    LocID: DWord;
    SerialNumber: array [0..15] of AnsiChar;
    Description: array [0..63] of AnsiChar;
    DeviceHandle: DWord;
  end;


type
  TDWordptr = ^DWord;

  // Structure to hold EEPROM data for FT_EE_Program function
  TFT_Program_Data = record
    Signature1: DWord;
    Signature2: DWord;
    Version: DWord;
    VendorID: word;
    ProductID: word;
    Manufacturer: PChar;
    ManufacturerID: PChar;
    Description: PChar;
    SerialNumber: PChar;
    MaxPower: word;
    PnP: word;
    SelfPowered: word;
    RemoteWakeup: word;
    // Rev4 extensions
    Rev4: byte;
    IsoIn: byte;
    IsoOut: byte;
    PullDownEnable: byte;
    SerNumEnable: byte;
    USBVersionEnable: byte;
    USBVersion: word;
    // FT2232C extensions
    Rev5: byte;
    IsoInA: byte;
    IsoInB: byte;
    IsoOutA: byte;
    IsoOutB: byte;
    PullDownEnable5: byte;
    SerNumEnable5: byte;
    USBVersionEnable5: byte;
    USBVersion5: word;
    AIsHighCurrent: byte;
    BIsHighCurrent: byte;
    IFAIsFifo: byte;
    IFAIsFifoTar: byte;
    IFAIsFastSer: byte;
    AIsVCP: byte;
    IFBIsFifo: byte;
    IFBIsFifoTar: byte;
    IFBIsFastSer: byte;
    BIsVCP: byte;
    // FT232R extensions
    UseExtOsc: byte;
    HighDriveIOs: byte;
    EndpointSize: byte;
    PullDownEnableR: byte;
    SerNumEnableR: byte;
    InvertTXD: byte;
    InvertRXD: byte;
    InvertRTS: byte;
    InvertCTS: byte;
    InvertDTR: byte;
    InvertDSR: byte;
    InvertDCD: byte;
    InvertRI: byte;
    Cbus0: byte;
    Cbus1: byte;
    Cbus2: byte;
    Cbus3: byte;
    Cbus4: byte;
    RIsVCP: byte;
  end;


// Exported Functions
// Classic Functions
function GetFTDeviceCount: FT_Result;
function GetFTDeviceSerialNo(DeviceIndex: DWord): FT_Result;
function GetFTDeviceDescription(DeviceIndex: DWord): FT_Result;
function GetFTDeviceLocation(DeviceIndex: DWord): FT_Result;
function Open_USB_Device: FT_Result;
function Open_USB_Device_By_Serial_Number(Serial_Number: string): FT_Result;
function Open_USB_Device_By_Device_Description(Device_Description: string): FT_Result;
function Open_USB_Device_By_Device_Location(Location: DWord): FT_Result;
function Close_USB_Device: FT_Result;
function Read_USB_Device_Buffer(Read_Count: integer): integer;
function Write_USB_Device_Buffer(Write_Count: integer): integer;
function Reset_USB_Device: FT_Result;
function Set_USB_Device_BaudRate: FT_Result;
function Set_USB_Device_BaudRate_Divisor(Divisor: Dword): FT_Result;
function Set_USB_Device_DataCharacteristics: FT_Result;
function Set_USB_Device_FlowControl: FT_Result;
function Set_USB_Device_RTS: FT_Result;
function Clr_USB_Device_RTS: FT_Result;
function Set_USB_Device_DTR: FT_Result;
function Clr_USB_Device_DTR: FT_Result;
function Get_USB_Device_ModemStatus: FT_Result;
function Set_USB_Device_Chars: FT_Result;
function Purge_USB_Device_Out: FT_Result;
function Purge_USB_Device_In: FT_Result;
function Set_USB_Device_TimeOuts(ReadTimeOut, WriteTimeOut: DWord): FT_Result;
function Get_USB_Device_QueueStatus: FT_Result;
function Set_USB_Device_Break_On: FT_Result;
function Set_USB_Device_Break_Off: FT_Result;
function Get_USB_Device_Status: FT_Result;
function Set_USB_Device_Event_Notification(EventMask: DWord): FT_Result;
function USB_FT_GetDeviceInfo(DevType, ID: DWord; SerialNumber, Description: array of char): FT_Result;
function Set_USB_Device_Reset_Pipe_Retry_Count(RetryCount: DWord): FT_Result;
function Stop_USB_Device_InTask: FT_Result;
function Restart_USB_Device_InTask: FT_Result;
function Reset_USB_Port: FT_Result;
function Cycle_USB_Port: FT_Result;
function Create_USB_Device_List: FT_Result;
function Get_USB_Device_List: FT_Result;
function Get_USB_Device_List_Detail(Index: DWord): FT_Result;
// EEPROM Functions
function USB_FT_EE_Read: FT_Result;
function USB_FT_C_EE_Read: FT_Result;
function USB_FT_R_EE_Read: FT_Result;
function USB_FT_EE_Program: FT_Result;
function USB_FT_ReadEE(WordAddr: Dword): FT_Result;
function USB_FT_WriteEE(WordAddr: Dword; WordData: word): FT_Result;
function USB_FT_EraseEE: FT_Result;
function USB_FT_EE_UARead: FT_Result;
function USB_FT_EE_UAWrite: FT_Result;
function USB_FT_EE_UASize: FT_Result;
// FT2232C, FT232BM and FT245BM Extended API Functions
function Get_USB_Device_LatencyTimer: FT_Result;
function Set_USB_Device_LatencyTimer(Latency: byte): FT_Result;
function Get_USB_Device_BitMode(var BitMode: byte): FT_Result;
function Set_USB_Device_BitMode(Mask, Enable: byte): FT_Result;
function Set_USB_Parameters(InSize, OutSize: Dword): FT_Result;

function Get_USB_Driver_Version(DrVersion: TDWordptr): FT_Result;
function Get_USB_Library_Version(LbVersion: TDWordptr): FT_Result;


var
  // Port Handle Returned by the Open Function
  // Used by the Subsequent Function Calls
  FT_HANDLE: DWord = 0;
  // Used to handle multiple device instances in future
  // versions. Must be set to 0 for now.
  //    PV_Device : DWord = 0;

  // Holding Variables for the current settings
  // Can be configured visually using the CFGUnit Unit
  // or manually before calling SetUp_USB_Device
  FT_Current_Baud: Dword;
  FT_Current_DataBits: byte;
  FT_Current_StopBits: byte;
  FT_Current_Parity: byte;
  FT_Current_FlowControl: word;
  FT_RTS_On: boolean;
  FT_DTR_On: boolean;
  FT_Event_On: boolean;
  FT_Error_On: boolean;
  FT_XON_Value: byte = $11;
  FT_XOFF_Value: byte = $13;
  FT_EVENT_Value: byte = $0;
  FT_ERROR_Value: byte = $0;
  // Used by CFGUnit to flag a bad value
  FT_SetupError: boolean;
  // Used to Return the current Modem Status
  FT_Modem_Status: DWord;
  //  Used to return the number of bytes pending
  //  in the Rx Buffer Queue
  FT_Q_Bytes: DWord;
  FT_TxQ_Bytes: DWord;
  FT_Event_Status: DWord;
  //  Used to Enable / Disable the Error Report Dialog
  FT_Enable_Error_Report: boolean = True;
  //  Deposit for Get latency timer
  FT_LatencyRd: byte;

  FT_DeviceInfoList: array of FT_Device_Info_Node;

  Manufacturer: array [0..63] of char;
  ManufacturerID: array [0..15] of char;
  Description: array [0..63] of char;
  SerialNumber: array [0..15] of char;
  LocID: DWord;
  EEDataBuffer: TFT_Program_Data;
  UserData: array [0..63] of byte;
  FT_UA_Size: integer;
  WordRead: word;


const
  // FT_Result Values
  FT_OK = 0;
  FT_INVALID_HANDLE = 1;
  FT_DEVICE_NOT_FOUND = 2;
  FT_DEVICE_NOT_OPENED = 3;
  FT_IO_ERROR = 4;
  FT_INSUFFICIENT_RESOURCES = 5;
  FT_INVALID_PARAMETER = 6;
  FT_SUCCESS = FT_OK;
  FT_INVALID_BAUD_RATE = 7;
  FT_DEVICE_NOT_OPENED_FOR_ERASE = 8;
  FT_DEVICE_NOT_OPENED_FOR_WRITE = 9;
  FT_FAILED_TO_WRITE_DEVICE = 10;
  FT_EEPROM_READ_FAILED = 11;
  FT_EEPROM_WRITE_FAILED = 12;
  FT_EEPROM_ERASE_FAILED = 13;
  FT_EEPROM_NOT_PRESENT = 14;
  FT_EEPROM_NOT_PROGRAMMED = 15;
  FT_INVALID_ARGS = 16;
  FT_OTHER_ERROR = 17;
  // FT_Open_Ex Flags
  FT_OPEN_BY_SERIAL_NUMBER = 1;
  FT_OPEN_BY_DESCRIPTION = 2;
  FT_OPEN_BY_LOCATION = 4;
  // FT_List_Devices Flags
  FT_LIST_NUMBER_ONLY = $80000000;
  FT_LIST_BY_INDEX = $40000000;
  FT_LIST_ALL = $20000000;
  // Baud Rate Selection
  FT_BAUD_300 = 300;
  FT_BAUD_600 = 600;
  FT_BAUD_1200 = 1200;
  FT_BAUD_2400 = 2400;
  FT_BAUD_4800 = 4800;
  FT_BAUD_9600 = 9600;
  FT_BAUD_14400 = 14400;
  FT_BAUD_19200 = 19200;
  FT_BAUD_38400 = 38400;
  FT_BAUD_57600 = 57600;
  FT_BAUD_115200 = 115200;
  FT_BAUD_230400 = 230400;
  FT_BAUD_460800 = 460800;
  FT_BAUD_921600 = 921600;
  // Data Bits Selection
  FT_DATA_BITS_7 = 7;
  FT_DATA_BITS_8 = 8;
  // Stop Bits Selection
  FT_STOP_BITS_1 = 0;
  FT_STOP_BITS_15 = 1;
  FT_STOP_BITS_2 = 2;
  // Parity Selection
  FT_PARITY_NONE = 0;
  FT_PARITY_ODD = 1;
  FT_PARITY_EVEN = 2;
  FT_PARITY_MARK = 3;
  FT_PARITY_SPACE = 4;
  // Flow Control Selection
  FT_FLOW_NONE = $0000;
  FT_FLOW_RTS_CTS = $0100;
  FT_FLOW_DTR_DSR = $0200;
  FT_FLOW_XON_XOFF = $0400;
  // Purge Commands
  FT_PURGE_RX = 1;
  FT_PURGE_TX = 2;
  // Notification Events
  FT_EVENT_RXCHAR = 1;
  FT_EVENT_MODEM_STATUS = 2;
  // Modem Status
  FT_CTS = $10;
  FT_DSR = $20;
  FT_RI  = $40;
  FT_DCD = $80;

  // device chip types
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

  // IO Buffer Sizes
  FT_In_Buffer_Size = $10000;    // 64k
  FT_In_Buffer_Index = FT_In_Buffer_Size - 1;
  FT_Out_Buffer_Size = $10000;    // 64k
  FT_Out_Buffer_Index = FT_Out_Buffer_Size - 1;
  // DLL Name
{$ifdef WINDOWS}
  FT_DLL_Name = 'FTD2XX.DLL';
{$else}
  FT_DLL_Name = 'FTD2XX';
{$endif}

var
  // Declare Input and Output Buffers
  FT_In_Buffer: array[0..FT_In_Buffer_Index] of byte;
  FT_Out_Buffer: array[0..FT_Out_Buffer_Index] of byte;
  // A variable used to detect time-outs
  // Attach a timer to the main project form
  // which decrements this every 10mS if
  // FT_TimeOut_Count <> 0
  FT_TimeOut_Count: integer = 0;
  // Used to determine how many bytes were
  // actually received by FT_Read_Device_All
  // in the case of a time-out
  FT_All_Bytes_Received: integer = 0;
  FT_IO_Status: Ft_Result = FT_OK;
  // Used By FT_ListDevices
  FT_Device_Count: DWord;
  FT_Device_String_Buffer: array [1..50] of char;
  FT_Device_String: string;
  FT_Device_Location: DWord;
  USB_Device_Info_Node: FT_Device_Info_Node;
  FT_Event_Handle: DWord;

//Classic functions
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_ListDevices{$IFNDEF S}: function{$ENDIF}(pvArg1: Dword; pvArg2: Pointer; dwFlags: Dword): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_GetNumDevices{$IFNDEF S}: function{$ENDIF}(pvArg1:Pointer; pvArg2:Pointer; dwFlags:Dword):FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name name 'FT_ListDevices';{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_Open{$IFNDEF S}: function{$ENDIF}(Index: integer; ftHandle: Pointer): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_OpenEx{$IFNDEF S}: function{$ENDIF}(pvArg1: Pointer; dwFlags: Dword; ftHandle: Pointer): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_OpenByLocation{$IFNDEF S}: function{$ENDIF}(pvArg1:DWord; dwFlags:Dword; ftHandle:Pointer):FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name name 'FT_OpenEx';{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_Close{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_Read{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword; FTInBuf: Pointer; BufferSize: longint; ResultPtr: Pointer): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_Write{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword; FTOutBuf: Pointer; BufferSize: longint; ResultPtr: Pointer): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_ResetDevice{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_SetBaudRate{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword; BaudRate: DWord): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_SetDivisor{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword; Divisor: DWord): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_SetDataCharacteristics{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword; WordLength, StopBits, Parity: byte): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_SetFlowControl{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword; FlowControl: word; XonChar, XoffChar: byte): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_SetDtr{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_ClrDtr{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_SetRts{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_ClrRts{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_GetModemStatus{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword; ModemStatus: Pointer): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_SetChars{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword; EventChar, EventCharEnabled, ErrorChar, ErrorCharEnabled: byte): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_Purge{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword; Mask: Dword): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_SetTimeouts{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword; ReadTimeout, WriteTimeout: Dword): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_GetQueueStatus{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword; RxBytes: Pointer): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_SetBreakOn{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_SetBreakOff{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_GetStatus{$IFNDEF S}: function{$ENDIF}(ftHandle: DWord; RxBytes, TxBytes, EventStatus: Pointer): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_SetEventNotification{$IFNDEF S}: function{$ENDIF}(ftHandle: DWord; EventMask: DWord; pvArgs: Dword): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_GetDeviceInfo{$IFNDEF S}: function{$ENDIF}(ftHandle: DWord; DevType, ID, SerNum, Desc, pvDummy: Pointer): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_SetResetPipeRetryCount{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword; RetryCount: Dword): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_StopInTask{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_RestartInTask{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_ResetPort{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_CyclePort{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_CreateDeviceInfoList{$IFNDEF S}: function{$ENDIF}(NumDevs: Pointer): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_GetDeviceInfoList{$IFNDEF S}: function{$ENDIF}(pFT_Device_Info_List: Pointer; NumDevs: Pointer): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_GetDeviceInfoDetail{$IFNDEF S}: function{$ENDIF}(Index: DWord; Flags, DevType, ID, LocID, SerialNumber, Description, DevHandle: Pointer): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_GetDriverVersion{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword; DrVersion: Pointer): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_GetLibraryVersion{$IFNDEF S}: function{$ENDIF}(LbVersion: Pointer): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_GetComPortNumber{$IFNDEF S}: function{$ENDIF}(ftHandle: DWord; pComPortNumber: Pointer): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}

// EEPROM functions
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_EE_Read{$IFNDEF S}: function{$ENDIF}(ftHandle: DWord; pEEData: Pointer): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_EE_Program{$IFNDEF S}: function{$ENDIF}(ftHandle: DWord; pEEData: Pointer): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
// EEPROM primitives - you need an NDA for EEPROM checksum
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_ReadEE{$IFNDEF S}: function{$ENDIF}(ftHandle: DWord; WordAddr: DWord; WordRead: Pointer): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_WriteEE{$IFNDEF S}: function{$ENDIF}(ftHandle: DWord; WordAddr: DWord; WordData: word): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_EraseEE{$IFNDEF S}: function{$ENDIF}(ftHandle: DWord): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_EE_UARead{$IFNDEF S}: function{$ENDIF}(ftHandle: DWord; Data: Pointer; DataLen: DWord; BytesRead: Pointer): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_EE_UAWrite{$IFNDEF S}: function{$ENDIF}(ftHandle: DWord; Data: Pointer; DataLen: DWord): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_EE_UASize{$IFNDEF S}: function{$ENDIF}(ftHandle: DWord; UASize: Pointer): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}

// FT2232C, FT232BM and FT245BM Extended API Functions
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_GetLatencyTimer{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword; Latency: Pointer): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_SetLatencyTimer{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword; Latency: byte): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_GetBitMode{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword; BitMode: Pointer): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_SetBitMode{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword; Mask, Enable: byte): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}
{$IFDEF S}function {$ELSE}var {$ENDIF}FT_SetUSBParameters{$IFNDEF S}: function{$ENDIF}(ftHandle: Dword; InSize, OutSize: Dword): FT_Result; stdcall;{$IFDEF S}external FT_DLL_Name;{$ENDIF}


implementation

var
{$IFDEF FPC}
  Libhandle: TLibHandle;
{$ELSE}
  Libhandle: HMODULE;
{$ENDIF}

function LoadLib(): Boolean;
begin
  Libhandle := LoadLibrary(FT_DLL_Name);
  if Libhandle <> 0 then
  begin
    FT_ListDevices := GetProcAddress(Libhandle, 'FT_ListDevices');
    FT_GetNumDevices := GetProcAddress(Libhandle, 'FT_ListDevices');
    FT_Open := GetProcAddress(Libhandle, 'FT_Open');
    FT_OpenEx := GetProcAddress(Libhandle, 'FT_OpenEx');
    FT_OpenByLocation := GetProcAddress(Libhandle, 'FT_OpenEx');
    FT_Close := GetProcAddress(Libhandle, 'FT_Close');
    FT_Read := GetProcAddress(Libhandle, 'FT_Read');
    FT_Write := GetProcAddress(Libhandle, 'FT_Write');
    FT_ResetDevice := GetProcAddress(Libhandle, 'FT_ResetDevice');
    FT_SetBaudRate := GetProcAddress(Libhandle, 'FT_SetBaudRate');
    FT_SetDivisor := GetProcAddress(Libhandle, 'FT_SetDivisor');
    FT_SetDataCharacteristics := GetProcAddress(Libhandle, 'FT_SetDataCharacteristics');
    FT_SetFlowControl := GetProcAddress(Libhandle, 'FT_SetFlowControl');
    FT_SetDtr := GetProcAddress(Libhandle, 'FT_SetDtr');
    FT_ClrDtr := GetProcAddress(Libhandle, 'FT_ClrDtr');
    FT_SetRts := GetProcAddress(Libhandle, 'FT_SetRts');
    FT_ClrRts := GetProcAddress(Libhandle, 'FT_ClrRts');
    FT_GetModemStatus := GetProcAddress(Libhandle, 'FT_GetModemStatus');
    FT_SetChars := GetProcAddress(Libhandle, 'FT_SetChars');
    FT_Purge := GetProcAddress(Libhandle, 'FT_Purge');
    FT_SetTimeouts := GetProcAddress(Libhandle, 'FT_SetTimeouts');
    FT_GetQueueStatus := GetProcAddress(Libhandle, 'FT_GetQueueStatus');
    FT_SetBreakOn := GetProcAddress(Libhandle, 'FT_SetBreakOn');
    FT_SetBreakOff := GetProcAddress(Libhandle, 'FT_SetBreakOff');
    FT_GetStatus := GetProcAddress(Libhandle, 'FT_GetStatus');
    FT_SetEventNotification := GetProcAddress(Libhandle, 'FT_SetEventNotification');
    FT_GetDeviceInfo := GetProcAddress(Libhandle, 'FT_GetDeviceInfo');
    FT_SetResetPipeRetryCount := GetProcAddress(Libhandle, 'FT_SetResetPipeRetryCount');
    FT_StopInTask := GetProcAddress(Libhandle, 'FT_StopInTask');
    FT_RestartInTask := GetProcAddress(Libhandle, 'FT_RestartInTask');
    FT_ResetPort := GetProcAddress(Libhandle, 'FT_ResetPort');
    FT_CyclePort := GetProcAddress(Libhandle, 'FT_CyclePort');
    FT_CreateDeviceInfoList := GetProcAddress(Libhandle, 'FT_CreateDeviceInfoList');
    FT_GetDeviceInfoList := GetProcAddress(Libhandle, 'FT_GetDeviceInfoList');
    FT_GetDeviceInfoDetail := GetProcAddress(Libhandle, 'FT_GetDeviceInfoDetail');
    FT_GetDriverVersion := GetProcAddress(Libhandle, 'FT_GetDriverVersion');
    FT_GetLibraryVersion := GetProcAddress(Libhandle, 'FT_GetLibraryVersion');
    FT_GetComPortNumber := GetProcAddress(Libhandle, 'FT_GetComPortNumber');

    FT_EE_Read := GetProcAddress(Libhandle, 'FT_EE_Read');
    FT_EE_Program := GetProcAddress(Libhandle, 'FT_EE_Program');
    FT_ReadEE := GetProcAddress(Libhandle, 'FT_ReadEE');
    FT_WriteEE := GetProcAddress(Libhandle, 'FT_WriteEE');
    FT_EraseEE := GetProcAddress(Libhandle, 'FT_EraseEE');
    FT_EE_UARead := GetProcAddress(Libhandle, 'FT_EE_UARead');
    FT_EE_UAWrite := GetProcAddress(Libhandle, 'FT_EE_UAWrite');
    FT_EE_UASize := GetProcAddress(Libhandle, 'FT_EE_UASize');
    FT_GetLatencyTimer := GetProcAddress(Libhandle, 'FT_GetLatencyTimer');
    FT_SetLatencyTimer := GetProcAddress(Libhandle, 'FT_SetLatencyTimer');
    FT_GetBitMode := GetProcAddress(Libhandle, 'FT_GetBitMode');
    FT_SetBitMode := GetProcAddress(Libhandle, 'FT_SetBitMode');
    FT_SetUSBParameters := GetProcAddress(Libhandle, 'FT_SetUSBParameters');
  end;
  Result:=(Libhandle <> 0);
end;

procedure UnloadLib();
begin
  if Libhandle <> 0 then
  begin
    FreeLibrary(Libhandle);
    Libhandle := 0;
  end;
end;


function FT_Error_Report(ErrStr: string; PortStatus: Integer): string;
var
  Str: string;
begin
  Result := '';
  if not FT_Enable_Error_Report then
  begin
    Exit;
  end;
  if PortStatus = FT_OK then
  begin
    Exit;
  end;
  case PortStatus of
    FT_INVALID_HANDLE:
    begin
      Str := ErrStr + ' - Invalid handle...';
    end;
    FT_DEVICE_NOT_FOUND:
    begin
      Str := ErrStr + ' - Device not found...';
    end;
    FT_DEVICE_NOT_OPENED:
    begin
      Str := ErrStr + ' - Device not opened...';
    end;
    FT_IO_ERROR:
    begin
      Str := ErrStr + ' - General IO error...';
    end;
    FT_INSUFFICIENT_RESOURCES:
    begin
      Str := ErrStr + ' - Insufficient resources...';
    end;
    FT_INVALID_PARAMETER:
    begin
      Str := ErrStr + ' - Invalid parameter...';
    end;
    FT_INVALID_BAUD_RATE:
    begin
      Str := ErrStr + ' - Invalid baud rate...';
    end;
    FT_DEVICE_NOT_OPENED_FOR_ERASE:
    begin
      Str := ErrStr + ' Device not opened for erase...';
    end;
    FT_DEVICE_NOT_OPENED_FOR_WRITE:
    begin
      Str := ErrStr + ' Device not opened for write...';
    end;
    FT_FAILED_TO_WRITE_DEVICE:
    begin
      Str := ErrStr + ' - Failed to write...';
    end;
    FT_EEPROM_READ_FAILED:
    begin
      Str := ErrStr + ' - EEPROM read failed...';
    end;
    FT_EEPROM_WRITE_FAILED:
    begin
      Str := ErrStr + ' - EEPROM write failed...';
    end;
    FT_EEPROM_ERASE_FAILED:
    begin
      Str := ErrStr + ' - EEPROM erase failed...';
    end;
    FT_EEPROM_NOT_PRESENT:
    begin
      Str := ErrStr + ' - EEPROM not present...';
    end;
    FT_EEPROM_NOT_PROGRAMMED:
    begin
      Str := ErrStr + ' - EEPROM not programmed...';
    end;
    FT_INVALID_ARGS:
    begin
      Str := ErrStr + ' - Invalid arguments...';
    end;
    FT_OTHER_ERROR:
    begin
      Str := ErrStr + ' - Other error ...';
    end;
  end;
  //MessageDlg(Str, mtError, [mbOk], 0);
  Result := Str;
end;


function GetDeviceString: string;
var
  I: Integer;
begin
  Result := '';
  I := 1;
  FT_Device_String_Buffer[50] := Chr(0); // Just in case !
  while FT_Device_String_Buffer[I] <> Chr(0) do
  begin
    Result := Result + FT_Device_String_Buffer[I];
    Inc(I);
  end;
end;


procedure SetDeviceString(S: string);
var
  I, L: Integer;
begin
  FT_Device_String_Buffer[1] := Chr(0);
  L := Length(S);
  if L > 49 then
  begin
    L := 49;
  end;
  if L = 0 then
  begin
    Exit;
  end;
  for I := 1 to L do
  begin
    FT_Device_String_Buffer[I] := S[I];
  end;
  FT_Device_String_Buffer[L + 1] := Chr(0);
end;


// FTD2XX functions from here
function GetFTDeviceCount: FT_Result;
begin
  Result := FT_GetNumDevices(@FT_Device_Count, nil, FT_LIST_NUMBER_ONLY);
  if Result <> FT_OK then
  begin
    FT_Error_Report('GetFTDeviceCount', Result);
  end;
end;

function GetFTDeviceSerialNo(DeviceIndex: DWord): FT_Result;
begin
  Result := FT_ListDevices(DeviceIndex, @SerialNumber,
    (FT_OPEN_BY_SERIAL_NUMBER or FT_LIST_BY_INDEX));
  if Result = FT_OK then
  begin
    FT_Device_String := SerialNumber;
  end;
  if Result <> FT_OK then
  begin
    FT_Error_Report('GetFTDeviceSerialNo', Result);
  end;
end;


function GetFTDeviceDescription(DeviceIndex: DWord): FT_Result;
begin
  Result := FT_ListDevices(DeviceIndex, @Description,
    (FT_OPEN_BY_DESCRIPTION or FT_LIST_BY_INDEX));
  if Result = FT_OK then
  begin
    FT_Device_String := Description;
  end;
  if Result <> FT_OK then
  begin
    FT_Error_Report('GetFTDeviceDescription', Result);
  end;
end;


function GetFTDeviceLocation(DeviceIndex: DWord): FT_Result;
begin
  Result := FT_ListDevices(DeviceIndex, @LocID, (FT_OPEN_BY_LOCATION or FT_LIST_BY_INDEX));
  if Result = FT_OK then
  begin
    FT_Device_Location := LocID;
  end;
  if Result <> FT_OK then
  begin
    FT_Error_Report('GetFTDeviceLocation', Result);
  end;
end;


function Open_USB_Device: FT_Result;
var
  DevIndex: DWord;
begin
  DevIndex := 0;
  Result := FT_Open(DevIndex, @FT_Handle);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_Open', Result);
  end;
end;


function Open_USB_Device_By_Serial_Number(Serial_Number: string): FT_Result;
begin
  SetDeviceString(Serial_Number);
  Result := FT_OpenEx(@FT_Device_String_Buffer, FT_OPEN_BY_SERIAL_NUMBER, @FT_Handle);
  if Result <> FT_OK then
  begin
    FT_Error_Report('Open_USB_Device_By_Serial_Number', Result);
  end;
end;


function Open_USB_Device_By_Device_Description(Device_Description: string): FT_Result;
begin
  SetDeviceString(Device_Description);
  Result := FT_OpenEx(@FT_Device_String_Buffer, FT_OPEN_BY_DESCRIPTION, @FT_Handle);
  if Result <> FT_OK then
  begin
    FT_Error_Report('Open_USB_Device_By_Device_Description', Result);
  end;
end;


function Open_USB_Device_By_Device_Location(Location: DWord): FT_Result;
begin
  Result := FT_OpenByLocation(Location, FT_OPEN_BY_LOCATION, @FT_Handle);
  if Result <> FT_OK then
  begin
    FT_Error_Report('Open_USB_Device_By_Device_Location', Result);
  end;
end;


function Close_USB_Device: FT_Result;
begin
  Result := FT_Close(FT_Handle);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_Close', Result);
  end;
end;


function Read_USB_Device_Buffer(Read_Count: Integer): Integer;
  // Reads Read_Count Bytes (or less) from the USB device to the FT_In_Buffer
  // Function returns the number of bytes actually received  which may range from zero
  // to the actual number of bytes requested, depending on how many have been received
  // at the time of the request + the read timeout value.
var
  Read_Result: Integer;
begin

  if (read_count = 1) then
  begin
    read_result := read_count;
  end;
  FT_IO_Status := FT_Read(FT_Handle, @FT_In_Buffer, Read_Count, @Read_Result);
  if FT_IO_Status <> FT_OK then
  begin
    FT_Error_Report('FT_Read', FT_IO_Status);
  end;
  Result := Read_Result;
end;


function Write_USB_Device_Buffer(Write_Count: Integer): Integer;
  // Writes Write_Count Bytes from FT_Out_Buffer to the USB device
  // Function returns the number of bytes actually sent
  // In this example, Write_Count should be 32k bytes max
var
  Write_Result: Integer;
begin
  FT_IO_Status := FT_Write(FT_Handle, @FT_Out_Buffer, Write_Count, @Write_Result);
  if FT_IO_Status <> FT_OK then
  begin
    FT_Error_Report('FT_Write', FT_IO_Status);
  end;
  Result := Write_Result;
end;


function Reset_USB_Device: FT_Result;
begin
  Result := FT_ResetDevice(FT_Handle);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_ResetDevice', Result);
  end;
end;


function Set_USB_Device_BaudRate: FT_Result;
begin
  Result := FT_SetBaudRate(FT_Handle, FT_Current_Baud);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_SetBaudRate', Result);
  end;
end;


function Set_USB_Device_BaudRate_Divisor(Divisor: Dword): FT_Result;
begin
  Result := FT_SetDivisor(FT_Handle, Divisor);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_SetDivisor', Result);
  end;
end;


function Set_USB_Device_DataCharacteristics: FT_Result;
begin
  Result := FT_SetDataCharacteristics(FT_Handle, FT_Current_DataBits,
    FT_Current_StopBits, FT_Current_Parity);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_SetDataCharacteristics', Result);
  end;
end;


function Set_USB_Device_FlowControl: FT_Result;
begin
  Result := FT_SetFlowControl(FT_Handle, FT_Current_FlowControl, FT_XON_Value,
    FT_XOFF_Value);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_SetFlowControl', Result);
  end;
end;


function Set_USB_Device_RTS: FT_Result;
begin
  Result := FT_SetRTS(FT_Handle);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_SetRTS', Result);
  end;
end;


function Clr_USB_Device_RTS: FT_Result;
begin
  Result := FT_ClrRTS(FT_Handle);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_ClrRTS', Result);
  end;
end;


function Set_USB_Device_DTR: FT_Result;
begin
  Result := FT_SetDTR(FT_Handle);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_SetDTR', Result);
  end;
end;


function Clr_USB_Device_DTR: FT_Result;
begin
  Result := FT_ClrDTR(FT_Handle);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_ClrDTR', Result);
  end;
end;


function Get_USB_Device_ModemStatus: FT_Result;
begin
  Result := FT_GetModemStatus(FT_Handle, @FT_Modem_Status);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_GetModemStatus', Result);
  end;
end;


function Set_USB_Device_Chars: FT_Result;
var
  Events_On, Errors_On: byte;
begin
  if FT_Event_On then
  begin
    Events_On := 1;
  end
  else
  begin
    Events_On := 0;
  end;
  if FT_Error_On then
  begin
    Errors_On := 1;
  end
  else
  begin
    Errors_On := 0;
  end;
  Result := FT_SetChars(FT_Handle, FT_EVENT_Value, Events_On, FT_ERROR_Value, Errors_On);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_SetChars', Result);
  end;
end;


function Purge_USB_Device_Out: FT_Result;
begin
  Result := FT_Purge(FT_Handle, FT_PURGE_RX);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_Purge RX', Result);
  end;
end;

function Purge_USB_Device_In: FT_Result;
begin
  Result := FT_Purge(FT_Handle, FT_PURGE_TX);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_Purge TX', Result);
  end;
end;


function Set_USB_Device_TimeOuts(ReadTimeOut, WriteTimeOut: DWord): FT_Result;
begin
  Result := FT_SetTimeouts(FT_Handle, ReadTimeout, WriteTimeout);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_SetTimeouts', Result);
  end;
end;


function Get_USB_Device_QueueStatus: FT_Result;
begin
  Result := FT_GetQueueStatus(FT_Handle, @FT_Q_Bytes);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_GetQueueStatus', Result);
  end;
end;


function Set_USB_Device_Break_On: FT_Result;
begin
  Result := FT_SetBreakOn(FT_Handle);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_SetBreakOn', Result);
  end;
end;


function Set_USB_Device_Break_Off: FT_Result;
begin
  Result := FT_SetBreakOff(FT_Handle);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_SetBreakOff', Result);
  end;
end;


function Get_USB_Device_Status: FT_Result;
begin
  Result := FT_GetStatus(FT_Handle, @FT_Q_Bytes, @FT_TxQ_Bytes, @FT_Event_Status);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_GetStatus', Result);
  end;
end;


function Set_USB_Device_Event_Notification(EventMask: DWord): FT_Result;
begin
  Result := FT_SetEventNotification(FT_Handle, EventMask, FT_Event_Handle);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_SetEventNotification ', Result);
  end;
end;


function USB_FT_GetDeviceInfo(DevType, ID: DWord;
  SerialNumber, Description: array of char): FT_Result;
begin
  Result := FT_GetDeviceInfo(FT_Handle, @DevType, @ID, @SerialNumber, @Description, nil);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_GetDeviceInfo ', Result);
  end;
end;


function Set_USB_Device_Reset_Pipe_Retry_Count(RetryCount: DWord): FT_Result;
begin
  Result := FT_SetResetPiperetryCount(FT_Handle, RetryCount);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_SetResetPipeRetryCount', Result);
  end;
end;


function Stop_USB_Device_InTask: FT_Result;
begin
  Result := FT_StopInTask(FT_Handle);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_StopInTask', Result);
  end;
end;


function Restart_USB_Device_InTask: FT_Result;
begin
  Result := FT_RestartInTask(FT_Handle);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_RestartInTask', Result);
  end;
end;


function Reset_USB_Port: FT_Result;
begin
  Result := FT_ResetPort(FT_Handle);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_ResetPort', Result);
  end;
end;


function Cycle_USB_Port: FT_Result;
begin
  Result := FT_CyclePort(FT_Handle);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_CyclePort', Result);
  end;
end;


function Create_USB_Device_List: FT_Result;
begin
  Result := FT_CreateDeviceInfoList(@FT_Device_Count);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_CreateDeviceInfoList', Result);
  end;
end;


function Get_USB_Device_List: FT_Result;
begin
  SetLength(FT_DeviceInfoList, FT_Device_Count);
  Result := FT_GetDeviceInfoList(@FT_DeviceInfoList, @FT_Device_Count);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_GetDeviceInfoList', Result);
  end;
end;

function Get_USB_Driver_Version(DrVersion: TDWordPtr): FT_Result;
begin
  Result := FT_GetDriverVersion(FT_Handle, DrVersion);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_GetDriverVersion', Result);
  end;
end;

function Get_USB_Library_Version(LbVersion: TDWordPtr): FT_Result;
begin
  Result := FT_GetLibraryVersion(LbVersion);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_GetLibraryVersion', Result);
  end;
end;


function Get_USB_Device_List_Detail(Index: DWord): FT_Result;
begin
  // Initialise structure
  USB_Device_Info_Node.Flags := 0;
  USB_Device_Info_Node.DeviceType := 0;
  USB_Device_Info_Node.ID := 0;
  USB_Device_Info_Node.LocID := 0;
  USB_Device_Info_Node.SerialNumber := '';
  USB_Device_Info_Node.Description := '';
  USB_Device_Info_Node.DeviceHandle := 0;
  Result := FT_GetDeviceInfoDetail(Index, @USB_Device_Info_Node.Flags,
    @USB_Device_Info_Node.DeviceType, @USB_Device_Info_Node.ID,
    @USB_Device_Info_Node.LocID, @USB_Device_Info_Node.SerialNumber,
    @USB_Device_Info_Node.Description, @USB_Device_Info_Node.DeviceHandle);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_GetDeviceInfoListDetail', Result);
  end;
end;


function USB_FT_EE_Read: FT_Result;
  // Read BM/AM device EEPROM
begin
  EEDataBuffer.Signature1 := 0;
  EEDataBuffer.Signature2 := $FFFFFFFF;
  EEDataBuffer.Version := 0;  // 0 for AM/BM, 1 for C, 2 for R
  EEDataBuffer.VendorId := 0;
  EEDataBuffer.ProductId := 0;
  EEDataBuffer.Manufacturer := @Manufacturer;
  EEDataBuffer.ManufacturerId := @ManufacturerId;
  EEDataBuffer.Description := @Description;
  EEDataBuffer.SerialNumber := @SerialNumber;
  EEDataBuffer.MaxPower := 0;
  EEDataBuffer.PnP := 0;
  EEDataBuffer.SelfPowered := 0;
  EEDataBuffer.RemoteWakeup := 0;
  EEDataBuffer.Rev4 := 0;
  EEDataBuffer.IsoIn := 0;
  EEDataBuffer.IsoOut := 0;
  EEDataBuffer.PullDownEnable := 0;
  EEDataBuffer.SerNumEnable := 0;
  EEDataBuffer.USBVersionEnable := 0;
  EEDataBuffer.USBVersion := 0;
  // FT2232C Extensions
  EEDataBuffer.Rev5 := 0;
  EEDataBuffer.IsoInA := 0;
  EEDataBuffer.IsoInB := 0;
  EEDataBuffer.IsoOutA := 0;
  EEDataBuffer.IsoOutB := 0;
  EEDataBuffer.PullDownEnable5 := 0;
  EEDataBuffer.SerNumEnable5 := 0;
  EEDataBuffer.USBVersionEnable5 := 0;
  EEDataBuffer.USBVersion5 := 0;
  EEDataBuffer.AIsHighCurrent := 0;
  EEDataBuffer.BIsHighCurrent := 0;
  EEDataBuffer.IFAIsFifo := 0;
  EEDataBuffer.IFAIsFifoTar := 0;
  EEDataBuffer.IFAIsFastSer := 0;
  EEDataBuffer.AIsVCP := 0;
  EEDataBuffer.IFBIsFifo := 0;
  EEDataBuffer.IFBIsFifoTar := 0;
  EEDataBuffer.IFBIsFastSer := 0;
  EEDataBuffer.BIsVCP := 0;
  // FT232R extensions
  EEDataBuffer.UseExtOsc := 0;
  EEDataBuffer.HighDriveIOs := 0;
  EEDataBuffer.EndpointSize := 0;
  EEDataBuffer.PullDownEnableR := 0;
  EEDataBuffer.SerNumEnableR := 0;
  EEDataBuffer.InvertTXD := 0;
  EEDataBuffer.InvertRXD := 0;
  EEDataBuffer.InvertRTS := 0;
  EEDataBuffer.InvertCTS := 0;
  EEDataBuffer.InvertDTR := 0;
  EEDataBuffer.InvertDSR := 0;
  EEDataBuffer.InvertDCD := 0;
  EEDataBuffer.InvertRI := 0;
  EEDataBuffer.Cbus0 := 0;
  EEDataBuffer.Cbus1 := 0;
  EEDataBuffer.Cbus2 := 0;
  EEDataBuffer.Cbus3 := 0;
  EEDataBuffer.Cbus4 := 0;
  EEDataBuffer.RIsVCP := 0;
  Result := FT_EE_Read(FT_Handle, @EEDataBuffer);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_EE_Read ', Result);
  end;
end;


function USB_FT_C_EE_Read: FT_Result;
  // Read FT2232C device EEPROM
begin
  EEDataBuffer.Signature1 := 0;
  EEDataBuffer.Signature2 := $FFFFFFFF;
  EEDataBuffer.Version := 1;  // 0 for AM/BM, 1 for C, 2 for R
  EEDataBuffer.VendorId := 0;
  EEDataBuffer.ProductId := 0;
  EEDataBuffer.Manufacturer := @Manufacturer;
  EEDataBuffer.ManufacturerId := @ManufacturerId;
  EEDataBuffer.Description := @Description;
  EEDataBuffer.SerialNumber := @SerialNumber;
  EEDataBuffer.MaxPower := 0;
  EEDataBuffer.PnP := 0;
  EEDataBuffer.SelfPowered := 0;
  EEDataBuffer.RemoteWakeup := 0;
  EEDataBuffer.Rev4 := 0;
  EEDataBuffer.IsoIn := 0;
  EEDataBuffer.IsoOut := 0;
  EEDataBuffer.PullDownEnable := 0;
  EEDataBuffer.SerNumEnable := 0;
  EEDataBuffer.USBVersionEnable := 0;
  EEDataBuffer.USBVersion := 0;
  // FT2232C Extensions
  EEDataBuffer.Rev5 := 0;
  EEDataBuffer.IsoInA := 0;
  EEDataBuffer.IsoInB := 0;
  EEDataBuffer.IsoOutA := 0;
  EEDataBuffer.IsoOutB := 0;
  EEDataBuffer.PullDownEnable5 := 0;
  EEDataBuffer.SerNumEnable5 := 0;
  EEDataBuffer.USBVersionEnable5 := 0;
  EEDataBuffer.USBVersion5 := 0;
  EEDataBuffer.AIsHighCurrent := 0;
  EEDataBuffer.BIsHighCurrent := 0;
  EEDataBuffer.IFAIsFifo := 0;
  EEDataBuffer.IFAIsFifoTar := 0;
  EEDataBuffer.IFAIsFastSer := 0;
  EEDataBuffer.AIsVCP := 0;
  EEDataBuffer.IFBIsFifo := 0;
  EEDataBuffer.IFBIsFifoTar := 0;
  EEDataBuffer.IFBIsFastSer := 0;
  EEDataBuffer.BIsVCP := 0;
  // FT232R extensions
  EEDataBuffer.UseExtOsc := 0;
  EEDataBuffer.HighDriveIOs := 0;
  EEDataBuffer.EndpointSize := 0;
  EEDataBuffer.PullDownEnableR := 0;
  EEDataBuffer.SerNumEnableR := 0;
  EEDataBuffer.InvertTXD := 0;
  EEDataBuffer.InvertRXD := 0;
  EEDataBuffer.InvertRTS := 0;
  EEDataBuffer.InvertCTS := 0;
  EEDataBuffer.InvertDTR := 0;
  EEDataBuffer.InvertDSR := 0;
  EEDataBuffer.InvertDCD := 0;
  EEDataBuffer.InvertRI := 0;
  EEDataBuffer.Cbus0 := 0;
  EEDataBuffer.Cbus1 := 0;
  EEDataBuffer.Cbus2 := 0;
  EEDataBuffer.Cbus3 := 0;
  EEDataBuffer.Cbus4 := 0;
  EEDataBuffer.RIsVCP := 0;
  Result := FT_EE_Read(FT_Handle, @EEDataBuffer);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_EE_Read ', Result);
  end;
end;


function USB_FT_R_EE_Read: FT_Result;
  // Read FT232R device EEPROM
begin
  EEDataBuffer.Signature1 := 0;
  EEDataBuffer.Signature2 := $FFFFFFFF;
  EEDataBuffer.Version := 2;  // 0 for AM/BM, 1 for C, 2 for R
  EEDataBuffer.VendorId := 0;
  EEDataBuffer.ProductId := 0;
  EEDataBuffer.Manufacturer := @Manufacturer;
  EEDataBuffer.ManufacturerId := @ManufacturerId;
  EEDataBuffer.Description := @Description;
  EEDataBuffer.SerialNumber := @SerialNumber;
  EEDataBuffer.MaxPower := 0;
  EEDataBuffer.PnP := 0;
  EEDataBuffer.SelfPowered := 0;
  EEDataBuffer.RemoteWakeup := 0;
  EEDataBuffer.Rev4 := 0;
  EEDataBuffer.IsoIn := 0;
  EEDataBuffer.IsoOut := 0;
  EEDataBuffer.PullDownEnable := 0;
  EEDataBuffer.SerNumEnable := 0;
  EEDataBuffer.USBVersionEnable := 0;
  EEDataBuffer.USBVersion := 0;
  // FT2232C Extensions
  EEDataBuffer.Rev5 := 0;
  EEDataBuffer.IsoInA := 0;
  EEDataBuffer.IsoInB := 0;
  EEDataBuffer.IsoOutA := 0;
  EEDataBuffer.IsoOutB := 0;
  EEDataBuffer.PullDownEnable5 := 0;
  EEDataBuffer.SerNumEnable5 := 0;
  EEDataBuffer.USBVersionEnable5 := 0;
  EEDataBuffer.USBVersion5 := 0;
  EEDataBuffer.AIsHighCurrent := 0;
  EEDataBuffer.BIsHighCurrent := 0;
  EEDataBuffer.IFAIsFifo := 0;
  EEDataBuffer.IFAIsFifoTar := 0;
  EEDataBuffer.IFAIsFastSer := 0;
  EEDataBuffer.AIsVCP := 0;
  EEDataBuffer.IFBIsFifo := 0;
  EEDataBuffer.IFBIsFifoTar := 0;
  EEDataBuffer.IFBIsFastSer := 0;
  EEDataBuffer.BIsVCP := 0;
  // FT232R extensions
  EEDataBuffer.UseExtOsc := 0;
  EEDataBuffer.HighDriveIOs := 0;
  EEDataBuffer.EndpointSize := 0;
  EEDataBuffer.PullDownEnableR := 0;
  EEDataBuffer.SerNumEnableR := 0;
  EEDataBuffer.InvertTXD := 0;
  EEDataBuffer.InvertRXD := 0;
  EEDataBuffer.InvertRTS := 0;
  EEDataBuffer.InvertCTS := 0;
  EEDataBuffer.InvertDTR := 0;
  EEDataBuffer.InvertDSR := 0;
  EEDataBuffer.InvertDCD := 0;
  EEDataBuffer.InvertRI := 0;
  EEDataBuffer.Cbus0 := 0;
  EEDataBuffer.Cbus1 := 0;
  EEDataBuffer.Cbus2 := 0;
  EEDataBuffer.Cbus3 := 0;
  EEDataBuffer.Cbus4 := 0;
  EEDataBuffer.RIsVCP := 0;
  Result := FT_EE_Read(FT_Handle, @EEDataBuffer);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_EE_Read ', Result);
  end;
end;


function USB_FT_EE_Program: FT_Result;
begin
  Result := FT_EE_Program(FT_Handle, @EEDataBuffer);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_EE_Read ', Result);
  end;
end;


function USB_FT_WriteEE(WordAddr: Dword; WordData: word): FT_Result;
begin
  Result := FT_WriteEE(FT_Handle, WordAddr, WordData);
end;


function USB_FT_ReadEE(WordAddr: Dword): FT_Result;
begin
  Result := FT_ReadEE(FT_Handle, WordAddr, @WordRead);
end;


function USB_FT_EraseEE: FT_Result;
begin
  Result := FT_EraseEE(FT_Handle);
end;


function USB_FT_EE_UARead: FT_Result;
begin
  Result := FT_EE_UARead(FT_Handle, @UserData, 64, @FT_UA_Size);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_EE_UARead ', Result);
  end;
end;


function USB_FT_EE_UAWrite: FT_Result;
begin
  Result := FT_EE_UAWrite(FT_Handle, @UserData, FT_UA_Size);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_EE_UAWrite ', Result);
  end;
end;


function USB_FT_EE_UASize: FT_Result;
begin
  Result := FT_EE_UASize(FT_Handle, @FT_UA_Size);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_EE_UASize ', Result);
  end;
end;


function Get_USB_Device_LatencyTimer: FT_Result;
begin
  Result := FT_GetLatencyTimer(FT_Handle, @FT_LatencyRd);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_GetLatencyTimer ', Result);
  end;
end;


function Set_USB_Device_LatencyTimer(Latency: byte): FT_Result;
begin
  Result := FT_SetLatencyTimer(FT_Handle, Latency);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_SetLatencyTimer ', Result);
  end;
end;


function Get_USB_Device_BitMode(var BitMode: byte): FT_Result;
begin
  Result := FT_GetBitMode(FT_Handle, @BitMode);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_GetBitMode ', Result);
  end;
end;


function Set_USB_Device_BitMode(Mask, Enable: byte): FT_Result;
begin
  Result := FT_SetBitMode(FT_Handle, Mask, Enable);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_SetBitMode ', Result);
  end;
end;


function Set_USB_Parameters(InSize, OutSize: Dword): FT_Result;
begin
  Result := FT_SetUSBParameters(FT_Handle, InSize, OutSize);
  if Result <> FT_OK then
  begin
    FT_Error_Report('FT_SetUSBParameters ', Result);
  end;
end;

{$IFNDEF S}
initialization
  Libhandle := 0;
  LoadLib();

finalization
  UnloadLib();
{$ENDIF}

end.

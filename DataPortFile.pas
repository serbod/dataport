{
Sergey Bodrov, 2012-2015

Data exchange via file. Suitable for device files (/dev/* under Unix or special
files in Windows). Conventional data files can be used too.

Properties:
  Filename - Path (optionally) and name of file.
  FilePos - Current position in file, bytes from beginning (for conventional files).
  QueryInterval - Interval for checking changes in file, milliseconds.
  MinDataBytes - Minimum number of bytes in buffer for triggering OnDataAppear event.
  KeepOpen - Keep the file open between read and write operations:
    True - file stay opened
    False - file will be opened before every read/write operation and closed after.
  WriteMode - File write mode:
    fwmRewrite - every write apply to beginning of file
    fwmAppend - data written from last operation position or appended to the end of file

Methods:
  Open() - Opens file with given name, "file:" prefix can be used.
}
{ TODO : Add thread-safe file reader }
unit DataPortFile;

interface

uses SysUtils, Classes, DataPort
{$IFDEF UNIX}
  , BaseUnix
{$ENDIF}
{$ifndef FPC}
  , Windows
{$endif};

type
  TFileWriteMode = (fwmRewrite, fwmAppend);

  { TDataPortFile }

  TDataPortFile = class(TDataPort)
  private
    sReadData: ansistring;
    lock: TMultiReadExclusiveWriteSynchronizer;
    FFileHandle: THandle;
    FFileName: string;
    FFilePos: cardinal;
    FQueryInterval: cardinal;
    FMinDataBytes: cardinal;
    FKeepOpen: boolean;
    FWriteMode: TFileWriteMode;
    procedure IncomingMsgHandler(Sender: TObject; AMsg: string);
    procedure ErrorEventHandler(Sender: TObject; AMsg: string);
    procedure FReadToSelf();
  protected
    procedure FSetActive(Val: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    { Opens file with given name, "file:" prefix can be used }
    procedure Open(InitStr: string = ''); override;
    procedure Close(); override;
    function Push(sMsg: ansistring): boolean; override;
    function Pull(size: integer = MaxInt): ansistring; override;
    function Peek(size: integer = MaxInt): ansistring; override;
    function PeekSize(): cardinal; override;
    function ioctl_cmd(ACmd: string): string;
  published
    property Active;
    { Path (optionally) and name of file }
    property FileName: string read FFileName write FFileName;
    { Current position in file, bytes from beginning (for conventional files) }
    property FilePos: cardinal read FFilePos write FFilePos;
    { Interval for checking changes in file, milliseconds }
    property QueryInterval: cardinal read FQueryInterval write FQueryInterval;
    { Minimum number of bytes in buffer for triggering OnDataAppear event }
    property MinDataBytes: cardinal read FMinDataBytes write FMinDataBytes;
    { Keep the file open between read and write operations:
      True - file stay opened
      False - file will be opened before every read/write operation and closed after. }
    property KeepOpen: boolean read FKeepOpen write FKeepOpen;
    { WriteMode - File write mode:
      fwmRewrite - every write apply to beginning of file
      fwmAppend - data written from last operation position or appended to the end of file }
    property WriteMode: TFileWriteMode read FWriteMode write FWriteMode;
    property OnDataAppear;
    property OnError;
    property OnOpen;
    property OnClose;
  end;

procedure Register;


implementation

{$ifndef FPC}
const
  feInvalidHandle = INVALID_HANDLE_VALUE;
  fsFromBeginning = 0;
  fsFromEnd = 2;

procedure FileTruncate(AFileHandle: cardinal; ASize: cardinal);
begin
  FileSeek(AFileHandle, ASize, fsFromBeginning);
  SetEndOfFile(AFileHandle);
end;

{$endif}

procedure Register;
begin
  RegisterComponents('DataPort', [TDataPortFile]);
end;


{ TDataPortFile }

constructor TDataPortFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.lock := TMultiReadExclusiveWriteSynchronizer.Create();
  sReadData := '';
  FFilePos := 0;
  FQueryInterval := 100;
  FMinDataBytes := 1;
  FFileHandle := feInvalidHandle;
  FKeepOpen := False;
  FWriteMode := fwmAppend;
  FActive := False;
end;

procedure TDataPortFile.Open(InitStr: string = '');
var
  n: integer;
begin
  // Set filename from init string
  if InitStr <> '' then
  begin
    n := Pos(':', InitStr);
    if n > 0 then
    begin
      Self.FFileName := Copy(InitStr, n + 1, MaxInt);
    end
    else
      Self.FFileName := InitStr;
  end;

  if FFileName = '' then
    Exit;
  if not FileExists(FFileName) then
  begin
    // file not exists - test file create and write
    try
      FFileHandle := FileCreate(FFileName);
    except
      FFileHandle := feInvalidHandle;
    end;
    if FFileHandle = feInvalidHandle then
      Exit;
    {$ifdef CHECK_FILE_WRITE}
    // try to write first char of filename into file
    try
      FileWrite(FFileHandle, FFileName[1], 1);
      FileTruncate(FFileHandle, 0);
      if not FKeepOpen then
      begin
        FileClose(FFileHandle);
        FFileHandle := feInvalidHandle;
      end;
    except
      FileClose(FFileHandle);
      FFileHandle := feInvalidHandle;
      Exit;
    end;
    {$endif}
  end
  else
  begin
    if KeepOpen then
    begin
      try
        FFileHandle := FileOpen(FFileName, fmOpenReadWrite);
      except
        FFileHandle := feInvalidHandle;
      end;
      if FFileHandle = feInvalidHandle then
        Exit;
      if WriteMode = fwmAppend then
        FileSeek(FFileHandle, 0, fsFromEnd);
    end;
  end;
  inherited Open(InitStr);
end;

procedure TDataPortFile.Close();
begin
  if KeepOpen or (FFileHandle <> feInvalidHandle) then
  begin
    FileClose(FFileHandle);
    FFileHandle := feInvalidHandle;
  end;
  inherited Close();
end;

destructor TDataPortFile.Destroy();
begin
  FreeAndNil(self.lock);
  inherited Destroy();
end;

procedure TDataPortFile.IncomingMsgHandler(Sender: TObject; AMsg: string);
begin
  if AMsg <> '' then
  begin
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

procedure TDataPortFile.ErrorEventHandler(Sender: TObject; AMsg: string);
begin
  if Assigned(Self.FOnError) then
    Self.FOnError(Self, AMsg);
end;

procedure TDataPortFile.FReadToSelf();
var
  buf: array [0..1023] of byte;
  s: string;
  res: integer;
begin
  if not KeepOpen then
  begin
    // open file
    if FFileName = '' then
      Exit;
    try
      FFileHandle := FileOpen(FFileName, fmOpenReadWrite or fmShareDenyNone);
      if WriteMode = fwmAppend then
        FileSeek(FFileHandle, FilePos, fsFromBeginning);
    except
      on E: Exception do
      begin
        if Assigned(FOnError) then
          FOnError(Self, E.Message);
        Exit;
      end;
    end;
  end;

  // read data to buf
  try
    res := FileRead(FFileHandle, buf, SizeOf(buf));
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError(Self, E.Message);
      Exit;
    end;
  end;

  // if write-mode Rewrite then truncate readed data
  if WriteMode = fwmRewrite then
    FileTruncate(FFileHandle, 0)
  else if WriteMode = fwmAppend then
    FilePos := FilePos + res;


  // read data from buf to result
  {$ifdef FPC}
  SetString(s, @buf, res);
  {$else}
  SetString(s, PChar(@buf), res);
  {$endif}
  sReadData := sReadData + s;

  if not KeepOpen then
  begin
    // close file
    FileClose(FFileHandle);
    FFileHandle := feInvalidHandle;
  end;
end;

function TDataPortFile.Peek(size: integer = MaxInt): ansistring;
begin
  lock.BeginRead();
  FReadToSelf();
  Result := Copy(sReadData, 1, size);
  lock.EndRead();
end;

function TDataPortFile.PeekSize(): cardinal;
begin
  lock.BeginRead();
  FReadToSelf();
  Result := cardinal(Length(sReadData));
  lock.EndRead();
end;

function TDataPortFile.ioctl_cmd(ACmd: string): string;
var
  iArg, iRes: integer;
begin
{
* Per POSIX guidelines, this module reserves the LP and lp prefixes
* These are the lp_table[minor].flags flags...
#define LP_EXIST 0x0001
#define LP_SELEC 0x0002
#define LP_BUSY  0x0004
#define LP_BUSY_BIT_POS 2
#define LP_OFFL  0x0008
#define LP_NOPA  0x0010
#define LP_ERR   0x0020
#define LP_ABORT 0x0040
#define LP_CAREFUL 0x0080 /* obsoleted -arca */
#define LP_ABORTOPEN 0x0100

 * bit defines for 8255 status port
 * base + 1
 * accessed with LP_S(minor), which gets the byte...
#define LP_PBUSY        0x80  /* inverted input, active high */
#define LP_PACK         0x40  /* unchanged input, active low */
#define LP_POUTPA       0x20  /* unchanged input, active high */
#define LP_PSELECD      0x10  /* unchanged input, active high */
#define LP_PERRORP      0x08  /* unchanged input, active low */

#define LPGETSTATUS 0x060b  /* return LP_S(minor) */
#define LPRESET     0x060c  /* reset printer */

}
  Result := '';

  if not KeepOpen then
  begin
    // open file
    if FFileName = '' then
      Exit;
    try
      FFileHandle := FileOpen(FFileName, fmOpenReadWrite or fmShareDenyNone);
    except
      on E: Exception do
      begin
        if Assigned(FOnError) then
          FOnError(Self, E.Message);
        Exit;
      end;
    end;
  end;

  if ACmd = 'LPGETSTATUS' then
  begin
    {$IFDEF UNIX}
    iRes := FpIOCtl(FFileHandle, $060b, @iArg);
    Result := IntToHex(iArg, 4) + ' ';
    if (iArg and $80) > 0 then
      Result := Result + 'busy ';  // busy input
    if (iArg and $40) = 0 then
      Result := Result + 'ack ';   // acknowleged input
    if (iArg and $20) > 0 then
      Result := Result + 'outpa '; // out-of-paper
    if (iArg and $10) > 0 then
      Result := Result + 'selectd '; // selected input
    if (iArg and $08) = 0 then
      Result := Result + 'errorp ';  // error input
    {$ENDIF}
  end

  else if ACmd = 'LPRESET' then
  begin
    {$IFDEF UNIX}
    iRes := FpIOCtl(FFileHandle, $060c, @iArg);
    {$ENDIF}
  end;

  if not KeepOpen then
  begin
    // close file
    FileClose(FFileHandle);
    FFileHandle := feInvalidHandle;
  end;
end;

function TDataPortFile.Pull(size: integer = MaxInt): ansistring;
begin
  Result := '';
  if not lock.BeginWrite() then
    Exit;
  FReadToSelf();
  Result := Copy(sReadData, 1, size);
  Delete(sReadData, 1, size);
  //sReadData:='';
  lock.EndWrite();
end;

function TDataPortFile.Push(sMsg: ansistring): boolean;
begin
  Result := False;
  if Length(sMsg) = 0 then
    Exit;
  if not lock.BeginWrite() then
    Exit;

  if KeepOpen then
  begin
    try
      FileWrite(FFileHandle, sMsg[1], Length(sMsg));
      Result := True;
    except
      on E: Exception do
      begin
        lock.EndWrite();
        if Assigned(FOnError) then
          FOnError(Self, E.Message);
        Exit;
      end;
    end;
  end
  else
  begin
    if FFileName = '' then
      Exit;
    try
      //FFileHandle:=FileOpen(FFileName, fmOpenReadWrite or fmShareCompat);
      FFileHandle := FileOpen(FFileName, fmOpenReadWrite or fmShareDenyWrite);
      if WriteMode = fwmAppend then
        FileSeek(FFileHandle, 0, fsFromEnd);
      FileWrite(FFileHandle, sMsg[1], Length(sMsg));
      FileClose(FFileHandle);
      Result := True;
    except
      on E: Exception do
      begin
        lock.EndWrite();
        if Assigned(FOnError) then
          FOnError(Self, E.Message);
        Exit;
      end;
    end;
    FFileHandle := feInvalidHandle;
  end;
  lock.EndWrite();
end;

procedure TDataPortFile.FSetActive(Val: boolean);
begin
  inherited FSetActive(Val);
  //if FActive then Open();
  //else if Assigned(self.IpClient) then FreeAndNil(self.IpClient);
end;

end.

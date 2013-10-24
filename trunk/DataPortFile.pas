unit DataPortFile;
{ TODO : Add thread-safe file reader }
interface
uses SysUtils, Classes, DataPort
{$IFDEF UNIX}
, BaseUnix
{$ENDIF}
;

type
  TFileWriteMode = (fwmRewrite, fwmAppend);

  { TDataPortFile }

  TDataPortFile = class(TDataPort)
  private
    sReadData: AnsiString;
    lock: TMultiReadExclusiveWriteSynchronizer;
    FFileHandle: THandle;
    FFileName: string;
    FFilePos: Cardinal;
    FQueryInterval: Cardinal;
    FMinDataBytes: Cardinal;
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
    procedure Open(InitStr: string = ''); override;
    procedure Close(); override;
    function Push(sMsg: AnsiString): Boolean; override;
    function Pull(size: Integer = MaxInt): AnsiString; override;
    function Peek(size: Integer = MaxInt): AnsiString; override;
    function PeekSize(): Cardinal; override;
    function ioctl_cmd(ACmd: string): string;
  published
    property Active;
    property FileName: string read FFileName write FFileName;
    property FilePos: Cardinal read FFilePos write FFilePos;
    property QueryInterval: Cardinal read FQueryInterval write FQueryInterval;
    property MinDataBytes: Cardinal read FMinDataBytes write FMinDataBytes;
    property KeepOpen: boolean read FKeepOpen write FKeepOpen;
    property WriteMode: TFileWriteMode read FWriteMode write FWriteMode;
    property OnDataAppear;
    property OnError;
    property OnOpen;
    property OnClose;
  end;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('DataPort', [TDataPortFile]);
end;


{ TDataPortFile }

constructor TDataPortFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.lock:=TMultiReadExclusiveWriteSynchronizer.Create();
  sReadData:='';
  FFilePos:=0;
  FQueryInterval:=100;
  FMinDataBytes:=1;
  FFileHandle:=0;
  FKeepOpen:=False;
  FWriteMode:=fwmAppend;
  FActive:=False;
  //Self.IpClient:=nil;
end;

procedure TDataPortFile.Open(InitStr: string = '');
var
  n: Integer;
  s: AnsiString;
begin
  // Set host and port from init string
  if InitStr<>'' then
  begin
    n:=Pos(':', InitStr);
    if n>0 then
    begin
      Self.FFileName:=Copy(InitStr, n+1, MaxInt);
    end
    else
      Self.FFileName:=InitStr;
  end;

  if not FileExists(FFileName) then
  begin
    s:='a';
    FFileHandle:=FileCreate(FFileName);
    FileWrite(FFileHandle, s[1], Length(s));
    FileTruncate(FFileHandle, 0);
    if not FKeepOpen then
    begin
      FileClose(FFileHandle);
      FFileHandle:=0;
    end;
  end
  else
  begin
    if KeepOpen then
    begin
      FFileHandle:=FileOpen(FFileName, fmOpenReadWrite);
      if WriteMode=fwmAppend then FileSeek(FFileHandle, 0, fsFromEnd);
    end;
  end;
  inherited Open(InitStr);
end;

procedure TDataPortFile.Close();
begin
  if KeepOpen or (FFileHandle<>0) then
  begin
    FileClose(FFileHandle);
    FFileHandle:=0;
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
  if AMsg<>'' then
  begin
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

procedure TDataPortFile.ErrorEventHandler(Sender: TObject; AMsg: string);
begin
  if Assigned(Self.FOnError) then Self.FOnError(Self, AMsg);
end;

procedure TDataPortFile.FReadToSelf();
var
  buf: array [0..1023] of byte;
  s: string;
  res: Integer;
begin
  if not KeepOpen then
  begin
    // open file
    if FFileName='' then Exit;
    try
      FFileHandle:=FileOpen(FFileName, fmOpenReadWrite or fmShareDenyNone);
      if WriteMode=fwmAppend then FileSeek(FFileHandle, FilePos, fsFromBeginning);
    except on E:Exception do
      begin
        if Assigned(FOnError) then FOnError(Self, E.Message);
        Exit;
      end;
    end;
  end;

  // read data to buf
  try
    res:=FileRead(FFileHandle, buf, SizeOf(buf));
  except on E:Exception do
    begin
      if Assigned(FOnError) then FOnError(Self, E.Message);
      Exit;
    end;
  end;

  // if write-mode Rewrite then truncate readed data
  if WriteMode=fwmRewrite then FileTruncate(FFileHandle, 0)
  else if WriteMode=fwmAppend then FilePos:=FilePos+res;


  // read data from buf to result
  SetString(s, @buf, res);
  sReadData:=sReadData+s;

  if not KeepOpen then
  begin
    // close file
    FileClose(FFileHandle);
    FFileHandle:=0;
  end;
end;

function TDataPortFile.Peek(size: Integer = MaxInt): AnsiString;
begin
  lock.BeginRead();
  FReadToSelf();
  Result:=Copy(sReadData, 1, size);
  lock.EndRead();
end;

function TDataPortFile.PeekSize(): Cardinal;
begin
  lock.BeginRead();
  FReadToSelf();
  // Length of all strings
  Result:=Cardinal(Length(sReadData));
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
  Result:='';

  if not KeepOpen then
  begin
    // open file
    if FFileName='' then Exit;
    try
      FFileHandle:=FileOpen(FFileName, fmOpenReadWrite or fmShareDenyNone);
    except on E:Exception do
      begin
        if Assigned(FOnError) then FOnError(Self, E.Message);
        Exit;
      end;
    end;
  end;

  if ACmd='LPGETSTATUS' then
  begin
    {$IFDEF UNIX}
    iRes:=FpIOCtl(FFileHandle, $060b, @iArg);
    Result:=IntToHex(iArg, 4)+' ';
    if (iArg and $80)>0 then Result:=Result+'busy ';  // busy input
    if (iArg and $40)=0 then Result:=Result+'ack ';   // acknowleged input
    if (iArg and $20)>0 then Result:=Result+'outpa '; // out-of-paper
    if (iArg and $10)>0 then Result:=Result+'selectd '; // selected input
    if (iArg and $08)=0 then Result:=Result+'errorp ';  // error input
    {$ENDIF}
  end

  else if ACmd='LPRESET' then
  begin
    {$IFDEF UNIX}
    iRes:=FpIOCtl(FFileHandle, $060c, @iArg);
    {$ENDIF}
  end;

  if not KeepOpen then
  begin
    // close file
    FileClose(FFileHandle);
    FFileHandle:=0;
  end;
end;

function TDataPortFile.Pull(size: Integer = MaxInt): AnsiString;
begin
  Result:='';
  if not lock.BeginWrite() then Exit;
  FReadToSelf();
  Result:=Copy(sReadData, 1, size);
  Delete(sReadData, 1, size);
  //sReadData:='';
  lock.EndWrite();
end;

function TDataPortFile.Push(sMsg: AnsiString): Boolean;
begin
  Result:=False;
  if Length(sMsg)=0 then Exit;
  if not lock.BeginWrite() then Exit;

  if KeepOpen then
  begin
    try
      FileWrite(FFileHandle, sMsg[1], Length(sMsg));
    except on E:Exception do
      begin
        if Assigned(FOnError) then FOnError(Self, E.Message);
        Exit;
      end;
    end;
  end
  else
  begin
    if FFileName='' then Exit;
    try
      //FFileHandle:=FileOpen(FFileName, fmOpenReadWrite or fmShareCompat);
      FFileHandle:=FileOpen(FFileName, fmOpenReadWrite or fmShareDenyWrite);
      if WriteMode=fwmAppend then FileSeek(FFileHandle, 0, fsFromEnd);
      FileWrite(FFileHandle, sMsg[1], Length(sMsg));
      FileClose(FFileHandle);
    except on E:Exception do
      begin
        if Assigned(FOnError) then FOnError(Self, E.Message);
        Exit;
      end;
    end;
    FFileHandle:=0;
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

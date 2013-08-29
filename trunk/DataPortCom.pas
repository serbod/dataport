unit DataPortCom;

interface
uses SysUtils, Classes, DataPort, CPort_411;

type
  TDataPortCom = class(TDataPort)
  private
    sReadData: AnsiString;
    lock: TMultiReadExclusiveWriteSynchronizer;
    FPort: string;
    FBaudRate: TBaudRate;
    FMinDataBytes: Integer;
    procedure OnRxCharHandler(Sender: TObject; Count: Integer);
    procedure OnComErrorHandler(Sender: TObject; Errors: TComErrors);
    procedure OnComExceptionHandler(Sender:TObject;
                          TComException:TComExceptions; ComportMessage:String;
                          WinError:Int64; WinMessage:String);
  protected
    procedure FSetActive(Val: boolean); override;
  public
    ComPort: TComPort;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure Open(InitStr: string);
    function Push(sMsg: AnsiString): Boolean; override;
    function Pull(size: Integer = MaxInt): AnsiString; override;
    function Peek(size: Integer = MaxInt): AnsiString; override;
    function PeekSize(): Cardinal; override;
  published
    // COM port name, 'COM1, COM2', etc..
    property Port: string read FPort write FPort;
    // baud rate for COM port
    property BaudRate: TBaudRate read FBaudRate write FBaudRate;
    // Minimum data bytes in buffer to trigger OnDataAppear event
    property MinDataBytes: integer read FMinDataBytes write FMinDataBytes;
    property Active;
    property OnDataAppear;
    property OnError;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('DataPort', [TDataPortCom]);
end;


{ TDataPortCom }

constructor TDataPortCom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.lock:=TMultiReadExclusiveWriteSynchronizer.Create();
  Self.sReadData:='';
  self.FBaudRate:=br9600;
  Self.FMinDataBytes:=1;
  self.FActive:=False;
  Self.FPort:='COM1';
  self.ComPort:=TComPort.Create(nil);
end;

procedure TDataPortCom.Open(InitStr: string);
begin

  Self.ComPort.Port:=InitStr;
  Self.ComPort.BaudRate:=Self.FBaudRate;
  //Self.ComPort.Parity:=prNone;
  //Self.ComPort.StopBits:=sbOneStopBit;
  //Self.ComPort.DataBits:=dbEight;
  Self.ComPort.OnRxChar:=OnRxCharHandler;
  Self.ComPort.OnError:=OnComErrorHandler;
  Self.ComPort.OnException:=OnComExceptionHandler;

  try
    Self.ComPort.Open();
    Self.FActive:=True;
  except
    self.FActive:=False;
  end;

end;

destructor TDataPortCom.Destroy();
begin
  FreeAndNil(self.ComPort);
  FreeAndNil(self.lock);

  inherited Destroy();
end;

procedure TDataPortCom.OnRxCharHandler(Sender: TObject; Count: Integer);
var
  s: string;
begin
  if Count < MinDataBytes then Exit;
  if lock.BeginWrite() then
  begin
    try
      ComPort.ReadStr(s, Count);
      sReadData:=sReadData+s;
    finally
      lock.EndWrite();
    end;

    if Assigned(FOnDataAppear) then FOnDataAppear(self);
  end;

end;

function TDataPortCom.Peek(size: Integer = MaxInt): AnsiString;
begin
  Result:='';
  lock.BeginRead();
  Result:=Copy(sReadData, 1, size);
  lock.EndRead();
end;

function TDataPortCom.PeekSize(): Cardinal;
begin
  lock.BeginRead();
  Result:=Length(sReadData);
  lock.EndRead();
end;

function TDataPortCom.Pull(size: Integer = MaxInt): AnsiString;
begin
  Result:='';
  if not lock.BeginWrite() then Exit;
  Result:=Copy(sReadData, 1, size);
  Delete(sReadData, 1, size);
  //sReadData:='';
  lock.EndWrite();
end;

function TDataPortCom.Push(sMsg: AnsiString): Boolean;
begin
  Result:=False;
  if not lock.BeginWrite() then Exit;
  if Assigned(self.ComPort) then
  begin
    self.ComPort.WriteStr(sMsg);
    Result:=True;
  end;
  lock.EndWrite();
end;

procedure TDataPortCom.FSetActive(Val: boolean);
begin
  inherited FSetActive(Val);
  if FActive then Self.Open(FPort)
  else Self.ComPort.Close();
end;

procedure TDataPortCom.OnComErrorHandler(Sender: TObject;
  Errors: TComErrors);
begin
  //
end;

procedure TDataPortCom.OnComExceptionHandler(Sender: TObject;
  TComException: TComExceptions; ComportMessage: String; WinError: Int64;
  WinMessage: String);
begin
  if Assigned(Self.FOnError) then
  begin
    Self.FOnError(Self, 'Com: '+ComportMessage+'  Win: '+WinMessage);
  end;
end;

end.

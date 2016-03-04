{
Data exchange through named pipes

Sergey Bodrov, 2012-2016

Data exchange through named pipes. Pipe name is platform-specific. On Windows,
'\\.\pipe\' prefix added automaticaly.

Pipe must be already exists, created by Linux 'mkfifo' command or some other program.

Methods:
  * Open() - open pipe channel with specified name
}
unit DataPortPipes;

interface

uses SysUtils, Classes, DataPort, Pipes;

type
  { TPipesClient - pipes port reader/writer }
  TPipesClient = class(TThread)
  private
    FInputPipeStream: TInputPipeStream;
    FOutputPipeStream: TOutputPipeStream;
    s: ansistring;
    sLastError: string;
    FSafeMode: boolean;
    FInputHandle: THandle;
    FOutputHandle: THandle;
    FOnIncomingMsgEvent: TMsgEvent;
    FOnErrorEvent: TMsgEvent;
    FOnConnectEvent: TNotifyEvent;
    procedure SyncProc();
    procedure SyncProcOnConnect();
  protected
    procedure Execute(); override;
  public
    InitStr: string;
    CalledFromThread: boolean;
    sToSend: ansistring;
    property SafeMode: boolean read FSafeMode write FSafeMode;
    property InputHandle: THandle read FInputHandle write FInputHandle;
    property OutputHandle: THandle read FOutputHandle write FOutputHandle;
    property OnIncomingMsgEvent: TMsgEvent read FOnIncomingMsgEvent
      write FOnIncomingMsgEvent;
    property OnErrorEvent: TMsgEvent read FOnErrorEvent write FOnErrorEvent;
    property OnConnectEvent: TNotifyEvent read FOnConnectEvent write FOnConnectEvent;
    function SendString(s: ansistring): boolean;
    procedure SendStream(st: TStream);
  end;

  { TDataPortPipes - serial DataPort }
  TDataPortPipes = class(TDataPort)
  private
    //slReadData: TStringList; // for storing every incoming data packet separately
    sReadData: ansistring;
    lock: TMultiReadExclusiveWriteSynchronizer;
    FInitStr: string;
    FMinDataBytes: integer;
    FInputHandle: THandle;
    FOutputHandle: THandle;
    procedure IncomingMsgHandler(Sender: TObject; AMsg: string);
    procedure ErrorEventHandler(Sender: TObject; AMsg: string);
    procedure ConnectHandler(Sender: TObject);
  public
    PipesClient: TPipesClient;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    { Open pipe, InitStr = pipe name }
    procedure Open(InitStr: string = ''); override;
    procedure Close(); override;
    function Push(sMsg: ansistring): boolean; override;
    function Pull(size: integer = MaxInt): ansistring; override;
    function Peek(size: integer = MaxInt): ansistring; override;
    function PeekSize(): cardinal; override;
  published
    property InputHandle: THandle read FInputHandle write FInputHandle;
    property OutputHandle: THandle read FOutputHandle write FOutputHandle;
    { Minimum bytes in incoming buffer to trigger OnDataAppear }
    property MinDataBytes: integer read FMinDataBytes write FMinDataBytes;
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
  RegisterComponents('DataPort', [TDataPortPipes]);
end;

// === TPipesClient ===
procedure TPipesClient.SyncProc();
begin
  if CalledFromThread then
    Exit;
  //if s:='' then Exit;
  CalledFromThread := True;
  if s <> '' then
  begin
    if Assigned(self.FOnIncomingMsgEvent) then
      FOnIncomingMsgEvent(self, s);
    s := '';
  end;
  if sLastError <> '' then
  begin
    if Assigned(self.FOnErrorEvent) then
      FOnErrorEvent(self, sLastError);
    self.Terminate();
  end;
  CalledFromThread := False;
end;

procedure TPipesClient.SyncProcOnConnect();
begin
  if CalledFromThread then
    Exit;
  CalledFromThread := True;
  if Assigned(self.FOnConnectEvent) then
    self.FOnConnectEvent(self);
  CalledFromThread := False;
end;

procedure TPipesClient.Execute();
var
  buf: array[0..1023] of byte;
  n: integer;
  ss: ansistring;
begin
  sLastError := '';

  try
    FInputPipeStream := TInputPipeStream.Create(FInputHandle);
    FOutputPipeStream := TOutputPipeStream.Create(FOutputHandle);

    Synchronize(SyncProcOnConnect);

    while not Terminated do
    begin
      n := FInputPipeStream.Read(buf, Length(buf));
      while n > 0 do
      begin
        SetString(ss, PAnsiChar(@buf), n);
        s := s + ss;
        n := FInputPipeStream.Read(buf, Length(buf));
      end;
      sLastError := '';
      if (Length(s) > 0) or (Length(sLastError) > 0) then
        Synchronize(SyncProc);

      Sleep(1);

      if sToSend <> '' then
      begin
        try
          Self.FOutputPipeStream.WriteAnsiString(sToSend);
        except
          on E: Exception do
          begin
            sLastError := E.Message;
            Synchronize(SyncProc);
          end;
        end;
        sToSend := '';
      end;
    end;
  finally
    FreeAndNil(FOutputPipeStream);
    FreeAndNil(FInputPipeStream);
  end;
end;

function TPipesClient.SendString(s: ansistring): boolean;
begin
  Result := False;
  if not Assigned(Self.FOutputPipeStream) then
    Exit;
  if SafeMode then
    self.sToSend := s
  else
  begin
    try
      Self.FOutputPipeStream.WriteAnsiString(s);
    except
      on E: Exception do
      begin
        sLastError := E.Message;
        Synchronize(SyncProc);
        Exit;
      end;
    end;
  end;
  Result := True;
end;

procedure TPipesClient.SendStream(st: TStream);
begin
  if not Assigned(Self.FOutputPipeStream) then
    Exit;
  try
    Self.FOutputPipeStream.CopyFrom(st, st.Size);
  except
    on E: Exception do
    begin
      sLastError := E.Message;
      Synchronize(SyncProc);
    end;
  end;
end;


{ TDataPortPipes }

constructor TDataPortPipes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.lock := TMultiReadExclusiveWriteSynchronizer.Create();
  FMinDataBytes := 1;
  FActive := False;
  Self.sReadData := '';
  Self.PipesClient := nil;
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

procedure TDataPortPipes.Open(InitStr: string = '');
var
  s, ss: string;
begin
  ss := InitStr;
  if ss = '' then
    ss := FInitStr
  else
    FInitStr := ss;
  if Assigned(self.PipesClient) then
  begin
    FreeAndNil(self.PipesClient);
  end;
  Self.PipesClient := TPipesClient.Create(True);
  Self.PipesClient.OnIncomingMsgEvent := self.IncomingMsgHandler;
  Self.PipesClient.OnErrorEvent := self.ErrorEventHandler;
  Self.PipesClient.OnConnectEvent := self.ConnectHandler;
  Self.PipesClient.SafeMode := True;

  if ss <> '' then
  begin
    s := InitStr;
    {$IFDEF MSWINDOWS}
    if Pos('\\.\pipe\', FInitStr) = 0 then
      s := '\\.\pipe\' + FInitStr;
    {$ENDIF}
    PipesClient.InputHandle := FileOpen(s, fmOpenReadWrite or fmShareDenyNone);
    PipesClient.OutputHandle := PipesClient.InputHandle;
  end
  else
  begin
    PipesClient.InputHandle := InputHandle;
    PipesClient.OutputHandle := OutputHandle;
  end;

  Self.PipesClient.Suspended := False;

  // don't inherits Open() - OnOpen event will be after successfull connection
end;

procedure TDataPortPipes.Close();
begin
  if Assigned(self.PipesClient) then
  begin
    if self.PipesClient.CalledFromThread then
      self.PipesClient.Terminate()
    else
      FreeAndNil(self.PipesClient);
  end;
  inherited Close();
end;

destructor TDataPortPipes.Destroy();
begin
  if Assigned(self.PipesClient) then
  begin
    self.PipesClient.OnIncomingMsgEvent := nil;
    self.PipesClient.OnErrorEvent := nil;
    self.PipesClient.OnConnectEvent := nil;
    FreeAndNil(self.PipesClient);
  end;
  FreeAndNil(self.lock);
  inherited Destroy();
end;

procedure TDataPortPipes.IncomingMsgHandler(Sender: TObject; AMsg: string);
begin
  if AMsg <> '' then
  begin
    if lock.BeginWrite then
    begin
      sReadData := sReadData + AMsg;
      lock.EndWrite;

      if Assigned(FOnDataAppear) then
        FOnDataAppear(self);
    end;

  end;
end;

procedure TDataPortPipes.ErrorEventHandler(Sender: TObject; AMsg: string);
begin
  if Assigned(Self.FOnError) then
    Self.FOnError(Self, AMsg);
  self.FActive := False;
end;

function TDataPortPipes.Peek(size: integer = MaxInt): ansistring;
begin
  lock.BeginRead();
  Result := Copy(sReadData, 1, size);
  lock.EndRead();
end;

function TDataPortPipes.PeekSize(): cardinal;
begin
  lock.BeginRead();
  Result := cardinal(Length(sReadData));
  lock.EndRead();
end;

function TDataPortPipes.Pull(size: integer = MaxInt): ansistring;
begin
  Result := '';
  if not lock.BeginWrite() then
    Exit;
  Result := Copy(sReadData, 1, size);
  Delete(sReadData, 1, size);
  lock.EndWrite();
end;

function TDataPortPipes.Push(sMsg: ansistring): boolean;
begin
  Result := False;
  if not Assigned(self.PipesClient) then
    Exit;
  if lock.BeginWrite() then
  begin
    self.PipesClient.SendString(sMsg);
    lock.EndWrite();
  end;
end;

procedure TDataPortPipes.ConnectHandler(Sender: TObject);
begin
  self.FActive := True;
  if Assigned(OnOpen) then
    OnOpen(Self);
end;

end.

unit DataPortHTTP;
{$DEFINE non-thread}
interface
uses SysUtils, Classes, DataPort, httpsend, synautil, synacode;

type
  THttpMethods = (httpGet, httpPost);

  THttpClient = class(TThread)
  private
    HttpSend: THTTPSend;
    s: string;
    sLastError: string;
    FOnIncomingMsgEvent: TMsgEvent;
    FOnErrorEvent: TMsgEvent;
    procedure SyncProc();
  protected
    procedure Execute(); override;
  public
    url: string;
    method: THttpMethods;
    data: string;
    property OnIncomingMsgEvent: TMsgEvent read FOnIncomingMsgEvent write FOnIncomingMsgEvent;
    property OnErrorEvent: TMsgEvent read FOnErrorEvent write FOnErrorEvent;
    function SendString(s: string): Boolean;
    procedure SendStream(st: TStream; Dest: string);
  end;

  { TDataPortHTTP }

  TDataPortHTTP = class(TDataPort)
  private
    //slReadData: TStringList; // for storing every incoming data packet separately
    sReadData: AnsiString;
    lock: TMultiReadExclusiveWriteSynchronizer;
    HttpClient: THttpClient;
    HttpSend: THTTPSend;
    FUrl: string;
    FParams: TStrings;
    FMethod: THttpMethods;
    procedure IncomingMsgHandler(Sender: TObject; AMsg: string);
    procedure ErrorEventHandler(Sender: TObject; AMsg: string);
  protected
    procedure FSetParams(Val: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure Open(InitStr: string = ''); override;
    procedure Close(); override;
    function Push(sMsg: AnsiString): Boolean; override;
    function Pull(size: Integer = MaxInt): AnsiString; override;
    function Peek(size: Integer = MaxInt): AnsiString; override;
    function PeekSize(): Cardinal; override;
  published
    property Url: string read FUrl write FUrl;
    property Params: TStrings read FParams write FSetParams;
    property Method: THttpMethods read FMethod write FMethod;
    property Active;
    property OnDataAppear;
    property OnError;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('DataPort', [TDataPortHTTP]);
end;

// === THttpClient ===
procedure THttpClient.SyncProc();
begin
  //if s:='' then Exit;
  if s<>'' then
  begin
    if Assigned(self.FOnIncomingMsgEvent) then FOnIncomingMsgEvent(self, s);
    s:='';
  end;
  if sLastError<>'' then
  begin
    if Assigned(self.FOnErrorEvent) then FOnErrorEvent(self, sLastError);
    //self.Terminate();
  end;
end;

procedure THttpClient.Execute();
var
  bResult: Boolean;
  sMethod: string;
begin
  Self.HttpSend:=THTTPSend.Create();
  sLastError:='';
  sMethod:='GET';
  synautil.WriteStrToStream(Self.HttpSend.Document, self.data);
  if self.method=httpPost then
  begin
    sMethod:='POST';
    Self.HttpSend.MimeType:='application/x-www-form-urlencoded';
  end;

  try
    bResult:=self.HttpSend.HTTPMethod(sMethod, Self.url);
    if not bResult then
    begin
      sLastError:='Cannot connect';
      Synchronize(SyncProc);
    end;
    while not Terminated do
    begin
      s:=synautil.ReadStrFromStream(Self.HttpSend.Document, Self.HttpSend.Document.Size);
      if Self.HttpSend.Document.Size=0 then sLastError:='Zero content size';
      Synchronize(SyncProc);
      Sleep(10);
      Terminate();
    end;
    Self.HttpSend.Clear();
  finally
    FreeAndNil(Self.HttpSend);
  end;
end;

function THttpClient.SendString(s: string): Boolean;
begin
  Result:=False;
  if Assigned(Self.HttpSend) then Exit;
  self.data:=s;
  Self.Start();
  Result:=True;
end;

procedure THttpClient.SendStream(st: TStream; Dest: string);
begin
  if Assigned(Self.HttpSend) then Exit;
  self.data:=synautil.ReadStrFromStream(st, st.Size);
  Self.Start();
end;


{ TDataPortHTTP }

constructor TDataPortHTTP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.lock:=TMultiReadExclusiveWriteSynchronizer.Create();
  FParams:=TStringList.Create();
  FMethod:=httpGet;
  FActive:=False;
  //Self.slReadData:=TStringList.Create();
  Self.sReadData:='';
  Self.HttpClient:=nil;
end;

procedure TDataPortHTTP.Open(InitStr: string);
begin
  if Assigned(self.HttpClient) then FreeAndNil(self.HttpClient);
  if Length(InitStr)>0 then Url:=InitStr;
  if Length(Url)=0 then
  begin
    if Assigned(Self.FOnError) then Self.FOnError(Self, 'Empty URL');
    Exit;
  end;
  {$IFNDEF non-thread}
  Self.HttpClient:=THttpClient.Create(true);
  Self.HttpClient.OnIncomingMsgEvent:=self.IncomingMsgHandler;
  Self.HttpClient.OnErrorEvent:=Self.ErrorEventHandler;
  Self.HttpClient.url:=Url;
  Self.HttpClient.FreeOnTerminate:=True;
  Self.HttpClient.Start();
  {$ENDIF}
  inherited Open(InitStr);
end;

procedure TDataPortHTTP.Close();
begin
  {$IFNDEF non-thread}
  if Assigned(self.HttpClient) then
  begin
    //Self.HttpClient.OnIncomingMsgEvent:=nil;
    //Self.HttpClient.OnErrorEvent:=nil;
    self.HttpClient.Terminate();
    //FreeAndNil(self.HttpClient);
    self.HttpClient:=nil;
  end;
  {$ENDIF}
  inherited Close();
end;

destructor TDataPortHTTP.Destroy();
begin
  if Assigned(self.HttpClient) then FreeAndNil(self.HttpClient);
  //FreeAndNil(self.slReadData);
  FreeAndNil(FParams);
  FreeAndNil(self.lock);
  inherited Destroy();
end;

procedure TDataPortHTTP.IncomingMsgHandler(Sender: TObject; AMsg: string);
begin
  if AMsg<>'' then
  begin
    if lock.BeginWrite then
    begin
      //slReadData.Add(AMsg);
      sReadData:=sReadData+AMsg;
      lock.EndWrite;

      if Assigned(FOnDataAppear) then FOnDataAppear(self);
    end
    else if Assigned(FOnError) then FOnError(self, 'Lock failed');

  end;
end;

procedure TDataPortHTTP.ErrorEventHandler(Sender: TObject; AMsg: string);
begin
  if Assigned(Self.FOnError) then Self.FOnError(Self, AMsg);
  self.FActive:=False;
end;

{
function TDataPortIP.Peek(size: Integer = MaxInt): AnsiString;
var
  i, num, remain: Integer;
begin
  Result:='';
  remain:=size;
  lock.BeginRead();
  for i:=0 to slReadData.Count do
  begin
    num:=Length(slReadData[i]);
    if num>remain then num:=remain;
    Result:=Result+Copy(slReadData[i], 1, num);
    remain:=remain-num;
    if remain<=0 then Break;
  end;
  lock.EndRead();
end;
}

function TDataPortHTTP.Peek(size: Integer = MaxInt): AnsiString;
begin
  lock.BeginRead();
  Result:=Copy(sReadData, 1, size);
  lock.EndRead();
end;

function TDataPortHTTP.PeekSize(): Cardinal;
//var i: Integer;
begin
  //Result:=0;
  lock.BeginRead();
  //// Length of all strings
  //for i:=0 to slReadData.Count-1 do Result:=Result+Cardinal(Length(slReadData[i]));
  Result:=Cardinal(Length(sReadData));
  lock.EndRead();
end;

{
function TDataPortIP.Pull(size: Integer = MaxInt): AnsiString;
var
  num, len, remain: Integer;
begin
  Result:='';
  remain:=size;
  if not lock.BeginWrite() then Exit;
  while slReadData.Count>0 do
  begin
    // we read every string to exclude line delimiters
    len:=Length(slReadData[0]);
    num:=len;
    if num>remain then num:=remain;
    Result:=Result+Copy(slReadData[0], 1, num);
    remain:=remain-num;
    if num>=len then slReadData.Delete(0)
    else
    begin
      Delete(slReadData[0], 1, num);
      Break;
    end;
    if remain<=0 then Break;
  end;
  lock.EndWrite();
end;
}

function TDataPortHTTP.Pull(size: Integer = MaxInt): AnsiString;
begin
  Result:='';
  if not lock.BeginWrite() then Exit;
  Result:=Copy(sReadData, 1, size);
  Delete(sReadData, 1, size);
  //sReadData:='';
  lock.EndWrite();
end;

function TDataPortHTTP.Push(sMsg: AnsiString): Boolean;
var
  i: Integer;
  sUrl, sParams, sData: string;
  sMethod, sLastError: string;
  bResult: Boolean;
begin
  Result:=False;

  sUrl:=url;
  sData:=sMsg;
  sParams:='';
  // encode params into string
  for i:=0 to FParams.Count-1 do
  begin
    if sParams<>'' then sParams:=sParams+'&';
    sParams:=sParams+synacode.EncodeURL(FParams[i]);
  end;

  if method=httpGet then
  begin
    if FParams.Count>0 then
    begin
      sUrl:=sUrl+'?'+sParams;
    end;
  end
  else if method=httpPost then
  begin
    sData:=sParams+sMsg;
  end;

  {$IFDEF non-thread}
  Self.HttpSend:=THTTPSend.Create();
  sLastError:='';
  sMethod:='GET';
  synautil.WriteStrToStream(Self.HttpSend.Document, sData);
  if self.method=httpPost then
  begin
    sMethod:='POST';
    Self.HttpSend.MimeType:='application/x-www-form-urlencoded';
  end;

  try
    bResult:=self.HttpSend.HTTPMethod(sMethod, sUrl);
  except
    if Assigned(OnError) then OnError(self, 'Cannot connect');
  end;

  if not bResult then
  begin
    if Assigned(OnError) then OnError(self, 'Cannot connect');
  end

  else if Self.HttpSend.Document.Size=0 then
  begin
    if Assigned(OnError) then OnError(self, 'Zero content size');
  end

  else
  begin
    if lock.BeginWrite() then
    begin
      sReadData:=sReadData+synautil.ReadStrFromStream(Self.HttpSend.Document, Self.HttpSend.Document.Size);
      lock.EndWrite();
    end;
    if Assigned(OnDataAppear) then OnDataAppear(self);
  end;

  FreeAndNil(Self.HttpSend);
  {$ELSE}

  if not Assigned(self.HttpClient) then Exit;
  if not Active then Exit;
  if lock.BeginWrite() then
  begin
    HttpClient.url:=FUrl;
    HttpClient.method:=FMethod;
    sParams:='';
    for i:=0 to FParams.Count-1 do
    begin
      if sParams<>'' then sParams:=sParams+'&';
      sParams:=sParams+synacode.EncodeURL(FParams[i]);
    end;

    if method=httpGet then
    begin
      if FParams.Count>0 then
      begin
        HttpClient.url:=HttpClient.url+'?'+sParams;
        self.HttpClient.SendString(sMsg);
      end;
    end
    else if method=httpPost then
    begin
      HttpClient.SendString(sParams+sMsg);
    end;
    Result:=True;
    lock.EndWrite();
  end;
  {$ENDIF}
end;

procedure TDataPortHTTP.FSetParams(Val: TStrings);
begin
  FParams.Assign(Val);
end;

end.
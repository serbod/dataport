{
Allows you to communicate via HTTP. Specify URL and request parameters,
then call Push() to connect and transfer data to a remote server.
After successful execution of the request, data can be read from the input buffer.
Large amounts of data received by parts, and OnDataAppear event can be triggered multiple times.

If POST method selected, then request parameter mime-type='application/x-www-form-urlencoded' set,
it allow transfer parameters as web form values.

Sergey Bodrov, 2012-2025

Properties:
  Url - address and params string, URL
  Params - HTTP request params in name=value format
  Method - HTTP request method
    httpGet - GET
    httpPost - POST

Methods:
  Open() - sets URL string for HTTP request, but not send request itself. Request will be sent on Push(). URL string format:
    URL = 'http://RemoteHost:RemotePort/Path'
    RemoteHost - IP-address or name of remote host
    RemotePort - remote UPD or TCP port number
    Path - path to requested resource
}
unit DataPortHTTP;

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
    Data: string;
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
    FSafeMode: Boolean;
    procedure OnIncomingMsgHandler(Sender: TObject; const AMsg: AnsiString);
    procedure OnErrorHandler(Sender: TObject; const AMsg: AnsiString);
  protected
    procedure FSetParams(Val: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    { Open() - sets URL string for HTTP request, but not send request itself. Request will be sent on Push(). URL string format:
      URL = 'http://RemoteHost:RemotePort/Path'
      RemoteHost - IP-address or name of remote host
      RemotePort - remote UPD or TCP port number
      Path - path to requested resource }
    procedure Open(const AInitStr: string = ''); override;
    procedure Close(); override;
    function Push(const AData: AnsiString): Boolean; override;
    function Pull(size: Integer = MaxInt): AnsiString; override;
    function Peek(size: Integer = MaxInt): AnsiString; override;
    function PeekSize(): Cardinal; override;
  published
    { address and params string, URL }
    property Url: string read FUrl write FUrl;
    { HTTP request params in name=value format }
    property Params: TStrings read FParams write FSetParams;
    { Method - HTTP request method
      httpGet - GET
      httpPost - POST }
    property Method: THttpMethods read FMethod write FMethod;
    { Use this property if you encounter troubles:
      True - Non-threaded synchronous behavior
      False - Asynchronous behavior }
    property SafeMode: Boolean read FSafeMode write FSafeMode;
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
  RegisterComponents('DataPort', [TDataPortHTTP]);
end;

// === THttpClient ===
procedure THttpClient.SyncProc();
begin
  //if s:='' then Exit;
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
    //self.Terminate();
  end;
end;

procedure THttpClient.Execute();
var
  bResult: Boolean;
  sMethod: string;
begin
  Self.HttpSend := THTTPSend.Create();
  sLastError := '';
  sMethod := 'GET';
  synautil.WriteStrToStream(Self.HttpSend.Document, self.Data);
  if self.method = httpPost then
  begin
    sMethod := 'POST';
    Self.HttpSend.MimeType := 'application/x-www-form-urlencoded';
  end;

  try
    bResult := self.HttpSend.HTTPMethod(sMethod, Self.url);
  except
    bResult := False;
  end;

  if not bResult then
  begin
    sLastError := 'Cannot connect';
    Synchronize(SyncProc);
  end;

  if Self.HttpSend.Document.Size = 0 then
  begin
    sLastError := 'Zero content size';
    Synchronize(SyncProc);
  end;

  if self.HttpSend.DownloadSize <> Self.HttpSend.Document.Size then
  begin
    sLastError := 'Download size=' + IntToStr(self.HttpSend.DownloadSize) +
      '  doc size=' + IntToStr(Self.HttpSend.Document.Size);
    Synchronize(SyncProc);
  end;

  s := synautil.ReadStrFromStream(Self.HttpSend.Document, Self.HttpSend.Document.Size);
  Synchronize(SyncProc);
  Self.HttpSend.Clear();
  FreeAndNil(Self.HttpSend);
  Terminate();
end;

function THttpClient.SendString(s: string): Boolean;
begin
  Result := False;
  if Assigned(Self.HttpSend) then
    Exit;
  self.Data := s;
  self.Suspended := False;
  Result := True;
end;

procedure THttpClient.SendStream(st: TStream; Dest: string);
begin
  if Assigned(Self.HttpSend) then
    Exit;
  self.Data := synautil.ReadStrFromStream(st, st.Size);
  self.Suspended := False;
end;


{ TDataPortHTTP }

constructor TDataPortHTTP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.lock := TMultiReadExclusiveWriteSynchronizer.Create();
  FParams := TStringList.Create();
  FMethod := httpGet;
  FActive := False;
  FSafeMode := True;
  //Self.slReadData:=TStringList.Create();
  Self.sReadData := '';
  Self.HttpClient := nil;
end;

procedure TDataPortHTTP.Open(const AInitStr: string);
begin
  if Assigned(self.HttpClient) then
    FreeAndNil(self.HttpClient);
  if Length(AInitStr) > 0 then
    Url := AInitStr;
  if Length(Url) = 0 then
  begin
    if Assigned(Self.FOnError) then
      Self.FOnError(Self, 'Empty URL');
    Exit;
  end;
  if not self.SafeMode then
  begin
    // threaded request
    Self.HttpClient := THttpClient.Create(True);
    Self.HttpClient.OnIncomingMsgEvent := self.OnIncomingMsgHandler;
    Self.HttpClient.OnErrorEvent := Self.OnErrorHandler;
    Self.HttpClient.url := Url;
    Self.HttpClient.FreeOnTerminate := True;
    Self.HttpClient.Suspended := False;
  end;
  inherited Open(AInitStr);
end;

procedure TDataPortHTTP.Close();
begin
  if not self.SafeMode then
  begin
    if Assigned(self.HttpClient) then
    begin
      //Self.HttpClient.OnIncomingMsgEvent:=nil;
      //Self.HttpClient.OnErrorEvent:=nil;
      self.HttpClient.Terminate();
      //FreeAndNil(self.HttpClient);
      self.HttpClient := nil;
    end;
  end;
  inherited Close();
end;

destructor TDataPortHTTP.Destroy();
begin
  if Assigned(self.HttpClient) then
    FreeAndNil(self.HttpClient);
  //FreeAndNil(self.slReadData);
  FreeAndNil(FParams);
  FreeAndNil(self.lock);
  inherited Destroy();
end;

procedure TDataPortHTTP.OnIncomingMsgHandler(Sender: TObject; const AMsg: AnsiString);
begin
  if AMsg <> '' then
  begin
    if lock.BeginWrite then
    begin
      //slReadData.Add(AMsg);
      sReadData := sReadData + AMsg;
      lock.EndWrite;

      if Assigned(FOnDataAppear) then
        FOnDataAppear(self);
    end
    else if Assigned(FOnError) then
      FOnError(self, 'Lock failed');

  end;
end;

procedure TDataPortHTTP.OnErrorHandler(Sender: TObject; const AMsg: AnsiString);
begin
  if Assigned(Self.FOnError) then
    Self.FOnError(Self, AMsg);
  self.FActive := False;
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
  Result := Copy(sReadData, 1, size);
  lock.EndRead();
end;

function TDataPortHTTP.PeekSize(): Cardinal;
  //var i: Integer;
begin
  //Result:=0;
  lock.BeginRead();
  //// Length of all strings
  //for i:=0 to slReadData.Count-1 do Result:=Result+Cardinal(Length(slReadData[i]));
  Result := Cardinal(Length(sReadData));
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
  Result := '';
  if not lock.BeginWrite() then
    Exit;
  Result := Copy(sReadData, 1, size);
  Delete(sReadData, 1, size);
  //sReadData:='';
  lock.EndWrite();
end;

function TDataPortHTTP.Push(const AData: AnsiString): Boolean;
var
  i: Integer;
  sUrl, sParams, sData: string;
  sMethod: string;
  bResult: Boolean;
begin
  Result := False;

  sUrl := url;
  sData := AData;
  sParams := '';
  // encode params into string
  for i := 0 to FParams.Count - 1 do
  begin
    if sParams <> '' then
      sParams := sParams + '&';
    sParams := sParams + synacode.EncodeURL(FParams[i]);
  end;

  if method = httpGet then
  begin
    if FParams.Count > 0 then
    begin
      sUrl := sUrl + '?' + sParams;
    end;
  end
  else if method = httpPost then
  begin
    sData := sParams + AData;
  end;

  if self.SafeMode then
  begin
    // non-threaded
    Self.HttpSend := THTTPSend.Create();
    sMethod := 'GET';
    synautil.WriteStrToStream(Self.HttpSend.Document, sData);
    if self.method = httpPost then
    begin
      sMethod := 'POST';
      Self.HttpSend.MimeType := 'application/x-www-form-urlencoded';
    end;

    try
      bResult := self.HttpSend.HTTPMethod(sMethod, sUrl);
    except
      bResult := False;
    end;

    if not bResult then
    begin
      if Assigned(OnError) then
        OnError(self, 'Cannot connect');
    end

    else if Self.HttpSend.Document.Size = 0 then
    begin
      if Assigned(OnError) then
        OnError(self, 'Zero content size');
    end

    else
    begin
      if lock.BeginWrite() then
      begin
        try
          sReadData := sReadData + synautil.ReadStrFromStream(Self.HttpSend.Document,
            Self.HttpSend.Document.Size);
        finally
          lock.EndWrite();
        end;
      end;
      if Assigned(OnDataAppear) then
        OnDataAppear(self);
    end;

    FreeAndNil(Self.HttpSend);
  end
  else
  begin
    // threaded
    if not Assigned(self.HttpClient) then
      Exit;
    if not Active then
      Exit;
    if lock.BeginWrite() then
    begin
      try
        HttpClient.url := FUrl;
        HttpClient.method := FMethod;
        sParams := '';
        for i := 0 to FParams.Count - 1 do
        begin
          if sParams <> '' then
            sParams := sParams + '&';
          sParams := sParams + synacode.EncodeURL(FParams[i]);
        end;

        if method = httpGet then
        begin
          if FParams.Count > 0 then
          begin
            HttpClient.url := HttpClient.url + '?' + sParams;
            self.HttpClient.SendString(AData);
          end;
        end
        else if method = httpPost then
        begin
          HttpClient.SendString(sParams + AData);
        end;
        Result := True;

      finally
        lock.EndWrite();
      end;
    end;
  end;
end;

procedure TDataPortHTTP.FSetParams(Val: TStrings);
begin
  FParams.Assign(Val);
end;

end.

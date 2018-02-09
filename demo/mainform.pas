unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ActnList, Buttons, DataPortSerial, DataPort, DataPortIP,
  DataPortHTTP, DataPortFile, LCLType, ComCtrls;

type

  { TFormMain }

  TFormMain = class(TForm)
    actConnect: TAction;
    actClear: TAction;
    alMain: TActionList;
    btnFileConnect: TBitBtn;
    btnSerialConnect: TBitBtn;
    btnHTTPConnect: TBitBtn;
    btnUDPConnect: TBitBtn;
    btnTCPConnect: TBitBtn;
    cbSerialBitrate: TComboBox;
    cbSerialPort: TComboBox;
    chkLocalEcho: TCheckBox;
    dpFile: TDataPortFile;
    dpHTTP: TDataPortHTTP;
    dpUDP: TDataPortUDP;
    dpTCP: TDataPortTCP;
    dpSerial: TDataPortSerial;
    edFileName: TEdit;
    edTCPHost: TEdit;
    edHTTPHost: TEdit;
    edUDPHost: TEdit;
    edTCPPort: TEdit;
    edUDPPort: TEdit;
    lbFileName: TLabel;
    lbTCPHost: TLabel;
    lbSerialBitrate: TLabel;
    lbSerialPort: TLabel;
    lbHTTPHost: TLabel;
    lbUDPHost: TLabel;
    lbTCPPort: TLabel;
    lbUDPPort: TLabel;
    memoTerminal: TMemo;
    pgcMain: TPageControl;
    tsTCP: TTabSheet;
    tsUDP: TTabSheet;
    tsHTTP: TTabSheet;
    tsFile: TTabSheet;
    tsSerial: TTabSheet;
    procedure actClearExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure memoTerminalKeyPress(Sender: TObject; var Key: char);
    procedure pgcMainChange(Sender: TObject);
  private
    { private declarations }
    FDataPort: TDataPort;
    procedure SetDataPort(AValue: TDataPort);
    procedure UpdateSerialPortList();
    procedure AppendToTerminal(const s: string);
    procedure OnDataAppearHandler(Sender: TObject);
    procedure OnErrorHandler(Sender: TObject; const AMsg: string);
    procedure OnOpenHandler(Sender: TObject);
    procedure OnCloseHandler(Sender: TObject);
  public
    { public declarations }
    property DataPort: TDataPort read FDataPort write SetDataPort;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.memoTerminalKeyPress(Sender: TObject; var Key: char);
begin
  if Assigned(DataPort) then
  begin
    DataPort.Push(Key);
    if chkLocalEcho.Checked then
    begin
      AppendToTerminal(Key);
    end;
  end;
end;

procedure TFormMain.pgcMainChange(Sender: TObject);
begin
  if pgcMain.ActivePage = tsSerial then
  begin
    DataPort := dpSerial;
    UpdateSerialPortList();
  end
  else
  if pgcMain.ActivePage = tsTCP then
  begin
    DataPort := dpTCP;
  end
  else
  if pgcMain.ActivePage = tsUDP then
  begin
    DataPort := dpUDP;
  end
  else
  if pgcMain.ActivePage = tsHTTP then
  begin
    DataPort := dpHTTP;
  end
  else
  if pgcMain.ActivePage = tsFile then
  begin
    DataPort := dpFile;
  end;
end;

procedure TFormMain.UpdateSerialPortList();
var
  sl: TStringList;
begin
  cbSerialPort.Items.Clear();
  sl := TStringList.Create();
  try
    sl.CommaText := dpSerial.GetSerialPortNames();
    cbSerialPort.Items.AddStrings(sl);
  finally
    sl.Free();
  end;

  if cbSerialPort.Items.Count > 0 then
    cbSerialPort.ItemIndex := 0
  else
    cbSerialPort.Text := '';
end;

procedure TFormMain.SetDataPort(AValue: TDataPort);
begin
  if FDataPort = AValue then Exit;

  if Assigned(FDataPort) then
  begin
    FDataPort.Close();
    FDataPort.OnOpen := nil;
    FDataPort.OnClose := nil;
    FDataPort.OnDataAppear := nil;
    FDataPort.OnError := nil;
    FDataPort := nil;
  end;

  FDataPort := AValue;

  if Assigned(FDataPort) then
  begin
    FDataPort.OnOpen := @OnOpenHandler;
    FDataPort.OnClose := @OnCloseHandler;
    FDataPort.OnDataAppear := @OnDataAppearHandler;
    FDataPort.OnError := @OnErrorHandler;
  end;
end;

procedure TFormMain.AppendToTerminal(const s: string);
begin
  memoTerminal.Lines.BeginUpdate();
  memoTerminal.Text := memoTerminal.Text + s;
  if memoTerminal.Lines.Count > 1200 then
  begin
    while memoTerminal.Lines.Count > 1000 do
      memoTerminal.Lines.Delete(0);
  end;
  memoTerminal.SelStart := MaxInt;
  memoTerminal.Lines.EndUpdate();
  memoTerminal.ScrollBy(0, -100000);
end;

procedure TFormMain.OnDataAppearHandler(Sender: TObject);
var
  sData: AnsiString;
begin
  sData := DataPort.Pull();
  AppendToTerminal(sData);
end;

procedure TFormMain.OnErrorHandler(Sender: TObject; const AMsg: string);
begin
  actConnect.Caption := 'Connect';
  AppendToTerminal('Error: ' + AMsg + sLineBreak);
end;

procedure TFormMain.OnOpenHandler(Sender: TObject);
begin
  actConnect.Caption := 'Disconnect';
end;

procedure TFormMain.OnCloseHandler(Sender: TObject);
begin
  actConnect.Caption := 'Connect';
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  memoTerminal.Clear();

  // bitrates
  cbSerialBitrate.Items.Clear();
  cbSerialBitrate.Items.Append('300');
  cbSerialBitrate.Items.Append('1200');
  cbSerialBitrate.Items.Append('9600');
  cbSerialBitrate.Items.Append('19200');
  cbSerialBitrate.Items.Append('57600');
  cbSerialBitrate.Items.Append('115200');
  cbSerialBitrate.Items.Append('230400');
  cbSerialBitrate.Items.Append('923076');
  cbSerialBitrate.ItemIndex := 2;

  pgcMain.ActivePage := tsSerial;
  pgcMainChange(pgcMain);
end;

procedure TFormMain.actConnectExecute(Sender: TObject);
begin
  if not Assigned(DataPort) then Exit;
  if DataPort.Active then
  begin
    DataPort.Close();
  end
  else
  begin
    if pgcMain.ActivePage = tsSerial then
    begin
      dpSerial.Port := cbSerialPort.Text;
      dpSerial.BaudRate := StrToIntDef(cbSerialBitrate.Text, 9600);
    end
    else
    if pgcMain.ActivePage = tsTCP then
    begin
      dpTCP.RemoteHost := edTCPHost.Text;
      dpTCP.RemotePort := edTCPPort.Text;
    end
    else
    if pgcMain.ActivePage = tsUDP then
    begin
      dpUDP.RemoteHost := edUDPHost.Text;
      dpUDP.RemotePort := edUDPPort.Text;
    end
    else
    if pgcMain.ActivePage = tsHTTP then
    begin
      dpHTTP.Url := edHTTPHost.Text;
    end
    else
    if pgcMain.ActivePage = tsFile then
    begin
      dpFile.FileName := edFileName.Text;
    end;
    actConnect.Caption := 'Connecting..';
    DataPort.Open();

    if pgcMain.ActivePage = tsHTTP then
    begin
      // send HTTP request
      DataPort.Push('');
    end;
  end;
end;

procedure TFormMain.actClearExecute(Sender: TObject);
begin
  memoTerminal.Clear();
end;


end.


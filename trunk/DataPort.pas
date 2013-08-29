unit DataPort;
{ Sergey Bodrov (serbod@gmail.com) 2012-2013 }

interface
uses Classes;

type
  TMsgEvent = procedure(Sender: TObject; AMsg: string) of object;

  { DataPort - thread-safe abstract port for data exchange

  DataPort является абстрактным портом для чтения и записи данных.
  Сам по себе он ничего не делает, а только описывает базовые методы для записи и
  чтения данных, а также базовые события.

  Свойства:
  Active - активность, готовность порта к обмену данными.

  Методы:
  Push() - отправка данных в порт
  Pull() - взятие данных из входящего буфера порта
  Peek() - чтение данных из входящего буфера порта, данные при этом остаются в буфере
  PeekSize() - число байтов данных, ожидающих во входящем буфере порта

  События:
  OnDataAppear - срабатывает при появлении данных во входящем буфере порта
  OnOpen - срабатывает после успешного открытия порта
  OnClose - срабатывает после закрытия порта
  OnError - срабатывает при ошибке порта, содержит описание ошибки
  }

  { TDataPort }

  TDataPort = class(TComponent)
  protected
    FOnDataAppear: TNotifyEvent;
    FOnOpen: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnError: TMsgEvent;
    FActive: Boolean;
    procedure FSetActive(Val: boolean); virtual;
  public
    property Active: boolean read FActive write FSetActive;
    // Occurs when new data appears in incoming buffer
    property OnDataAppear: TNotifyEvent read FOnDataAppear write FOnDataAppear;
    // Occurs immediately after dataport has been sucsessfully opened
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    // Occurs after dataport has been closed
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    // Occurs when dataport operations fails, contain error description
    property OnError: TMsgEvent read FOnError write FOnError;
    { Open dataport with specified initialization string
      If InitStr not specified, used default or designed settings }
    procedure Open(InitStr: string = ''); virtual;
    // Close dataport
    procedure Close(); virtual;
    // Write data string to port
    function Push(sMsg: AnsiString): Boolean; virtual; abstract;
    // Read and remove <size> bytes from incoming buffer. By default, read all data.
    function Pull(size: Integer = MaxInt): AnsiString; virtual; abstract;
    // Read, but not remove <size> bytes from incoming buffer.
    function Peek(size: Integer = MaxInt): AnsiString; virtual; abstract;
    // Get number of bytes waiting in incoming buffer
    function PeekSize(): Cardinal; virtual; abstract;
  end;


implementation

{ TDataPort }

procedure TDataPort.FSetActive(Val: boolean);
begin
  if FActive=Val then Exit;
  if Val then Open() else Close();
end;

procedure TDataPort.Open(InitStr: string);
begin
  FActive:=True;
  if Assigned(OnOpen) then OnOpen(self);
end;

procedure TDataPort.Close();
begin
  FActive:=False;
  if Assigned(OnClose) then OnClose(self);
end;

end.

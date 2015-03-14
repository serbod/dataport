# Introduction #

DataPort - thread-safe abstract port for data exchange. You can open DataPort and push some data into - data will appear on other side. And if other side send some data, you will get notifyed and can pull data from port at any time.

All DataPort components are asynchronous and use separate thread for waiting and reading data from port. TDataPort can be used as property for your components, then just assign one of non-visual DataPorts in form designer.

# Features #

  * non-visual components for Lazarus/Delphi
  * network (TCP/UDP/HTTP)
  * serial port (COM-port)
  * regular file
  * device file (ioctl supported)

Coming soon:

  * listener (server)
  * pipes
  * paralel port (LPT)
  * DDE
  * IPC

# TDataPort #

TDataPort is abstract component for reading and writing data to some port. It don't do anything and needs to be used as property or parent class for new components.

Properties:

  * Active - is port ready for data exchange.

Methods:

  * Push() - Send data to port
  * Pull() - Get data from port. Data readed from incoming buffer, and removed after that. You can specify number of bytes for read. If incoming buffer have less bytes, than specified, then will be returned while buffer. By default, return whole buffer and clear it after.
  * Peek() - Read data from incoming buffer, but don't remove. You can specify number of bytes for read. If incoming buffer have less bytes, than specified, then will be returned while buffer. By default, return whole buffer.
  * PeekSize() - Returns number of bytes in incoming buffer of port.
  * Open() - Open data port for data exchange. You can specify port initialisation string
  * Close() - Shut down port communication

Events:

  * OnDataAppear - Triggered in data appear in incoming buffer of dataport.
  * OnError - Triggered on error, contain error description.

## TDataPortSerial ##

Serial communication port. In Windows it COM-port (real or virtual). In Linux it /dev/ttyS or /dev/ttyUSB. Also, Linux use file /var/lock/LCK..ttyS for port locking

Properties:

  * Port - port name (COM1, /dev/ttyS01)
  * BaudRate - data excange speed
  * MinDataBytes - minimal bytes count in buffer for triggering event OnDataAppear

Methods:

  * Open() - Opens port. As parameter it use port initialization string:
```
InitStr = 'Port,BaudRate,DataBits,Parity,StopBits,SoftFlow,HardFlow'

Port - COM port name (COM1, /dev/ttyS01)
BaudRate - connection speed (50..4000000 bits per second), default 9600
DataBits - default 8
Parity - (N - None, O - Odd, E - Even, M - Mark or S - Space) default N
StopBits - (1, 1.5, 2)
SoftFlow - Enable XON/XOFF handshake, default 1
HardFlow - Enable CTS/RTS handshake, default 0
```

Events:

  * OnConnect - Triggered after sucсessful connection.
  * OnDisconnect - Triggered after disconnection.

## TDataPortIP, TDataPortTCP, TDataPortUDP ##

Asynchronous wrapper around Synapse TBlockSocket.

When using UDP, remember, that it not session protocol, data delivery and correct order not guaranteed. To start receive tde data, you must send empty packet to remote side, it tell remote side return address.

Properties:

  * RemoteHost - IP-address or name of remote host
  * RemotePort - remote UPD or TCP port number

Methods:

  * Open() - Connect to remote port. Session establiched for TCP and just port initialised for UDP. Init string format:
```
InitStr = 'RemoteHost:RemotePort'

RemoteHost - IP-address or name of remote host
RemotePort - remote UPD or TCP port number
```

Events:

  * OnConnect - Triggered after UDP port init or TCP session establiched.

## TDataPortUdpFtdi ##

Purposed for communicate with FTDI serial port chip over UDP. It same as TDataPortSerial, but working over UDP.

## TDataPortHTTP ##

Allows you to communicate via HTTP. Specify URL and request parameters, then call Push() to connect and transfer data to a remote server. After successful execution of the request, data can be read from the input buffer. Large amounts of data received by parts, and OnDataAppear event can be triggered multiple times.

If POST method selected, then request parameter mime-type='application/x-www-form-urlencoded' set, it allow transfer parameters as web form values.

Properties:

  * Url: address and params string, URL
  * Params: HTTP request params in name=value format
  * Method: HTTP request method
    * httpGet - GET
    * httpPost - POST

Methods:

  * Open() - sets URL string for HTTP request, but not send request itself. Request will be sent on Push(). URL string format:
```
URL = 'http://RemoteHost:RemotePort/Path'

RemoteHost - IP-address or name of remote host
RemotePort - remote UPD or TCP port number
Path - path to requested resource
```

## Компонент TDataPortFile ##

Обмен данными через файл. Подходит для использования /dev/**в Unix или специальных файлов в Windows. Можно использовать и обычные файлы.**

Свойства:

  * Filename: имя файла
  * FilePos: текущее положение в файле, в байтах от начала файла (для обычных файлов)
  * QueryInterval: интервал опроса изменений в файле, мс
  * MinDataBytes: минимальное число байтов в буфере, для срабатывания события OnDataAppear
  * KeepOpen: нужно ли держать файл открытым между операциями чтения и записи
    * True - файл остается открытым
    * False - файл открывается перед каждой операцией чтения-записи и закрывается после завершения операции
  * WriteMode: режим записи в файл
    * fwmRewrite - запись каждый раз ведется в начало файла
    * fwmAppend - данные записываются с позиции последней операции или добавляются в конец файла

Методы:

  * Open() - Открывает файл с указанным именем. Можно использовать префикс «file:»
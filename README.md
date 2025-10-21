# dataport
DataPort - thread-safe abstract port for data exchange. You can open DataPort and push some data into - data will appear on other side. And if other side send some data, you will get notifyed and can pull data from port at any time.

DataPort use Ararat Synapse library ( http://sourceforge.net/p/synalist/code/HEAD/tree/trunk/ ), which must be installed first as separate package for Lasarus.

DataPort can be freely used and modified under MIT License.

Features:

  * non-visual components for Lazarus/Delphi
  * network (TCP/UDP/HTTP)
  * serial port (UART, COM-port, FTDI)
  * device file (ioctl supported) and conventional file
  * named pipes

Coming soon:

  * listener (server)
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

  * OnConnect - Triggered after successful connection.
  * OnDisconnect - Triggered after disconnection.

## TDataPortFtdi ##

Serial communication port based on FTD2XX library. 

Properties:

  * SerialNumber - device serial number
  * DeviceDescription - device description string
  * BaudRate - data excange speed (300, 1200, 9600, 115384, 230769, 923076)
  * MinDataBytes - minimal bytes count in buffer for triggering event OnDataAppear

Methods:

  * Open() - Opens port. As parameter it use port initialization string:
```
InitStr = '<DeviceDescription>:<SerialNumber>:<PortInitStr>'
<PortInitStr> = 'Port,BaudRate,DataBits,Parity,StopBits,SoftFlow,HardFlow'

Examples:
  'USB Serial:' - first device of 'USB Serial' type
  ':FT425622'   - device with s/n FT425622

If device specified by <DeviceDescription> and/or <SerialNumber>
then 'Port' parameter in <PortInitStr> is ignored 
```
  * GetFtdiDeviceList() - list of available devices in format:
```
<DeviceDescription>:<SerialNumber><LineFeed>
```

Events:

  * OnModemStatus - Triggered when modem status changes (CTS, DTR, RI, DCD)

## TDataPortIP, TDataPortTCP, TDataPortUDP ##

Asynchronous wrapper around Synapse TBlockSocket.

When using UDP, remember, that it not session protocol, data delivery and correct order not guaranteed. To start receive data, you must send empty packet to remote side, it tell remote side return address.

From version 1.0.3 multiple DataPortIP instances uses common socket reader with single thread. It allow open thousands IP connections without performance losses.

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

## TDataPortFile ##

Data exchange via file. Suitable for device files (/dev/* under Unix or special
files in Windows). Conventional data files can be used too.

Properties:

  * Filename - Path (optionally) and name of file.
  * FilePos - Current position in file, bytes from beginning (for conventional files).
  * QueryInterval - Interval for checking changes in file, milliseconds.
  * MinDataBytes - Minimum number of bytes in buffer for triggering OnDataAppear event.
  * KeepOpen - Keep the file open between read and write operations:
    * True - file stay opened
    * False - file will be opened before every read/write operation and closed after.
  * WriteMode - File write mode:
    * fwmRewrite - every write apply to beginning of file
    * fwmAppend - data written from last operation position or appended to the end of file

Methods:

  * Open() - Opens file with given name, "file:" prefix can be used.

## TDataPortPipes ##

Data exchange through named pipes. Pipe name is platform-specific. On Windows, '\\.\pipe\' prefix added automaticaly.

Pipe must be already exists, created by Linux 'mkfifo' command or some other program.

Methods:

  * Open() - open pipe channel with specified name
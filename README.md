# dataport
DataPort - thread-safe abstract port for data exchange. You can open DataPort and push some data into - data will appear on other side. And if other side send some data, you will get notifyed and can pull data from port at any time.
DataPort use Ararat Synapse library ( http://sourceforge.net/p/synalist/code/HEAD/tree/trunk/ ), which must be installed as separate package for Lasarus.
DataPort can be freely used and modified under MIT License.

Features:
  * non-visual components for Lazarus/Delphi
  * network (TCP/UDP/HTTP)
  * serial port (UART, COM-port)
  * device file (ioctl supported) and conventional file
  * named pipes

Coming soon:
  * listener (server)
  * paralel port (LPT)
  * DDE
  * IPC

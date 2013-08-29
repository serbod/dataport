{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DataPortLasarus;

interface

uses
  DataPort, DataPortFTDI, DataPortHTTP, DataPortIP, DataPortSerial, 
  DataPortLasarus_reg, DataPortFile, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('DataPortFTDI', @DataPortFTDI.Register);
  RegisterUnit('DataPortHTTP', @DataPortHTTP.Register);
  RegisterUnit('DataPortIP', @DataPortIP.Register);
  RegisterUnit('DataPortSerial', @DataPortSerial.Register);
  RegisterUnit('DataPortFile', @DataPortFile.Register);
end;

initialization
  RegisterPackage('DataPortLasarus', @Register);
end.

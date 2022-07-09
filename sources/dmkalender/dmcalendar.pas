{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit DMCalendar;

interface

uses
  DMKalender, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('DMKalender', @DMKalender.Register);
end;

initialization
  RegisterPackage('DMCalendar', @Register);
end.

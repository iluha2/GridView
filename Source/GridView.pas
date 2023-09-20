{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GridView;

{$warn 5023 off : no warning about unused units}
interface

uses
  Ex_RegGrid, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Ex_RegGrid', @Ex_RegGrid.Register);
end;

initialization
  RegisterPackage('GridView', @Register);
end.

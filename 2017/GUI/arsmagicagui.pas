{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit arsmagicagui;

{$warn 5023 off : no warning about unused units}
interface

uses
  AMFormPanel, AMHelpAboutUnit, AMAboutFormUnit, AMMessages, 
  AMDataAppMainForm, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AMFormPanel', @AMFormPanel.Register);
  RegisterUnit('AMHelpAboutUnit', @AMHelpAboutUnit.Register);
end;

initialization
  RegisterPackage('arsmagicagui', @Register);
end.

{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ArsMagicaCLI;

{$warn 5023 off : no warning about unused units}
interface

uses
  AMAppUtils, AMCompSci, AMDatabase, AMDatabaseTest, AMDebug, AMDotIni, 
  AMFileUtils, AMObjectFactory, AMPersists, AMStrings, AMTextIO, AMDataUnit, 
  AMStateMachine, AMPlayAudioFile, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('ArsMagicaCLI', @Register);
end.

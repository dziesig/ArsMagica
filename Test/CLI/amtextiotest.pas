unit AMTextIOTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

  procedure DoTest;

implementation

uses
  AMDebug, AMAppUtils,
  AMTextIO;

var
  Input  : TTextIO;
  Output : TTextIO;

procedure TestCreateDestroy;
begin
  Output := TTextIO.Create( DefaultSaveLocation('','test') + 'theFile', True );
  Output.Free;
  Input := TTextIO.Create( DefaultSaveLocation('','test') + 'theFile', False  );
  Input.Free;
end; // TestCreateDestroy

procedure TestOutputToInput;
begin
  Input := TTextIO.Create( DefaultSaveLocation('','test') + 'theFile', False  );
  try
    try
      Input.Writeln('Hello World');
      raise Exception.Create('Output to Input did NOT raise Exception');
    except on E : Exception do
    end;
  finally
    Input.Free;
  end;
end; // TestOutputToInput

procedure TestWriteRead;
var
  C : Cardinal;
  I : Integer;
  B : Boolean;
  D : Double;
  S : String;
  Ce : Cardinal;
  Ie : Integer;
  Be : Boolean;
  De : Double;
  Se : String;
begin
  B := False;
  D := 0.0;
  I := 0;
  C := 0;
  Output := TTextIO.Create( DefaultSaveLocation('','test') + 'theFile', True );
  Ce :=  4000000000;
  Output.WriteLn( Ce );
  Ie := 32768;
  Output.WriteLn( Ie );
  De := 3.141592654;
  Output.WriteLn( De );
  Se := 'Now is the time for all good men to Party';
  Output.Writeln( Se );
  Be := True;
  Output.WriteLn( Be );
  Output.Free;

  Input := TTextIO.Create( DefaultSaveLocation('','test') + 'theFile', False  );

  Input.Readln( C );
  if C <> Ce then
    raise Exception.CreateFmt('Cardinal expected [%f], got [%f]',[Ce,C]);
  Input.ReadLn( I );
  if I <> Ie then
    raise Exception.CreateFmt('Integer expected [%d], got [%d]',[Ie,I]);
  Input.ReadLn( D );
  if D <> De then
    raise Exception.CreateFmt('Double expected [%f], got [%f]',[De,D]);
  Input.ReadLn( S );
  if S <> Se then
    raise Exception.CreateFmt('String expected [%s], got [%s]',[Se,S]);
  Input.ReadLn( B );
  if B <> Be then
    raise Exception.CreateFmt('Bool expected True, got False',[Be,B]);

  Input.Free;


end;

procedure DoTest;
begin

  TestCreateDestroy;
  Debug('TestCreateDestroy passed');
  TestOutputToInput;
  Debug('TestOutputToInput passed');
  TestWriteRead;
  Debug('TestWriteRead passed');

end;



end.


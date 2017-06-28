program AMCLITest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, AMTextIOTest, AMStringsTest, AMPersistsTest, AMDebug,
  AMDatabaseTest, CustApp, AMFSMTest, AM1dArrayTest, AM2dArrayTest, AMCompSci
  { you can add units after this };

type

  { TArsMagicaCLITest }

  TArsMagicaCLITest = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TArsMagicaCLITest }

procedure TArsMagicaCLITest.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  try
  //AMStringsTest.DoTest;
  //AMTextIOTest.DoTest;
  //AMPersistsTest.DoTest;
  //AM1dArrayTest.DoTest;
  //AM2dArrayTest.DoTest;
  AMDatabaseTest.DoTest;
  except
    on What : Exception do
      begin
        Debug('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
        Debug('%S',[what.Message]);
      end;
  end;
  // stop program loop
  Terminate;
end;

constructor TArsMagicaCLITest.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TArsMagicaCLITest.Destroy;
begin
  inherited Destroy;
end;

procedure TArsMagicaCLITest.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TArsMagicaCLITest;
begin
  Application:=TArsMagicaCLITest.Create(nil);
  Application.Title:='ArsMagicaCLITest';
  Application.Run;
  Application.Free;
end.


unit AMPersistsTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AMPersists, AMTextIO;

type

  { TTestObject }

  TTestObject = class( TAMPersists )
    private
      fName : String;
      const
        TheVersion = 1;
      procedure SetName(AValue: String);

    public
      constructor Create( aParent : TAMPersists = nil ); override;

      procedure Read( TextIO : TTextIO; aVersion : Integer ); override;
      procedure Write( TextIO : TTextIO ); override;

      procedure MakeNew;  override;

      property Name : String read fName write SetName;
  end;

procedure DoTest;

implementation

uses
  AMDebug, AMAppUtils, AMObjectFactory;

var
  TO1, TO2 : TTestObject;

procedure TestModified;
begin
  if TO1.Modified then
    raise Exception.Create( 'TO1 indicates modified.  It was NOT modified');
  if TO2.Modified then
    raise Exception.Create( 'TO2 indicates modified.  It was NOT modified');

  TO1.Name := 'I am TO1';
  if not TO1.Modified then
    raise Exception.Create( 'TO1 indicates not modified.  It WAS modified');
  if TO2.Modified then
    raise Exception.Create( 'TO2 indicates modified.  It was NOT modified');

  TO2.Name := 'I am TO2';
  if not TO1.Modified then
    raise Exception.Create( 'TO1 indicates not modified.  It WAS modified');
  if not TO2.Modified then
    raise Exception.Create( 'TO2 indicates not modified.  It WAS modified');

  TO1.UNMODIFY;
  TO2.UNMODIFY;

  if TO1.Modified then
    raise Exception.Create( 'TO1 indicates modified.  It was NOT modified');
  if TO2.Modified then
    raise Exception.Create( 'TO2 indicates modified.  It was NOT modified');

end; // TestModified

procedure TestName;
var
  E : String;
  S : String;
begin
  E := 'NO1';
  TO1.Name := E;
  S := TO1.Name;
  if S <> E then
    raise Exception.CreateFmt('TO1:  TestName got [%s], expected [%s]',[S,E]);

  E := 'Hello World';
  TO2.Name := E;
  S := TO2.Name;
  if S <> E then
    raise Exception.CreateFmt('TO2:  TestName got [%s], expected [%s]',[S,E]);

end; // TestName

procedure TestStoreLoad;
var
  Input  : TTextIO;
  Output : TTextIO;
  IN1    : TTestObject;
  IN2    : TTestObject;
begin
  Output := TTextIO.Create( DefaultSaveLocation('','test') + 'theObject', True );
  TO1.Store( Output );
  TO2.Store( Output );
  Output.Free;

  Input := TTextIO.Create( DefaultSaveLocation('','test') + 'theObject', False  );

  IN1 := TTestObject.Load( Input ) as TTestObject;
  IN2 := TTestObject.Load( Input ) as TTestObject;

  if IN1.Version <> TO1.Version then
    raise Exception.CreateFmt( 'Load IN1 Version expected [%d], got [%d]',[TO1.Version, IN1.Version]);
  if IN1.Id <> TO1.Id then
    raise Exception.CreateFmt( 'Load IN1 Id expected [%d], got [%d]',[TO1.Id, IN1.Id]);
  if IN1.Name <> TO1.Name then
    raise Exception.CreateFmt( 'Load IN1 Name expected [%s], got [%s]',[TO1.Name, IN1.Name]);


  if IN2.Version <> TO2.Version then
    raise Exception.CreateFmt( 'Load IN2 Version expected [%d], got [%d]',[TO2.Version, IN2.Version]);
  if IN2.Id <> TO2.Id then
    raise Exception.CreateFmt( 'Load IN2 Id expected [%d], got [%d]',[TO2.Id, IN2.Id]);
  if IN1.Name <> TO1.Name then
    raise Exception.CreateFmt( 'Load IN2 Name expected [%s], got [%s]',[TO2.Name, IN2.Name]);

  Input.Free;
  IN1.Free;
  IN2.Free;
end;

procedure DoTest;
begin
  TO1 := nil;
  TO2 := nil;
  try
    TO1 := TTestObject.Create( nil );
    TO1.Id := 22;

    TO2 := TTestObject.Create( nil );
    TO2.Id := 26;

    TestModified;
    Debug('TestModified passes');

    TestName;
    Debug('TestName passes');

    TestStoreLoad;
    Debug('TestStoreLoad passes');

  finally
    if Assigned( TO2 ) then
      TO2.Free;
    if Assigned( TO1 ) then
      TO1.Free;
  end;

end;

{ TTestObject }

constructor TTestObject.Create(aParent: TAMPersists);
begin
  inherited Create(aParent);
  fVersion := TheVersion;
end;

procedure TTestObject.MakeNew;
begin
  inherited MakeNew;
end;

procedure TTestObject.Read(TextIO: TTextIO; aVersion: Integer);
begin
  MakeNew;
  if aVersion >= 1 then
    TextIO.Readln( fName );
end;

procedure TTestObject.SetName(AValue: String);
begin
  Update( fName, AValue );
end;

procedure TTestObject.Write(TextIO: TTextIO);
begin
  TextIO.Writeln( fName );
end;

initialization
  ObjectFactory.RegisterClass( TTestObject );

end.


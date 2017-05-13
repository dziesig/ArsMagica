unit AMPersists;

{$mode objfpc}{$H+}
{$M+} // Enables RTTI

interface

uses
  Classes, SysUtils, AMTextIO;

type

{ TAMPersists }

{
  TAMPersists is the root of streamable objects, which may be created by
  TObjectFactory (primarily for loading from the stream).

  These objects write and read a version field which supports upgrading by
  allowing older objects to be read from the stream and created as the most
  recent object (see MakeNew).

  These objects also maintain a Modified status and provide the means of
  passing this status to their parent (if any).  This automates what used to
  be an error-prone (for Don Ziesig) approach to manually tracking modifications
  made by the user.
}

  TAMPersists = class
  private
    fOnModify : TNotifyEvent;
    fParent   : TAMPersists;
    fId       : Cardinal;

    function GetVersion: Cardinal;
    procedure SetOnModify(AValue: TNotifyEvent);

    procedure SaveHeader( TextIO : TTextIO );
    procedure SaveTrailer( TextIO : TTextIO );
  protected
    fModified : Boolean;
    fVersion  : Cardinal; // Version of child object .. should be set by
                          // constructor to be a value > 0
    function GetIndexName(Idx : Integer): String; virtual;
    function  IsModified : Boolean; virtual;
  public
    DebugString : String;
    constructor Create( aParent : TAMPersists = nil); virtual;
    destructor  Destroy; override;
    procedure   MakeNew; virtual; // Initialize new instances.  Particularly useful
                                  // for handling reading older versions of the object
                                  // which do not have the newer data elements.

    class function Load( TextIO : TTextIO; aParent : TAMPersists = nil ) : TAMPersists; // Calls read after
                                                         // creating object
    procedure Store( TextIO : TTextIO ); virtual; // Calls write after
                                                  // SaveHeader and
                                                  // before SaveTrailer;

    function Compare( ToItem : TAMPersists; Index : Integer ) : Integer; virtual;

    procedure Read( TextIO : TTextIO; aVersion : Integer ); virtual;
    procedure Write( TextIO : TTextIO ); virtual;

    function  ToString : String; override;

    procedure DumpToFile( var F : Text; Path : String = '' ); virtual;

    procedure Assign( Source : TAMPersists ); virtual;
    procedure AssignTo( Dest : TAMPersists ); virtual;

    // Consistent approach to setting the modified flag.

    procedure Update( var Data : Integer;  NewValue : Integer );  overload;
    procedure Update( var Data : Cardinal; NewValue : Cardinal ); overload;
    procedure Update( var Data : Double;   NewValue : Double );   overload;
    procedure Update( var Data : String;   NewValue : String );   overload;
    procedure Update( var Data : Boolean;  NewValue : Boolean );  overload;

    procedure Modify; virtual;
    procedure UNMODIFY; virtual; // LOOK HERE there are only a few places where
                                 // this is valid }

    class function IndexCount : Integer; virtual;
    class function IndexName( Idx : Integer ) : String; virtual;

    property Version    : Cardinal read GetVersion;

    property Modified   : Boolean read IsModified;
    property Parent     : TAMPersists read fParent write fParent;
    property Id         : Cardinal  read fId write fId;
    property OnModify   : TNotifyEvent read fOnModify write SetOnModify; // Needed to
                                                                       // monitor mods.
  end;

  EModifingID       = Exception;
  ENonExistentIndex = Exception;
  EEndOfClass       = Exception;
  EStartOfClass     = Exception;

implementation

uses
  AMDebug,
  AMObjectFactory;

{ TAMPersists }

procedure TAMPersists.Assign(Source: TAMPersists);
begin
  fModified := false;
  fParent := Source.Parent;
end;

procedure TAMPersists.AssignTo(Dest: TAMPersists);
begin
  Dest.fModified := False;
  Dest.Parent := Parent;
end;

function TAMPersists.Compare(ToItem: TAMPersists; Index: Integer): Integer;
begin
  Result := Id - ToItem.Id;
  if Index <> 0 then
    raise Exception.CreateFMT('TAMPersists.Compare with index %d (must be zero)',[Index]);
end;

constructor TAMPersists.Create(aParent: TAMPersists);
begin
  fParent := aParent;
  fOnModify := nil;
  if Assigned(fOnModify) then
    raise Exception.Create('fOnModify not nil after Create');
  MakeNew;
end;

destructor TAMPersists.Destroy;
begin
  inherited Destroy;
end;

procedure TAMPersists.DumpToFile(var F: Text; Path: String);
begin
  WriteLn(F, 'TPersists.DumpToFile called directly:  ',Path);
end;

function TAMPersists.GetIndexName(Idx : Integer): String;
begin
  Result := ''; // The primary index is un-named.
  if Idx <> 0 then
    raise Exception.CreateFMT( 'TAMPersists.GetIndexName( %d ), should be 0',
                               [Idx] );
end;

function TAMPersists.GetVersion: Cardinal;
begin
  if fVersion = 0 then
    raise Exception.Create('Getting unset Version');
  Result := fVersion;
end;

class function TAMPersists.IndexCount: Integer;
begin
  Result := 1;  // The ID index
end;

class function TAMPersists.IndexName(Idx: Integer): String;
begin
  if Idx = 0 then
    Result := 'Id';
end;

function TAMPersists.IsModified: Boolean;
begin
  Result := fModified;
end;

class function TAMPersists.Load(TextIO: TTextIO; aParent : TAMPersists ): TAMPersists;
  var
    ClsName  : String;
    S        : String;
    aVersion : Integer;
    anId     : Integer;

    procedure CheckEndClass(FileClass, ExpectedClass: String; TextIO : TTextIO);
    var
      Cls : String;
      Len : Integer;
      Line : Integer;
      H, T : String;
    begin
      Len := Length( FileClass );
      Cls := Copy( FileClass,3,Len-3);
      Line := TextIO.LineNo;
      H := Copy( FileClass,1,2);
      T := Copy( FileClass,Len,1);
      if (H <> '</') or (T <> '>') then
        raise EEndOfClass.CreateFmt( 'Invalid End of Class format [%s], expecting [%s] at line %d.',
                                     [FileClass,ExpectedClass,Line] );
      if Cls <> ExpectedClass then
        raise EEndOfClass.CreateFmt( 'End of Class mismatch.  [%s] found, [%s] expected at line %d.',
                                     [Cls, ExpectedClass, Line] );
    end;
    procedure CheckStartClass(var FileClass : String; TextIO : TTextIO);
    var
      Cls : String;
      Len : Integer;
    begin
      Len := Length( FileClass );
      Cls := Copy( FileClass,2,Len-2);
      if (Copy( FileClass,1,1) <> '<') or (Copy( FileClass,Len,1) <> '>') then
        raise EStartOfClass.CreateFmt( 'Invalid Start of Class format [%s] at line %d.',
                                       [FileClass, TextIO.LineNo] );
      FileClass := Cls;
    end;

  begin
    aVersion := 0;
    anId := 0;
    S := '';
    ClsName := self.ClassName;    // Get the expected class name
    TextIO.ReadLn(S);             // Read the start of class
    CheckStartClass(S, TextIO);   // Assert they are correct and of correct format
    ClsName := S;
    TextIO.Readln(aVersion);       // Read the Object's version
    Result := ObjectFactory.MakeObject( ClsName ) as TAMPersists;
    Result.Parent := aParent;
    Result.fVersion := aVersion;  // This sets the version of the newly created object.
    //Result.Read( TextIO, aVersion );
    //Result.Read( TextIO, anId );
    TextIO.ReadLn( anId );
    Result.Read( TextIO, aVersion );
    Result.fId := anId;
    TextIO.Readln(S);             // Read the end of class
    CheckEndClass(S,ClsName, TextIO);     // Assert end of class is correct and of correct format
    Result.UNMODIFY;              // make sure this was NOT modified by the load.
end;

procedure TAMPersists.MakeNew;
begin
  fModified := false;
end;

procedure TAMPersists.SetOnModify(AValue : TNotifyEvent);
begin
  fOnModify := AValue;
  Debug('TAMPersists.SetOnModify');
end;

procedure TAMPersists.Store(TextIO: TTextIO);
begin
  SaveHeader( TextIO  );
  Write( TextIO );
  SaveTrailer( TextIO );
end;

function TAMPersists.ToString: String;
begin
  Result := '';
end;


procedure TAMPersists.Modify;
begin
  if not Assigned(self) then
    exit;
  if (not fModified) and (fParent <> nil) then
    fParent.Modify;
  fModified := true;
  //if Assigned(fOnModify) then
  if fOnModify <> nil then
    begin
      Debug('TAMPersists.Modify');
      fOnModify( self as TObject );
    end;
end;

procedure TAMPersists.Read(TextIO: TTextIO; aVersion: Integer);
begin
  raise Exception.Create('TAMPersists.Read called directly');
end;

procedure TAMPersists.SaveHeader(TextIO: TTextIO);
var
  S : String;
begin
  S := self.ClassName;          // Get our class name
  TextIO.Writeln('<'+S+'>');    // Write the start of class
  TextIO.Writeln( Version );
  TextIO.WriteLn( fId );
end;

procedure TAMPersists.SaveTrailer(TextIO: TTextIO);
var
  S : String;
begin
  S := self.ClassName;          // Get our class name
  TextIO.Writeln('</'+S+'>');   // Write the end of class
  UNMODIFY;                     // if it were modified, it isn't any more.
end;

procedure TAMPersists.UNMODIFY;
begin
  fModified := false;  // The original version of TAMPersists UNMODIFIED the Parent
                       // but that doesn't make sense since other parts of the
                       // parent tree might still be modified.  It only makes
                       // sense to set UNMODIFIED from the root.
end;

procedure TAMPersists.Update(var Data: Boolean; NewValue: Boolean);
begin
  if Data <> NewValue then
    begin
      Data := NewValue;
      Modify;
    end;
end;

procedure TAMPersists.Update(var Data: Cardinal; NewValue: Cardinal);
begin
  if Data <> NewValue then
    begin
      Data := NewValue;
      Modify;
    end;
end;

procedure TAMPersists.Update(var Data: Double; NewValue: Double);
begin
  if Data <> NewValue then
    begin
      Data := NewValue;
      Modify;
    end;
end;

procedure TAMPersists.Update(var Data: Integer; NewValue: Integer);
begin
  if Data <> NewValue then
    begin
      Data := NewValue;
      Modify;
    end;
end;

procedure TAMPersists.Update(var Data: String; NewValue: String);
begin
  if Data <> NewValue then
    begin
      Data := NewValue;
      Modify;
    end;
end;

procedure TAMPersists.Write(TextIO: TTextIO);
begin
  raise Exception.Create('TAMPersists.Write called directly');
end;

end.


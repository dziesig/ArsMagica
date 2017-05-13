unit AMDataUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AMPersists, AMTextIO;

type
  //TModifiedChangedEvent = procedure( Sender : TObject; var Changed : Boolean ) of object;

  { TAMData }

  TAMData = class{( TAMPersists )}
  private
    function GetModified: Boolean;
  protected
    fModified : Boolean;
    fOnModifyEvent: TNotifyEvent;
    fOnUnModifyEvent : TNotifyEvent;
  public
      constructor Create( aParent : TAMPersists = nil); virtual; { override;  }
      destructor  Destroy; override;

      procedure  New; virtual;
      function   Open( FilePath : String ) : TAMData; virtual;
      function   Save( FilePath : String ) : Boolean; virtual;

      procedure  Read( TextIO : TTextIO; aVersion : Integer ); virtual;
      procedure  Write( TextIO : TTextIO );  virtual;

      procedure  DoSetModified( Value : Boolean );

      property   Modified : Boolean read GetModified;

// Events sent to the main program
      property   OnModifyEvent : TNotifyEvent read fOnModifyEvent write fOnModifyEvent;
      property   OnUnModifyEvent : TNotifyEvent read fOnUnModifyEvent write fOnUnModifyEvent;
  end;

implementation

uses
  AMDebug;

{ TAMData }

constructor TAMData.Create(aParent: TAMPersists);
begin
  {inherited Create(aParent); }
end;

destructor TAMData.Destroy;
begin
  fOnModifyEvent := nil;
  fOnUnModifyEvent := nil;
{  inherited Destroy;    }
end;

procedure TAMData.DoSetModified( Value : Boolean );
begin
  fModified := Value;
  //Debug('DoSetModified  %p',[Pointer(@fOnModifyEvent)]);
  if Value then
    begin
      if Assigned( @fOnModifyEvent ) then
        fOnModifyEvent( Self );
    end
  else
    begin
      if Assigned( @fOnUnModifyEvent ) then
        fOnUnModifyEvent( Self );
    end;
end;

function TAMData.GetModified: Boolean;
begin
  Result := fModified;
end;

procedure TAMData.New;
begin
  inherited;
  DoSetModified( False );
end;

function TAMData.Open(FilePath: String): TAMData;
var
  TextIO : TTextIO;
begin
  TextIO := TTextIO.Create( FilePath, False );
{  Result := TAMData(Load( TextIO ));  }
  TextIO.Free;
  DoSetModified( False );
end;

procedure TAMData.Read(TextIO: TTextIO; aVersion: Integer);
begin
{  inherited;  }
end;

function TAMData.Save(FilePath: String): Boolean;
var
  TextIO : TTextIO;
begin
  TextIO := TTextIO.Create( FilePath, True );
  Result := Assigned( TextIO );
  TextIO.WriteLn('ZZZ');
  TextIO.Free;
  DoSetModified( False );
end;

procedure TAMData.Write(TextIO: TTextIO);
begin
  inherited;
end;

end.


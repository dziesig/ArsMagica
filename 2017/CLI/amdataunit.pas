{ AMData.  A generic interface for persistent data.

  Copyright (C) 1995..2017 by Donald R. Ziesig donald@ziesig.org

  This code is derived from the various "MagicLibraryYYYY"s by the same author.
  It has been Refactored to separate non-gui and gui modules.

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}
unit AMDataUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AMPersists, AMTextIO;

type

  { TAMData }

  TAMData = class( TAMPersists )
  private
    function GetModified: Boolean;
  protected
    //fModified : Boolean;
    fOnModifyEvent: TNotifyEvent;
    fOnUnModifyEvent : TNotifyEvent;
  public
    constructor Create( aParent : TAMPersists = nil); virtual; { override;  }
    destructor  Destroy; override;

    procedure  New; virtual;
    function   Open( FilePath : String ) : TAMData; virtual;
    function   Save( FilePath : String ) : Boolean; virtual;

    procedure Assign( Source : TAMPersists ); override;
    procedure AssignTo( Dest : TAMPersists ); override;

    //procedure  Read( TextIO : TTextIO; aVersion : Integer ); override;
    //procedure  Write( TextIO : TTextIO );  override;

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
  fOnModifyEvent := nil;
  fOnUnModifyEvent := nil;
  {inherited Create(aParent); }
end;

procedure TAMData.Assign(Source : TAMPersists);
begin
  inherited Assign(Source);
end;

procedure TAMData.AssignTo(Dest : TAMPersists);
begin
  inherited AssignTo(Dest);
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
      if Assigned( fOnModifyEvent ) then
        fOnModifyEvent( Self );
    end
  else
    begin
      if Assigned( fOnUnModifyEvent ) then
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
//  TextIO := TTextIO.Create( FilePath, False );
////{  Result := TAMData(Load( TextIO ));  }
//  TextIO.Free;
//  DoSetModified( False );
end;

//procedure TAMData.Read(TextIO: TTextIO; aVersion: Integer);
//begin
//{  inherited;  }
//end;

function TAMData.Save(FilePath: String): Boolean;
var
  TextIO : TTextIO;
begin
  TextIO := TTextIO.Create( FilePath, True );
  Store( TextIO );
  //Result := Assigned( TextIO );
  //TextIO.WriteLn('ZZZ');
  TextIO.Free;
  DoSetModified( False );
end;

//procedure TAMData.Write(TextIO: TTextIO);
//begin
//  //inherited;
//end;

end.


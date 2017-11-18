{ Object Factory.  Creates objects of arbitrary type (must be registered first)
  programatically.  This was designed specifically to allow objects to be read
  from files, but has other uses as well.

  Copyright (C) 2004..2017 Donald R. Ziesig donald@ziesig.org

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
unit AMObjectFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  AMPersists, AMTextIO;

type

  { TAMPersistsMapping }

  TAMPersistsMapping = class
  private
    fPersistsClass : TAMPersistsClass;
    function GetName : String;
  public
    constructor CreateEx( aPersistsClass : TAMPersistsClass );

    property PersistsClass : TAMPersistsClass read fPersistsClass;
    property Name : String read GetName;
  end;

  { TAMObjectFactory }

  TAMObjectFactory = class( TStringList )
  public
    constructor Create;
    destructor  Destroy; override;

    procedure   RegisterClass( aClass : TAMPersistsClass );
    function    MakeObject( TextIO : TTextIO; aName : String ) : TAMPersists; overload;
    function    MakeObject( TextIO : TTextIO; aClass : TClass ) : TAMPersists; overload;
    function    MakeObject( aName : String ) : TAMPersists; overload;
    function    MakeObject( aClass : TClass ) : TAMPersists; overload;
  end;

var
  ObjectFactory : TAMObjectFactory;

implementation

uses
  AMDebug;

{ TAMPersistsMapping }

constructor TAMPersistsMapping.CreateEx(aPersistsClass : TAMPersistsClass);
begin
  Create; // don't call inherited Create!
  fPersistsClass := aPersistsClass;
end;

function TAMPersistsMapping.GetName : String;
begin
  Result := fPersistsClass.ClassName;
end;

{ TAMObjectFactory }

constructor TAMObjectFactory.Create;
begin
  inherited;
  Sorted := True;
  Duplicates := dupError;
  SortStyle  := sslAuto;
  OwnsObjects := True;
end;

destructor TAMObjectFactory.Destroy;
begin
  inherited Destroy;
end;

function TAMObjectFactory.MakeObject(aName : String) : TAMPersists;
var
  I : Integer;
  vPersistsClass : TAMPersistsClass;
begin
  I := IndexOf( aName );
  if I < 0 then
    raise Exception.Create(aName + ' not found in Object Factory');
  vPersistsClass := TAMPersistsMapping( Objects[I] ).PersistsClass;
  Result := vPersistsClass.Create;
  Debug('Object:  %s',[Result.ClassName]);
  //Debug('Object:  %s',[Result.ToString]);
end;

function TAMObjectFactory.MakeObject(aClass : TClass) : TAMPersists;
var
  I : Integer;
  vPersistsClass : TAMPersistsClass;
  N : String;
begin
  N := aClass.ClassName;
  I := IndexOf( aClass.ClassName );
  if I < 0 then
    raise Exception.Create(aClass.ClassName + ' not found in Object Factory');
  vPersistsClass := TAMPersistsMapping( Objects[I] ).PersistsClass;
  Result := vPersistsClass.Create;
  Debug('Object:  %s',[Result.ClassName]);
  //Debug('Object:  %s',[Result.ToString]);
end;

function TAMObjectFactory.MakeObject(TextIO : TTextIO; aName : String
  ) : TAMPersists;
var
  I : Integer;
  vPersistsClass : TAMPersistsClass;
begin
  I := IndexOf( aName );
  if I < 0 then
    raise Exception.Create(aName + ' not found in Object Factory');
  vPersistsClass := TAMPersistsMapping( Objects[I] ).PersistsClass;
  Result := vPersistsClass.Create( TextIO );
  Debug('Object:  %s',[Result.ClassName]);
  //Debug('Object:  %s',[Result.ToString]);
end;

function TAMObjectFactory.MakeObject(TextIO : TTextIO; aClass : TClass
  ) : TAMPersists;
var
  I : Integer;
  vPersistsClass : TAMPersistsClass;
  N : String;
begin
  N := aClass.ClassName;
  I := IndexOf( aClass.ClassName );
  if I < 0 then
    raise Exception.Create(aClass.ClassName + ' not found in Object Factory');
  vPersistsClass := TAMPersistsMapping( Objects[I] ).PersistsClass;
  Result := vPersistsClass.Create( TextIO );
  //Debug('Object:  %s',[Result.ToString]);
  Debug('Object:  %s',[Result.ClassName]);
end;

procedure TAMObjectFactory.RegisterClass(aClass : TAMPersistsClass);
var
  vPersistsMapping : TAMPersistsMapping;
begin
  vPersistsMapping := TAMPersistsMapping.CreateEx( aClass);
  AddObject(vPersistsMapping.Name, vPersistsMapping);  //S := aClass.ClassName;
end;


initialization
  //ObjectFactory := TObjectFactory.Create;
  ObjectFactory := TAMObjectFactory.Create;
finalization
  //ObjectFactory.Free;
  ObjectFactory.Free;
end.


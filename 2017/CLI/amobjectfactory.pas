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
  Classes, SysUtils;

type

  TMagicClassArray = array of TClass;

  { TObjectFactory }

  TObjectFactory = class
  private
    function GetCount: Integer;
  protected
    public
    fClassList : TMagicClassArray;
  public
    constructor Create;
    procedure   Clear;
    procedure   RegisterClass( aClass : TClass );
    function    MakeObject( aName : String ) : TObject;
    property    Count : Integer read GetCount;
    property    ClassList : TMagicClassArray read fClassList;
  end;

var
  ObjectFactory : TObjectFactory;

implementation
{ TObjectFactory }

function TObjectFactory.GetCount: Integer;
begin
  Result := Length(fClassList);
end;

constructor TObjectFactory.Create;
begin
  SetLength(fClassList,0);
end;

procedure TObjectFactory.Clear;
begin
  SetLength(fClassList,0);
end;

procedure TObjectFactory.RegisterClass(aClass: TClass);
//var
//  //I : Integer;
//  aClassName : String; // For Debug
begin
  //aClassName := aClass.ClassName;
  SetLength(fClassList,Length(fClassList) + 1);
  fClassList[Length(fClassList)-1] := aClass;
end;

function TObjectFactory.MakeObject(aName: String): TObject;
var
  I : Integer;
  aClassName : String; // For Debug
begin
  for I := 0 to pred(Length(fClassList)) do
    begin
      aClassName := fClassList[I].ClassName;
      if aClassName = aName then
        begin
          Result := fClassList[I].Create;
          exit;
        end;
    end;
  raise Exception.Create(aName + ' not found in Object Factory');
end;

initialization
  ObjectFactory := TObjectFactory.Create;
finalization
  ObjectFactory.Free;
end.


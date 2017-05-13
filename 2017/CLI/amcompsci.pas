{ <Computer Sciency Stuff>

  Copyright (C) 1995..2017 by Donald R. Ziesig donald@ziesig.org

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
unit AMCompSci;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{==============================================================================}
{ Inheritance tests                                                            }
{==============================================================================}

function IsAncestor( Obj, Anc : TClass ) : Boolean;

{==============================================================================}
{ TStack }
{==============================================================================}

type
  generic TStack<T> = class
  private
    SP : Integer;
    Stack : array of T;
  public
    constructor Create;  virtual;
    destructor  Destroy; override;

    procedure Push( Value : T ); virtual;
    function  Pop : T; virtual;
    procedure Reset;
  end;


implementation

{==============================================================================}
{ Inheritance tests                                                            }
{==============================================================================}

function IsAncestor( Obj, Anc : TClass ) : Boolean;
var
  ObjCR, AncCR : TClass;
begin
  ObjCR := Obj;
  AncCR := Anc;
  while ObjCR <> nil do
    if ObjCR = AncCR then
      begin
        Result := True;
        Exit;
      end
    else
      ObjCR := ObjCR.ClassParent;
end;

{==============================================================================}
{ TStack }
{==============================================================================}

constructor TStack.Create;
const
  InitialListSize = 4;
var
  I : Integer;
begin
  SP := -1;
  SetLength(Stack,InitialListSize);
  for I := 0 to pred(InitialListSize) do
    if Stack[i] <> 0 then
      raise EListError.Create('non 0 in creation of Stack');
end;

destructor TStack.Destroy;
begin
  SetLength(Stack,0);
  inherited Destroy;
end;

procedure TStack.Push(Value: T);
begin
  Inc(SP);
  if SP >= Length(Stack) then
    SetLength(Stack,Length(Stack)*2);
  Stack[SP] := Value;
end;

procedure TStack.Reset;
begin
  SP := -1;
end;

function TStack.Pop: T;
begin
  if SP < 0 then
    raise EListError.Create('Stack Underflow');
  Result := Stack[SP];
  Dec(SP);
end;

end.


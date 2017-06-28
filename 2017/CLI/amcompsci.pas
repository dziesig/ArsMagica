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
  Classes, SysUtils, Types;

{==============================================================================}
{ Dynamic Arrays (extended)                                                    }
{==============================================================================}

type

  { T1dArray }

  generic T1dArray< Elem > = class
    private
      Data : array of Elem;
      function GetItem(I : Integer): Elem;
      procedure SetItem(I : Integer; AValue: Elem);
    public
      constructor Create( Size : Integer );
      destructor  Destroy; override;

      // Returns the new length of the array;
      function  Add( Item : Elem; At : Integer ) : Integer;
      function  Append( Item : Elem ) : Integer;
      function  Del( At : Integer ) : Integer;

      function  Length : Integer;

      procedure Clear; // Sets array to length 0;

      property  Items[I : Integer] : Elem read GetItem write SetItem; default;
  end;

  { T2dArray }

  // T2dArray is ordered like a 2d Matrix where Aij => i increases downward,
  // j increases rightward.  E.G. Row-major storage

  generic T2dArray< Elem > = class
    private
      Data : array of array of Elem;
      Null : Elem;
      function GetItem(I, J : Integer): Elem;
      procedure SetItem(I, J : Integer; AValue: Elem);
    public
      // aNull is the value to be inserted when inserting a row or column.
      constructor Create( Rows, Cols : Integer; aNull : Elem );
      destructor  Destroy; override;

      // Returns the new length or width of the array;
      function InsertRow( At : Integer ) : Integer;
      function InsertCol( At : Integer ) : Integer;

      function DeleteRow( At : Integer ) : Integer;
      function DeleteCol( At : Integer ) : Integer;

      function  Width  : Integer;
      function  Height : Integer;

      procedure Clear; // Sets array to width 0, height 0;

      property  Items[I, J : Integer] : Elem read GetItem write SetItem; default;
  end;

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

uses
  Math;

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

{ T2dArray }

constructor T2dArray.Create(Rows, Cols: Integer; aNull: Elem);
begin
  SetLength( Data, Rows, Cols );
  Null := ANull;
end;

procedure T2dArray.Clear;
begin
  SetLength( Data, 0, 0 );
end;

function T2dArray.DeleteCol(At: Integer): Integer;
var
  H, W : Integer;
  I, J : Integer;
begin
  H := Height;
  W := Width - 1;
  for I := 0 to pred( H ) do
    for J := At to pred( W ) do
      Data[I,J] := Data[I,J+1];
  SetLength( Data, H, W );
  Result := W;
end;

function T2dArray.DeleteRow(At: Integer): Integer;
var
  H, W : Integer;
  I, J : Integer;
begin
  H := Height - 1;
  W := Width;
  for I := At to pred( H ) do
    for J := 0 to pred( W ) do
      Data[I,J] := Data[I+1,J];
  SetLength( Data, H, W );
  Result := H;

end;

destructor T2dArray.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function T2dArray.GetItem(I, J: Integer): Elem;
begin
  Result := Data[I,J];
end;

function T2dArray.Height: Integer;
begin
  Result := Length( Data );
end;

function T2dArray.InsertCol(At: Integer): Integer;
var
  H, W : Integer;
  I, J : Integer;
begin
  H := Height;
  W := Width + 1;
  if At > W then
    W := At + 1;
  //if H = 0 then
  //  H := 1;
  H := Max(H,1);
  SetLength( Data, H, W );

  for I := 0 to pred( H ) do
    for J := pred( W ) downto succ( At ) do
      Data[I,J] := Data[I,J-1];
  for I := 0 to pred( H ) do
    Data[I,At] := Null;
  Result := W;
end;

function T2dArray.InsertRow(At: Integer): Integer;
var
  H, W : Integer;
  I, J : Integer;
begin
  H := Height + 1;
  W := Width;
  if At > H then
    H := At + 1;
  W := Max(W,1);
  SetLength( Data, H, W );
  for I := pred(H) downto succ(At) do
    for J := 0 to pred( W ) do
      Data[I,J] := Data[I-1,J];
  for J := 0 to pred( W ) do
    Data[At,J] := Null;
end;

procedure T2dArray.SetItem(I, J: Integer; AValue: Elem);
begin
  Data[I, J] := AValue;
end;

function T2dArray.Width: Integer;
begin
  if Height > 0 then
    Result := Length( Data[0] )
  else
    Result := 0;
end;

{==============================================================================}
{ T1dArray }
{==============================================================================}

constructor T1dArray.Create(Size: Integer);
begin
  SetLength( Data, Size );
end;

destructor T1dArray.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function T1dArray.GetItem(I : Integer): Elem;
begin
  Result := Data[I];
end;

function T1dArray.Add(Item: Elem; At: Integer): Integer;
var
  I : Integer;
begin
  Result := Length + 1;
  SetLength( Data, Result );
  if At >= Result then
    raise Exception.CreateFmt( 'Adding at %d beyond Length %d of 1d Array',
                               [ At, Length ] );
  for I := pred( Result ) downto succ( At ) do
    Data[I] := Data[I-1];
  Data[At] := Item;   // 2017-05-22 was [I]
end;

function T1dArray.Append(Item: Elem): Integer;
begin
  Result := Length + 1;
  SetLength( Data, Result );
  Data[ Result-1] := Item;
end;

procedure T1dArray.Clear;
begin
  SetLength( Data, 0);
end;

function T1dArray.Del(At: Integer): Integer;
var
  I : Integer;
  L : Integer;
begin
  L := Length;
  for I := succ(At) to pred(L) do
    Data[I-1] := Data[I];
  Result := L - 1;
  SetLength( Data, Result );
end;

function T1dArray.Length: Integer;
begin
  Result := System.Length( Data );
end;

procedure T1dArray.SetItem(I : Integer; AValue: Elem);
begin
  Data[I] := AValue;
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


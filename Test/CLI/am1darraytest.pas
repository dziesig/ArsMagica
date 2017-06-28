unit AM1dArrayTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure DoTest;

implementation

uses
  AMDebug, AMCompSci;

type
  TArray1d = specialize T1dArray< Integer >;

procedure TestCreate;
var
  Arr : TArray1d;

  E : Integer;
  A : Integer;
  W : String;
begin
  try
    E := 5;
    W := 'Array Create';
    Arr := TArray1d.Create( E );
    A   := Arr.Length;
    if A <> E then
      raise Exception.CreateFmt( '%s failed.  Length %d, expected %d',
                                 [W,A,E] );
    E := 0;
    Arr.Clear;
    A := Arr.Length;
    W := 'Array Clear';
    if A <> E then
      raise Exception.CreateFmt( '%s failed.  Length %d, expected %d',
                                 [W,A,E] );

  finally
    Arr.Free;
  end;
end;

procedure TestAppend;
var
  Arr : TArray1d;

  E : Integer;
  A : Integer;
  W : String;
  I : Integer;
begin
  try
    Arr := TArray1d.Create( 0 );
    W := 'Array Append';
    for I := 1 to 5 do
      begin
        E := I;
        A := Arr.Append( I );
        if A <> E then
          raise Exception.CreateFmt( '%s failed.  Length %d, expected %d',
                                     [W,A,E] );
        Debug('Arr[%d]:  %d',[pred(I), Arr[pred(I)]]);
      end;
  finally
    Arr.Free;
  end;
end;

procedure TestInsert;
var
  Arr : TArray1d;

  E : Integer;
  A : Integer;
  W : String;
  I : Integer;
begin
  try
    Arr := TArray1d.Create( 5 );
    for I := 0 to pred( Arr.Length ) do
      Debug('Arr[%d]:  %d',[I,Arr[I]]);
    for I := 0 to 4 do
      Arr[I] := I;
    for I := 0 to pred( Arr.Length ) do
      Debug('Arr[%d]:  %d',[I,Arr[I]]);
    Arr.Add(99,0);
    for I := 0 to pred( Arr.Length ) do
      Debug('Arr[%d]:  %d',[I,Arr[I]]);
    W := 'Add 99 at 0';
    E := 99;
    A := Arr[0];
    if A <> E then
      raise Exception.CreateFmt( '%s failed.  Value %d, expected %d',
                                 [W,A,E] );

    Arr.Add(88,4);
    for I := 0 to pred( Arr.Length ) do
      Debug('Arr[%d]:  %d',[I,Arr[I]]);
    W := 'Add 88 at 4';
    E := 88;
    A := Arr[4];
    if A <> E then
      raise Exception.CreateFmt( '%s failed.  Value %d, expected %d',
                                 [W,A,E] );

  finally
    Arr.Free;
  end;
end;

procedure TestDelete;
var
  Arr : TArray1d;

  E : Integer;
  A : Integer;
  W : String;
  I : Integer;
begin
  try
    Arr := TArray1d.Create( 5 );
    for I := 0 to pred(Arr.Length) do
      Arr[I] := I;
    for I := 0 to pred( Arr.Length ) do
      Debug('Arr[%d]:  %d',[I,Arr[I]]);
    A := Arr.Del( 2 );
    E := 4;
    W := 'Length after Delete item at 2';
    for I := 0 to pred( Arr.Length ) do
      Debug('Arr[%d]:  %d',[I,Arr[I]]);
    if A <> E then
      raise Exception.CreateFmt( '%s failed.  Length %d, expected %d',
                                 [W,A,E] );
    A := Arr[2];
    E := 3;
    W := 'Value after Delete Item at 2';
    if A <> E then
      raise Exception.CreateFmt( '%s failed.  Value %d, expected %d',
                                 [W,A,E] );

  finally
    Arr.Free;
  end;
end;

procedure DoTest;
begin
  Debug('Testing AM1dArray');
  TestCreate;
  Debug('TestCreate passed');
  TestAppend;
  Debug('TestAppend passed');
  TestInsert;
  Debug('TestInsert passed');
  TestDelete;
  Debug('TestDelete passed');

  Debug('Testing AM1dArray done');
end;

end.


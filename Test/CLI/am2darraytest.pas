unit AM2dArrayTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure DoTest;


implementation

uses
  AMDebug, AMCompSci;

type
  TArray2d = specialize T2dArray< Integer >;

procedure TestCreateAndFree;
var
  Arr : TArray2d;
  I, J : Integer;
  E : Integer;
  A : Integer;
  W : String;
const
  Height = 4;
  Width  = 5;
begin
  Arr := TArray2d.Create( Height, Width, 0);
  //for I := 0 to 3 do
  //  for J := 0 to 4 do
  //    Debug('Arr[%d,%d]:  %d',[I,J,Arr[I,J]] );
  E := Height;
  A := Arr.Height;
  W := 'Arr.Height:  ';
  if E <> A then
    raise Exception.CreateFmt( '%s, %d, expected:  %d',
                               [W,A,E] );
  //Debug( 'Arr.Height:  %d',[Arr.Height]);
  E := Width;
  A := Arr.Width;
  W := 'Arr.Width:  ';
  if E <> A then
    raise Exception.CreateFmt( '%s, %d, expected:  %d',
                               [W,A,E] );
  //Debug( 'Arr.Width: %d',[Arr.Width]);
  Arr.Free;
  Debug('TestCreateAndFree passed.');
end;

procedure TestGetAndSet;
var
  Arr : TArray2d;
  I, J : Integer;
  E : Integer;
  A : Integer;
  W : String;
begin
  Arr := TArray2d.Create( 4, 5, 0);
  for I := 0 to 3 do
    for J := 0 to 4 do
        Arr[I,J] := I*10 + J;

  for I := 0 to 3 do
    for J := 0 to 4 do
      begin
        //Debug('Arr[%d,%d]:  %d',[I,J,Arr[I,J]] );
        E :=  I*10 + J;
        A := Arr[I,J];
        W := Format('Arr[%d,%d]',[I,J]);
        if E <> A then
          raise Exception.CreateFmt( '%s failed.  Vaiue %d, expected %d',
                                     [W,A,E] );
      end;
  Arr.Free;
  Debug('TestGetAndSet passed.');
end;

procedure TestInserts;
const
  Height = 4;
  Width  = 5;
  At     = 2;
var
  Arr : TArray2d;
  I, J : Integer;
  E : Integer;
  A : Integer;
  W : String;
begin
  Debug('Test Inserts');
  try
    Arr := TArray2d.Create( Height, Width, 99 );
    for I := 0 to 3 do
      for J := 0 to 4 do
          Arr[I,J] := I*10 + J;
    Arr.InsertCol( At );
    Debug('Inserted Column at 2');
    for I := 0 to pred(Arr.Height) do
      for J := 0 to pred(Arr.Width) do
        Debug('Arr[%d,%d]:  %d',[I,J,Arr[I,J]] );
    E := Width  + 1;
    A := Arr.Width;
    W := 'InsertCol Arr.Width:  ';
    if E <> A then
      raise Exception.CreateFmt( '%s %d, expected:  %d',
                                 [W,A,E] );
    for I := 0 to pred(Arr.Height) do
      begin
        E := 99;
        A := Arr[I,At];
        W := Format('InsertCol Data[%d]:',[At]);
        if A <> E then
          raise Exception.CreateFmt( '%s %d, expected %d',[ W,A,E] );
      end;

  finally
    Arr.Free;
  end;
  try
    Arr := TArray2d.Create( Height, Width, 99 );
    for I := 0 to 3 do
      for J := 0 to 4 do
          Arr[I,J] := I*10 + J;
    Arr.InsertRow( At );
    for I := 0 to pred(Arr.Height) do
      for J := 0 to pred(Arr.Width) do
        Debug('Arr[%d,%d]:  %d',[I,J,Arr[I,J]] );
    E := 99;
    for J := 0 to pred(Arr.Width) do
      begin
        A := Arr[At,J];
        W := Format( 'Arr[%d,%d]: ',[At,J]);
        if A <> E then
          raise Exception.CreateFmt( '%s  %d, expected %d',[W,A,E]);
      end;

  finally
    Arr.Free;
  end;
  Debug('Test Inserts passed');
end;

procedure TestDeletes;
const
  Height = 4;
  Width  = 5;
  At     = 2;
var
  Arr : TArray2d;
  I, J : Integer;
  E : Integer;
  A : Integer;
  W : String;
  NewSize : Integer;
begin
  Debug('Test Deletes');
  try
    Arr := TArray2d.Create( Height, Width, 99 );
    for I := 0 to 3 do
      for J := 0 to 4 do
          Arr[I,J] := I*10 + J;
    NewSize := Arr.DeleteCol( At );
    A := NewSize;
    E := Width - 1;
    W := 'DeleteCol width:';
    if A <> E then
      raise Exception.CreateFmt('%s  %d, expected %d',[W,A,E]);
    for I := 0 to pred(Arr.Height) do
      for J := 0 to pred(Arr.Width) do
        Debug('Arr[%d,%d]:  %d',[I,J,Arr[I,J]] );
    E := Width  - 1;
    A := Arr.Width;
    W := 'DeleteCol Arr.Width:  ';
    if E <> A then
      raise Exception.CreateFmt( '%s %d, expected:  %d',
                                 [W,A,E] );
    //for I := 0 to pred(Arr.Height) do
    //  begin
    //    E := 99;
    //    A := Arr[I,At];
    //    W := Format('InsertCol Data[%d]:',[At]);
    //    if A <> E then
    //      raise Exception.CreateFmt( '%s %d, expected %d',[ W,A,E] );
    //  end;
    //
  finally
    Arr.Free;
  end;
  try
    Debug('Delete Row');
    Arr := TArray2d.Create( Height, Width, 99 );
    for I := 0 to 3 do
      for J := 0 to 4 do
          Arr[I,J] := I*10 + J;
    NewSize := Arr.DeleteRow( At );
    for I := 0 to pred(Arr.Height) do
      for J := 0 to pred(Arr.Width) do
        Debug('Arr[%d,%d]:  %d',[I,J,Arr[I,J]] );
    //E := 99;
    //for J := 0 to pred(Arr.Width) do
    //  begin
    //    A := Arr[At,J];
    //    W := Format( 'Arr[%d,%d]: ',[At,J]);
    //    if A <> E then
    //      raise Exception.CreateFmt( '%s  %d, expected %d',[W,A,E]);
    //  end;
  finally
    Arr.Free;
  end;
  Debug('Test Deletes passed');
end;

procedure DoTest;
begin
  Debug('Testing AM2dArray');
  TestCreateAndFree;
  TestGetAndSet;
  TestInserts;
  TestDeletes;
  Debug('Test AM2dArray done');
end;

end.


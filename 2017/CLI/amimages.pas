unit AMImages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AMImageProcessing;

type

  TAMBoolImage = specialize TAMImage< Boolean >;

  { AMNumImage }

  { TAMNumImage }

  generic  TAMNumImage< T > = class( specialize TAMImage<T> )

    public
      procedure Convo( A, B, C : T; Vert : Boolean = False );

      procedure MinMaxMean( out aMin, aMax, Mean : T );

      // Ans is cleared and resized first
      procedure Thresh( Ans : TAMBoolImage; Value : T );

  end;

  //TAMDoubleImage = specialize TAMImage< Double >;

  TAMDoubleImage = class( specialize TAMNumImage<Double> )
    procedure Random;  // Initializes image to random numbers;
  end;

implementation

uses
  Math;

{ TAMDoubleImage }

procedure TAMDoubleImage.Random;
var
  I, J : Integer;
begin
  for I := 0 to pred(Width) do
    for J := 0 to pred(Height) do
      Pixel[I,J] := System.Random;
end;

{ AMNumImage }

procedure TAMNumImage.Convo(A, B, C : T; Vert : Boolean);
var
  X, Y, K : Integer;
  Temp    : TAMNumImage;
begin
  Temp := TAMNumImage.Create( Width, Height );
  for Y := 0 to pred( Height ) do
    for X := 0 to pred( Width ) do
      Temp[X,Y] := Pixel[X,Y];

  Self.SetConst( 0.0 );
  if Vert then
    begin
      for X := 0 to pred( Width ) do
        for Y := 1 to pred( Height-1 ) do
          Pixel[X,Y] := Temp[X,Y-1]*A + Temp[X,Y]*B + Temp[X,Y+1]*C;
    end
  else
    begin
      for Y := 0 to pred( Height ) do
        begin
          for X := 1 to pred( Width-1 ) do
            Pixel[X,Y] := Temp[X-1,Y]*A + Temp[X,Y]*B + Temp[X+1,Y]*C;
          //Pixel[0,Y] := Temp[Width-1,Y]*A + Temp[0,Y]*B + Temp[1,Y]*C;
          Pixel[Width-1,Y] := Temp[0,Y] + Temp[Width-2,Y] + Temp[Width-1,Y];
        end;
    end;
  Temp.Free;
end;

procedure TAMNumImage.MinMaxMean(out aMin, aMax, Mean : T);
var
  I, J : Integer;
begin
  Mean := 0;
  aMin := Pixel[0,0];
  aMax := aMin;
  for I := 0 to pred( Height ) do
    for J := 0 to pred( Width ) do
      begin
        if Pixel[I,J] < aMin then aMin := Pixel[I,J];
        if Pixel[I,J] > aMax then aMax := Pixel[I,J];
        Mean := Mean + Pixel[I,J];
      end;
  Mean := Mean / (Height*Width);
end;

procedure TAMNumImage.Thresh(Ans : TAMBoolImage; Value : T);
var
  X, Y : Integer;
begin
  Ans.Resize( Width, Height );
  for Y := 0 to pred( Height ) do
    for X := 0 to pred( Width ) do
      Ans[X,Y] := Pixel[X,Y] > Value;
end;

end.


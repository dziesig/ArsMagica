unit AMImageProcessing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AMPersists, AMTextIO;

type

  { TAMImage }

  generic TAMImage<T> = class( TAMPersists )
    type
      TAMImageRow = array of T;
      TAMImageCol = array of TAMImageRow;

  private
    fHeight : Integer;
    fWidth : Integer;
    fImage : TAMImageCol;
    function GetPixel(X, Y : Integer) : T;
    procedure SetPixel(X, Y : Integer; AValue : T);
    public
      constructor Create( theWidth, theHeight : Integer ); overload;
      constructor Create( theWidth, theHeight : Integer; theDefault : T ); overload;
      constructor Create( anImage : TAMImage ); overload;
      destructor  Destroy; override;

      procedure Read( TextIO : TTextIO; aVersion : Integer ); override;
      procedure Write( TextIO : TTextIO ); override;

      procedure SetConst( Value : T );

      procedure Assign( anImage : TAMImage );

      procedure Resize( theWidth, theHeight : Integer ); // Destroys prior data

      property Pixel[X, Y : Integer] : T read GetPixel write SetPixel; default;
      property Width : Integer read fWidth;
      property Height : Integer read fHeight;
  end;


implementation

{ TAMImage }


constructor TAMImage.Create(theWidth, theHeight : Integer);
var
  I : Integer;
begin
  fVersion := 1;
  fWidth := theWidth;
  fHeight := theHeight;
  SetLength( fImage, fHeight );
  for I := 0 to pred( fHeight ) do
    SetLength( fImage[I], fWidth );
end;

constructor TAMImage.Create(theWidth, theHeight : Integer; theDefault : T);
var
  I, J : Integer;
begin
  fVersion := 1;
 fWidth := theWidth;
 fHeight := theHeight;
 SetLength( fImage, fHeight );
 for I := 0 to pred( fHeight ) do
   begin
     SetLength( fImage[I], fWidth );
     for J := 0 to pred( fWidth ) do
       fImage[I][J] := theDefault;
   end;
end;

constructor TAMImage.Create(anImage : TAMImage);
var
  X, Y : Integer;
begin
  fVersion := 1;
  fWidth  := anImage.Width;
  fHeight := anImage.Height;
  SetLength( fImage, fHeight );
  for Y := 0 to pred( fHeight ) do
    begin
      SetLength( fImage[Y], fWidth );
      for X := 0 to pred( fWidth ) do
        Pixel[X,Y] := anImage[X,Y];
    end;
end;

procedure TAMImage.Assign(anImage : TAMImage);
var
  I, J : Integer;
begin
  Resize( anImage.Width, anImage.Height );
  for I := 0 to pred( fHeight ) do
    for J := 0 to pred( fWidth ) do
      Pixel[I,J] := anImage[I,J];
end;

destructor TAMImage.Destroy;
var
  I : Integer;
begin
  for I := 0 to pred( fHeight ) do
    SetLength( fImage[I], 0);
  SetLength( fImage, 0);
  inherited Destroy;
end;

function TAMImage.GetPixel(X, Y : Integer) : T;
begin
  Result := fImage[Y,X];
end;

procedure TAMImage.Read(TextIO : TTextIO; aVersion : Integer);
var
  I, J : Integer;
  Pix : T;
begin
  //inherited Read(TextIO, aVersion);
  if aVersion >= 1 then
    begin
      TextIO.Readln( fWidth );
      TextIO.ReadLn( fHeight );
      Resize( fWidth, fHeight );
      for I := 0 to pred( fHeight ) do
        TextIO.ReadMulti( fImage[I], fWidth, sizeof( T ) );
    end;
end;

procedure TAMImage.Resize(theWidth, theHeight : Integer);
var
  I : Integer;
begin
  for I := 0 to pred( fHeight ) do
    SetLength( fImage[I], 0);
  SetLength( fImage, 0);
  fWidth := theWidth;
  fHeight := theHeight;
  SetLength( fImage, fHeight );
  for I := 0 to pred( fHeight ) do
    SetLength( fImage[I], fWidth );
end;

procedure TAMImage.SetConst(Value : T);
var
  X, Y : Integer;
begin
  for Y := 0 to pred( fHeight ) do
    for X := 0 to pred( fWidth ) do
      Pixel[Y,X] := Value;
end;

procedure TAMImage.SetPixel(X, Y : Integer; AValue : T);
begin
  fImage[Y,X] := AValue;
end;

procedure TAMImage.Write(TextIO : TTextIO);
var
  I, J : Integer;
begin
  inherited Write(TextIO);
  TextIO.WriteLn( fWidth );
  TextIO.WriteLn( fHeight );
  for I := 0 to pred( fHeight ) do
    TextIO.WriteMulti( fImage[I], fWidth, SizeOf( T ) );
end;

end.


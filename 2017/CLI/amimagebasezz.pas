unit AMImageBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  AMPersists, AMTextIO;

type

  { TAMImageBase }

  TAMImageBase = class( TAMPersists )
  protected
    fPixelType : String;
    function GetBoolPix(X, Y : Integer) : Boolean;
    function GetBytePix(X, Y : Integer) : Byte;
    function GetDoublePix(X, Y : Integer) : Double;
    function GetIntegerPix(X, Y : Integer) : Integer;
    function GetSinglePix(X, Y : Integer) : Single;
    function GetSmallIntPix(X, Y : Integer) : Integer;
    procedure SetBoolPix(X, Y : Integer; AValue : Boolean);
    procedure SetBytePix(X, Y : Integer; AValue : Byte);
    procedure SetDoublePix(X, Y : Integer; AValue : Double);
    procedure SetIntegerPix(X, Y : Integer; AValue : Integer);
    procedure SetPixSize(AValue : Word);
    procedure SetSinglePix(X, Y : Integer; AValue : Single);
    procedure SetSmallIntPix(X, Y : Integer; AValue : Integer);
  protected
    type
      TCols = array of Byte;
      TRows = array of TCols;
    var
      fHeight : Cardinal;
      fWidth : Cardinal;
      fPixSize : Word; // BytesPerPixel

      vPixels : TRows;

    function XX( X : Integer ) : Cardinal;
    function YY( Y : Integer ) : Cardinal;

    procedure SetHeight(AValue : Cardinal);
    procedure SetWidth(AValue : Cardinal);

    procedure Initialize;

    procedure Random; virtual;

    procedure Reset;

    procedure Resize( theWidth, theHeight : Cardinal ); virtual;
    procedure Resize; virtual;

    procedure Dump( var Value : array of Byte; Size : Integer );
  public
    constructor Create( theWidth, theHeight : Cardinal; thePixSize : Word;
                        thePixelType : String );
    constructor Create( theSource : TAMImageBase );

    procedure Read( TextIO : TTextIO; aVersion : Integer ); override;
    procedure Write( TextIO : TTextIO ); override;


    // Changing Height width or pixsize erases all contents

    property Height  : Cardinal read fHeight  write SetHeight;
    property Width   : Cardinal read fWidth   write SetWidth;
    property PixSize : Word     read fPixSize write SetPixSize;

    property BoolPix[X, Y : Integer]     : Boolean read GetBoolPix
                                                   write SetBoolPix;
    property DoublePix[X, Y : Integer]   : Double  read GetDoublePix
                                                   write SetDoublePix;

    property SinglePix[X, Y : Integer]   : Single  read GetSinglePix
                                                   write SetSinglePix;
    property IntegerPix[X, Y : Integer]  : Integer read GetIntegerPix
                                                   write SetIntegerPix;
    property BytePix[X, Y : Integer]     : Byte    read GetBytePix
                                                   write SetBytePix;
    property SmallIntPix[X, Y : Integer] : Integer read GetSmallIntPix
                                                   write SetSmallIntPix;
    property PixelType : String                    read fPixelType
                                                   write fPixelType;

  end;

implementation

uses
  AMDebug, AMObjectFactory;

{ TAMImageBase }

constructor TAMImageBase.Create(theWidth, theHeight : Cardinal;
  thePixSize : Word; thePixelType : String);
begin
  fWidth := theWidth;
  fHeight := theHeight;
  fPixSize := thePixSize;
  fPixelType := thePixelType;
  fVersion := 1;
  Initialize;
end;

constructor TAMImageBase.Create(theSource : TAMImageBase);
var
  X, Y : Integer;
begin
  fWidth := theSource.Width;
  fHeight := theSource.Height;
  fPixSize := theSource.PixSize;
  fPixelType :=theSource.PixelType;
  fVersion := theSource.fVersion;
  Initialize;
  for Y := 0 to pred(fHeight) do
    for x := 0 to pred(fWidth) do
      Move( theSource.vPixels[X,Y], vPixels[X, Y], fPixSize );
end;

procedure TAMImageBase.Dump(var Value : array of Byte; Size : Integer);
var
  I : Integer;
  S : String;
begin
  S := '';
  for I := 0 to pred(Size) do
    S := S + IntToStr( Value[I] ) + ' ';
  Debug( S );
end;

function TAMImageBase.GetBoolPix(X, Y : Integer) : Boolean;
begin
  Move(vPixels[Y,XX(X)],Result,SizeOf( Boolean ));
end;

function TAMImageBase.GetBytePix(X, Y : Integer) : Byte;
begin
  Move(vPixels[Y,XX(X)],Result,SizeOf( Byte ));
end;

function TAMImageBase.GetDoublePix(X, Y : Integer) : Double;
begin
  Move(vPixels[yy(Y),XX(X)],Result,SizeOf( Double ));
end;

function TAMImageBase.GetIntegerPix(X, Y : Integer) : Integer;
begin
  Move(vPixels[yy(Y),XX(X)],Result,SizeOf( Integer ));
end;

function TAMImageBase.GetSinglePix(X, Y : Integer) : Single;
begin
  Move(vPixels[Y,XX(X)],Result,SizeOf( Single ));
end;

function TAMImageBase.GetSmallIntPix(X, Y : Integer) : Integer;
begin
  Move(vPixels[Y,XX(X)],Result,SizeOf( SmallInt ));
end;

procedure TAMImageBase.Initialize;
var
  I : Integer;
begin
  Reset;
  SetLength( vPixels, fHeight );
  for I := 0 to pred(fHeight) do
    SetLength( vPixels[I], fWidth*fPixSize );
end;

procedure TAMImageBase.Random;
begin
  ;
end;

procedure TAMImageBase.Read(TextIO : TTextIO; aVersion : Integer);
var
  I : Integer;
begin
  //inherited;
  if aVersion >= 1 then
    begin
      TextIO.ReadLn( fPixelType );
      TextIO.ReadLn( fHeight );
      TextIO.ReadLn( fWidth );
      TextIO.ReadLn( fPixSize );
      Initialize;
      for I := 0 to pred( fHeight ) do
        TextIO.ReadMulti( vPixels[I], fWidth, fPixSize );
    end;
end;

procedure TAMImageBase.Reset;
var
  I : Integer;
begin
  for I := 0 to pred(Length( vPixels )) do
    SetLength( vPixels[I], 0 );
  SetLength( vPixels, 0 );
end;

procedure TAMImageBase.Resize(theWidth, theHeight : Cardinal);
begin
  Reset;
  fWidth := theWidth;
  fHEight := theHeight;
  Initialize;
end;

procedure TAMImageBase.Resize;
begin
  Reset;
  Initialize;
end;

procedure TAMImageBase.SetBoolPix(X, Y : Integer; AValue : Boolean);
begin
  Move(AValue,vPixels[Y,XX(X)],SizeOf( Boolean ));
end;

procedure TAMImageBase.SetBytePix(X, Y : Integer; AValue : Byte);
begin
  Move(AValue,vPixels[Y,XX(X)],SizeOf( Byte ));
end;

procedure TAMImageBase.SetDoublePix(X, Y : Integer; AValue : Double);
begin
  Move(AValue,vPixels[Y,XX(X)],SizeOf( Double ));
end;

procedure TAMImageBase.SetHeight(AValue : Cardinal);
begin
  if fHeight = AValue then Exit;
  fHeight := AValue;
  Initialize;
end;

procedure TAMImageBase.SetIntegerPix(X, Y : Integer; AValue : Integer);
begin
  Move(AValue,vPixels[Y,XX(X)],SizeOf( Integer ));
end;

procedure TAMImageBase.SetPixSize(AValue : Word);
begin
  if fPixSize = AValue then Exit;
  fPixSize := AValue;
  Initialize;
end;

procedure TAMImageBase.SetSinglePix(X, Y : Integer; AValue : Single);
begin
  Move(AValue,vPixels[Y,XX(X)],SizeOf( Single ));
end;

procedure TAMImageBase.SetSmallIntPix(X, Y : Integer; AValue : Integer);
var
  XXX : Integer;
begin
  XXX := XX(X);
  Move(AValue,vPixels[Y,XX(X)],SizeOf( SmallInt ));
end;

procedure TAMImageBase.SetWidth(AValue : Cardinal);
begin
  if fWidth = AValue then Exit;
  fWidth := AValue;
  Initialize;
end;

procedure TAMImageBase.Write(TextIO : TTextIO);
var
  I : Integer;
begin
  //inherited;
  TextIO.WriteLn( fPixelType );
  TextIO.WriteLn( fHeight );
  TextIO.WriteLn( fWidth );
  TextIO.WriteLn( fPixSize );
  for I := 0 to pred( fHeight ) do
    TextIO.WriteMulti( vPixels[I], fWidth, fPixSize );

end;

function TAMImageBase.XX(X : Integer) : Cardinal;
var
  L : Integer;
begin
  X := X * fPixSize;
  L := fWidth*fPixSize;
  while X < 0 do X := X + L;
  X := X mod L;
  Result := X;
end;

function TAMImageBase.YY(Y : Integer) : Cardinal;
begin
  while Y < 0 do Y := Y + fHeight;
  Y := Y mod fHeight;
  Result := Y;
end;

initialization
  ObjectFactory.RegisterClass( TAMImageBase );
end.


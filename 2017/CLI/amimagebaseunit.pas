unit amimagebaseunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  AMPersists, AMTextIO;

type

  TLMMask = array[-1..1,-1..1] of Boolean;

// MakeLMMask( Input : Integer; out Mask : TLMMask)
// Input expects octal number such that
//
// &400 = X 0 0      Y = -1
//        0 0 0      Y = 0
//        0 0 0      Y = 1
//
// &200 = 0 X 0
//        0 0 0
//        0 0 0
//
// &100 = 0 0 X
//        0 0 0
//        0 0 0
//
// &040 = 0 0 0
//        X 0 0
//        0 0 0
//
// etc.
//

{ TAMImageBase }

  TAMImageBase = class( TAMPersists )
  private
    function  GetBoolPixP(P : TPoint ) : Boolean;
    function GetBytePixP(P : TPoint) : Byte;
    function GetDoublePixP(P : TPoint) : Double;
    function GetIntegerPixP(P : TPoint) : Integer;
    function GetSinglePixP(P : TPoint) : Single;
    function GetSmallIntPixP(P : TPoint) : SmallInt;
    procedure SetBoolPixP(P : TPoint ; AValue : Boolean);
    procedure SetBytePixP(P : TPoint; AValue : Byte);
    procedure SetDoublePixP(P : TPoint; AValue : Double);
    procedure SetIntegerPixP(P : TPoint; AValue : Integer);
    procedure SetSinglePixP(P : TPoint; AValue : Single);
    procedure SetSmallIntPix(X, Y : Integer; AValue : SmallInt);
    procedure SetSmallIntPixP(P : TPoint; AValue : SmallInt);
  protected
    fPixelType : String;
    function GetBoolPix(X, Y : Integer) : Boolean;
    function GetBytePix(X, Y : Integer) : Byte;
    function GetDoublePix(X, Y : Integer) : Double;
    function GetIntegerPix(X, Y : Integer) : Integer;
    function GetSinglePix(X, Y : Integer) : Single;
    function GetSmallIntPix(X, Y : Integer) : SmallInt;
    procedure SetBoolPix(X, Y : Integer; AValue : Boolean);
    procedure SetBytePix(X, Y : Integer; AValue : Byte);
    procedure SetDoublePix(X, Y : Integer; AValue : Double);
    procedure SetIntegerPix(X, Y : Integer; AValue : Integer);
    procedure SetPixSize(AValue : Word);
    procedure SetSinglePix(X, Y : Integer; AValue : Single);
    //procedure SetSmallIntPix(X, Y : Integer; AValue : Integer);
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

    procedure Draw( Canvas : TCanvas; Zoom : Double; ColorMap : Integer ); virtual; abstract;
    procedure DrawPixel( theCanvas : TCanvas; X, Y, Z : Integer; Pix : TColor );

    procedure SetHeight(AValue : Cardinal);
    procedure SetWidth(AValue : Cardinal);

    procedure Initialize;

    procedure Random; virtual;

    procedure Reset;

    procedure Resize( theWidth, theHeight : Cardinal ); virtual;
    procedure Resize; virtual;

    procedure Dump( var Value : array of Byte; Size : Integer );


    procedure MakeLMMask( Mask : Integer; out LMMask : TLMMask );
  public
    constructor Create( theWidth, theHeight : Cardinal; thePixSize : Word;
                        thePixelType : String );
    constructor Create( theSource : TAMImageBase );

    function PixVal( X, Y : Integer ) : String; virtual; abstract;

    procedure Read( TextIO : TTextIO; aVersion : Integer ); override;
    procedure Write( TextIO : TTextIO ); override;
    procedure DebugOut( Path : String );


    // Changing Height width or pixsize erases all contents

    property Height  : Cardinal read fHeight  write SetHeight;
    property Width   : Cardinal read fWidth   write SetWidth;
    property PixSize : Word     read fPixSize write SetPixSize;

    // General Image Processing Methods

    procedure Dilate( Mask : Integer ); virtual; abstract;
    procedure Erode( Mask : Integer ); virtual; abstract;
    //procedure Cover( aPixVal : Integer; WithVal : Integer = 0); virtual; abstract;
    //procedure Cover( aPixVal : Double; WithVal : Double = 0); virtual; abstract;

    property BoolPix[X, Y : Integer]     : Boolean read GetBoolPix
                                                   write SetBoolPix;
    property BoolPixP[P : TPoint ]       : Boolean read GetBoolPixP
                                                   write SetBoolPixP;

    property DoublePix[X, Y : Integer]   : Double  read GetDoublePix
                                                   write SetDoublePix;
    property DoublePixP[P : TPoint]      : Double  read GetDoublePixP
                                                   write SetDoublePixP;

    property SinglePix[X, Y : Integer]   : Single  read GetSinglePix
                                                   write SetSinglePix;
    property SinglePixP[P : TPoint]  : Single  read GetSinglePixP
                                                   write SetSinglePixP;

    property IntegerPix[X, Y : Integer]  : Integer read GetIntegerPix
                                                   write SetIntegerPix;
    property IntegerPixP[P : TPoint]  : Integer read GetIntegerPixP
                                                   write SetIntegerPixP;

    property BytePix[X, Y : Integer]     : Byte    read GetBytePix
                                                   write SetBytePix;
    property BytePixP[P : TPoint]    : Byte    read GetBytePixP
                                                   write SetBytePixP;

    property SmallIntPix[X, Y : Integer] : SmallInt read GetSmallIntPix
                                                    write SetSmallIntPix;
    property SmallIntPixp[P : TPoint] : SmallInt read GetSmallIntPixP
                                                     write SetSmallIntPixP;

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
      begin
        Dump( theSource.vPixels[X,Y],sizeof(double) );
        Move( theSource.vPixels[X,Y], vPixels[X, Y], fPixSize );
      end;
end;

procedure TAMImageBase.DebugOut(Path : String);
var
  TextIO : TTextIO;
begin
  TextIO := TTextIO.Create(Path,true);
  Store( TextIO );
  TextIO.Free;
end;

procedure TAMImageBase.DrawPixel( theCanvas : TCanvas;
                                  X, Y, Z : Integer; Pix : TColor);
var
  X0, Y0, X1, Y1 : Integer;
begin
  X0 := X * Z;
  Y0 := Y * Z;
  X1 := X0 + Z;
  Y1 := Y0 + Z;
  theCanvas.Brush.Color := Pix;
  theCanvas.Brush.Style := bsSolid;
  theCanvas.FillRect(X0,Y0,X1,Y1);
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
  Result := False;
  Move(vPixels[YY(Y),XX(X)],Result,SizeOf( Boolean ));
end;

//function TAMImageBase.GetBoolPix(P : TPoint ) : Boolean;
//begin
//  Result := GetBoolPix[P.X,P.Y];
//end;

function TAMImageBase.GetBoolPixP(P : TPoint ) : Boolean;
begin
  Result := GetBoolPix(P.X,P.Y);
end;

function TAMImageBase.GetBytePix(X, Y : Integer) : Byte;
begin
  Result := 0;
  Move(vPixels[YY(Y),XX(X)],Result,SizeOf( Byte ));
end;

function TAMImageBase.GetBytePixP(P : TPoint) : Byte;
begin
  Result := GetBytePix(P.X,P.Y);
end;

function TAMImageBase.GetDoublePix(X, Y : Integer) : Double;
begin
  Result := 0.0;
  Move(vPixels[YY(Y),XX(X)],Result,SizeOf( Double ));
end;

function TAMImageBase.GetDoublePixP(P : TPoint) : Double;
begin
  Result := GetDoublePix(P.X,P.Y);
end;

function TAMImageBase.GetIntegerPix(X, Y : Integer) : Integer;
begin
  Result := 0;
  Move(vPixels[YY(Y),XX(X)],Result,SizeOf( Integer ));
end;

function TAMImageBase.GetIntegerPixP(P : TPoint) : Integer;
begin
  Result := GetIntegerPix(P.X,P.Y);
end;

//function TAMImageBase.GetIntegerPixP(P : TPoint) : Integer;
//begin
//
//end;

function TAMImageBase.GetSinglePix(X, Y : Integer) : Single;
begin
  Result := 0.0;
  Move(vPixels[YY(Y),XX(X)],Result,SizeOf( Single ));
end;

//function TAMImageBase.GetSinglePixP(X, Y : Integer) : Single;
//begin
//  Result := GetSinglePix(X,Y);
//end;

function TAMImageBase.GetSinglePixP(P : TPoint) : Single;
begin
  Result := GetSinglePix(P.X,P.Y);
end;

function TAMImageBase.GetSmallIntPix(X, Y : Integer) : SmallInt;
begin
  Result := 0;
  Move(vPixels[YY(Y),XX(X)],Result,SizeOf( SmallInt ));
end;

//function TAMImageBase.GetSmallIntPixP(X, Y : Integer) : SmallInt;
//begin
//  Result := GetSmallIntPix(X,Y);
//end;

function TAMImageBase.GetSmallIntPixP(P : TPoint) : SmallInt;
begin
  Result := GetSmallIntPix(P.X,P.Y);
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

procedure TAMImageBase.MakeLMMask(Mask : Integer; out LMMask : TLMMask);
var
  X, Y : Integer;
  P : Integer;
begin
  for Y := -1 to 1 do
    for X := -1 to 1 do
      LMMask[X,Y] := False;
  P := 1;
  for Y := 1 downto -1 do
    begin
      for X := 1 downto -1 do
        begin
          LMMask[X,Y] := (Mask and P) = P;
          P := P shl 1;
        end;
    end;
  LMMask[0,0] := False; // Always kill the center pixel
end;

procedure TAMImageBase.Random;
begin
  ;
end;

procedure TAMImageBase.Read(TextIO : TTextIO; aVersion : Integer);
var
  I : Integer;
begin
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

//procedure TAMImageBase.SetBoolPix(P : TPoint; AValue : Boolean);
//begin
//
//end;

procedure TAMImageBase.SetBoolPixP(P : TPoint ; AValue : Boolean);
begin
  SetBoolPix(P.X,P.Y,aValue);
end;

procedure TAMImageBase.SetBytePix(X, Y : Integer; AValue : Byte);
begin
  Move(AValue,vPixels[Y,XX(X)],SizeOf( Byte ));
end;

//procedure TAMImageBase.SetBytePixP(X, Y : Integer; AValue : Byte);
//begin
//  SetBytePix(P.X,P.Y);
//end;

procedure TAMImageBase.SetBytePixP(P : TPoint; AValue : Byte);
begin
  SetBytePix(P.X,P.Y,AValue);
end;

procedure TAMImageBase.SetDoublePix(X, Y : Integer; AValue : Double);
begin
  Move(AValue,vPixels[YY(Y),XX(X)],SizeOf( Double ));
end;

procedure TAMImageBase.SetDoublePixP(P : TPoint; AValue : Double);
begin
  SetDoublePix(P.X,P.Y,AValue);
end;

procedure TAMImageBase.SetHeight(AValue : Cardinal);
begin
  if fHeight = AValue then Exit;
  fHeight := AValue;
  Initialize;
end;

procedure TAMImageBase.SetIntegerPix(X, Y : Integer; AValue : Integer);
begin
  Move(AValue,vPixels[YY(Y),XX(X)],SizeOf( Integer ));
end;

procedure TAMImageBase.SetIntegerPixP(P : TPoint; AValue : Integer);
begin
  SetIntegerPix(P.X,P.Y,AValue);
end;

procedure TAMImageBase.SetPixSize(AValue : Word);
begin
  if fPixSize = AValue then Exit;
  fPixSize := AValue;
  Initialize;
end;

procedure TAMImageBase.SetSinglePix(X, Y : Integer; AValue : Single);
begin
  Move(AValue,vPixels[YY(Y),XX(X)],SizeOf( Single ));
end;

procedure TAMImageBase.SetSinglePixP(P : TPoint; AValue : Single);
begin
  SetSinglePix(P.X,P.Y,AValue);
end;

//procedure TAMImageBase.SetSmallIntPix(X, Y : Integer; AValue : Integer);
//begin
//  Move(AValue,vPixels[YY(Y),XX(X)],SizeOf( SmallInt ));
//end;

procedure TAMImageBase.SetSmallIntPix(X, Y : Integer; AValue : SmallInt);
begin
  Move(AValue,vPixels[YY(Y),XX(X)],SizeOf( SmallInt ));
end;

procedure TAMImageBase.SetSmallIntPixP(P : TPoint; AValue : SmallInt);
begin
  SetSmallIntPix(P.X,P.Y,AValue);
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


unit amimageUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  AMImageBaseUnit, AMTextIO, AMPersists;

type

  { TAMBooleanImage }

  TAMBooleanImage = class( TAMImageBase )
  private
    function GetPix(P : TPoint) : Boolean;
    function GetPixel(X, Y : Integer) : Boolean;
    procedure SetPix(P : TPoint; AValue : Boolean);
    procedure SetPixel(X, Y : Integer; AValue : Boolean);
  public
    constructor Create( theWidth, theHeight : Cardinal ); virtual;
    constructor Create( theWidth, theHeight : Cardinal; theValue : Boolean ); virtual;
    constructor Create( theSource : TAMImageBase ); virtual;
    constructor Create( TextIO : TTextIO; aParent : TAMPersists = nil ); override;

    destructor  Destroy; override;

    function PixVal( X, Y : Integer ) : String; override;

    procedure Draw( Canvas : TCanvas; Zoom :Double; ColorMap : Integer ); override;

    procedure Resize; override;
    procedure Resize( theWidth, theHeight : Cardinal ); override;

    // General Image Processing Methods

    procedure Dilate( Mask : Integer ); override;
    procedure Erode( Mask : Integer ); override;
    procedure Invert;

    property Pixel[X,Y : Integer] : Boolean read GetPixel write SetPixel; default;
    property Pix[P : TPoint] : Boolean read GetPix write SetPix;
  end;

  { TAMShortIntImage }

  TAMShortIntImage = class( TAMImageBase )
  private
    function GetPix(P : TPoint) : SmallInt;
    function GetPixel(X, Y : Integer) : SmallInt;
    procedure SetPix(P : TPoint; AValue : SmallInt);
    procedure SetPixel(X, Y : Integer; AValue : SmallInt);
  protected
    procedure LabelBlob(BlobImg : TAMBooleanImage; X, Y, Val : Integer );
  public
    constructor Create( theWidth, theHeight : Cardinal ); virtual;
    constructor Create( theWidth, theHeight : Cardinal; theValue : SmallInt ); virtual;

    destructor  Destroy; override;

    function PixVal( X, Y : Integer ) : String; override;

    procedure Draw( Canvas : TCanvas; Zoom :Double; ColorMap : Integer ); override;

    procedure LabelBlobs( BlobImg : TAMBooleanImage );

    procedure Resize; override;
    procedure Resize( theWidth, theHeight : Cardinal ); override;

    procedure Dilate( Mask : Integer ); override;
    procedure Erode( Mask : Integer ); override;
    procedure Cover( aPixVal : Integer; WithVal : Integer = 0 ); //override;
    procedure Threshold( AValue : SmallInt; var BoolImg : TAMBooleanImage;
                         Invert : Boolean = False );

    property Pixel[X,Y : Integer] : SmallInt read GetPixel write SetPixel; default;
    property Pix[P : TPoint] : SmallInt read GetPix write SetPix;
  end;

  { TAMDoubleImg }

  TAMDoubleImg = class( TAMImageBase )
  private
    function GetPix(P : TPoint) : Double;
    function GetPixel(X, Y : Integer) : Double;
    procedure SetPix(P : TPoint; AValue : Double);
    procedure SetPixel(X, Y : Integer; AValue : Double);
  public
    constructor Create( theWidth, theHeight : Cardinal);
    constructor Create( theWidth, theHeight : Cardinal; theValue : Double );
    constructor Create( theSource : TAMDoubleImg );

    procedure Convo( Kernel : TAMDoubleImg );
    procedure Convo2d( Kernel : TAMDoubleImg );

    procedure Draw( Canvas : TCanvas; Zoom :Double; ColorMap : Integer ); override;

    procedure Dump(var Value : array of Byte; Size : Integer);

    procedure MinMaxMean( var theMin, theMax, theMean : Double );

    procedure KernelNormalize; // Symmetric kernel sum set to 1.0;

    procedure Random; override;

    procedure Resize; override;
    procedure Resize( theWidth, theHeight : Cardinal ); override;
    function PixVal( X, Y : Integer ) : String; override;

    procedure Cover( aPixVal : Double; WithVal : Double = 0.0 );// override;
    procedure Dilate( Mask : Integer ); override;
    procedure Erode( Mask : Integer ); override;
    procedure Threshold( AValue : Double; var BoolImg : TAMBooleanImage;
                         Invert : Boolean = False );

    property Pixel[X,Y : Integer] : Double read GetPixel write SetPixel; default;
    property Pix[P : TPoint] : Double read GetPix write SetPix;
  end;

implementation

uses
  Math,
  AMDebug, AMObjectFactory, AMMessages;

{ TAMShortIntImage }

constructor TAMShortIntImage.Create(theWidth, theHeight : Cardinal);
begin
  inherited Create( theWidth, theHeight, Sizeof( SmallInt ), 'ShortInt' );
end;

constructor TAMShortIntImage.Create(theWidth, theHeight : Cardinal;
  theValue : SmallInt);
var
  X, Y: Cardinal;
begin
  inherited Create(theWidth, theHeight, SizeOf(SmallInt), 'SmallInt' );
  for Y := 0 to pred( theHeight ) do
    for X := 0 to pred( theWidth ) do
      SmallIntPix[X,Y] := theValue;
end;

procedure TAMShortIntImage.Cover(aPixVal : Integer; WithVal : Integer);
var
  X, Y : Integer;
begin
  for Y := 0 to pred(Height) do
    for X := 0 to pred(Width) do
      if Pixel[X,Y] = aPixVal then
        Pixel[X,Y] := WithVal;
end;

destructor TAMShortIntImage.Destroy;
begin
  inherited Destroy;
end;

procedure TAMShortIntImage.Dilate(Mask : Integer);
begin
  ;
end;

type
  TColorLabeled = array[0..7] of TColor;

const
  ColorLabeled : TColorLabeled =
    ( clLime, clBlue, clGray, clYellow, clRed, clGreen, clPurple, clWhite );

procedure TAMShortIntImage.Draw(Canvas : TCanvas; Zoom : Double; ColorMap : Integer);
  function MapColor( PixVal : SmallInt; Map : Integer ) : TColor;
  begin
    case Map of
      0, 1, 2:
        begin
          if PixVal = 0 then
            Result := clBlack
          else
            Result := ColorLabeled[ PixVal mod 8 ];
        end;
    end;

  end;

var
  //ZZ : Single;
  X, Y, Z : Integer;
  P     : TColor;
begin
  if Zoom >= 1.0 then
    begin
      Z := round( Zoom );
      for Y := 0 to pred( Height ) do
        for X := 0 to pred( Width ) do
          begin
            P := MapColor( Pixel[X,Y], ColorMap );
            DrawPixel( Canvas, X, Y, Z, P );
          end;
    end
  else
    begin
      //ZZ := intPower(2.0, Zoom);
      Stub('TAMShortIntImage.Draw with size reduction');
    end;
end;

procedure TAMShortIntImage.Erode(Mask : Integer);
begin
  ;
end;

function TAMShortIntImage.GetPix(P : TPoint) : SmallInt;
begin
  Result := SmallIntPix[P.X,P.Y];
end;

function TAMShortIntImage.GetPixel(X, Y : Integer) : SmallInt;
begin
  Result := SmallIntPix[X,Y];
end;

procedure TAMShortIntImage.LabelBlob( BlobImg : TAMBooleanImage;
                                      X, Y, Val : Integer);
var
  X1, Y1 : Integer;
begin
  for X1 := -1 to 1 do
    for Y1 := -1 to 1 do
      if BlobImg[X+X1,Y+Y1] and (Pixel[X+X1,Y+Y1] = 0) then
        begin
          Pixel[X+X1, y+Y1] := Val;
          LabelBlob( BlobImg,X+X1,Y+Y1,Val);
        end;
end;

procedure TAMShortIntImage.LabelBlobs(BlobImg : TAMBooleanImage);
var
  X, Y : Integer;
  Val  : Integer;
begin
  fWidth := BlobImg.Width;
  fHeight := BlobImg.Height;
  Reset;
  Initialize;
  Val := 1; // The first label
  for Y := 0 to pred(Height) do
    for X := 0 to pred(Width) do
      if BlobImg[X,Y] and (Pixel[X,Y] = 0) then
        begin
          LabelBlob( BlobImg, X, Y, Val );
          Inc(Val);
        end;
end;

function TAMShortIntImage.PixVal(X, Y : Integer) : String;
begin
  Result := IntToStr( Pixel[X, Y] );
end;

procedure TAMShortIntImage.Resize(theWidth, theHeight : Cardinal);
begin
  inherited Resize(theWidth, theHeight);
end;

procedure TAMShortIntImage.Resize;
begin
  inherited Resize;
end;

procedure TAMShortIntImage.SetPix(P : TPoint; AValue : SmallInt);
begin
  SmallIntPix[P.X,P.Y] := AValue;
end;

procedure TAMShortIntImage.SetPixel(X, Y : Integer; AValue : SmallInt);
begin
  SmallIntPix[X,Y] := AValue;
end;

procedure TAMShortIntImage.Threshold(AValue : SmallInt;
  var BoolImg : TAMBooleanImage; Invert : Boolean);
var
  X, Y : Cardinal;
  B    : Boolean;
begin
  if Assigned( BoolImg ) then
    BoolImg.Free;
  BoolImg := TAMBooleanImage.Create( Width, Height, False );
  for X := 0 to pred( Width ) do
    for Y := 0 to pred( Height ) do
      begin
        B := (Pixel[X,Y] < AValue) <> Invert;
        BoolImg[X,Y] := B;
      end;
end;

{ TAMBooleanImage }

constructor TAMBooleanImage.Create(theWidth, theHeight : Cardinal);
begin
  inherited Create( theWidth, theHeight, Sizeof( Boolean ), 'Boolean' );
end;

constructor TAMBooleanImage.Create(theWidth, theHeight : Cardinal;
  theValue : Boolean);
var
  X, Y: Cardinal;
begin
  inherited Create(theWidth, theHeight, SizeOf(Boolean), 'Boolean' );
  for Y := 0 to pred( theHeight ) do
    for X := 0 to pred( theWidth ) do
      BoolPix[X,Y] := theValue;
end;

constructor TAMBooleanImage.Create(theSource : TAMImageBase);
begin
  inherited Create(theSource);
end;

constructor TAMBooleanImage.Create(TextIO : TTextIO; aParent : TAMPersists);
begin
  inherited Create(TextIO, aParent);
end;

destructor TAMBooleanImage.Destroy;
begin
  inherited;
end;

procedure TAMBooleanImage.Dilate(Mask : Integer);
var
  theMask : TLMMask;
  Temp : TAMBooleanImage;
  Val  : Boolean;
  X, Y, X0, Y0 : Integer;
begin
  Temp := TAMBooleanImage.Create( Self );
  Initialize;
  MakeLMMask( Mask, theMask );
  theMask[0,0] := True;
  for Y := 0 to pred(Height) do
    for X := 0 to pred(Width) do
      begin
        Val := False;
        for Y0 := -1 to 1 do
          for X0 := -1 to 1 do
            Val := Val or (Temp[X+X0,Y+Y0] and theMask[X0,Y0]);
        Pixel[X,Y] := Val;
      end;
end;

procedure TAMBooleanImage.Draw(Canvas : TCanvas; Zoom : Double; ColorMap : Integer);
var
  //ZZ : Single;
  X, Y, Z : Integer;
  P       : TColor;
begin
  if Zoom >= 1.0 then
    begin
      Z := round( Zoom );
      //Canvas.Height := Height * Z;
      //Canvas.Width := Width * Z;
      for Y := 0 to pred( Height ) do
        for X := 0 to pred( Width ) do
          begin
            P := ord(Pixel[X,Y])*clWhite;
            DrawPixel( Canvas, X, Y, Z, P );
          end;
    end
  else
    begin
      //ZZ := intPower(2.0, Zoom);
      Stub('TAMShortIntImage.Draw with size reduction');
    end;
end;

procedure TAMBooleanImage.Erode(Mask : Integer);
//var
//  theMask : TLMMask;
begin
  //MakeLMMask( Mask, theMask );
  Invert;
  Dilate(Mask);
  Invert;
end;

function TAMBooleanImage.GetPix(P : TPoint) : Boolean;
begin
  Result := BoolPix[P.X,P.Y];
end;

function TAMBooleanImage.GetPixel(X, Y : Integer) : Boolean;
begin
  Result := BoolPix[X,Y];
end;

procedure TAMBooleanImage.Invert;
var
  X, Y : Integer;
begin
  for X := 0 to pred(Width) do
    for Y := 0 to pred(Height) do
      Pixel[X,Y] := not Pixel[X,Y];
end;

function TAMBooleanImage.PixVal(X, Y : Integer) : String;
begin
  if Pixel[X, Y] then
    Result := 'True'
  else
    Result := 'False';
end;

procedure TAMBooleanImage.Resize(theWidth, theHeight : Cardinal);
begin
  inherited Resize(theWidth, theHeight);
end;

procedure TAMBooleanImage.Resize;
begin
  inherited Resize;
end;

procedure TAMBooleanImage.SetPix(P : TPoint; AValue : Boolean);
begin
  BoolPix[P.X,P.Y] := AValue;
end;

procedure TAMBooleanImage.SetPixel(X, Y : Integer; AValue : Boolean);
begin
  BoolPix[X,Y] := AValue;
end;

{ TAMDoubleImg }

constructor TAMDoubleImg.Create(theWidth, theHeight : Cardinal);
begin
  inherited Create(theWidth, theHeight, SizeOf(Double), 'Double' );
end;

constructor TAMDoubleImg.Create( theWidth, theHeight : Cardinal;
                                   theValue : Double);
var
  X, Y: Cardinal;
begin
  inherited Create(theWidth, theHeight, SizeOf(Double), 'Double' );
  for Y := 0 to pred( theHeight ) do
    for X := 0 to pred( theWidth ) do
      DoublePix[X,Y] := theValue;
end;

constructor TAMDoubleImg.Create(theSource : TAMDoubleImg);
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
        //Debug('X,Y: %d, %d = %f',[X,Y, theSource[x,y]]);
        Pixel[X,Y] := theSource[X,Y];
      end;
end;

procedure TAMDoubleImg.Convo(Kernel : TAMDoubleImg);
var
  X, Y, X0, Y0 : Integer;
  HH, WW : Integer;
  Temp : TAMDoubleImg;
  P, T, K, R : Double;
begin
  Temp := TAMDoubleImg.Create( Self );
  Initialize;
  try
    HH := pred(Kernel.Height);
    WW := pred(Kernel.Width);
    for X := 0 to pred(Width) do
      for Y := 0 to pred(Height) do
        for X0 := -WW to WW do
          for Y0 := -HH to HH do
            begin
            //DoublePix[X,Y] := DoublePix[X,Y] +
            //                  Kernel.DoublePix[X0,Y0]*Temp.DoublePix[X+X0, Y+Y0];
              P := DoublePix[X,Y];
              K := Kernel[abs(X0),abs(Y0)];
              T := Temp[x+x0, Y+Y0];
              R := K*T;
              DoublePix[X,Y] := P + R;
            end;
  finally
    Temp.Free;
  end;
end;

procedure TAMDoubleImg.Convo2d(Kernel : TAMDoubleImg);
var
  X, Y : Integer;
  //I, J : Integer;
  Temp : TAMDoubleImg;
  I    : Integer;
  P, T, K, R : Double;
begin

  if Kernel.Height <> 1 then
    raise Exception.Create('2d Kernel must be an N by 1 image.');

  Temp := TAMDoubleImg.Create( Self );
  Initialize;

  for Y := 0 to pred(Height) do
    for X := 0 to pred(Width) do
      for I := 0 to pred(Kernel.Width) do
      begin

      end;

end;

procedure TAMDoubleImg.Cover(aPixVal : Double; WithVal : Double);
begin
  ;
end;

procedure TAMDoubleImg.Dilate(Mask : Integer);
begin
  ;
end;

procedure TAMDoubleImg.Draw(Canvas : TCanvas; Zoom : Double; ColorMap : Integer
  );
function MapColor( PixVal : Double; Map : Integer ) : TColor;
var
  IPix : Integer;
begin
  IPix := Trunc( PixVal );

  case Map of
    0:
      begin
        //Result := IPix + IPix*256 + IPix*256*256;
        Result := RGBToColor( IPIX, IPIX, IPIX );
      end;
    1:
      begin

      end;
    2:
      begin
        if IPix in [0,255] then
          Result := clBlack
        else
          Result := ColorLabeled[ IPix mod 8 ];
      end;
  end;

end;

var
//ZZ : Single;
  X, Y, Z : Integer;
  P     : TColor;
begin
if Zoom >= 1.0 then
  begin
    Z := round( Zoom );
    for Y := 0 to pred( Height ) do
      for X := 0 to pred( Width ) do
        begin
          P := MapColor( Pixel[X,Y], ColorMap );
          DrawPixel( Canvas, X, Y, Z, P );
        end;
  end
else
  begin
    //ZZ := intPower(2.0, Zoom);
    Stub('TAMShortIntImage.Draw with size reduction');
  end;
end;

procedure TAMDoubleImg.Dump(var Value : array of Byte; Size : Integer);
var
  S : String;
begin
  S := FloatToStr( Double( Value ));
  Debug( S );
end;

procedure TAMDoubleImg.Erode(Mask : Integer);
begin
  //Invert;
  Dilate(Mask);
  //Invert;
end;

function TAMDoubleImg.GetPix(P : TPoint) : Double;
begin

  Result := DoublePix[P.X,P.Y];
end;

function TAMDoubleImg.GetPixel(X, Y : Integer) : Double;
begin
  Result := DoublePix[X,Y];
end;

procedure TAMDoubleImg.KernelNormalize;
var
  Sum : Double;
  X, Y : Integer;
  H, W : Integer;
begin
  Sum := 0.0;
  H := pred(Height);
  W := pred(Width);
  for X := 0 to W do
    for Y := 0 to H do
      Sum := Sum + Pixel[X,Y]*2.0;
  Sum := Sum - Pixel[0,0];
  for X := 0 to W do
    for Y := 0 to H do
      Pixel[X,Y] := Pixel[X,Y] / Sum;
end;

procedure TAMDoubleImg.MinMaxMean(var theMin, theMax, theMean : Double);
var
  X, Y : Integer;
  P    : Double;
begin
  theMean := 0.0;
  theMin := 1.0e6; //Pixel[0,0];
  theMax := -1.0e6; //Pixel[0,0];
  for Y := 0 to pred( Height ) do
    for X := 0 to pred( Width ) do
      begin
        P := Pixel[X,Y];
        theMin := Min( theMin, P );
        theMax := Max( theMax, P );
        theMean := theMean + P;
      end;
  theMean := theMean / Double( Height*Width );
end;

function TAMDoubleImg.PixVal(X, Y : Integer) : String;
begin
  Result := FloatToStr(Pixel[X,Y]);
end;

procedure TAMDoubleImg.Random;
var
  X, Y : Cardinal;
  P    : Double;
begin
  //inherited Random;
  for X := 0 to pred( Width ) do
    for Y := 0 to pred( Height ) do
      begin
        P := System.Random;
        Pixel[X,Y] := P;
      end;
end;

procedure TAMDoubleImg.Resize(theWidth, theHeight : Cardinal);
begin
  inherited Resize(theWidth, theHeight);
end;

procedure TAMDoubleImg.Resize;
begin
  inherited Resize;
end;

procedure TAMDoubleImg.SetPix(P : TPoint; AValue : Double);
begin
  DoublePix[P.X,P.Y] := AValue;
end;

procedure TAMDoubleImg.SetPixel(X, Y : Integer; AValue : Double);
begin
  DoublePix[X,Y] := AValue;
end;

procedure TAMDoubleImg.Threshold(AValue : Double; var BoolImg : TAMBooleanImage;
  Invert : Boolean);
var
  X, Y : Cardinal;
  B    : Boolean;
begin
  if Assigned( BoolImg ) then
    BoolImg.Free;
  BoolImg := TAMBooleanImage.Create( Width, Height, False );
  for X := 0 to pred( Width ) do
    for Y := 0 to pred( Height ) do
      begin
        B := (Pixel[X,Y] < AValue) <> Invert;
        BoolImg[X,Y] := B;
      end;
end;

initialization
  ObjectFactory.RegisterClass( TAMBooleanImage );
  ObjectFactory.RegisterClass( TAMDoubleImg );
  ObjectFactory.RegisterClass( TAMShortIntImage );
  //AMObjectFactory.RegisterClass( TAMBooleanImage );
  //AMObjectFactory.RegisterClass( TAMDoubleImg );
  //AMObjectFactory.RegisterClass( TAMShortIntImage );
end.


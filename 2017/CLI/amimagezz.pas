unit AMImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AMImageBase;

type

  { TAMBooleanImage }

  TAMBooleanImage = class( TAMImageBase )
  private
    function GetPixel(X, Y : Integer) : Boolean;
    procedure SetPixel(X, Y : Integer; AValue : Boolean);
  public
    constructor Create( theWidth, theHeight : Cardinal ); virtual;
    constructor Create( theWidth, theHeight : Cardinal; theValue : Boolean ); virtual;

    destructor  Destroy; virtual;

    procedure Resize; override;
    procedure Resize( theWidth, theHeight : Cardinal ); override;

    property Pixel[X,Y : Integer] : Boolean read GetPixel write SetPixel; default;
  end;

  { TAMDoubleImage }

  TAMDoubleImage = class( TAMImageBase )

  private
    function GetPixel(X, Y : Integer) : Double;
    procedure SetPixel(X, Y : Integer; AValue : Double);
  public
    constructor Create( theWidth, theHeight : Cardinal);
    constructor Create( theWidth, theHeight : Cardinal; theValue : Double );

    procedure Convo( Kernel : TAMDoubleImage );

    procedure Random; override;

    procedure Resize; override;
    procedure Resize( theWidth, theHeight : Cardinal ); override;

    procedure Threshold( AValue : Double; var BoolImg : TAMBooleanImage;
                         Invert : Boolean = False );

    property Pixel[X,Y : Integer] : Double read GetPixel write SetPixel; default;
  end;

implementation

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

destructor TAMBooleanImage.Destroy;
begin
  inherited;
end;

function TAMBooleanImage.GetPixel(X, Y : Integer) : Boolean;
begin
  Result := BoolPix[X,Y];
end;

procedure TAMBooleanImage.Resize(theWidth, theHeight : Cardinal);
begin
  inherited Resize(theWidth, theHeight);
end;

procedure TAMBooleanImage.Resize;
begin
  inherited Resize;
end;

procedure TAMBooleanImage.SetPixel(X, Y : Integer; AValue : Boolean);
begin
  BoolPix[X,Y] := AValue;
end;

{ TAMDoubleImage }

constructor TAMDoubleImage.Create(theWidth, theHeight : Cardinal);
begin
  inherited Create(theWidth, theHeight, SizeOf(Double), 'Double' );
end;

constructor TAMDoubleImage.Create( theWidth, theHeight : Cardinal;
                                   theValue : Double);
var
  X, Y: Cardinal;
begin
  inherited Create(theWidth, theHeight, SizeOf(Double), 'Double' );
  for Y := 0 to pred( theHeight ) do
    for X := 0 to pred( theWidth ) do
      DoublePix[X,Y] := theValue;
end;

procedure TAMDoubleImage.Convo(Kernel : TAMDoubleImage);
var
  X, Y, X0, Y0 : Integer;
  HH, WW : Integer;
  Temp : TAMDoubleImage;
begin
  Temp := TAMDoubleImage(inherited Create( TAMImageBase(Self) ));
  Initialize;
  try
    HH := pred(Kernel.Height);
    WW := pred(Kernel.Width);
    for X := 0 to pred(Width) do
      for Y := 0 to pred(Height) do
        for X0 := -WW to WW do
          for Y0 := -HH to HH do
            DoublePix[X,Y] := DoublePix[X,Y] +
                              Kernel.DoublePix[X0,Y0]*Temp.DoublePix[X+X0, Y+Y0];
  finally
    Temp.Free;
  end;
end;

function TAMDoubleImage.GetPixel(X, Y : Integer) : Double;
begin
  Result := DoublePix[X,Y];
end;

procedure TAMDoubleImage.Random;
var
  X, Y : Cardinal;
begin
  //inherited Random;
  for X := 0 to pred( Width ) do
    for Y := 0 to pred( Height ) do
      Pixel[X,Y] := System.Random;
end;

procedure TAMDoubleImage.Resize(theWidth, theHeight : Cardinal);
begin
  inherited Resize(theWidth, theHeight);
end;

procedure TAMDoubleImage.Resize;
begin
  inherited Resize;
end;

procedure TAMDoubleImage.SetPixel(X, Y : Integer; AValue : Double);
begin
  DoublePix[X,Y] := AValue;
end;

procedure TAMDoubleImage.Threshold(AValue : Double; var BoolImg : TAMBooleanImage;
  Invert : Boolean);
var
  X, Y : Cardinal;
begin
  if Assigned( BoolImg ) then
    BoolImg.Free;
  BoolImg := TAMBooleanImage.Create( Width, Height, False );
  for X := 0 to pred( Width ) do
    for Y := 0 to pred( Height ) do
      BoolImg[X,Y] := (Pixel[X,Y] < AValue) <> Invert;
end;

end.


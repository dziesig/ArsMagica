unit AMHistogramUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TAMHistogram }

  TAMHistogram = class
  private
    Values : array of Integer;
    fMin, fMax : Integer;
    function GetPeakCount : Integer;
    function GetValue( Idx : Integer ) : Integer;

  public
    constructor Create( theMin, theMax : Integer );
    destructor  Destroy; override;

    procedure Add( AValue : Integer );
    procedure Add( AValue, Scale : Double );

    procedure Reset;

    property MinVal : integer read fMin;
    property MaxVal : Integer read fMax;
    property PeakCount : Integer read GetPeakCount;
    property Value[ Idx : Integer ] : Integer read GetValue; default;
  end;

implementation

uses
  Math;

{ TAMHistogram }

constructor TAMHistogram.Create(theMin, theMax : Integer);
begin
  fMIn := theMin;
  fMax := theMax;
  if fMin >= theMax then
    raise Exception.Create('Creating zero-length Histogram');
  //SetLength( Values, fMax - fMin );
  Reset;
end;

procedure TAMHistogram.Add(AValue, Scale : Double);
var
  theVal : Integer;
begin
  theVal := Round( aValue*Scale );
  Add( theVal );
end;

procedure TAMHistogram.Add(AValue : Integer);
var
  Idx : Integer;
begin
  Idx := AValue - fMin;
  Idx := Min( Idx, fMax );
  Values[Idx] := Values[Idx] + 1;
end;

destructor TAMHistogram.Destroy;
begin
  SetLength( Values, 0 );
  inherited;
end;

function TAMHistogram.GetPeakCount : Integer;
var
  I : Integer;
begin
  Result := 0;
  for I := MinVal to MaxVal do
    Result := Max( Result, Values[I] );
end;

function TAMHistogram.GetValue( Idx : Integer ) : Integer;
begin
  Result := Values[Idx - fMin];
end;

procedure TAMHistogram.Reset;
var
  I : Integer;
begin
  SetLength( Values, 0 );
  SetLength( Values, fMax - fMin );
  for I := 0 to pred(fMax - fMin ) do
    Values[I] := 0;
end;

end.


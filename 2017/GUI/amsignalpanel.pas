unit AMSignalPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls;

type

  TLamps = ( spiRed, spiYellow, spiGreen, spiOff );

  { TAMSignalPanel }

  TAMSignalPanel = class(TCustomPanel)
  private
    fAspect : TLamps;
    fLampBorder : Integer;
    fOnClick : TNotifyEvent;
    fReadOnly : Boolean;
    function GetLampLeft : Integer;
    function GetLampSize : Integer;
    function GetLampTop : Integer;
    function GetLampVSpacing : Integer;
    procedure SetAspect(AValue : TLamps);
    procedure SetLampBorder(AValue : Integer);
    procedure SetReadOnly(AValue : Boolean);

  protected
    Lamps : array [TLamps] of TShape;
    Resizing : Boolean;
    procedure LampsAllOff;
    procedure LampOn( Idx : TLamps );
    procedure InitLamp( Idx : TLamps );
    procedure IniTLamps;
    procedure Paint; override;
    procedure Resize; override;
    procedure ResizeAndPositionLamps;
    procedure LampClick( Sender : TObject );
  public
    constructor Create( TheOwner : TComponent ); override;
    destructor  Destroy; override;
    property LampSize     : Integer read GetLampSize;
    property LampLeft     : Integer read GetLampLeft;
    property LampTop      : Integer read GetLampTop;
    property LampVSpacing : Integer read GetLampVSpacing;
  published
    property Aspect       : TLamps  read fAspect     write SetAspect default spiRed;
    property LampBorder   : Integer read fLampBorder write SetLampBorder default 3;
    property ReadOnly     : Boolean read fReadOnly   write SetReadOnly default False;

    property OnClick      : TNotifyEvent  read fOnClick    write fOnClick;
  end;

procedure Register;

implementation

uses
  AMDebug, lazlogger,
  Math;

procedure Register;
begin
  {$I amsignalpanel_icon.lrs}
  RegisterComponents('Ars Magica',[TAMSignalPanel]);
end;

{ TAMSignalPanel }

constructor TAMSignalPanel.Create(TheOwner : TComponent);
begin
  fOnClick := nil;
  inherited Create(TheOwner);
  Resizing := False;
  Height := 184;
  Width  := 60;
  fLampBorder := 3;
  InitLamps;
  Aspect := spiRed;
  Refresh;
end;

destructor TAMSignalPanel.Destroy;
var
  I : TLamps;
begin
  for I := spiRed to spiOff do
    Lamps[I].Free;
  inherited Destroy;
end;

function TAMSignalPanel.GetLampLeft : Integer;
begin
  Result := (Width div 5);
end;

function TAMSignalPanel.GetLampSize : Integer;
begin
  Result := (Width div 5) * 3;
end;

function TAMSignalPanel.GetLampTop : Integer;
begin
  Result := (Width div 5);
end;

function TAMSignalPanel.GetLampVSpacing : Integer;
begin
  Result := LampSize + LampTop;
end;

procedure TAMSignalPanel.InitLamp(Idx : TLamps);
var
  aColor : TColor;
begin
  Lamps[Idx] := TShape.Create( Self );
  Lamps[Idx].Shape := stCircle;
  Lamps[Idx].Pen.Color := clBlack;
  Lamps[Idx].Pen.Width := Max(1,fLampBorder);
  Lamps[Idx].Parent := Self;
  Lamps[Idx].SetSubcomponent( true );
  Lamps[Idx].Tag := ord(Idx);
  Lamps[Idx].OnClick := @LampClick;
  case Idx of
    spiRed    : aColor := clRed;
    spiYellow : aColor := clYellow;
    spiGreen  : aColor := clLime;
    spiOff    : aColor := clSilver;
  end;
  Lamps[Idx].Brush.Color := aColor;
end;

procedure TAMSignalPanel.IniTLamps;
var
  Idx : TLamps;
begin
  for Idx := spiRed to spiOff do
    InitLamp(Idx);
  ResizeAndPositionLamps;
end;

procedure TAMSignalPanel.LampClick(Sender : TObject);
begin
  if fReadOnly then exit;
  Aspect := TLamps(TShape(Sender).Tag);
  if Assigned( @fonClick ) then
    fonClick( Self );
end;

procedure TAMSignalPanel.LampOn(Idx : TLamps);
begin
  Lamps[Idx].Pen.Color := Lamps[Idx].Brush.Color;
end;

procedure TAMSignalPanel.LampsAllOff;
var
  Idx : TLamps;
begin
  for Idx := spiRed to spiOff do
    Lamps[Idx].Pen.Color := clBlack;
end;

procedure TAMSignalPanel.Paint;
var
  H, W : Integer;
begin
  H := LampTop + 4 * LampVSpacing;
  W := Width;
  Height := H;
  with Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := clGray;
      Canvas.FillRect(0,0,pred(W),pred(H));
      Lamps[spiRed].Paint;
      Lamps[spiYellow].Paint;
      Lamps[spiGreen].Paint;
      Lamps[spiOff].Paint;
    end;
end;

procedure TAMSignalPanel.Resize;
var
  H, W : Integer;
begin
  if not assigned(Lamps[spiRed]) then exit;
  Resizing := True;
  ResizeAndPositionLamps;
  Refresh;
end;

procedure TAMSignalPanel.ResizeAndPositionLamps;
var
  Idx : TLamps;
begin
  for Idx := spiRed to spiOff do
    begin
      Lamps[Idx].Left := LampLeft;
      Lamps[Idx].Top := LampTop + ord(Idx) * LampVSpacing;
      Lamps[Idx].Height := LampSize;
      Lamps[Idx].Width := LampSize;
    end;
end;

procedure TAMSignalPanel.SetAspect(AValue : TLamps);
begin
  if fAspect = AValue then Exit;
  fAspect := AValue;
  LampsAllOff;
  LampOn( fAspect );
end;

procedure TAMSignalPanel.SetLampBorder(AValue : Integer);
begin
  if fLampBorder = AValue then Exit;
  fLampBorder := AValue;
  IniTLamps;
  Refresh;
end;

procedure TAMSignalPanel.SetReadOnly(AValue : Boolean);
begin
  if fReadOnly = AValue then Exit;
  fReadOnly := AValue;
end;

end.

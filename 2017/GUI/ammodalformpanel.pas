unit AMModalFormPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Messages;

type

  { TAMModalFormPanel }

  TAMModalFormPanel = class(TCustomPanel)
  private
    fForm : TForm;
    fOwner : TForm;
    fTrimHeight : Integer;
    fTrimLeft : Integer;
    fTrimTop : Integer;
    fTrimWidth : Integer;
    procedure SetForm(AValue : TForm);

  protected
    //procedure WMMoving(var Msg: TMessage); message 3;//WM_WINDOWPOSCHANGING;
    //procedure WMSizing(var Msg: TMessage); message WM_SIZE;
    procedure OnChangeBounds( Sender : TObject );
  public
    constructor Create( aOwner : TComponent ); override;
    property Form      : TForm{Panel} read fForm write SetForm;
    property Owner     : TForm read fOwner;
  published

    property Align;
    property Alignment;
    Property Anchors;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property ParentColor;
    property TrimTop    : Integer read fTrimTop    write fTrimTop default -40;
    property TrimLeft   : Integer read fTrimLeft   write fTrimLeft;
    property TrimHeight : Integer read fTrimHeight write fTrimHeight;
    property TrimWidth  : Integer read fTrimWidth  write fTrimWidth;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I ammodalformpanel_icon.lrs}
  RegisterComponents('Ars Magica',[TAMModalFormPanel]);
end;

{ TAMModalFormPanel }

const
  FormHeaderHeight = 40; // LOOK HERE there should be some way to get this

constructor TAMModalFormPanel.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  fOwner := TForm(aOwner);
  fOwner.OnChangeBounds := @OnChangeBounds;
end;

procedure TAMModalFormPanel.OnChangeBounds(Sender : TObject);
var
  P : TPoint;
begin
  if not Assigned( fForm ) then exit;
  P.SetLocation( Owner.Left + Left, Owner.Top + Top);
  fForm.Top := P.Y + fTrimTop;
  fForm.Left := P.X + fTrimLeft;
  fForm.ClientHeight := ClientHeight + fTrimHeight;;
  fForm.Width := Width + fTrimWidth;
end;

procedure TAMModalFormPanel.SetForm(AValue : TForm);
begin
  if Assigned( fForm ) then
    begin
      fForm.ModalResult := mrCancel;
      fForm.Close;
    end;
  fForm:=AValue;
  if not Assigned( fForm ) then exit;
  OnChangeBounds( nil );
  fForm.ShowModal;
end;

end.

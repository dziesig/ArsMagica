unit AMFormPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type
  TFormPanelCentering = (fpcNone, fpcHoriz, fpcVert, fpcBoth);

  { TCustomFormPanel }

  TCustomFormPanel = class(TCustomPanel)
  private
    fCentering: TFormPanelCentering;
    fForm: TForm; // TFormPanel;
    procedure SetCentering(AValue: TFormPanelCentering);
    procedure SetForm(AValue: TForm{Panel});
    { Private declarations }
  protected
    { Protected declarations }
    vLeft : Integer;
    vRight : Integer;
    vTop  : Integer;
    vBottom : Integer;
    procedure CenteringNone;
    procedure CenteringVert;
    procedure CenteringHoriz;
  public
    { Public declarations }
    constructor Create( aOwner : TComponent ); override;
    property Form      : TForm{Panel} read fForm write SetForm;
  published
    { Published declarations }
    property Centering : TFormPanelCentering read fCentering write SetCentering;
  end;

  TAMFormPanel = class(TCustomFormPanel)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
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
    property Visible;
  end;

procedure Register;

implementation

uses
  LazLogger,
  Math;

procedure Register;
begin
  RegisterComponents('Ars Magica',[TAMFormPanel]);
end;

{ TAMFormPanel }

constructor TCustomFormPanel.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fCentering := fpcBoth;
end;

procedure TCustomFormPanel.CenteringHoriz;
var
  W : Integer;
begin
  w := VRight + 2*VLeft;
  VLeft := (Width - W) div 2;
end;

procedure TCustomFormPanel.CenteringNone;
begin
  vLeft := 0;
  vTop  := 0;
end;

procedure TCustomFormPanel.CenteringVert;
var
  H : Integer;
begin
  H := VBottom + vTop;
  vTop := (Height - H) div 2;
end;

procedure TCustomFormPanel.SetCentering(AValue: TFormPanelCentering);
begin
  if fCentering=AValue then Exit;
  fCentering:=AValue;
end;

procedure TCustomFormPanel.SetForm(AValue: TForm);
  procedure RestoreSourceControls;
  var
    I : Integer;
    Control : TControl;
    Component : TComponent;
  begin
    if Assigned( fForm ) then
      begin
        fForm.Hide;
        for I := pred(fForm.ComponentCount) downto 0 do
          begin
            Component := fForm. Components[I];
            if Component is TControl then
              begin
                Control := Component as TControl;
                if Control.Parent = Self then
                  begin
                    Control.Parent := FForm;
                    Control.Left := Control.Left - vLeft;
                    Control.Top  := Control.Top  - vTop;
                  end;
              end;
          end;
      end;
  end;
  procedure GetCenteringData;
  var
    I : Integer;
    Control : TControl;
    Component : TComponent;
    T, B, R, L : Integer; // For diagnostics
  begin
    vLeft := MaxInt;
    vTop  := MaxInt;
    vRight := -MaxInt;
    vBottom := -MaxInt;
    for I := 0 to pred(fForm.ComponentCount) do
      begin
        Component := fForm.Components[I];
        if Component is TControl then
          begin
            Control := Component as TControl;
            if Control.Parent = fForm then
              begin
                T := Control.Top;
                B := Control.Top + Control.Height;
                L := Control.Left;
                R := Control.Left + Control.Width;
                vBottom := Max( vBottom, Control.Top + Control.Height );
                vRight  := Max( vRight,  Control.Left + Control.Width );
                vLeft   := Min( vLeft,   Control.Left );
                vTop    := Min( vTop,    Control.Top );
              end;
          end;
      end;
    case fCentering of
      fpcNone  : CenteringNone;
      fpcHoriz : CenteringHoriz;
      fpcVert  : CenteringVert;
      fpcBoth :
        begin
          CenteringHoriz;
          CenteringVert;
        end;
    end;
  end;
  procedure LoadSourceControls;
  var
    I : Integer;
    Component : TComponent;
    Control : TControl;
  begin
    for I := 0 to pred(fForm.ComponentCount) do
      begin
        Component := fForm.Components[I];
        if (Component is TControl)then
          begin
            Control := Component as TControl;
            if Control.Parent = fForm then
              begin
                Control.Parent := Self;
                Control.Left := Control.Left + vLeft;
                Control.Top  := Control.Top  + vTop;
              end
          end;
      end;
    //fForm.Show;

  end;
var
  I : Integer;
  Event : TNotifyEvent;
  C : TColor; // For debug
begin
  if fForm = AValue then
    exit;
  Visible := False;
  if Assigned( fForm ) then
    begin
      Event := fForm.OnHide;
      if Assigned( Event ) then
        Event( Self );
    end;
  try
    RestoreSourceControls;
    fForm := AValue;
    if Assigned( fForm ) then
      begin
        GetCenteringData;
        LoadSourceControls;
        C := fForm.Color;
        Color := fForm.Color;
      end;
  finally
    Visible := True;
    if Assigned( fForm ) then
      begin
        Event := fForm.OnShow;
        if Assigned( Event ) then
          Event( Self );
      end;
  end;
end;

end.

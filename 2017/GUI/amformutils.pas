unit AMFormUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

procedure OnCreate( aForm : TForm );

procedure OnDestroy( aForm : TForm );

implementation

uses
  AMAppUtils, AMDotIni;

procedure OnCreate(aForm : TForm);
var
  Section : String;
  T, L, H, W : Integer;
begin
  Section := 'Form-' + aForm.Name;
  T    := GetConfig( Section, 'Top', aForm.Top );
  L   := GetConfig( Section, 'Left', aForm.Left );
  H := GetConfig( Section, 'Height', aForm.Height );
  W  := GetConfig( Section, 'Width', aForm.Width );
  aForm.SetBounds( L, T, W, H );
end;

procedure OnDestroy(aForm : TForm);
var
  Section : String;
begin
  Section := 'Form-' + aForm.Name;
  SetConfig( Section, 'Top', aForm.Top );
  SetConfig( Section, 'Left', aForm.Left );
  SetConfig( Section, 'Height', aForm.Height );
  SetConfig( Section, 'Width', aForm.Width );
end;

end.


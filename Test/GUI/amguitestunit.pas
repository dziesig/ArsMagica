unit AMGUITestUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  AMFormPanelunit, AMFormPanel, AMAboutFormUnit;

type

  { TForm1 }

  TForm1 = class(TForm)
    AMFormPanel1: TAMFormPanel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses
  Page1, Page2, Page3;

{$R *.lfm}

{ TForm1 }

procedure TForm1.MenuItem6Click(Sender: TObject);
begin
  AMFormPanel1.Form := nil;
  Close;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  AMFormPanel1.Form := Form2;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  AMFormPanel1.Form := Form3;
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  AMFormPanel1.Form := Form4;
end;

end.


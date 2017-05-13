unit AMGuiTestFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  AMFormPanel, AMHelpAboutUnit;

type

  { TAMGuiTestForm }

  TAMGuiTestForm = class(TForm)
    AMFormPanel1: TAMFormPanel;
    AMHelpAbout1: TAMHelpAbout;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
  private

  public

  end;

var
  AMGuiTestForm: TAMGuiTestForm;

implementation

uses
  Page1, Page2, Page3;

{$R *.lfm}

{ TAMGuiTestForm }

procedure TAMGuiTestForm.MenuItem2Click(Sender: TObject);
begin
  AMFormPanel1.Form := Page1Form;
end;

procedure TAMGuiTestForm.MenuItem3Click(Sender: TObject);
begin
  AMFormPanel1.Form := Page2Form;
end;

procedure TAMGuiTestForm.MenuItem4Click(Sender: TObject);
begin
  AMFormPanel1.Form := Page3Form;
end;

procedure TAMGuiTestForm.MenuItem6Click(Sender: TObject);
begin
  AMFormPanel1.Form := nil;
  Close;
end;

procedure TAMGuiTestForm.MenuItem8Click(Sender: TObject);
begin
  AMHelpAbout1.Execute;
end;

end.


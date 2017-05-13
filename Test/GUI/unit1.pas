unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, AMFormPanel,
  AMHelpAboutUnit;

type

  { TForm1 }

  TForm1 = class(TForm)
    AMFormPanel1: TAMFormPanel;
    AMHelpAbout1: TAMHelpAbout;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.


unit themainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, 
    AMDataAppMainForm;

type

  { TMainForm1 }

  TMainForm1 = class(TMainForm)
  private
  protected
    function GetAppName: String; override;

  public

  end;

var
  MainForm1: TMainForm1;

implementation

{$R *.lfm}

{ TMainForm1 }

function TMainForm1.GetAppName: String;
begin
  Result := 'StandardApp';
end;

end.


unit AMMessages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function SaveModified( Doing, What : String ) : Integer;  overload;
function SaveModified( What : String ) : Integer; overload;
function ReallyDelete( Kind, What : String ) : Boolean;


implementation

uses
  Controls, Dialogs;

function SaveModified(Doing, What: String): Integer;
begin
  Result := MessageDlg( What + ' has been modified.'#13#10 +
                     'Do you want to save it before ' + Doing + ' a new one?',
                     mtConfirmation, [mbYes, mbNo, mbCancel], 0);
end;

function SaveModified(What: String): Integer;
begin
  Result := MessageDlg( What + ' has been modified.'#13#10 +
                     'Do you want to save it before exiting the program?',
                     mtConfirmation, [mbYes, mbNo, mbCancel], 0);
end;

function ReallyDelete(Kind, What: String): Boolean;
var
  Ans : Integer;
begin
  Ans := MessageDlg( Format('Do you really want to delete %s %s?',[Kind,What]),
                     mtConfirmation, [mbYes, mbNo], 0);
  Result := Ans = mrYes;
end;

end.


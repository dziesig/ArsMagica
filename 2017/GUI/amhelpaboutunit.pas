unit AMHelpAboutUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  AMAboutFormUnit;

type

  { TAMHelpAbout }

  TAMHelpAbout = class(TComponent)
    private
      fAppName: String;
      fCopyrightBy: String;
      fCopyrightYears: String;
      TheForm : TAMAboutForm;
    protected

    public
      { Public declarations }
      constructor Create( AOwner : TComponent );  override;
      destructor  Destroy; override;

      function  Execute : Boolean;

    published
      property AppName        : String read fAppName        write fAppName;
      property CopyrightYears : String read fCopyrightYears write fCopyrightYears;
      property CopyrightBy    : String read fCopyrightBy    write fCopyrightBy;

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I amhelpaboutUnit_icon.lrs}
  RegisterComponents('Ars Magica',[TAMHelpAbout]);
end;

{ TAMHelpAbout }

constructor TAMHelpAbout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TAMHelpAbout.Destroy;
begin
  inherited Destroy;
end;

function TAMHelpAbout.Execute: Boolean;
begin
  Result := True; // Only thing that can happen is Close the form.
  TheForm := TAMAboutForm.Create( Self );
  try
    TheForm.AppName        := fAppName;
    TheForm.CopyrightYears := fCopyrightYears;
    TheForm.CopyrightBy    := fCopyrightBy;
    TheForm.ShowModal;
  finally
    TheForm.Free;
  end;
end;

end.

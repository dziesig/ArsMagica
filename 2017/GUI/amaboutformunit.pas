unit AMAboutFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { TAMAboutForm }

  TAMAboutForm = class(TForm)
    BitBtn1: TBitBtn;
    ConfigFolderLabel: TLabel;
    CopyrightByLabel: TLabel;
    CopyrightYearsLabel: TLabel;
    DefaultDataFolderLabel: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LastBuildLabel: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    fAppName: String;
    fCopyrightBy: String;
    fCopyrightYears: String;
    procedure SetAppName(AValue: String);
    procedure SetCopyrightBy(AValue: String);
    procedure SetCopyrightYears(AValue: String);

  public
    function ShowModal : Integer; override;

    property AppName        : String read fAppName        write SetAppName;
    property CopyrightYears : String read fCopyrightYears write SetCopyrightYears;
    property CopyrightBy    : String read fCopyrightBy    write SetCopyrightBy;
  end;

var
  AMAboutForm: TAMAboutForm;

implementation

uses
  AMAppUtils;

{$R *.lfm}

{ TAMAboutForm }

procedure TAMAboutForm.FormCreate(Sender: TObject);
begin
  ConfigFolderLabel.Caption      := GetAppConfigFile( False, True );
  LastBuildLabel.Caption         := BuildDateTime;
  DefaultDataFolderLabel.Caption := DefaultSaveLocation;
end;

procedure TAMAboutForm.SetCopyrightBy(AValue: String);
begin
  if fCopyrightBy=AValue then Exit;
  fCopyrightBy:=AValue;
end;

procedure TAMAboutForm.SetAppName(AValue: String);
begin
  if fAppName=AValue then Exit;
  fAppName:=AValue;
end;

procedure TAMAboutForm.SetCopyrightYears(AValue: String);
begin
  if fCopyrightYears=AValue then Exit;
  fCopyrightYears:=AValue;
end;

function TAMAboutForm.ShowModal: Integer;
begin
  Caption                        := 'About ' + fAppName;
  CopyrightYearsLabel.Caption    := fCopyrightYears;
  CopyrightByLabel.Caption       := fCopyrightBy;
  Result:=inherited ShowModal;
end;

end.


unit AMDataAppMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, ActnList, StdCtrls, ExtCtrls, AMFormPanel, AMHelpAboutUnit, AMDataUnit;

type

  { TMainForm }

  TMainForm = class(TForm)
    ActionList1: TActionList;
    AMFormPanel1: TAMFormPanel;
    AMHelpAbout1: TAMHelpAbout;
    FileExitAction: TAction;
    FileNewAction: TAction;
    FileOpenAction: TAction;
    FilePreferencesAction: TAction;
    FilePrintAction: TAction;
    FileSaveAction: TAction;
    FileSaveAsAction: TAction;
    HelpAboutAction: TAction;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    HackTimer: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FileExitActionExecute(Sender: TObject);
    procedure FileNewActionExecute(Sender: TObject);
    procedure FileOpenActionExecute(Sender: TObject);
    procedure FilePreferencesActionExecute(Sender: TObject);
    procedure FilePrintActionExecute(Sender: TObject);
    procedure FileSaveActionExecute(Sender: TObject);
    procedure FileSaveAsActionExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HackTimerTimer(Sender: TObject);
    procedure HelpAboutActionExecute(Sender: TObject);
  private
    fFilePath  : String;
    fData      : TAMData;
    procedure SetFilePath(AValue: String);
    function  FileName : String;
    function  FileOk : Boolean;

    procedure ModifiedChanged( Sender : TObject; var Value : Boolean );

    procedure ControlsToData;
    procedure DataToControls;

    procedure SavePositionAndSize;

  protected
    function GetAppName: String; virtual; abstract;
  public
    property FilePath : String read fFilePath write SetFilePath;
    property Data     : TAMData read fData;
    property AppName  : String read GetAppName;
  end;

var
  MainForm: TMainForm;

implementation

uses
  AMAppUtils, AMMessages, AMDotIni;

{$R *.lfm}

const
  NoFile = 'NONAME';

{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);
begin
  Data.DoSetModified( True );
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  Data.DoSetModified( False );
end;

procedure TMainForm.ControlsToData;
begin
  ;
end;

procedure TMainForm.DataToControls;
begin
  ;
end;

procedure TMainForm.FileExitActionExecute(Sender: TObject);
begin
  Close;
end;

function TMainForm.FileName: String;
begin
  Result := ExtractFileName( fFilePath );
end;

procedure TMainForm.FileNewActionExecute(Sender: TObject);
var
  Answer : Integer;
begin
  if Data.Modified then
    begin
      Answer := SaveModified( 'Creating', FileName );
      case Answer of
        mrYes:
          begin
            ControlsToData;
            FileSaveActionExecute( Sender );
            Data.New;
            DataToControls;
            FilePath := NoFile;
          end;
        mrNo:
          begin
            Data.New;
            DataToControls;
            FilePath := NoFile;
          end;
      end;
    end
  else
    begin
      Data.New;
      DataToControls;
      FilePath := NoFile;
    end;
end;

function TMainForm.FileOk: Boolean;
begin
  Result := FileName <> NoFile;
end;

procedure TMainForm.FileOpenActionExecute(Sender: TObject);
var
  Answer : Integer;
  procedure DoOpen;
  begin
    OpenDialog1.InitialDir := AMAppUtils.DefaultSaveLocation(appName);
    if OpenDialog1.Execute then
      begin
        if Data.Open( OpenDialog1.FileName ) then
          begin
            DataToControls;
            FilePath := OpenDialog1.FileName;
          end;
      end;
  end;
begin
  if Data.Modified then
    begin
      Answer := SaveModified( 'Opening', FileName );
      case Answer of
        mrYes :
          begin
            ControlsToData;
            Data.Save( FilePath );
            DoOpen;
          end;
        mrNo: DoOpen;
        mrCancel : exit;
      end

    end
  else
    DoOpen;
end;

procedure TMainForm.FilePreferencesActionExecute(Sender: TObject);
begin
  ;
end;

procedure TMainForm.FilePrintActionExecute(Sender: TObject);
begin
  ;
end;

procedure TMainForm.FileSaveActionExecute(Sender: TObject);
begin
  if FileOk then
    begin
      ControlsToData;
      Data.Save( FilePath )
    end
  else
    FileSaveAsActionExecute( Sender );
end;

procedure TMainForm.FileSaveAsActionExecute(Sender: TObject);
begin
  SaveDialog1.InitialDir := AMAppUtils.DefaultSaveLocation(appName);
  if SaveDialog1.Execute then
    begin
      if Data.Save( SaveDialog1.FileName ) then
        begin
          FilePath := SaveDialog1.FileName;
          ControlsToData;
          Data.Save( FilePath );
        end;
    end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SavePositionAndSize;
  CloseAction := caFree;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Answer : Integer;
begin
  if Data.Modified then
    begin
      Answer := SaveModified(FileName);
      case Answer of
        mrYes:
          begin
            FileSaveActionExecute( Sender );
            CanClose := True;
          end;
        mrNo: CanClose := True;
        mrCancel : CanClose := False;
      end;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  TheTop, TheLeft, TheHeight, TheWidth : Integer;
begin
  FilePath := NoFile;
  fData   := TAMData.Create;
  fData.OnModified := @ModifiedChanged;
  TheTop := GetConfig('Size-Position','Top',Top);
  TheLeft := GetConfig('Size-Position','Left',Left);
  TheHeight := GetConfig('Size-Position','Height',Height);
  TheWidth := GetConfig('Size-Position','Width',Width);
  Top := TheTop;
  Left := TheLeft;
  Height := TheHeight;
  Width := TheWidth;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  SavePositionAndSize;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  // This works around a problem with StatusBar Height on Startup
  // Took 3 hours to figure out why nothing was showing up on the StatusBar
  HackTimer.Enabled := True;
end;


procedure TMainForm.HackTimerTimer(Sender: TObject);
begin
  // This works around a problem with StatusBar Height on Startup
  // Took 3 hours to figure out why nothing was showing up on the StatusBar
  StatusBar1.Height := 50;
  HackTimer.Enabled := False;
end;

procedure TMainForm.HelpAboutActionExecute(Sender: TObject);
begin
  AMHelpAbout1.Execute;
end;

procedure TMainForm.ModifiedChanged(Sender: TObject; var Value: Boolean);
begin
  if Value then
    StatusBar1.Panels[1].Text := '*' + ExtractFileName( fFilePath )
  else
      StatusBar1.Panels[1].Text := ExtractFileName( fFilePath );
end;

procedure TMainForm.SavePositionAndSize;
begin
  SetConfig('Size-Position','Top',Top);
  SetConfig('Size-Position','Left',Left);
  SetConfig('Size-Position','Height',Height);
  SetConfig('Size-Position','Width',Width);
end;

procedure TMainForm.SetFilePath(AValue: String);
begin
  if fFilePath=AValue then Exit;
  fFilePath:=AValue;
  StatusBar1.Panels[1].Text := ExtractFileName( fFilePath );
end;

end.


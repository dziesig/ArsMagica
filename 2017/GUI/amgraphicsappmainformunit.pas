unit AMGraphicsAppMainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, ActnList, StdCtrls, ExtCtrls, AMHelpAboutUnit, AMDataUnit;

type

  { TMainForm }

  TMainForm = class(TForm)
    ActionList1: TActionList;
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
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HackTimerTimer(Sender: TObject);
    procedure HelpAboutActionExecute(Sender: TObject);
  private
    fFilePath  : String;
    //fData      : TAMData;
    procedure SetFilePath(AValue: String);
    function  FileName : String;
    function  FileOk : Boolean;

    //procedure ModifiedChanged( Sender : TObject; var Value : Boolean );
    procedure OnModify( Sender : TObject );
    procedure OnUnModify( Sender : TObject );

  protected
    function GetData: TAMData;  virtual; abstract;
    function  GetApplicationName: String; virtual;
    function  OnCreate : TAMData ; virtual;
    procedure ControlsToData; virtual;
    procedure DataToControls; virtual;
    function BeforeNew : Boolean; virtual;
    procedure AfterNew; virtual;
  public

    procedure EditPreferences; virtual;
    procedure DoPrint; virtual;
 
    property AppName  : String read GetApplicationName;
    property FilePath : String read fFilePath write SetFilePath;
    property Data     : TAMData read GetData;
  end;

var
  MainForm: TMainForm;

implementation

uses
  AMAppUtils, AMMessages, AMDebug;

{$R *.lfm}

const
  NoFile = 'NONAME';

{ TMainForm }

procedure TMainForm.AfterNew;
begin
  ;
end;

function TMainForm.BeforeNew : Boolean;
begin
  Result := False; // Nothing to do here.
end;

procedure TMainForm.ControlsToData;
begin
  //if Assigned( AMFormPanel1.Form ) and Assigned( AMFormPanel1.Form.OnHide ) then
  //  AMFormPanel1.Form.OnHide(nil);
end;

procedure TMainForm.DataToControls;
begin
  //if Assigned( AMFormPanel1.Form ) then
  //  AMFormPanel1.Form.OnShow(nil);
end;

procedure TMainForm.DoPrint;
begin
  // Do Nothing
end;

procedure TMainForm.EditPreferences;
begin
  // Do Nothing
end;

procedure TMainForm.FileExitActionExecute(Sender: TObject);
begin
  Debug('FileExitActionExecute');
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
            if BeforeNew then
              begin
                Data.New;
                AfterNew;
                DataToControls;
                FilePath := NoFile;
              end;
          end;
        mrNo:
          begin
            if BeforeNew then
              begin
                Data.New;
                AfterNew;
                DataToControls;
                FilePath := NoFile;
              end;
          end;
      end;
    end
  else
    begin
      if BeforeNew then
        begin
          Data.New;
          AfterNew;
          DataToControls;
          FilePath := NoFile;
        end;
    end;
end;

function TMainForm.FileOk: Boolean;
begin
  Result := FileName <> NoFile;
end;

procedure TMainForm.FileOpenActionExecute(Sender: TObject);
var
  Answer : Integer;
  tmpData : TAMData;
  procedure DoOpen;
  begin
    OpenDialog1.InitialDir := AMAppUtils.DefaultSaveLocation(appName);
    if OpenDialog1.Execute then
      begin
        //tmpData := Data.Open( OpenDialog1.FileName );
        Data.Open( OpenDialog1.FileName );
        //if Assigned( tmpData ) then
        //  begin
        //    fData.Free;
        //    fData := tmpData;
            DataToControls;
            FilePath := OpenDialog1.FileName;
          //end;
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
  EditPreferences;
end;

procedure TMainForm.FilePrintActionExecute(Sender: TObject);
begin
  DoPrint;
end;

procedure TMainForm.FileSaveActionExecute(Sender: TObject);
begin
  if FileOk then
    begin
      ControlsToData;
      //if Assigned( AMFormPanel1.Form.OnHide ) then
      //  AMFormPanel1.Form.OnHide(nil);
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
          //if Assigned( AMFormPanel1.Form ) and
          //   Assigned( AMFormPanel1.Form.OnHide )  then
          //  AMFormPanel1.Form.OnHide(nil);
          Data.Save( FilePath );
        end;
    end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //AMFormPanel1.Form := nil;
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
  Debug('FormCloseQuery %d',[ord(CanClose)])
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FilePath := NoFile;
  //fData   := OnCreate; //TAMData.Create;
  //"<procedure variable type of procedure(TObject;var Boolean) of object;Register>" expected
  //"<procedure variable type of procedure(TObject) of object;Register>"
  Data.OnModifyEvent := @OnModify;
  Data.OnUnModifyEvent := @OnUnModify;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Data.OnModifyEvent := nil;
  Data.OnUnModifyEvent  := nil;
  //FData.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  // This works around a problem with StatusBar Height on Startup
  // Took 3 hours to figure out why nothing was showing up on the StatusBar
  HackTimer.Enabled := True;
end;

function TMainForm.GetApplicationName: String;
begin
  Result := 'DEFAULT';
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

//procedure TMainForm.ModifiedChanged(Sender: TObject; var Value: Boolean);
//begin
//  if Value then
//    StatusBar1.Panels[1].Text := '*' + ExtractFileName( fFilePath )
//  else
//      StatusBar1.Panels[1].Text := ExtractFileName( fFilePath );
//end;

function TMainForm.OnCreate: TAMData;
begin
  raise Exception.Create('TMainForm.OnCreate called improperly');
  Result := TAMData.Create;
end;

procedure TMainForm.OnModify(Sender: TObject);
begin
  StatusBar1.Panels[1].Text := '*' + ExtractFileName( fFilePath )
end;

procedure TMainForm.OnUnModify(Sender: TObject);
begin
  StatusBar1.Panels[1].Text := ExtractFileName( fFilePath );
end;

procedure TMainForm.SetFilePath(AValue: String);
begin
  if fFilePath=AValue then Exit;
  fFilePath:=AValue;
  StatusBar1.Panels[1].Text := ExtractFileName( fFilePath );
end;

end.


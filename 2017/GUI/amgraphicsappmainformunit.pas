{ AMGraphicsAppMainForm - Parent form for Applications using Graphics.

  Copyright (C) 1995..2017 by Donald R. Ziesig donald@ziesig.org

  This code is derived from the various "MagicLibraryYYYY"s by the same author.
  It has been Refactored to separate non-gui and gui modules.

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.

  NOTE:  Due to some issues with the Lazarus IDE, it is necessary to jump
         through some hoops to use this code.

         The source code is stored with the code for the GUI version of
         ArsMagica.  As of 2017, that path is:

           /home/donz/ArsMagica/2017/GUI/

         The files are:

             amgraphicsappmainformunit.pas, and
             amgraphicsappmainformunit.lfm

         When creating a new project, save the original IDE generated
         form as something like tempmainform.pas/lfm.  This will eventually
         be deleted.  Then, in the source code directory for the project,

         ln -s /home/donz/ArsMagica/2017/GUI/amgraphicsappmainformunit.pas amgraphicsappmainformunit.pas
         ln -s /home/donz/ArsMagica/2017/GUI/amgraphicsappmainformunit.lfm amgraphicsappmainformunit.lfm

         In the IDE, open the .pas file, then Project | Add Editor File to project.

         Finally, create the actual main form unit by:

         File | New ... Inherited Item | Inherited Project Component

         Select the amgraphicsappmainformunit.pas file as the source.  Rename
         the newly generatd file as appropriate for the project.

}
unit AMGraphicsAppMainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, ActnList, StdCtrls, ExtCtrls, AMHelpAboutUnit, {AMDataUnit}
  AMTextIO, AMPersists;

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
    procedure FormCreate(Sender: TObject); virtual;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject); virtual;
    procedure HackTimerTimer(Sender: TObject);
    procedure HelpAboutActionExecute(Sender: TObject);
    //procedure OnCreate( Sender : TObject );
  private
    fData : TAMPersists;
    fDataClass : TClass;
    fFilePath  : String;
    fPData : PAMPersists;
    //fData      : TAMData;
    procedure SetFilePath(AValue: String);
    function  FileName : String;
    function  FileOk : Boolean;

    //procedure ModifiedChanged( Sender : TObject; var Value : Boolean );
    procedure OnModify( Sender : TObject );
    procedure OnUnModify( Sender : TObject );

  protected
    function GetData: TAMPersists;  virtual;// abstract;
    function GetDataClass : TClass; virtual; abstract;
    function  GetApplicationName: String; virtual;
    function  OnCreate : TAMPersists ; virtual;
    procedure ControlsToData; virtual;
    procedure DataToControls; virtual;
    function BeforeNew : Boolean; virtual;
    procedure AfterNew; virtual;
    function BeforeOpen : Boolean; virtual;
    procedure AfterOpen; virtual;
    procedure BeforeExit; virtual;// abstract;
    procedure AfterCreate; virtual;
    procedure BeforeDestroy; virtual;
    function  OpenWrite( FilePath : String ) : TTextIO;
    function  OpenRead( FilePath : String ) : TTextIO;
  public

    procedure EditPreferences; virtual;
    procedure DoPrint; virtual;
 
    property AppName  : String read GetApplicationName;
    property FilePath : String read fFilePath write SetFilePath;
    property Data     : TAMPersists read fData write fData;
    property PData    : PAMPersists read fPData write fPData;
    property DataClass : TClass read fDataClass write fDataClass;
  end;

var
  MainForm: TMainForm;

implementation

uses
  AMAppUtils, AMMessages, AMDebug, AMFormUtils, AMObjectFactory,
  CQWorldUnit;

{$R *.lfm}

const
  NoFile = 'NONAME';

{ TMainForm }

procedure TMainForm.AfterCreate;
begin
  ;
end;

procedure TMainForm.AfterNew;
begin
  ;
end;

procedure TMainForm.AfterOpen;
begin
  ;
end;

procedure TMainForm.BeforeDestroy;
begin
  ;
end;

procedure TMainForm.BeforeExit;
begin
  //Debug( 'TMainForm.BeforeExit was abstract' );
end;

function TMainForm.BeforeNew : Boolean;
begin
  Result := True; // Nothing to do here.
end;

function TMainForm.BeforeOpen : Boolean;
begin
  Result := True; // Nothing to do here.
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
  BeforeExit;
  //Debug('FileExitActionExecute');
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
  //if not Assigned( Data ) then exit;
  if Assigned( PData^ ) and PData^.Modified then
    begin
      Answer := SaveModified( 'Creating', FileName );
      case Answer of
        mrYes:
          begin
            ControlsToData;
            FileSaveActionExecute( Sender );
            if BeforeNew then
              begin
                PData^.Free;
                //Data.New;
                AfterNew;
                DataToControls;
                FilePath := NoFile;
              end;
          end;
        mrNo:
          begin
            if BeforeNew then
              begin
                PData^.Free;
               //Data.New;
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
          PData^.Free;
          //Debug( 'Creating new version of %s',[DataClass.ClassName]);
          PData^ := ObjectFactory.MakeObject( DataClass.ClassName );
          PData^.MakeNew;
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
  TextIO : TTextIO;
  DC     : TAMPersists;
  procedure DoOpen;
  begin
    OpenDialog1.InitialDir := AMAppUtils.DefaultSaveLocation(appName);
    if OpenDialog1.Execute then
      begin
        if Assigned( PData^ ) then //FreeAndNil( Data );
          PData^.Free;
        TextIO := TTextIO.Create( OpenDialog1.FileName, False );
        PData^ := ObjectFactory.MakeObject( TextIO, DataClass.ClassName );
        //Debug( PData^.ToString );
        TextIO.Free;
        DataToControls;
        FilePath := OpenDialog1.FileName;
        AfterOpen;
        Invalidate;
      end;
  end;
begin
  if not Assigned(PData^ ) then exit;
  if PData^.Modified then
    begin
      Answer := SaveModified( 'Opening', FileName );
      case Answer of
        mrYes :
          begin
            ControlsToData;
            TextIO := OpenWrite( FilePath );
            Data.Store( TextIO );
            TextIO.Free;
            DoOpen;
          end;
        mrNo: DoOpen;
        mrCancel : exit;
      end

    end
  else
    DoOpen;
  //Debug( 'FileOpenActionExecute' );
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
var
  TextIO : TTextIO;
begin
  if not Assigned( PData^ ) then exit;
  if FileOk then
    begin
      ControlsToData;
      TextIO := OpenWrite( FilePath );
      PData^.Store( TextIO );
      TextIO.Free;
    end
  else
    FileSaveAsActionExecute( Sender );
end;

procedure TMainForm.FileSaveAsActionExecute(Sender: TObject);
var
  TextIO : TTextIO;
begin
  if not Assigned( PData^ ) then exit;
  SaveDialog1.InitialDir := AMAppUtils.DefaultSaveLocation(appName);
  if SaveDialog1.Execute then
    begin
      TextIO := OpenWrite( SaveDialog1.FileName );
      FilePath := SaveDialog1.FileName;
      ControlsToData;
      PData^.Store( TextIO );
      TextIO.Free;
    end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Answer : Integer;
begin
  if not Assigned( Data ) then exit;
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
  //if not Assigned( Data ) then exit;
  //Data.OnModifyEvent := @OnModify;
  //Data.OnUnModifyEvent := @OnUnModify;
  //AfterCreate;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  BeforeDestroy;
  AMFormUtils.OnDestroy( Self );
  if not Assigned( PData^ ) then exit;
    PData^.Free;
  //Data.OnModifyEvent := nil;
  //Data.OnUnModifyEvent  := nil;
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

function TMainForm.GetData : TAMPersists;
begin
  Result := nil;
end;

procedure TMainForm.HackTimerTimer(Sender: TObject);
begin
  // This works around a problem with StatusBar Height on Startup
  // Took 3 hours to figure out why nothing was showing up on the StatusBar
  AMFormUtils.OnCreate( Self );
  //StatusBar1.Height := 50;
  HackTimer.Enabled := False;
end;

procedure TMainForm.HelpAboutActionExecute(Sender: TObject);
begin
  AMHelpAbout1.AppName := AppName;
  AMHelpAbout1.Execute;
end;

//procedure TMainForm.OnCreate(Sender : TObject);
//begin
//  ;
//end;

//procedure TMainForm.ModifiedChanged(Sender: TObject; var Value: Boolean);
//begin
//  if Value then
//    StatusBar1.Panels[1].Text := '*' + ExtractFileName( fFilePath )
//  else
//      StatusBar1.Panels[1].Text := ExtractFileName( fFilePath );
//end;

function TMainForm.OnCreate: TAMPersists;
begin
  raise Exception.Create('TMainForm.OnCreate called improperly');
  //Result := TAMData.Create;
  Result := nil;
end;

procedure TMainForm.OnModify(Sender: TObject);
begin
  StatusBar1.Panels[1].Text := '*' + ExtractFileName( fFilePath )
end;

procedure TMainForm.OnUnModify(Sender: TObject);
begin
  StatusBar1.Panels[1].Text := ExtractFileName( fFilePath );
end;

function TMainForm.OpenRead(FilePath : String) : TTextIO;
begin
  Result := TTextIO.Create( FilePath, False );
end;

function TMainForm.OpenWrite(FilePath : String) : TTextIO;
begin
  Result := TTextIO.Create( FilePath, True );
end;

procedure TMainForm.SetFilePath(AValue: String);
begin
  if fFilePath=AValue then Exit;
  fFilePath:=AValue;
  StatusBar1.Panels[1].Text := ExtractFileName( fFilePath );
end;

end.


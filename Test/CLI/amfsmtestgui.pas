unit amfsmtestgui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, AMStateMachine;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    CurrentStateLabel: TLabel;
    Label2: TLabel;
    CrossingStateLabel: TLabel;
    Label3: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    Timeout: TButton;
    SouthCheckBox: TCheckBox;
    NorthCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure NorthCheckBoxChange(Sender: TObject);
    procedure SouthCheckBoxChange(Sender: TObject);
  private
    fState: String;
    procedure SetState(AValue: String);
  private

    property State : String read fState write SetState;
  public
    FSM : TMealyStateMachine;
  end;

var
  Form1: TForm1;

implementation

uses
  AMTextIO;

{$R *.lfm}


{ TForm1 }



procedure TForm1.FormCreate(Sender: TObject);
begin
  FSM := TMealyStateMachine.Create;
  //FSM.Init( ['Inactive',
  //           'South1',  'South2', 'South3',
  //           'North1',  'North2', 'North3'],
  //          ['SouthOn', 'SouthOff',
  //           'NorthOn', 'NorthOff',
  //           'Timeout'],
  //          'Inactive' );
  //FSM.Add('Inactive','SouthOn',  'South1');
  //FSM.Add('South1',  'NorthOn',  'North2');
  //FSM.Add('North2',  'SouthOff', 'North3');
  //FSM.Add('North3',  'NorthOff', 'Inactive');
  //
  //FSM.Add('Inactive','NorthOn','North1');
  //FSM.Add('North1',  'SouthOn','South2');
  //FSM.Add('South2',  'NorthOff','South3');
  //FSM.Add('South3',  'SouthOff','Inactive');
  //
  ListBox1.Clear;
  ListBox2.Clear;
  if FSM.TheStates <> nil then
    ListBox1.Items := FSM.TheStates;
  if FSM.TheEvents <> nil then
    ListBox2.Items := FSM.TheEvents;
  State := FSM.State;

  Label3.Caption := Format( 'Index Count:  %d, IndexName[0]:  [%s]',
                            [FSM.IndexCount, FSM.IndexName(0)] );
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FSM.Free;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
var
  TextIO : TTextIO;
begin
  if Assigned( FSM ) then
    FreeAndNil( FSM );
  TextIO := TTextIO.Create('/home/donz/Desktop/FSM.txt',False);
  FSM := TMealyStateMachine.Load( TextIO );
  TextIO.Free;
  ListBox1.Items := FSM.TheStates;
  ListBox2.Items := FSM.TheEvents;
  State := FSM.State;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
var
  TextIO : TTextIO;
begin
  TextIO := TTextIO.Create('/home/donz/Desktop/FSM.txt',True);
  FSM.Store( TextIO );
  TextIO.Free;
end;

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.NorthCheckBoxChange(Sender: TObject);
begin
  if NorthCheckbox.Checked then
    State := FSM.Event('NorthOn')
  else
    State := FSM.Event('NorthOff');
end;

procedure TForm1.SetState(AValue: String);
var
  CrossingState : String;
begin
  if AValue = '' then exit;
  if fState=AValue then Exit;
  fState:=AValue;
  CurrentStateLabel.Caption := fState;

  if fState = 'Inactive' then
    CrossingState := 'Inactive'
  else
    CrossingState := 'Active';
  //else if (fState = 'South1') or (fState = 'North1' ) then
  //  CrossingState := 'Active';

  CrossingStateLabel.Caption := CrossingState;
end;

procedure TForm1.SouthCheckBoxChange(Sender: TObject);
begin
  if SouthCheckbox.Checked then
    State := FSM.Event('SouthOn')
  else
    State := FSM.Event('SouthOff');
end;

end.


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
    FSM : TFiniteStateMachine;
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
  FSM := TFiniteStateMachine.Create;

  //FSM.AddEvent(['South On', 'South Off', 'North On', 'North Off', 'Timeout'] );
  //FSM.AddState(['South1', 'South2', 'South3','North1', 'North2', 'North3', 'Inactive']);
  //FSM.SetTransition('Inactive','South On',  'South1');
  //FSM.SetTransition('South1',  'North On',  'North2');
  //FSM.SetTransition('North2',  'South Off', 'North3');
  //FSM.SetTransition('North3',  'North Off', 'Inactive');
  //
  //FSM.SetTransition('South3',  'South Off','Inactive');
  //FSM.SetTransition('Inactive','North On','North1');
  //FSM.SetTransition('North1',  'South On','South2');
  //FSM.SetTransition('South2',  'North Off','South3');
  //FSM.SetStartState('Inactive');

  ListBox1.Clear;
  ListBox2.Clear;
  if FSM.States <> nil then
    ListBox1.Items := FSM.States;
  if FSM.Events <> nil then
    ListBox2.Items := FSM.Events;

  CurrentStateLabel.Caption := FSM.State;
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
  FSM := TFiniteStateMachine.Load( TextIO );
  TextIO.Free;
  ListBox1.Items := FSM.States;
  ListBox2.Items := FSM.Events;
  CurrentStateLabel.Caption := FSM.State;
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
    State := FSM.Event('North On')
  else
    State := FSM.Event('North Off');
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
  CrossingStateLabel.Caption := CrossingState;
end;

procedure TForm1.SouthCheckBoxChange(Sender: TObject);
begin
  if SouthCheckbox.Checked then
    State := FSM.Event('South On')
  else
    State := FSM.Event('South Off');
end;

end.


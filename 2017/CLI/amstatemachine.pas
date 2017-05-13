unit AMStateMachine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FGL, AMPersists, AMTextIO;

type

  TFSMException = Exception;

  TAMStrings = array of String;

  { TMealyStateMachine }

  TMealyStateMachine = class(TAMPersists)
    private
      const
        TheVersion = 1;
        fIndexCount = 1;
      var
        CurrentState : Integer;
        StateList  : TStringList;
        fStartState : String;
      function GetEvents( Idx : Integer ): TStringList;
      function GetState: String;
      function GetTheEvents: TStrings;
      function GetTheStates: TStrings;
      function MakeStateList : TStringList;
      function MakeEventList : TStringList;
    public
      //constructor Create( States, Events : array of String; StartState : String );
      constructor Create( aParent : TAMPersists = nil ); override;
      destructor  Destroy; override;

      procedure Add( State, Event, NewState: String );
      procedure Delete( State : String );

      function Event( TheEvent : String ) : String;

      procedure Init( States, Events : array of String; StartState : String );

      class function Load( TextIO : TTextIO; aParent : TAMPersists = nil ) : TMealyStateMachine;

      procedure Read( TextIO : TTextIO; aVersion : Integer ); override;
      procedure Write( TextIO : TTextIO ); override;

      property StartState : String read fStartState;
      property State : String read GetState;
      property States : TStringList read StateList;
      property Events[ Idx : Integer ] : TStringList read GetEvents;
      property TheStates : TStrings read GetTheStates;
      property TheEvents : TStrings read GetTheEvents;
  end;

implementation

uses
  AMObjectFactory;

{ TMealyStateMachine }

procedure TMealyStateMachine.Init( States, Events: array of String;
                                   StartState : String );
var
  I, J : Integer;
  EventList : TStringList;
begin
  if Assigned( StateList) then
    FreeAndNil( StateList );
  StateList := MakeStateList; // TStringList.Create;
  //StateList.OwnsObjects := True;
  //StateList.Sorted := True;
  //StateList.CaseSensitive := False;
  for I := 0 to pred(Length(States)) do
    begin
      EventList := MakeEventList; //TStringList.Create;
      //EventList.OwnsObjects := False;
      //EventList.Sorted := True;
      //EventList.CaseSensitive := False;
      for J := 0 to pred(Length( Events )) do
        EventList.AddObject( Events[J], TObject(Pointer(-1)) );

      StateList.AddObject( States[I], EventList );
    end;
  if not StateList.Find( StartState, CurrentState ) then
    raise TFSMException.CreateFmt( 'New state [%s] is not in States',
                                   [StartState] );
  fStartState := StartState;
end;

class function TMealyStateMachine.Load(TextIO: TTextIO; aParent: TAMPersists
  ): TMealyStateMachine;
begin
  Result := TMealyStateMachine( TAMPersists.Load( TextIO, aParent ) );
end;

function TMealyStateMachine.MakeEventList: TStringList;
begin
  Result := TStringList.Create;
  Result.OwnsObjects := False;
  Result.Sorted := True;
  Result.CaseSensitive := False;
end;

function TMealyStateMachine.MakeStateList: TStringList;
begin
  Result := TStringList.Create;
  Result.OwnsObjects := True;
  Result.Sorted := True;
  Result.CaseSensitive := False;
end;

procedure TMealyStateMachine.Read(TextIO: TTextIO; aVersion: Integer);
var
  I, J       : Integer;
  StateCount : Integer;
  StateName  : String;
  EventList  : TStringList;
  EventCount : Integer;
  EventName  : String;
  NextState  : Integer;
begin
  //inherited Read(TextIO, aVersion);
  if Assigned( StateList ) then
    FreeAndNil( StateList );
  StateList := MakeStateList; //TStringList.Create;
  if aVersion >= 1 then
    begin
      TextIO.ReadLn( StateCount );
      for I := 0 to pred( StateCount ) do
        begin
          TextIO.ReadLn( StateName );
          EventList := MakeEventList; //TStringList.Create;
          //EventList.OwnsObjects := False;
          //EventList.Sorted := True;
          //EventList.CaseSensitive := False;
          StateList.AddObject( StateName, EventList );
          TextIO.ReadLn( EventCount );
          for J := 0 to pred( EventCount ) do
            begin
              TextIO.ReadLn( EventName );
              TextIO.ReadLn( NextState );
              EventList.AddObject( EventName, TObject( Pointer( NextState) ) );
            end;
        end;
      TextIO.ReadLn( fStartState );
      if not StateList.Find( fStartState, CurrentState ) then
        raise TFSMException.CreateFmt( 'Start state [%s] is not in States',
                                       [fStartState] );
    end;
end;

procedure TMealyStateMachine.Write(TextIO: TTextIO);
var
  I, J : Integer;
  StateCount : Integer;
  EventList  : TStringList;
  EventCount : Integer;
  NextState  : Integer;
begin
  //inherited Write(TextIO);
  StateCount := StateList.Count;
  TextIO.WriteLn( StateList.Count );
  for I := 0 to pred( StateCount ) do
    begin
      TextIO.WriteLn( StateList[I] );
      EventList := TStringList( StateList.Objects[I] );
      EventCount := EventList.Count;
      TextIO.WriteLn( EventCount );
      for J := 0 to pred( EventCount ) do
        begin
          NextState := Integer( Pointer(EventList.Objects[J]) );
          TextIO.WriteLn( EventList[J] );
          TextIO.WriteLn( NextState );
        end;
    end;
  TextIO.WriteLn( fStartState );
end;

constructor TMealyStateMachine.Create(aParent: TAMPersists);
begin
  inherited Create(aParent);
  fVersion := TheVersion;
  StateList := nil;
end;

procedure TMealyStateMachine.Add(State, Event, NewState: String );
var
  I, J, K : Integer;
  EventList : TStringList;
begin
  //StateList.Find( State, I );
  //K := StateList.Find( NewState );
  if not StateList.Find( State, I ) then
    raise TFSMException.CreateFmt( 'Adding Event [%s] to unknown state {%s]',
                                   [Event, State] );
  if not StateList.Find( NewState, K ) then
    raise TFSMException.CreateFmt( 'New State [%s] unknown when adding Event [%s]',
                                   [NewState, Event] );
  EventList := TStringList( StateList.Objects[I] );
  //J := EventList.Find( Event );
  if not EventList.Find( Event, J ) then
    raise TFSMException.CreateFmt( 'Adding unknown event [%s] to state [%s]',
                                   [Event, State] );
  if Integer(Pointer(EventList.Objects[J])) <> -1 then
    raise TFSMException.CreateFmt( 'Duplicate event {%s] for state [%s] [%p]',
                                   [Event, State, Pointer(EventList.Objects[J])] );
  EventList.Objects[J] := TObject(Pointer(K));
end;

procedure TMealyStateMachine.Delete(State: String);
var
  Idx : Integer;
begin
  if StateList.Find( State, Idx ) then
    StateList.Delete( Idx );
end;

destructor TMealyStateMachine.Destroy;
begin
  StateList.Free;
  inherited Destroy;
end;

function TMealyStateMachine.Event( TheEvent : String ): String;
var
  Next : Integer;
  CurrList : TStringList;
  vState : Integer;
begin
  Result := '';
  CurrList := TStringList(StateList.Objects[ CurrentState ] );
  //Next := CurrList.Find( TheEvent );
  if not CurrList.Find( TheEvent, Next ) then
    raise TFSMException.CreateFmt( 'Event [%s] does not exist.',[ TheEvent ] );
  vState := Integer( Pointer( TStringList( CurrList.Objects[ Next ] )));
  if vState >= 0 then
    begin
      CurrentState := vState;
      Result := StateList[ CurrentState ];
    end;

end;

function TMealyStateMachine.GetEvents( Idx : Integer ): TStringList;
begin
  Result := StateList.Objects[Idx] as TStringList;
end;

function TMealyStateMachine.GetState: String;
begin
  if Assigned( StateList ) then
    Result := StateList[ CurrentState ]
  else
    Result := '';
end;

function TMealyStateMachine.GetTheEvents: TStrings;
begin
  if Assigned( StateList ) and Assigned( StateList.Objects[0] ) then
    Result := TStrings(StateList.Objects[0])
  else
    Result := nil;
end;

function TMealyStateMachine.GetTheStates: TStrings;
begin
  if Assigned( StateList ) then
    Result := StateList
  else
    Result := nil;
end;

initialization
  ObjectFactory.RegisterClass( TMealyStateMachine );

end.


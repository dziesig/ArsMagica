{ AMStateMachine.  A Streamable Finite State Machine

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
}
unit AMStateMachine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AMPersists, AMTextIO, AMCompSci;

type

  TFSMException = Exception;

  //TAMStrings = array of String;
  TStateString = String[15];

  TNextStates = specialize T2dArray<TStateString>;

  { TFiniteStateMachine }

  TFiniteStateMachine = class(TAMPersists)
    private
      fStartState: String;
      const
        TheVersion = 1;
        fIndexCount = 1;
      var
        fCurrentState : Integer;
        fStateList    : TStringList;
        fEventList    : TStringList;
        fNextStates   : TNextStates;
        fInitialState : String;
      //function GetEvents( Idx : Integer ): TStringList;
      //function GetState: String;
      //function GetTheEvents: TStrings;
      //function GetTheStates: TStrings;
      //function MakeStateList : TStringList;
      //function MakeEventList : TStringList;
      function GetNextStates( EventIdx, StateIdx : Integer ) : String;
      function GetState : String;
      procedure SetNextStates( EventIdx, StateIdx : Integer ; AValue : String);
    public
      constructor Create( aParent : TAMPersists = nil ); override;
      destructor  Destroy; override;

      procedure AddEvent( TheEvents : array of String ); overload;
      procedure AddEvent( TheEvent : String ); overload;

      procedure AddState( TheStates : array of String ); overload;
      procedure AddState( TheState  : String ); overload;

      procedure DelState( TheState : String );
      procedure DelEvent( TheEvent : String );

      function  Event( TheEvent : String ) : String;

      procedure SetTransition( FromState, ByEvent, ToState: String );

      class function Load( TextIO : TTextIO; aParent : TAMPersists = nil ) : TFiniteStateMachine;

      procedure MakeNew; override;
      procedure Read( TextIO : TTextIO; aVersion : Integer ); override;
      procedure Write( TextIO : TTextIO ); override;

      procedure RenameEvent( OldValue, NewValue : String );
      procedure RenameState( OldValue, NewValue : String );

      procedure SetStartState( aValue : String );

      property StartState : String read fInitialState;
      property State : String read GetState;
      property States : TStringList read fStateList;
      property Events : TStringList read fEventList;
      property NextStates[ EventIdx, StateIdx : Integer ] : String
                          read GetNextStates write SetNextStates;
  end;

implementation

uses
  AMObjectFactory, AMStrings;

{ TFiniteStateMachine }

constructor TFiniteStateMachine.Create(aParent: TAMPersists);
begin
  inherited Create(aParent);
  fVersion := TheVersion;
end;

procedure TFiniteStateMachine.AddEvent(TheEvents : array of String);
var
  I : Integer;
begin
  for I := Low(TheEvents) to High(TheEvents) do
    AddEvent( TheEvents[I] );
end;

procedure TFiniteStateMachine.AddEvent(TheEvent : String);
var
  Idx : Integer;
  H : Integer;
begin
  Idx := fEventList.Add( TheEvent );
  fNextStates.InsertRow( Idx );
  H := fNextStates.Height;
end;

procedure TFiniteStateMachine.AddState(TheStates : array of String);
var
  I : Integer;
begin
  for I := Low(TheStates) to High(TheStates) do
    AddState( TheStates[I] );
end;

procedure TFiniteStateMachine.AddState(TheState : String);
var
  Idx : Integer;
begin
  Idx := fStateList.Add( TheState );
  fNextStates.InsertCol( Idx );
end;

procedure TFiniteStateMachine.DelEvent(TheEvent : String);
var
  I, J : Integer;
  Idx  : Integer;
begin
  if not Events.Find( TheEvent, Idx ) then
    raise TFSMException.CreateFmt( 'Attempt to delete non-existent Event [%s]',
                                   [TheEvent] );
  Events.Delete( Idx );
  fNextStates.DeleteRow( Idx );
end;

procedure TFiniteStateMachine.DelState(TheState : String);
var
  I, J : Integer;
  Idx  : Integer;
begin
  if not States.Find( TheState, Idx ) then
    raise TFSMException.CreateFmt( 'Attempt to delete non-existent State [%s]',
                                   [TheState] );
  // Change all of the NextStates = TheState to '' before
  // deleting the Column associated with TheState
  for I := 0 to pred(fNextStates.Height) do
    for J := 0 to pred( fNextStates.Width ) do
      if fNextStates[I,J] = TheState then
        fNextStates[I,J] := '';
  // Now delete the state from the list and the column from
  // NextStates;
  States.Delete( Idx );
  fNextStates.DeleteCol( Idx );
end;

destructor TFiniteStateMachine.Destroy;
begin
  fStateList.Free;
  fEventList.Free;
  fNextStates.Free;
  inherited Destroy;
end;

function TFiniteStateMachine.Event(TheEvent : String) : String;
var
  I, J : Integer;
begin
  if not fEventList.Find( TheEvent, I ) then
    raise TFSMException.CreateFmt('Triggering unknown Event [%s]',[TheEvent] );
  Result := fNextStates[I,fCurrentState];
  if not Empty( Result ) then
    fStateList.Find( Result, fCurrentState );
end;

function TFiniteStateMachine.GetNextStates( EventIdx, StateIdx : Integer
  ) : String;
begin
  Result := fNextStates[ EventIdx, StateIdx ];
end;

function TFiniteStateMachine.GetState : String;
var
  Count : Integer;
begin
  Count := fStateList.Count;
  if (Count > 0) and (fCurrentState <= Count) then
    Result := fStateList[fCurrentState]
  else
    Result := '';
end;

class function TFiniteStateMachine.Load(TextIO: TTextIO; aParent: TAMPersists
  ): TFiniteStateMachine;
begin
  Result := TFiniteStateMachine( TAMPersists.Load( TextIO, aParent ) );
end;

procedure TFiniteStateMachine.MakeNew;
var
  H, W : Integer;
begin
  inherited MakeNew;
  fStateList := TStringList.Create;
  fStateList.CaseSensitive := False;
  fStateList.Sorted := True;
  fEventList := TStringList.Create;
  fEventList.CaseSensitive := False;
  fEventList.Sorted := True;
  fNextStates := TNextStates.Create(0,0,'');
  H := fNextStates.Height;
  W := fNextStates.Width;
end;

procedure TFiniteStateMachine.Read(TextIO: TTextIO; aVersion: Integer);
var
  I, J : Integer;
  StateCount : Integer;
  EventCount : Integer;
  Item       : String;
begin
  MakeNew;
  if aVersion >= 1 then
    begin
      TextIO.ReadLn( StateCount );
      for I := 0 to pred( StateCount ) do
        begin
          TextIO.ReadLn( Item );
          AddState( Item );
        end;
      TextIO.ReadLn( EventCount );
      for I := 0 to pred( EventCount ) do
        begin
          TextIO.ReadLn( Item );
          AddEvent( Item );
        end;
      for I := 0 to pred( EventCount ) do
        for J := 0 to pred( StateCount ) do
          begin
            TextIO.ReadLn( Item );
            fNextStates[I,J] := Item;
          end;
      TextIO.ReadLn( fInitialState );
    end;
end;

procedure TFiniteStateMachine.RenameEvent(OldValue, NewValue : String);
var
  I , J: Integer;
  Idx : Integer;
begin
  if not Events.Find( OldValue, Idx ) then
    raise TFSMException.CreateFmt( 'Renaming non-existent event [%s]',[OldValue] );
  if Events.Find( NewValue, I ) then
    raise TFSMException.CreateFmt( 'Renaming event %s: New Event [%s] already exists',
                                   [OldValue, NewValue] );
  Events.Sorted := False;
  Events[Idx] := NewValue;
  Events.Sorted := True;
end;

procedure TFiniteStateMachine.RenameState(OldValue, NewValue : String);
var
  I , J: Integer;
  Idx : Integer;
begin
  if not States.Find( OldValue, Idx ) then
    raise TFSMException.CreateFmt( 'Renaming non-existent state [%s]',[OldValue] );
  if States.Find( NewValue, I ) then
    raise TFSMException.CreateFmt( 'Renaming state %s: New State [%s] already exists',
                                   [OldValue, NewValue] );
  States.Sorted := False;
  States[Idx] := NewValue;
  States.Sorted := True;
  for I := 0 to pred( Events.Count ) do
    for J := 0 to pred( States.Count ) do
      if fNextStates[I,J] = OldValue then
        fNextStates[I,J] := NewValue;
end;

procedure TFiniteStateMachine.SetNextStates( EventIdx, StateIdx : Integer ;
  AValue : String);
begin
  fNextStates[ EventIdx, StateIdx ] := AValue;
end;

procedure TFiniteStateMachine.SetStartState(aValue : String);
var
  I : Integer;
begin
  if fStateList.Find( aValue, I ) then
    begin
      fInitialState := aValue;
      fCurrentState := I;
    end
  else
    raise TFSMException.CreateFmt('Unknown Start State [%s]',[aValue]);
end;

procedure TFiniteStateMachine.SetTransition(FromState, ByEvent, ToState : String
  );
var
  I, J : Integer;
begin
  if Empty( FromState ) then exit;
  if not Empty( ToState ) then
    if not fStateList.Find( ToState, J) then
      raise TFSMException.CreateFmt( 'Set Transition to [%s], [%s] does not exist',
                                     [ToState,ToState] );
  if not fStateList.Find( FromState, J) then
    raise TFSMException.CreateFmt( 'Set Transition from [%s], [%s] does not exist',
                                   [FromState,FromState] );
  if not fEventList.Find( ByEvent, I) then
    raise TFSMException.CreateFmt( 'Set Transition from [%s], Event [%s] does not exist',
                                   [FromState,ByEvent] );
  fNextStates[I,J] := ToState;
end;

procedure TFiniteStateMachine.Write(TextIO: TTextIO);
var
  I, J : Integer;
  StateCount : Integer;
  EventCount : Integer;
begin
  //inherited Write(TextIO);
    StateCount := fStateList.Count;
    TextIO.WriteLn( StateCount );
    for I := 0 to pred( StateCount ) do
      TextIO.WriteLn( fStateList[I] );
    EventCount := fEventList.Count;
    TextIO.WriteLn( EventCount );
    for I := 0 to pred( EventCount ) do
      TextIO.WriteLn( fEventList[I] );
    for I := 0 to pred( EventCount ) do
      for J := 0 to pred( StateCount ) do
        TextIO.WriteLn( fNextStates[I,J] );
    TextIO.WriteLn( fInitialState );
end;

//
//
//procedure TMealyStateMachine.Init( States, Events: array of String;
//                                   StartState : String );
//var
//  I, J : Integer;
//  EventList : TStringList;
//begin
//  if Assigned( StateList) then
//    FreeAndNil( StateList );
//  StateList := MakeStateList; // TStringList.Create;
//  //StateList.OwnsObjects := True;
//  //StateList.Sorted := True;
//  //StateList.CaseSensitive := False;
//  for I := 0 to pred(Length(States)) do
//    begin
//      EventList := MakeEventList; //TStringList.Create;
//      //EventList.OwnsObjects := False;
//      //EventList.Sorted := True;
//      //EventList.CaseSensitive := False;
//      for J := 0 to pred(Length( Events )) do
//        EventList.AddObject( Events[J], TObject(Pointer(-1)) );
//
//      StateList.AddObject( States[I], EventList );
//    end;
//  if not StateList.Find( StartState, CurrentState ) then
//    raise TFSMException.CreateFmt( 'New state [%s] is not in States',
//                                   [StartState] );
//  fStartState := StartState;
//end;
//
//class function TMealyStateMachine.Load(TextIO: TTextIO; aParent: TAMPersists
//  ): TMealyStateMachine;
//begin
//  Result := TMealyStateMachine( TAMPersists.Load( TextIO, aParent ) );
//end;
//
//function TMealyStateMachine.MakeEventList: TStringList;
//begin
//  Result := TStringList.Create;
//  Result.OwnsObjects := False;
//  Result.Sorted := True;
//  Result.CaseSensitive := False;
//end;
//
//function TMealyStateMachine.MakeStateList: TStringList;
//begin
//  Result := TStringList.Create;
//  Result.OwnsObjects := True;
//  Result.Sorted := True;
//  Result.CaseSensitive := False;
//end;
//
//procedure TMealyStateMachine.Read(TextIO: TTextIO; aVersion: Integer);
//var
//  I, J       : Integer;
//  StateCount : Integer;
//  StateName  : String;
//  EventList  : TStringList;
//  EventCount : Integer;
//  EventName  : String;
//  NextState  : Integer;
//begin
//  //inherited Read(TextIO, aVersion);
//  if Assigned( StateList ) then
//    FreeAndNil( StateList );
//  StateList := MakeStateList; //TStringList.Create;
//  if aVersion >= 1 then
//    begin
//      TextIO.ReadLn( StateCount );
//      for I := 0 to pred( StateCount ) do
//        begin
//          TextIO.ReadLn( StateName );
//          EventList := MakeEventList; //TStringList.Create;
//          //EventList.OwnsObjects := False;
//          //EventList.Sorted := True;
//          //EventList.CaseSensitive := False;
//          StateList.AddObject( StateName, EventList );
//          TextIO.ReadLn( EventCount );
//          for J := 0 to pred( EventCount ) do
//            begin
//              TextIO.ReadLn( EventName );
//              TextIO.ReadLn( NextState );
//              EventList.AddObject( EventName, TObject( Pointer( NextState) ) );
//            end;
//        end;
//      TextIO.ReadLn( fStartState );
//      if not StateList.Find( fStartState, CurrentState ) then
//        raise TFSMException.CreateFmt( 'Start state [%s] is not in States',
//                                       [fStartState] );
//    end;
//end;
//
//procedure TMealyStateMachine.Write(TextIO: TTextIO);
//var
//  I, J : Integer;
//  StateCount : Integer;
//  EventList  : TStringList;
//  EventCount : Integer;
//  NextState  : Integer;
//begin
//  //inherited Write(TextIO);
//  StateCount := StateList.Count;
//  TextIO.WriteLn( StateList.Count );
//  for I := 0 to pred( StateCount ) do
//    begin
//      TextIO.WriteLn( StateList[I] );
//      EventList := TStringList( StateList.Objects[I] );
//      EventCount := EventList.Count;
//      TextIO.WriteLn( EventCount );
//      for J := 0 to pred( EventCount ) do
//        begin
//          NextState := Integer( Pointer(EventList.Objects[J]) );
//          TextIO.WriteLn( EventList[J] );
//          TextIO.WriteLn( NextState );
//        end;
//    end;
//  TextIO.WriteLn( fStartState );
//end;
//
//constructor TMealyStateMachine.Create(aParent: TAMPersists);
//begin
//  inherited Create(aParent);
//  fVersion := TheVersion;
//  StateList := nil;
//end;
//
//procedure TMealyStateMachine.Add(State, Event, NewState: String );
//var
//  I, J, K : Integer;
//  EventList : TStringList;
//begin
//  //StateList.Find( State, I );
//  //K := StateList.Find( NewState );
//  if not StateList.Find( State, I ) then
//    raise TFSMException.CreateFmt( 'Adding Event [%s] to unknown state {%s]',
//                                   [Event, State] );
//  if not StateList.Find( NewState, K ) then
//    raise TFSMException.CreateFmt( 'New State [%s] unknown when adding Event [%s]',
//                                   [NewState, Event] );
//  EventList := TStringList( StateList.Objects[I] );
//  //J := EventList.Find( Event );
//  if not EventList.Find( Event, J ) then
//    raise TFSMException.CreateFmt( 'Adding unknown event [%s] to state [%s]',
//                                   [Event, State] );
//  if Integer(Pointer(EventList.Objects[J])) <> -1 then
//    raise TFSMException.CreateFmt( 'Duplicate event {%s] for state [%s] [%p]',
//                                   [Event, State, Pointer(EventList.Objects[J])] );
//  EventList.Objects[J] := TObject(Pointer(K));
//end;
//
//procedure TMealyStateMachine.Delete(State: String);
//var
//  Idx : Integer;
//begin
//  if StateList.Find( State, Idx ) then
//    StateList.Delete( Idx );
//end;
//
//destructor TMealyStateMachine.Destroy;
//begin
//  StateList.Free;
//  inherited Destroy;
//end;
//
//function TMealyStateMachine.Event( TheEvent : String ): String;
//var
//  Next : Integer;
//  CurrList : TStringList;
//  vState : Integer;
//begin
//  Result := '';
//  CurrList := TStringList(StateList.Objects[ CurrentState ] );
//  //Next := CurrList.Find( TheEvent );
//  if not CurrList.Find( TheEvent, Next ) then
//    raise TFSMException.CreateFmt( 'Event [%s] does not exist.',[ TheEvent ] );
//  vState := Integer( Pointer( TStringList( CurrList.Objects[ Next ] )));
//  if vState >= 0 then
//    begin
//      CurrentState := vState;
//      Result := StateList[ CurrentState ];
//    end;
//
//end;
//
//function TMealyStateMachine.GetEvents( Idx : Integer ): TStringList;
//begin
//  Result := StateList.Objects[Idx] as TStringList;
//end;
//
//function TMealyStateMachine.GetState: String;
//begin
//  if Assigned( StateList ) then
//    Result := StateList[ CurrentState ]
//  else
//    Result := '';
//end;
//
//function TMealyStateMachine.GetTheEvents: TStrings;
//begin
//  if Assigned( StateList ) and Assigned( StateList.Objects[0] ) then
//    Result := TStrings(StateList.Objects[0])
//  else
//    Result := nil;
//end;
//
//function TMealyStateMachine.GetTheStates: TStrings;
//begin
//  if Assigned( StateList ) then
//    Result := StateList
//  else
//    Result := nil;
//end;

initialization
  ObjectFactory.RegisterClass( TFiniteStateMachine );

end.


{ A Database that vaguely resembles the Borland Paradox interface.

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
unit AMDatabase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AMPersists, Contnrs,
  AMTextIO, AMCompSci;

type
  TDBFind = (dbfExact, dbfFirst, dbfLast);

  TIndexStackItem = record
    Low, High, Current : Integer;
  end;

  TIndexStack = specialize TStack< TIndexStackItem >;

  { TAMIndex }

  TAMIndex = class ( TObjectList )
  private
    vCurrentItem : Integer; // -1 means none.
    vLastId : Integer;
    vLow, vHigh : Integer;
    function GetItems(Idx : Integer): TAMPersists;
    function GetOwnsObjects: Boolean;
    procedure SetItems(Idx : Integer; AValue: TAMPersists);
    procedure SetOwnsObjects(AValue: Boolean);
  private
    fName : String;
    FStack : TIndexStack;
    // These methods are private so that they may only be accessed within the
    // AMDatabase unit.  Specifically they are used by TAMDatabase itself
      function Extract( Item : TAMPersists ) : TAMPersists; // Caution, does not free Item
                                                        // Id is zeroed !!!!

      // this Extract must be restricted to the primary index only!!!!!
      //function Extract( Id : Cardinal ) : TAMPersists;  // Caution, does not free Item
      //                                                // Id is zeroed !!!!
      //function Find( Id : Cardinal ) : TAMPersists;
      //function Remove( Index : Integer ) : Integer;  overload; // Frees the Item
      //function Remove( Item : TAMPersists ) : Integer; overload; // Frees the Item

      function Seek( Id : Cardinal ) : Integer; virtual; abstract;
      procedure Sort; virtual;
  protected
    vIndex : Integer;
    function BinarySearch( Item : TAMPersists; Kind : TDBFind ) : Integer;
    // Callers of AMDatabase are not allowed write access
    property OwnsObjects : Boolean read GetOwnsObjects write SetOwnsObjects;
    property Stack : TIndexStack read FStack;
  public
    constructor Create( FreeObjects : Boolean; aName : String );
    destructor Destroy; override;
    function BOF : Boolean;
    function EOF : Boolean;
    function Current : TAMPersists;

    procedure Push;
    procedure Pop;

    function IndexOfItem( Item : TAMPersists ) : Integer; // Position at which Item is found.
    function IndexOf( Item : TObject ) : Integer;

    function First : TAMPersists;
    function Last : TAMPersists;
    function Next : TAMPersists;
    function Prev : TAMPersists;

    function Find( Item : TAMPersists; Kind : TDBFind = dbfExact ) : TAMPersists;

    // Range is [Low, High)
    function SetRange( Low, High : TAMPersists ) : Integer; // Count in range //Boolean; // false if range empty
    procedure ClearRange;

    function Remove( Index : Integer ) : Integer;  overload; // Frees the Item
    function Remove( Item : TAMPersists ) : Integer; overload; // Frees the Item
    function Update( Item : TAMPersists ) : Integer; virtual;

    property Items[Idx : Integer] : TAMPersists read GetItems write SetItems;

    property Name : String read fName;
  end;  // TAMindex

  { TPrimaryIndex }

  TPrimaryIndex = class ( TAMIndex )
  private
    function GetOwnsObjects : Boolean;
    function Seek( Id : Cardinal ) : Integer; override;
 public
    constructor Create( Idx : Integer = 0 );

    function Add( Item : TAMPersists ) : Integer; overload;

    property OwnsObjects : Boolean read GetOwnsObjects;

  end; // TPrimaryIndex

  { TSecondaryIndex }

  TSecondaryIndex = class (TAMIndex )
  private
    function GetOwnsObjects : Boolean;
    function Seek( Id : Cardinal ) : Integer; override;
    procedure Sort;  override;
  public
    constructor Create( Idx : Integer; aName : String );

    function Add( Item : TAMPersists ) : Integer; overload;
    function Update( Item : TAMPersists ) : Integer; override;

    property OwnsObjects : Boolean read GetOwnsObjects;

  end;// TSecondaryIndex


  { TAMTable }

  TIndexNameStackBase = specialize TStack< String >;

  { TIndexNameStack }

  TIndexNameStack = class( TIndexNameStackBase )
    public
      procedure Dump( Heading : String ); override;
  end;

  generic TAMTable<T : TAMPersists> = class( TAMPersists )
  private
    FIndexStack : TIndexNameStack;
    private
      fOnItemChanged: TNotifyEvent;
      const
        TheVersion = 1;
      var
        fIndex: String;
        fIndexIdx : Integer;
        Indices : TStringList;

        ISCount : Integer;

      function GetCount: Integer;
      function GetIndexNameL( Idx : Integer ) : String;
      function GetItems(Idx : Integer): T;
      procedure SetIndex(AValue: String);
      procedure SetOnItemChanged(AValue: TNotifyEvent);
      //procedure SetItems(Idx : Integer; AValue: T);
      property IndexStack : TIndexNameStack read FIndexStack;
    public
      constructor Create( aParent : TAMPersists = nil); override;
      destructor  Destroy; override;

      procedure Clear; virtual;

      function Add( Item : T ) : Integer;
      function BOF : Boolean;
      function EOF : Boolean;
      function Del( Item : T ) : Integer;
      function Update( Item : T ) : Integer;

      function Current : T;

      function First : T;
      function Last : T;
      function Next : T;
      function Prev : T;

      function Find( Item : T ) : T;

      function SetRange( Low, High : T ) : Integer; // Boolean; // true if range not empty
      procedure ClearRange;

      procedure Read( TextIO : TTextIO; aVersion : Integer ); override;
      procedure Write( TextIO : TTextIO ); override;

      procedure MakeNew;  override;

      procedure PushIndex( theIndexName : String );
      procedure PopIndex;

      procedure DebugIndex;

      property Count : Integer read GetCount;
      property IndexName[Idx : Integer] : String read GetIndexNameL;
      property IndexIdx : Integer read fIndexIdx;
      property Items[Idx : Integer] : T read GetItems;
      property Index : String read fIndex write SetIndex;

      property OnItemChanged : TNotifyEvent read fOnItemChanged write SetOnItemChanged;
    end;


implementation

uses
  AMDebug, AMStrings, AMObjectFactory,
  Math;

{ TIndexNameStack }

procedure TIndexNameStack.Dump(Heading : String);
var
  I : Integer;
begin
  DebugLn( Heading );
  for I := SP downto 0 do
    DebugLn(Format( '%2d:  [%s]',[I,Stack[I]] ));
  DebugLn( '========================' );
end;

{ TAMTable }

//var
//  ISCount : Integer;

constructor TAMTable.Create(aParent: TAMPersists);
var
  TheIndex : TSecondaryIndex;
  I        : Integer;
  N        : String;
begin
  //Debug( 'TAMTable.Create %s',[Name] );
  inherited Create(aParent);
  fVersion := TheVersion;
  fIndexIdx := 0;
  MakeNew;
  //fIndexStack := nil;
  //fIndexStack := TIndexNameStack.Create;
  //Inc( ISCOunt );
  //Debug( 'ISCount:  %d zzzzzzz',[ISCount]);
  PushIndex( '' );
  //fIndexStack.Dump('TAMTable.Create');
end;

destructor TAMTable.Destroy;
var
  I, J : Integer;
  L : TObjectList;
begin
  //if IndexStack.SP >= 0 then
  //  PopIndex;
  //fIndexStack.Dump('TAMTable.Destroy');
  Indices.Free;
  if Assigned( fIndexStack ) then
    begin
      fIndexStack.Free;
      //Dec(IsCount);
      //Debug( 'ISCount:  %d',[ISCount]);
    end;
  inherited Destroy;

end;

function TAMTable.Add(Item: T): Integer;
var
  I : Integer;
begin
  Item.OnModify := fOnItemChanged;
  Result := TPrimaryIndex( Indices.Objects[0] ).Add( Item );
  for I := 1 to pred(Indices.Count) do
    TSecondaryIndex( Indices.Objects[I] ).Add( Item );
end;

function TAMTable.BOF: Boolean;
begin
  Result := TAMIndex( Indices.Objects[fIndexIdx] ).BOF;
end;

procedure TAMTable.Clear;
begin
  Debug('TAMTable.Clear');
  MakeNew;
end;

procedure TAMTable.ClearRange;
begin
  TAMIndex( Indices.Objects[fIndexIdx] ).ClearRange;
end;

function TAMTable.Current: T;
var
  ItemCount : Integer;
begin
  ItemCount := Count;
  if fIndexIdx < Indices.Count then
    Result := T(TAMIndex( Indices.Objects[ fIndexIdx] ).Current)
  else
    Result := nil;
end;

procedure TAMTable.DebugIndex;
//var
//  P : Pointer;
begin
  //P := Pointer(1234567);
  Debug( 'TAMTable.DebugIndex:' );
end;

function TAMTable.Del(Item : T) : Integer;
var
  I : Integer;
begin
  Item.OnModify := fOnItemChanged;
  Result := TPrimaryIndex( Indices.Objects[0] ).Remove( Item );
  for I := 1 to pred(Indices.Count) do
    TSecondaryIndex( Indices.Objects[I] ).Remove( Item );
end;

//destructor TAMTable.Destroy;
//begin
//  Debug('TAMTable.Destroy');
//  if Assigned( Indices ) then
//    FreeAndNil(Indices);
//  inherited Destroy;
//end;
//
function TAMTable.EOF: Boolean;
begin
  Result := TAMIndex( Indices.Objects[fIndexIdx] ).EOF;
end;

function TAMTable.Find(Item: T): T;
begin
  //if fIndexIdx = 0 then
    //raise Exception.Create('Index required to perform "Find"');
    //Result := T( ).Find( Item )
  //else
  Result := T(TAMIndex( Indices.Objects[fIndexIdx] ).Find( Item ));
end;

function TAMTable.First: T;
begin
  //Debug('First IndexIdx:  %d',[fIndexIdx]);
  Result := T(TAMIndex( Indices.Objects[fIndexIdx] ).First);
end;

function TAMTable.GetCount: Integer;
begin
  Result := TAMIndex(Indices.Objects[0]).Count;
end;

function TAMTable.GetIndexNameL(Idx: Integer): String;
begin
  Result := Indices[Idx];
end;

function TAMTable.GetItems(Idx : Integer): T;
begin
  Result := T(TAMIndex( Indices.Objects[fIndexIdx] ).Items[Idx]);
end;

function TAMTable.Last: T;
begin
  Result := T(TAMIndex( Indices.Objects[fIndexIdx] ).Last);
end;

procedure TAMTable.MakeNew;
var
  TheIndex : TSecondaryIndex;
  I        : Integer;
  N        : String;
begin
  //Debug('TAMTable.MakeNew');
  inherited MakeNew;
  if Assigned( Indices ) then
    FreeAndNil( Indices );
  Indices := TStringList.Create;
  Indices.OwnsObjects := True;
  Indices.Sorted := True;
  Indices.CaseSensitive := False;
  Indices.Duplicates := dupError;
  Indices.AddObject( '',TPrimaryIndex.Create);
  for I := 1 to T.IndexCount do
    begin
      N := T.IndexName(I);
      TheIndex := TSecondaryIndex.Create(I,N);
      Indices.AddObject( N, TheIndex );
    end;
  if Assigned( IndexStack ) then
    begin
      fIndexStack.Free;
      fIndexStack := nil;
      //Dec(ISCount);
      //Debug( 'ISCount:  %d',[ISCount]);
    end;
  //fIndexStack := TIndexNameStack.Create;
  //Inc( ISCount );
  //Debug( 'ISCount:  %d',[ISCount]);
  //PushIndex('');
  if not Assigned( fIndexStack ) then
    begin
      //Debug('findexstack nil');
      fIndexStack := TIndexNameStack.Create;
      //Inc( ISCount );
      //Debug( 'ISCount:  %d',[ISCount]);
      PushIndex('');
    end;
  //Debug('TAMTable.MakeNew Done');
end;

function TAMTable.Next: T;
begin
  //Debug('Next IndexIdx:  %d',[fIndexIdx]);
  Result := T(TAMIndex( Indices.Objects[fIndexIdx] ).Next);
end;

procedure TAMTable.PopIndex;
var
  I : Integer;
  N : String;
begin
  //IndexStack.Dump( ' Before PopIndex' );
{$Define USE_ORIGINAL}
{$ifdef USE_ORIGINAL}  // 2017-10-03
  IndexStack.Pop;
  N := IndexStack.Top;
//{$else}
//  N := IndexStack.Pop;
{$endif}
  //Debug('Popping index [%10s]',[N]);
  //Debug('PopIndex');
  SetIndex( N );
  for I := 0 to pred( Indices.Count ) do
    TAMIndex( Indices.Objects[I] ).Pop;
  //IndexStack.Dump( ' After PopIndex' );
end;

function TAMTable.Prev: T;
begin
  Result := T(TAMIndex( Indices.Objects[fIndexIdx] ).Prev);
end;

procedure TAMTable.PushIndex( theIndexName : String);
var
  I : Integer;
  //S : String;
begin
  //Debug('PushIndex:  <%s>',[theIndexName]);
  //IndexStack.Dump( ' Before PushIndex' );
  //if IndexStack.SP < 0 then
  //  S := 'EMPTY'
  //else
  //  S := IndexStack.Top;
  //DEbug('Pushing index [%s] from [%s]',[theIndexName, S] );
  //if not Assigned( fIndexStack ) then Debug( 'XXXXX:  %p',[fIndexStack] );
  fIndexStack.Push( theIndexName );
  for I := 0 to pred( Indices.Count ) do
    TAMIndex( Indices.Objects[I] ).Push;
  SetIndex( theIndexName );
  //IndexStack.Dump( ' After PushIndex' );
end;

procedure TAMTable.Read(TextIO: TTextIO; aVersion: Integer);
var
  vCount : Integer;
  I     : Integer;
  N     : String;
  SP    : Integer;
  Item  : T;
begin
  MakeNew;
  N := '';
  //Debug('TAMTable.Read Index Count %d.  Should be 3',[Indices.Count]);
  vCount := 0; // eliminate spurious Hint
  if aVersion >= 1 then
    begin
      TextIO.ReadLn(fIndex);
      TextIO.ReadLn(fIndexIdx);
      TextIO.ReadLn(vCount);
      TextIO.ReadLn( SP );
      fIndexStack.Reset;
      for I := 0 to SP do
        begin
          TextIO.ReadLn( N );
          fIndexStack.Push( N );
        end;
      for I := 1 to pred(vCount) do
        begin
          TextIO.Readln(N);
          // Indices already created in MakeNew.
          //Indices.AddObject(N, TSecondaryIndex.Create(I));
        end;
      TextIO.ReadLn(vCount); // Number if items in primary index
      for I := 0 to pred(vCount) do
        begin
          //Item := T(T.Load(TextIO) );
          N := TextIO.ReadAhead;
          N := Copy(N,2,Length(N)-2);
          Item := T( AMPersistsFactory.MakeObject( TextIO, N ));
          Item.Id := 0;
          Add( Item );
          //Item.Free;
        end;
    end;
end;

procedure TAMTable.SetIndex(AValue: String);
var
  C : TAMPersists;
  Idx : Integer;
begin
  if (fIndex=AValue) and (fIndex <> '') then Exit;
  C := TAMPersists(Current);
  fIndex:=AValue;
  //for Idx := 0 to pred(Indices.Count) do
  //  Debug( 'Index[%d]: [%s]',[Idx,Indices.Strings[Idx]] );
  Idx := Indices.IndexOf( fIndex );
  if ( Idx < 1) and (not Empty( AValue )) then
    raise Exception.Create( 'Oh SHIT');
  if Idx < 0 then
    raise Exception.CreateFmt('Setting undefined Index: "%s"',[AValue]);
  fIndexIdx := Idx;
  TAMIndex( Indices.Objects[fIndexIdx] ).IndexOf( C );
end;

procedure TAMTable.SetOnItemChanged(AValue: TNotifyEvent);
var
  C : Integer;
  I : Integer;
  PIndex : TAMIndex;
begin
  if fOnItemChanged=AValue then Exit;
  fOnItemChanged:=AValue;

  PIndex := TAMIndex( Indices.Objects[0] );

  C := PIndex.Count;
  for I := 0 to pred( C ) do
    begin
      T(PIndex.Items[I]).OnModify := AValue;
    end;

end;

function TAMTable.SetRange(Low, High: T): Integer;
begin
  if fIndexIdx = 0 then
    raise Exception.Create('Index required to perform "SetRange"');

  Result := TAMIndex( Indices.Objects[fIndexIdx] ).SetRange( Low, High );
end;

function TAMTable.Update(Item : T) : Integer;
var
  I : Integer;
begin
  if Item.Id = 0 then
    Add(Item)
  else
    begin
      TPrimaryIndex( Indices.Objects[0] ).Update( Item );
      for I := 1 to pred(Indices.Count) do
        TSecondaryIndex( Indices.Objects[I] ).Update( Item );
    end;
  Result := -1;
end;

procedure TAMTable.Write(TextIO: TTextIO);
var
  I : Integer;
  C : Integer;
begin
  TextIO.WriteLn(fIndex);     // The current index's name
  Debug('Writing fIndexIdx <%s>',[fIndex]);
  TextIO.WriteLn(fIndexIdx);  // the current index's index
  Debug('current Index idx %d',[fIndexIdx]);
  TextIO.WriteLN(Indices.Count);
  Debug('Indices.Count %d',[Indices.Count]);
  TextIO.WriteLn( fIndexStack.SP );
  Debug('IndexStack SP %d',[fIndexStack.SP] );
  for I := 0 to fIndexStack.SP do
    begin
      TextIO.WriteLn( fIndexStack[I] );
      Debug('fIndexStack[%d]:  %s',[I,fIndexStack[I]]);
    end;
  for I := 1 to pred(Indices.Count) do
    begin
      TextIO.WriteLn(Indices[I]);
      Debug( 'Indices[%d]:  %s',[I,Indices[I]]);
    end;
  // Now write the data stored in the primary index
  C := TAMIndex(Indices.Objects[0]).Count;
  Debug('PrimaryIndex count:  %d',[C]);
  TextIO.WriteLn(C);
  for I := 0 to pred(C) do
    begin
      TAMIndex(Indices.Objects[0]).Items[I].Store(TextIO);
      Debug('Items[%d]',[I]);
    end;
end;

{ TAMIndex }

var
  IndexInstanceCount : Integer;

constructor TAMIndex.Create(FreeObjects: Boolean; aName : String);
var
  Idx : TIndexStackItem;
begin
  inherited Create(FreeObjects);
  fName := aName;
  VLow := 0;
  VHigh := -1;
  Inc(IndexInstanceCount);
  fStack := TIndexStack.Create;
  Idx.High := -1;
  Idx.Low  := 0;
  Idx.Current := -1;
  fStack.Push( Idx );
  //Debug('IndexInstanceCount ++ :  %d',[indexInstanceCount]);
end;

function TAMIndex.BinarySearch(Item: TAMPersists; Kind: TDBFind): Integer;
var
  L, R, M : Integer;
  C : Integer;
begin
  //DebugLn(Format('BinarySearch:  %d',[Count]));
  if Count <= 0 then
    begin
      //Debug('BinarySearch: Table empty');
      Result := -1;
      exit;
    end;
  L := 0;
  R := Count-1;
  while L <= R do
    begin
      M := Floor((L + R) / 2.0);
      C := Item.Compare( Items[M], vIndex );
      if C < 0 then
        L := M + 1
      else if C > 0 then
        R := M - 1
      else
        break;
    end;
  //DebugLn(Format('C:  %d,  M:  %d',[C, M] ));
  if C = 0 then
    case Kind of
      dbfExact : Result := M;
      dbfFirst :
        begin
          while (M > 0) and (C = 0) do
            begin
              C := Item.Compare( Items[M-1], vIndex );
              if C = 0 then Dec(M);
            end;
          Result := M;
        end;
      dbfLast :
        begin
          while (M < pred(Count)) and (C = 0) do
            begin
              C := Item.Compare( Items[M+1], vIndex );
              if C = 0 then Inc(M);
            end;
          Result := M;
        end;
    end
  else
    case Kind of
      dbfExact : Result := -1;
      dbfFirst : Result := 0;
      dbfLast  : Result := pred(Count);
      //dbfFirst : Result := pred(Count);
      //dbfLast  : Result := 0;
    end;
    //Result := -1;
end;

//function TAMIndex.BinarySearch(Item: TAMPersists; Kind: TDBFind): Integer;
//var
//  L, R, M : Integer;
//  C : Integer;
//begin
//  if Count <= 0 then
//    begin
//      Result := -1;
//      exit;
//    end;
//  L := 0;
//  R := Count-1;
//  while L <= R do
//    begin
//      M := Floor((L + R) / 2.0);
//      C := Item.Compare( Items[M], vIndex );
//      if C < 0 then
//        L := M + 1
//      else if C > 0 then
//        R := M - 1
//      else
//        break;
//    end;
//  //if C = 0 then
//  //  Result := M
//  //else
//    case Kind of
//      //dbfExact : Result := -1;
//      dbfExact :
//        if C = 0 then
//          Result := M
//        else
//          Result := -1;
//      dbfFirst : Result := L;
//      dbfLast : Result := R; // Confirm that this is correct!
//    end;
//end;

function TAMIndex.BOF: Boolean;
begin
  Result := (vCurrentItem < vLow) or (vCurrentItem < 0);
  //Debug( 'BOF High, Low, Current %d, %d, %d = %d',[vHigh,vLow,vCurrentItem,ord(Result)] );
end;

procedure TAMIndex.ClearRange;
begin
  vLow := 0;
  vHigh := pred(Count);
end;

function TAMIndex.Current: TAMPersists;
var
  C : Integer;
begin
  C := Count;
  //Debug( 'Current:  %d, Count : %d',[vCurrentItem, C] );
  if (vCurrentItem >= 0) and (vCurrentItem < C) then
    Result := TAMPersists(Items[vCurrentItem])
  else
    begin
      Result := nil;
      //vCurrentItem := -1;
    end;
end;

destructor TAMIndex.Destroy;
begin
  Dec(IndexInstanceCount);
  Stack.Free;
  //Debug('IndexInstanceCount --:  %d',[indexInstanceCount]);
  //Debug('Count: %d',[Count]);
  inherited Destroy;
end;

function TAMIndex.EOF: Boolean;
begin
  Result := (vCurrentItem > vHigh) or (vCurrentItem < 0);
  //Debug( 'EOF High, Current %d %d = %d',[vHigh,vCurrentItem,ord(Result)] );
end;

function TAMIndex.Extract(Item: TAMPersists): TAMPersists;
begin
  Result := TAMPersists( inherited Extract( TObject ( Item ) ) );
  if Assigned( Result ) then
    Result.Id := 0;
end;

function TAMIndex.Find(Item: TAMPersists; Kind: TDBFind): TAMPersists;
var
  Idx : Integer;
begin
  Idx := BinarySearch( Item, Kind );
  if Idx >= 0 then
    Result := TAMPersists( Items[Idx] )
  else
    Result := nil;
end;

function TAMIndex.First: TAMPersists;
begin
  if (vHigh >= vLow) and (vLow >= 0) then
    begin
      //Debug( 'First:  %d >= %d',[vHigh,vLow]);
      Result := Items[vLow];
      vCurrentItem := vLow;
    end
  else
    begin
      Result := nil;
      vCurrentItem := -1;
    end;
end;

function TAMIndex.GetItems(Idx : Integer): TAMPersists;
begin
  //Debug('TAMIndex.GetItems(%d)',[Idx]);
  Result := nil;
  if Idx < 0 then
    Debug('Hell');
  try
    Result := TAMPersists( inherited Items[Idx] );
    if Assigned( Result ) then
      vCurrentItem := Idx
    else
      vCurrentItem := -1;
  except
    Debug('TAMIndex.GetItems(%d) FAILED',[Idx]);
  end;
end;

function TAMIndex.GetOwnsObjects: Boolean;
begin
  Result := inherited OwnsObjects;
end;

function TAMIndex.IndexOf(Item: TObject): Integer;
begin
  Result:=inherited IndexOf(Item);
  vCurrentItem := Result;
end;

function TAMIndex.IndexOfItem(Item: TAMPersists): Integer;
begin
  Result := Seek( Item.Id );
  vCurrentItem := Result;
end;

function TAMIndex.Last: TAMPersists;
begin
  if (vHigh >= vLow) and (vHigh >= 0) then
    begin
      Result := Items[ vHigh ];
      vCurrentItem :=  vHigh;
    end
  else
    begin
      Result := nil;
      vCurrentItem := -1;
    end;
  //Debug( 'Last High, Low, Current %d %d %d',[vHigh,vLow,vCurrentItem] );
end;

function TAMIndex.Next: TAMPersists;
begin
  Inc(vCurrentItem);
  Result := Current;
end;

procedure TAMIndex.Pop;
var
  Idx : TindexStackItem;
begin
  Idx := Stack.Pop;
  //Idx := Stack.Top;
  vLow := Idx.Low;
  vHigh := Idx.High;
  vCurrentItem := Idx.Current;
  //Debug( '%10s Index.Pop  SP: %d, High %d, Low %d, Current %d',
  //       [fName, Stack.SP, vHIgh, vLow, vCurrentItem] );
end;

function TAMIndex.Prev: TAMPersists;
begin
  Dec(vCurrentItem);
  Result := Current;
end;

procedure TAMIndex.Push;
var
  Idx : TindexStackItem;
begin
  Idx.Low     := vLow;
  Idx.High    := vHigh;
  Idx.Current := vCurrentItem;
  Stack.Push( Idx );
  //Debug( '%10s Index.Push  SP: %d, High %d, Low %d, Current %d',
  //       [fName, Stack.SP, vHIgh, vLow, vCurrentItem] );
end;

function TAMIndex.Remove(Index: Integer): Integer;
begin
  Result := inherited Remove( inherited Items[Index] );
  ClearRange;
  vCurrentItem := -1; // Remove kills the ordering
end;

function TAMIndex.Remove(Item: TAMPersists): Integer;
begin
  Result := inherited Remove( TObject( Item ) );
  ClearRange;
  vCurrentItem := -1; // Remove kills the ordering
end;

procedure TAMIndex.SetItems(Idx : Integer; AValue: TAMPersists);
begin
  inherited Items[Idx] := TObject( AValue );
end;

procedure TAMIndex.SetOwnsObjects(AValue: Boolean);
begin
  inherited OwnsObjects := AValue;
end;

function TAMIndex.SetRange(Low, High: TAMPersists): Integer;
var
  NotEmpty : Boolean;
begin
  Debug('TAMIndex.SetRange');
  vLow := BinarySearch( Low, dbfFirst );
  vHigh := BinarySearch( High, dbfLast );
  Debug('TAMIndex.SetRange %d .. %d done',[vLow,vHigh]);
  NotEmpty := (vHigh >= vLow) and (vHigh >= 0) and (vLow >= 0);
  if NotEmpty then
    Result := vHigh - vLow + 1
  else
    Result := 0;
  //Debug('TAMIndex.SetRange %d done',[ord(Result)]);
end;

procedure TAMIndex.Sort;
var
  I, J : Integer;
  Lowest, Highest : Integer;
  Odd : Boolean;
  Swap : Boolean;
begin
  Lowest := 0;
  Highest := pred( Count );
  Odd := False;
  Swap := True;
  while Swap do
    begin
      Swap := False;
      if Odd then
        begin
          for I := pred(Highest) downto Lowest do
            if Items[I].Compare( Items[I+1], vIndex ) < 0 then
              begin
                Exchange( I+1, I );
                Swap := True;
              end;
        end
      else
        begin
          for I := Lowest to pred(Highest) do
            if Items[I].Compare( Items[I+1], vIndex ) < 0 then
              begin
                Exchange( I+1, I );
                Swap := True;
              end;
        end;

    end;

end;

function TAMIndex.Update(Item : TAMPersists) : Integer;
begin
  ClearRange;
  Sort;
  Result := -1;
end;

{ TPrimaryIndex }

constructor TPrimaryIndex.Create(Idx: Integer);
begin
  inherited Create(True,'Primary');
  if Idx <> 0 then
    raise Exception.CreateFmt('TPrimaryIndex Idx = %d.  It MUST be 0',[Idx]);
  vIndex := 0;
  inherited OwnsObjects := True; // was False!!!! 2017-05-22
end;

function TPrimaryIndex.Add(Item: TAMPersists): Integer;
begin
  if Item.Id > 0 then
    raise Exception.CreateFmt('Attempt to add an item with non-zero Id of %d',[Item.Id]);
  Inc( vLastId );
  Item.Id := vLastId;
  Result := Add( TObject(Item) );
  ClearRange;
end;

function TPrimaryIndex.GetOwnsObjects: Boolean;
begin
  Result := inherited;
end;

function TPrimaryIndex.Seek(Id: Cardinal): Integer;
var
  L, R, M : Integer;
  C       : Integer;
  A       : TAMPersists;
begin
  try
    A := TAMPersists.Create;
    A.Id := Id;
    Result := -1;
    L := 0;
    R := Count - 1;
    while L <= R do
      begin
        M := Floor( (L + R) / 2.0 );
        C := A.Compare(Items[M], 0);
        if C > 0 then
          L := M + 1
        else if C < 0 then
          R := M - 1
        else
          begin
            Result := M;
            exit;
          end;
      end;
  finally
    A.Free;
  end;
end;

{ TSecondaryIndex }

constructor TSecondaryIndex.Create(Idx: Integer; aName : String);
begin
  inherited Create(False,aName);
  if Idx <= 0 then
    raise Exception.CreateFmt('TSecondaryIndex Idx = %d.  It MUST be > 0',[Idx]);
  vIndex := Idx;
  inherited OwnsObjects := False;
end;

function TSecondaryIndex.Add(Item: TAMPersists): Integer;
var
  I : Integer;
  Temp : TAMPersists;
begin
  Result := inherited Add( Item );  // Add it to the end
  ClearRange;
  // Move item down till it fits
  for I := pred(Count) downto 1 do
    begin
      if (Items[I].Compare( Items[I-1], vIndex ) > 0) then
        begin
          Temp := Items[I];
          Items[I] := Items[I-1];
          Items[I-1] := Temp;
          Result := I-1;        // Not sure we need this for the indices, but...
                                // The result is correct if it is ever needed
        end
      else
        exit;
    end;
end;

function TSecondaryIndex.GetOwnsObjects: Boolean;
begin
  Result := Inherited;
end;

function TSecondaryIndex.Seek(Id: Cardinal): Integer;
begin
  Result := Id;
  raise Exception.Create('TSecondaryIndex.Seek(Id: Cardinal) not implemented');
end;

procedure TSecondaryIndex.Sort;
var
  I, J : Integer;
  Lowest, Highest : Integer;
  Odd : Boolean;
  Swap : Boolean;
begin
  Lowest := 0;
  Highest := pred( Count );
  Odd := False;
  Swap := True;
  while Swap do
    begin
      Swap := False;
      if Odd then
        begin
          for I := pred(Highest) downto Lowest do
            if Items[I].Compare( Items[I+1], vIndex ) < 0 then
              begin
                Exchange( I+1, I );
                Swap := True;
              end;
        end
      else
        begin
          for I := Lowest to pred(Highest) do
            if Items[I].Compare( Items[I+1], vIndex ) < 0 then
              begin
                Exchange( I+1, I );
                Swap := True;
              end;
        end;

    end;

end;

function TSecondaryIndex.Update(Item : TAMPersists) : Integer;
begin
  ClearRange;
  Sort;
  Result := -1;
end;

end.


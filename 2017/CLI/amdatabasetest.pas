unit AMDatabaseTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure DoTest;

implementation

uses
  AMDebug,
  AMDatabase, AMPersists, AMTextIO, AMAppUtils, AMObjectFactory,
  AMFileUtils;

type

  { TTestIndexItem }

  TTestIndexItem = class( TAMPersists )
  private
    const
      TheVersion = 1;
      fIndexCount = 2;
      fIndexName : array[1..fIndexCount] of String =
        ('Forward','Reverse');
    var
    fName : String;
  public
    constructor Create( aParent : TAMPersists = nil ); override;
    function Compare( Item : TAMPersists; anIndex : Integer ) : Integer; override;
    function GetIndexName(Idx : Integer): String; override;

    property Name : String read fName write fName;

    class function IndexCount : Integer; override;
    class function IndexName( Idx : Integer ) : String; override;

    procedure Read( TextIO : TTextIO; aVersion : Integer ); override;
    procedure Write( TextIO : TTextIO ); override;

  end;

  TTable = specialize TAMTable<TTestIndexItem>;

constructor TTestIndexItem.Create(aParent: TAMPersists);
begin
  inherited Create(aParent);
  fVersion := TheVersion;
  //fIndexCount := 2; // Does NOT include the primary index
end;

function TTestIndexItem.Compare( Item : TAMPersists; anIndex : Integer ) : Integer;
var
  anItem : TTestIndexItem;
begin
  anItem := TTestIndexItem( Item );
  case anIndex of
    0 : Result := inherited;
    1,2 :
      begin
        if Self.Name > anItem.Name then
          Result := 1
        else if Self.Name < anItem.Name then
          Result := -1
        else
          Result := 0;
        if anIndex = 1 then
          Result := - Result;
      end;
  end;
end;

function TTestIndexItem.GetIndexName(Idx: Integer): String;
begin
  if Idx < 1 then
    Result := inherited
  else
    Result := fIndexName[Idx];
end;

class function TTestIndexItem.IndexCount: Integer;
begin
  Result := fIndexCount;
end;

class function TTestIndexItem.IndexName(Idx: Integer): String;
begin
  if Idx < 1 then
    Result := inherited
  else
    Result := fIndexName[Idx];
end;

procedure TTestIndexItem.Read(TextIO: TTextIO; aVersion: Integer);
begin
  if aVersion >= 1 then
    begin
      TextIO.ReadLn( fName );
    end;
end;

procedure TTestIndexItem.Write(TextIO: TTextIO);
begin
  TextIO.WriteLn( fName );
end;



procedure TestCreateFreePrimary;
var
  PrimaryList : TPrimaryIndex;
  Item        : TAMPersists;
begin
  Item := TAMPersists.Create;
  PrimaryList := TPrimaryIndex.Create;
  if not PrimaryList.OwnsObjects then
    raise Exception.Create('Primary List MUST own objects but DOESN''T');
  if PrimaryList.Count <> 0 then
    raise Exception.Create('Primary List was not create with 0 Items');
  PrimaryList.Add( Item );
  if PrimaryList.Count <> 1 then
    raise Exception.Create('Primary List count invalid after adding Item');
  PrimaryList.Free;

end; // TestCreateFreePrimary

procedure TestAddAndRemoveItems;
var
  PL : TPrimaryIndex;
  Item : TAMPersists;
  I : Integer;
  Id : Integer;
begin
  PL := TPrimaryIndex.Create;
  for I := 1 to 10 do
    begin
      Item := TAMPersists.Create;
      PL.Add( Item );
    end;

  if PL.Count <> 10 then
    raise Exception.CreateFmt('Added 10 items, got $d items',[PL.Count]);

  for I := 1 to 10 do
    begin
      Id := PL.Items[I-1].Id;  // Items is zero based
      if Id <> I then
        raise Exception.CreateFmt('Item[%d] Id is %d',[I,Id]);
    end;

  PL.Remove( PL.Items[5] ); // Remove item at [5] (Id should be 6)

  for I := 0 to pred(PL.Count) do
    begin
      Item := PL.Items[I];
      if Item.Id = 6 then
        raise Exception.Create('Failed to remove Item at [5]');
    end;

  PL.Remove( PL.Items[3] );

  for I := 0 to pred(PL.Count) do
    begin
      Item := PL.Items[I];
      if Item.Id = 4 then
        raise Exception.Create('Failed to remove Item at [3]');
    end;

  PL.Free;

end;  // TestAddAndRemoveItems

procedure TestIndexOfItem;
var
  PL : TPrimaryIndex;
  Item : TAMPersists;
  I    : Integer;
  Index : Integer;
begin
  PL := TPrimaryIndex.Create;
  for I := 1 to 10 do
    begin
      Item := TAMPersists.Create;
      PL.Add( Item );
    end;

  for I := 1 to 10 do
    begin
      Item := TAMPersists.Create;
      Item.Id := I;
      Index := PL.IndexOfItem( Item );
      if Index <> I - 1 then
        raise Exception.CreateFmt('IndexOf Item with Id = %d is %d should by %d',[I,Index,I-1]);
      Item.Free;
    end;
  PL.Free;
end; // TestIndexOfItem

procedure TestCreateFreeIndex;
var
  Index0 : TSecondaryIndex;
  Item   : TTestIndexItem;
begin
  Index0 := nil;
  Index0 := TSecondaryIndex.Create( 1, 'XXX' );

  Item := TTestIndexItem.Create;
  Item.Name := 'One';
  Index0.Add(Item);
  if Index0.Count <> 1 then
    raise Exception.CreateFmt( 'Count is %d, expected %d',[Index0.Count,1]);
  Debug('Item.Name:  %s',[Item.Name]);
  Index0.Items[0].Free;
  Index0.Free;
end;  // TestCreateFreeIndex

procedure TestAddAndRemoveIndexItems;
  procedure TestByIndex( Idx : Integer );
  var
    Index0 : TSecondaryIndex;
    Item   : TTestIndexItem;
    Items  : array [0..4] of TTestIndexItem;
    I      : Integer;
    //ID     : Integer;
    //OK     : Boolean;
  begin
    Index0 := TSecondaryIndex.Create( Idx,'TTT' );

    Item   := TTestIndexItem.Create;
    Item.Name := 'C';
    Index0.Add( Item );

    Item   := TTestIndexItem.Create;
    Item.Name := 'D';
    Index0.Add( Item );

    Item   := TTestIndexItem.Create;
    Item.Name := 'B';
    Index0.Add( Item );

    Item   := TTestIndexItem.Create;
    Item.Name := 'A';
    Index0.Add( Item );

    Item   := TTestIndexItem.Create;
    Item.Name := 'E';
    Index0.Add( Item );

    if Index0.Count <> 5 then
      raise Exception.CreateFmt('Added 5 index items, got %d',[Index0.Count]);

    for I := 0 to 4 do
      Items[I] := TTestIndexItem(Index0.Items[I]);

    case Idx of
      1:
        begin
          if (Items[0].Name <> 'A') or
             (Items[1].Name <> 'B') or
             (Items[2].Name <> 'C') or
             (Items[3].Name <> 'D') or
             (Items[4].Name <> 'E') then
            raise Exception.CreateFmt( 'Expecting "ABCDE" got "%s%s%s%s%s"',
                                       [Items[0].Name, Items[1].Name,
                                        Items[2].Name, Items[3].Name,
                                        Items[4].Name ] );
        end;
      2:
        begin
          if (Items[4].Name <> 'A') or
             (Items[3].Name <> 'B') or
             (Items[2].Name <> 'C') or
             (Items[1].Name <> 'D') or
             (Items[0].Name <> 'E') then
            raise Exception.CreateFmt( 'Expecting "EDCBA" got "%s%s%s%s%s"',
                                       [Items[0].Name, Items[1].Name,
                                        Items[2].Name, Items[3].Name,
                                        Items[4].Name ] );
        end;
    end;

    Index0.Items[0].Free;
    Index0.Items[1].Free;
    Index0.Items[2].Free;
    Index0.Items[3].Free;
    Index0.Items[4].Free;

    Index0.Free;
  end;
begin
  TestByIndex( 1 );
  TestByIndex( 2 );
end; // TestAddAndRemoveIndexItems

procedure TestCreateFreeTable;
//type
//  TTable = specialize TAMTable<TTestIndexItem>;
var
  ATable : TTable;
  AnItem : TTestIndexItem;
begin
  AnItem := TTestIndexItem.Create;  // This is local .. it is not stored in table
  ATable := TTable.Create( AnItem );
  //Debug( 'IndexName %d:  [%s]',[0,ATable.IndexName[0]]);
  //Debug( 'IndexName %d:  [%s]',[1,ATable.IndexName[1]]);
  //Debug( 'IndexName %d:  [%s]',[2,ATable.IndexName[2]]);
  ATable.Free;
  AnItem.Free;
end;

procedure TestAddAndRemoveTableItems;
var
  ATable : TTable;
  Item    : TTestIndexItem;
  //Id      : Integer;
  E, A, T : String; // expected, actual, Test
begin
  ATable := TTable.Create;

  Item := TTestIndexItem.Create;
  Item.Name := 'Y';
  ATable.Add( Item );
  Item := TTestIndexItem.Create;
  Item.Name := 'V';
  ATable.Add( Item );
  Item := TTestIndexItem.Create;
  Item.Name := 'Z';
  ATable.Add( Item );
  Item := TTestIndexItem.Create;
  Item.Name := 'W';
  ATable.Add( Item );
  Item := TTestIndexItem.Create;
  Item.Name := 'X';
  ATable.Add( Item );

  ATable.Current;

  //ATable.Index := '';
  //ATable.Current;
  //ATable.Index := 'forward';
  //ATable.Current;
  //ATable.Index := 'Reverse';
  //ATable.Current;

  try
    //ATable.Index := 'invalid';
    raise Exception.Create('Setting ATable.Index to ''invalid'' succeeded.  It shouldn''t have');
  except
    Debug('Invalid index test passed');
  end;

  //ATable.Index := '';
  Item := TTestIndexItem(ATable.First);
  T := 'As Entered, First';
  E := 'Y';
  A := Item.Name;
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);
  Item := TTestIndexItem(ATable.Next);
  T := 'As Entered, Next';
  E := 'V';
  A := Item.Name;
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);
  Item := TTestIndexItem(ATable.Next);
  T := 'As Entered, Next';
  E := 'Z';
  A := Item.Name;
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);
  //Debug('First - Item.Name = %s, expected %s',[Item.Name,'Y']);
  Item := TTestIndexItem(ATable.Last);
  T := 'As Entered, Last';
  E := 'X';
  A := Item.Name;
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);
  Item := TTestIndexItem(ATable.Prev);
  T := 'As Entered, Prev';
  E := 'W';
  A := Item.Name;
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);
  Item := TTestIndexItem(ATable.Prev);
  T := 'As Entered, Prev';
  E := 'Z';
  A := Item.Name;
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);
//  Debug('Last - Item.Name = %s, expected %s',[Item.Name,'X']);

  //ATable.Index := 'forward';
  Item := TTestIndexItem(ATable.First);
  T := 'Forward First';
  E := 'V';
  A := Item.Name;
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);
  Item := TTestIndexItem(ATable.Next);
  T := 'Forward Next';
  E := 'W';
  A := Item.Name;
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);
   Item := TTestIndexItem(ATable.Next);
  T := 'Forward Next';
  E := 'X';
  A := Item.Name;
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);
 //Debug('First - Item.Name = %s, expected %s',[Item.Name,'V']);
  Item := TTestIndexItem(ATable.Last);
  T := 'Forward Last';
  E := 'Z';
  A := Item.Name;
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);
  Item := TTestIndexItem(ATable.Prev);
  T := 'Forward Prev';
  E := 'Y';
  A := Item.Name;
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);
  Item := TTestIndexItem(ATable.Prev);
  T := 'Forward Prev';
  E := 'X';
  A := Item.Name;
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);
  //Debug('Last - Item.Name = %s, expected %s',[Item.Name,'Z']);

  //ATable.Index := 'reverse';
  Item := TTestIndexItem(ATable.First);
  T := 'Reverse First';
  E := 'Z';
  A := Item.Name;
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);
  Item := TTestIndexItem(ATable.Next);
  T := 'Reverse Next';
  E := 'Y';
  A := Item.Name;
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);
  Item := TTestIndexItem(ATable.Next);
  T := 'Reverse Next';
  E := 'X';
  A := Item.Name;
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);
  //Debug('First - Item.Name = %s, expected %s',[Item.Name,'Z']);
  Item := TTestIndexItem(ATable.Last);
  T := 'Reverse Last';
  E := 'V';
  A := Item.Name;
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);
  Item := TTestIndexItem(ATable.Prev);
  T := 'Reverse Prev';
  E := 'W';
  A := Item.Name;
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);
  Item := TTestIndexItem(ATable.Prev);
  T := 'Reverse Prev';
  E := 'X';
  A := Item.Name;
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);
  //Debug('Last - Item.Name = %s, expected %s',[Item.Name,'V']);

  ATable.Free;
end;  //  TestAddAndRemoveTableItems

procedure TestSwitchBetweenIndices;
//type
//  TTable = specialize TAMTable<TTestIndexItem>;
var
  ATable : TTable;
  Item    : TTestIndexItem;
  I       : Integer;
  E, A, T : String; // expected, actual, Test
const
  Items = 'GIHACDBEF';
begin
  ATable := TTable.Create;
  for I := 1 to Length(Items) do
    begin
      Item := TTestIndexItem.Create;
      Item.Name := Items[I];
      //Debug('Items[%d]: [%s]',[I,Item.Name]);
      ATable.Add(Item);
    end;

  //ATable.Index := 'Reverse';
  //ATable.Index := '';
  ATable.First;    // G
  ATable.Next;     // I
  ATable.Next;;    // H
  ATable.Next;     // A
  Item := ATable.Next;     // C
  E := 'C';
  A := Item.Name;
  T := 'As Entered after 4 Nexts';
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);

  //ATable.Index := 'Forward';

  Item := ATable.Current;
  E := 'C';
  A := Item.Name;
  T := 'As Entered after change index to "Forward"';
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);
  Item := ATable.Next;
  E := 'D';
  A := Item.Name;
  T := 'As Entered after change index to "Forward" and Next';
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);


  //ATable.Index := 'Reverse';

  Item := ATable.Current;
  E := 'D';
  A := Item.Name;
  T := 'As Entered after change index to "Reverse"';
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);
  Item := ATable.Next;
  E := 'C';
  A := Item.Name;
  T := 'As Entered after change index to "Reverse" and Next';
  if E <> A then
    raise Exception.CreateFmt('%s - expected [%s], got [%s]',[T,E,A]);

  ATable.Free;
end; //TestSwitchBetweenIndices

procedure TestBOFandEOF;
var
  ATable : TTable;
  Item    : TTestIndexItem;
  I       : Integer;
  A       : String;
const
  Items = 'GIHACDBEF';
begin

  ATable := TTable.Create;
  for I := 1 to Length(Items) do
    begin
      Item := TTestIndexItem.Create;
      Item.Name := Items[I];
      //Debug('Items[%d]: [%s]',[I,Item.Name]);
      ATable.Add(Item);
    end;

  //Debug('--------------- insert first-last');
  Item := ATable.First;
  I := 0;
  while not ATable.EOF do
    begin
      //Debug('Item[%d]: %s',[I,Item.Name]);
      Inc(I);
      Item := ATable.Next;
      if Assigned( Item ) then
        A := Item.Name;
    end;
  if I <> 9 then
    raise Exception.CreateFmt('Insert order EOF count %d, should be 9',[I]);
  if A <> 'F' then
    raise Exception.CreateFmt('Insert order EOF last item [%s], should be F',[A]);

  //Debug('--------------- insert last-first');
  Item := ATable.Last;
  I := 0;
  while not ATable.BOF do
    begin
      //Debug('Item[%d]: %s',[I,Item.Name]);
      Inc(I);
      Item := ATable.Prev;
      if Assigned( Item ) then
        A := Item.Name;
    end;
  if I <> 9 then
    raise Exception.CreateFmt('Insert order BOF count %d, should be 9',[I]);
  if A <> 'G' then
    raise Exception.CreateFmt('Insert order BOF last item [%s], should be G',[A]);
  //Debug('total: %d',[I]);

  //Debug('--------------- forward first-last');
  //ATable.Index := 'Forward';
  Item := ATable.First;
  I := 0;
  while not ATable.EOF do
    begin
      //Debug('Item[%d]: %s',[I,Item.Name]);
      Inc(I);
      Item := ATable.Next;
      if Assigned( Item ) then
        A := Item.Name;
    end;
  if I <> 9 then
    raise Exception.CreateFmt('Forward order EOF count %d, should be 9',[I]);
  if A <> 'I' then
    raise Exception.CreateFmt('Forward order EOF last item [%s], should be I',[A]);

  //Debug('--------------- forward last-first');
  Item := ATable.Last;
  I := 0;
  while not ATable.BOF do
    begin
      //Debug('Item[%d]: %s',[I,Item.Name]);
      Inc(I);
      Item := ATable.Prev;
      if Assigned( Item ) then
        A := Item.Name;
    end;
  //Debug('total: %d',[I]);
  if I <> 9 then
    raise Exception.CreateFmt('Forward order BOF count %d, should be 9',[I]);
  if A <> 'A' then
    raise Exception.CreateFmt('Forward order BOF last item [%s], should be A',[A]);

  //ATable.Index := 'reverse';
  Item := ATable.First;
  I := 0;
  while not ATable.EOF do
    begin
      //Debug('Item[%d]: %s',[I,Item.Name]);
      Inc(I);
      Item := ATable.Next;
      if Assigned( Item ) then
        A := Item.Name;
    end;
  if I <> 9 then
    raise Exception.CreateFmt('Reverse order EOF count %d, should be 9',[I]);
  if A <> 'A' then
    raise Exception.CreateFmt('Reverse order EOF last item [%s], should be A',[A]);

  //Debug('--------------- forward last-first');
  Item := ATable.Last;
  I := 0;
  while not ATable.BOF do
    begin
      //Debug('Item[%d]: %s',[I,Item.Name]);
      Inc(I);
      Item := ATable.Prev;
      if Assigned( Item ) then
        A := Item.Name;
    end;
  //Debug('total: %d',[I]);
  if I <> 9 then
    raise Exception.CreateFmt('Reverse order BOF count %d, should be 9',[I]);
  if A <> 'I' then
    raise Exception.CreateFmt('Reverse order BOF last item [%s], should be I',[A]);

  ATable.Free;
end; // TestBOFandEOF

procedure TestSearches;
var
  ATable : TTable;
  Item    : TTestIndexItem;
  SearchItem0, SearchItem1 : TTestIndexItem;
  I       : Integer;
  //E, A, T : String; // expected, actual, Test
const
  Items = 'GIHACDBEF';
begin
  ATable := TTable.Create;
  for I := 1 to Length(Items) do
    begin
      Item := TTestIndexItem.Create;
      Item.Name := Items[I];
      ATable.Add(Item);
    end;

  SearchItem0 := TTestIndexItem.Create;
  SearchItem0.Name := 'D';
  SearchItem1 := TTestIndexItem.Create;
  SearchItem1.Name := 'G';

  try
    //ATable.Index := 'Forward';
    Item := ATable.Find( SearchItem0 );

    if not Assigned( Item ) then
      raise Exception.Create('Item: D not found in Forward index');

    if Item.Name <> 'D' then
      raise Exception.CreateFMT('ATable.Find:  got [%s], expected [%s]',[Item.Name,SearchItem0.Name]);

    //ATable.Index := 'Forward';
    Item := ATable.Find( SearchItem0 );

    if not Assigned( Item ) then
      raise Exception.Create('Item: D not found in Reverse index');

    if Item.Name <> 'D' then
      raise Exception.CreateFMT('ATable.Find:  got [%s], expected [%s]',[Item.Name,SearchItem0.Name]);

    ATable.SetRange( SearchItem0,SearchItem1 );

    Item := ATable.First;
    I := 0;
    while not ATable.EOF do
      begin
        Debug('Item[%d]:  [%s]',[I,Item.Name]);
        Item := ATable.Next;
        Inc(I);
      end;

    if I <> 3 then
      raise Exception.CreateFmt('Range[''D'',''G'') size is %d, 3 expected',[I]);

    if ATable.SetRange(SearchItem1,SearchItem0) > 0 then
      raise Exception.Create('Range [''G'',''D'') is NOT empty');

    ATable.SetRange( SearchItem0,SearchItem1 );
  finally
    SearchItem0.Free;
    SearchItem1.Free;

    ATable.Free;
  end;
end; // TestSearches

procedure TestStoreLoad;
var
  Input  : TTextIO;
  Output : TTextIO;
  PathA, PathB: String;
  ATable, BTable : TTable;
  Item   : TTestIndexItem;
  Items  : String;
  I      : Integer;
begin
  Items := 'ABCDEFG';

  PathA := DefaultSaveLocation('','test') + 'theDatabase';
  Debug('Database store to [%s]',[PathA]);
  Output := TTextIO.Create( PathA, True );

  ATable := TTable.Create;
  for I := 1 to Length(Items) do
    begin
      Item := TTestIndexItem.Create;
      Item.Name := Items[I];
      ATable.Add( Item );
    end;
  ATable.Store( Output );

  ATable.Free;
  Output.Free;

  Input := TTextIO.Create( PathA, False );
  BTable := TTable.Load( Input ) as TTable;
  Input.Free;

  PathB := DefaultSaveLocation('','test') + 'theDatabaseCheck';
  Debug('Database store check to [%s]',[PathB]);
  Output := TTextIO.Create( PathB, True );
  BTable.Store( Output );

  BTable.Free;
  Output.Free;

  if CompareContents( PathA, PathB ) <> 0 then
    raise Exception.CreateFmt( 'File comparison failed, see %s vs. %s',[PathA,PathB] );

end; // TestStoreLoad

procedure TestStoreLoadIndices;
var
  Input  : TTextIO;
  Output : TTextIO;
  PathA  : String;
  ATable, BTable : TTable;
  ItemA, ItemB   : TTestIndexItem;
  Items  : String;
  I      : Integer;
begin
  Items := 'AR0BvC432DEsFHG';

  PathA := DefaultSaveLocation('','test') + 'theDatabase';
  Debug('Database store to [%s]',[PathA]);
  Output := TTextIO.Create( PathA, True );

  ATable := TTable.Create;
  for I := 1 to Length(Items) do
    begin
      ItemA := TTestIndexItem.Create;
      ItemA.Name := Items[I];
      ATable.Add( ItemA );
    end;
  ATable.Store( Output );

  //ATable.Free;
  Output.Free;

  Input := TTextIO.Create( PathA, False );
  BTable := TTable.Load( Input ) as TTable;
  Input.Free;

  //ATable.Index := '';
  //BTable.Index := '';

  for I := 0 to pred(ATable.Count) do
    begin
      ItemA := ATable.Items[I];
      ItemB := BTable.Items[I];
      Debug('Inserted %d:  [%s], [%s]',[I,ItemA.Name,ItemB.Name] );
      if ItemA.Name <> ItemB.Name then
        raise Exception.CreateFmt('Inserted Items[%d] (A): [%s] <> (B): [%s]',
                                  [ItemA.Name,ItemB.Name] );
    end;

  //ATable.Index := 'Forward';
  //BTable.Index := 'Forward';

  for I := 0 to pred(ATable.Count) do
    begin
      ItemA := ATable.Items[I];
      ItemB := bTable.Items[I];
      Debug('Forward %d:  [%s], [%s]',[I,ItemA.Name,ItemB.Name] );
      if ItemA.Name <> ItemB.Name then
        raise Exception.CreateFmt('Forward Items[%d] (A): [%s] <> (B): [%s]',
                                  [ItemA.Name,ItemB.Name] );
    end;

  //ATable.Index := 'Reverse';
  //BTable.Index := 'Reverse';

  for I := 0 to pred(ATable.Count) do
    begin
      ItemA := ATable.Items[I];
      ItemB := bTable.Items[I];
      Debug('Reverse %d:  [%s], [%s]',[I,ItemA.Name,ItemB.Name] );
      if ItemA.Name <> ItemB.Name then
        raise Exception.CreateFmt('Forward Items[%d] (A): [%s] <> (B): [%s]',
                                  [ItemA.Name,ItemB.Name] );
    end;

  ATable.Free;
  BTable.Free;

end; // TestStoreLoadIndices

procedure DoTest;
begin
  Debug('TestCreateFreePrimary');
  TestCreateFreePrimary;
  Debug('TestCreateFreePrimary passed');
  Debug('TestIndexOfItem');
  TestIndexOfItem;
  Debug('TestIndexOfItem passed');
  Debug('TestAddAndRemoveItems');
  TestAddAndRemoveItems;
  Debug('TestAddAndRemoveItems passed');

  Debug('TestCreateFreeIndex');
  TestCreateFreeIndex;
  Debug('TestCreateFreeIndex passed');
  Debug('TestAddAndRemoveIndexItems');
  TestAddAndRemoveIndexItems;
  Debug('TestAddAndRemoveIndexItems passed');

  Debug('TestCreateFreeTable');
  TestCreateFreeTable;
  Debug('TestCreateFreeTable passed');
  Debug('TestAddAndRemoveTableItems');
  TestAddAndRemoveTableItems;
  Debug('TestAddAndRemoveTableItems passed');

  Debug('TestSwitchBetweenIndices');
  TestSwitchBetweenIndices;
  Debug('TestSwitchBetweenIndices passed');

  Debug('TestBOFandEOF');
  TestBOFandEOF;
  Debug('TestBOFandEOF passed');

  Debug('TestSearches');
  TestSearches;
  Debug('TestSearches passed');

  Debug('TestStoreLoad');
  TestStoreLoad;
  Debug('TestStoreLoad passes');
  Debug('TestStoreLoadIndices');
  TestStoreLoadIndices;
  Debug('TestStoreLoadIndices passed');

end;

initialization
//ObjectFactory.RegisterClass( TTable );
//ObjectFactory.RegisterClass( TTestIndexItem );


end.


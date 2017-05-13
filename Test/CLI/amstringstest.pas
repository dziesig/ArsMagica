unit AMStringsTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure DoTest;

implementation

uses
  AMStrings;

procedure TestDollarsToFloat;
var
  D : Extended;
  S : String;
  NoException : Boolean;
begin
  S := '40.04';
  D := DollarsToFloat(S);
  if D <> 40.04 then
    raise Exception.Create(S +' to float failed');

  S := '$400,000.00';
  D := DollarsToFloat(S);
  if D <> 400000.0 then
    raise Exception.Create(S +' to float failed');

  S := '($400,000.00)';
  D := DollarsToFloat(S);
  if D <> -400000.0 then
    raise Exception.Create(S +' to float failed');

  S := '-$400,000.00';
  D := DollarsToFloat(S);
  if D <> -400000.0 then
    raise Exception.Create(S +' to float failed');

  S := '-400,000.00';
  D := DollarsToFloat(S);
  if D <> -400000.0 then
    raise Exception.Create(S +' to float failed');

  S := '-40.04';
  D := DollarsToFloat(S);
  if D <> -40.04 then
    raise Exception.Create(S +' to float failed');

  NoException := True;
  try
    S := '-40-.04';
    D := DollarsToFloat(S);
  except on EConvertError do
    NoException := False;
  end;
  if NoException then
    raise Exception.Create('DollarsToFloat failed to raise exception with invalid input.');

end;  // TestDollarsToFloat

procedure TestFloatToDollars;
var
  S : String;
  D : Extended;
begin
  D := 0.00;
  S := FloatToDollars(D);
  if S <> '$     0.00' then
    raise Exception.Create('FloatToDollars failed for $0.00');

  D := 0.00;
  S := FloatToDollars(D,8);
  if S <> '$   0.00' then
    raise Exception.Create('FloatToDollars failed for $0.00');

  D := 1.00;
  S := FloatToDollars(D);
  if S <> '$     1.00' then
    raise Exception.Create('FloatToDollars failed for $1.00');

  D := 1000.00;
  S := FloatToDollars(D);
  if S <> '$ 1,000.00' then
    raise Exception.Create('FloatToDollars failed for $1,000.00');

  D := 9999.99;
  S := FloatToDollars(D);
  if S <> '$ 9,999.99' then
    raise Exception.Create('FloatToDollars failed for $9,999.99');

  D := -9999.99;
  S := FloatToDollars(D);
  if S <> '$-9,999.99' then
    raise Exception.Create('FloatToDollars failed for -$9,999.99');

  D := -1.00;
  S := FloatToDollars(D);
  if S <> '$    -1.00' then
    raise Exception.Create('FloatToDollars failed for $-1.00');

  D := -1000.00;
  S := FloatToDollars(D);
  if S <> '$-1,000.00' then
    raise Exception.Create('FloatToDollars failed for $-1.000.00');

  D := 100000000.00;
  S := FloatToDollars(D);
  if S <> '$100,000,000.00' then
    raise Exception.Create('FloatToDollars failed for $100,000,000.00');

  D := -100000000.00;
  S := FloatToDollars(D);
  if S <> '$-100,000,000.00' then
    raise Exception.Create('FloatToDollars failed for $-100,000,000.00');

end; // TestFloatToDollars

procedure TestFloatToPercent;
var
  E : String; // Expected result
  D : Extended; // Value
  S : String; // Result
begin
  E := '  50.00%';
  D := 0.5;
  S := FloatToPercent(D); // Width defaults to 8
  if S <> E then
    raise Exception.CreateFmt('FloatToPercent failed. Result [%s] <> [%s]',[S,E]);

  E := '50.00%';
  D := 0.5;
  S := FloatToPercent(D,0);
  if S <> E then
    raise Exception.CreateFmt('FloatToPercent failed. Result [%s] <> [%s]',[S,E]);

  E := '1,050.00%';
  D := 10.5;
  S := FloatToPercent(D,0);
  if S <> E then
    raise Exception.CreateFmt('FloatToPercent failed. Result [%s] <> [%s]',[S,E]);

  E := ' -50.00%';
  D := -0.5;
  S := FloatToPercent(D); // Width defaults to 8
  if S <> E then
    raise Exception.CreateFmt('FloatToPercent failed. Result [%s] <> [%s]',[S,E]);

  E := '-50.00%';
  D := -0.5;
  S := FloatToPercent(D,0);
  if S <> E then
    raise Exception.CreateFmt('FloatToPercent failed. Result [%s] <> [%s]',[S,E]);

end; // TestFloatToPercent

procedure TestEmpty;
var
  S : String;
begin
  S := '';
  if not Empty(S) then
    raise Exception.CreateFmt('Empty Failed for [%s]',[S]);

  S := '     ';
  if not Empty(S) then
    raise Exception.CreateFmt('Empty Failed for [%s]',[S]);

  S := '     '#10;
  if not Empty(S) then
    raise Exception.CreateFmt('Empty Failed for [%s]',[S]);

  S := 'It''s party time!';
  if Empty(S) then
    raise Exception.CreateFmt('Empty Failed for [%s]',[S]);

end;  // TestEmpty

procedure TestStringToFloat;
var
  D : Extended;
  S : String;
  E : Extended;
begin
  E := 0.0;
  S := '';
  D := StringToFloat(S);
  if D <> E then
    raise Exception.CreateFmt('StringToFloat failed.  Expected [%f], got [%f]',[E,D]);

  E := 0.0;
  S := '   '#10;
  D := StringToFloat(S);
  if D <> E then
    raise Exception.CreateFmt('StringToFloat failed.  Expected [%f], got [%f]',[E,D]);

  E := 1000.0;
  S := '1,000'#10;
  D := StringToFloat(S);
  if D <> E then
    raise Exception.CreateFmt('StringToFloat failed.  Expected [%f], got [%f]',[E,D]);

  E := -1000.0;
  S := '-1,000'#10;
  D := StringToFloat(S);
  if D <> E then
    raise Exception.CreateFmt('StringToFloat failed.  Expected [%f], got [%f]',[E,D]);

end;  //  TestStringToFloat

procedure TestStringToInt;
var
  D : Integer;
  S : String;
  E : Integer;
begin
  E := 0;
  S := '';
  D := StringToInt(S);
  if D <> E then
    raise Exception.CreateFmt('StringToInt failed.  Expected [%f], got [%f]',[E,D]);

  E := 0;
  S := '   '#10;
  D := StringToInt(S);
  if D <> E then
    raise Exception.CreateFmt('StringToInt failed.  Expected [%d], got [%d]',[E,D]);

  E := 1000;
  S := '1,000'#10;
  D := StringToInt(S);
  if D <> E then
    raise Exception.CreateFmt('StringToInt failed.  Expected [%d], got [%d]',[E,D]);

  E := -1000;
  S := '-1,000'#10;
  D := StringToInt(S);
  if D <> E then
    raise Exception.CreateFmt('StringToInt failed.  Expected [%d], got [%d]',[E,D]);

end;  //  TestStringToInt

procedure TestUpCase;
var
  E : String; // Expected
  D : String; // result
  S : String; // Source value
begin
  E := '';
  S := '';
  D := UpCase(S);
  if D <> E then
    raise Exception.CreateFmt('UpCase Failed.  Expected[%s], got [%s]',[E,D]);

  E := 'Hello World';
  S := 'hello World';
  D := UpCase(S);
  if D <> E then
    raise Exception.CreateFmt('UpCase Failed.  Expected[%s], got [%s]',[E,D]);

  E := 'Hello world';
  S := 'hello world';
  D := UpCase(S);
  if D <> E then
    raise Exception.CreateFmt('UpCase Failed.  Expected[%s], got [%s]',[E,D]);

end;  // TestUpCase

procedure TestCreateString;
var
  E : String; // Expected
  D : String; // result
  S : Char; // Source value
begin
  E := '';
  S := 'x';
  D := CreateString(0,S);
  if D <> E then
    raise Exception.CreateFmt('CreateString Failed.  Expected[%s], got [%s]',[E,D]);

  E := 'xxx';
  S := 'x';
  D := CreateString(3,S);
  if D <> E then
    raise Exception.CreateFmt('CreateString Failed.  Expected[%s], got [%s]',[E,D]);

  E := '   ';
  S := 'x'; // Not needed
  D := CreateString(3); // Defaults to ' '
  if D <> E then
    raise Exception.CreateFmt('CreateString Failed.  Expected[%s], got [%s]',[E,D]);

end; // TestCreateString

procedure TestIndentBy;
var
  S : String;
  E : String;
  D : String;
begin
  E := '    ABC';
  S :=  'ABC';
  D := IndentBy(S,1); // 1 x 4 chars
  if D <> E then
    raise Exception.CreateFmt('IndentBy Failed.  Expected[%s], got [%s]',[E,D]);

  E := '  ABC';
  S :=  'ABC';
  D := IndentBy(S,1,2); // 1 x 2 chars
  if D <> E then
    raise Exception.CreateFmt('IndentBy Failed.  Expected[%s], got [%s]',[E,D]);

  E := 'ABC';
  S := 'ABC';
  D := IndentBy(S,0); // 0 x 4 chars
  if D <> E then
    raise Exception.CreateFmt('IndentBy Failed.  Expected[%s], got [%s]',[E,D]);

  E := 'ABC';
  S := 'ABC';
  D := IndentBy(S,-4); // -4 x 4 chars  should do nothing
  if D <> E then
    raise Exception.CreateFmt('IndentBy Failed.  Expected[%s], got [%s]',[E,D]);

end;  // TestIndentBy

procedure TestBoolToStr;
var
  E : String;
  D : String;
  S : Boolean;
begin
  S := False;
  E := 'False';
  D := BoolToStr(S);
  if D <> E then
    raise Exception.CreateFmt('BoolToStr Failed.  Expected[%s], got [%s]',[E,D]);

  S := True;
  E := 'True';
  D := BoolToStr(S);
  if D <> E then
    raise Exception.CreateFmt('BoolToStr Failed.  Expected[%s], got [%s]',[E,D]);

end;  // TestBoolToStr

procedure TestIntToStr;
var
  D, E : String;
  S : Integer;
begin
  E := '00000001';
  S := 1;
  D := IntToStr(S,8,True);
  if D <> E then
    raise Exception.CreateFmt('IntToStr Failed.  Expected[%s], got [%s]',[E,D]);

  E := '       1';
  S := 1;
  D := IntToStr(S,8,False);
  if D <> E then
    raise Exception.CreateFmt('IntToStr Failed.  Expected[%s], got [%s]',[E,D]);

  E := '-0000001';
  S := -1;
  D := IntToStr(S,8,True);
  if D <> E then
    raise Exception.CreateFmt('IntToStr Failed.  Expected[%s], got [%s]',[E,D]);

  E := '      -1';
  S := -1;
  D := IntToStr(S,8,False);
  if D <> E then
    raise Exception.CreateFmt('IntToStr Failed.  Expected[%s], got [%s]',[E,D]);

  E := '-0001001';
  S := -1001;
  D := IntToStr(S,8,True);
  if D <> E then
    raise Exception.CreateFmt('IntToStr Failed.  Expected[%s], got [%s]',[E,D]);

  E := '   -1001';
  S := -1001;
  D := IntToStr(S,8,False);
  if D <> E then
    raise Exception.CreateFmt('IntToStr Failed.  Expected[%s], got [%s]',[E,D]);

end; // TestIntToStr

procedure TestUSPhone;
var
  D, E, S : String;
begin
  E := '(407)733-7771';
  S := '(407)733-7771';
  D := USPhone(S);
  if D <> E then
    raise Exception.CreateFmt('USPhone Failed.  Expected[%s], got [%s]',[E,D]);

  E := '(407)733-7771 x 19';
  S := '4077337771 ext 19';
  D := USPhone(S);
  if D <> E then
    raise Exception.CreateFmt('USPhone Failed.  Expected[%s], got [%s]',[E,D]);

  E := '(407)733-7771 x 1000';
  S := '40773377711000';
  D := USPhone(S);
  if D <> E then
    raise Exception.CreateFmt('USPhone Failed.  Expected[%s], got [%s]',[E,D]);

end;

procedure DoTest;
begin
  try
 // First since needed by following routines
    TestCreateString;
    Writeln('CreateString passed');
    TestEmpty;
    WriteLn('Empty passed');

 // Users of CreateString or Empty
    TestDollarsToFloat;
    WriteLn('DollarsToFloat passed');
    TestFloatToDollars;
    WriteLn('FloatToDollars passed');
    TestFloatToPercent;
    WriteLn('FloatToPercent passed');
    TestEmpty;
    WriteLn('Empty passed');
    TestStringToFloat;
    WriteLn('StringToFloat passed');
    TestStringToInt;
    WriteLn('StringToInt passed');
    TestUpCase;
    WriteLn('TestUpCase passed');
    TestIndentBy; // Only tests the string version, not the file version
    WriteLn('TestIndentBy passed');
    TestBoolToStr;
    WriteLn('TestBoolToStr passed');
    TestIntToStr;
    WriteLn('TestIntToStr passed');
    TestUSPhone;
    WriteLn('TestUSPhone passed');
  except on E : Exception do
    Writeln(E.Message);
  end;
end;

end.


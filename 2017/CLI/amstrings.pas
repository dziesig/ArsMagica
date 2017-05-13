{ <description>

  Copyright (C) 1995 .. 2017 Donald R. Ziesig donald@ziesig.org

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

// ArsMagica Strings (derived from MagicLibrary Stringsubs, with FPC String
// handling routines removed).

unit AMStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{------------------------------------------------------------------------------}
{ Interprets string (as output by floatToStrF( ... ffCurrency ...) and returns }
{ a float.  Raises exception if invalid format.  ( empty string = 0.00 )       }
{------------------------------------------------------------------------------}
function DollarsToFloat( Value : String ) : Extended;

{------------------------------------------------------------------------------}
{ Returns a Dollar string with the $ at a constant position (Width).           }
{------------------------------------------------------------------------------}
function FloatToDollars( Value : Extended; Width : Integer = 10 ) : String;

{------------------------------------------------------------------------------}
{ Converts a fraction (e.g. 0.5) to a string representing a percent            }
{ (e.g. 50.0%)                                                                 }
{------------------------------------------------------------------------------}
function FloatToPercent( Value : Extended; Width : Integer = 8 ) : String;

{------------------------------------------------------------------------------}
{ Interprets string as percent (ignores %) (e.g. 90% -> 0.90, empty = 0.0)     }
{------------------------------------------------------------------------------}
function PercentToFloat( Value : String ) : Extended;

{------------------------------------------------------------------------------}
{ Empty strings have no non-printing characters (e.g. tabs, whitespace)        }
{------------------------------------------------------------------------------}
function Empty( S : String ) : Boolean;

{------------------------------------------------------------------------------}
{ Same as StrToFloat and StrToInt except Empty String returns 0                }
{------------------------------------------------------------------------------}
function StringToFloat( Value : String ) : Extended;
function StringToInt( Value : String ) : Integer;

{------------------------------------------------------------------------------}
{ Capitalizes first character of string                                        }
{------------------------------------------------------------------------------}
function UpCase( S : String ) : String;

{------------------------------------------------------------------------------}
{ Creates a string of N-C characters                                           }
{------------------------------------------------------------------------------}
function CreateString( N : Integer; C : Char = ' ' ) : String;

{------------------------------------------------------------------------------}
{ Indents result (String or File Line) by Count * BaseIndent spaces            }
{ Does NOT take into account pre-existing characters (a la Tabs)               }
{------------------------------------------------------------------------------}
procedure IndentBy( var F : TextFile; Count : Integer; BaseIndent : Integer = 4 );
function  IndentBy( const Text : String; Count : Integer; BaseIndent : Integer = 4 ) : String; overload;

function BoolToStr( Value : Boolean ) : String;

function IntToStr( Value : Integer; Width : Integer; ZeroPad : Boolean = False ) : String; overload;

function USPhone( Value : String ) : String;

implementation

uses
  StrUtils, Math;

function BoolToStr(Value: Boolean): String;
begin
  Result := IfThen(Value,'True','False');
end;

function CreateString(N: Integer; C: Char): String;
var
  I : Integer;
begin
  Result := '';
  for I := 1 to N do
    Result := Result + C;
end;


function DollarsToFloat(Value: String): Extended;
var
  I, L : Integer;
  S : String;
  Negative : Boolean;
  C0, C1 : Char;
begin
  S := '';
  Value := Trim(Value);
  L := Length(Value);
  C0 := Value[1];
  C1 := Value[L];
  Negative := False;
  if (C0 = '(') and (C1 = ')') then
    begin
      Negative := True;
      Value := LeftStr( Value,L-1 );
      Value := RightStr( Value, L-2 );
    end;

  for I := 1 to Length( Value ) do
    begin
      if Value[I] in ['0'..'9','.','+','-'] then
        S := S + Value[I]
      else
        if not (Value[I] in [',','$']) then
          raise Exception.Create( 'Value is not a valid USD value' );
    end;
  Result := StrToFloat(S);
  if Negative then Result := -Result;
  Result := RoundTo(Result,-2);
end;

function Empty(S: String): Boolean;
begin
  Result := IsEmptyStr( S, [#0..#31,' ']);
end;

function FloatToDollars(Value: Extended; Width: Integer): String;
var
  Wid  : Integer;
begin
  Result := FloatToStrF( Value, ffNumber,Width,2);
  Wid := Width - Length(Result);
  Result := '$' +  CreateString(Wid-1,' ') + Result;
end;

function FloatToPercent(Value: Extended; Width: Integer): String;
var
  Temp : String;
  Dec  : Integer;
  I    : Integer;
  Wid  : Integer;
begin
  Temp := FloatToStrF( Value * 100.0, ffFixed,Width,2);
  Dec  := Pos('.',Temp);
  Result := Copy(Temp,Dec,3);
  for I := 1 to pred(Dec) do
    begin
      Result := Temp[Dec - I] + Result;
      if ((I mod 3) = 0) and (I < pred(Dec)) then
        Result := ',' + Result;
    end;
  Wid := Width - Length(Result);
  if Wid > 0 then
    for I := 1 to pred(Wid) do
      Result := ' ' + Result;
  Result := Result + '%';
end;

procedure IndentBy(var F: TextFile; Count: Integer; BaseIndent: Integer);
begin
  Write(F,CreateString(Count*BaseIndent,' '));
end;

function IndentBy(const Text: String; Count: Integer; BaseIndent: Integer
  ): String;
begin
  Result := CreateString(Count * BaseIndent,' ');
  Result := Result + Text;
end;

{ TODO -odonz -cSimplification : Simplify the following code.  It is much too complex for the function it performs. }
function IntToStr(Value: Integer; Width: Integer; ZeroPad: Boolean): String;
var
  Positive : Boolean;
  L : Integer;
begin
  Positive := Value >= 0;
  Result := IntToStr(Value);
  L := Length(Result);
  if L >= Width then exit;
  if Positive then
    if ZeroPad then
      Result := CreateString(Width-L,'0') + Result
    else
      Result := CreateString(Width-L,' ') + Result
  else
    if ZeroPad then
      Result := '-' + CreateString(Width-L,'0') + RightStr(Result,L-1)
    else
      Result := CreateString(Width-L,' ') + Result;

end; // IntToStr

function PercentToFloat(Value: String): Extended;
var
  I : Integer;
  S : String;
begin
  S := '';
  for I := 1 to Length( Value ) do
    begin
      if Value[I] in ['0'..'9','.','+','-'] then
        S := S + Value[I]
      else
        if not (Value[I] in [',','%']) then
          raise Exception.Create( 'Value is not a valid Percentage value' );
    end;
  Result := Trunc(StringToFloat(S) * 100) / 10000.0;
end;


function StringToFloat(Value: String): Extended;
var
  I : Integer;
  S : String;
begin
  Result := 0.0;
  Value := Trim( Value );
  if Empty( Value ) then exit;
  S := '';
  for I := 1 to Length( Value ) do
    begin
      if Value[I] in ['0'..'9','.','+','-'] then
        S := S + Value[I]
      else
        if not (Value[I] in [',']) then
          raise Exception.Create( 'Value is not a valid Float value' );
    end;
  if not Empty( S ) then Result := StrToFloat( S );
end;

function StringToInt(Value: String): Integer;
var
  I : Integer;
  S : String;
begin
  Value := Trim( Value );
  if Empty(Value) then
    Result := 0
  else
    begin
      S := '';
      for I := 1 to Length( Value ) do
        begin
          if Value[I] in ['0'..'9','+','-'] then
            S := S + Value[I]
          else
            if not (Value[I] in [',']) then
              raise Exception.Create( 'Value is not a valid Integer value' );
        end;

      Result := StrToInt( S );
    end;
end;

function UpCase(S: String): String;
begin
  if not Empty( S ) then
  if S[1] in ['a'..'z'] then
    S[1] := chr( ord( S[1] ) - ord('a') + ord('A'));
  Result := S;
end;

function USPhone(Value: String): String;
var
  Numbers : String;
  I       : Integer;
  L       : Integer;
begin
  Numbers := '';
  for I := 1 to Length(Value) do
    if Value[I] in ['0'..'9'] then
      Numbers := Numbers + Value[I];
  // Give it the best shot
  if not Empty(Numbers) then
    Result := '(';
  L := Length(Numbers);
  for I := 1 to L do
    begin
      Result := Result + Numbers[I];
      if I = 3 then
        Result := Result + ')'
      else if I = 6 then
        Result := Result + '-'
      else if (I = 10) and (L > 10) then
        Result := Result + ' x ';
    end;
end;

end.


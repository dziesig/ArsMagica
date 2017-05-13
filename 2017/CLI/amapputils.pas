{ AMAppUtils - General support for Application info.

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
unit AMAppUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{ Returns (after creating, if necessary) the path to the Default Save Location }
{ for this App.                                                                }
function DefaultSaveLocation( AppName : String = ''; WorkingDir : String = '' ): string;

{ This is here because even after using TurboPascal from version 1.0 til now,  }
{ I never can remember if it is ParamStr(0) or ParamStr[0].                    }
function ExePath : String;

{ Strips File Type (extension) if necessary }
function ExeName : String;

function BuildDateTime : String;

implementation

uses
  AMStrings;

function DefaultSaveLocation(AppName: String; WorkingDir: String): string;
begin
  Result := GetUserDir;
  if Empty( AppName ) then
    Result := Result + ApplicationName + DirectorySeparator
  else
    Result := Result + AppName + DirectorySeparator;
  if not Empty( WorkingDir ) then
    Result := Result + WorkingDir + DirectorySeparator;

  if not DirectoryExists( Result ) then
    ForceDirectories( Result );
end;

function ExePath: String;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function ExeName: String;
var
  P : Integer;
begin
  Result := ExtractFileName(ParamStr(0));
  P := Pos('.',Result);
  if P > 0 then
    Result := Copy(Result,1,P-1);
end;

function BuildDateTime: String;
var
  fa : LongInt;
  BD : TDateTime;
begin
  fa:=FileAge(ParamStr(0));
  BD := FileDateTodateTime(fa);
  Result := FormatDateTime('dddddd tt',bd);
end;


end.


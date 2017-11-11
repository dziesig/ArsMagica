{ General support comparing files.

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
unit AMFileUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

  function CompareContents( PathA, PathB : String ) : Integer;

implementation

function CompareContents(PathA, PathB: String): Integer;
var
  StreamA, StreamB : TFileStream;
  I                : Integer;
begin
  StreamA := TFileStream.Create( PathA, fmOpenRead, fmShareExclusive );
  StreamB := TFileStream.Create( PathB, fmOpenRead, fmShareExclusive );

  Result := StreamB.Size - StreamA.Size;

  if Result = 0 then
    begin
      for I := 0 to pred( StreamA.Size ) do
        begin
          Result := StreamB.ReadByte - StreamA.ReadByte;
          if Result <> 0 then
            break;
        end;
    end;

  StreamB.Free;
  StreamA.Free;
end;

end.


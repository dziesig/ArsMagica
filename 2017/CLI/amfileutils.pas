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


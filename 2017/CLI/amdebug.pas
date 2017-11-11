{ <Debugging and Logging Support>

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
unit AMDebug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Debug( const Message : String); overload;
procedure Debug( const Fmt : String; const Args: array of const); overload;
procedure DebugLn( const Message : String = '');
procedure DebugLn( const Fmt : String; const Args: array of const); overload;

procedure DebugMsg( const Message : String); overload;
procedure DebugMsg( const Fmt : String; const Args: array of const); overload;


implementation

uses
  LCLProc, lnfodwrf, Dialogs,
  AMStrings;

function FileLineString( Frame : Pointer ) : String;
var
  PCode : CodePointer;
  Func  : ShortString;
  theLine  : LongInt;
  Src   : ShortString;
  SourceFileName : String;
begin
  Result := '#####';
  try
    PCode := get_caller_addr( Frame );
    if Assigned( PCode ) then
      begin
        if GetLineInfo( PtrUint(PCode), Func, Src, theLine ) then
          begin
            SourceFileName := ChangeFileExt( ExtractFileName( Src ), '');
            Result := Format( '%s.%d',
                            [SourceFileName, theLine] );
            Result := Format( '%-30s',[Result]);
          end;
      end
    except
      Result := 'Error getting caller';
    end;
end;

procedure Debug(const Message: String);
var
  Msg : String;
  FLS : String;
  RightLen, LeftLen : Integer;
  //Ptr : Pointer;
begin
  FLS := FileLineString( get_frame );
  RightLen := Length(Message);
  LeftLen  := Length( FLS );
  if (LeftLen + RightLen) > 120 then
    begin
      DebugLn(FLS);
      DebugLn('  ' + Message)
    end
  else
    begin
      Msg := Format('%-30s %s',[FLS,Message]);
      DebugLn( Msg );
    end;
  //Msg := Message;
  //DbgOut( [ FileLineString( get_frame ) ] ); DBgOut([#13]) ;
//{$ifdef WIN32}
//  OutputDebugString( PChar(Msg) );
//{$else}
//  DbgOut( Msg + #13); //#10 );
//{$endif}
end;

procedure Debug(const Fmt: String; const Args: array of const );
var
  Msg : String;
  FLS : String;
  RightLen, LeftLen : Integer;
begin
  Msg := Format( Fmt, Args );
  FLS := FileLineString( get_frame );
  RightLen := Length( Msg );
  LeftLen  := Length( FLS );
  if (LeftLen + RightLen) > 120 then
    begin
      DebugLn(FLS);
      DebugLn('  ' + Msg)
    end
  else
    begin
      Msg := Format('%-22s %s',[FLS,Msg]);
      DebugLn( Msg );
    end;
end;

procedure DebugLn(const Fmt: String; const Args: array of const );
var
  Msg : String;
begin
  Msg := Format( Fmt, Args );
  DebugLn(Msg);
end;

procedure DebugLn(const Message: String);
begin
  LCLProc.DebugLn( Message );
end;

procedure DebugMsg(const Message : String);
begin
  ShowMessage( Message );
end;

procedure DebugMsg(const Fmt : String; const Args : array of const);
begin
  DebugMsg( Format( Fmt, Args ) );
end;

end.


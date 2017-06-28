unit AMDebug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Debug( const Message : String); overload;
procedure Debug( const Fmt : String; const Args: array of const); overload;
procedure DebugLn( const Message : String = '');

implementation

uses
  LCLProc, lnfodwrf,
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
  if (LeftLen + RightLen) > 75 then
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
  if (LeftLen + RightLen) > 75 then
    begin
      DebugLn(FLS);
      DebugLn('  ' + Msg)
    end
  else
    begin
      Msg := Format('%-30s %s',[FLS,Msg]);
      DebugLn( Msg );
    end;
end;

procedure DebugLn(const Message: String);
begin
  LCLProc.DebugLn( Message );
end;

end.


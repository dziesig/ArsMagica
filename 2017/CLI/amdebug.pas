unit AMDebug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Debug( const Message : String ); overload;
procedure Debug( const Value : Integer ); overload;
procedure Debug( const Value : Double ); overload;
procedure Debug( const Fmt : String; const Args : array of const);
procedure DebugLn( const Message : String = '');

implementation

uses
  LCLProc;

procedure Debug(const Message: String);
begin
{$ifdef WIN32}
  OutputDebugString( PChar(Message) );
{$else}
  DbgOut( Message + #13); //#10 );
{$endif}
end;

procedure Debug(const Value: Integer);
begin
  DbgOut( IntToStr( Value ) );
end;

procedure Debug(const Value: Double);
begin
  DbgOut( FloatToStr( Value ) );
end;

procedure Debug(const Fmt: String; const Args: array of const);
begin
  DbgOut( Format( Fmt, Args ) + #10);
end;

procedure DebugLn(const Message: String);
begin
  LCLProc.DebugLn( Message );
end;

end.


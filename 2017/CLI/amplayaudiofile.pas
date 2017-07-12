unit AMPlayAudioFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure PlayAudioFile( FilePath, FileName : String );
procedure StopAudioFile;

implementation

uses
  Dos, Process,
  AMStrings, AMDebug;

var
  Player : TProcess;

procedure PlayAudioFile(FilePath, FileName : String );
var
  PlayerPath : String;
  FullPath : String;
begin
  PlayerPath := GetEnv( 'AudioPlayer' );
  if Empty( PlayerPath ) then
    begin
      Debug('Environment variable [%s] undefined.',[PlayerPath]);
      exit;
    end;
  FullPath := '"' + FilePath + DirectorySeparator + FileName + '"';
  //Player := TProcess.Create( nil );
  //try
    Player.CommandLine := PlayerPath + ' ' + FullPath;
    Player.Execute;
  //finally
  //  Player.Free;
  //end;
end;

procedure StopAudioFile;
begin
  Player.Terminate(1);
end;

initialization
  Player := TProcess.Create( nil );
finalization
  Player.Free;

end.


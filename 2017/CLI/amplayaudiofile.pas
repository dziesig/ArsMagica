unit AMPlayAudioFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure PlayAudioFile( FilePath, FileName : String );

implementation

uses
  Dos, Process,
  AMStrings, AMDebug;

procedure PlayAudioFile(FilePath, FileName : String );
var
  Player : TProcess;
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
  Player := TProcess.Create( nil );
  try
    Player.CommandLine := PlayerPath + ' ' + FullPath;
    Player.Execute;
  finally
    Player.Free;
  end;
end;

end.


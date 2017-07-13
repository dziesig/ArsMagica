unit AMPlayAudioFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

type

  { TAudioFilePlayer }

  TAudioFilePlayer = class(TProcess)
    private
      fPlayerPath : String;
    public
      constructor Create( thePlayerPath : String );
      destructor  Destroy; override;

      procedure Play( FilePath, FileName : String );
      procedure Stop;

      property PlayerPath : String read fPlayerPath;
  end;

implementation

uses
  Dos,
  AMStrings, AMDebug;

//var
//  Player : TProcess;
//
//procedure xPlayAudioFile(FilePath, FileName : String );
//var
//  PlayerPath : String;
//  FullPath : String;
//begin
//  PlayerPath := GetEnv( 'AudioPlayer' );
//  if Empty( PlayerPath ) then
//    begin
//      Debug('Environment variable [%s] undefined.',[PlayerPath]);
//      exit;
//    end;
//  FullPath := '"' + FilePath + DirectorySeparator + FileName + '"';
//  //Player := TProcess.Create( nil );
//  //try
//    Player.CommandLine := PlayerPath + ' ' + FullPath;
//    Player.Execute;
//  //finally
//  //  Player.Free;
//  //end;
//end;
//
//procedure xStopAudioFile;
//begin
//  Player.Terminate(1);
//end;

{ TAudioFilePlayer }

constructor TAudioFilePlayer.Create(thePlayerPath : String);
begin
  inherited Create( nil );
  fPlayerPath := thePlayerPath;
end;

destructor TAudioFilePlayer.Destroy;
begin
  inherited Destroy;
end;

procedure TAudioFilePlayer.Play(FilePath, FileName : String);
var
  vFullPath : String;
begin
  vFullPath := '"' + FilePath + DirectorySeparator + FileName + '"';
  CommandLine := PlayerPath + ' ' + vFullPath;
  Execute;
end;

procedure TAudioFilePlayer.Stop;
begin
  Terminate(1);
end;

//initialization
//  Player := TProcess.Create( nil );
//finalization
//  Player.Free;

end.


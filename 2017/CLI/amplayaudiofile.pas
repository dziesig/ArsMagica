{ AMPlayAudioFile.  An interface to programattically play audio files.

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


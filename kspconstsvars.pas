{
--------------------------------------------------------------------
Copyright (c) 2009 KSP Developers Team
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

unit KSPConstsVars;

{$I ksp_version.inc}

interface

uses FileSupportLst, SysUtils, BASSPlayer, Graphics, Classes, PresetsU,
  Playlists, app_db_utils, LCLType, ksplua;

const
  KSPHost    = 'http://ksplayer.com';
  KSPHost2   = KSPHost + '/?kmajor=%s&kminor=%s&krelease=%s&kbuild=%s&from_ksp=1';
  KSPHowHelp = KSPHost + '/help-wanted';
  KSPSupportURL = 'http://project.ksplayer.com';
  KSPUpdates = 'http://ksplayer.com/updates.php?kbuild=';
  KSPTellAFriend =
    'http://www.freetellafriend.com/tell/?u=5653&title=KSPSoundPlayer&url=http://ksplayer.com';

  KSPMDir    = KSPHost + '/music-links';
  KSPMDirAdd = KSPMDir + '/add';
  KSPForum   = KSPHost + '/support/forum';

  DefSetupFileName = 'data\setup.opt';
  //KSPCopyrightNote = 'Copyright ® 2009 KSP Developer Team';
  //  KSPMsgDefaultServerPort = 12007;
  //  KSPSupportAutomatedMessage = 'This is an automated message sent from KSP';
  //  KSPSupportAutomatedSubject = 'KSP automated bug report';

  //  BackColor = clBlack;
  BlockWidth = 6;
  VLimit     = 59;

  HBlockCount = NumFFTBands;
  HBlockGap   = 1;

  KSPPlaylistsVersion = '1';

  CDefPlaylistFormat   = '[%title] - [%artist] - [%album] ([%length])';
  CFormatedHintInfo    = '[%title] ([%length])- [%artist] - [%album]';
  CDefSuggHintFormat   = '[%title]' + #13 + '[%artist]' + #13 + '[%album]';
  CFormatedHintInfoPls = '[%title] ([%length])- [%artist] - [%album]\n[%comment]';

  DB_MAX_THREADS  = 1;
  DB_MAX_THREADS2 = 1;

  //To be var
  MaxSuggestions = 10;
  TopItems   = 10;
  VDJTimeOut = 5; //60sec

const
  art     = '[%artist]';
  album   = '[%album]';
  title   = '[%title]';
  genre   = '[%genre]';
  year    = '[%year]';
  comment = '[%comment]';
  track   = '[%track]';

//database

const
  InsStat = 'INSERT INTO meta (GID, Track, Comment, metaYear, Album,' +
    ' Artist, Title, PlayedEver, Meta, PlayCount, Fav, LastPlay, FirstPlay,' +
    ' FileName, Genre) Values(%s, %s, ''%s'', ''%s'', ''%s'', ''%s''' +
    ', ''%s'', %s, %s, %s, %s, ''%s'', ''%s'', ''%s'', ''%s'')';

  InsStatNew = 'INSERT INTO meta (GID, Track, Comment, metaYear, Album,' +
    ' Artist, Title, PlayedEver, Meta, PlayCount, Fav, LastPlay, FirstPlay,' +
    ' FileName, Genre) Values(%s, %s, ''%s'', ''%s'', ''%s'', ''%s''' +
    ', ''%s'', %s, %s, %s, %s, ''%s'', ''%s'', ''%s'', ''%s'');';

  UpdateStat = 'UPDATE meta SET GID = %s, Track = %s, Comment = ''%s''' +
    ', metaYear = ''%s'', Album = ''%s'', Artist = ''%s'', Title = ''%s''' +
    ', PlayedEver = %s, Meta = %s, PlayCount =%s, Fav =%s, LastPlay = ''%s''' +
    ', FirstPlay = ''%s'', FileName = ''%s'', Genre = ''%s'' WHERE FileName = ''%s''';

  SelectGetItem     = 'SELECT * FROM meta WHERE FileName=''%s''';
  SelectGetItemLike = 'SELECT * FROM meta WHERE FileName LIKE ''%s''';

  RemoveItem     = 'DELETE FROM meta WHERE FileName = ''%s''';
  RemoveItemDupl = 'DELETE FROM meta WHERE FileName = ''%s'' AND PlayCount=%s';

  InsLyrics    = 'INSERT INTO lyrics (lyric, item_id) VALUES (''%s'', %s)';
  SelectLyrics = 'SELECT * FROM lyrics WHERE item_id=%s';
  DelLyrics    = 'DELETE FROM lyrics WHERE item_id=%s';

  DB_VERSION = 5;

{$IFNDEF WINDOWS}
  KSP_APP_FOLDER = '/usr/share/KSP/';
{$ENDIF}

  MAX_PATH_KSP = 4096;

var
  SaveRemFolder: string;
  SetupFileName: string;// = 'data\setup.opt';
  HotKeyList:    TList;
  KSPDataFolder: string;
  KSPVersion2:   string;
  KSPVersion:    string;
  AllSongs:      TAppDBConnection;
  SuggestionList: TPlayList;
  SuggFindHelpPlaylist: TPlaylist;

  ScriptedAddons: TAddonManager;
  DefaultScript:  string;

  FindApproxVals: TPlayList;
  EqList: TEqList;
  InsertingDataInThread: boolean;
  KSPAssociatedFiles: TStringList;
  AlreadyEncoding: boolean;
  KSPStartupTime: TDateTime;
  KSPLogFilename: string;
  KSPPluginsBlacklist: string;

const
  ASKey = '053e8f8c040a00ba100a2eaf5aa17fd9';
  ASTrackFeed =
    'ws.audioscrobbler.com/2.0/?method=track.getsimilar&artist=%s&track=%s&api_key=' + ASKey;

var
  Player: TBassPlayer;
  OSName: string;
 //  KSPUserEmail: string;
 //  KSPUserName: string;

var
  FileSupportList: TFileSupportList;

  PeakValue:     array[1..NumFFTBands] of single;
  PassedCounter: array[1..NumFFTBands] of integer;

  //Semaphores
  GetCountSem2: integer;
  LoadPlsSem2:  integer;

  DBCritSection: LCLType.TCriticalSection;
  KSPCPUID:      string;
  KSPMP3SettingsList: TStringList;

implementation

end.

unit KSPConstsVars;

interface

uses Messages, FileSupportLst, SysUtils, BASSPlayer,
  Graphics, ID3Mgmnt, Classes, KSPTypes, MediaItems,
  Playlists, app_db_utils;

const
  KSPHost = 'http://ksplayer.boo.pl';
  KSPHowHelp = 'http://www.ksplayer.boo.pl/help-wanted';
  KSPSupportURL = 'http://code.google.com/p/kspnew/issues/list';
  NetworkStreams = 'http://dir.xiph.org/yp.xml';
  DefSetupFileName = 'data\setup.opt';
  KSPCopyrightNote = 'Copyright ® 2009 KSP Developer Team';
  KSPMsgDefaultServerPort = 12007;
  KSPSupportAutomatedMessage = 'This is an automated message sent from KSP';
  KSPSupportAutomatedSubject = 'KSP automated bug report';

//  BackColor = clBlack;
  BlockWidth = 6;
  VLimit = 59;

  HBlockCount = NumFFTBands;
  HBlockGap = 1;

  KSPPlaylistsVersion ='1';

  CDefPlaylistFormat = '[%title] - [%artist] - [%album] ([%length])';
  CFormatedHintInfo = '[%title] ([%length])- [%artist] - [%album]';
  CDefSuggHintFormat = '[%title]'+#13+'[%artist]'+#13+'[%album]';
  CFormatedHintInfoPls = '[%title] ([%length])- [%artist] - [%album]\n[%comment]';

//Messenger
  MSG_Error_RegisterNewUser_NotConnected        = 60026;
  MSG_Error_RegisterNewUser_SendCommandFailed   = 60027;
  MSG_Error_RegisterNewUser_ReceiveResultFailed = 60028;
  MSG_Error_RegisterNewUser_InvalidParams       = 60029;
  MSG_Error_RegisterNewUser_InvalidServerReply  = 60030;
  MSG_Error_RegisterNewUser_Failed              = 60031;
  MSG_Error_RegisterNewUser_UserAlreadyExists   = 60032;

  DB_MAX_THREADS = 1;
  DB_MAX_THREADS2 = 1;

//To be var
  MaxSuggestions = 10;
  TopItems = 10;
  VDJTimeOut = 5; //60sec
  KSPRotateAdInterval = 1000 * 60 * 60 * 3;

const
  art='[%artist]';
  album='[%album]';
  title = '[%title]';
  genre = '[%genre]';
  year = '[%year]';
  comment = '[%comment]';
  track = '[%track]';

//database
const
  DBMemUsage = 0.005;
  DBMemUsageDBSize = 0.25;
  
const
  InsStat = 'INSERT INTO meta (GID, Track, Comment, metaYear, Album,'+
      ' Artist, Title, PlayedEver, Meta, PlayCount, Fav, LastPlay, FirstPlay,'+
      ' FileName, Genre) Values(%s, %s, ''%s'', ''%s'', ''%s'', ''%s'''+
      ', ''%s'', %s, %s, %s, %s, ''%s'', ''%s'', ''%s'', ''%s'')';

  InsStatNew = 'INSERT INTO meta (GID, Track, Comment, metaYear, Album,'+
      ' Artist, Title, PlayedEver, Meta, PlayCount, Fav, LastPlay, FirstPlay,'+
      ' FileName, Genre) Values(%s, %s, ''%s'', ''%s'', ''%s'', ''%s'''+
      ', ''%s'', %s, %s, %s, %s, ''%s'', ''%s'', ''%s'', ''%s'');';

  UpdateStat = 'UPDATE meta SET GID = %s, Track = %s, Comment = ''%s'''+
      ', metaYear = ''%s'', Album = ''%s'', Artist = ''%s'', Title = ''%s'''+
      ', PlayedEver = %s, Meta = %s, PlayCount =%s, Fav =%s, LastPlay = ''%s'''+
      ', FirstPlay = ''%s'', FileName = ''%s'', Genre = ''%s'' WHERE FileName = ''%s''';

  SelectGetItem = 'SELECT * FROM meta WHERE FileName=''%s''';
  SelectGetItemLike = 'SELECT * FROM meta WHERE FileName LIKE ''%s''';

  RemoveItem = 'DELETE FROM meta WHERE FileName = ''%s''';
  RemoveItemDupl = 'DELETE FROM meta WHERE FileName = ''%s'' AND PlayCount=%s';

  DB_VERSION = 5;

var
  SaveRemFolder: string;
  SetupFileName: string;// = 'data\setup.opt';
  HotKeyList: TList;
  KSPDataFolder: string;
  KSPVersion2: string;
  KSPVersion: string;
  AllSongs: TAppDBConnection;
  SuggestionList: TPlayList;
  SuggFindHelpPlaylist: TPlaylist;
  FindApproxVals: TPlayList;
  InsertingDataInThread: boolean;
  KSPAssociatedFiles: TStringList;
  AlreadyEncoding: boolean;
  KSPStartupTime: TDateTime;
  KSPLogFilename: string;
  KSPPluginsBlacklist: string;

var
  Player: TBassPlayer;
//  KSPUserEmail: string;
//  KSPUserName: string;

var
  FileSupportList: TFileSupportList;

  PeakValue : array[1..NumFFTBands] of single;
  PassedCounter : array[1..NumFFTBands] of integer;

//Semaphores
  {GetCountSem: THandle;
  LoadPlsSem: THandle;
  StartupThreadSem: THandle;
  LoadOptionsSem: THandle;
  LoadVarsSem: THandle;
  CreateObjectsSem: THandle;}
  GetCountSem2: integer;
  LoadPlsSem2: integer;
  StartupThreadSem2: integer;
  LoadOptionsSem2: integer;
  LoadVarsSem2: integer;
  CreateObjectsSem2: integer;

  KSPDatabaseThreads: integer;
  KSPDatabaseThreadsInternal: integer;
  KSPCPUID: string;
  KSPMP3SettingsList: TStringList;

implementation

end.

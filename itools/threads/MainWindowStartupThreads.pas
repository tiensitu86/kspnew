unit MainWindowStartupThreads;

interface

uses
  Classes, SysUtils, KSPTypes, DateUtils, Forms, StrUtils, Graphics, LCLIntf,
  BookmarksU;

type
  TCreateObjectsThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  end;

  TSetVarsThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  end;

implementation

uses Main, IniFiles, BASS, KSPConstsVars, BASSPlayer,
  Playlists, PresetsU, MediaFolders, KSPMessages, kspfiles, MRNG, MultiLog;

procedure TCreateObjectsThread.Execute;
begin
  { Place thread code here }
    hLog.Send('Creating objects');
//    KSPMainWindow.Prevent:=TStringList.Create;
    KSPMainWindow.BookmarksList:=TBookmarksList.Create;
    KSPMainWindow.BookmarksList.LoadFromFile(KSPDataFolder+'data\bookmarks.xml');
    //SysIconDefault:=TIcon.Create;
    //SysIconDefault:=spTrayIcon1.Icon;
    //CDDatabaseForm:=TCDDatabaseForm.Create(nil);
    KSPMainWindow.PlayList:=TPlayList.Create;
    SuggestionList:=TPlayList.Create;
    SuggFindHelpPlaylist:=TPlaylist.Create;
    FindApproxVals:=TPlayList.Create;
    
    HotKeyList := TList.Create;
    KSPMainWindow.MediaFoldersList:=TMediaFoldersList.Create;
    KSPMainWindow.MediaFoldersList.LoadFromFile(KSPDataFolder+'data\MediaLib.xml');
    KSPMainWindow.MediaSongs:=TPlayList.Create;

    KSPMP3SettingsList:=TStringList.Create;
    KSPMainWindow.Forbidden:=TStringList.Create;
    if FileExists(KSPDataFolder+'data\vdj\last') then
      KSPMainWindow.Forbidden.LoadFromFile(KSPDataFolder+'data\vdj\last');
    
    hLog.Send('Creating objects done');

    //ForceDirectories(ExtractFilePath(Application.ExeName)+'temp\update');
    //WebUpdate1.TempDirectory:=ExtractFilePath(Application.ExeName)+'temp\update';

//    Cddb.LocalCDDBPath := ExtractFilePath(Application.ExeName)+'data\cddb\';

    CreateObjectsSem2:=0;//ReleaseSemaphore(CreateObjectsSem, 1, nil);
end;

procedure TSetVarsThread.Execute;
begin
  { Place thread code here }
    hLog.Send('Setting vars');
    //KSPMainWindow.DSPPlay:=false;
    KSPPluginsBlacklist:=KSPDataFolder+'plgdisabled.lst';
    KSPMainWindow.LoadingPlaylist:=false;
    Application.ShowHint:=true;
//    KSPMainWindow.AlertVisible:=false;

//    KSPMainWindow.LoadingMediaLibInfo:=false;
//    KSPMainWindow.MaxiHeight:=KSPMainWindow.ClientHeight;
    //KSPMainWindow.ForceRescan:=false;
    KSPMainWindow.PlayedPrevious:=false;

    KSPMainWindow.TimeFormat:=tfElapsed;

    KSPMainWindow.Seeking:=false;
    KSPMainWindow.PlayListMove:=false;

    //KSPMainWindow.RipJobs.Encode:=false;
    //KSPMainWindow.RipJobs.Rip:=false;

    hLog.Send('Loading randomizer...');
    
    Randomize;
    AlreadyEncoding:=false;

    //if TheNewestKSP<>'' then begin
    //    KSPPortalURL:=Format(KSPPortalURLConst, [KSPVersion2, TheNewestKSP]);
    //    KSPPortalParams:=Format(KSPPortalURLConstP, [KSPVersion2, TheNewestKSP]);
    //  end
    //else begin
//        KSPPortalURL:=Format(KSPPortalURLConst2, [KSPVersion2]);
//        KSPPortalParams:=Format(KSPPortalURLConstP2, [KSPVersion2]);
    //  end;
//    FixURL(KSPPortalURL);
//    FixURL(KSPPortalParams);
//    KSPMainWindow.MsgID_QueryCancelAutoPlay := RegisterWindowMessage('QueryCancelAutoPlay');
    LoadVarsSem2:=0;//ReleaseSemaphore(LoadVarsSem, 1, nil);
end;

end.

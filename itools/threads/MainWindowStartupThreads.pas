unit MainWindowStartupThreads;

interface

uses
  Classes, SysUtils, KSPTypes, DateUtils, Forms, StrUtils, Windows, Graphics,
  BookmarksU;

type
  TLoadOptionsThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  end;

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

uses Main, IniFiles, Dynamic_BASS, KSPConstsVars, BASSPlayer,
  Playlists, PresetsU, MediaFolders, KSPMessages, kspfiles, MRNG, MultiLog;

{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TLoadOptionsThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TLoadOptionsThread }

function CheckIfNewVersion: boolean;
  begin
    Result:=CompareText(KSPVersion2, Version)<0;
//    ShowMessage(BoolToStr(Result, true));
  end;


procedure TLoadOptionsThread.Execute;
var
  I: TIniFile;
  version:string;
  s2: TStringList;
begin
{  try
    s2:=TStringList.Create;

    KSPFiles.DownloadURL(InfoNewVersion, s2);
    s2.SaveToFile(KSPDataFolder+'data\info');
    s2.Free;

    if FileExists(KSPDataFolder+'data\info') then begin
      I:=TIniFile.Create(KSPDataFolder+'data\info');
      version:=I.ReadString('Version','Number','0');

      KSPDownloadURL:=I.ReadString('URLS','Download','');
      if (KSPDownloadURL<>'')and CheckIfNewVersion then begin
        TheNewestKSP:=Version;
        PostMessage(KSPMainWindow.Handle, WM_NEWVERSIONTHREADDONE, 0, 0);
      end else TheNewestKSP:='';
      I.Free;
    end;

  finally

  end;   }
end;

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
    


    //ForceDirectories(ExtractFilePath(Application.ExeName)+'temp\update');
    //WebUpdate1.TempDirectory:=ExtractFilePath(Application.ExeName)+'temp\update';

//    Cddb.LocalCDDBPath := ExtractFilePath(Application.ExeName)+'data\cddb\';

    ReleaseSemaphore(CreateObjectsSem, 1, nil);
end;

procedure TSetVarsThread.Execute;
begin
  { Place thread code here }
    hLog.Send('Setting vars');
    //KSPMainWindow.DSPPlay:=false;
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
    MRandSeed(GetTickCount);
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
    ReleaseSemaphore(LoadVarsSem, 1, nil);
end;

end.

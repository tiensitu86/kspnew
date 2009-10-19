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

unit MainWindowStartupThreads;

interface

uses
  Classes, SysUtils, KSPTypes, DateUtils, Forms, Graphics, LCLIntf,
  BookmarksU, profilefunc;

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

uses Main, KSPConstsVars, Playlists, PresetsU, MediaFolders, MultiLog;

procedure TCreateObjectsThread.Execute;
var
  s: string;
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
    EqList:=TEqList.Create;
    s:=KSPDataFolder+'data\vdj\last';
    FixFolderNames(s);
    if FileExists(s) then
      KSPMainWindow.Forbidden.LoadFromFile(s);
    
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

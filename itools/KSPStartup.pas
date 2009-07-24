unit KSPStartup;

interface

uses Forms, Windows, SysUtils, Classes, Dialogs, filechannel, sharedlogger, FileSupportLst;

procedure SetupKSP;

implementation

uses KSPConstsVars, StartupThread, ProfileFunc, MultiLog, kspfiles, KSPMessages;

procedure SetupStage1;
var
  Pc: TPathChar;
begin
  Application.Title := 'KSP';

  KSPDataFolder:=GetUserDataFolder+'.KSP/';
  FixFolderNames(KSPDataFolder);

  hLog:=TLogger.Create;
  hLog.Channels.Add(TFileChannel.Create(KSPDataFolder+'ksp.log'));
  hLog.ActiveClasses:=lcAll;

    hLog.Send('Loading version info...');

//    KSPMainWindow.ApplicationVisible:=true;
    KSPVersion:=GetKSPVersion('');
    StrPCopy(Pc, ExtractFilePath(Application.ExeName));
    KSPVersion2:=GetKSPVersion2(Pc);

  hLog.Send('Version loaded: '+Application.Title+' '+KSPVersion+' ('+KSPVersion2+')');

  hLog.Send('Setting basic file support');

  ForceDirectoriesKSP(KSPDataFolder);
  ForceDirectoriesKSP(KSPDataFolder+'data');

  SetupFileName:=KSPDataFolder+DefSetupFileName;

  Application.ShowMainForm := True;
end;

procedure SetupKSP;
begin
  SetupStage1;

  ForceDirectoriesKSP(KSPDataFolder+'data');
  ForceDirectoriesKSP(KSPDataFolder+'data\db\cd');
  ForceDirectoriesKSP(KSPDataFolder+'data\vdj');

  KSPDatabaseThreads:=0; KSPDatabaseThreadsInternal:=0;
  FileSupportList:=TFileSupportList.Create;

  StartupThreadSem := CreateSemaphore(nil, 0,1,'MediaLibGetCount');
  TStartupThread.Create(False);
end;

end.

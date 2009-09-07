unit KSPStartup;

interface

uses Forms, SysUtils, Classes, Dialogs, filechannel, sharedlogger, FileSupportLst,
  Qt4, BassPlayer;

procedure SetupKSP;

implementation

uses KSPConstsVars, StartupThread, ProfileFunc, MultiLog, kspfiles, KSPMessages,
  qtproc;

//Clear old logs
procedure ClearLogs;
var
  LogsFolder: string;
  s: TStringList;
  i: integer;
begin
  hLog.Send('Clearing old logs');

  LogsFolder:=KSPDataFolder+'logs';
  s:= TStringList.Create;

  ListFolders(LogsFolder, s, 7);

  for i:=0 to s.Count-1 do
    begin
      hLog.Send('Deleting log: '+s.Strings[i]);
      KSPDeleteFolder(s.Strings[i]);
    end;

  s.Free;
end;

procedure SetupStage1;
var
  Pc: TPathChar;
  i: integer;
  W : WideString;
  s: string;
begin
  Application.Title := 'KSP';
  KSPStartupTime:=Now;

  KSPDataFolder:=GetUserDataFolder+'.KSP/';

  FixFolderNames(KSPDataFolder);

  for i:=0 to MaxInt do
    begin
      KSPLogFilename:=KSPDataFolder+'logs\'+FormatDateTime('DD_MM_YYYY_hh_mm', KSPStartupTime)+'_'+IntToStr(i);
      FixFolderNames(KSPLogFilename);
      if not DirectoryExists(KSPLogFilename) then Break;
    end;

  ForceDirectories(KSPLogFilename);

  hLog:=TLogger.Create;
  hLog.Channels.Add(TFileChannel.Create(KSPDataFolder+'ksp.log'));
  hLog.ActiveClasses:=lcAll;

  ClearLogs;

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
  FixFolderNames(SetupFileName);

  GetCountSem2:=0;
  LoadPlsSem2:=0;
  StartupThreadSem2:=0;
  LoadOptionsSem2:=0;
  LoadVarsSem2:=0;
  CreateObjectsSem2:=0;

  s:=ExtractFilePath(Application.ExeName)+'plugins_qt\';
  W:=GetUtf8String(s);
  QCoreApplication_addLibraryPath(@W);

  Application.ShowMainForm := True;
  Player:=TBassPlayer.Create(nil);
end;

procedure SetupKSP;
begin
  SetupStage1;

  ForceDirectoriesKSP(KSPDataFolder+'data');
  ForceDirectoriesKSP(KSPDataFolder+'data\db\cd');
  ForceDirectoriesKSP(KSPDataFolder+'data\vdj');

  KSPDatabaseThreads:=0; KSPDatabaseThreadsInternal:=0;
  FileSupportList:=TFileSupportList.Create;

  StartupThreadSem2:=1;// := CreateSemaphore(nil, 0,1,'MediaLibGetCount');
  TStartupThread.Create(False);
end;

end.

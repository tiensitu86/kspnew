program ksp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}//{$IFDEF UseCThreads}
  cthreads,
  //{$ENDIF}
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, main, LResources, BASSPlayer, Dynamic_Bass,
  RT_basscd, PlayLists, SpkXMLParser, ID3Mgmnt, FileSupport,
  KSPMessages, KSPConstsVars, FileSupportLst, kspfiles, LoadPlsThread,
  FileUtils, MRNG, profilefunc, KSPStrings, SQLDBLaz, MediaItems,
  MediaItemsInfo, KSPCrossList,
  KSPStartup, MainWindowStartupThreads, MediaFolders, FoldersScan,
  StartupThread, BookmarksU, PresetsU, app_db_utils, app_sql, MultiLog,
  OptionsFrm2, splash, complib, KSPThreadUtils, TurboPowerIPro, Support,
  WAVfile, APETag, OggVorbis
  {$IFDEF KSP_STATIC}, KSPDLLFileUtils{$ENDIF}
  , MPEGaudio, MPEGInfoBox, IdHTTP, blcksock, KSPWebKit, suggfind;

{$IFDEF WINDOWS}{$R ksp.rc}{$ENDIF}

begin
  {$I ksp.lrs}
  Application.Initialize;

  SetupKSP;

  Application.CreateForm(TKSPMainWindow, KSPMainWindow);
  Application.Run;
end.


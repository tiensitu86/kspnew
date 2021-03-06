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
program ksp;

{$mode objfpc}{$H+}

{$I ksp_version.inc}

uses {$IFDEF UNIX} //{$IFDEF UseCThreads}
  cthreads,  //{$ENDIF}
 {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms { you can add units after this },
  main,
  LResources,
  BASSPlayer,
  Bass,
  RT_basscd,
  PlayLists,
  SpkXMLParser,
  ID3Mgmnt,
  FileSupport,
  KSPMessages,
  KSPConstsVars,
  FileSupportLst,
  kspfiles,
  LoadPlsThread,
  FileUtils,
  MRNG,
  profilefunc,
  KSPStrings,
  SQLDBLaz,
  MediaItems,
  MediaItemsInfo,
  KSPCrossList,
  KSPStartup,
  MediaFolders,
  FoldersScan,
  BookmarksU,
  PresetsU,
  app_db_utils,
  app_sql,
  MultiLog,
  complib,
  KSPThreadUtils,
  Support,
  WAVfile,
  APETag,
  OggVorbis {$IFDEF KSP_STATIC},
  KSPDLLFileUtils {$ENDIF}  ,
  MPEGaudio,
  MPEGInfoBox,
  suggfind,
  cplayer, updates, updfrm,
  ksplua,
  closefrm, mplayer;

{$IFDEF WINDOWS}{$R ksp.rc}{$ENDIF}

begin
  {$I ksp.lrs}
  Application.Initialize;

  SetupKSP;

  Application.CreateForm(TKSPMainWindow, KSPMainWindow);
  Application.Run;
end.

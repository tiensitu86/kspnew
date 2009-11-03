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

unit FoldersScan;

interface

uses
  Forms, Classes, ComCtrls, MediaItems, Dialogs, SysUtils,
  Graphics, MediaFolders, DateUtils, KSPFiles;

type
  TFoldersScanThread = class(TThread)
  private
    { Private declarations }
    ItemsNo: integer;
    procedure MInfo(var Entry: TMediaFolder);
    procedure SearchForNews;
  public
    Scanning: boolean;
    STemp:    TStringList;
    Index:    integer;
    ForceRescan: boolean;
  protected
    procedure Execute; override;
  end;


implementation

uses Main, KSPConstsVars, KSPStrings, MultiLog;

procedure TFoldersScanThread.MInfo(var Entry: TMediaFolder);
var
  st:     TStringList;
  i:      integer;
  ToDate: boolean;

  function isDuplicated(s: string): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to st.Count - 1 do
      if UpperCase(s) = UpperCase(st.Strings[i]) then
        Result := True;
  end;

begin
  STemp := TStringList.Create;

  Scanning := True;

  KSPSetStatusText(SScanning);
  //  e.OnStatistics:=KSPMainWIndow.EasyFileSearch1Statistics;

  ToDate := (Entry.ScannedEver) and (not ForceRescan);

  STEmp := TStringList.Create;
  hLog.Send('MEDIA LIBRARY: Scanning folder ' + Entry.Folder);
  if ToDate then
    SearchForFiles(Entry.Folder, True, STemp, Entry.LastScanned)
  else
    SearchForFilesFS(Entry.Folder, True, STemp);

  st := TStringList.Create;
  for i := 0 to STemp.Count - 1 do
    if (not IsDuplicated(STemp.Strings[i])) and
      (FileSupportList.FindExtension(ExtractFileExt(STemp.Strings[i]), False) > -1) then
      st.Add(STemp.Strings[i]);

  hLog.Send('MEDIA LIBRARY: Scanning folder (part 2) ' + Entry.Folder);

  ItemsNo := BuildMediaInfo(st, AllSongs);

  st.Free;

  Entry.ScannedEver := True;
  Entry.LastScanned := Now;

  KSPSetStatusText('');

  STemp.Free;
  hLog.Send('MEDIA LIBRARY: Scanning folder done');
end;

procedure TFoldersScanThread.SearchForNews;
var
  i:     integer;
  Entry: TMediaFolder;
begin
  if KSPMainWindow.MediaFoldersList.Count = 0 then
    Exit;

  for i := 0 to KSPMainWindow.MediaFoldersList.Count - 1 do
  begin
    Index := i;
    Entry := KSPMainWindow.MediaFoldersList.GetItem(i);
    MInfo(Entry);
    //    CreateWatch(Entry.Folder);
    KSPMainWindow.MediaFoldersList.ReplaceEntry(i, Entry);
    //KSPMainWindow.Frame11.MediaBuild.Value:=i+1;
  end;
end;

procedure TFoldersScanThread.Execute;
begin
  KSPMainWindow.WaitForB := 1;
  hLog.Send('Scanning folders for media files');
  ItemsNo := 0;

  AllSongs.CompactLib;//Delete non-existing entries

  GetCountSem2  := 0;
  Self.Priority := tpHigher;

  SearchForNews;

  KSPMainWindow.WaitForB := 2;

  hLog.Send('MEDIA LIBRARY: Scanning done');

  KSPMainWindow.MediaFoldersList.SaveToFile(KSPDataFolder + 'data\MediaLib.xml');

  KSPMainWindow.SongsInLib := ItemsNo;

  //s:=AllSongs.CompactLib;
  KSPMainWindow.WaitForB := 0;
  AllSongs.CompactLib;//(2, s);

  KSPMainWindow.TabSheet1.Visible := ItemsNo > 0;

  if not ForceRescan then
  begin

  end
  else
    KSPShowMessage(SScanningDone);

  //  end;

end;

end.

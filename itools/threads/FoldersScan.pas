unit FoldersScan;

interface

uses
  Forms, Windows, Classes, ComCtrls, MediaItems, Dialogs, SysUtils,
  Graphics, MediaFolders, DateUtils, Playlists, KSPFiles, ID3Mgmnt;

type
  TFoldersScanThread = class(TThread)
  private
    { Private declarations }
    ItemsNo: integer;
    procedure MInfo(var Entry: TMediaFolder);
    procedure SearchForNews;
  public
    Scanning: boolean;
    STemp: TStringList;
    Index: Integer;
    ForceRescan: boolean;
  protected
    procedure Execute; override;
  end;


implementation

uses Main, KSPConstsVars, KSPStrings, MultiLog;

procedure TFoldersScanThread.MInfo(var Entry: TMediaFolder);
var
  st: TStringList;
  i: integer;
  ToDate: boolean;

  function isDuplicated(s: string):boolean;
  var
    i: integer;
  begin
    Result:=false;
    for i := 0 to st.Count - 1 do
      if UpperCase(s)=UpperCase(st.Strings[i]) then
        Result:=true;
  end;

begin
  STemp:=TStringList.Create;

  Scanning:=true;

  KSPMainWindow.ScanFolder.Caption:=(SScanning);
//  e.OnStatistics:=KSPMainWIndow.EasyFileSearch1Statistics;

  ToDate:=(Entry.ScannedEver) and (not ForceRescan);

  STEmp:=TStringList.Create;
  if ToDate then
  SearchForFiles(Entry.Folder, true, STemp, Entry.LastScanned) else
  SearchForFilesFS(Entry.Folder, true, STemp);

  st:=TStringList.Create;
  for i := 0 to STemp.Count - 1 do
    if (not IsDuplicated(STemp.Strings[i])) and
      (FileSupportList.FindExtension(ExtractFileExt(STemp.Strings[i]), false)>-1) then
        st.Add(STemp.Strings[i]);

  ItemsNo:=BuildMediaInfo(st, true, AllSongs, FileSupportList,
    KSPMainWindow.SongsInLib);

  st.Free;

  Entry.ScannedEver:=true;
  Entry.LastScanned:=Now;

  KSPMainWindow.ScanFolder.Caption:='';

  STemp.Free;
  hLog.Send('MEDIA LIBRARY: Scanning folder done');
end;

procedure TFoldersScanThread.SearchForNews;
var
  i: integer;
  Entry: TMediaFolder;
begin
  if KSPMainWindow.MediaFoldersList.Count=0 then
    Exit;

  for i:=0 to KSPMainWindow.MediaFoldersList.Count-1 do begin
    Index:=i;
    Entry:=KSPMainWindow.MediaFoldersList.GetItem(i);
    MInfo(Entry);
//    CreateWatch(Entry.Folder);
    KSPMainWindow.MediaFoldersList.ReplaceEntry(i, Entry);
    //KSPMainWindow.Frame11.MediaBuild.Value:=i+1;
    end;
end;

procedure TFoldersScanThread.Execute;
var
  //d: TDateTime;
  Result: Cardinal;
begin
//  while not Application.Terminated do begin
//  WaitForSingleObject(GetCountSem, 0);
  KSPMainWindow.WaitForB:=1;
  hLog.Send('Scanning folders for media files');
  ItemsNo:=0;

//  AllSongs.CompactLib;//Delete non-existing entries

  ReleaseSemaphore(GetCountSem, 1, nil);
//  LoadPlsSem := CreateSemaphore(nil, 0,1,'KSPLoadPls');
  Self.Priority:=tpHigher;
//  repeat
//    Result := WaitForSingleObject(LoadPlsSem, 2000);
//  until Result=WAIT_OBJECT_0;

  SearchForNews;

  KSPMainWindow.WaitForB:=2;

  hLog.Send('MEDIA LIBRARY: Scanning done');

  KSPMainWindow.MediaFoldersList.SaveToFile(KSPDataFolder+'data\MediaLib.xml');

  KSPMainWindow.SongsInLib:=ItemsNo;

  KSPMainWindow.WaitForB:=0;

  if not ForceRescan then begin
      
    end else ShowMessage(SScanningDone);

//  end;

end;

end.

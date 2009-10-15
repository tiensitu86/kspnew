unit MediaFolders;

interface

uses Classes, SysUtils, Dialogs, ID3Mgmnt, profilefunc;

type TMediaFolder = record
  Folder: string;
  Name: string;
  Description: string;
  LastScanned: TDateTime;
  ScannedEver: boolean;
  end;

  TMediaFolderItem = class(TObject)
  public
    Entry: TMediaFolder;
  end;

  TMediaFoldersList = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Entry: TMediaFolder): boolean;
    procedure Remove(Index: Integer);
    function GetItem(Index: Integer): TMediaFolder;
    procedure ReplaceEntry(Index: Integer; new: TMediaFolder);
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    function FileInFolders(FileName: string): boolean;
  end;

implementation

uses IniFiles, KSPConstsVars, multilog;

constructor TMediaFoldersList.Create;
begin
  inherited Create;
end;

{The Items should be freed here but it isn't. Doesn't matter.
TPlayList is created only once and destroyed only while KSP is
to be closed}

destructor TMediaFoldersList.Destroy;
var
  i: integer;
begin
  for I := 0 to Count-1 do
    TMediaFolderItem(Items[I]).Free;
  inherited Destroy;
end;

function TMediaFoldersList.Add(Entry: TMediaFolder): boolean;
var
  T: TMediaFolderItem;

  function CheckIfExists: boolean;
  var
    i: integer;
  begin
    Result:=false;
    if Self.Count>0 then
      for i:=0 to Self.Count-1 do
        if TMediaFolderItem(Items[i]).Entry.Folder=Entry.Folder then Result:=true;
  end;

begin
  Result:=not CheckIfExists;

  if Result then begin
      T:=TMediaFolderItem.Create;
      T.Entry:=Entry;
      inherited Add(T);
    end;
end;

function TMediaFoldersList.FileInFolders(FileName: string): boolean;
var
  i: integer;
begin
  FixFileNameDB2(FileName);
  Result:=false;
  if Count>0 then
    for i:=0 to Count-1 do
      if Pos(UpperCase(GetItem(i).Folder), UpperCase(FileName))>0 then begin
        Result:=true;
      end;

  if not Result then
    hLog.Send('MEDIA LIBRARY: File not in library folder: '+FileName);
end;

procedure TMediaFoldersList.ReplaceEntry(Index: Integer; new: TMediaFolder);
begin
  TMediaFolderItem(Items[Index]).Entry:=new;
end;

procedure TMediaFoldersList.Remove(Index: Integer);
var
  s: string;
  i: integer;
  p: TPLEntry;
  RecCo: integer;
  s2: TStringList;
begin
  s:=TMediaFolderItem(Items[Index]).Entry.Folder;
  s2:=TStringList.Create;

  AllSongs.OpenQuery('SELECT * FROM meta');
  RecCo:=AllSongs.ReturnRecordsCount;
  if RecCo>0 then for i:=RecCo-1 downto 0 do
    begin
      AllSongs.GoToNext;
      p:=AllSongs.ReadEntry;
      s2.Add(p.FileName);
    end;

  AllSongs.CloseQuery;

  for i:=0 to s2.Count-1 do;
    if Pos(s, s2.Strings[i])>-1 then
      AllSongs.Remove(s2.Strings[i]);

  s2.Free;
  TMediaFolderItem(Items[Index]).Free;

  Delete(Index);
end;

function TMediaFoldersList.GetItem(Index: Integer): TMediaFolder;
begin
  Result:=TMediaFolderItem(Items[Index]).Entry;
end;

procedure TMediaFoldersList.SaveToFile(FileName: string);
var
  XMLFile: TIniFile;
  i: integer;
  mf: TMediaFolder;
begin
  FixFolderNames(FileName);
  if FileExists(FileName) then DeleteFile(FileName);
  if Self.Count=0 then Exit;
  XMLFile:=TIniFile.Create(FileName);
  //XMLFile.Clear;

  for i:=0 to Self.Count-1 do begin
      mf:=TMediaFolderItem(Items[i]).Entry;
      XMLFile.WriteString(IntToStr(i),'Folder',mf.Folder);
      XMLFile.WriteString(IntToStr(i),'Desc',mf.Description);
      XMLFile.WriteString(IntToStr(i),'Name',mf.Name);
      XMLFile.WriteDateTime(IntToStr(i),'LastScanned',mf.LastScanned);
      XMLFile.WriteBool(IntToStr(i),'ScannedEver',mf.ScannedEver);
    end;

  XMLFile.UpdateFile;
  XMLFile.Free;
end;

procedure TMediaFoldersList.LoadFromFile(FileName: string);
var
  XMLFile: TIniFile;
  i: integer;
  mf: TMediaFolder;
  s: TStringList;
begin
  FixFolderNames(FileName);
  XMLFile:=TIniFile.Create(FileName);
  s:=TStringList.Create;
  XMLFile.ReadSections(s);
  if s.Count=0 then begin
      s.Free;
      XMLFile.Free;
      Exit;
    end;
  //XMLFile.Clear;

  for i:=0 to s.Count-1 do begin
      mf.Folder:=XMLFile.ReadString(IntToStr(i),'Folder','');
      mf.Description:=XMLFile.ReadString(IntToStr(i),'Desc','');
      mf.Name:=XMLFile.ReadString(IntToStr(i),'Name','');
      mf.LastScanned:=XMLFile.ReadDateTime(IntToStr(i),'LastScanned',Now);
      mf.ScannedEver:=XMLFile.ReadBool(IntToStr(i),'ScannedEver',false);
      Add(mf);
    end;

  XMLFile.Free;
end;

end.

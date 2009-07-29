unit MediaItems;

interface

uses Forms, Types, Classes, ID3Mgmnt, PlayLists, Dialogs, SysUtils, FileSupport,
    IniFiles, FileSupportLst, kspfiles, DateUtils, MediaItemsInfo, Math,
    StdCtrls, KSPCrossList, ExtCtrls, KSPMessages, app_db_utils, DB, DOM;

const
  faReadOnly  = $00000001;
  faHidden    = $00000002;
  faSysFile   = $00000004;
  faVolumeID  = $00000008;
  faDirectory = $00000010;
  faArchive   = $00000020;
  faSymLink   = $00000040;
  faAnyFile   = $0000003F;

type TCDEntryInfo = record
      CDID: string;
      Title: string;
      Artist: string;
      Genre: string;
      Tracks: TStringList;
    end;

type TASParseType = (asptTracks, asptAlbums, asptRelated);

type
  TFavouriteList = class;
  TMediaItemsList = class;

  TPlayNextSong = record
      FileName: string;
      PlayCount: Cardinal;
      Favourite: Double;
      end;

  TFavInfo = class(TObject)
  public
    Entry: TPlayNextSong;
  end;

  TFavouriteList = class(TList)
    fIM: integer;
  public
    property InternalName: integer read fIM write fIM;
    constructor Create; overload;
    constructor Create(AFileName: string; IM: Integer); overload;
    destructor Destroy; override;
    function Add(Entry: TPlayNextSong; Songs: TAppDBConnection; Insert: boolean): boolean;
    procedure Remove(Index: Integer);
    function GetItem(Index: Integer): TPlayNextSong;
    procedure ReplaceEntry(new: TPlayNextSong; Songs: TAppDBConnection);
    function FindItem(FileName: string): integer;
    procedure Sort;
  end;

  TCDEntry = class(TObject)
  public
    Entry: TCDEntryInfo;
    constructor Create;
    destructor Destroy; override;
  end;

  TCDList = class (TList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Entry: TCDEntry);
    procedure Remove(Index: Integer);
    function GetItem(Index: Integer): TCDEntry;
  end;

  TMediaItemsList = class(TObject)
  private
//    fXML: TXMLEntryFile;
    //FavouriteList: TFavouriteList;
    //fMetaXML: TXMLMetaTags;
    //fDataBase: TKSPDataBase;
    //function SetupDatabase: TAppDBConnection;
    //procedure AddToDataBase(p: TPLEntry); overload;
    {procedure nxEventLog1LogDataFilter(const aLogData: InxLogData;
      var aFiltered: Boolean);}
    //procedure MemoryTimer(Sender: TObject);
    //procedure MigrateDatabase;
    //procedure SaveVersion;
  public
//    DBName: string;
    //DBFileName: string;
    //fDatabase: TAppDBConnection;
    //constructor Create;
//    procedure Migrate;
    //destructor Destroy; override;
    //property KSPDatabase: TKSPDataBase read fDatabase write fDatabase;
    //procedure Add(Entry: TPLEntry; OnLoad: boolean);
    //procedure AddToDataBase(p: TPlaylist); overload;
    //function QueryFindNext: boolean;
    //function RetFields: TPLEntry;
//    function RetVDJEntry: TPlayNextSong;
    //procedure OpenQuery(sql: string);
    //procedure QueryInsert(sql: string);
    //procedure CloseQuery;
    //procedure RenameInBase(P: TPLEntry; OldName: string);
    //procedure CompactLib;
    //function GetFavList(FileName: string): TFavouriteList;
//    procedure SaveToFile;//(FileName: string);
    //procedure AddPlayNext(FileName: String; OldFileName: string);
    //function Count: integer;
    //procedure Remove(FileName: string);
    //function ReturnFromGID(FileName: string; GID: integer): TPlayList;
    //function ReturnFromArtist(FileName: string; Artist: string): TPlayList;
    //procedure UpdateItems;
    //function FileInLib(FileName: string): boolean;
    //function FindSongFileName(Song: integer): string;
    //procedure FindSongs(var Songs: TPlayList; Artist, Album: string);
    //function ReturnAllItems: TPlaylist;
    //function IsPlaylist(FileName: string): boolean;
  end;

procedure SortMediaFavList(FavList: TFavouriteList; mItems: TAppDBConnection;
  Song: TPLEntry; Forb: TStringList; UseGID: boolean; GID: integer;
  UseArtist: boolean; Artist: string; UseAS: boolean);

procedure ReturnArtists(var Artists: TCrossList; mItems: TAppDBConnection); overload;
procedure ReturnArtists(var Artists: TStringList; mItems: TAppDBConnection; Album: string); overload;
procedure ReturnAlbums(var Albums: TCrossList; mItems: TAppDBConnection); overload;
procedure ReturnAlbums(var Albums: TStringList; mItems: TAppDBConnection; Artist: string); overload;
function BuildMediaInfo(stemp: TStringList; Rec: Boolean; var mItems: TAppDBConnection;
 FileSL: TFileSupportList; SongsInLib: integer): integer;
//procedure FindSongs(var Songs: TPlayList; mItems: TMediaItemsList; Artist, Album: string);
procedure FindSongsLike(var Songs: TPlayList; mItems: TAppDBConnection; FileName: string);
procedure FindSongsArtist(var Songs: TPlayList; mItems: TAppDBConnection; Artist: string);
procedure FindSongsAlbum(var Songs: TPlayList; mItems: TAppDBConnection; Album: string);
procedure FindSongsByYear(var Songs: TPlayList; mItems: TAppDBConnection; Year, Album: string); overload;
procedure FindSongsByGenre(var Songs: TPlayList; mItems: TAppDBConnection; Genre, Album: string); overload;
procedure FindSongsByYear(var Songs: TPlayList; mItems: TAppDBConnection; Year: string); overload;
procedure FindSongsByGenre(var Songs: TPlayList; mItems: TAppDBConnection; Genre: string); overload;
function ArtistInLib(mItems: TAppDBConnection; Artist: string): boolean;
//function FindSongFileName(Song: integer; mItems: TMediaItemsList): string;
//function FindSong(Song: string; mItems: TMediaItemsList): integer;

procedure ReturnYears(var Years: TCrossList; mItems: TAppDBConnection);
procedure ReturnGenres(var Gn: TCrossList; mItems: TAppDBConnection);
procedure ReturnAlbumsFromYear(var Albums: TStringList; mItems: TAppDBConnection; Year: string);
procedure ReturnAlbumsFromGenre(var Albums: TStringList; mItems: TAppDBConnection; Gn: string);

//procedure SearchFiles(Path: string; Rec: boolean; var s: TStringList); external 'kspfiles.dll';


implementation

uses MRNG, Main, KSPConstsVars, ProfileFunc, KSPStrings, MultiLog;

var FilesNo: integer;
  FSL:TFileSupportList;


constructor TCDEntry.Create;
begin
  inherited Create;
  Entry.Tracks:=TStringList.Create;
end;

destructor TCDEntry.Destroy;
begin
  Entry.Tracks.Free;
  inherited Destroy;
end;

procedure RemoveSpacesFromEnd(var s: string);
var
  i: integer;

  function PosLastChar(str: string): integer;
  var
    x, xp: integer;
  begin
    xp:=-1;
    for x:=0 to Length(str)-1 do
      if str[x]<>' ' then xp:=x;
    Result:=xp;
  end;

begin
  i:=PosLastChar(s);

  if (i=-1) or (i=0) then Exit;

  Inc(i);
  Delete(s, i, Length(s)-i);
end;

procedure ReturnArtists(var Artists: TCrossList; mItems: TAppDBConnection);
var
  i, ap: integer;
  s: string;
  P: TPLEntry;

  function FindArtist(A: string): integer;
  var
    i: integer;
  begin
    Result:=-1;
    if Artists.Count>0 then begin
        for i:=0 to Artists.Count-1 do
          if UpperCase(TCrossEntry(Artists.Items[i]).Name)=UpperCase(A) then
            Result:=i;
      end;
  end;

  function FindAlbum(A: string; aTemp: TStringList): boolean;
  var
    i: integer;
  begin
    Result:=false;
    if aTemp.Count>0 then begin
        for i:=0 to aTemp.Count-1 do
          if UpperCase(aTemp.Strings[i])=UpperCase(A) then Result:=true;
      end;
  end;

begin
  Artists.Clear;

  mItems.OpenQuery('SELECT * FROM meta');
  if mItems.ReturnRecordsCount>0 then begin
    mItems.GoToFirst;
    while not mItems.EndOfDB do begin
      p:=mItems.ReadEntry;
      mItems.GoToNext;

      if p.Tag.Artist = '' then p.Tag.Artist:=(SUnknownArtist);
      if p.Tag.Album = '' then p.Tag.Album:=(SUnknownAlbum);

      s:=p.Tag.Artist;

      ap:=FindArtist(s);

      if ap=-1 then begin
        Artists.Add(s);
        ap:=Artists.Count-1; end;

      if not FindAlbum(p.Tag.Album, TCrossEntry(Artists.Items[ap]).SubList) then
        TCrossEntry(Artists.Items[ap]).SubList.Add(p.Tag.Album);

    end;
  end;

  mItems.CloseQuery;

  Artists.Sort;
end;

procedure ReturnAlbums(var Albums: TCrossList; mItems: TAppDBConnection);
var
  i, ap: integer;
  s: string;
  p: TPLEntry;

  function FindAlbum(A: string): integer;
  var
    i: integer;
  begin
    Result:=-1;
    if Albums.Count>0 then begin
        for i:=0 to Albums.Count-1 do
          if UpperCase(TCrossEntry(Albums.Items[i]).Name)=UpperCase(A) then Result:=i;
      end;
  end;

  function FindArtist(A: string; aTemp: TStringList): boolean;
  var
    i: integer;
  begin
    Result:=false;
    if aTemp.Count>0 then begin
        for i:=0 to aTemp.Count-1 do
          if UpperCase(aTemp.Strings[i])=UpperCase(A) then Result:=true;
      end;
  end;

begin
  Albums.Clear;

  mItems.OpenQuery('SELECT * FROM meta');

  if mItems.ReturnRecordsCount>0 then
  for i:=0 to mItems.ReturnRecordsCount-1 do begin
      p:=mItems.ReadEntry;
      mItems.GoToNext;
      if p.Tag.Artist = '' then p.Tag.Artist:=(SUnknownArtist);
      if p.Tag.Album = '' then p.Tag.Album:=(SUnknownAlbum);

      s:=p.Tag.Album;
      ap:=FindAlbum(s);
      if ap=-1 then begin
        Albums.Add(s);
        ap:=Albums.Count-1  end;

      if not FindArtist(p.Tag.Artist, TCrossEntry(Albums.Items[ap]).SubList) then
        TCrossEntry(Albums.Items[ap]).SubList.Add(p.Tag.Artist);

    end;

  mItems.CloseQuery;
  Albums.Sort;
end;

procedure ReturnArtists(var Artists: TStringList; mItems: TAppDBConnection; Album: string);
var
  i: integer;
  s: TStringList;
  p: TPLEntry;

  aTemp, ArtTemp: TStringList;

  function FindArtist(A: string; Art: string): boolean;
  var
    i: integer;
  begin
    Result:=false;
    if aTemp.Count>0 then begin
        for i:=0 to aTemp.Count-1 do
          if (UpperCase(aTemp.Strings[i])=UpperCase(A)) and
            (UpperCase(ArtTemp.Strings[i])=UpperCase(Art)) then Result:=true;
      end;
  end;

begin
  Artists.Clear;
  s:=TStringList.Create;
  aTemp:=TStringList.Create;
  ArtTemp:=TStringList.Create;

  mItems.OpenQuery('SELECT * FROM meta');

  if mItems.ReturnRecordsCount>0 then
  for i:=0 to mItems.ReturnRecordsCount-1 do begin
      p:=mItems.ReadEntry;
      mItems.GoToNext;
      if (not FindArtist(p.Tag.Artist, p.Tag.Album))and
        (p.Tag.Album=Album) then begin
            Artists.Add(p.Tag.Artist);
            aTemp.Add(p.Tag.Artist);
            ArtTemp.Add(p.Tag.Album);
            s.Add(p.Tag.Album);
          end;

    end;

  mItems.CloseQuery;

  Artists.Sort;

  s.Free;

end;

procedure ReturnAlbums(var Albums: TStringList; mItems: TAppDBConnection; Artist: string);
var
  i: integer;
  p: TPLEntry;
  Pc: TPathChar;

  {function FindAlbum(A: string; Art: string): boolean;
  var
    i: integer;
  begin
    Result:=false;
    if aTemp.Count>0 then begin
        for i:=0 to aTemp.Count-1 do
          if (UpperCase(aTemp.Strings[i])=UpperCase(A)) and
            (UpperCase(ArtTemp.Strings[i])=UpperCase(Art)) then Result:=true;
      end;
  end;}

begin
  Albums.Clear;
  //s:=TStringList.Create;
  //aTemp:=TStringList.Create;
  //ArtTemp:=TStringList.Create;

  StrPCopy(Pc, Artist);

  mItems.OpenQuery('SELECT * FROM meta WHERE Artist='''+PrepareString(Pc)+'''');

  if mItems.ReturnRecordsCount>0 then
  for i:=0 to mItems.ReturnRecordsCount-1 do begin
      p:=mItems.ReadEntry;
      mItems.GoToNext;
      //if (not FindAlbum(p.Tag.Album, p.Tag.Artist))and
      //  (UpperCase(p.Tag.Artist)=UpperCase(Artist)) then begin
            Albums.Add(p.Tag.Album);
      //      aTemp.Add(p.Tag.Album);
      //      ArtTemp.Add(p.Tag.Artist);
      //      s.Add(p.Tag.Artist);
          //end;

    end;

  mItems.CloseQuery;

  Albums.Sort;

  //s.Free;

end;

function BuildMediaInfo(stemp: TStringList; Rec: Boolean; var mItems: TAppDBConnection;
 FileSL: TFileSupportList; SongsInLib: integer): integer;
var
  i: integer;
  x: integer;
  id3tag: TPLEntry;
  tag: boolean;

//  onestep, curstep: integer;
//  p: TPlaylist;
//  Thr: TAddToDatabaseThread;
  SemDone: DWORD;

begin
  FSL:=FileSL;
//  p:=TPlaylist.Create;

//  KSPMainWindow.MediaLibProgress.Visible:=true;
  KSPMainWindow.MediaLibProgress.Max:=stemp.Count;
//  onestep:=stemp.Count div 100;
//  curstep:=0;
  stemp.SaveToFile(KSPdataFolder+'new_files.txt');
  if stemp.Count>0 then
    KSPMainWindow.MediaLibProgress.Max:=stemp.Count;
  for i:=0 to stemp.Count-1 do begin

   hLog.Send('X:='+IntToStr(x)+'; Count:='+IntToStr(stemp.Count));
        id3tag.Tag:=ReadID32(stemp.Strings[i], tag, x);
          id3tag.PlayCount:=0;
          id3tag.PlayedEver:=false;
          id3tag.Fav:=0;
          id3tag.FirstPlay:=0;
          id3tag.LastPlay:=0;
          id3tag.MetaTag:=0;
          if not tag then begin
              id3tag.Tag.Album:='';
              id3tag.Tag.Artist:='';
              id3tag.Tag.Title:='';
              id3tag.Tag.Year:='';
              id3tag.Tag.Genre:='';

              id3tag.Tag.GID:=0;
              id3tag.Tag.Track:=0;
            end;
            id3Tag.FileName:=stemp.Strings[i];
            //if (id3tag.Tag.Track>127)or(id3tag.Tag.Track<0) then id3tag.Tag.Track:=0;
            //if (id3tag.Tag.GID>65000)or(id3tag.Tag.GID<0) then id3tag.Tag.GID:=0;
        hLog.Send('MEDIA LIBRARY: Adding item ', id3tag.FileName);
        mItems.Add(id3tag, false);
//        stemp.Delete(0);
    //while Thr.Preparing do
    //  Sleep(100);
//    KSPMainWindow.MediaLibProgress.ShowProgressText:=true;


    KSPMainWindow.MediaLibProgress.Position:=KSPMainWindow.MediaLibProgress.Max-stemp.Count;
//    KSPMainWindow.MediaLibProgress.ProgressText:=IntToStr(KSPMainWindow.MediaLibProgress.Value)+'/'+

    end;

//  KSPMainWindow.MediaLibProgress.Visible:=false;
//  p.Free;
  //while KSPDatabaseThreads>0 do
  //  Sleep(100);
  Result:=mItems.ReturnRecordsCount;
end;

{function TMediaItemsList.SetupDatabase: TAppDBConnection;
var
  fDatabase: TAppDBConnection;
begin
  ForceDirectoriesKSP(KSPDataFolder+'data\db');
  ForceDirectoriesKSP(KSPDataFolder+'db');
  fDatabase:=TAppDBConnection.Create;
  with fDatabase do begin
    SetupDatabase(KSPDataFolder+'db/ksp.kspdb');

    MigrateDatabase;

    MemoryTimer(nil);
    Result:=fDatabase;
  end;
end; }

{procedure TMediaItemsList.SaveVersion;
var
  i: TIniFile;
begin
  I:=TIniFile.Create(KSPDataFolder+'data\db\version');
  I.WriteInteger('Version', 'Number', DB_VERSION);
  I.Free;
end;}

{procedure TMediaItemsList.MigrateDatabase;
var
  s: TStringList;

  procedure SaveVersion;
  var
    i: TIniFile;
  begin
    I:=TIniFile.Create(KSPDataFolder+'data\db\version');
    I.WriteInteger('Version', 'Number', DB_VERSION);
    I.Free;
  end;

  function ShouldMigrate: boolean;
  var
    i: TIniFile;
    v: integer;
  const
    ConstMsg = 'You''ve upgraded from KSP which used a very old table format'+#13+
      'New database has been created but you should re-scan your Media Library'+#13+
      '(If you have it) to fill new database with media entries';
  begin
     v:=0;
     if FileExists(KSPDataFolder+'data\db\version') then
       begin
          I:=TIniFile.Create(KSPDataFolder+'data\db\version');
          v:=i.ReadInteger('Version', 'Number', 0);
          I.Free;
        end;
     Result:=v<DB_VERSION;
     if V<5 then begin
        ShowMessage(ConstMsg);
        Result:=false;
        SaveVersion;
      end;

  end;

  procedure AddToNewDataBase(p: TPLEntry);
  var
    fm: TFormatSettings;
    Pc1, Pc2, Pc3, Pc4, Pc5, Pc6, Pc7: TPathChar;
  begin
//    GetLocaleFormatSettings(KSPLangID, fm);
    fm.DecimalSeparator:='.';

    StrPCopy(Pc1, p.Tag.Comment);
    StrPCopy(Pc2, p.Tag.Year);
    StrPCopy(Pc3, p.Tag.Album);
    StrPCopy(Pc4, p.FileName);
    StrPCopy(Pc5, p.Tag.Genre);
    StrPCopy(Pc6, p.Tag.Artist);
    StrPCopy(Pc7, p.Tag.Title);

    s.Add(Format(InsStatNew, [IntToStr(p.Tag.GID),
                                                  IntToStr(p.Tag.Track),
                                                  PrepareString(Pc1),
                                                  PrepareString(Pc2),
                                                  PrepareString(Pc3),
                                                  PrepareString(Pc6),
                                                  PrepareString(Pc7),
                                                  BoolToStr(p.PlayedEver, true),
                                                  IntToStr(p.MetaTag),
                                                  IntToStr(p.PlayCount),
                                                  FloatToStr(p.Fav, fm),
                                                  DateToStr(p.LastPlay),
                                                  DateToStr(p.FirstPlay),
                                                  PrepareString(Pc4),
                                                  PrepareString(Pc5)]));
        //Application.ProcessMessages;
  end;

  procedure PrepareSQL;
  var
    i: integer;
    p: TPLEntry;
    Data: TAppDBConnection;
  begin
    s:=TStringList.Create;
    Data:=OpenQuery('SELECT * FROM meta_old ORDER BY I_Name');

    if Data.ReturnRecordsCount>0 then
      for i:=0 to Data.ReturnRecordsCount-1 do
        begin
          p:=RetFields(Data);
          AddToNewDataBase(p);
          QueryFindNext(Data);
        end;

    CloseQuery(Data);

    if s.Count>0 then
    {for i:=0 to s.Count-1 do} //begin
        {QueryInsert(s.Text);
      end;

    s.Free;
  end;

begin
  if ShouldMigrate then begin
      //CreateDatabase('meta_new');
      RenameFile(KSPDataFolder+'db\meta.kspdb', KSPDataFolder+'db\meta_old.kspdb');
      RenameFile(KSPDataFolder+'db\meta_new.kspdb', KSPDataFolder+'db\meta.kspdb');
      {CreateDatabaseVDJ('vdjentries_new');
      RenameFile(KSPDataFolder+'db\vdjentries.kspdb', KSPDataFolder+'db\vdjentries_old.kspdb');
      RenameFile(KSPDataFolder+'db\vdjentries_new.kspdb', KSPDataFolder+'db\vdjentries.kspdb');}
      {PrepareSQL;
      SaveVersion;
      ShowMessage(('SMigratedToNew'));
    end;
end;   }

{procedure TMediaItemsList.CreateDatabaseVDJ(Table: string);
var
  ProductsDict: TnxDataDictionary;
  p: boolean;
begin
  p:=fDataBase.Database.Active;
  fDataBase.Database.Active:=true;
  ProductsDict := TnxDataDictionary.Create;

  try
     ProductsDict.FieldsDescriptor.AddField('I_NAME', '', nxtInt16, 7, 0, True);
     ProductsDict.FieldsDescriptor.AddField('PlayCount', '', nxtInt16, 7, 0, True);
     ProductsDict.FieldsDescriptor.AddField('FileName', '', nxtWideString, MAX_PATH, 0, True);
     ProductsDict.FieldsDescriptor.AddField('Fav', '', nxtDouble, 16, 0, True);
     NewIndex := ProductsDict.IndicesDescriptor.AddIndex('I_Name', 0, False,
       'Prim Key for media items', TnxCompKeyDescriptor);
     TnxCompKeyDescriptor(NewIndex.KeyDescriptor).Add(
       ProductsDict.GetFieldFromName('FileName'));

     Self.fDataBase.Database.CreateTable(true, Table, '', ProductsDict);
   finally
     FreeAndNil(ProductsDict);
   end;

  fDataBase.Database.Active:=p;
end; }
{The Items should be freed here but it isn't. Doesn't matter.
TPlayList is created only once and destroyed only while KSP is
to be closed}

{procedure TMediaItemsList.Add(Entry: TPLEntry; OnLoad: boolean);
begin
  if ExtractFileExt(Entry.FileName)='' then Exit;
  if FileSupportList.FindExtension(UpperCase(ExtractFileExt(Entry.FileName)), false)=-1 then Exit;
  if not OnLoad then AddToDataBase(Entry);
end;

function TMediaItemsList.RetFields: TPLEntry;
begin
//  if Index>0 then repeat fDataBase.Query.FindNext; Inc(i); until i=Index;
  ReadEntry(Result, fDatabase.ReturnFields);
end;

function TMediaItemsList.QueryFindNext: boolean;
begin
  fDatabase.GoToNext;
  Result:=true;
end;

function TMediaItemsList.OpenQuery(sql: string): TAppDBConnection;
begin
//  while QueryOpened do begin end;
//  if fDataBase.Query.Active then ShowMessage('Ok');
//  Result.aDatabase.SQL:=sql;

  //fDatabase.Table.
  try
    try
      fDatabase.OpenQuery(sql);
    except
      on E:Exception do begin
        //hLog.SendException(E, 'QueryInsert', [sql, Result.aQuery.Active, Result.aSession.Active]);
        CloseQuery(Result);
      end;
    end;
  finally
//    Result:=fDataBase.aQuery.RecordCount;
  end;

end;

procedure TMediaItemsList.QueryInsert(sql: string);
begin
//  while QueryOpened do begin end;
//  if fDataBase.Query.Active then ShowMessage('Ok');
try
  {fDataBase.aQuery.SQL.Text:=sql;
  fDataBase.aQuery.ExecSQL;
  fDataBase.aQuery.Close;}

{  fDatabase.ExecuteSQL(sql);
//  Data.aDatabase.ExecSQL;
//  CloseQuery(Data);
except
//  on E:Exception do
    //hLog.SendException(E, 'QueryInsert', [sql, Data.aQuery.Active, Data.aSession.Active]);
  //hLog.Send('ERROR RUNNIN SCRIPT: '+sql);
end;
end;}

{procedure TMediaItemsList.CompactLib;
var
  i, x: integer;
  s: TStringList;
  NotShown: boolean;
  Pc: TPathChar;
  Data: TAppDBConnection;

  function IsDuplicated(str: string; Start: integer): boolean;
  var
    i: integer;
  begin
    Result:=false;
    if Start<s.Count-1 then
      for i:=start+1 to s.Count-1 do
        if UpperCase(str)=UpperCase(s.Strings[i]) then Result:=true;

    if Result and NotShown then begin
        NotShown:=false;
        MessageDlg(('sDuplicatedItems'), mtInformation, [mbOk], 0);
        {WizardProgressForm:=TWizardProgressForm.Create(nil);
        WizardProgressForm.Show;
        WizardProgressForm.Progress.Value:=0;
        WizardProgressForm.Progress.MaxValue:=s.Count;}
{      end;
  end;

  procedure RemoveDuplicates(s: string);
  var
    pls: TPlaylist;
    p: PPLEntry;
    i, Cnt: integer;
    Pc: TPathChar;
    Data: TAppDBConnection;
  begin
    pls:=TPlayList.Create;
    //WizardProgressForm.Progress.Value:=WizardProgressForm.Progress.Value+1;
    Application.ProcessMessages;
    Sleep(100);

    repeat
      Pls.Clear;
      StrPCopy(Pc, s);
      Data:=OpenQuery(Format(SelectGetItem, [PrepareString(Pc)]));
      Cnt:=Data.ReturnRecordsCount;
      for i:=0 to Data.ReturnRecordsCount-1 do begin
          pls.Add(RetFields(Data));
          QueryFindNext(Data);
        end;
      CloseQuery(Data);
      Pls.SortPlaylist(pstPlayCount);
      if Pls.Count>0 then
        p:=Pls.GetItem(0);
      if Pls.Count>1 then begin
        StrPCopy(Pc, Pls.GetItem(1).FileName);
        QueryInsert(Format(RemoveItemDupl, [PrepareString(Pc), IntToStr(Pls.GetItem(1).PlayCount)]));
      end;
    until Cnt<=1;
      StrPCopy(Pc, s);
      Data:=OpenQuery(Format(SelectGetItem, [PrepareString(Pc)]));
      Cnt:=Data.ReturnRecordsCount;
      CloseQuery(Data);
      if (Cnt=0) then AddToDatabase(p^);

    pls.Free;
  end;

  procedure FindDuplicates;
  var
    i: integer;
  begin
    NotShown:=true;
    for i:=0 to s.Count-1 do begin
      if IsDuplicated(s.Strings[i], i) then begin
          //hLog.Send('MEDIA LIBRARY: Removing duplicates for '+s.Strings[i]+
          //  ' ('+IntToStr(i)+')');
          RemoveDuplicates(s.Strings[i]);
        end;
    end;
    if not NotShown then begin
        //Self.fDataBase.aDatabase.PackTable('meta', '');
        //WizardProgressForm.Free;
      end;
  end;


begin
  Data:=OpenQuery('SELECT * FROM meta');
  s:=TStringList.Create;

  //hLog.Send('MEDIA LIBRARY: Compact part 1');

  if Data.ReturnRecordsCount>0 then for x:=0 to Data.ReturnRecordsCount-1 do begin
      s.Add(RetFields(Data).FileName);
      QueryFindNext(Data);
    end;

  CloseQuery(Data);

  //hLog.Send('MEDIA LIBRARY: Compact part 2');

  if s.Count>0 then begin
    for i:=0 to s.Count-1 do
      if (not FileExists(s.Strings[i])) or
      (not (KSPMainWindow.MediaFoldersList.FileInFolders(s.Strings[i])))
      or (s.Strings[i]='')
      or (FileSupportList.FindExtension(ExtractFileExt(s.Strings[i]), false)=-1) then
      begin
        StrPCopy(Pc, s.Strings[i]);
        QueryInsert(Format(RemoveItem, [PrepareString(Pc)]));
        //fDataBase.aQuery.ExecSQL;
      end;
    FindDuplicates;
  end;
  s.Free;
//  fDatabase.Database.PackTable('meta', '');
//  fDatabase.Database
//  hLog.Send('MEDIA LIBRARY: Compact done');
end;

procedure TMediaItemsList.CloseQuery;
begin
  fDataBase.CloseQuery;
//    Table.Free;
end;

function TMediaItemsList.GetFavList(FileName: string): TFavouriteList;
var
  f: TFavouriteList;
  Data: TAppDBConnection;
  P: TPLEntry;
  PC: TPathChar;

  procedure FillFavList;
  var
    i: integer;
    pns: TPlayNextSong;
    Data: TAppDBConnection;
  begin
    Data:=OpenQuery('SELECT * FROM vdjentries WHERE I_NAME='+IntToStr(p.IM));

    if Data.ReturnRecordsCount>0 then
      for i := 0 to Data.ReturnRecordsCount - 1 do begin
        pns.PlayCount:=Data.ReturnFields.FieldByName('PlayCount').AsInteger;
        pns.Favourite:=Data.ReturnFields.FieldByName('Fav').AsFloat;
        pns.FileName:=Data.ReturnFields.FieldByName('FileName').AsString;
        QueryFindNext(Data);
        f.Add(pns, Self, false);
      end;

    CloseQuery(Data);
  end;

begin
  StrPCopy(PC, FileName);
  try
  Data:=OpenQuery(Format(SelectGetItem, [PrepareString(Pc)]));
  if Data.ReturnRecordsCount>0 then begin
      p:=RetFields(Data);
      CloseQuery(Data);
      f:=TFavouriteList.Create(FileName, p.IM);
      FillFavList;
      if f.Count>0 then f.Sort;
    end else begin
      CloseQuery(Data);
      f:=TFavouriteList.Create;
      f.InternalName:=p.IM;
    end;
  Result:=f;
  except
    Result:=nil;
  end;
end;  }

procedure GetFav(f: TFavouriteList; var e: TPlayNextSong);
var
  T: Cardinal;
  i: integer;
begin
  if f.Count=1 then e.Favourite:=1 else
  begin
    T:=0;
    for i:=0 to f.Count-1 do
      T:=T+f.GetItem(i).PlayCount;
    e.Favourite:=e.PlayCount / T;
  end;
end;



{procedure TMediaItemsList.AddPlayNext(FileName: string; OldFileName: string);
var
  e: TPlayNextSong;
  i: integer;
  //t: TMediaItemInfo;

begin
  if not FileExists(FileName) then Exit;
  if not Self.FileInLib(FileName) then Exit;

  FavouriteList:=Self.GetFavList(OldFileName);//TFavouriteList.Create(OldFileName, fXML);
  //i:=fXML.FindSong(FileName);
  //fXML.ReadEntry(i, FavouriteList);
  i:=FavouriteList.FindItem(FileName);
//  hLog.Send('MEDIA LIBRARY Fav list: Item Found at: '+IntToStr(i));
  if i=-1 then begin
      e.FileName:=FileName;
      e.PlayCount:=1;
      e.Favourite:=1;
      FavouriteList.Add(e, Self, true);
    end else begin
      e:=FavouriteList.GetItem(i);
      e.PlayCount:=e.PlayCount+1;
      GetFav(FavouriteList, e);
      FavouriteList.ReplaceEntry(e, Self);
    end;
//  hLog.Send('MEDIA LIBRARY: Adding fav for '+OldFileName);
//  fXML.Add(OldFileName, FavouriteList);

//  hLog.Send('MEDIA LIBRARY: Adding done');
  FavouriteList.Free;
end;

function TMediaItemsList.Count: integer;
var
  Data: TAppDBConnection;
begin
  Data:=OpenQuery('Select * FROM meta');
  Result:=Data.ReturnRecordsCount;
  CloseQuery(Data);
end;

procedure TMediaItemsList.Remove(FileName: string);
var
  Pc: TPathChar;
begin
  StrPCopy(Pc, FileName);
  QueryInsert(Format(RemoveItem, [PrepareString(Pc)]));
end;

{function TMediaItemsList.GetItem(Index: Integer): TPLEntry;
begin
  //TMediaItemInfo(Items[Index]).Free;
  Result:=GetItem2(Index);
end;  }

{function TMediaItemsList.ReturnFromGID(FileName: string; GID: integer): TPlayList;
var
  i: integer;
  p: TPLEntry;
  Data: TAppDBConnection;
begin
  Result:=TPlayList.Create;
  try
  Data:=OpenQuery('SELECT * FROM meta where GID='+IntToStr(GID));

  if Data.ReturnRecordsCount>0 then
  for i:=0 to Data.ReturnRecordsCount-1 do begin
      p:=RetFields(Data);
      QueryFindNext(Data);
          if p.MetaTag=0 then p.Tag:=ReadID3(p.FileName);

          if (UpperCase(FileName)<>UpperCase(p.FileName)) then
            Result.Add(p);
    end;
  CloseQuery(Data);
  except
    //CloseQuery;
  end;
end;         }

{function TMediaItemsList.ReturnFromArtist(FileName: string; Artist: string): TPlayList;
var
  i: integer;
  p: TPLEntry;
  Data: TAppDBConnection;
  Pc: TPathChar;
begin
  Result:=TPlayList.Create;
  StrPCopy(Pc, Artist);
  try
  Data:=OpenQuery('SELECT * FROM meta where Artist='''+PrepareString(Pc)+'''');

  if Data.ReturnRecordsCount>0 then
  for i:=0 to Data.ReturnRecordsCount-1 do begin
      p:=RetFields(Data);
      QueryFindNext(Data);
          if p.MetaTag=0 then p.Tag:=ReadID3(p.FileName);

          if (UpperCase(FileName)<>UpperCase(p.FileName)) then
            Result.Add(p);
    end;
  CloseQuery(Data);
  except
    //CloseQuery;
  end;
end;

{function TMediaItemsList.GetItemMeta(Index: Integer): TPLEntry;
begin
  Result:=GetItem2(Index);

  if Result.MetaTag=0 then Result.Tag:=ReadID3(Result.FileName);
end; }

{function TMediaItemsList.GetItemPlayCount(Index: Integer): TPLEntry;
begin
  Result:=GetItem2(Index);
end;  }

{procedure TMediaItemsList.SaveInfo(p: TPLEntry);
var
  i: integer;
  fm: TFormatSettings;
begin
  GetLocaleFormatSettings(KSPLangID, fm);
  fm.DecimalSeparator:='.';
  fDataBase.Query.SQL.Text:=Format(SelectGetItem, [PrepareString(p.FileName)]);
  fDataBase.Query.Open;

  i:=fDataBase.Query.RecordCount;

  fDataBase.Query.Close;

  if i=0 then Exit;

  fDataBase.Query.SQL.Text:=Format(UpdateStat, [IntToStr(p.Tag.GID),
                                                IntToStr(p.Tag.Track),
                                                PrepareString(p.Tag.Comment),
                                                PrepareString(p.Tag.Year),
                                                PrepareString(p.Tag.Album),
                                                PrepareString(p.Tag.Artist),
                                                PrepareString(p.Tag.Title),
                                                BoolToStr(p.PlayedEver, true),
                                                IntToStr(p.MetaTag),
                                                IntToStr(p.PlayCount),
                                                FloatToStr(p.Fav, fm),
                                                DateToStr(p.LastPlay),
                                                DateToStr(p.FirstPlay),
                                                PrepareString(p.FileName),
                                                PrepareString(p.Tag.Genre),
                                                PrepareString(p.FileName)]);
  fDataBase.Query.SQL.SaveToFile('sql.txt');

  fDataBase.Query.Open;
end;    }

{procedure TMediaItemsList.SaveItemInfo(Index: integer; p: TPLEntry);
begin
  //if Index>=Count then Exit;
  //TPLEntryInfo(Items[Index]).Entry:=p;
  AdToDatabase(p);
end;  }

{procedure TMediaItemsList.SaveToFile;//(FileName: string);
var
//  sl: TStringList;
  i: integer;
  p: TPLEntry;
begin

  //sl:=TStringList.Create;

  fDataBase.Query.SQL.Text:='SELECT * FROM meta';
  fDataBase.Query.Open;

  for i:=0 to fDataBase.Query.RecordCount-1 do
    begin
      ReadEntry(p, fDataBase.Query.Fields);
      fXML.Add(p.FileName, p.Tag);
      fDataBase.Query.FindNext;
    end;

  fDataBase.Query.Close;

  fXML.UpdateFile;
    //sl.Add(GetItem(i).FileName);

  //sl.SaveToFile(FileName);
end;    }

{procedure TMediaItemsList.RenameInBase(P: TPLEntry; OldName: string);
var
  fm: TFormatSettings;
  i: integer;
  Pc1, Pc2, Pc3, Pc4, Pc5, Pc6, Pc7, Pc8: TPathChar;
  Data: TAppDBConnection;
begin
//  GetLocaleFormatSettings(KSPLangID, fm);
  fm.DecimalSeparator:='.';

    StrPCopy(Pc1, p.Tag.Comment);
    StrPCopy(Pc2, p.Tag.Year);
    StrPCopy(Pc3, p.Tag.Album);
    StrPCopy(Pc4, p.FileName);
    StrPCopy(Pc5, p.Tag.Genre);
    StrPCopy(Pc6, p.Tag.Artist);
    StrPCopy(Pc7, p.Tag.Title);
    StrPCopy(Pc8, OldName);

  Data:=OpenQuery(Format(SelectGetItem, [PrepareString(Pc4)]));

  i:=Data.ReturnRecordsCount;

  CloseQuery(Data);

  if i>0 then
      QueryInsert(Format(UpdateStat, [IntToStr(p.Tag.GID),
                                                IntToStr(p.Tag.Track),
                                                PrepareString(Pc1),
                                                PrepareString(Pc2),
                                                PrepareString(Pc3),
                                                PrepareString(Pc6),
                                                PrepareString(Pc7),
                                                BoolToStr(p.PlayedEver, true),
                                                IntToStr(p.MetaTag),
                                                IntToStr(p.PlayCount),
                                                FloatToStr(p.Fav, fm),
                                                DateToStr(p.LastPlay),
                                                DateToStr(p.FirstPlay),
                                                PrepareString(Pc4),
                                                PrepareString(Pc5),
                                                PrepareString(Pc8)]))
  else
      QueryInsert(Format(InsStat, [IntToStr(p.Tag.GID),
                                                IntToStr(p.Tag.Track),
                                                PrepareString(Pc1),
                                                PrepareString(Pc2),
                                                PrepareString(Pc3),
                                                PrepareString(Pc6),
                                                PrepareString(Pc7),
                                                BoolToStr(p.PlayedEver, true),
                                                IntToStr(p.MetaTag),
                                                IntToStr(p.PlayCount),
                                                FloatToStr(p.Fav, fm),
                                                DateToStr(p.LastPlay),
                                                DateToStr(p.FirstPlay),
                                                PrepareString(Pc4),
                                                PrepareString(Pc5)]));

//      fDataBase.Query.SQL.SaveToFile('sql.txt');
      //fDataBase.aDatabase.ExecSQL;
end;

procedure TMediaItemsList.AddToDataBase(p: TPlaylist);
var
  fm: TFormatSettings;
  i: integer;
  s: TStringList;
  str: string;
  Pc1, Pc2, Pc3, Pc4, Pc5, Pc6, Pc7: TPathChar;

  procedure AddToScript(Index: integer);
  var
    i: integer;
    Data: TAppDBConnection;
  begin
    StrPCopy(Pc1, p.GetItem(Index).Tag.Comment);
    StrPCopy(Pc2, p.GetItem(Index).Tag.Year);
    StrPCopy(Pc3, p.GetItem(Index).Tag.Album);
    StrPCopy(Pc4, p.GetItem(Index).FileName);
    StrPCopy(Pc5, p.GetItem(Index).Tag.Genre);
    StrPCopy(Pc6, p.GetItem(Index).Tag.Artist);
    StrPCopy(Pc7, p.GetItem(Index).Tag.Title);

    Data:=OpenQuery(Format(SelectGetItem, [PrepareString(Pc4)]));
    i:=Data.ReturnRecordsCount;
    CloseQuery(Data);

    if i>0 then
      str:=Format(UpdateStat, [IntToStr(p.GetItem(Index).Tag.GID),
                                                IntToStr(p.GetItem(Index).Tag.Track),
                                                PrepareString(Pc1),
                                                PrepareString(Pc2),
                                                PrepareString(Pc3),
                                                PrepareString(Pc6),
                                                PrepareString(Pc7),
                                                BoolToStr(p.GetItem(Index).PlayedEver, true),
                                                IntToStr(p.GetItem(Index).MetaTag),
                                                IntToStr(p.GetItem(Index).PlayCount),
                                                FloatToStr(p.GetItem(Index).Fav, fm),
                                                DateToStr(p.GetItem(Index).LastPlay),
                                                DateToStr(p.GetItem(Index).FirstPlay),
                                                PrepareString(Pc4),
                                                PrepareString(Pc5),
                                                PrepareString(Pc4)])
  else
      str:=Format(InsStat, [IntToStr(p.GetItem(Index).Tag.GID),
                                                IntToStr(p.GetItem(Index).Tag.Track),
                                                PrepareString(Pc1),
                                                PrepareString(Pc2),
                                                PrepareString(Pc3),
                                                PrepareString(Pc6),
                                                PrepareString(Pc7),
                                                BoolToStr(p.GetItem(Index).PlayedEver, true),
                                                IntToStr(p.GetItem(Index).MetaTag),
                                                IntToStr(p.GetItem(Index).PlayCount),
                                                FloatToStr(p.GetItem(Index).Fav, fm),
                                                DateToStr(p.GetItem(Index).LastPlay),
                                                DateToStr(p.GetItem(Index).FirstPlay),
                                                PrepareString(Pc4),
                                                PrepareString(Pc5)]);

      s.Add(str+';');
  end;

begin
//  GetLocaleFormatSettings(KSPLangID, fm);
  fm.DecimalSeparator:='.';
  s:=TStringList.Create;
  for i := 0 to p.Count - 1 do
    AddToScript(i);

  QueryInsert(s.Text);
  s.Free;
end;

procedure TMediaItemsList.AddToDataBase(p: TPLEntry);
var
  fm: TFormatSettings;
  i: integer;
  s: TStringList;
  Pc1, Pc2, Pc3, Pc4, Pc5, Pc6, Pc7: TPathChar;
  Data: TAppDBConnection;
begin
//  GetLocaleFormatSettings(KSPLangID, fm);
  fm.DecimalSeparator:='.';

    StrPCopy(Pc1, p.Tag.Comment);
    StrPCopy(Pc2, p.Tag.Year);
    StrPCopy(Pc3, p.Tag.Album);
    StrPCopy(Pc4, p.FileName);
    StrPCopy(Pc5, p.Tag.Genre);
    StrPCopy(Pc6, p.Tag.Artist);
    StrPCopy(Pc7, p.Tag.Title);

  Data:=OpenQuery(Format(SelectGetItem, [PrepareString(Pc4)]));
  i:=Data.ReturnRecordsCount;
  CloseQuery(Data);
  s:=TStringList.Create;

  if i>0 then
      s.Text:=Format(UpdateStat, [IntToStr(p.Tag.GID),
                                                IntToStr(p.Tag.Track),
                                                PrepareString(Pc1),
                                                PrepareString(Pc2),
                                                PrepareString(Pc3),
                                                PrepareString(Pc6),
                                                PrepareString(Pc7),
                                                BoolToStr(p.PlayedEver, true),
                                                IntToStr(p.MetaTag),
                                                IntToStr(p.PlayCount),
                                                FloatToStr(p.Fav, fm),
                                                DateToStr(p.LastPlay),
                                                DateToStr(p.FirstPlay),
                                                PrepareString(Pc4),
                                                PrepareString(Pc5),
                                                PrepareString(Pc4)])
  else
      s.Text:=Format(InsStat, [IntToStr(p.Tag.GID),
                                                IntToStr(p.Tag.Track),
                                                PrepareString(Pc1),
                                                PrepareString(Pc2),
                                                PrepareString(Pc3),
                                                PrepareString(Pc6),
                                                PrepareString(Pc7),
                                                BoolToStr(p.PlayedEver, true),
                                                IntToStr(p.MetaTag),
                                                IntToStr(p.PlayCount),
                                                FloatToStr(p.Fav, fm),
                                                DateToStr(p.LastPlay),
                                                DateToStr(p.FirstPlay),
                                                PrepareString(Pc4),
                                                PrepareString(Pc5)]);

//      s.SaveToFile('sql.txt');
//      fDataBase.Query.ExecSQL;
      //Application.ProcessMessages;
      try
        QueryInsert(s.Text);
      finally

      end;
end;

{procedure TMediaItemsList.LoadFromFile;//(FileName: string);
var
  Entry: TPLEntry;
//  sl: TStringList;
  s: string;
  i, x, n: integer;
  t: boolean;
begin

  //if not FileExists(FileName) then Exit;
//  if Self.Count>0 then Self.Clear;
  //fXML.LoadFromFile;
  //fXML.ReadFiles(sl);

  fDataBase.Query.SQL.Text:='SELECT * FROM meta';
  fDataBase.Query.Open;
//  ShowMessage(IntToStr(fDataBase.Query.RecordCount));

  i:=0;
  if fDataBase.Query.RecordCount>0 then begin
  KSPMainWindow.MediaLibProgress.MaxValue:=fDataBase.Query.RecordCount;
  KSPMainWindow.MediaLibProgress.Value:=0;
  KSPMainWindow.MediaLibProgress.Visible:=true;
  for x:=0 to fDataBase.Query.RecordCount-1 do begin
      KSPMainWindow.MediaLibProgress.Value:=KSPMainWindow.MediaLibProgress.Value+1;
      fDataBase.Query.RecNo:=x;
      s:=fDataBase.Query.FieldByName('FileName').AsString;

      if FileExists(s) then begin
          Entry.Tag:=ReadID3(s, t, n);
          Entry.FileName:=s;

          if not t then begin
              Entry.Tag.Track:=0;
              Entry.Tag.GID:=-1;
            end;

          if Entry.Tag.Album='' then Entry.Tag.Album:=SUnknownAlbum;
          if Entry.Tag.Artist='' then Entry.Tag.Artist:=SUnknownArtist;
          if Entry.Tag.Title='' then Entry.Tag.Title:=SUnknownTitle;
          if Entry.Tag.Year='' then Entry.Tag.Year:=SUnknownYear;
          if Entry.Tag.Genre='' then Entry.Tag.Genre:=SUnknownGenre;


          Entry.InternalNumberName:=i;
          Inc(i);
          Add(Entry, true);
        end;
    end;

  end;

  KSPMainWindow.MediaLibProgress.Visible:=false;
  fDataBase.Query.Close;


end;  }

constructor TCDList.Create;
begin
  inherited Create;
end;

{The Items should be freed here but it isn't. Doesn't matter.
TPlayList is created only once and destroyed only while KSP is
to be closed}

destructor TCDList.Destroy;
var
  i: integer;
begin
  for I := 0 to Count-1 do
    TCDEntry(Items[I]).Free;
  inherited Destroy;
end;

procedure TCDList.Add(Entry: TCDEntry);
var
  T: TCDEntry;
begin
  T:=Entry;
  inherited Add(T);
end;

procedure TCDList.Remove(Index: Integer);
begin
  TCDEntry(Items[Index]).Free;
  Delete(Index);
end;

function TCDList.GetItem(Index: Integer): TCDEntry;
begin
  Result:=TCDEntry(Items[Index]);
end;



procedure PrepareLike(var Input: string);
const
  art='[%artist]';
  album='[%album]';
  title = '[%title]';
  genre = '[%genre]';
  year = '[%year]';
  comment = '[%comment]';
  track = '[%track]';
  tracklength = '[%length]';
var
  artp, albump, titlep, genrep, yearp,
  commentp, trackp, tracklengthp: integer;
  tmp, tmp2: string;
  i: integer;
  Pc: TPathChar;

  function CheckForTags: boolean;
  begin
    artp:=Pos(art, Input);
    albump:=Pos(album, Input);
    titlep:=Pos(title, Input);
    genrep:=Pos(genre, Input);
    yearp:=Pos(year, Input);
    commentp:=Pos(comment, Input);
    trackp:=Pos(track, Input);
    tracklengthp:=Pos(tracklength, Input);

    Result:=(artp+albump+titlep+genrep+yearp+commentp+trackp+tracklengthp>0);
  end;

  procedure PrepareFirst;
  begin
    Tmp:='SELECT * FROM meta WHERE';
  end;

  procedure AddAnd;
  begin
    Tmp:=Tmp+' AND';
  end;

  procedure ProduceSQLPart(Field, Value: string);
  var
    Pc: TPathChar;
  begin
    if tmp='' then PrepareFirst else AddAnd;
    StrPCopy(Pc, Value);
    Tmp:=Tmp+' '+Field+' LIKE '''+PrepareString(Pc)+'''';
  end;

  procedure ProduceSQLPart2(Position: integer; FType: string);
  var
    t: string;
  begin
    t:=Copy(Input, Position, Length(Input));
    Delete(t, Pos(FType, t), length(FType));
    Delete(Input, Position, Length(Input));
    if t<>'' then
      while t[1]=' ' do Delete(t, 1, 1);
    if t<>'' then
      while t[Length(t)]=' ' do Delete(t, Length(t), 1);
    if t='' then Exit;
    if FType=art then ProduceSQLPart('Artist', t) else
    if FType=album then ProduceSQLPart('Album', t) else
    if FType=title then ProduceSQLPart('Title', t) else
    if FType=genre then ProduceSQLPart('Genre', t) else
    if FType=year then ProduceSQLPart('Year', t) else
    if FType=comment then ProduceSQLPart('Comment', t) else
    if FType=track then ProduceSQLPart('Track', t);
  end;

  function GetHighest(From: integer): integer;
  begin
    Result:=0;
    if (artp>Result)and(artp<From) then Result:=artp;
    if (albump>Result)and(albump<From) then Result:=albump;
    if (titlep>Result)and(titlep<From) then Result:=titlep;
    if (genrep>Result)and(genrep<From) then Result:=genrep;
    if (yearp>Result)and(yearp<From) then Result:=yearp;
    if (commentp>Result)and(commentp<From) then Result:=commentp;
    if (trackp>Result)and(trackp<From) then Result:=trackp;
    if (tracklengthp>Result)and(tracklengthp<From) then Result:=tracklengthp;
  end;

  function FindString(Position: integer): string;
  begin
    Result:='';
    if (artp=Position) then Result:=art;
    if (albump=Position) then Result:=album;
    if (titlep=Position) then Result:=title;
    if (genrep=Position) then Result:=genre;
    if (yearp=Position) then Result:=year;
    if (commentp=Position) then Result:=comment;
    if (trackp=Position) then Result:=track;
    if (tracklengthp=Position) then Result:=tracklength;
  end;

begin
  if not CheckForTags then begin
      StrPCopy(Pc, Input);
      Input:=Format(SelectGetItemLike, [PrepareString(Pc)]);
      Exit;
    end;

  tmp:='';

  i:=GetHighest(Length(Input));

  while i>0 do begin
      tmp2:=FindString(i);
      ProduceSQLPart2(i, tmp2);
      i:=GetHighest(Length(Input));
    end;

  Input:=Tmp;
end;

procedure FindSongsLike(var Songs: TPlayList; mItems: TAppDBConnection; FileName: string);
var
  i: integer;
  T: TPLEntry;
begin
  Songs.Clear;
  PrepareLike(FileName);
//  StrPCopy(Pc, FileName);
  if FileName='' then Exit;
  mItems.OpenQuery(FileName);

  if mItems.ReturnRecordsCount>0 then
  for i:=0 to mItems.ReturnRecordsCount-1 do begin
      T:=mItems.ReadEntry;
      mItems.GoToNext;
      Songs.Add(T);
    end;

  mItems.CloseQuery;

  Songs.SortPlaylist(pstFileName);
end;

procedure FindSongsArtist(var Songs: TPlayList; mItems: TAppDBConnection; Artist: string);
var
  i: integer;
  T: TPLEntry;
begin
  Songs.Clear;
  mItems.OpenQuery('SELECT * FROM meta');
  if Artist = (SUnknownArtist) then Artist:='';


  if mItems.ReturnRecordsCount>0 then
  for i:=0 to mItems.ReturnRecordsCount-1 do begin
      T:=mItems.ReadEntry;
      mItems.GoToNext;
      if (UpperCase(T.Tag.Artist)=UpperCase(Artist)) then begin
            Songs.Add(T);
          end;
    end;

  mItems.CloseQuery;
  Songs.SortPlaylist(pstArtist);
end;

procedure FindSongsAlbum(var Songs: TPlayList; mItems: TAppDBConnection; Album: string);
var
  i: integer;
  T: TPLEntry;
begin
  Songs.Clear;
  if Album = (SUnknownAlbum) then Album:='';

  mItems.OpenQuery('SELECT * FROM meta');

  if mItems.ReturnRecordsCount>0 then
  for i:=0 to mItems.ReturnRecordsCount-1 do begin
      T:=mItems.ReadEntry;
      mItems.GoToNext;
      if (UpperCase(T.Tag.Album)=UpperCase(Album)) then begin
            Songs.Add(T);
          end;
    end;

  mItems.CloseQuery;
  Songs.SortPlaylist(pstArtist);
end;

procedure FindSongsByYear(var Songs: TPlayList; mItems: TAppDBConnection; Year, Album: string);
var
  i: integer;
  T: TPLEntry;
begin
  Songs.Clear;
  if Album=(SUnknownAlbum) then Album:='';
  if Year=(SUnknownYear) then Year:='';

  mItems.OpenQuery('SELECT * FROM meta');

  if mItems.ReturnRecordsCount>0 then
  for i:=0 to mItems.ReturnRecordsCount-1 do begin
      T:=mItems.ReadEntry;
      mItems.GoToNext;
      if (UpperCase(T.Tag.Album)=UpperCase(Album)) and
        (UpperCase(T.Tag.Year)=UpperCase(Year)) then begin
          if not mItems.IsPlaylist(T.FileName) then
            Songs.Add(T);
          end;
    end;

  mItems.CloseQuery;

  Songs.SortPlaylist(pstArtist);

  //Songs.Sort;
end;

procedure FindSongsByGenre(var Songs: TPlayList; mItems: TAppDBConnection; Genre, Album: string);
var
  i: integer;
  T: TPLEntry;
begin
  Songs.Clear;
  if Album=(SUnknownAlbum) then Album:='';
  if Genre=(SUnknownGenre) then Genre:='';

  mItems.OpenQuery('SELECT * FROM meta');

  if mItems.ReturnRecordsCount>0 then
  for i:=0 to mItems.ReturnRecordsCount-1 do begin
      T:=mItems.ReadEntry;
      mItems.GoToNext;
      if (UpperCase(T.Tag.Album)=UpperCase(Album)) and
        (UpperCase(T.Tag.Genre)=UpperCase(Genre)) then begin
          if not mItems.IsPlaylist(T.FileName) then
            Songs.Add(T);
          end;
    end;

  mItems.CloseQuery;

  Songs.SortPlaylist(pstArtist);

  //Songs.Sort;
end;

procedure FindSongsByYear(var Songs: TPlayList; mItems: TAppDBConnection; Year: string);
var
  i: integer;
  T: TPLEntry;
begin
  Songs.Clear;
  if Year=(SUnknownYear) then Year:='';

  mItems.OpenQuery('SELECT * FROM meta');

  if mItems.ReturnRecordsCount>0 then
  for i:=0 to mItems.ReturnRecordsCount-1 do begin
      T:=mItems.ReadEntry;
      mItems.ReadEntry;
      if (UpperCase(T.Tag.Year)=UpperCase(Year)) then begin
            Songs.Add(T);
          end;
    end;

  mItems.CloseQuery;

  Songs.SortPlaylist(pstArtist);
end;

function ArtistInLib(mItems: TAppDBConnection; Artist: string): boolean;
var
  p: TPathChar;
begin
  StrPCopy(p, Artist);
  mItems.OpenQuery(Format('SELECT * FROM meta WHERE Artist=''%s''', [PrepareString(p)]));
  Result:=mItems.ReturnRecordsCount>0;
  mItems.CloseQuery;
end;

procedure FindSongsByGenre(var Songs: TPlayList; mItems: TAppDBConnection; Genre: string);
var
  i: integer;
  T: TPLEntry;
begin
  Songs.Clear;
  if Genre=(SUnknownGenre) then Genre:='';

  mItems.OpenQuery('SELECT * FROM meta');

  if mItems.ReturnRecordsCount>0 then
  for i:=0 to mItems.ReturnRecordsCount-1 do begin
      T:=mItems.ReadEntry;
      mItems.GoToNext;
      if (UpperCase(T.Tag.Genre)=UpperCase(Genre)) then begin
            Songs.Add(T);
          end;
    end;

  mItems.CloseQuery;

  Songs.SortPlaylist(pstArtist);

end;

procedure ReturnYears(var Years: TCrossList; mItems: TAppDBConnection);
var
  i, ap: integer;
  s: string;
  p: TPLEntry;

  function FindYears(A: string): integer;
  var
    i: integer;
  begin
    Result:=-1;
    if Years.Count>0 then begin
        for i:=0 to Years.Count-1 do
          if UpperCase(TCrossEntry(Years.Items[i]).Name)=UpperCase(A) then Result:=i;
      end;
  end;

  function FindAlbum(A: string; aTemp: TStringList): boolean;
  var
    i: integer;
  begin
    Result:=false;
    if aTemp.Count>0 then begin
        for i:=0 to aTemp.Count-1 do
          if UpperCase(aTemp.Strings[i])=UpperCase(A) then Result:=true;
      end;
  end;

begin
  Years.Clear;

  mItems.OpenQuery('SELECT * FROM meta');

  if mItems.ReturnRecordsCount>0 then
  for i:=0 to mItems.ReturnRecordsCount-1 do begin
      p:=mItems.ReadEntry;
      mItems.GoToNext;
      s:=p.Tag.Year;

      ap:=FindYears(s);

      if ap=-1  then begin
        Years.Add(s);
        ap:=Years.Count-1 end;

      if not FindAlbum(p.Tag.Album, TCrossEntry(Years.Items[ap]).SubList) then
        TCrossEntry(Years.Items[ap]).SubList.Add(p.Tag.Album);

    end;

  mItems.CloseQuery;
  Years.Sort;
end;

procedure ReturnGenres(var Gn: TCrossList; mItems: TAppDBConnection);
var
  i, ap: integer;
  s: string;
  p: TPLEntry;

  function FindGN(A: string): integer;
  var
    i: integer;
  begin
    Result:=-1;
    if Gn.Count>0 then begin
        for i:=0 to Gn.Count-1 do
          if UpperCase(TCrossEntry(Gn.Items[i]).Name)=UpperCase(A) then Result:=i;
      end;
  end;

  function FindAlbum(A: string; aTemp: TStringList): boolean;
  var
    i: integer;
  begin
    Result:=false;
    if aTemp.Count>0 then begin
        for i:=0 to aTemp.Count-1 do
          if UpperCase(aTemp.Strings[i])=UpperCase(A) then Result:=true;
      end;
  end;

begin
  Gn.Clear;

  mItems.OpenQuery('SELECT * FROM meta');

  if mItems.ReturnRecordsCount>0 then
  for i:=0 to mItems.ReturnRecordsCount-1 do begin
      p:=mItems.ReadEntry;
      mItems.GoToNext;
      s:=p.Tag.Genre;

      ap:=FindGn(s);

      if ap=-1 then begin
        Gn.Add(s);
        ap:=Gn.Count-1  end;

      if not FindAlbum(p.Tag.Album, TCrossEntry(Gn.Items[ap]).SubList) then
        TCrossEntry(Gn.Items[ap]).SubList.Add(p.Tag.Album);

    end;
  mItems.CloseQuery;

  Gn.Sort;

end;

procedure ReturnAlbumsFromYear(var Albums: TStringList; mItems: TAppDBConnection; Year: string);
var
  i: integer;
  s: TStringList;
  p: TPLEntry;

  aTemp, ArtTemp: TStringList;

  function FindAlbum(A: string; Year: string): boolean;
  var
    i: integer;
  begin
    Result:=false;
    if aTemp.Count>0 then begin
        for i:=0 to aTemp.Count-1 do
          if (UpperCase(aTemp.Strings[i])=UpperCase(A)) and
            (UpperCase(ArtTemp.Strings[i])=UpperCase(Year)) then Result:=true;
      end;
  end;

begin
  Albums.Clear;
  s:=TStringList.Create;
  aTemp:=TStringList.Create;
  ArtTemp:=TStringList.Create;

  mItems.OpenQuery('SELECT * FROM meta');

  if mItems.ReturnRecordsCount>0 then
  for i:=0 to mItems.ReturnRecordsCount-1 do begin
      p:=mItems.ReadEntry;
      mItems.GoToNext;
      if (not FindAlbum(p.Tag.Album, p.Tag.Year))and
        (p.Tag.Year=Year) then begin
            Albums.Add(p.Tag.Album);
            aTemp.Add(p.Tag.Album);
            ArtTemp.Add(p.Tag.Year);
            s.Add(p.Tag.Year);
          end;

    end;

  mItems.CloseQuery;

  Albums.Sort;

  s.Free;
  aTemp.Free;
  ArtTemp.Free;

end;

procedure ReturnAlbumsFromGenre(var Albums: TStringList; mItems: TAppDBConnection; Gn: string);
var
  i: integer;
  s: TStringList;

  aTemp, ArtTemp: TStringList;
  p: TPLEntry;

  function FindAlbum(A: string; Gn: string): boolean;
  var
    i: integer;
  begin
    Result:=false;
    if aTemp.Count>0 then begin
        for i:=0 to aTemp.Count-1 do
          if (UpperCase(aTemp.Strings[i])=UpperCase(A)) and
            (UpperCase(ArtTemp.Strings[i])=UpperCase(Gn)) then Result:=true;
      end;
  end;

begin
  Albums.Clear;
  s:=TStringList.Create;
  aTemp:=TStringList.Create;
  ArtTemp:=TStringList.Create;

  mItems.OpenQuery('SELECT * FROM meta');

  if mItems.ReturnRecordsCount>0 then
  for i:=0 to mItems.ReturnRecordsCount-1 do begin
      p:=mItems.ReadEntry;
      mItems.ReadEntry;
      if (not FindAlbum(p.Tag.Album, p.Tag.Genre))and
        (p.Tag.Genre=Gn) then begin
            Albums.Add(p.Tag.Album);
            aTemp.Add(p.Tag.Album);
            ArtTemp.Add(p.Tag.Genre);
            s.Add(p.Tag.Genre);
          end;

    end;

  mItems.CloseQuery;

  Albums.Sort;

  s.Free;

end;



procedure ReturnCDArtists(var Artists: TStringList; mItems: TCDList); overload;
var
  i: integer;
  aTemp: TStringList;
  s: string;

  function FindArtist(A: string): boolean;
  var
    i: integer;
  begin
    Result:=false;
    if aTemp.Count>0 then begin
        for i:=0 to aTemp.Count-1 do
          if UpperCase(aTemp.Strings[i])=UpperCase(A) then Result:=true;
      end;
  end;

begin
  Artists.Clear;
  aTemp:=TStringList.Create;

  for i:=0 to mItems.Count-1 do begin
      s:=mItems.GetItem(i).Entry.Artist;

      if not FindArtist(s) then begin
        aTemp.Add(s);
        Artists.Add(s);  end;

    end;

  Artists.Sort;
 aTemp.Free;
end;

procedure ReturnCDAlbums(var Albums: TStringList; mItems: TCDList); overload;
var
  i: integer;
  aTemp: TStringList;
  s: string;

  function FindAlbum(A: string): boolean;
  var
    i: integer;
  begin
    Result:=false;
    if aTemp.Count>0 then begin
        for i:=0 to aTemp.Count-1 do
          if UpperCase(aTemp.Strings[i])=UpperCase(A) then Result:=true;
      end;
  end;

begin
  Albums.Clear;
  aTemp:=TStringList.Create;

  for i:=0 to mItems.Count-1 do begin
      s:=mItems.GetItem(i).Entry.Title;

      if not FindAlbum(s) then begin
        aTemp.Add(s);
        Albums.Add(s);  end;

    end;

  Albums.Sort;
  aTemp.Free;
end;

procedure ReturnCDAlbums(var Albums: TStringList; mItems: TCDList; Artist: string); overload;
var
  i: integer;
  s: TStringList;

  aTemp, ArtTemp: TStringList;

  function FindAlbum(A: string; Art: string): boolean;
  var
    i: integer;
  begin
    Result:=false;
    if aTemp.Count>0 then begin
        for i:=0 to aTemp.Count-1 do
          if (UpperCase(aTemp.Strings[i])=UpperCase(A)) and
            (UpperCase(ArtTemp.Strings[i])=UpperCase(Art)) then Result:=true;
      end;
  end;

begin
  Albums.Clear;
  s:=TStringList.Create;
  aTemp:=TStringList.Create;
  ArtTemp:=TStringList.Create;

  for i:=0 to mItems.Count-1 do begin
      if (not FindAlbum(mItems.GetItem(i).Entry.Title, mItems.GetItem(i).Entry.Artist))and
        (mItems.GetItem(i).Entry.Artist=Artist) then begin
            Albums.Add(mItems.GetItem(i).Entry.Title);
            aTemp.Add(mItems.GetItem(i).Entry.Title);
            ArtTemp.Add(mItems.GetItem(i).Entry.Artist);
            s.Add(mItems.GetItem(i).Entry.Artist);
          end;

    end;

  Albums.Sort;

  s.Free;

end;

procedure FindCDSongs(var Songs: TCDEntry; mItems: TCDList; Artist, Album: string);
var
  i: integer;
  T: TCDEntry;
begin
  if mItems.Count=0 then Exit;
  for i:=0 to mItems.Count-1 do begin
      T:=mItems.GetItem(i);
      if (UpperCase(T.Entry.Title)=UpperCase(Album)) and
        (UpperCase(T.Entry.Artist)=UpperCase(Artist)) then begin
            //ShowMessage(T.Entry.Tracks.Text);
            Songs:=T;
          end;
    end;
end;

procedure FindCDArtistSongs(var Songs: TCDEntry; mItems: TCDList; Artist: string);
var
  i: integer;
  T: TCDEntry;
begin
  if mItems.Count=0 then Exit;
  for i:=0 to mItems.Count-1 do begin
      T:=mItems.GetItem(i);
      if (UpperCase(T.Entry.Artist)=UpperCase(Artist)) then begin
            //ShowMessage(T.Entry.Tracks.Text);
            Songs:=T;
          end;
    end;
end;

procedure SortMediaFavList(FavList: TFavouriteList; mItems: TAppDBConnection;
  Song: TPLEntry; Forb: TStringList; UseGID: boolean; GID: integer;
  UseArtist: boolean; Artist: string; UseAS: boolean);
var
//  s: TStringList;
  i: integer;
  p: TPLEntry;

  function FoundOnForb(s: string): boolean;
  var
    i: integer;
  begin
    Result:=false;
    if Forb.Count=0 then Exit;
    for i:=0 to Forb.Count-1 do
      if UpperCase(Forb.Strings[i])=UpperCase(s) then Result:=true;
    for i := 0 to SuggFindHelpPlaylist.Count - 1 do
      if UpperCase(SuggFindHelpPlaylist.GetItem(i).FileName)=UpperCase(s) then
        Result:=true;
  end;

  procedure PrepareNormal;
  var
    i: integer;
  begin
      //s:=TStringList.Create;
    mItems.OpenQuery('SELECT * FROM meta');

    if mItems.ReturnRecordsCount>0 then
    for i:=0 to mItems.ReturnRecordsCount-1 do begin
      p:=mItems.ReadEntry;
      mItems.GoToNext;

      if FileExists(p.FileName) then begin
          //p.Tag:=ReadID3(p.FileName);
          if (not FoundOnForb(p.FileName)) and (p.FileName<>Song.FileName) then
            SuggFindHelpPlaylist.Add(p);
        end;
    end;

    mItems.CloseQuery;
  end;

begin
  SuggFindHelpPlaylist.Clear;
//  UseAS:=UseAS and KSPMainWindow.UseInternet;
      if FavList.Count>1 then
        FavList.Sort;

      if UseGID then begin
        SuggFindHelpPlaylist.Free;
        SuggFindHelpPlaylist:=mItems.ReturnFromGID(Song.FileName, GID);
      end
      else if UseArtist then begin
        SuggFindHelpPlaylist.Free;
        SuggFindHelpPlaylist:=mItems.ReturnFromArtist(Song.FileName, Artist)
      end
      else PrepareNormal;

//      hLog.Send('Fav list sort 1..');

      if FavList.Count>0 then begin
          for i:=0 to FavList.Count-1 do
            if FileExists(FavList.GetItem(i).FileName) then begin
                p.FileName:=FavList.GetItem(i).FileName;
                p.Tag:=ReadID3(p.FileName);
                p.Fav:=p.Fav+FavList.GetItem(i).Favourite;
                if not FoundOnForb(p.FileName) then
                  SuggFindHelpPlaylist.Add(p);
                SuggFindHelpPlaylist.Add(p);
              end;
        end;

//      hLog.Send('Fav list sort 2...');
  SuggFindHelpPlaylist.SortFav;
  //s.Free;

end;

{function TMediaItemsList.RetVDJEntry: TPlayNextSong;
begin
  try
    Result.PlayCount:=fDataBase.aDatabase.Fields.FieldByName('PlayCount').AsInteger;
  except
    Log.Add('PlayCount is missing');
  end;
  try
    Result.Favourite:=fDataBase.aDatabase.Fields.FieldByName('Fav').AsFloat;
  except
    Log.Add('Fav is missing');
  end;
  try
    Result.FileName:=fDataBase.aDatabase.Fields.FieldByName('FileName').AsString;
  except
    Log.Add('FileName is missing');
  end;
end;  }

{
function TMediaItemsList.ReturnAllItems: TPlaylist;
var
  i: integer;
  Data: TAppDBConnection;
begin
  Result:=TPlaylist.Create;
  Data:=OpenQuery('SELECT * FROM meta');
  if Data.ReturnRecordsCount>0 then
    for i:=0 to Data.ReturnRecordsCount-1 do
      begin
        Result.Add(RetFields(Data));
        QueryFindNext(Data);
      end;
  CloseQuery(Data);
end;

function TMediaItemsList.IsPlaylist(FileName: string): boolean;
begin
  FileName:=ExtractFileExt(UpperCase(FileName));
  Result:=(FileName='.M3U')or(FileName='.PLS')or(FileName='.KPL');
end;     }

constructor TFavouriteList.Create;
begin
  inherited Create;
end;

constructor TFavouriteList.Create(AFileName: string; IM: Integer);
var
//  s: string;
//  s2: string;
//  s3: string;
  Ini: TIniFile;
  sl: TStringList;
//  f: TPlayNextSong;

  procedure ParseEntry(i: integer);
  var
    s: string;
  begin
    s:=Ini.ReadString(IntToStr(i),'FileName', '');
    if s='' then Exit;
    if FileExists(s) then Exit;

    sl.Delete(i);
  end;

begin
  inherited Create;
  fIM:=IM;

  {  fIM:=IM;
  s:=IntToStr(IM);
  if Length(s)>2 then begin
      s2:=Copy(s, 1, 2);
      s3:=Copy(s, 3, Length(s));
    end else begin
      s2:='0';
      s3:=IntToStr(IM);
    end;

  ForceDirectories(KSPDataFolder+'data\vdj\entries\'+s2);
  s:=KSPDataFolder+'data\vdj\entries\'+s2+'\'+s3;

  if not FileExists(s) then Exit;

  Ini:=TIniFile.Create(s);

  sl:=TStringList.Create;
  Ini.ReadSections(sl);

  if sl.Count>0 then begin
      for i:=sl.Count-1 downto 0 do
        ParseEntry(i);

      for i:=0 to sl.Count-1 do begin
          f.Favourite:=Ini.ReadFloat(IntToStr(i), 'Fav', 0);
          f.PlayCount:=Ini.ReadInteger(IntToStr(i), 'PlayCount', 0);
          f.FileName:=Ini.ReadString(IntToStr(i),'FileName', '');

          Add(f);
        end;
    end;

  Ini.Free;
  Sl.Free;   }
end;

{procedure TFavouriteList.Save;
var
  i: integer;
  Ini: TIniFile;
  sl: TStringList;
  f: TPlayNextSong;
  s, s2, s3: string;
begin
  s:=IntToStr(fIM);
  if Length(s)>2 then begin
      s2:=Copy(s, 1, 2);
      s3:=Copy(s, 3, Length(s));
    end else begin
      s2:='0';
      s3:=IntToStr(fIM);
    end;
  s:=KSPDataFolder+'data\vdj\entries\'+s2+'\'+s3;
  Ini:=TIniFile.Create(s);
  sl:=TStringList.Create;
  Ini.ReadSections(sl);

  if sl.Count>0 then for i:=0 to sl.Count-1 do
    Ini.EraseSection(IntToStr(i));

  if Count>0 then for i:=0 to Count-1 do begin
      f:=GetItem(i);
      Ini.WriteFloat(IntToStr(i), 'Fav', f.Favourite);
      Ini.WriteInteger(IntToStr(i), 'PlayCount', f.PlayCount);
      Ini.WriteString(IntToStr(i),'FileName', f.FileName);
    end;

  Ini.Free;
  sl.Free;

//  i:=f.FindSong(AFileName);
//  try
//    f.Add(AFileName, Self);
//  finally
//    f.UpdateFile;
//  end;
end;  }

{The Items should be freed here but it isn't. Doesn't matter.
TPlayList is created only once and destroyed only while KSP is
to be closed}

destructor TFavouriteList.Destroy;
var
  i: integer;
begin
  if Count>0 then
  for I := 0 to Count-1 do
    TFavInfo(Items[I]).Free;
  inherited Destroy;
end;

function TFavouriteList.Add(Entry: TPlayNextSong; Songs: TAppDBConnection; Insert: boolean): boolean;
var
  T: TFavInfo;
  sql: string;
  Pc: TPathChar;
  fm: TFormatSettings;

  function CheckIfExists: boolean;
  var
    i: integer;
  begin
    //Result:=Songs.OpenQuery('SELECT * FROM vdjentries WHERE I_NAME='+IntToStr(fIM)+
    //  ' AND FileName='''+PrepareString(Pc)+'''')>0;
    //Songs.CloseQuery;
    Result:=false;
    if Self.Count>0 then
      for i:=0 to Self.Count-1 do
        if TFavInfo(Items[i]).Entry.FileName=Entry.FileName then Result:=true;
  end;

begin
  StrPCopy(Pc, Entry.FileName);
  Result:=not CheckIfExists;

//  GetLocaleFormatSettings(KSPLangID, fm);
  fm.DecimalSeparator:='.';

  if Result then begin
      T:=TFavInfo.Create;
      T.Entry:=Entry;
      if Insert then begin
          sql:=Format('INSERT INTO vdjentries (FileName, I_NAME, Fav, PlayCount) values (''%s'', %s, %s, %s)',
            [PrepareString(Pc), IntToStr(fIM), FloatToStr(Entry.Favourite, fm),
            IntToStr(Entry.PlayCount)]);
          try
            Songs.ExecuteSQL(sql);
          except
          end;
        end;
      inherited Add(T);
    end;
end;

procedure TFavouriteList.ReplaceEntry(new: TPlayNextSong; Songs: TAppDBConnection);
var
  sql: string;
  Pc: TPathChar;
  fm: TFormatSettings;
begin
  StrPCopy(Pc, new.FileName);
//  GetLocaleFormatSettings(KSPLangID, fm);
  fm.DecimalSeparator:='.';
  sql:=Format('UPDATE vdjentries SET FileName=''%s'', I_NAME=%s, Fav=%s, PlayCount=%s WHERE I_NAME=%s AND FileName=''%s''',
        [PrepareString(Pc), IntToStr(fIM),
        FloatToStr(new.Favourite, fm), IntToStr(new.PlayCount),
        IntToStr(fIM), PrepareString(Pc)]);
  try
    Songs.ExecuteSQL(sql);
  except

  end;
  //TFavInfo(Items[Index]).Entry:=new;
end;

procedure TFavouriteList.Remove(Index: Integer);
begin
  TFavInfo(Items[Index]).Free;
  Delete(Index);
end;

function TFavouriteList.GetItem(Index: Integer): TPlayNextSong;
begin
  Result:=TFavInfo(Items[Index]).Entry;
end;

function TFavouriteList.FindItem(FileName: String): Integer;
var
  i: integer;
begin
  Result:=-1;
  for i:=0 to Count-1 do
    if UpperCase(TFavInfo(Items[i]).Entry.FileName)=UpperCase(FileName) then
      Result:=i;
end;

function CompareFav(Item1, Item2: Pointer): Integer;
begin
  Result := -CompareValue(TFavInfo(Item1).Entry.Favourite, TFavInfo(Item2).Entry.Favourite);
end;

procedure TFavouriteList.Sort;
begin
  inherited Sort(@CompareFav);
end;

end.

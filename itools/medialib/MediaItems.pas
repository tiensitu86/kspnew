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
  TMediaItemsList = class;

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

procedure SortMediaFavList(FavList: TFavouriteList; Song: TPLEntry; Forb: TStringList);

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

//  onestep:=stemp.Count div 100;
//  curstep:=0;
  stemp.SaveToFile(KSPdataFolder+'new_files.txt');
  hLog.Send('Starting reading tags...');
//   if stemp.Count>0 then
  Application.QueueAsyncCall(KSPMainWindow.MediaLibProgressMax, stemp.Count);

//  KSPMainWindow.MediaLibProgress.Visible:=true;
//  KSPMainWindow.MediaLibProgress.Max:=stemp.Count;

  for i:=0 to stemp.Count-1 do begin

   hLog.Send('X:='+IntToStr(x)+'; Count:='+IntToStr(stemp.Count));
   if not FileExists(stemp.Strings[i]) then Continue;
        id3tag.Tag:=ReadID3(stemp.Strings[i], tag, x);
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


      //KSPMainWindow.MediaLibProgress.Position:=i;//KSPMainWindow.MediaLibProgress.Max-stemp.Count;
      Application.QueueAsyncCall(KSPMainWindow.MediaLibProgressInc, 1);
//    KSPMainWindow.MediaLibProgress.ProgressText:=IntToStr(KSPMainWindow.MediaLibProgress.Value)+'/'+
    end;


//  p.Free;
  //while KSPDatabaseThreads>0 do
  //  Sleep(100);
  //KSPMainWindow.MediaLibProgress.Visible:=false;
  Application.QueueAsyncCall(KSPMainWindow.MediaLibProgressHide, 0);
  Result:=mItems.ReturnRecordsCount;
end;

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
      Input:=Format(SelectGetItemLike, ['%'+PrepareString(Pc)+'%']);
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
  hLog.Send('LIKE: '+Input);
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

procedure SortMediaFavList(FavList: TFavouriteList; Song: TPLEntry; Forb: TStringList);
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
    AllSongs.OpenQuery('SELECT * FROM meta');

    if AllSongs.ReturnRecordsCount>0 then
    for i:=0 to AllSongs.ReturnRecordsCount-1 do begin
      p:=AllSongs.ReadEntry;
      AllSongs.GoToNext;

      if FileExists(p.FileName) then begin
          //p.Tag:=ReadID3(p.FileName);
          if (not FoundOnForb(p.FileName)) and (p.FileName<>Song.FileName) then
            SuggFindHelpPlaylist.Add(p);
        end;
    end;

    AllSongs.CloseQuery;
  end;

begin
  SuggFindHelpPlaylist.Clear;
//  UseAS:=UseAS and KSPMainWindow.UseInternet;
  PrepareNormal;

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

end.

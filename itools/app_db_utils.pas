unit app_db_utils;

interface

uses Classes, SysUtils, Forms, mysql50conn, sqlite3conn, sqldb, ProfileFunc, app_sql, DB, ID3Mgmnt,
  Playlists, Math, DOM, XMLRead, LCLIntf;

type TFDatabase = record
    MySQLConnection: TMySQL50Connection;
    SQLiteConnection: TSQLite3Connection;
    SQLQuery: TSQLQuery;
    Trans: TSQLTransaction;
end;

//type TKSPDatabaseType = (kdtMeta, kdtAS);

type
  TAppDBConnection = class;
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
    procedure Clear; override;
    constructor Create(IM: Integer); overload;
    destructor Destroy; override;
    function Add(Entry: TPlayNextSong; Songs: TAppDBConnection): boolean;
    procedure Remove(Index: Integer);
    function GetItem(Index: Integer): TPlayNextSong;
    procedure ReplaceEntry(new: TPlayNextSong; Songs: TAppDBConnection);
    function FindItem(FileName: string): integer;
    procedure Sort;
  end;

  TAppDBConnection = class
  public
    constructor Create;
    destructor Destroy; override;
  private
    Database:app_db_utils.TFDatabase;
  public
    function InitDatabase(FileName : String = ''): Integer;
    function CheckDatabase: Integer;
  private
    function CloseDatabase: Integer;
    function CreateMissingTables(Tables: TStringList): integer;
    procedure AddToDataBase(p: TPLEntry); overload;
    procedure AddToDataBase(p: TPlaylist); overload;
  public
    procedure Add(Entry: TPLEntry; OnLoad: boolean);
    function SetupDatabase: Integer;
    function ExecuteSQL( Sql : String; NoFixName: boolean = false): Integer;
    function OpenQuery(Sql: string): integer;
    function CloseQuery: integer;
    procedure GoToFirst;
    procedure GoToLast;
    procedure GoToNext;
    function EndOfDB: boolean;
    property Query: TSQLQuery read Database.SQLQuery;
    function ReturnFieldStr(Name: string; GoToNext: boolean = true): string;
    function ReturnRecordsCount: integer;
    function ReturnFields: TFields;
    procedure StartTransaction(var Res: integer);
    function TransactionSupported: LongBool;
    procedure TransactionCommit;
    procedure TransactionRollback;
    function InTransaction: boolean;
    function MultipleTransactionsSupported: boolean;

    function ReadEntry: TPLEntry;
    procedure CompactLib;
    procedure Remove(FileName: string);
    function IsPlaylist(FileName: string): boolean;
    function ReturnFromArtist(FileName: string; Artist: string): TPlayList;
    function ReturnFromGID(FileName: string; GID: integer): TPlayList;
    procedure FindSongs(var Songs: TPlayList; Artist, Album: string);
    function FileInLib(FileName: string): boolean;
    function GetItemIndex(FileName: string): integer;
    procedure SaveLyrics(Lyrics: WideString; IM: integer);
    function ReadLyrics(IM: integer): WideString;
    procedure DeleteLyrics(IM: integer);
    function GetFavList(FileName: string): TFavouriteList;
  end;

var
  Log: TStringList;

implementation

uses Main, Dialogs, KSPConstsVars, IniFiles, KSPMessages, KSPFiles, MultiLog;


function FindNoCase(Text: string; List: TStringList): integer;
var
  i: integer;
begin
  Result:=-1;
  for i := 0 to List.Count - 1 do
    if UpperCase(Text) = UpperCase(List.Strings[i]) then
      Result:=i;
end;

{DIAGNOSTICS}

{Checks if database all tables are present. Doesn't check their structure or
version but creates tables if it is needed}

function TAppDBConnection.CreateMissingTables(Tables: TStringList): integer;
var
  Res: integer;

  function CreateTableIfMissing(name, sql: string): boolean;
  var
    i: integer;
  begin
    hLog.Send('Creating table: '+name);
    Result:=false;
    if Tables<>nil then
      i:=FindNoCase(name, Tables) else i:=-1;
    if i=-1 then
      begin
        ExecuteSQL(sql);
        Result:=true;
      end;
  end;

begin
  Res:=0;

  CreateTableIfMissing('artists', TB_ARTISTS);
  CreateTableIfMissing('meta', TB_META);
  CreateTableIfMissing('lyrics', TB_LYRICS);
  Result:=Res;
end;

{Checks structure of each present table and updates it if needed. Also checks if
newer version was installed before so we cannot go backwards with it}

constructor TAppDBConnection.Create;
begin
	inherited Create;
  InitializeCriticalSection(DBCritSection)
end;

destructor TAppDBConnection.Destroy;
begin
  CloseDatabase;
  DeleteCriticalSection(DBCritSection);
	inherited Destroy;
end;

function TAppDBConnection.InitDatabase( FileName : String = '' ): Integer;
begin
  Result:=SetupDatabase;
end;

function TAppDBConnection.CheckDatabase: Integer;
begin
  Result:=InitDatabase;
end;

function TAppDBConnection.CloseDatabase: Integer;
begin
  Result:=0;
  hLog.Send('Closing database');
  try
    Database.SQLiteConnection.CloseDataSets;

    Database.SQLQuery.Close;
    Database.SQLQuery.Free;

    Database.SQLiteConnection.Close;
    Database.SQLiteConnection.Free;

  except
    on Ex: Exception do begin
      Result:=-1;
      hLog.Send('ERROR: '+Ex.Message);
    end;
  end;
end;

function TAppDBConnection.SetupDatabase: Integer;
var
  Tables: TStringList;
  db_name: string;
  db_exists: boolean;
  ParamsLoaded: boolean;

  procedure SetupSQLite;
  var
    FileName: string;
  begin
    Result:=0;
    Database.SQLiteConnection:=TSQLite3Connection.Create(nil);
    Database.SQLQuery:=TSQLQuery.Create(nil);
    Database.Trans:=TSQLTransaction.Create(nil);
//  Database.SQLMonitor:=TSQLmonitor.Create(nil);
    Database.SQLQuery.Database:=Database.SQLiteConnection;

    FileName:=KSPDataFolder+'db\db.sqlite';
    FixFolderNames(FileName);
    Database.SQLiteConnection.DatabaseName:=FileName;
    try
      Database.SQLiteConnection.Open;
    except
      Result:=3;
    end;

    if Result=0 then begin
        Database.Trans.DataBase:=Database.SQLiteConnection;
        Database.SQLiteConnection.Transaction:=Database.Trans;
        Database.SQLQuery.Transaction:=Database.Trans;

        Tables:=TStringList.Create;

        Database.Trans.StartTransaction;

        Database.SQLiteConnection.GetTableNames(Tables);

        Database.Trans.Commit;

        CreateMissingTables(Tables);

        Tables.Free;
      end;
  end;

begin
  SetupSqlite;

  hLog.Send('Database set');
//  end;
end;

function TAppDBConnection.ExecuteSQL( Sql : String; NoFixName: boolean = false): Integer;
var
  s: string;
begin
  //hLog.Send('Entering Execute SQL: '+Sql);

  LCLIntf.EnterCriticalSection(DBCritSection);
  try
  if Database.SQLQuery.Active then Result:=-1 else begin//Database.SQLQuery.Active:=false;
    if not NoFixName then
      FixFileNameDB(sql);
    Log.Add('Executing SQL: '+sql);
    s:=KSPDataFolder+'\sql.log';
    FixFolderNames(s);
    Log.SaveToFile(s);
    Database.SQLQuery.SQL.Text:=Sql;
//    Database.SQLQuery.Open;
//    Database.SQLQuery.Close;
    Database.SQLQuery.ExecSQL;
    Database.Trans.Commit;
    Result:=0;//Database.SQLQuery.Result;
    if Result=Database.SQLQuery.RowsAffected then
      Result:=0;
  end;
  finally
    LCLIntf.LeaveCriticalSection(DBCritSection);
  end;
end;

function TAppDBConnection.OpenQuery(Sql: string): integer;
var
  s: string;
begin
  LCLIntf.EnterCriticalSection(DBCritSection);
  try
  if Database.SQLQuery.Active then Result:=-1 else begin
    FixFileNameDB(sql);
    Database.SQLQuery.SQL.Text:=Sql;
    Log.Add('Opening query for: '+sql);
    s:=KSPDataFolder+'\sql.log';
    FixFolderNames(s);
    Log.SaveToFile(s);
    Database.SQLQuery.Open;
    Result:=0;
  end;
  finally

  end;
end;

function TAppDBConnection.CloseQuery: integer;
begin
  if not Database.SQLQuery.Active then Result:=-1 else begin
    Database.SQLQuery.Close;
    Result:=0;
    LCLIntf.LeaveCriticalSection(DBCritSection);
  end;
end;

function TAppDBConnection.ReturnFieldStr(Name: string; GoToNext: boolean = true): string;
begin
  if not Database.SQLQuery.Active then Result:='' else begin
//    Database.SQLQuery.FieldList.SaveToFile(ExtractFilePath(Application.ExeName)+'temp.txt');
    Result:=Database.SQLQuery.FieldByName(Name).AsString;

    if GoToNext then
      Database.SQLQuery.Next;
  end;
end;

function TAppDBConnection.ReturnRecordsCount: integer;
var
  cnt: integer;
begin
  cnt:=0;
  if Database.SQLQuery.Active then begin
    while not EndOfDB do begin
      GoToNext;
      Inc(cnt);
    end;
    GoToFirst;
  end;
  Result:=cnt;
end;

procedure TAppDBConnection.GoToFirst;
begin
  if not Database.SQLQuery.Active then Exit;
  Database.SQLQuery.First;
end;

procedure TAppDBConnection.GoToLast;
begin
  if not Database.SQLQuery.Active then Exit;
  Database.SQLQuery.Last;
end;

procedure TAppDBConnection.GoToNext;
begin
  if not Database.SQLQuery.Active then Exit;
  Database.SQLQuery.Next;
end;

function TAppDBConnection.EndOfDB: boolean;
begin
  Result:=Database.SQLQuery.EOF;
end;

procedure TAppDBConnection.StartTransaction(var Res: integer);
begin
  Res:=0;
  if Database.Trans.Active then
    Res:=1;

  if Res=0 then
    Database.Trans.StartTransaction;
end;

function TAppDBConnection.TransactionSupported: LongBool;
begin
  Result:=true;//Database.SQLConnection.TransactionsSupported;
end;

procedure TAppDBConnection.TransactionCommit;
begin
  Database.Trans.Commit;
end;

procedure TAppDBConnection.TransactionRollback;
begin
  Database.Trans.Rollback;
end;

function TAppDBConnection.InTransaction: boolean;
begin
  Result:=Database.Trans.Active;
end;

function TAppDBConnection.ReturnFields: TFields;
begin
  Result:=Self.Database.SQLQuery.Fields;
end;

function TAppDBConnection.MultipleTransactionsSupported: boolean;
begin
  Result:=false;//Database.SQLConnection.MultipleTransactionsSupported;
end;

function TAppDBConnection.ReadEntry: TPLEntry;
var
  s: TStringList;
  p: TPLEntry;

    function FieldExists(fname: string): boolean;
    var
      i: integer;
    begin
      Result:=false;
      if s.Count>0 then for i:=0 to s.Count-1 do
        if UpperCase(s.Strings[i])=UpperCase(fname) then Result:=true;
    end;

begin
      s:=TStringList.Create;
      Self.Database.SQLQuery.GetFieldNames(s);
      p.FirstPlay:=StrToDate(Self.Database.SQLQuery.FieldByName('FirstPlay').AsString);
      p.LastPlay:=StrToDate(Self.Database.SQLQuery.FieldByName('LastPlay').AsString);
      p.Fav:=StrToFloat(Self.Database.SQLQuery.FieldByName('Fav').AsString);

      p.PlayCount:=Self.Database.SQLQuery.FieldByName('PlayCount').AsInteger;
//      p.PlayedEver:=f.FieldByName('PlayedEver').AsBoolean;

      p.FileName:=Self.Database.SQLQuery.FieldByName('FileName').AsString;

      //p.Tag:=ReadID3(p.FileName);
      p.MetaTag:=Self.Database.SQLQuery.FieldByName('Meta').AsInteger;

      p.Tag.Title:=Self.Database.SQLQuery.FieldByName('Title').AsString;
      p.Tag.Artist:=Self.Database.SQLQuery.FieldByName('Artist').AsString;

      p.Tag.Album:=Self.Database.SQLQuery.FieldByName('Album').AsString;
      p.Tag.Year:=Self.Database.SQLQuery.FieldByName('metaYear').AsString;
      p.Tag.Comment:=Self.Database.SQLQuery.FieldByName('Comment').AsString;
      p.Tag.Genre:=Self.Database.SQLQuery.FieldByName('Genre').AsString;

      p.Tag.Track:=Self.Database.SQLQuery.FieldByName('Track').AsInteger;

      if FieldExists('I_Name') then
        p.IM:=Self.Database.SQLQuery.FieldByName('I_Name').AsInteger;
      p.Tag.GID:=Self.Database.SQLQuery.FieldByName('GID').AsInteger;

      s.Free;

      Result:=p;


//      ShowMessage(IntToStr(f.Count));
//      ShowMessage(IntToStr(f.FieldByName('I_Name').AsInteger));

//      p.InternalNumberName:=f.FieldByName('IM').AsInteger;
end;

procedure TAppDBConnection.CompactLib;
var
  Pc: TPathChar;
  s: TStringList;

  procedure Step1;
  var
    x: integer;
  begin
    OpenQuery('SELECT * FROM meta');
    if ReturnRecordsCount>0 then for x:=0 to ReturnRecordsCount-1 do begin
      s.Add(ReadEntry.FileName);
      GoToNext;
    end;
    hLog.Send('MEDIA LIBRARY: '+IntToStr(s.Count)+' items returned');
    CloseQuery;
  end;


  procedure Step2;
  var
    i: integer;
    str: string;
  begin
    hLog.Send('MEDIA LIBRARY: '+IntToStr(s.Count)+' items to process');
    if s.Count>0 then begin
    for i:=0 to s.Count-1 do
      if (not FileExists(s.Strings[i])) or
      (not (KSPMainWindow.MediaFoldersList.FileInFolders(s.Strings[i])))
      or (s.Strings[i]='')
      or (FileSupportList.FindExtension(ExtractFileExt(s.Strings[i]), false)=-1) then
      begin
        if not FileExists(s.Strings[i]) then hLog.Send('MEDIA LIBRARY: File doesn''t exist: '+s.Strings[i]);
        str:=s.Strings[i];
        FixFileNameDB2(str);
        hLog.Send('MEDIA LIBRARY: Removing item from library: '+str);
        StrPCopy(Pc, str);
        ExecuteSQL(Format(RemoveItem, [PrepareString(Pc)]));
      end;
    end;
    hLog.Send('MEDIA LIBRARY: Compact done');
  end;

begin
  s:=TStringList.Create;

  Step1;
  Step2;

  s.Free;
end;

procedure TAppDBConnection.AddToDataBase(p: TPlaylist);
var
  fm: TFormatSettings;
  i: integer;
  s: TStringList;
  str: string;
  Pc1, Pc2, Pc3, Pc4, Pc5, Pc6, Pc7: TPathChar;

  procedure AddToScript(Index: integer);
  var
    i: integer;
  begin
    StrPCopy(Pc1, p.GetItem(Index).Tag.Comment);
    StrPCopy(Pc2, p.GetItem(Index).Tag.Year);
    StrPCopy(Pc3, p.GetItem(Index).Tag.Album);
    StrPCopy(Pc4, p.GetItem(Index).FileName);
    StrPCopy(Pc5, p.GetItem(Index).Tag.Genre);
    StrPCopy(Pc6, p.GetItem(Index).Tag.Artist);
    StrPCopy(Pc7, p.GetItem(Index).Tag.Title);

    OpenQuery(Format(SelectGetItem, [PrepareString(Pc4)]));
    i:=ReturnRecordsCount;
    CloseQuery;

    if i>0 then
      str:=Format(UpdateStat, [IntToStr(p.GetItem(Index).Tag.GID),
                                                IntToStr(p.GetItem(Index).Tag.Track),
                                                PrepareString(Pc1),
                                                PrepareString(Pc2),
                                                PrepareString(Pc3),
                                                PrepareString(Pc6),
                                                PrepareString(Pc7),
                                                BoolToStr(p.GetItem(Index).PlayedEver, false),
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
                                                BoolToStr(p.GetItem(Index).PlayedEver, false),
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

  ExecuteSQL(s.Text);
  s.Free;
end;

procedure TAppDBConnection.AddToDataBase(p: TPLEntry);
var
  fm: TFormatSettings;
  i: integer;
  s: TStringList;
  Pc1, Pc2, Pc3, Pc4, Pc5, Pc6, Pc7: TPathChar;
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

  OpenQuery(Format(SelectGetItem, [PrepareString(Pc4)]));
  i:=ReturnRecordsCount;
  CloseQuery;
  s:=TStringList.Create;

  if i>0 then
      s.Text:=Format(UpdateStat, [IntToStr(p.Tag.GID),
                                                IntToStr(p.Tag.Track),
                                                PrepareString(Pc1),
                                                PrepareString(Pc2),
                                                PrepareString(Pc3),
                                                PrepareString(Pc6),
                                                PrepareString(Pc7),
                                                BoolToStr(p.PlayedEver, false),
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
                                                BoolToStr(p.PlayedEver, false),
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
        ExecuteSQL(s.Text);
      finally

      end;
  s.Free;
end;

procedure TAppDBConnection.Add(Entry: TPLEntry; OnLoad: boolean);
begin
  if ExtractFileExt(Entry.FileName)='' then Exit;
  if FileSupportList.FindExtension(UpperCase(ExtractFileExt(Entry.FileName)), false)=-1 then Exit;
  if not OnLoad then AddToDataBase(Entry);
end;

procedure TAppDBConnection.Remove(FileName: string);
var
  Pc: TPathChar;
begin
  StrPCopy(Pc, FileName);
  ExecuteSQL(Format(RemoveItem, [PrepareString(Pc)]));
end;

function TAppDBConnection.IsPlaylist(FileName: string): boolean;
begin
  FileName:=ExtractFileExt(UpperCase(FileName));
  Result:=(FileName='.M3U')or(FileName='.PLS')or(FileName='.KPL');
end;

function TAppDBConnection.ReturnFromGID(FileName: string; GID: integer): TPlayList;
var
  i: integer;
  p: TPLEntry;
begin
  Result:=TPlayList.Create;
  try
  OpenQuery('SELECT * FROM meta where GID='+IntToStr(GID));

  if ReturnRecordsCount>0 then
  for i:=0 to ReturnRecordsCount-1 do begin
      p:=ReadEntry;
      GoToNext;
          if p.MetaTag=0 then p.Tag:=ReadID3(p.FileName);

          if (UpperCase(FileName)<>UpperCase(p.FileName)) then
            Result.Add(p);
    end;
  CloseQuery;
  except
    //CloseQuery;
  end;
end;

function TAppDBConnection.ReturnFromArtist(FileName: string; Artist: string): TPlayList;
var
  i: integer;
  p: TPLEntry;
  Pc: TPathChar;
begin
  Result:=TPlayList.Create;
  StrPCopy(Pc, Artist);
  try
  OpenQuery('SELECT * FROM meta where Artist='''+PrepareString(Pc)+'''');

  if ReturnRecordsCount>0 then
  for i:=0 to ReturnRecordsCount-1 do begin
      p:=ReadEntry;
      GoToNext;
          if p.MetaTag=0 then p.Tag:=ReadID3(p.FileName);

          if (UpperCase(FileName)<>UpperCase(p.FileName)) then
            Result.Add(p);
    end;
  CloseQuery;
  except
    //CloseQuery;
  end;
end;

procedure TAppDBConnection.FindSongs(var Songs: TPlayList; Artist, Album: string);
var
  i: integer;
  T: TPLEntry;

  ar, al: TPathChar;
begin
  Songs.Clear;

  StrPCopy(al, Album);
  StrPCopy(ar, Artist);

  if Artist = ('SUnknownArtist') then Artist:='';
  if Album = ('SUnknownAlbum') then Album:='';

  OpenQuery(Format('SELECT * FROM meta WHERE Artist=''%s'' AND Album=''%s''',
    [PrepareString(ar), PrepareString(al)]));

  if ReturnRecordsCount>0 then
  for i:=0 to ReturnRecordsCount-1 do begin
      T:=ReadEntry;
      GoToNext;
//      if (UpperCase(T.Tag.Album)=UpperCase(Album)) and
//        (UpperCase(T.Tag.Artist)=UpperCase(Artist)) then begin
      if not Self.IsPlaylist(T.FileName) then
        Songs.Add(T);
//          end;
    end;

  CloseQuery;

  Songs.SortPlaylist(pstArtist);
end;

function TAppDBConnection.FileInLib(FileName: string): boolean;
var
  Pc: TPathChar;
begin
  Result:=false;
  //FixFileNameDB(FileName);
  StrPCopy(Pc, FileName);

  OpenQuery(Format(SelectGetItem, [PrepareString(Pc)]));
  if Self.ReturnRecordsCount>0 then
    Result:=true;//RetFields.InternalNumberName;
  CloseQuery;

end;

function TAppDBConnection.GetItemIndex(FileName: string): integer;
var
  Pc: TPathChar;
begin
  Result:=-1;
  //FixFileNameDB(FileName);
  StrPCopy(Pc, FileName);

  OpenQuery(Format(SelectGetItem, [PrepareString(Pc)]));
  if Self.ReturnRecordsCount>0 then
    Result:=ReadEntry.IM;//RetFields.InternalNumberName;
  CloseQuery;

  hLog.Send('IM of '+FileName+' is '+IntToStr(Result));
end;

procedure TAppDBConnection.SaveLyrics(Lyrics: WideString; IM: integer);
var
  sql: string;
begin
  sql:=Format(InsLyrics, [PrepareString(Lyrics), IntToStr(IM)]);
  //ShowMessage(IntToStr(Pos(#13, sql)));
  //ReplaceStr(sql, #13, '\new');
  Self.ExecuteSQL(sql, true);
end;

function TAppDBConnection.ReadLyrics(IM: integer): WideString;
var
  sql: string;
begin
  Result:='';
  //FixFileNameDB(FileName);
 // try
  sql:=Format(SelectLyrics, [IntToStr(IM)]);
  //ReplaceStr(sql, '\n', #13);
  Self.OpenQuery(sql);
  //Self.Database.SQLQuery.SQL.Text:='SELECT * FROM lyrics WHERE item_id=234';
  //Self.Database.SQLQuery.Open;
  if Self.ReturnRecordsCount>0 then begin
    Result:=Self.Database.SQLQuery.FieldByName('lyric').AsString;
    end;
  CloseQuery;
  //except
    //CloseQuery;
  //end;
end;

procedure TAppDBConnection.DeleteLyrics(IM: integer);
begin
  Self.ExecuteSQL(Format(DelLyrics, [IntToStr(IM)]));
end;

function TAppDBConnection.GetFavList(FileName: string): TFavouriteList;
var
  f: TFavouriteList;
  P: TPLEntry;
  PC: TPathChar;
  tmp_xml: string;

  function FindNode(x: TDOMNode; Name: string): TDOMNode;
  var
    i: integer;
  begin
    Result:=nil;
    //if x=nil then Exit;
    for i:=0 to x.ChildNodes.Count-1 do begin
      if UpperCase(x.ChildNodes.Item[i].NodeName)=UpperCase(Name) then begin
          Result:=x.ChildNodes.Item[i]
        end;
      end;
    if Result=nil then hLog.Send('Node not found: '+Name);
  end;

  procedure ProcessFeed;
  var
    XMLPls: TXMLDocument;
    Main, Node: TDOMNode;
    i: integer;
    pls: TPlayList;
    item: TPlayNextSong;
  begin
    hLog.Send('Processing Feed...');
    ReadXMLFile(XMLPls, tmp_xml);
    Main:=FindNode(XMLPls.DocumentElement, 'similartracks');
    if Main=nil then begin
      hLog.Send('No similar tracks found...');
      Exit;
    end;
    hLog.Send('Found '+IntToStr(Main.ChildNodes.Count)+' similar tracks');

    for i:=0 to Main.ChildNodes.Count-1 do begin
      Node:=Main.ChildNodes.Item[i]; //<track>
      p.Tag.Title:=FindNode(Node, 'name').FirstChild.NodeValue;
      p.Tag.Artist:=FindNode(FindNode(Node, 'artist'), 'name').FirstChild.NodeValue;

      pls:=Self.ReturnFromArtist(FileName, p.Tag.Artist);

      if Pls.FindTrack(p.Tag.Artist, '', p.Tag.Title) then begin
        p:=Pls.GetTrack(p.Tag.Artist, '', p.Tag.Title, KSPMainWindow.Forbidden);
        if p.FileName<>'' then begin
          item.FileName:=p.FileName;
          FixFileNameDB2(item.FileName);
          f.Add(item, Self);
        end;
      end;
      pls.Free;
    end;

    hLog.Send('Similar items in library: '+IntToStr(f.Count));
  end;


  function DownloadFeed: boolean;
  var
    s: TStringList;
  begin
    s:=TStringList.Create;
    tmp_xml:='http://'+Url_encode(Format(ASTrackFeed, [p.Tag.Artist, p.Tag.Title]));
    hLog.Send('AS feed: '+tmp_xml);
    Result:=DownloadURLi(tmp_xml, s);
    tmp_xml:=KSPDataFolder+'astrack.xml';
    FixFolderNames(tmp_xml);
    if Result then s.SaveToFile(tmp_xml);
    s.Free;
  end;

  procedure FillFavList;
  begin
    if KSPMainWindow.OfflineMode then Exit;
    DownloadFeed;
    ProcessFeed;
  end;

begin
  StrPCopy(PC, FileName);
  try
  OpenQuery(Format(SelectGetItem, [PrepareString(Pc)]));
  if ReturnRecordsCount>0 then begin
      p:=ReadEntry;
      hLog.Send(Format('Title: %s, Artist: %s', [p.Tag.Title, p.Tag.Artist]));
      CloseQuery;
      f:=TFavouriteList.Create(p.IM);
      FillFavList;
      if f.Count>0 then f.Sort;
    end else begin
      CloseQuery;
      f:=TFavouriteList.Create;
      f.InternalName:=p.IM;
    end;
  Result:=f;
  except
    Result:=nil;
  end;
end;

constructor TFavouriteList.Create;
begin
  inherited Create;
end;

constructor TFavouriteList.Create(IM: Integer);
begin
  inherited Create;
  fIM:=IM;
end;

destructor TFavouriteList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TFavouriteList.Clear;
var
  i: integer;
begin
  if Count>0 then
  for I := Count-1 downto 0 do begin
    TFavInfo(Items[I]).Free;
    Delete(i);
  end;
end;

function TFavouriteList.Add(Entry: TPlayNextSong; Songs: TAppDBConnection): boolean;
var
  T: TFavInfo;
  Pc: TPathChar;

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

  if Result then begin
      T:=TFavInfo.Create;
      T.Entry:=Entry;
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

initialization
  Log:=TStringList.Create;

finalization
  Log.Free;

end.

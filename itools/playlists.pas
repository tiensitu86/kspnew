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

unit PlayLists;

interface

uses Forms, SysUtils, Classes, ID3Mgmnt, Dialogs, FileSupport,
  DateUtils, kspfiles, KSPMessages, SpkXMLParser, IniFiles, DOM,
  XMLRead, FileUtil, plsFrame, Controls;

{This unit includes all playlist management clases and structures}

type
  TPlsType = (plKPL, plM3U, plPLS);

  TPLEntryInfo = class(TObject)
  public
    Entry: TPLEntry;
    Frame: TPlaylistFrame;
    constructor Create;
    destructor Destroy;
  end;

  TPlayListSortType = (pstTrack, pstArtist, pstAlbum, pstYear, pstGenre, pstPlayCount,
    pstFileName);

  //type TPlayList = array of TPLEntry;
  PPLEntry = ^TPLEntry;

  TPlayList = class(TList)
  private
    fMainPls: boolean;
  public
    constructor Create(isMainPlaylist: boolean = false);
    destructor Destroy; override;
    procedure Add(Entry: TPLEntry);
    procedure Insert(Index: integer; Entry: TPLEntry);
    procedure ChangeEntry(Index: integer; Entry: TPLEntry);
    procedure Remove(Index: integer);
    function GetItem(Index: integer): PPLEntry;
    procedure SetPlayed(i: integer);
    procedure UnSetPlayed(i: integer);
    procedure Exchange(Index1, Index2: integer);
    procedure SortFav;
    procedure SortPlaylist(SortType: TPlayListSortType);
    procedure Clear; override;
    function FindArtist(Artist: string): TPlaylist;
    function FindAlbum(Artist, Album: string): TPlaylist;
    function FindArtists(Artist: TStringList): TPlaylist;
    function FindTrack(Artist, Album, Title: string): boolean;
    function GetTrack(Artist, Album, Title: string; ForbList: TStringList): TPLEntry;
    function ArtistInPlaylist(Artist: string): boolean;
    function ContainsFileName(fname: string): boolean;
    function MatchesSearch(Index: integer; Pattern: string;
      Artist, ALbum, Title: boolean): boolean;
  end;

{  TPlayLists = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Entry: TPlayList);
    procedure Remove(Index: Integer);
    function GetItem(Index: Integer): TPlayList;
    procedure UpdateItem(Index: integer; p: TPlayList);
  end;    }

type
  TXMLPlayList = class(TObject)
    //XMLFile: TXMLIniFile;
    fPLSType: TPLSType;
    procedure SetPLSType(Pls: TPlsType);
    function GetPLSType: TPlsType;
    procedure SaveKPLPls(Pls: TPlayList; FileName: string; Relative: boolean);
    procedure SaveM3UPls(Pls: TPlayList; FileName: string; Relative: boolean);
    procedure SavePLSPls(Pls: TPlayList; FileName: string; Relative: boolean);
    procedure LoadKPLPls(FileName: string; var Pls: TPlayList);
    procedure LoadM3UPls(FileName: string; var Pls: TPlayList);
    procedure LoadPlsPls(FileName: string; var Pls: TPlayList);
    procedure LoadPlsXSPF(FileName: string; var Pls: TPlayList);
  public
    property PLSType: TPlsType Read GetPlsType Write SetPlsType;
    constructor Create;
    destructor Destroy; override;
    procedure SavePls(Pls: TPlayList; FileName: string; Relative: boolean);
    procedure LoadPls(FileName: string; var Pls: TPlayList);
  end;

procedure SearchForKPL(Path: string; Rec: boolean; var s: TStringList);

implementation

uses Math, KSPConstsVars, Main, multilog, profilefunc;

function CompareTracks(Item1, Item2: Pointer): integer;
begin
  Result := CompareValue(TPLEntryInfo(Item1).Entry.Tag.Track,
    TPLEntryInfo(Item2).Entry.Tag.Track);
end;

function ComparePlayCount(Item1, Item2: Pointer): integer;
begin
  Result := -CompareValue(TPLEntryInfo(Item1).Entry.PlayCount,
    TPLEntryInfo(Item2).Entry.PlayCount);
end;

function CompareFileName(Item1, Item2: Pointer): integer;
begin
  Result := CompareFileNames(ExtractFileName(TPLEntryInfo(Item1).Entry.FileName),
    ExtractFileName(TPLEntryInfo(Item2).Entry.FileName));
end;

function CompareFav(Item1, Item2: Pointer): integer;
begin
  Result := -CompareValue(TPLEntryInfo(Item1).Entry.Fav, TPLEntryInfo(Item2).Entry.Fav);
end;

function CompareArtist(Item1, Item2: Pointer): integer;
begin
  Result := CompareText(TPLEntryInfo(Item1).Entry.Tag.Artist,
    TPLEntryInfo(Item2).Entry.Tag.Artist);
end;

function CompareAlbum(Item1, Item2: Pointer): integer;
begin
  Result := CompareText(TPLEntryInfo(Item1).Entry.Tag.Album,
    TPLEntryInfo(Item2).Entry.Tag.Album);
end;

function CompareYear(Item1, Item2: Pointer): integer;
begin
  Result := CompareText(TPLEntryInfo(Item1).Entry.Tag.Year,
    TPLEntryInfo(Item2).Entry.Tag.Year);
end;

function CompareGenre(Item1, Item2: Pointer): integer;
begin
  Result := CompareValue(TPLEntryInfo(Item1).Entry.Tag.GID,
    TPLEntryInfo(Item2).Entry.Tag.GID);
end;

procedure SearchForKPL(Path: string; Rec: boolean; var s: TStringList);
var
  sr: TSearchRec;
  FileAttrs: integer;
  s2: TStringList;
  i:  integer;
begin
  FileAttrs := faAnyFile;//+faDirectory;
  s2 := TStringList.Create;

  if FindFirst(Path + '\*.kpl', FileAttrs, sr) = 0 then

  begin
    repeat
      //if (sr.Attr and FileAttrs) = sr.Attr then
      begin
        if (sr.Name <> '') and (sr.Name <> '.') and (sr.Name <> '..') then
        begin
          //ShowMessage(ExtractFileExt(sr.Name));
          if ((sr.Attr and faDirectory) <> sr.Attr) then
            s.Add(Path + '\' + sr.Name);
          if Rec and ((sr.Attr and faDirectory) = sr.Attr) then
            s2.Add(Path + '\' + sr.Name);
        end;

      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;

  if s2.Count > 0 then
    for i := 0 to s2.Count - 1 do
      SearchForKPL(s2.Strings[i], Rec, s);

  s2.Free;

end;

constructor TPLEntryInfo.Create;
begin
  inherited Create;
  Self.Frame:=TPlayListFrame.Create(KSPMainWindow.PlsPanel);
//  Frame.Align:=alTop;
end;

destructor TPLENtryInfo.Destroy;
begin
  Frame.Free;
  inherited Destroy;
end;

procedure TPlayList.SortPlaylist(SortType: TPlayListSortType);
begin
  case SortType of
    pstTrack: Sort(@CompareTracks);
    pstArtist: Sort(@CompareArtist);
    pstAlbum: Sort(@CompareAlbum);
    pstYear: Sort(@CompareYear);
    pstGenre: Sort(@CompareGenre);
    pstPlayCount: Sort(@ComparePlayCount);
    pstFileName: Sort(@CompareFileName);
  end;
end;

constructor TPlayList.Create(isMainPlaylist: boolean = false);
begin
  inherited Create;
  fMainPls:=isMainPlaylist;
end;

{The Items should be freed here but it isn't. Doesn't matter.
TPlayList is created only once and destroyed only while KSP is
to be closed}

destructor TPlayList.Destroy;
  //var
  //  i: integer;
begin
  //  for I := Count-1 downto 0 do
  //    TPLEntryInfo(Items[I]).Free;
  Clear;
  inherited Destroy;
end;

procedure TPlayList.ChangeEntry(Index: integer; Entry: TPLEntry);
begin
  if Count <= Index then
    Exit;
  TPLEntryInfo(Items[Index]).Entry := Entry;
end;

procedure TPlayList.Clear;
var
  i: integer;
  //  p: TPLEntryInfo;
begin
  for i := Count - 1 downto 0 do
  begin
    TPLEntryInfo(Items[i]).Free;
    //FreeAndnil(TPLEntryInfo(Items[i]));
    //FreeAndNil(p);//.Free;//FreeMem(Items[I], SizeOf(TPLEntry);
    Delete(i);
  end;
  inherited;

  //inherited Clear;
end;

procedure TPlayList.Exchange(Index1, Index2: integer);
var
  T: TPLEntry;
begin
  if Index1 = Index2 then
    Exit;
  t := TPLEntryInfo(Items[Index1]).Entry;
  TPLEntryInfo(Items[Index1]).Entry := TPLEntryInfo(Items[Index2]).Entry;
  TPLEntryInfo(Items[Index2]).Entry := t;
end;

procedure TPlayList.Add(Entry: TPLEntry);
var
  T: TPLEntryInfo;
begin
  T := TPLEntryInfo.Create;
  if Entry.Tag.Artist = '' then
    Entry.Tag.Artist := 'Unknown Artist';
  if Entry.Tag.Album = '' then
    Entry.Tag.Album := 'Unknown Album';
  T.Entry := Entry;
  T.Entry.Played := False;

  if Self.fMainPls then
    KSPMainWindow.PlsPanel.InsertControl(T.Frame);

  inherited Add(T);
end;

procedure TPlayList.Insert(Index: integer; Entry: TPLEntry);
var
  T: TPLEntryInfo;
begin
  T := TPLEntryInfo.Create;
  T.Entry := Entry;
  T.Entry.Played := False;
  if Self.fMainPls then
    KSPMainWindow.PlsPanel.InsertControl(T.Frame);

  inherited Insert(Index, T);
end;

procedure TPlayList.Remove(Index: integer);
begin
  TPLEntryInfo(Items[Index]).Free;
  //FreeAndNil(TPLEntryInfo(Items[Index]));
  Delete(Index);
end;

procedure TPlayList.SortFav;
begin
  Sort(@CompareFav);
  ;
end;

function TPlayList.GetItem(Index: integer): PPLEntry;
begin
  if Index >= Count then
    Result := @(TPLEntryInfo(Items[Count - 1]).Entry)
  else
  if Index < 0 then
    Result := @(TPLEntryInfo(Items[0]).Entry)
  else
    Result := @(TPLEntryInfo(Items[Index]).Entry);
end;

function TPlayList.FindArtist(Artist: string): TPlaylist;
var
  i: integer;
  p: TPLEntry;
begin
  Result := TPlaylist.Create(false);
  for i := 0 to Count - 1 do
  begin
    p := GetItem(i)^;
    if UpperCase(p.Tag.Artist) = UpperCase(Artist) then
      Result.Add(p);
  end;
end;

function TPlayList.ArtistInPlaylist(Artist: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    if UpperCase(GetItem(i).Tag.Artist) = UpperCase(Artist) then
      Result := True;
  end;
end;

function TPlayList.ContainsFileName(fname: string): boolean;
var
  x: integer;
begin
  Result := False;
  for x := 0 to Count - 1 do
  begin
    Result := Result or (CompareFilenames(fname, GetItem(x)^.FileName) = 0);
  end;
end;

function TPlayList.MatchesSearch(Index: integer; Pattern: string;
  Artist, ALbum, Title: boolean): boolean;
var
  p: TPLEntry;
begin
  p      := Self.GetItem(Index)^;
  Result := False;
  if Artist then
    Result := Pos(UpperCase(Pattern), UpperCase(p.Tag.Artist)) <> 0;
  if Album then
    Result := Result or (Pos(UpperCase(Pattern), UpperCase(p.Tag.Album)) <> 0);
  if Title then
    Result := Result or (Pos(UpperCase(Pattern), UpperCase(p.Tag.Title)) <> 0);
end;

function TPlayList.FindAlbum(Artist, Album: string): TPlaylist;
var
  i: integer;
  p: TPLEntry;
begin
  Result := TPlaylist.Create(false);
  for i := 0 to Count - 1 do
  begin
    p := GetItem(i)^;
    if (UpperCase(p.Tag.Artist) = UpperCase(Artist)) and
      (UpperCase(p.Tag.Album) = UpperCase(Album)) then
      Result.Add(p);
  end;
end;

function TPlayList.FindArtists(Artist: TStringList): TPlaylist;
var
  i, x: integer;
  p:    TPLEntry;
begin
  Result := TPlaylist.Create(false);
  for i := 0 to Count - 1 do
  begin
    p := GetItem(i)^;
    for x := 0 to Artist.Count - 1 do
      if UpperCase(p.Tag.Artist) = UpperCase(Artist.Strings[x]) then
        Result.Add(p);
  end;
end;

function TPlayList.FindTrack(Artist, Album, Title: string): boolean;
var
  UseArtist, UseAlbum: boolean;
  i: integer;

  function CheckIfMatch(Index: integer): boolean;
  begin
    Result := UpperCase(GetItem(Index).Tag.Title) = UpperCase(Title);
    if UseArtist then
      Result := Result and (UpperCase(GetItem(Index).Tag.Artist) = UpperCase(Artist));
    if UseAlbum then
      Result := Result and (UpperCase(GetItem(Index).Tag.Album) = UpperCase(Album));
  end;

begin
  UseArtist := Artist <> '';
  UseAlbum  := Album <> '';
  Result    := False;
  for i := 0 to Count - 1 do
    Result := Result or CheckIfMatch(i);
end;

function TPlayList.GetTrack(Artist, Album, Title: string;
  ForbList: TStringList): TPLEntry;
var
  UseArtist, UseAlbum: boolean;
  i: integer;
  LastRemembered: integer;

  function CheckIfMatch(Index: integer): boolean;
  var
    x: integer;
  begin
    Result := (UpperCase(GetItem(Index).FileName) <> KSPMainWindow.GetCurrentFile) and
      (UpperCase(GetItem(Index).Tag.Title) = UpperCase(Title));

    if UseArtist then
      Result := Result and (UpperCase(GetItem(Index).Tag.Artist) = UpperCase(Artist));
    if UseAlbum then
      Result := Result and (UpperCase(GetItem(Index).Tag.Album) = UpperCase(Album));
    if Result then
    begin
      LastRemembered := Index;
      for x := 0 to ForbList.Count - 1 do
        if UpperCase(GetItem(Index).FileName) = UpperCase(ForbList.Strings[x]) then
          Result := False;
    end;
  end;

begin
  LastRemembered := -1;
  UseArtist := Artist <> '';
  UseAlbum := Album <> '';
  Result.FileName := '';
  for i := 0 to Count - 1 do
    if CheckIfMatch(i) then
      Result := GetItem(i)^;

  if (Result.FileName = '') and (LastRemembered > -1) then
    Result := GetItem(LastRemembered)^;
end;


procedure TPlayList.SetPlayed(i: integer);
begin
  if (i < Count) and (i >= 0) then
    TPLEntryInfo(Self.Items[i]).Entry.Played := True;
end;

procedure TPlayList.UnSetPlayed(i: integer);
begin
  if (i < Count) and (i >= 0) then
    TPLEntryInfo(Self.Items[i]).Entry.Played := False;
end;

{constructor TPlayLists.Create;
begin
  inherited Create;
end;  }

{The Items should be freed here but it isn't. Doesn't matter.
TPlayList is created only once and destroyed only while KSP is
to be closed}

{destructor TPlayLists.Destroy;
var
  i: integer;
begin
  for I := 0 to Count-1 do
    TPlayList(Items[I]).Free;
  inherited Destroy;
end;

procedure TPlayLists.Add(Entry: TPlayList);
begin
  inherited Add(Entry);
end;

procedure TPlayLists.Remove(Index: Integer);
begin
  Delete(Index);
end;

function TPlayLists.GetItem(Index: Integer): TPlayList;
begin
  Result:=TPlayList(Items[Index]);
end;

procedure TPlayLists.UpdateItem(Index: integer; p: TPlayList);
begin
  TPlayList(Items[Index]).Assign(p);
end;  }

function TXMLPlaylist.GetPLSType: TPlsType;
begin
  Result := fPlsType;
end;

procedure TXMLPlaylist.SetPLSType(Pls: TPlsType);
begin
  if Pls <> fPlsType then
    fPlsType := Pls;
end;

constructor TXMLPlaylist.Create;
begin
  inherited Create;
  fPlsType := plKPL;
end;

destructor TXMLPlaylist.Destroy;
begin
  inherited Destroy;
end;

procedure TXMLPlaylist.SaveKPLPls(Pls: TPlayList; FileName: string; Relative: boolean);
var
  i:      integer;
  XMLPls: TSpkXMLParser;
  Node, Main, Info, Tag: TSpkXMLNode;
  Entry:  TSpkXMLParameter;
  InfoParams: TSpkXMLParameter;
  P:      PPLEntry;
  Pc:     TPathChar;
  //CurrentD: string;
begin
  //if Pls.Count=0 then Exit;
  if FileExists(FileName) then
    DeleteFile(FileName);

  //CurrentD:=GetCurrentDir;
  //SetCurrentDir(ExtractFilePath(FileName));

  XMLPls := TSpkXMLParser.Create;
  Main   := TSpkXMLNode.Create('xml');
  Info   := TSpkXMLNode.Create('info');
  InfoParams := TSpkXMLParameter.Create('creation_day', DateToStr(Now));
  Info.Parameters.Add(InfoParams);
  InfoParams := TSpkXMLParameter.Create('modified_day', DateToStr(Now));
  Info.Parameters.Add(InfoParams);
  InfoParams := TSpkXMLParameter.Create('author', '');
  Info.Parameters.Add(InfoParams);
  InfoParams := TSpkXMLParameter.Create('player', Application.Title);
  Info.Parameters.Add(InfoParams);
  InfoParams := TSpkXMLParameter.Create('player_version', KSPVersion2);
  Info.Parameters.Add(InfoParams);
  InfoParams := TSpkXMLParameter.Create('kpl_version', KSPPlaylistsVersion);
  Info.Parameters.Add(InfoParams);

  if Pls.Count > 0 then
    for i := 0 to Pls.Count - 1 do
    begin
      P    := Pls.GetItem(i);
      Node := TSpkXMLNode.Create(IntToStr(i));
      Tag  := TSpkXMLNode.Create('tag');
      if Relative then
        Entry := TSpkXMLParameter.Create('filename',
          ExtractRelativePath(FileName, p.FileName))
      else
        Entry := TSpkXMLParameter.Create('filename', p.FileName);
      Node.Parameters.Add(Entry);
      StrPCopy(Pc, p.FileName);
      if (IsStream(PC)) or (not p.Tag.IsTag) then
      begin
        p.Tag.Album   := '';
        p.Tag.Artist  := '';
        p.Tag.Comment := '';
        p.Tag.Title   := '';
        p.Tag.Year    := '';
        p.Tag.Genre   := '';
        p.Tag.Track   := 0;
        p.Tag.GID     := 0;
      end;

      //saving tag info
      Entry := TSpkXMLParameter.Create('artist', p.Tag.Artist);
      Tag.Parameters.Add(Entry);
      Entry := TSpkXMLParameter.Create('album', p.Tag.Album);
      Tag.Parameters.Add(Entry);
      Entry := TSpkXMLParameter.Create('title', p.Tag.Title);
      Tag.Parameters.Add(Entry);
      Entry := TSpkXMLParameter.Create('year', p.Tag.Year);
      Tag.Parameters.Add(Entry);
      Entry := TSpkXMLParameter.Create('comment', p.Tag.Comment);
      Tag.Parameters.Add(Entry);
      Entry := TSpkXMLParameter.Create('genre', p.Tag.Genre);
      Tag.Parameters.Add(Entry);
      Entry := TSpkXMLParameter.Create('track', IntToStr(p.Tag.Track));
      Tag.Parameters.Add(Entry);
      Entry := TSpkXMLParameter.Create('gid', IntToStr(p.Tag.GID));
      Tag.Parameters.Add(Entry);
      Entry := TSpkXMLParameter.Create('has_tag', BoolToStr(p.Tag.IsTag, True));
      Tag.Parameters.Add(Entry);
      //building structure
      Node.Add(Tag);
      Main.Add(Node);
    end;

  Main.Add(Info);
  XmlPls.Add(Main);

  XMLPls.SaveToFile(FileName);
  XMLPls.Free;

  //SetCurrentDir(CurrentD);


  {Self.XMLFile:=TXMLIniFile.Create(FileName);
  for i:=0 to Pls.Count-1 do begin
      Self.XMLFile.WriteString(IntToStr(i),'File', Pls.GetItem(I).FileName);
    end;

  Self.XMLFile.UpdateFile;
  Self.XMLFile.Free;   }
end;

procedure TXMLPlaylist.SaveM3UPls(Pls: TPlayList; FileName: string; Relative: boolean);
var
  i:   integer;
  f:   textFile;
  s:   string;
  Dir: string;
begin
  AssignFile(f, FileName);
  Dir := GetCurrentDir;
  SetCurrentDir(ExtractFilePath(FileName));
  if FileExists(FileName) then
    Erase(F);
  Rewrite(f);
  Writeln(f, '#EXTM3U');

  for i := 0 to Pls.Count - 1 do
  begin
    if Relative then
      s := ExtractRelativePath(FileName, Pls.GetItem(i).FileName)
    else
      s := Pls.GetItem(i).FileName;
    Writeln(f, s);
    //ShowMessage(s);
  end;

  CloseFile(f);
  SetCurrentDir(Dir);
end;

procedure TXMLPlaylist.SavePLSPls(Pls: TPlayList; FileName: string; Relative: boolean);
var
  i:   integer;
  f:   TIniFile;
  s:   string;
  Dir: string;
  p:   PPLEntry;
begin
  if FileExists(FileName) then
    DeleteFile(FileName);
  Dir := GetCurrentDir;
  SetCurrentDir(ExtractFilePath(FileName));
  f := TIniFile.Create(FileName);

  if Pls.Count > 0 then
    for i := 0 to Pls.Count - 1 do
    begin
      p := Pls.GetItem(i);
      s := ProduceFormatedString(KSPMainWindow.GetFormatedPlayListInfo,
        p.Tag, GetDuration(p.Stream), i + 1);
      f.WriteString('playlist', 'File' + IntToStr(i + 1), p.FileName);
      f.WriteString('playlist', 'Title' + IntToStr(i + 1), s);
      f.WriteInteger('playlist', 'Length' + IntToStr(i + 1), p.Stream.Duration div 1000);
    end;

  f.WriteInteger('playlist', 'Version', 2);
  f.WriteInteger('playlist', 'NumberOfEntries', Pls.Count);

  SetCurrentDir(Dir);
  f.Free;
end;

procedure TXMLPlaylist.LoadPlsPls(FileName: string; var Pls: TPlayList);
var
  i:   integer;
  f:   TIniFile;
  Dir: string;
  p:   TPLEntry;
  Pc:  TPathChar;
  Cnt: integer;
begin
  f   := TIniFile.Create(FileName);
  Dir := GetCurrentDir;
  SetCurrentDir(ExtractFilePath(FileName));

  for i := Pls.Count - 1 downto 0 do
    Pls.Remove(i);

  Cnt := f.ReadInteger('playlist', 'NumberOfEntries', 0);
  if Cnt > 0 then
  begin
    for i := 1 to Cnt do
    begin
      p.FileName := f.ReadString('playlist', 'File' + IntToStr(i), '');
      if Length(p.FileName) > MAX_PATH then
        Continue;
      StrPCopy(Pc, p.FileName);
      if not (IsStream(Pc) or IsCD(Pc)) then
        p.FileName := ExpandFileName(p.FileName);
      if p.FileName <> '' then
      begin
        if FileExists(p.FileName) or IsStream(Pc) or IsCD(Pc) then
        begin
          Pls.Add(p);
        end;
      end;
    end;
  end;

  SetCurrentDir(Dir);
  f.Free;
end;

procedure TXMLPlaylist.LoadM3UPls(FileName: string; var Pls: TPlayList);
var
  i, lack: integer;
  f:   textFile;
  s:   string;
  Dir: string;
  p:   TPLEntry;
  Pc:  TPathChar;
begin
  AssignFile(f, FileName);
  //if FileExists(FileName) then Erase(F);
  Reset(f);
  //i:=0;

  Dir := GetCurrentDir;
  SetCurrentDir(ExtractFilePath(FileName));

  for i := Pls.Count - 1 downto 0 do
    Pls.Remove(i);

  Readln(f, s);
  while Length(s) > 0 do
  begin
    if (s[1] <> '#') and (s[1] <> ' ') and (Length(s) < MAX_PATH) then
    begin
{If provided string is too long and it's path to the file then don't process
too it if it is too long (it might lead to stack overflow). This can't be checked
in any other part of this routine (if it is not a file name then BASS should handle this)}
      StrPCopy(Pc, s);
      if not (IsStream(Pc) or IsCD(Pc)) then
      begin
        try
          p.FileName := ExpandFileName(s)
        except
          Continue;
        end;
      end
      else
        p.FileName := s;
      //if not FileExists(p.FileName) then
      //  p.FileName:=s;
      if p.FileName <> '' then
      begin
        try
          if FileExists(p.FileName) or IsStream(Pc) or IsCD(Pc) then
          begin
            lack := 0;
            //                  p.Stream:=GetStreamInfoSimple(p.FileName);
            //                  p.Tag:=GetFromInfo(p.Stream, lack);
            if lack > 2 then
            begin
              //                    p.Tag.IsTag:=false;
            end;
            Pls.Add(p);
          end;
        except
          Continue;
        end;
      end;
    end;

    Readln(f, s);
  end;

  CloseFile(f);

  SetCurrentDir(Dir);
end;

procedure TXMLPlaylist.SavePls(Pls: TPlayList; FileName: string; Relative: boolean);
begin
  //if Pls.Count=0 then Exit;
  //ShowMessage(ExtractFileExt(FileName));
  if UpperCase(ExtractFileExt(FileName)) = '.KPL' then
    SaveKPLPls(Pls, FileName, Relative)
  else if UpperCase(ExtractFileExt(FileName)) = '.M3U' then
    SaveM3UPls(Pls, FileName, Relative)
  else
    SavePLSPls(Pls, FileName, Relative);
end;

procedure TXMLPlaylist.LoadKPLPls(FileName: string; var Pls: TPlayList);
var
  i:      integer;
  XMLPls: TSpkXMLParser;
  Node, Main: TSpkXMLNode;
  Entry:  TSpkXMLParameter;
  P:      TPLEntry;
  Dir:    string;
  Pc:     TPathChar;

  function ReadTag(node: TSpkXMLNode): TID3Tag;
  begin
    //In KPL v2 there are metatags, tags are read from KPL so there is no
    //need to reread them from files. In many times.
    //For now IsTag is set to make sure that tag will be reread
    Result.IsTag := True;
  end;

begin
  if not FileExists(FileName) then
    begin
      hLog.Send('File does not exist');
      Exit;
    end;
  Dir := GetCurrentDir;
  SetCurrentDir(ExtractFilePath(FileName));

  XMLPls := TSpkXMLParser.Create;
  XMLPls.LoadFromFile(FileName);

  Main := XMLPls.NodeByName['xml', True];
  if Main.Count = 0 then
  begin
    XMLPls.Free;
    SetCurrentDir(Dir);
    Exit;
  end;

  for i := 0 to Main.Count - 1 do
  begin
    Node := Main.SubNodeByName[IntToStr(i), False];
    if Node <> nil then
    begin
      Entry := Node.Parameters.ParamByName['filename', False];
      if Entry = nil then
        p.FileName := ''
      else
      begin
        if Length(Entry.Value) > MAX_PATH then
          Continue;
        StrPCopy(Pc, Entry.Value);
        if (IsStream(Pc)) or (IsCD(Pc)) then
          p.FileName := Entry.Value
        else
          p.FileName := ExpandFileName(Entry.Value);
        p.Tag := ReadTag(Node.SubNodeByName['tag', False]);
        if (p.Tag.IsTag) and (not (IsStream(pc) or IsCD(Pc))) then
          p.Tag := ReadID3(p.FileName);
        Pls.Add(p);
      end;
    end;
  end;

  XMLPls.Free;
  SetCurrentDir(Dir);
end;

procedure TXMLPlaylist.LoadPlsXSPF(FileName: string; var Pls: TPlayList);
var
  XMLPls: TXMLDocument;
  Node, Main: TDOMNode;
  i:     integer;
  fname: string;
  Pc:    TPathChar;
  P:     TPLEntry;

  function FindNode(x: TDOMNode; Name: string): TDOMNode;
  var
    i: integer;
  begin
    Result := nil;
    for i := 0 to x.ChildNodes.Count - 1 do
    begin
      if UpperCase(x.ChildNodes.Item[i].NodeName) = UpperCase(Name) then
      begin
        hLog.Send('Node found: ' + Name);
        Result := x.ChildNodes.Item[i];
      end;
      hLog.Send(x.ChildNodes.Item[i].NodeName);
    end;
    if Result = nil then
      hLog.Send('Node not found: ' + Name);
  end;

begin
  ReadXMLFile(XMLPls, FileName);

  hLog.Send('Loading XSPF playlist');
  hLog.Send(XMLPls.NodeName + ' ' + IntToStr(XMLPls.DocumentElement.ChildNodes.Count));

  hLog.Send(XMLPls.DocumentElement.ChildNodes.Item[2].FirstChild.TextContent);

  Main := FindNode(XMLPls.DocumentElement, 'tracklist');
  hLog.Send(Main.FirstChild.NodeValue);

  hLog.Send('Number of items in playlist: ' + IntToStr(Main.ChildNodes.Count));

  for i := 0 to Main.ChildNodes.Count - 1 do
  begin
    //Node:=FindNode(Main.ChildNodes.Item[i].FirstChild, 'track');
    Node := FindNode(Main.ChildNodes.Item[i], 'location');
    hLog.Send(Node.FirstChild.NodeValue);
    fname := Node.FirstChild.NodeValue;
    if Length(fname) > MAX_PATH then
      Continue;
    //ShowMessage(fname);
    hLog.Send(fname);
    StrPCopy(Pc, fname);
    if (IsStream(Pc) or IsCD(Pc)) then
      p.FileName := fname
    else
      p.FileName := ExpandFileName(fname);
    if not (IsStream(pc) or IsCD(Pc)) then
      p.Tag := ReadID3(p.FileName);
    Pls.Add(p);

  end;

  XMLPls.Free;
end;

procedure TXMLPlaylist.LoadPls(FileName: string; var Pls: TPlayList);
begin
  //if Pls.Count=0 then Exit;
  //ShowMessage(ExtractFileExt(FileName));
  FixFolderNames(FileName);
  hLog.Send('Loading playlist from file: ' + FileName);
  if UpperCase(ExtractFileExt(FileName)) = '.KPL' then begin
    hLog.Send('KPL Playlist');
    LoadKPLPls(FileName, Pls);
  end
  else if UpperCase(ExtractFileExt(FileName)) = '.M3U' then
    LoadM3UPls(FileName, Pls)
  else if UpperCase(ExtractFileExt(FileName)) = '.XSPF' then
    LoadPlsXSPF(FileName, Pls)
  else
    LoadPLSPls(FileName, Pls);
end;

end.

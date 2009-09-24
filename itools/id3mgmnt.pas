unit ID3Mgmnt;

{This unit includes all routines to manage ID3 tags}

interface

uses Classes, SysUtils, FileSupport, ID3v2, Dialogs;

type
{ID3Tag is the current structure for getting tags from MP3 and MP3-like files}
TID3Tag = packed record
   IsTag: boolean;          // 128 bytes
   Title: ShortString;    //  30 bytes: Song's title
   Artist: ShortString;   //  30 bytes: Song's artist
   Album: ShortString;    //  30 bytes: Song's album
   Year: ShortString;      //   4 bytes: Publishing year
   Comment: ShortString;  //  30 bytes: Comment
   Genre: ShortString;
   Track: integer;
   GID: integer;                     //   1 byte:  Genere-ID
end;

type KSPMetaInt = 0..2;
  TPLEntry = record
    Tag: TID3Tag;
    Stream: TStreamInfo;
    FileName: string;
    Played: boolean;
//    InternalNumberName: Integer;
    FirstPlay: TDateTime;
    LastPlay: TDateTime;
    Fav: Double;
    PlayCount: Cardinal;
    MetaTag: KSPMetaInt;//0 - disabled, 1 - refresh, 2 - constant
    PlayedEver: boolean;
    IM: Integer;
  end;

function ReadID3(FileName: WideString; var Tag: boolean; var NotFound: integer): TID3Tag; overload;
function ReadID32(FileName: WideString; var Tag: boolean; var NotFound: integer): TID3Tag;
function ReadID3(FileName: WideString): TID3Tag; overload;
//procedure WriteID3(FileName: string; FCD: TAccessDBEntry; TrackNo: Integer);
function GetFromInfo(S: TStreamInfo; var notFound: integer): TID3Tag;
function GetStreamInfoSimple(FileName: string; var GetIsTag: boolean): TStreamInfo;
procedure FillByFolder(var p: TPLEntry);

implementation

uses BassPlayer;

{Read ID3 tag from file}

function ReadID3(FileName: WideString): TID3Tag;
var
  t: boolean;
  i: integer;
begin
  Result:=ReadID3(FileName, t, i);
end;

procedure FillByFolder(var p: TPLEntry);
var
  i: integer;
  s: string;
  Found: boolean;
  s2: string;

  function IsInteger(s: string): boolean;
  var
    i: integer;
    LetterFound: boolean;
  begin
    Result:=false;

    if s<>'' then
      for i:=1 to Length(s) do begin
          case s[i] of
            '1', '2', '3', '4', '5', '6', '7', '8', '9', '0': LetterFound:=false;
          else
            LetterFound:=true;
          end;

        Result:=Result or LetterFound;
        end;

    Result:=not Result;
    {try
      StrToInt(s);
    except
      Result:=false;
    end;  }
  end;

  procedure FindNext;
  begin
    i:=Pos('-', s);
    if i=0 then begin
        s2:=s;
        Found:=false;
        Exit;
      end;
    s2:=Copy(s, 1, i-1);
    Delete(s, 1, i);
    while s2[Length(s2)]=' ' do
      Delete(s2, Length(s2), 1);
    while s[1]=' ' do
      Delete(s, 1, 1);
    Found:=true;
  end;

  procedure TreatAsSimple;
  begin
    p.Tag.Track:=StrToInt(s2);
    if Found then begin
      FindNext;
      p.Tag.Artist:=s2;
      if Found then begin
        i:=Pos('.', s);
        s2:=Copy(s, 1, i-1);
        p.Tag.Title:=s2;
      end;
    end;
    s:=ExtractFilePath(p.FileName);
    if s[Length(s)]='\' then
      Delete(s, Length(s), 1);
    s:=ExtractFileName(s);
    FindNext;
    if Found then
      begin
        if p.Tag.Artist<>'' then
          p.Tag.Artist:=s2;
        p.Tag.Album:=s;
      end else p.Tag.Album:=s;
  end;

  procedure TreatAsTree;
  var
   s3: string;
  begin
    i:=Pos('.', s);
    s2:=Copy(s, 1, i-1);
    p.Tag.Title:=s2;
    s:=ExtractFilePath(p.FileName);
    if s[Length(s)]='\' then
      Delete(s, Length(s), 1);
    s3:=s;
    s:=ExtractFileName(s);
    FindNext;
    if Found then
      begin
        if p.Tag.Artist<>'' then
          p.Tag.Artist:=s2;
        p.Tag.Album:=s;
      end else begin
        p.Tag.Album:=s;
        s:=ExtractFilePath(s3);
        if s[Length(s)]='\' then
          Delete(s, Length(s), 1);
        p.Tag.Artist:=ExtractFileName(s);
      end;
  end;

begin
//Check if filename is not chosen by artist-track-title-album or
//track-title-album-artist
  Found:=false;
  p.Tag.Artist:='';
  try
  s:=ExtractFileName(p.FileName);

  FindNext;
  if IsInteger(s2) then
    TreatAsSimple else
    TreatAsTree;
  except
  end;
end;

function GetFromInfo(S: TStreamInfo; var notFound: integer): TID3Tag;
begin
      Result.Title:=S.Title;
      Result.Album:=S.Album;
      Result.Artist:=S.Artist;
      Result.Year:=S.Year;
      Result.Comment:=S.Comment;
      Result.Genre:=S.Genre;
      Result.Track:=S.Track;
      Result.GID:=S.GenreID;
      NotFound:=0;
      if Result.Album='' then Inc(NotFound);
      if Result.Artist='' then Inc(NotFound);
      if Result.Title='' then Inc(NotFound);
end;

function GetStreamInfoSimple(FileName: string; var GetIsTag: boolean): TStreamInfo;
var
  Sup: TSupportedBy;
begin
  GetIsTag:=GetStreamInfo2(FileName, Result, Sup);
end;

function ReadID3(FileName: WideString; var Tag: boolean; var NotFound: integer): TID3Tag;
var
  id3tag: Tid3tag;
  Sup: TSupportedBy;
  StreamInfo2 : TStreamInfo;
begin
  NotFound:=0;

  id3tag.Album:='';
  id3tag.Artist:='';
  id3tag.Comment:='';
  id3tag.Genre:='';
  id3tag.GID:=0;
  id3tag.Title:='';
  id3tag.Track:=0;
  id3tag.Year:='';


  tag:=GetStreamInfo2(FileName, StreamInfo2, Sup);
  id3tag.IsTag:=tag;
  
  if id3tag.Album='' then Inc(NotFound);
  if id3tag.Artist='' then Inc(NotFound);
  if id3tag.Title='' then Inc(NotFound);

  if tag then begin
      id3tag.Title:=StreamInfo2.Title;
      id3tag.Album:=StreamInfo2.Album;
      id3tag.Artist:=StreamInfo2.Artist;
      id3tag.Year:=StreamInfo2.Year;
      id3tag.Comment:=StreamInfo2.Comment;
      id3tag.Genre:=StreamInfo2.Genre;
      id3tag.Track:=StreamInfo2.Track;
      id3tag.GID:=StreamInfo2.GenreID;
    end;

  Result:=id3tag;
end;

function ReadID32(FileName: WideString; var Tag: boolean; var NotFound: integer): TID3Tag;
var
  id3tag: Tid3tag;
  Sup: TSupportedBy;
  StreamInfo2 : TStreamInfo;
begin
  NotFound:=0;

  tag:=GetStreamInfo2(FileName, StreamInfo2, Sup);
  id3tag.IsTag:=tag;
  
  if id3tag.Album='' then Inc(NotFound);
  if id3tag.Artist='' then Inc(NotFound);
  if id3tag.Title='' then Inc(NotFound);

  if tag then begin
      id3tag.Title:=StreamInfo2.Title;
      id3tag.Album:=StreamInfo2.Album;
      id3tag.Artist:=StreamInfo2.Artist;
      id3tag.Year:=StreamInfo2.Year;
      id3tag.Comment:=StreamInfo2.Comment;
      id3tag.Genre:=StreamInfo2.Genre;
      if StreamInfo2.Track<128 then
        id3tag.Track:=StreamInfo2.Track else
        id3tag.Track:=0;
      id3tag.GID:=StreamInfo2.GenreID;
    end;

  Result:=id3tag;
end;

end.

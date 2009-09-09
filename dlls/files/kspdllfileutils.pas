unit KSPDLLFileUtils;

{$MODE Delphi}

interface

uses SysUtils, Graphics, ID3Mgmnt;

const
  art='[%artist]';
  album='[%album]';
  title = '[%title]';
  genre = '[%genre]';
  year = '[%year]';
  comment = '[%comment]';
  track = '[%track]';
  tracklength = '[%length]';
  plindex = '[%plindex]';

function MinimizeName(const Filename: PChar; Canvas: TCanvas;
  MaxLen: Integer): TFileName;
procedure RemoveForbiddenChars(var Str: String; ReplaceWith: Char);
function ProduceFormatedString(Input: ShortString; Tag: TID3Tag; LengthVal: Cardinal;
  PlsIndex: integer): ShortString;
function IsCD(str: string): boolean;
function IsStream(str: string): boolean;

implementation

procedure CutFirstDirectory(var S: TFileName);
var
  Root: Boolean;
  P: Integer;
begin
  if S = '\' then
    S := ''
  else
  begin
    if S[1] = '\' then
    begin
      Root := True;
      Delete(S, 1, 1);
    end
    else
      Root := False;
    if S[1] = '.' then
      Delete(S, 1, 4);
    P := AnsiPos('\',S);
    if P <> 0 then
    begin
      Delete(S, 1, P);
      S := '...\' + S;
    end
    else
      S := '';
    if Root then
      S := '\' + S;
  end;
end;

function MinimizeName(const Filename: PChar; Canvas: TCanvas;
  MaxLen: Integer): TFileName;
var
  Drive: TFileName;
  Dir: TFileName;
  Name: TFileName;
begin
  Result := FileName;
  Dir := ExtractFilePath(Result);
  Name := ExtractFileName(Result);

  if (Length(Dir) >= 2) and (Dir[2] = ':') then
  begin
    Drive := Copy(Dir, 1, 2);
    Delete(Dir, 1, 2);
  end
  else
    Drive := '';
  while ((Dir <> '') or (Drive <> '')) and (Canvas.TextWidth(Result) > MaxLen) do
  begin
    if Dir = '\...\' then
    begin
      Drive := '';
      Dir := '...\';
    end
    else if Dir = '' then
      Drive := ''
    else
      CutFirstDirectory(Dir);
    Result := Drive + Dir + Name;
  end;
end;

function IsStream(str: string): boolean;
var
  NameHeader1, NameHeader2: string;
begin
  NameHeader1 := copy(str, 1, 7);
  NameHeader2 := copy(str, 1, 6);
  Result:=(NameHeader1 = 'http://') or (NameHeader2 = 'ftp://') or
      (NameHeader2 = 'mms://');
end;

function IsCD(str: string): boolean;
begin
    Result:=(Pos('CDA://', UpperCase(str))=1);
end;

function ProduceFormatedString(Input: ShortString; Tag: TID3Tag; LengthVal: Cardinal;
  PlsIndex: integer): ShortString;
var
  s: ShortString;
  i: integer;
begin
  s:=Input;

  i:=pos(art, s);
  if i>0 then begin
      Delete(s, i, Length(art));
      Insert(Tag.Artist, s, i);
    end;

  i:=pos(album, s);
  if i>0 then begin
      Delete(s, i, Length(album));
      Insert(Tag.Album, s, i);
    end;

  i:=pos(title, s);
  if i>0 then begin
      Delete(s, i, Length(title));
      Insert(Tag.Title, s, i);
    end;

  i:=pos(genre, s);
  if i>0 then begin
      Delete(s, i, Length(genre));
      Insert(Tag.Genre, s, i);
    end;

  i:=pos(year, s);
  if i>0 then begin
      Delete(s, i, Length(year));
      Insert(Tag.Year, s, i);
    end;

  i:=pos(track, s);
  if i>0 then begin
      Delete(s, i, Length(track));
      Insert(IntToStr(Tag.Track), s, i);
    end;

  i:=pos(comment, s);
  if i>0 then begin
      Delete(s, i, Length(comment));
      Insert(Tag.Comment, s, i);
    end;

  i:=pos(plindex, s);
  if i>0 then begin
      Delete(s, i, Length(plindex));
      Insert(IntToStr(PlsIndex), s, i);
    end;

  i:=pos(tracklength, s);
  if i>0 then begin
      Delete(s, i, Length(tracklength));
      if LengthVal > (1000 * 60 * 60) then
        Insert(FormatDateTime ('hh:nn:ss', LengthVal / (1000 * 24 * 60 * 60)), s, i)
      else
        Insert(FormatDateTime ('nn:ss', LengthVal / (1000 * 24 * 60 * 60)), s, i);
    end;

  Result:=s;
end;

procedure RemoveForbiddenChars(var Str: String; ReplaceWith: Char);
var
  i: integer;
begin
  if Str='' then Exit;

  for i:=1 to Length(Str) do
    case Str[i] of
      '\', '/', ':', '*', '?', '"', '<', '>', '|':
        str[i]:=ReplaceWith;
    end;
end;

end.

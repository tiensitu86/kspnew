library kspfiles;

{$MODE Delphi}

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

//{$Include FastMM4Options.inc}

{%TogetherDiagram 'ModelSupport_kspfiles\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_kspfiles\FileSupportLst\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_kspfiles\kspfiles\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_kspfiles\KSPMessages\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_kspfiles\AdditFiles\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_kspfiles\FileUtils\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_kspfiles\default.txvpck'}

uses
//  LCLIntf,
//  Windows,
  SysUtils,
//  Classes,
//  Dialogs,
//  DateUtils,
//  FileSupportLst in '..\..\itools\filesupportlst.pas',
  FileUtils in 'FileUtils.pas',
  KSPMessages in '..\..\itools\kspmessages.pas'{,
  AdditFiles in 'C:\medialib\AdditFiles.pas'};

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
   GID: integer;                  //   1 byte:  Genere-ID
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

procedure WriteChangeFile(P: TFileRenamed);
var
  f: file of TFileRenamed;
begin
  AssignFile(f, ExtractFilePath(ParamStr(0))+'changes.fil');
  if FileExists(ExtractFilePath(ParamStr(0))+'changes.fil') then
    DeleteFile(ExtractFilePath(ParamStr(0))+'changes.fil');
  Rewrite(f);
  Write(f, p);
  CloseFile(f);
end;

function ReadChangeFile: TFileRenamed;
var
  f: file of TFileRenamed;
begin
  AssignFile(f, ExtractFilePath(ParamStr(0))+'changes.fil');
  if not FileExists(ExtractFilePath(ParamStr(0))+'changes.fil') then Exit;

  Reset(f);
  Read(f, Result);
  CloseFile(f);

  DeleteFile(ExtractFilePath(ParamStr(0))+'changes.fil');
end;

exports
  //SearchFiles,
  MinimizeName,
  FileSetAttr,
  TrimRight,
  TrimRightA,
  RemoveForbiddenChars,
  ProduceFormatedString,
//  GetFav2,
//  GetFav,
  IsStream,
  IsCD,
//  GetFileVersion,
//  GetFileVersion2,
  //PrepareString,
  ReadChangeFile,
  WriteChangeFile;

begin
end.

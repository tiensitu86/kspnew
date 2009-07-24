library kspfiles;

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
  Windows,
  SysUtils,
  Classes,
  Dialogs,
  DateUtils,
  FileSupportLst in '..\..\itools\FileSupportLst.pas',
  FileUtils in 'FileUtils.pas',
  KSPMessages in '..\..\itools\KSPMessages.pas',
  AdditFiles in 'C:\medialib\AdditFiles.pas';

{$R *.res}

const
  faReadOnly  = $00000001;
  faHidden    = $00000002;
  faSysFile   = $00000004;
  faVolumeID  = $00000008;
  faDirectory = $00000010;
  faArchive   = $00000020;
  faSymLink   = $00000040;
  faAnyFile   = $0000003F;

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
begin
    Result:=(Pos('HTTP://', UpperCase(str))=1);
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

procedure SearchFiles(var FilesNo: integer; Path: PChar; Rec: boolean; var s: TStringList; FileSL: TFileSupportList);
var
  sr: TSearchRec;
  FileAttrs: Integer;
  s2: TStringList;
  i: integer;
  P: TPathChar;
begin
  FileAttrs := faAnyFile;//+faDirectory;
  s2:=TStringList.Create;

    if FindFirst(Path+'\*.*', FileAttrs, sr) = 0 then

    begin
      repeat
        //if (sr.Attr and FileAttrs) = sr.Attr then
        begin
        if (sr.Name<>'') and (sr.Name<>'.') and (sr.Name<>'..') then begin
            //ShowMessage(ExtractFileExt(sr.Name));
            Inc(FilesNo);
            if (FileSL.FindExtension(ExtractFileExt(sr.Name), false)>-1)
              and ((sr.Attr and faDirectory) <> sr.Attr) then s.Add(String(Path)+'\'+sr.Name);
            if Rec and ((sr.Attr and faDirectory) = sr.Attr) then
              s2.Add(Path+'\'+sr.Name);
          end;

        end;
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;

  if s2.Count> 0 then
    for i:=0 to s2.Count-1 do begin
      StrPCopy(P, s2.Strings[i]);
      SearchFiles(FilesNo, p, Rec, s, FileSL);
    end;

  s2.Free;

end;

procedure GetFileVersionEx(FileName: PChar; var Major1, Major2, Minor1,
Minor2: Integer );
{ Helper function to get the actual file version information }
var
  Info: Pointer;
  InfoSize: DWORD;
  FileInfo: PVSFixedFileInfo;
  FileInfoSize: DWORD;
  Tmp: DWORD;
begin
  // Get the size of the FileVersionInformatioin
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Tmp);
  // If InfoSize = 0, then the file may not exist, or
  // it may not have file version information in it.
  if InfoSize = 0 then
  begin
    Major1 := 0;
    Major2 := 0;
    Minor1 := 0;
    Minor2 := 0;
  end else
  begin
    // Allocate memory for the file version information
    GetMem(Info, InfoSize);
    try      // Get the information
      GetFileVersionInfo(PChar(FileName), 0, InfoSize, Info);
      // Query the information for the version
      VerQueryValue(Info, '\', Pointer(FileInfo), FileInfoSize);
      // Now fill in the version information
      Major1 := FileInfo.dwFileVersionMS shr 16;
      Major2 := FileInfo.dwFileVersionMS and $FFFF;
      Minor1 := FileInfo.dwFileVersionLS shr 16;
      Minor2 := FileInfo.dwFileVersionLS and $FFFF;
    finally
      FreeMem(Info, FileInfoSize);
    end;
  end;
end;

function GetFileVersion(Filename: TPathChar): ShortString;
var
  M1, M2, M3, M4: Integer;

begin
  GetFileVersionEx( FileName, M1, M2, M3, M4 );
  result := InttoStr(M1) + '.' + InttoStr(M2) + '.' + InttoStr(M3) + ' (Build ' + InttoStr(M4) + ')';
end;

function GetFileVersion2(Filename: TPathChar ): ShortString;
var
  M1, M2, M3, M4: Integer;

begin
  GetFileVersionEx( FileName, M1, M2, M3, M4 );
  result := InttoStr(M1) + '.' + InttoStr(M2) + '.' + InttoStr(M3);
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
  SearchFiles,
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
  GetFileVersion,
  GetFileVersion2,
  //PrepareString,
  ReadChangeFile,
  WriteChangeFile;

begin
end.

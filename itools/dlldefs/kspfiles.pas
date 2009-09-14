unit kspfiles;

interface

uses LResources, Forms, ID3Mgmnt, Classes, FileSupportLst, KSPMessages, IdHTTP, DateUtils, Dialogs,
  KSPDLLFileUtils, FileUtil;

const
{$IFDEF WINDOWS}
  LIB_SUFFIX = '.dll';
{$ELSE}
  LIB_SUFFIX = '.so';
{$ENDIF}

{$IFNDEF WINDOWS}
  {$DEFINE KSP_SPECIAL_BUILD}
{$ENDIF}

{$IFDEF KSP_SPECIAL_BUILD}
const KSPSpecialInfo = 'R3 alpha 2';
{$ELSE}
const KSPSpecialInfo = 'R2.2';
{$ENDIF}
const KSPMajorVersion = '2009';

const Version = 0;
  Major = 2;
  Minor = 100;
  Build = 136;

procedure RemoveForbiddenChars(var Str: String; ReplaceWith: Char);
function ProduceFormatedString(Input: ShortString; Tag: TID3Tag; LengthVal: Cardinal;
  PlsIndex: integer): ShortString;
function IsCD(str: string): boolean;
function IsStream(str: string): boolean;

function GetKSPVersion: ShortString;
function GetKSPVersion2: ShortString;
procedure GetKSPVersion3(var kversion, kmajor, kminor, kbuild: string);

//FileUtils

function PrepareString(str: string): string;// external 'kspfiles.dll';

function DownloadURLi(const aUrl: string; var Output: TStringList): Boolean;
procedure SearchForFilesFS(Path: string; Rec: boolean; var s: TStringList); overload;
procedure SearchForFiles(Path: string; Rec: boolean; var s: TStringList; DateM: TDateTime); overload;
procedure KSPDeleteFolder(Path: string);
procedure ListFolders(Path: string; s: TStringList; OlderThan: integer);

function IsPlaylist(FileName: string): boolean;
function KSPGetFileSize(const FileName: WideString): Int64;

procedure KSPShowMessage(msg: string);



implementation

uses SysUtils, main, multilog;

procedure RemoveForbiddenChars(var Str: String; ReplaceWith: Char);
begin
  KSPDLLFileUtils.RemoveForbiddenChars(Str, ReplaceWith);
end;

function ProduceFormatedString(Input: ShortString; Tag: TID3Tag; LengthVal: Cardinal;
  PlsIndex: integer): ShortString;
begin
  Result:=KSPDLLFileUtils.ProduceFormatedString(Input, Tag, LengthVal, PlsIndex);
end;

function IsCD(str: string): boolean;
begin
  Result:=KSPDLLFileUtils.IsCD(str);
end;

function IsStream(str: string): boolean;
begin
 Result:=KSPDLLFileUtils.IsStream(str);
end;

procedure KSPDeleteFolder(Path: string);
var
  sr: TSearchRec;
  FileAttrs: Integer;
begin
{$IFDEF WINDOWS}
    FileAttrs:=faAnyFile;
    if FindFirst(Path+'\*.*', FileAttrs, sr) = 0 then
{$ELSE}
    FileAttrs := faReadOnly+faHidden+faSysFile+faVolumeId+faDirectory+faArchive+
    faSymLink+faAnyFile;//+faDirectory;
    if FindFirst(Path+'/*', FileAttrs, sr) = 0 then
{$ENDIF}
  begin
    repeat
      if (sr.Name<>'') and (sr.Name<>'.') and (sr.Name<>'..') then begin
        if (sr.Attr and faDirectory) = sr.Attr then
          KSPDeleteFolder(Path+'\'+sr.Name) else
        if FileExists(Path+'\'+sr.Name) then DeleteFile(Path+'\'+sr.Name);
      end;
    until FindNext(sr) <> 0;
      FindClose(sr);
  end;

  if DirectoryExists(Path) then
    try
      RmDir(Path)
    except
      hLog.Send('Unable to delete folder: '+Path);
    end;


end;

procedure ListFolders(Path: string; s: TStringList; OlderThan: integer);
var
  sr: TSearchRec;
  FileAttrs: Integer;
begin
  FileAttrs := faDirectory;
  if FindFirst(Path+'\*.*', FileAttrs, sr) = 0 then
  begin
    repeat
      if (sr.Name<>'') and (sr.Name<>'.') and (sr.Name<>'..') then begin
        if ((sr.Attr and faDirectory) = sr.Attr) and (DaysBetween(Now, FileDateToDateTime(sr.Time))>OlderThan) then
          s.Add(Path+'\'+sr.Name);
      end;
    until FindNext(sr) <> 0;
      FindClose(sr);
  end;
end;

function IsPlaylist(FileName: string): boolean;
begin
  Result:=(UpperCase(ExtractFileExt(FileName))='.KPL') or
    (UpperCase(ExtractFileExt(FileName))='.M3U') or
    (UpperCase(ExtractFileExt(FileName))='.XSPF') or
    (UpperCase(ExtractFileExt(FileName))='.PLS');
end;

procedure SearchForFiles(Path: string; Rec: boolean; var s: TStringList; DateM: TDateTime); overload;
var
  sr: TSearchRec;
  FileAttrs: Integer;
  s2: TStringList;
  i: integer;
begin
  s2:=TStringList.Create;

{$IFDEF WINDOWS}
    FileAttrs:=faAnyFile;
    if FindFirst(Path+'\*.*', FileAttrs, sr) = 0 then
{$ELSE}
    FileAttrs := faReadOnly+faHidden+faSysFile+faVolumeId+faDirectory+faArchive+
    faSymLink+faAnyFile;//+faDirectory;
    if FindFirst(Path+'/*', FileAttrs, sr) = 0 then
{$ENDIF}

    begin
      repeat
        //if (sr.Attr and FileAttrs) = sr.Attr then
        begin
        if (sr.Name<>'') and (sr.Name<>'.') and (sr.Name<>'..') then begin
            //ShowMessage(ExtractFileExt(sr.Name));

{$IFDEF WINDOWS}
            if ((sr.Attr and faDirectory) <> sr.Attr)and
              (FileDateToDateTime(sr.Time)>DateM) then
                s.Add(Path+'\'+sr.Name);
{$ELSE}
            if not DirectoryExists(Path+'/'+sr.Name) then begin
              if FileDateToDateTime(sr.Time)>DateM then
                s.Add(Path+'/'+sr.Name) end else
              s2.Add(Path+'/'+sr.Name);
{$ENDIF}

            if Rec and ((sr.Attr and faDirectory) = sr.Attr) then
{$IFDEF WINDOWS}
                s2.Add(Path+'\'+sr.Name);
{$ELSE}
                s2.Add(Path+'/'+sr.Name);
{$ENDIF}
          end;

        end;
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;

  if s2.Count> 0 then
    for i:=0 to s2.Count-1 do
      SearchForFiles(s2.Strings[i], Rec, s, DateM);

  s2.Free;

end;

procedure SearchForFilesFS(Path: string; Rec: boolean; var s: TStringList); overload;
var
  sr: TSearchRec;
  FileAttrs: Integer;
  s2: TStringList;
  i: integer;
begin
  s2:=TStringList.Create;
{$IFDEF WINDOWS}
    FileAttrs:=faAnyFile;
    if FindFirst(Path+'\*.*', FileAttrs, sr) = 0 then
{$ELSE}
    FileAttrs := faReadOnly+faHidden+faSysFile+faVolumeId+faDirectory+faArchive+
    faSymLink+faAnyFile;//+faDirectory;
    if FindFirst(Path+'/*', FileAttrs, sr) = 0 then
{$ENDIF}

    begin
      repeat
        //if (sr.Attr and FileAttrs) = sr.Attr then
        begin
         if (sr.Name<>'') and (sr.Name<>'.') and (sr.Name<>'..') then begin
            //ShowMessage(ExtractFileExt(sr.Name));

{$IFDEF WINDOWS}
            if ((sr.Attr and faDirectory) <> sr.Attr) then
                s.Add(Path+'\'+sr.Name);
{$ELSE}
            if not DirectoryExists(Path+'/'+sr.Name) then
                s.Add(Path+'/'+sr.Name) else
                s2.Add(Path+'/'+sr.Name);
{$ENDIF}

            if Rec and ((sr.Attr and faDirectory) = sr.Attr) then
{$IFDEF WINDOWS}
              s2.Add(Path+'\'+sr.Name);
{$ELSE}
              s2.Add(Path+'/'+sr.Name);
{$ENDIF}
          end;

        end;
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;

  if s2.Count> 0 then
    for i:=0 to s2.Count-1 do
      SearchForFilesFS(s2.Strings[i], Rec, s);

  s2.Free;

end;

function PrepareString(str: string): string;
var
  i: integer;
begin
  Result:='';
  if str='' then Exit;
  Result:=str;

  for i:=Length(Result) downto 1 do
    if Result[i]='''' then System.Insert('''', Result, i);

end;


function DownloadURLi(const aUrl: string; var Output: TStringList): Boolean;
var
  HTTP: TIdHTTP;
begin
  HTTP:=TIdHTTP.Create;
  Output.Text:=Http.Get(aUrl);
  HTTP.Free;
  Result:=Output.Text<>'';
end;


function GetKSPVersion: ShortString;
begin
  Result := KSPMajorVersion;
  Result:=Result+' '+KSPSpecialInfo;
end;

function GetKSPVersion2: ShortString;
begin
  Result:=IntToStr(Version)+'.'+
    IntToStr(Major)+'.'+
    IntToStr(Minor)+'.'+
    IntToStr(Build);//GetKSPVersion(AppPath);
end;

procedure GetKSPVersion3(var kversion, kmajor, kminor, kbuild: string);
begin
  kversion:=IntToStr(Version);
  kmajor:=IntToStr(Major);
  kminor:=IntToStr(Minor);
  kbuild:=IntToStr(Build);
end;

function KSPGetFileSize(const FileName: WideString): Int64;
var
  SourceFile: TFileStream;
begin
  { Get info from file }
  Result := 0;
  SourceFile := nil;
  try
    SourceFile := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    Result := SourceFile.Size;
  finally
    SourceFile.Free;
  end;
end;

procedure KSPShowMessage(msg: string);
var
  pc: pChar;
begin
  pc:=pChar(msg);
//  p:=pc;
  if KSPMainWindow<>nil then try
    Application.QueueAsyncCall(KSPMainWindow.KSPShowMessage, DWORD(pc));
  except
    hLog.Send('KSP MAIN WINDOWS MESSAGE: '+msg)
  end;
end;

end.

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

unit kspfiles;

interface

uses {$IFDEF WINDOWS}Windows, {$ENDIF}LResources, Forms, ID3Mgmnt, Classes,
  httpsend, DateUtils, Dialogs, KSPDLLFileUtils, FileUtil, Process;

const
{$IFDEF WINDOWS}
  LIB_SUFFIX = '.dll';
{$ELSE}
  LIB_SUFFIX = '.so';
{$ENDIF}
  READ_BYTES = 2048;

{$IFNDEF WINDOWS}
  //{$DEFINE KSP_SPECIAL_BUILD}
{$ENDIF}

{$IFDEF KSP_SPECIAL_BUILD}
const KSPSpecialInfo_i = 'R4 beta 1';
{$ELSE}
const KSPSpecialInfo_i = 'R4 beta 1';
{$ENDIF}

{$IFDEF KSP_DEVEL}
const KSPSpecialInfo = KSPSpecialInfo_i+' EXPERIMENTAL';
{$ELSE}
const KSPSpecialInfo = KSPSpecialInfo_i;
{$ENDIF}
const KSPMajorVersion = '2009';

const Version = 0;
  Major = 4;
  Minor = 0;
  Build = 288;

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
function Url_encode(const url:string):string;
procedure SearchForFilesFS(Path: string; Rec: boolean; var s: TStringList); overload;
procedure SearchForFiles(Path: string; Rec: boolean; var s: TStringList; DateM: TDateTime); overload;
procedure KSPDeleteFolder(Path: string);
procedure ListFolders(Path: string; s: TStringList; OlderThan: integer);

function IsPlaylist(FileName: string): boolean;
function KSPGetFileSize(const FileName: WideString): Int64;

procedure KSPShowMessage(msg: string);
procedure KSPShowMessageP(msg: PChar);
procedure KSPSetStatusText(msg: string);
function ExecuteCommand(cmd: string): string;
function GetOSVersion: string;




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
{$IFDEF WINDOWS}
        if (sr.Attr and faDirectory) = sr.Attr then
          KSPDeleteFolder(Path+'\'+sr.Name) else
        if FileExists(Path+'\'+sr.Name) then DeleteFile(Path+'\'+sr.Name);
{$ELSE}
        if DirectoryExists(Path+'/'+sr.Name) then
          KSPDeleteFolder(Path+'/'+sr.Name) else
        if FileExists(Path+'/'+sr.Name) then DeleteFile(Path+'/'+sr.Name);
{$ENDIF}
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
{$IFDEF WINDOWS}
  FileAttrs:=faDirectory;
  if FindFirst(Path+'\*.*', FileAttrs, sr) = 0 then
{$ELSE}
  FileAttrs := faReadOnly+faHidden+faSysFile+faVolumeId+faDirectory+faArchive+
  faSymLink+faAnyFile;//+faDirectory;
  if FindFirst(Path+'/*', FileAttrs, sr) = 0 then
{$ENDIF}
  begin
    repeat
      if (sr.Name<>'') and (sr.Name<>'.') and (sr.Name<>'..') then begin
{$IFDEF WINDOWS}
        if ((sr.Attr and faDirectory) = sr.Attr) and (DaysBetween(Now, FileDateToDateTime(sr.Time))>OlderThan) then begin
          s.Add(Path+'\'+sr.Name);
          hLog.Send('Found folder: '+sr.Name);
        end;
{$ELSE}
        if DirectoryExists(Path+'/'+sr.Name) and (DaysBetween(Now, FileDateToDateTime(FileAge(Path+'/'+sr.Name)))>OlderThan) then begin
          s.Add(Path+'/'+sr.Name);
          hLog.Send('Found folder: '+sr.Name);
        end;
{$ENDIF}
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
begin
  HttpGetText(aUrl, Output);
  Result:=Output.Text<>'';
end;

function Url_encode(const url:string):string;
var
i: integer;
begin
  result:='';
  for i:=1 to length(url) do begin
    case url[i] of
      'a'..'z','A'..'Z','0'..'9','/','.','&','-','=', '?'
(* maybe some more are allowed *)
        : result:=result+ url[i];
      else result:=result+'%'+uppercase(inttohex(ord(url[i]),2));
    end;
  end;
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
begin
  KSPShowMessageP(pChar(msg));
end;

procedure KSPShowMessageP(msg: PChar);
begin
  if KSPMainWindow<>nil then try
    Application.QueueAsyncCall(KSPMainWindow.KSPShowMessage, DWORD(msg));
  except
    hLog.Send('KSP MAIN WINDOW MESSAGE: '+msg)
  end;
end;

procedure KSPSetStatusText(msg: string);
var
  pc: pChar;
begin
  pc:=pChar(msg);
  if KSPMainWindow<>nil then try
    Application.QueueAsyncCall(KSPMainWindow.KSPShowStatusBarText, DWORD(pc));
  except
    hLog.Send('KSP MAIN WINDOW STATUS: '+msg)
  end;
end;

function ExecuteCommand(cmd: string): string;
var
   S: TStringList;
   M: TMemoryStream;
   P: TProcess;
   n: LongInt;
   BytesRead: LongInt;

begin
   // We cannot use poWaitOnExit here since we don't
   // know the size of the output. On Linux the size of the
   // output pipe is 2 kB. If the output data is more, we
   // need to read the data. This isn't possible since we are
   // waiting. So we get a deadlock here.
   //
   // A temp Memorystream is used to buffer the output

   M := TMemoryStream.Create;
   BytesRead := 0;

   P := TProcess.Create(nil);
   P.CommandLine := cmd;
   P.Options := [poUsePipes];
   P.Execute;
   while P.Running do
   begin
     // make sure we have room
     M.SetSize(BytesRead + READ_BYTES);

     // try reading it
     n := P.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
     if n > 0
     then begin
       Inc(BytesRead, n);
       Write('.')
     end
     else begin
       // no data, wait 100 ms
       Sleep(100);
     end;
   end;
   // read last part
   repeat
     // make sure we have room
     M.SetSize(BytesRead + READ_BYTES);
     // try reading it
     n := P.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
     if n > 0
     then begin
       Inc(BytesRead, n);
       Write('.');
     end;
   until n <= 0;
   if BytesRead > 0 then WriteLn;
   M.SetSize(BytesRead);

   S := TStringList.Create;
   S.LoadFromStream(M);
   Result:='';
   for n := 0 to S.Count - 1 do
   begin
     Result:=Result+S[n];
   end;
   S.Free;
   P.Free;
   M.Free;
end;

function GetOSVersion: string;
{$IFDEF WINDOWS}
var
   osVerInfo: TOSVersionInfo;
   majorVersion, minorVersion: Integer;
begin
   Result := '';
   osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo) ;
   if GetVersionEx(osVerInfo) then
   begin
     minorVersion := osVerInfo.dwMinorVersion;
     majorVersion := osVerInfo.dwMajorVersion;
     if osVerInfo.dwPlatformId = VER_PLATFORM_WIN32_NT then
      Result:='Windows NT' else Result:='Windows';
     Result:=Result+' '+IntToStr(majorVersion)+'.'+IntToStr(minorVersion);
   end;
end;
{$ELSE}
begin
  Result:='Linux '+ExecuteCommand('uname -m');
end;
{$ENDIF}

end.

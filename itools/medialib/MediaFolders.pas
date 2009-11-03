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

unit MediaFolders;

interface

uses Classes, SysUtils, Dialogs, ID3Mgmnt, profilefunc;

type
  TMediaFolder = record
    Folder: string;
    Name:   string;
    Description: string;
    LastScanned: TDateTime;
    ScannedEver: boolean;
  end;

  TMediaFolderItem = class(TObject)
  public
    Entry: TMediaFolder;
  end;

  TMediaFoldersList = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Entry: TMediaFolder): boolean;
    procedure Remove(Index: integer);
    function GetItem(Index: integer): TMediaFolder;
    procedure ReplaceEntry(Index: integer; new: TMediaFolder);
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    function FileInFolders(FileName: string): boolean;
  end;

implementation

uses IniFiles, KSPConstsVars, multilog;

constructor TMediaFoldersList.Create;
begin
  inherited Create;
end;

{The Items should be freed here but it isn't. Doesn't matter.
TPlayList is created only once and destroyed only while KSP is
to be closed}

destructor TMediaFoldersList.Destroy;
var
  i: integer;
begin
  for I := 0 to Count - 1 do
    TMediaFolderItem(Items[I]).Free;
  inherited Destroy;
end;

function TMediaFoldersList.Add(Entry: TMediaFolder): boolean;
var
  T: TMediaFolderItem;

  function CheckIfExists: boolean;
  var
    i: integer;
  begin
    Result := False;
    if Self.Count > 0 then
      for i := 0 to Self.Count - 1 do
        if TMediaFolderItem(Items[i]).Entry.Folder = Entry.Folder then
          Result := True;
  end;

begin
  Result := not CheckIfExists;

  if Result then
  begin
    T := TMediaFolderItem.Create;
    T.Entry := Entry;
    inherited Add(T);
  end;
end;

function TMediaFoldersList.FileInFolders(FileName: string): boolean;
var
  i: integer;
begin
  FixFileNameDB2(FileName);
  Result := False;
  if Count > 0 then
    for i := 0 to Count - 1 do
      if Pos(UpperCase(GetItem(i).Folder), UpperCase(FileName)) > 0 then
      begin
        Result := True;
      end;

  if not Result then
    hLog.Send('MEDIA LIBRARY: File not in library folder: ' + FileName);
end;

procedure TMediaFoldersList.ReplaceEntry(Index: integer; new: TMediaFolder);
begin
  TMediaFolderItem(Items[Index]).Entry := new;
end;

procedure TMediaFoldersList.Remove(Index: integer);
var
  s:     string;
  i:     integer;
  p:     TPLEntry;
  RecCo: integer;
  s2:    TStringList;
begin
  s  := TMediaFolderItem(Items[Index]).Entry.Folder;
  s2 := TStringList.Create;

  AllSongs.OpenQuery('SELECT * FROM meta');
  RecCo := AllSongs.ReturnRecordsCount;
  if RecCo > 0 then
    for i := RecCo - 1 downto 0 do
    begin
      AllSongs.GoToNext;
      p := AllSongs.ReadEntry;
      s2.Add(p.FileName);
    end;

  AllSongs.CloseQuery;

  for i := 0 to s2.Count - 1 do ;
  if Pos(s, s2.Strings[i]) > -1 then
    AllSongs.Remove(s2.Strings[i]);

  s2.Free;
  TMediaFolderItem(Items[Index]).Free;

  Delete(Index);
end;

function TMediaFoldersList.GetItem(Index: integer): TMediaFolder;
begin
  Result := TMediaFolderItem(Items[Index]).Entry;
end;

procedure TMediaFoldersList.SaveToFile(FileName: string);
var
  XMLFile: TIniFile;
  i:  integer;
  mf: TMediaFolder;
begin
  FixFolderNames(FileName);
  if FileExists(FileName) then
    DeleteFile(FileName);
  if Self.Count = 0 then
    Exit;
  XMLFile := TIniFile.Create(FileName);
  //XMLFile.Clear;

  for i := 0 to Self.Count - 1 do
  begin
    mf := TMediaFolderItem(Items[i]).Entry;
    XMLFile.WriteString(IntToStr(i), 'Folder', mf.Folder);
    XMLFile.WriteString(IntToStr(i), 'Desc', mf.Description);
    XMLFile.WriteString(IntToStr(i), 'Name', mf.Name);
    XMLFile.WriteDateTime(IntToStr(i), 'LastScanned', mf.LastScanned);
    XMLFile.WriteBool(IntToStr(i), 'ScannedEver', mf.ScannedEver);
  end;

  XMLFile.UpdateFile;
  XMLFile.Free;
end;

procedure TMediaFoldersList.LoadFromFile(FileName: string);
var
  XMLFile: TIniFile;
  i:  integer;
  mf: TMediaFolder;
  s:  TStringList;
begin
  FixFolderNames(FileName);
  XMLFile := TIniFile.Create(FileName);
  s := TStringList.Create;
  XMLFile.ReadSections(s);
  if s.Count = 0 then
  begin
    s.Free;
    XMLFile.Free;
    Exit;
  end;
  //XMLFile.Clear;

  for i := 0 to s.Count - 1 do
  begin
    mf.Folder := XMLFile.ReadString(IntToStr(i), 'Folder', '');
    mf.Description := XMLFile.ReadString(IntToStr(i), 'Desc', '');
    mf.Name := XMLFile.ReadString(IntToStr(i), 'Name', '');
    mf.LastScanned := XMLFile.ReadDateTime(IntToStr(i), 'LastScanned', Now);
    mf.ScannedEver := XMLFile.ReadBool(IntToStr(i), 'ScannedEver', False);
    Add(mf);
  end;

  XMLFile.Free;
end;

end.

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

unit BookmarksU;

interface

uses Classes, SysUtils, SpkXMLParser, profilefunc;

type
  TBookmarkItem = record
    URL:  string;
    Name: string;
  end;

type
  TBookmarkItemObject = class
    Entry: TBookmarkItem;
  end;

type
  TBookmarksList = class(TList)
  public
    procedure Clear;
    constructor Create;
    destructor Destroy;
    procedure Add(Item: TBookmarkItem);
    procedure ReplaceEntry(NewEntry: TBookmarkItem; Index: integer);
    procedure RemoveEntry(Index: integer);
    function GetItem(Index: integer): TBookmarkItem;
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
  end;


implementation

uses kspfiles, KSPConstsVars, multilog;

constructor TBookmarksList.Create;
begin
  inherited Create;
end;

destructor TBookmarksList.Destroy;
begin
  //Clear;
  inherited Destroy;
end;

procedure TBookmarksList.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    TBookmarkItemObject(Items[i]).Free;
end;

procedure TBookmarksList.Add(Item: TBookmarkItem);
var
  t: TBookmarkItemObject;
begin
  t := TBookmarkItemObject.Create;
  t.Entry := Item;
  inherited Add(T);
end;

function TBookmarksList.GetItem(Index: integer): TBookmarkItem;
begin
  Result.URL  := '';
  Result.Name := '';
  if (Index >= 0) and (Index < Count) then
    Result := TBookmarkItemObject(Items[Index]).Entry;
end;

procedure TBookmarksList.LoadFromFile(FileName: string);
var
  XML: TSpkXMLParser;
  Main, Sub: TSpkXMLNode;
  Par: TSpkXMLParameter;
  p:   TBookmarkItem;
  i:   integer;
  s:   string;
  s2:  TStringList;

  function ItemExists(item: string): boolean;
  var
    i: integer;
  begin
    Result := False;
    for i := 0 to Self.Count - 1 do
      if Self.GetItem(i).URL = item then
        Result := True;
  end;

begin
  FixFolderNames(FileName);
  if not FileExists(FileName) then
    Exit;
  XML := TSpkXMLParser.Create;
  XML.LoadFromFile(FileName);
  Main := XML.NodeByName['bookmarks', False];
  if Main <> nil then
    for i := 0 to Main.Count - 1 do
    begin
      Sub := Main.SubNodeByIndex[i];
      Par := Sub.Parameters.ParamByName['name', False];
      if Par = nil then
        Continue;
      p.Name := Par.Value;
      Par    := Sub.Parameters.ParamByName['url', False];
      if Par = nil then
        Continue;
      p.URL := Par.Value;
      Add(p);
    end;
  XML.Free;

  s := KSPDataFolder + 'bookmarks';
  FixFolderNames(s);

  s2 := TStringList.Create;

  SearchForFilesFS(s, True, s2);
  for i := 0 to s2.Count - 1 do
  begin
    if not ItemExists(s2.Strings[i]) then
    begin
      hLog.Send('Deleting old bookmark: ' + s2.Strings[i]);
      DeleteFile(s2.Strings[i]);
    end;
  end;

  s2.Free;
end;

procedure TBookmarksList.SaveToFile(FileName: string);
var
  XML: TSpkXMLParser;
  Main, Sub: TSpkXMLNode;
  p:   TBookmarkItem;
  i:   integer;
begin
  FixFolderNames(FileName);
  XML  := TSpkXMLParser.Create;
  Main := TSpkXMLNode.Create('bookmarks');
  for i := 0 to Count - 1 do
  begin
    p   := GetItem(i);
    Sub := TSpkXMLNode.Create('Item' + IntToStr(i));
    Sub.Parameters.Add(p.URL, 'url');
    Sub.Parameters.Add(p.Name, 'name');
    Main.Add(Sub);
  end;
  XML.Add(Main);
  XML.SaveToFile(FileName);
  ;
  XML.Free;
end;

procedure TBookmarksList.ReplaceEntry(NewEntry: TBookmarkItem; Index: integer);
begin
  if (Index >= 0) and (Index < Count) then
    TBookmarkItemObject(Items[Index]).Entry := NewEntry;
end;

procedure TBookmarksList.RemoveEntry(Index: integer);
begin
  if (Index >= 0) and (Index < Count) then
    Delete(Index);
end;

end.

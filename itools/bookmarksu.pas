unit BookmarksU;

interface

uses Classes, SysUtils, SpkXMLParser, profilefunc;

type TBookmarkItem = record
    URL: string;
    Name: string;
  end;

type TBookmarkItemObject = class
    Entry: TBookmarkItem;
  end;

type TBookmarksList = class(TList)
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
  t:=TBookmarkItemObject.Create;
  t.Entry:=Item;
  inherited Add(T);
end;

function TBookmarksList.GetItem(Index: Integer): TBookmarkItem;
begin
  Result.URL:='';
  Result.Name:='';
  if (Index>=0)and(Index<Count) then
    Result:=TBookmarkItemObject(Items[Index]).Entry;
end;

procedure TBookmarksList.LoadFromFile(FileName: string);
var
  XML: TSpkXMLParser;
  Main, Sub: TSpkXMLNode;
  Par: TSpkXMLParameter;
  p: TBookmarkItem;
  i: integer;
  s: string;
  s2: TStringList;

  function ItemExists(item: string): boolean;
  var
    i: integer;
  begin
    Result:=false;
    for i:=0 to Self.Count-1 do
      if Self.GetItem(i).URL=item then Result:=true;
  end;

begin
  FixFolderNames(FileName);
  if not FileExists(FileName) then Exit;
  XML:=TSpkXMLParser.create;
  XML.LoadFromFile(FileName);
  Main:=XML.NodeByName['bookmarks', false];
  if Main<>nil then
    for i := 0 to Main.Count - 1 do
      begin
        Sub:=Main.SubNodeByIndex[i];
        Par:=Sub.Parameters.ParamByName['name', false];
        if Par=nil then
          Continue;
        p.Name:=Par.Value;
        Par:=Sub.Parameters.ParamByName['url', false];
        if Par=nil then
          Continue;
        p.URL:=Par.Value;
        Add(p);
      end;
  XML.Free;

  s:=KSPDataFolder+'bookmarks';
  FixFolderNames(s);

  s2:= TStringList.Create;

  SearchForFilesFS(s, true, s2);
  for i:=0 to s2.Count-1 do begin
    if not ItemExists(s2.Strings[i]) then begin
      hLog.Send('Deleting old bookmark: '+s2.Strings[i]);
      DeleteFile(s2.Strings[i]);
    end;
  end;

  s2.Free;
end;

procedure TBookmarksList.SaveToFile(FileName: string);
var
  XML: TSpkXMLParser;
  Main, Sub: TSpkXMLNode;
  p: TBookmarkItem;
  i: integer;
begin
  FixFolderNames(FileName);
  XML:=TSpkXMLParser.create;
  Main:=TSpkXMLNode.create('bookmarks');
  for i := 0 to Count - 1 do
    begin
      p:=GetItem(i);
      Sub:=TSpkXMLNode.create('Item'+IntToStr(i));
      Sub.Parameters.Add(p.URL, 'url');
      Sub.Parameters.Add(p.Name, 'name');
      Main.Add(Sub);
    end;
  XML.Add(Main);
  XML.SaveToFile(FileName);;
  XML.Free;
end;

procedure TBookmarksList.ReplaceEntry(NewEntry: TBookmarkItem; Index: integer);
begin
  if (Index>=0)and(Index<Count) then
    TBookmarkItemObject(Items[Index]).Entry:=NewEntry;
end;

procedure TBookmarksList.RemoveEntry(Index: integer);
begin
  if (Index>=0)and(Index<Count) then
    Delete(Index);
end;

end.

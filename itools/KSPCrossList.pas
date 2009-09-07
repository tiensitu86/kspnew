unit KSPCrossList;

interface

uses SysUtils, Classes;

type TCrossEntry = class
      SubList: TStringList;
      Name: string;
    public
      constructor Create;
      destructor Destroy;
  end;

type TCrossList = class(TList)
      fEntry: TCrossEntry;
    public
      procedure Add(name: string);
      destructor Destroy;
      property Entry: TCrossEntry read fEntry write fEntry;
      procedure Sort;
    end;

implementation

function CompareName(Item1, Item2: Pointer): integer;
begin
  Result := CompareText(TCrossEntry(Item1).Name, TCrossEntry(Item2).Name);
end;

procedure TCrossList.Sort;
var
  i: integer;
begin
  inherited Sort(@CompareName);
  if Count>0 then
    for i:=0 to Count-1 do
      TCrossEntry(Items[i]).SubList.Sort;
end;

procedure TCrossList.Add(name: string);
var
  T: TCrossEntry;
begin
  T:=TCrossEntry.Create;
  T.Name:=name;
  inherited Add(T);
end;

constructor TCrossEntry.Create;
begin
  inherited Create;
  SubList:=TStringList.Create;
end;

destructor TCrossEntry.Destroy;
begin
  SubList.Free;
  inherited Destroy;
end;

destructor TCrossList.Destroy;
var
  i: integer;
begin
  if Count>0 then
    for i:=0 to Count-1 do
      TCrossEntry(Items[i]).Free;
  inherited Destroy;
end;

end.

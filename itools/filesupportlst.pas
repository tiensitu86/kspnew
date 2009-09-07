unit FileSupportLst;

{$MODE Delphi}

interface

uses LCLIntf, {PluginCtrl,} Classes, Dynamic_bass,
    SysUtils, {RT_basscd,} Dialogs;

const MAXCDDRIVES = 8;

type TSupportedBy = (Both, BASSNative, WinampPlugin, None);

TCDDriveList     = array[0..MAXCDDRIVES-1] of string[255];

type
  ExtArr = array[0..15] of string;

  TFileDesc = record
//        Extensions: ExtArr;
        Description: string;
        Handle : HPLUGIN;
        Name   : string;
        Version : DWORD;
        NumFormat  : DWORD;
        FormatP : PBASS_PLUGINFORMS;
      end;

    TFileInfo = class(TObject)
  public
    Entry: TFileDesc;
  end;

TFileSupportList = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Entry: TFileDesc);
    procedure Remove(Index: DWORD; IsHandle: boolean = false);
    function GetItem(Index: Integer): TFileDesc;
    function FindExtension(Ext: string; CharSize: boolean): integer;
    function FindName(eName: string; ForbiddenCheck: boolean = false): integer;
    procedure SetEnableStatus(eName: string; Enable: boolean);
    function PluginsForbidden(eName: string): boolean; overload;
  end;

implementation

uses KSPConstsVars;

constructor TFileSupportList.Create;
begin
  inherited Create;
end;

{The Items should be freed here but it isn't. Doesn't matter.
TPlayList is created only once and destroyed only while KSP is
to be closed}

destructor TFileSupportList.Destroy;
var
  i: integer;
begin
  for I := 0 to Count-1 do
    TFileInfo(Items[I]).Free;
  inherited Destroy;
end;

procedure TFileSupportList.Add(Entry: TFileDesc);
var
  T: TFileInfo;
begin
  T:=TFileInfo.Create;
  T.Entry:=Entry;
  inherited Add(T);
end;

procedure TFileSupportList.Remove(Index: DWORD; IsHandle: boolean = false);
var
  i: integer;
begin
  if not IsHandle then begin
      TFileInfo(Items[Index]).Free;
      Delete(Index);
    end else
    begin
      for i:=0 to Count-1 do
        if GetItem(i).Handle=Index then begin
            TFileInfo(Items[i]).Free;
            Delete(i);
            Break;
          end;
    end;
end;

function TFileSupportList.GetItem(Index: Integer): TFileDesc;
begin
  Result:=TFileInfo(Items[Index]).Entry;
end;

function TFileSupportList.FindName(eName: string; ForbiddenCheck: boolean = false): integer;
var
  i: integer;
begin
  Result:=-1;
  if ForbiddenCheck then begin
    if PluginsForbidden(eName) then
      Result:=-2;
  end else begin
    eName:=UpperCase(eName);
    if (Count>0) and (eName<>'') then begin
        for i:=0 to Count-1 do begin
            if UpperCase(TFileInfo(Items[i]).Entry.Name)=eName then
              begin Result:=i; Break; end;
          end;
      end;
  end;
end;

function TFileSupportList.PluginsForbidden(eName: string): boolean;
var
  i: integer;
  s: TStringList;
begin
  Result:=false;
  s:=TStringList.Create;
  if FileExists(KSPPluginsBlacklist) then begin
    s.LoadFromFile(KSPPluginsBlacklist);

    for i:=0 to s.Count-1 do
      if s.Strings[i]=eName then Result:=true;
  end;

  s.Free;
end;

procedure TFileSupportList.SetEnableStatus(eName: string; Enable: boolean);
var
  s: TStringlist;

  procedure DisablePlugin;
  begin
    s.Add(eName);
  end;

  procedure EnablePlugin;
  var
    i: integer;
  begin
    for i:=s.Count-1 downto 0 do
    begin
      if s.Strings[i]=eName then s.Delete(i);
    end;
  end;

begin
  s:=TStringList.Create;
  if FileExists(KSPPluginsBlacklist) then
    s.LoadFromFile(KSPPluginsBlacklist);
  if Enable then begin
    EnablePlugin;
  end else begin
    if not PluginsForbidden(eName) then DisablePlugin;
  end;
  s.SaveToFile(KSPPluginsBlacklist);
  s.Free;
end;

function TFileSupportList.FindExtension(Ext: string; CharSize: boolean): integer;
var
  i, x: integer;
  found: boolean;
begin
  Result:=-1;
  if not CharSize then Ext:=UpperCase(Ext);

  if Pos(Ext, UpperCase(Player.NativeFileExts))>-1 then Result:=0;

  if (Count>0) and (Ext<>'') and (Result<>-2) then begin
      for i:=0 to Count-1 do begin
          found:=false;
          for x:=0 to TFileInfo(Items[i]).Entry.NumFormat-1 do
            if UpperCase(TFileInfo(Items[i]).Entry.FormatP[x].exts)=Ext then begin
                found:=true;
              end;

          if found then begin Result:=i; Break; end;
        end;
    end;
end;

end.
 

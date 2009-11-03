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

unit FileSupportLst;

{$MODE Delphi}

interface

uses LCLIntf, {PluginCtrl,} Classes, bass,
  SysUtils, {RT_basscd,} Dialogs;

const
  MAXCDDRIVES = 8;

type
  TSupportedBy = (Both, BASSNative, WinampPlugin, None);

  TCDDriveList = array[0..MAXCDDRIVES - 1] of string[255];

type
  ExtArr = array[0..15] of string;

  TFileDesc = record
    //        Extensions: ExtArr;
    Description: string;
    Handle:    HPLUGIN;
    Name:      string;
    Version:   DWORD;
    NumFormat: DWORD;
    FormatP:   PBASS_PLUGINFORMS;
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
    procedure Remove(Index: DWORD; IsHandle: boolean = False);
    function GetItem(Index: integer): TFileDesc;
    function FindExtension(Ext: string; CharSize: boolean): integer;
    function FindName(eName: string; ForbiddenCheck: boolean = False): integer;
    procedure SetEnableStatus(eName: string; Enable: boolean);
    function PluginsForbidden(eName: string): boolean; overload;
  end;

implementation

uses KSPConstsVars, multilog;

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
  for I := 0 to Count - 1 do
    TFileInfo(Items[I]).Free;
  inherited Destroy;
end;

procedure TFileSupportList.Add(Entry: TFileDesc);
var
  T: TFileInfo;
begin
  T := TFileInfo.Create;
  T.Entry := Entry;
  inherited Add(T);
end;

procedure TFileSupportList.Remove(Index: DWORD; IsHandle: boolean = False);
var
  i: integer;
begin
  if not IsHandle then
  begin
    TFileInfo(Items[Index]).Free;
    Delete(Index);
  end
  else
  begin
    for i := 0 to Count - 1 do
      if GetItem(i).Handle = Index then
      begin
        TFileInfo(Items[i]).Free;
        Delete(i);
        Break;
      end;
  end;
end;

function TFileSupportList.GetItem(Index: integer): TFileDesc;
begin
  Result := TFileInfo(Items[Index]).Entry;
end;

function TFileSupportList.FindName(eName: string;
  ForbiddenCheck: boolean = False): integer;
var
  i: integer;
begin
  Result := -1;
  if ForbiddenCheck then
  begin
    if PluginsForbidden(eName) then
      Result := -2;
  end
  else
  begin
    eName := UpperCase(eName);
    if (Count > 0) and (eName <> '') then
    begin
      for i := 0 to Count - 1 do
      begin
        if UpperCase(TFileInfo(Items[i]).Entry.Name) = eName then
        begin
          Result := i;
          Break;
        end;
      end;
    end;
  end;
end;

function TFileSupportList.PluginsForbidden(eName: string): boolean;
var
  i: integer;
  s: TStringList;
begin
  Result := False;
  s      := TStringList.Create;
  if FileExists(KSPPluginsBlacklist) then
  begin
    s.LoadFromFile(KSPPluginsBlacklist);

    for i := 0 to s.Count - 1 do
      if s.Strings[i] = eName then
        Result := True;
  end;

  s.Free;
end;

procedure TFileSupportList.SetEnableStatus(eName: string; Enable: boolean);
var
  s: TStringList;

  procedure DisablePlugin;
  begin
    s.Add(eName);
  end;

  procedure EnablePlugin;
  var
    i: integer;
  begin
    for i := s.Count - 1 downto 0 do
    begin
      if s.Strings[i] = eName then
        s.Delete(i);
    end;
  end;

begin
  s := TStringList.Create;
  if FileExists(KSPPluginsBlacklist) then
    s.LoadFromFile(KSPPluginsBlacklist);
  if Enable then
  begin
    EnablePlugin;
  end
  else
  begin
    if not PluginsForbidden(eName) then
      DisablePlugin;
  end;
  s.SaveToFile(KSPPluginsBlacklist);
  s.Free;
end;

function TFileSupportList.FindExtension(Ext: string; CharSize: boolean): integer;
var
  i, x:  integer;
  found: boolean;
begin
  Result := -1;
  if not CharSize then
    Ext := UpperCase(Ext);

  if Pos(Ext, UpperCase(Player.NativeFileExts)) > 0 then
    Result := 0;

  if (Count > 0) and (Ext <> '') and (Result <> -2) then
  begin
    for i := 0 to Count - 1 do
    begin
      found := False;
      //hLog.Send('Extension2: '+TFileInfo(Items[i]).Entry.FormatP[0].exts);
      for x := 0 to TFileInfo(Items[i]).Entry.NumFormat - 1 do
        if UpperCase(TFileInfo(Items[i]).Entry.FormatP[x].exts) = Ext then
        begin
          found := True;
        end;

      if found then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

end.
 
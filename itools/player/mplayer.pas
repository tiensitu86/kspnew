unit mplayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cplayer, FileSupportLst;

type
  TMainPlayer = class(TComponent)
  private
    fPlayer: TCorePlayer;
    function GetNativeFileExts: string;
  public
    property NativeFileExts: string Read GetNativeFileExts;
    function AddonLoad(FilePath: string; ForceLoad: boolean = False): TFileDesc;
    function AddonFree(AddonHandle: DWORD): integer; overload;
    function AddonFree(Addon: string): integer; overload;
  end;

implementation

function TMainPlayer.AddonLoad(FilePath: string; ForceLoad: boolean = False): TFileDesc;
begin
  if fPlayer.PlayerEnabled then fPlayer.AddonLoad(FilePath, ForceLoad);
end;

function TMainPlayer.AddonFree(AddonHandle: DWORD): integer; overload;
begin
  if fPlayer.PlayerEnabled then fPlayer.AddonFree(AddonHandle);
end;

function TMainPlayer.AddonFree(Addon: string): integer; overload;
begin
  if fPlayer.PlayerEnabled then fPlayer.AddonFree(Addon);
end;

function TMainPlayer.GetNativeFileExts: string;
begin
//  if fPlayer.PlayerEnabled then Result:=fPlayer.NativeFileExts else Result:='';
end;

end.


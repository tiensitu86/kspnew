unit plsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, ID3Mgmnt, Graphics;

type

  { TPlaylistFrame }

  TPlaylistFrame = class(TFrame)
    ItemName: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FrameDblClick(Sender: TObject);
    procedure ItemNameClick(Sender: TObject);
  private
    fIndex: integer;
    { private declarations }
  public
    { public declarations }
    procedure SetTrackInfo(Info: TPLEntry; Index: integer);
    procedure SetCurrentlyPlayed(CurrentlyPlayed: boolean);
  end; 

implementation

uses kspfiles, main, KSPConstsVars, FileSupport, kspstrings;

procedure TPlaylistFrame.SetCurrentlyPlayed(CurrentlyPlayed: boolean);
begin
  if CurrentlyPlayed then
    ItemName.Font.Style:=ItemName.Font.Style+[fsBold] else
    ItemName.Font.Style:=ItemName.Font.Style-[fsBold];
end;

procedure TPlaylistFrame.ItemNameClick(Sender: TObject);
begin
  if Height=18 then Height:=95 else Height:=18;
end;

procedure TPlaylistFrame.FrameDblClick(Sender: TObject);
begin
  KSPMainWindow.CurrentIndex:=fIndex;
  KSPMainWindow.PlayFile;
end;

procedure TPlaylistFrame.SetTrackInfo(Info: TPLEntry; Index: integer);
var
  s: string;
begin
  if not IsStream(Info.FileName) then
  s:=ProduceFormatedString(KSPMainWindow.FormatedPlayListInfo,
            Info.Tag, GetDuration(Info.Stream),
            Index+1) else begin
      if (KSPMainWindow.CurrentIndex=Index) and (Player.StreamInfo.Title<>'') then
      s:=Format(SShoutcastEntry, [Player.StreamInfo.Title]) else
      s:=Format(SShoutcastEntry, [Info.FileName]);
    end;
  ItemName.Caption:=s;
end;

initialization
  {$I plsframe.lrs}

end.


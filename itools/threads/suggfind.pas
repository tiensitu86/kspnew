{This thread will do some additional things while PlayFile is called.
For example VDJ setup is done here, here are suggestions found, etc.
It is because it cannot be done in main thread where PlayFile is called.
Some things done here can take a lot CPU time so if we don't want to have
so big breaks, we should do it in additional thread so PlayFile can be finished
and playback can be started}
unit suggfind;

interface

uses
  Classes, SysUtils, Dialogs, KSPFiles, KSPMessages, FileUtil, KSPStrings;

type
  TFindSugg = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  end;

implementation

uses Playlists, Main, app_db_utils, KSPConstsVars, Math,
  ID3Mgmnt, DateUtils, FileSupport, multilog, MediaItems;

{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TFindSugg.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TFindSugg }

function IsForbidden(name: string): boolean;
var
  x: integer;
begin
  Result:=false;
  for x:=0 to KSPMainWindow.Forbidden.Count-1 do begin
    Result:=Result or (CompareFilenames(name, KSPMainWindow.Forbidden.Strings[x])=0);
  end;
  Result:=Result or KSPMainWindow.Playlist.ContainsFileName(name);
end;

procedure TFindSugg.Execute;
var
  Fav: TFavouriteList;
  p: TPLEntry;
  i: integer;
  Max: integer;
  cfile, s: string;
  GetIsTag: boolean;
begin
  { Place thread code here }
      Self.Priority:=tpLower;
      cfile:=KSPMainWindow.GetCurrentFile;
      SuggestionList.Clear;
      KSPMainWindow.SuggList.Clear;
      KSPMainWindow.SuggList.Items.Add(SFetchingSuggs);


//      StrPCopy(Pc, KSPMainWindow.GetCurrentFile);

      hLog.Send('Reading info...');

      AllSongs.OpenQuery(Format(SelectGetItem, [PrepareString(cfile)]));
      i:=AllSongs.ReturnRecordsCount;
      if i>0 then p:=AllSongs.ReadEntry;
      AllSongs.CloseQuery;


      if i>0 then begin
          //p:=KSPMainWindow.AllSongs.GetItem2(i);
          hLog.Send('Track in library, updating info...');
          p.LastPlay:=Now;
          if not p.PlayedEver then
            p.FirstPlay:=Now;
          p.PlayedEver:=true;
          p.PlayCount:=p.PlayCount+1;
          p.Fav:=p.PlayCount / KSPMainWindow.TotalPlayCount;
          hLog.Send('Writing...');
          AllSongs.Add(p, false);
        end;

      hLog.Send('Searching for suggestions...');

      //KSPMainWindow.VDJ.Forbidden.Add(KSPMainWindow.GetCurrentFile);

//      Fav:=TFavouriteList.Create;

      fav:=AllSongs.GetFavList(cfile);

      KSPMainWindow.SuggList.Clear;

      for i:=0 to fav.Count-1 do begin
        p.FileName:=fav.GetItem(i).FileName;
        p.Tag:=ReadID3(p.FileName);
        if p.Tag.Artist = '' then p.Tag.Artist:=(SUnknownArtist);
        if p.Tag.Album = '' then p.Tag.Album:=(SUnknownAlbum);
        if p.Tag.Title = '' then p.Tag.Album:=(SUnknownTitle);
        if p.Tag.Genre = '' then p.Tag.Album:=(SUnknownGenre);
        if p.Tag.Year = '' then p.Tag.Album:=(SUnknownYear);
        p.Stream:=GetStreamInfoSimple(p.FileName, GetIsTag);
        SuggestionList.Add(p);
        s:=ProduceFormatedString(KSPMainWindow.FormatedPlayListInfo,
            p.Tag, GetDuration(p.Stream),
            i+1);
        KSPMainWindow.SuggList.Items.Add(s);
      end;

      hLog.Send('Fav list fetched...');

      if (KSPMainWindow.UseVDJ) then begin

          while KSPMainWindow.CurrentIndex>4 do begin
            KSPMainWindow.RemoveFromPlayList(0);
          end;

          {if KSPMainWindow.VDJ.Max+KSPMainWindow.KSPPlaylist.Count<KSPMainWindow.VDJ.MaxPls then
            Max:=KSPMainWindow.VDJ.Max else
            Max:=KSPMainWindow.VDJ.MaxPls-KSPMainWindow.KSPPlaylist.Count;}
          Max:=10;

          if Max<0 then Max:=0;

          hLog.Send('VDJ: Looking for '+IntToStr(Max)+' new songs');

          KSPMainWindow.Forbidden.Add(cfile);

          for i:=0 to fav.Count-1 do
            if not IsForbidden(fav.GetItem(i).FileName) then begin
              hLog.Send('VDJ: adding file: '+fav.GetItem(i).FileName);
              KSPMainWindow.AddToPlayList(fav.GetItem(i).FileName);
            end;


        end;

      fav.Free;
      hLog.Send('Preparing to post message...');
      while KSPMainWindow.Forbidden.Count>100 do
        KSPMainWindow.Forbidden.Delete(0);


  //PostMessage(KSPMainWindow.Handle, WM_SUGGESTIONSFOUND, 0, 0);

end;

end.

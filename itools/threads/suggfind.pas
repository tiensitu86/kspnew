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

unit suggfind;

interface

uses
  Classes, SysUtils, Dialogs, KSPFiles, FileUtil, KSPStrings;

type
  TFindSugg = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  end;

implementation

uses Main, app_db_utils, KSPConstsVars,
  ID3Mgmnt, DateUtils, FileSupport, multilog;

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
      if fav.Count=0 then
        KSPMainWindow.SuggList.Items.Add(SNoSuggestions);

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

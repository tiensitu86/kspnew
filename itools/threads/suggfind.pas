{This thread will do some additional things while PlayFile is called.
For example VDJ setup is done here, here are suggestions found, etc.
It is because it cannot be done in main thread where PlayFile is called.
Some things done here can take a lot CPU time so if we don't want to have
so big breaks, we should do it in additional thread so PlayFile can be finished
and playback can be started}
unit suggfind;

interface

uses
  Classes, SysUtils, Dialogs, KSPFiles, KSPMessages;

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

procedure TFindSugg.Execute;
var
  Fav: TFavouriteList;
  p: TPLEntry;
  i: integer;
//  Sup: TSupportedBy;
//  StreamInfo2 : TStreamInfo;
//  PNum: Integer;
//  PName: string;
  Max: integer;
//  Pc: TPathChar;
begin
  { Place thread code here }
      Self.Priority:=tpLower;


//      StrPCopy(Pc, KSPMainWindow.GetCurrentFile);

      hLog.Send('Reading info...');

      AllSongs.OpenQuery(Format(SelectGetItem, [PrepareString(KSPMainWindow.GetCurrentFile)]));
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

      fav:=AllSongs.GetFavList(KSPMainWindow.GetCurrentFile);

      hLog.Send('Fav list fetched...');
      //SuggestionList.Clear;

      //if we are not looking for items from chosen genre then GID should be -1
      //we must ensure that it is
      //SortMediaFavList(fav, p, KSPMainWindow.Forbidden);
      hLog.Send('Fav list sorted...');

      fav.Free;

      hLog.Send('Suggestion search done');

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

          //GetNextSongs(Max, KSPMainWindow.VDJ.Forbidden, KSPMainWindow.VDJ.Names,
          //  KSPMainWindow.VDJ.Rare, KSPMainWindow.KSPPlaylist);
        end;
  hLog.Send('Preparing to post message...');
  //    while KSPMainWindow.Forbidden.Count>KSPMainWindow.VDJ.ForbiddenCount do
  //      KSPMainWindow.VDJ.Forbidden.Delete(0);

  //PostMessage(KSPMainWindow.Handle, WM_SUGGESTIONSFOUND, 0, 0);

end;

end.

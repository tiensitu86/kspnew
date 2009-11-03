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

unit LoadPlsThread;

interface

uses
  Classes, SysUtils;

type
  TLoadPlsThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  public
    aFileName:   string;
    aFromMemory: boolean;
  end;

implementation

uses Main, PlayLists, KSPConstsVars, MultiLog;

{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TLoadPlsThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TLoadPlsThread }

procedure TLoadPlsThread.Execute;
var
  Pls: TXmlPlayList;
  i:   integer;
  PlayList2: TPlayList;
begin
  if FileExists(aFileName) then
  begin
    PlayList2 := TPlayList.Create;
    hLog.Send('PLAYLIST LOADING: Playlist object created');
    //  LoadPlsSem := CreateSemaphore(nil, 0,1,'KSPLoadPls');
    if not aFromMemory then
    begin

      KSPMainWindow.LoadingPlaylist := True;
      Self.Priority := tpHigher;
      Pls := TXMLPlayList.Create;
      Pls.LoadPls(aFileName, PlayList2);
      hLog.Send('PLAYLIST LOADING: Playlist read from file');
      Pls.Free;

    end
    else
    begin
      for i := 0 to KSPMainWindow.MediaSongs.Count - 1 do
      begin
        PlayList2.Add(KSPMainWindow.MediaSongs.GetItem(i)^);
      end;
    end;

    //KSPMainWindow.lbPlayList.Items.Clear;

    if PlayList2.Count = 0 then
    begin
      KSPMainWindow.LoadingPlaylist := False;
      LoadPlsSem2 := 0;//ReleaseSemaphore(LoadPlsSem, 1, nil);
      Exit;
    end;

    for i := 0 to PlayList2.Count - 1 do
    begin
      KSPMainWindow.AddToPlayList(PlayList2.GetItem(i).FileName, True);
    end;

    hLog.Send('PLAYLIST LOADING: Playlist loaded. Recalculating times');

    KSPMainWindow.PlayListTotalTime;
    PlayList2.Free;
    KSPMainWindow.LoadingPlaylist := False;
    hLog.Send('PLAYLIST LOADING: Playlist loading done');
  end;

  LoadPlsSem2 := 0;//ReleaseSemaphore(LoadPlsSem, 1, nil);
end;

end.

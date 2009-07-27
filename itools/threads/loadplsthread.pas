unit LoadPlsThread;

interface

uses
  Classes;

type
  TLoadPlsThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  public
    aFileName: string;
    constructor Create(Suspended: boolean; FileName: string); overload;
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

constructor TLoadPlsThread.Create(Suspended: boolean; FileName: string);
begin
  inherited Create(Suspended);
  aFileName:=FileName;
end;

procedure TLoadPlsThread.Execute;
var
  Pls:TXmlPlayList;
  i: integer;
  PlayList2: TPlayList;
begin
//  LoadPlsSem := CreateSemaphore(nil, 0,1,'KSPLoadPls');

  KSPMainWindow.LoadingPlaylist:=true;
  Self.Priority:=tpHigher;
  Pls:=TXMLPlayList.create;
  PlayList2:=TPlayList.Create;
  hLog.Send('PLAYLIST LOADING: Playlist object created');
  Pls.LoadPls(aFileName, PlayList2);
  hLog.Send('PLAYLIST LOADING: Playlist read from file');
  Pls.Free;

  KSPMainWindow.lbPlayList.Items.Clear;

  if PlayList2.Count=0 then begin
    KSPMainWindow.LoadingPlaylist:=false;
    LoadPlsSem2:=0;//ReleaseSemaphore(LoadPlsSem, 1, nil);
    Exit;
  end;

    for i:=0 to PlayList2.Count-1 do begin
      KSPMainWindow.AddToPlayList(PlayList2.GetItem(i).FileName, true);
    end;

  hLog.Send('PLAYLIST LOADING: Playlist loaded. Recalculating times');

  KSPMainWindow.PlayListTotalTime;
  PlayList2.Free;
  KSPMainWindow.LoadingPlaylist:=false;
  hLog.Send('PLAYLIST LOADING: Playlist loading done');

  LoadPlsSem2:=0;//ReleaseSemaphore(LoadPlsSem, 1, nil);
end;

end.

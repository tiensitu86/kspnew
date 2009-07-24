unit StartupThread;

interface

uses
  Windows, Classes, SysUtils, Forms;

type
  TStartupThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  end;

implementation

uses KSPConstsVars, FileSupportLst,MultiLog;

{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TStartupThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TStartupThread }

procedure SetupNativeFileFormats;
var
  a:TFileDesc;
const
  nMO3 = '.MO3';
  nIT ='.IT';
  nXM ='.XM';
  nS3M = '.S3M';
  nMTM = '.MTM';
  nMOD = '.MOD';
  nUMX = '.UMX';
var
  i: integer;
begin
//  EXE_Running(Application.ExeName, true);
{  a.name:='Playlists';
  a.Extensions[0]:='.kpl';
  a.Extensions[1]:='.m3u';
  a.Extensions[2]:='.pls';
  a.Description:='Playlists';
  FileSupportList.Add(a);
  for i:=0 to Length(a.Extensions)-1 do a.Extensions[i]:='';

  a.Extensions[0]:=nMO3; a.Extensions[1]:=nIT; a.Extensions[2]:=nXM;
  a.Extensions[3]:=nS3M; a.Extensions[4]:=nMTM; a.Extensions[5]:=nMOD;
  a.Extensions[6]:=nUMX;
  a.name:='Native KSP Music Files';
  a.Description:='Native KSP Music Files Support';
  FileSupportList.Add(a);
  for i:=0 to Length(a.Extensions)-1 do a.Extensions[i]:='';

  a.Extensions[0]:='.mp3'; a.Extensions[1]:='.mp2'; a.Extensions[2]:='.mp1';
  a.name:='MPEG1 Layer 3 file';
  a.Description:='MPEG files support';
  FileSupportList.Add(a);
  for i:=0 to Length(a.Extensions)-1 do a.Extensions[i]:='';

  a.Extensions[0]:='.wav';
  a.name:='Windows Wave Files';
  a.Description:='WAV files';
  FileSupportList.Add(a);
  for i:=0 to Length(a.Extensions)-1 do a.Extensions[i]:='';

  a.Extensions[0]:='.wma';
  a.name:='WMA';
  a.Description:='Windows Media Audio';
  FileSupportList.Add(a);
  for i:=0 to Length(a.Extensions)-1 do a.Extensions[i]:='';

  a.Extensions[0]:='.ogg';
  a.name:='OGGFile';
  a.Description:='OGG Vorbis';
  FileSupportList.Add(a);
  for i:=0 to Length(a.Extensions)-1 do a.Extensions[i]:='';   }
end;

procedure TStartupThread.Execute;
begin
  { Place thread code here }
  hLog.Send('Running startup thread');
  SetupNativeFileFormats;
  hLog.Send('Releasing startup thread sems');
  ReleaseSemaphore(StartupThreadSem, 1, nil);
  //SetupExtensions;
  hLog.Send('Startup thread finished');
end;

end.

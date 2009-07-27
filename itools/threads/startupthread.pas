unit StartupThread;

interface

uses
  Classes, SysUtils, Forms;

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

procedure TStartupThread.Execute;
begin
  { Place thread code here }
  hLog.Send('Running startup thread');
  hLog.Send('Releasing startup thread sems');
  StartupThreadSem2:=0;//ReleaseSemaphore(StartupThreadSem, 1, nil);
  //SetupExtensions;
  hLog.Send('Startup thread finished');
end;

end.

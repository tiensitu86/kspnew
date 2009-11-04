unit updates;

interface

uses
  Classes, SysUtils, Dialogs, Forms;

type
  TCheckUpdates = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  public
    UpdatesStyle: integer;
    ManualCheck: boolean;
  end;

implementation

uses KSPConstsVars, kspfiles, IniFiles, multilog, main, kspstrings;

procedure TCheckUpdates.Execute;
var
  tmpFolder: string;
  Ini: TIniFile;
  i: integer;
  s1, s2, s3, s4: string;
  s: TStringList;
  CanUpdate: boolean;
  URL: string;
begin
  hLog.Send('Checking for updates');
  tmpFolder:=KSPDataFolder+'temp\';

  GetKSPVersion3(s1, s2, s3, s4);
  s:=TStringList.Create;
  DownloadURLi(KSPUpdates+s4, s);
  s.SaveToFile(KSPDataFolder+'update.ini');
  s.Clear;

  Ini:=TIniFile.Create(KSPDataFolder+'update.ini');
  s:=TStringList.Create;
  Ini.ReadSections(s);
  //ShowMessage(s.Text);
  hLog.Send('Updates list downloaded');

  CanUpdate:=s.Count>1;
  if (Self.UpdatesStyle<5) and FileExists(tmpFolder+'setup.exe') then DeleteFile(tmpFolder+'setup.exe');

  if CanUpdate then begin
    URL:=Ini.ReadString(s.Strings[0], 'Path', '');
    CanUpdate:=URL<>'';
    if CanUpdate then begin
      hLog.Send('Downloading file: '+URL);
      if (Self.UpdatesStyle>1) and (Self.UpdatesStyle<5) then
        CanUpdate:=DownloadFile(URL, tmpFolder+'setup.exe');
    end else hLog.Send('URL is empty');
  end;
  s.Free;
  Ini.Free;

  if (Self.UpdatesStyle=4) or (Self.UpdatesStyle=5) then begin
    Ini:=TIniFile.Create(KSPDataFolder+'updater.ini');
    Ini.EraseSection('updater');
    Ini.WriteBool('updater', 'run_update', true);
    Ini.Free;
  end;

  if CanUpdate then
    Application.QueueAsyncCall(KSPMainWindow.KSPUpdate, Self.UpdatesStyle) else
  if Self.ManualCheck then KSPShowMessage(SNoUpdate)
end;

end.


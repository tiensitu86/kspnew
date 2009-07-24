unit OptionsFrm2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls;

type

  { TKSPOptions }

  TKSPOptions = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    DBName: TLabeledEdit;
    DBPassword: TLabeledEdit;
    DBUsername: TLabeledEdit;
    Memo1: TMemo;
    ServerAddress: TLabeledEdit;
    ServerPort: TLabeledEdit;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  KSPOptions: TKSPOptions;

resourcestring
  sErrorConnectionToDatabase='An error occured while trying to connect to database server.\nPlease read applications documentation for more details\n\nError code: %d';

implementation

uses app_db_utils, IniFiles, KSPConstsVars;

{ TKSPOptions }

procedure TKSPOptions.Button2Click(Sender: TObject);
var
  i: integer;
  AppDB: TAppDBConnection;
  ConfFileName: string;
  Ini:TIniFile;
begin
//  KSPOptions.SQLConnection1.Params.SaveToFile(ExtractFilePath(ParamStr(0))+'db.ini');
  AppDB:=TAppDBConnection.Create;

  if not CheckBox1.Checked then begin

    ConfFileName:=KSPDataFolder+'db\ksp2.kspdb';
    Ini:=TIniFile.Create(ConfFileName);
    Ini.WriteString('MySQLConnection', 'DriverName', 'MySQL');
    Ini.WriteString('MySQLConnection', 'HostName', ServerAddress.Text);
    Ini.WriteString('MySQLConnection', 'Port', ServerPort.Text);
    Ini.WriteString('MySQLConnection', 'Database', DBName.Text);
    Ini.WriteString('MySQLConnection', 'User_Name', DBUserName.Text);
    Ini.WriteString('MySQLConnection', 'Password', DBPassword.Text);
    Ini.WriteString('MySQLConnection', 'BlobSize', '1');
    Ini.WriteString('MySQLConnection', 'ErrorResourceFile', '');
    Ini.WriteString('MySQLConnection', 'LocaleCode', '0000');
    Ini.WriteString('MySQLConnection', 'Compressed', 'False');
    Ini.WriteString('MySQLConnection', 'Encrypted', 'False');
    Ini.WriteString('MySQLConnection', 'ServerCharSet', 'utf8');

    Ini.Free;

    i:=AppDB.InitDatabase(ConfFileName);

  end else i:=AppDB.InitDatabase;

  AppDB.Free;
  if i<>0 then
    MessageDlg(Format(sErrorConnectionToDatabase, [i]), mtError, [mbOk], 0) else
    begin
      ModalResult:=mrOk;
      DeleteFile(KSPDataFolder+'db\ksp.kspdb');
      if not CheckBox1.Checked then
        RenameFile(ConfFileName, KSPDataFolder+'db\ksp.kspdb');
    end;
end;

procedure TKSPOptions.FormCreate(Sender: TObject);
var
  Ini:TIniFile;
  ConfFileName: string;
begin
  ConfFileName:=KSPDataFolder+'db\ksp.kspdb';

  if not FileExists(ConfFileName) then CheckBox1.Checked:=true else begin

    Ini:=TIniFile.Create(ConfFileName);

    ServerAddress.Text:=Ini.ReadString('MySQLConnection', 'HostName', '');
    ServerPort.Text:=Ini.ReadString('MySQLConnection', 'Port', '');
    DBName.Text:=Ini.ReadString('MySQLConnection', 'Database', '');
    DBUserName.Text:=Ini.ReadString('MySQLConnection', 'User_Name', '');
    DBPassword.Text:=Ini.ReadString('MySQLConnection', 'Password', '');

    Ini.Free;
  end;
end;

initialization
  {$I optionsfrm2.lrs}

end.


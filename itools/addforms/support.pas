unit Support;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TSupportForm }

  TSupportForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    SDD: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  SupportForm: TSupportForm;

implementation

{ TSupportForm }

procedure TSupportForm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TSupportForm.Button2Click(Sender: TObject);
begin
  if SDD.Execute then Memo1.Lines.SaveToFile(SDD.FileName);
end;

initialization
  {$I support.lrs}

end.


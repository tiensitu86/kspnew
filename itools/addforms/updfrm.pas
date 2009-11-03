unit updfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TUpdateForm }

  TUpdateForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  UpdateForm: TUpdateForm;

implementation

initialization
  {$I updfrm.lrs}

end.


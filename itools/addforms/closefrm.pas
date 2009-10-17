unit closefrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons;

type

  { TCloseActionForm }

  TCloseActionForm = class(TForm)
    AsDefault: TCheckBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  CloseActionForm: TCloseActionForm;

implementation

initialization
  {$I closefrm.lrs}

end.


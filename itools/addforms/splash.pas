unit splash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls;

type

  { TSplashForm }

  TSplashForm = class(TForm)
    Image1: TImage;
    Prog: TProgressBar;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  SplashForm: TSplashForm;

implementation

{ TSplashForm }


initialization
  {$I splash.lrs}

end.


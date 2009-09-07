unit IdAbout;

interface
{$I IdCompilerDefines.inc}
{$I IdDsgnCompilerDefines.inc}
uses
  {$IFDEF WidgetWinForms}
  IdAboutDotNET;
  {$ENDIF}
  {$IFDEF WidgetVCLLikeOrKylix}
  IdAboutVCL;
  {$ENDIF}

Procedure ShowAboutBox(const AProductName, AProductVersion : String);
Procedure ShowDlg;

implementation
 {$IFDEF WidgetWinForms}
 //for some reason, the Winforms designer doesn't like this in the same unit
 //as the class it's for
 {$R 'IdAboutDotNET.TfrmAbout.resources' 'IdAboutDotNET.resx'}
 {$ENDIF}

Procedure ShowAboutBox(const AProductName, AProductVersion : String);
begin
                                     
  TfrmAbout.ShowAboutBox(AProductName, AProductVersion);
end;

Procedure ShowDlg;
begin
  TfrmAbout.ShowDlg;
end;

end.

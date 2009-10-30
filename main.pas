unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, StdCtrls, AsyncProcess, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    Process1: TProcess;
    P1: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
    procedure ExecuteCmd(cmd: string);
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('IMPORTANT: During this process program may stop to respond'+#13+'If it occurs don''t worry. DO NOT terminate it or restart.'
    +#13+'Note that this process may take up to several minutes depending on your computer spec');
  Self.Memo1.Lines.Clear;
  P1.Position:=0;
  ExecuteCmd('upx -9 --lzma QtCore4.dll');
  ExecuteCmd('upx -9 --lzma QtGui4.dll');
  ExecuteCmd('upx -9 --lzma QtNetwork4.dll');
  ExecuteCmd('upx -9 --lzma QtSvg4.dll');
  ExecuteCmd('upx -9 --lzma QtWebKit4.dll');
  ExecuteCmd('upx -9 --lzma ksp.exe');
  ShowMessage('KSP packed successfully');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Self.Memo1.Lines.Clear;
  P1.Position:=0;
  ExecuteCmd('upx -d QtCore4.dll');
  ExecuteCmd('upx -d QtGui4.dll');
  ExecuteCmd('upx -d QtNetwork4.dll');
  ExecuteCmd('upx -d QtSvg4.dll');
  ExecuteCmd('upx -d QtWebKit4.dll');
  ExecuteCmd('upx -d ksp.exe');
  ShowMessage('KSP unpacked successfully');
end;

procedure TForm1.ExecuteCmd(cmd: string);
const
   READ_BYTES = 2048;

 var
   S: TStringList;
   M: TMemoryStream;
   n: LongInt;
   BytesRead: LongInt;

 begin
   // We cannot use poWaitOnExit here since we don't
   // know the size of the output. On Linux the size of the
   // output pipe is 2 kB. If the output data is more, we
   // need to read the data. This isn't possible since we are
   // waiting. So we get a deadlock here.
   //
   // A temp Memorystream is used to buffer the output

   M := TMemoryStream.Create;
   BytesRead := 0;

   Process1.CommandLine := cmd;
   Process1.Options := [poUsePipes];
   Self.Memo1.Lines.Add('-- executing --');
   Process1.Execute;
   while Process1.Running do
   begin
     // make sure we have room
     M.SetSize(BytesRead + READ_BYTES);

     // try reading it
     n := Process1.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
     Application.ProcessMessages;
     if n > 0
     then begin
       Inc(BytesRead, n);
       //Write('.')
     end
     else begin
       // no data, wait 100 ms
       Sleep(100);
     end;
   end;
   // read last part
   repeat
     // make sure we have room
     M.SetSize(BytesRead + READ_BYTES);
     // try reading it
     n := Process1.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
     Application.ProcessMessages;
     if n > 0
     then begin
       Inc(BytesRead, n);
       //Write('.');
     end;
   until n <= 0;
   if BytesRead > 0 then Self.Memo1.Lines.Add('');
   M.SetSize(BytesRead);
   Self.Memo1.Lines.Add('-- executed --');

   S := TStringList.Create;
   S.LoadFromStream(M);
   Self.Memo1.Lines.Add('-- linecount = '+ IntToStr(S.Count) + ' --');
   for n := 0 to S.Count - 1 do
   begin
     Self.Memo1.Lines.Add('| ' + S[n]);
   end;
   Self.Memo1.Lines.Add('-- end --');
   S.Free;
   M.Free;
   P1.Position:=P1.Position+1;
   Application.ProcessMessages;
 end;

initialization
  {$I main.lrs}

end.


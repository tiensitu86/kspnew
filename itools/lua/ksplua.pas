{
--------------------------------------------------------------------
Copyright (c) 2009 KSP Developers Team
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

unit ksplua;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uPSComponent, uPSCompiler, uPSRuntime, StdCtrls, Controls,
  ComCtrls, Dialogs, Forms;

procedure LuaShowMessage(Msg: string);
procedure LuaLogEntry(Msg: string);
function CreateButton(Caption: string; Workspace: TWinControl): TButton;
function CreateMemo(Caption: string; Workspace: TWinControl): TMemo;
function CreateWorkspace(name: string): TWinControl;
//function LuaLoadInterface(L: Plua_State): Integer; cdecl;

type
  TAddonManager = class
  private
    fPascal: TPSScript;
    function ceNeedFile(Sender: TObject;
      const OrginFileName: String;
      var FileName, Output: String): Boolean;
    procedure IFPS3ClassesPlugin1CompImport(Sender: TObject; x: TPSPascalCompiler);
    procedure IFPS3ClassesPlugin1ExecImport(Sender: TObject; Exec: TPSExec;
      x: TPSRuntimeClassImporter);
    procedure PSScriptCompile(Sender: TPSScript);
    procedure PSScriptExecute(Sender: TPSScript);
    procedure OutputMessages;
  public
    constructor Create(scr: string);
    destructor Destroy;
  end;

procedure SetupLua;
procedure FreeLua;

implementation

uses kspfiles, multilog, KSPConstsVars, ProfileFunc, main,
  uPSR_std,
  uPSC_std,
  uPSR_stdctrls,
  uPSC_stdctrls,
  uPSR_forms,
  uPSC_forms,
  uPSC_graphics,
  uPSC_controls,
  uPSC_classes,
  uPSR_graphics,
  uPSR_controls,
  uPSR_classes;

function TAddonManager.ceNeedFile(Sender: TObject;
  const OrginFileName: String;
  var FileName, Output: String): Boolean;
var
  path: string;
  f: TFileStream;
begin
  if FileExists(ExtractFilePath(Application.ExeName)+'addons\units\'+FileName) then
    Path := ExtractFilePath(Application.ExeName)+'addons\units\' + FileName else
    Path := ExtractFilePath(OrginFileName) + FileName;
  hLog.Send(Format('Unit %s tries to open %s', [OrginFileName, Path]));
  try
    F := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
  except
    Result := false;
    exit;
  end;
  try
    SetLength(Output, f.Size);
    f.Read(Output[1], Length(Output));
  finally
  f.Free;
  end;
  Result := True;
end;

procedure TAddonManager.IFPS3ClassesPlugin1CompImport(Sender: TObject;
  x: TIFPSPascalcompiler);
begin
  SIRegister_Std(x);
  SIRegister_Classes(x, True);
  SIRegister_Graphics(x, True);
  SIRegister_Controls(x);
  SIRegister_stdctrls(x);
  SIRegister_Forms(x);
end;

procedure TAddonManager.OutputMessages;
var
  l: longint;
  b: boolean;
begin
  b := False;

  for l := 0 to fPascal.CompilerMessageCount - 1 do
  begin
    hLog.Send('Compiler: ' + fPascal.CompilerErrorToStr(l));
    if (not b) and (fPascal.CompilerMessages[l] is TIFPSPascalCompilerError) then
    begin
      b := True;
    end;
  end;
end;

procedure TAddonManager.IFPS3ClassesPlugin1ExecImport(Sender: TObject;
  Exec: TIFPSExec; x: TIFPSRuntimeClassImporter);
begin
  RIRegister_Std(x);
  RIRegister_Classes(x, True);
  RIRegister_Graphics(x, True);
  RIRegister_Controls(x);
  RIRegister_stdctrls(x);
  RIRegister_Forms(x);
end;

constructor TAddonManager.Create(scr: string);
begin
  inherited Create;
  fPascal := TPSScript.Create(nil);
  fPascal.OnCompImport := @IFPS3ClassesPlugin1CompImport;
  fPascal.OnExecImport := @IFPS3ClassesPlugin1ExecImport;
  fPascal.OnCompile := @PSScriptCompile;
  fPascal.OnExecute := @PSScriptExecute;
  fPascal.OnNeedFile:= @ceNeedFile;
  fPascal.UsePreProcessor:=true;
  fPascal.MainFileName:=scr;
  fPascal.Script.LoadFromFile(scr);
  hLog.Send('Compiling addons...');
  if fPascal.Compile then
  begin
    OutputMessages;
    hLog.Send('Executing code...');
    if not fPascal.Execute then
    begin
      Self.OutputMessages;
      hLog.Send('Execution failed');
    end
    else
      hLog.Send('Addons code executed');
  end
  else
  begin
    Self.OutputMessages;
    hLog.Send('Compilation failed');
  end;
end;

procedure TAddonManager.PSScriptCompile(Sender: TPSScript);
begin
  Sender.AddFunction(@LuaShowMessage, 'procedure ShowMessage(s: string);');
  Sender.AddFunction(@LuaLogEntry, 'procedure AddLog(question: string);');
  Sender.AddFunction(@GetOSVersion, 'function GetOSVersion: string;');
  Sender.AddFunction(@CreateButton, 'function CreateButton(Caption: string; Workspace: TWinControl): TButton;');
  Sender.AddFunction(@CreateMemo, 'function CreateMemo(Caption: string; Workspace: TWinControl): TMemo;');
  Sender.AddFunction(@CreateWorkspace, 'function CreateWorkspace(name: string): TWinControl;');
  Sender.AddRegisteredVariable('Application', 'TApplication');
  Sender.AddRegisteredVariable('Self', 'TForm');
  Sender.AddRegisteredVariable('Basic', 'TControl');
end;

procedure TAddonManager.PSScriptExecute(Sender: TPSScript);
begin
  Sender.SetVarToInstance('SELF', KSPMainWindow);
end;

destructor TAddonManager.Destroy;
begin
  fPascal.Free;
  inherited Destroy;
end;

{function LuaLoadInterface(L: Plua_State): Integer; cdecl;
var
  p: pChar;
begin
  p:=lua_tostring(L, -1);
  hLog.SendLua('Loading addon interface: '+p);
end; }

procedure LuaShowMessage(Msg: string);
begin
  hLog.Send('MSG FROM LUA: ' + Msg);
  KSPShowMessage(Msg);
end;

procedure LuaLogEntry(Msg: string);
begin
  hLog.SendLua(Msg);
end;

function CreateButton(Caption: string; Workspace: TWinControl): TButton;
begin
  Result := TButton.Create(Workspace);
  Workspace.InsertControl(Result);
  Result.Caption := Caption;
end;

function CreateMemo(Caption: string; Workspace: TWinControl): TMemo;
begin
  Result := TMemo.Create(Workspace);
  Workspace.InsertControl(Result);
  Result.Text := Caption;
end;

function CreateWorkspace(name: string): TWinControl;
var
  t: TTabSheet;
begin
  t:=TTabSheet.Create(KSPMainWindow.AddonsControl);
  t.Caption:=name;
  t.PageControl:=KSPMainWindow.AddonsControl;
  Result:=t;
end;

procedure SetupLua;
begin
  DefaultScript := KSPDataFolder + 'addons/runaddons.pas';
  FixFolderNames(DefaultScript);
  if FileExists(DefaultScript) then
    ScriptedAddons := TAddonManager.Create(DefaultScript);
end;

procedure FreeLua;
begin
  ScriptedAddons.Free;
end;

end.

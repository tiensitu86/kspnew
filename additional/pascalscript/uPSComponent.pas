unit uPSComponent;

{$I PascalScript.inc}
interface

uses
  SysUtils, Classes, uPSRuntime, uPSDebugger, uPSUtils,
  uPSCompiler, uPSC_dll, uPSR_dll, uPSPreProcessor;

const
  {alias to @link(ifps3.cdRegister)}
  cdRegister = uPSRuntime.cdRegister;
  {alias to @link(ifps3.cdPascal)}
  cdPascal   = uPSRuntime.cdPascal;

  CdCdecl = uPSRuntime.CdCdecl;

  CdStdCall = uPSRuntime.CdStdCall;

type
  TPSScript = class;

  TDelphiCallingConvention = uPSRuntime.TPSCallingConvention;
  {Alias to @link(ifps3.TPSRuntimeClassImporter)}
  TPSRuntimeClassImporter  = uPSRuntime.TPSRuntimeClassImporter;

  TPSPlugin = class(TComponent)
  public
    procedure CompOnUses(CompExec: TPSScript); virtual;

    procedure ExecOnUses(CompExec: TPSScript); virtual;

    procedure CompileImport1(CompExec: TPSScript); virtual;

    procedure CompileImport2(CompExec: TPSScript); virtual;

    procedure ExecImport1(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); virtual;

    procedure ExecImport2(CompExec: TPSScript;
      const ri: TPSRuntimeClassImporter); virtual;
  end;

  TIFPS3Plugin = class(TPSPlugin);

  TPSDllPlugin = class(TPSPlugin)
  public
    procedure CompOnUses(CompExec: TPSScript); override;
    procedure ExecOnUses(CompExec: TPSScript); override;
  end;

  TIFPS3DllPlugin = class(TPSDllPlugin);


  TPSPluginItem = class(TCollectionItem)
  private
    FPlugin: TPSPlugin;
    procedure SetPlugin(const Value: TPSPlugin);
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override; //Birb
  published
    property Plugin: TPSPlugin Read FPlugin Write SetPlugin;
  end;


  TIFPS3CEPluginItem = class(TPSPluginItem);


  TPSPlugins = class(TCollection)
  private
    FCompExec: TPSScript;
  protected

    function GetOwner: TPersistent; override;
  public

    constructor Create(CE: TPSScript);
  end;

  TIFPS3CEPlugins = class(TPSPlugins);


  TPSOnGetNotVariant = function(Sender: TPSScript;
    const Name: tbtstring): variant of object;
  TPSOnSetNotVariant = procedure(Sender: TPSScript; const Name: tbtstring;
    V: variant) of object;
  TPSCompOptions = set of (icAllowNoBegin, icAllowUnit, icAllowNoEnd,
    icBooleanShortCircuit);

  TPSVerifyProc = procedure(Sender: TPSScript; Proc: TPSInternalProcedure;
    const Decl: tbtstring; var Error: boolean) of object;

  TPSEvent = procedure(Sender: TPSScript) of object;

  TPSOnCompImport = procedure(Sender: TObject; x: TPSPascalCompiler) of object;

  TPSOnExecImport = procedure(Sender: TObject; se: TPSExec;
    x: TPSRuntimeClassImporter) of object;
  {Script engine event function}
  TPSOnNeedFile = function(Sender: TObject; const OrginFileName: tbtstring;
    var FileName, Output: tbtstring): boolean of object;

  TPSOnProcessDirective = procedure(Sender: TPSPreProcessor;
    Parser: TPSPascalPreProcessorParser;
    const Active: boolean;
    const DirectiveName, DirectiveParam: tbtstring;
    var Continue: boolean) of object;  // jgv

  TPSScript = class(TComponent)
  private
    FOnGetNotificationVariant: TPSOnGetNotVariant;
    FOnSetNotificationVariant: TPSOnSetNotVariant;
    FCanAdd: boolean;
    FComp:   TPSPascalCompiler;
    FCompOptions: TPSCompOptions;
    FExec:   TPSDebugExec;
    FSuppressLoadData: boolean;
    FScript: TStrings;
    FOnLine: TNotifyEvent;
    FUseDebugInfo: boolean;
    FOnAfterExecute, FOnCompile, FOnExecute: TPSEvent;
    FOnCompImport: TPSOnCompImport;
    FOnExecImport: TPSOnExecImport;
    RI:      TPSRuntimeClassImporter;
    FPlugins: TPSPlugins;
    FPP:     TPSPreProcessor;
    FMainFileName: tbtstring;
    FOnNeedFile: TPSOnNeedFile;
    FUsePreProcessor: boolean;
    FDefines: TStrings;
    FOnVerifyProc: TPSVerifyProc;
    FOnProcessDirective: TPSOnProcessDirective;
    FOnProcessUnknowDirective: TPSOnProcessDirective;
    FOnFindUnknownFile: TPSOnNeedFile;
    function GetRunning: boolean;
    procedure SetScript(const Value: TStrings);
    function GetCompMsg(i: integer): TPSPascalCompilerMessage;
    function GetCompMsgCount: longint;
    function GetAbout: tbtstring;
    function ScriptUses(Sender: TPSPascalCompiler; const Name: tbtstring): boolean;
    function GetExecErrorByteCodePosition: cardinal;
    function GetExecErrorCode: TIFError;
    function GetExecErrorParam: tbtstring;
    function GetExecErrorProcNo: cardinal;
    function GetExecErrorString: tbtstring;
    function GetExecErrorPosition: cardinal;
    function GetExecErrorCol: cardinal;
    function GetExecErrorRow: cardinal;
    function GetExecErrorFileName: tbtstring;
    procedure SetDefines(const Value: TStrings);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  protected
    //jgv move where private before - not very usefull
    procedure OnLineEvent; virtual;
    procedure SetMainFileName(const Value: tbtstring); virtual;

    //--jgv new
    function DoOnNeedFile(Sender: TObject; const OrginFileName: tbtstring;
      var FileName, Output: tbtstring): boolean; virtual;
    function DoOnUnknowUses(Sender: TPSPascalCompiler; const Name: tbtstring): boolean;
      virtual; // return true if processed
    procedure DoOnCompImport; virtual;
    procedure DoOnCompile; virtual;
    function DoVerifyProc(Sender: TPSScript; Proc: TPSInternalProcedure;
      const Decl: tbtstring): boolean; virtual;

    procedure DoOnExecImport(RunTimeImporter: TPSRuntimeClassImporter); virtual;
    procedure DoOnExecute(RunTimeImporter: TPSRuntimeClassImporter); virtual;
    procedure DoAfterExecute; virtual;
    function DoOnGetNotificationVariant(const Name: tbtstring): variant; virtual;
    procedure DoOnSetNotificationVariant(const Name: tbtstring; V: variant); virtual;

    procedure DoOnProcessDirective(Sender: TPSPreProcessor;
      Parser: TPSPascalPreProcessorParser;
      const Active: boolean;
      const DirectiveName, DirectiveParam: tbtstring;
      var Continue: boolean); virtual;
    procedure DoOnProcessUnknowDirective(Sender: TPSPreProcessor;
      Parser: TPSPascalPreProcessorParser;
      const Active: boolean;
      const DirectiveName, DirectiveParam: tbtstring;
      var Continue: boolean); virtual;
  public
    property RuntimeImporter: TPSRuntimeClassImporter Read RI;

    function FindNamedType(const Name: tbtstring): TPSTypeRec;

    function FindBaseType(Bt: TPSBaseType): TPSTypeRec;

    property SuppressLoadData: boolean Read FSuppressLoadData Write FSuppressLoadData;

    function LoadExec: boolean;

    procedure Stop; virtual;

    constructor Create(AOwner: TComponent); override;

    destructor Destroy; override;

    function Compile: boolean; virtual;

    function Execute: boolean; virtual;

    property Running: boolean Read GetRunning;

    procedure GetCompiled(var Data: tbtstring);

    procedure SetCompiled(const Data: tbtstring);

    property comp: TPSPascalCompiler Read FComp;

    property Exec: TPSDebugExec Read FExec;

    property CompilerMessageCount: longint Read GetCompMsgCount;

    property CompilerMessages[i: longint]: TPSPascalCompilerMessage Read GetCompMsg;

    function CompilerErrorToStr(I: longint): tbtstring;

    property ExecErrorCode: TIFError Read GetExecErrorCode;

    property ExecErrorParam: tbtstring Read GetExecErrorParam;

    property ExecErrorToString: tbtstring Read GetExecErrorString;

    property ExecErrorProcNo: cardinal Read GetExecErrorProcNo;

    property ExecErrorByteCodePosition: cardinal Read GetExecErrorByteCodePosition;

    property ExecErrorPosition: cardinal Read GetExecErrorPosition;

    property ExecErrorRow: cardinal Read GetExecErrorRow;

    property ExecErrorCol: cardinal Read GetExecErrorCol;

    property ExecErrorFileName: tbtstring Read GetExecErrorFileName;

    function AddFunctionEx(Ptr: Pointer; const Decl: tbtstring;
      CallingConv: TDelphiCallingConvention): boolean;

    function AddFunction(Ptr: Pointer; const Decl: tbtstring): boolean;


    function AddMethodEx(Slf, Ptr: Pointer; const Decl: tbtstring;
      CallingConv: TDelphiCallingConvention): boolean;

    function AddMethod(Slf, Ptr: Pointer; const Decl: tbtstring): boolean;

    function AddRegisteredVariable(const VarName, VarType: tbtstring): boolean;
    function AddNotificationVariant(const VarName: tbtstring): boolean;

    function AddRegisteredPTRVariable(const VarName, VarType: tbtstring): boolean;

    function GetVariable(const Name: tbtstring): PIFVariant;

    function SetVarToInstance(const VarName: tbtstring; cl: TObject): boolean;

    procedure SetPointerToData(const VarName: tbtstring; Data: Pointer;
      aType: TIFTypeRec);

    function TranslatePositionPos(Proc, Position: cardinal; var Pos: cardinal;
      var fn: tbtstring): boolean;

    function TranslatePositionRC(Proc, Position: cardinal; var Row, Col: cardinal;
      var fn: tbtstring): boolean;

    function GetProcMethod(const ProcName: tbtstring): TMethod;

    function ExecuteFunction(const Params: array of variant;
      const ProcName: tbtstring): variant;
  published

    property About: tbtstring Read GetAbout stored False;

    property Script: TStrings Read FScript Write SetScript;

    property CompilerOptions: TPSCompOptions Read FCompOptions Write FCompOptions;

    property OnLine: TNotifyEvent Read FOnLine Write FOnLine;

    property OnCompile: TPSEvent Read FOnCompile Write FOnCompile;

    property OnExecute: TPSEvent Read FOnExecute Write FOnExecute;

    property OnAfterExecute: TPSEvent Read FOnAfterExecute Write FOnAfterExecute;

    property OnCompImport: TPSOnCompImport Read FOnCompImport Write FOnCompImport;

    property OnExecImport: TPSOnExecImport Read FOnExecImport Write FOnExecImport;

    property UseDebugInfo: boolean Read FUseDebugInfo Write FUseDebugInfo default True;

    property Plugins: TPSPlugins Read FPlugins Write FPlugins;

    property MainFileName: tbtstring Read FMainFileName Write SetMainFileName;

    property UsePreProcessor: boolean Read FUsePreProcessor Write FUsePreProcessor;

    property OnNeedFile: TPSOnNeedFile Read FOnNeedFile Write FOnNeedFile;

    property Defines: TStrings Read FDefines Write SetDefines;

    property OnVerifyProc: TPSVerifyProc Read FOnVerifyProc Write FOnVerifyProc;
    property OnGetNotificationVariant: TPSOnGetNotVariant
      Read FOnGetNotificationVariant Write FOnGetNotificationVariant;
    property OnSetNotificationVariant: TPSOnSetNotVariant
      Read FOnSetNotificationVariant Write FOnSetNotificationVariant;
    property OnFindUnknownFile: TPSOnNeedFile
      Read FOnFindUnknownFile Write FOnFindUnknownFile;

  published
    //-- jgv
    property OnProcessDirective: TPSOnProcessDirective
      Read FOnProcessDirective Write FOnProcessDirective;
    property OnProcessUnknowDirective: TPSOnProcessDirective
      Read FOnProcessUnknowDirective Write FOnProcessUnknowDirective;
  end;

  TIFPS3CompExec = class(TPSScript);


  TPSBreakPointInfo = class
  private
    FLine:     longint;
    FFileNameHash: longint;
    FFileName: tbtstring;
    procedure SetFileName(const Value: tbtstring);
  public

    property FileName: tbtstring Read FFileName Write SetFileName;

    property FileNameHash: longint Read FFileNameHash;

    property Line: longint Read FLine Write FLine;
  end;

  TPSOnLineInfo = procedure(Sender: TObject; const FileName: tbtstring;
    Position, Row, Col: cardinal) of object;

  TPSScriptDebugger = class(TPSScript)
  private
    FOnIdle:      TNotifyEvent;
    FBreakPoints: TIFList;
    FOnLineInfo:  TPSOnLineInfo;
    FLastRow:     cardinal;
    FOnBreakpoint: TPSOnLineInfo;
    function GetBreakPoint(I: integer): TPSBreakPointInfo;
    function GetBreakPointCount: longint;
  protected
    procedure SetMainFileName(const Value: tbtstring); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;


    procedure Pause; virtual;

    procedure Resume; virtual;


    procedure StepInto; virtual;

    procedure StepOver; virtual;

    procedure SetBreakPoint(const Fn: tbtstring; Line: longint);

    procedure ClearBreakPoint(const Fn: tbtstring; Line: longint);

    property BreakPointCount: longint Read GetBreakPointCount;

    property BreakPoint[I: longint]: TPSBreakPointInfo Read GetBreakPoint;

    function HasBreakPoint(const Fn: tbtstring; Line: longint): boolean;

    procedure ClearBreakPoints;

    function GetVarContents(const Name: tbtstring): tbtstring;
  published

    property OnIdle: TNotifyEvent Read FOnIdle Write FOnIdle;

    property OnLineInfo: TPSOnLineInfo Read FOnLineInfo Write FOnLineInfo;

    property OnBreakpoint: TPSOnLineInfo Read FOnBreakpoint Write FOnBreakpoint;
  end;

  TIFPS3DebugCompExec = class(TPSScriptDebugger);

  TPSCustumPlugin = class(TPSPlugin)
  private
    FOnCompileImport2: TPSEvent;
    FOnExecOnUses:     TPSEvent;
    FOnCompOnUses:     TPSEvent;
    FOnCompileImport1: TPSEvent;
    FOnExecImport1:    TPSOnExecImport;
    FOnExecImport2:    TPSOnExecImport;
  public
    procedure CompOnUses(CompExec: TPSScript); override;
    procedure ExecOnUses(CompExec: TPSScript); override;
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure CompileImport2(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
      override;

    procedure ExecImport2(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
      override;
  public
  published
    property OnCompOnUses: TPSEvent Read FOnCompOnUses Write FOnCompOnUses;
    property OnExecOnUses: TPSEvent Read FOnExecOnUses Write FOnExecOnUses;
    property OnCompileImport1: TPSEvent Read FOnCompileImport1 Write FOnCompileImport1;
    property OnCompileImport2: TPSEvent Read FOnCompileImport2 Write FOnCompileImport2;
    property OnExecImport1: TPSOnExecImport Read FOnExecImport1 Write FOnExecImport1;
    property OnExecImport2: TPSOnExecImport Read FOnExecImport2 Write FOnExecImport2;
  end;

implementation


{$IFDEF DELPHI3UP }
resourcestring
{$ELSE }
const
{$ENDIF }

  RPS_UnableToReadVariant = 'Unable to read variant';
  RPS_UnableToWriteVariant = 'Unable to write variant';
  RPS_ScripEngineAlreadyRunning = 'Script engine already running';
  RPS_ScriptNotCompiled = 'Script is not compiled';
  RPS_NotRunning = 'Not running';
  RPS_UnableToFindVariable = 'Unable to find variable';
  RPS_UnknownIdentifier = 'Unknown Identifier';
  RPS_NoScript   = 'No script';

function MyGetVariant(Sender: TPSExec; const Name: tbtstring): variant;
begin
  Result := TPSScript(Sender.Id).DoOnGetNotificationVariant(Name);
end;

procedure MySetVariant(Sender: TPSExec; const Name: tbtstring; V: variant);
begin
  TPSScript(Sender.Id).DoOnSetNotificationVariant(Name, V);
end;

function CompScriptUses(Sender: TPSPascalCompiler; const Name: tbtstring): boolean;
begin
  Result := TPSScript(Sender.ID).ScriptUses(Sender, Name);
end;

procedure ExecOnLine(Sender: TPSExec);
begin
  if assigned(TPSScript(Sender.ID).FOnLine) then
  begin
    TPSScript(Sender.ID).OnLineEvent;
  end;
end;

function CompExportCheck(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure;
  const ProcDecl: tbtstring): boolean;
begin
  Result := TPSScript(Sender.ID).DoVerifyProc(Sender.ID, Proc, ProcDecl);
end;


procedure callObjectOnProcessDirective(Sender: TPSPreProcessor;
  Parser: TPSPascalPreProcessorParser; const Active: boolean;
  const DirectiveName, DirectiveParam: tbtstring; var Continue: boolean);
begin
  TPSScript(Sender.ID).DoOnProcessUnknowDirective(Sender, Parser,
    Active, DirectiveName, DirectiveParam, Continue);
end;

procedure callObjectOnProcessUnknowDirective(Sender: TPSPreProcessor;
  Parser: TPSPascalPreProcessorParser; const Active: boolean;
  const DirectiveName, DirectiveParam: tbtstring; var Continue: boolean);
begin
  TPSScript(Sender.ID).DoOnProcessDirective(Sender, Parser, Active,
    DirectiveName, DirectiveParam, Continue);
end;


{ TPSPlugin }
procedure TPSPlugin.CompileImport1(CompExec: TPSScript);
begin
  // do nothing
end;

procedure TPSPlugin.CompileImport2(CompExec: TPSScript);
begin
  // do nothing
end;

procedure TPSPlugin.CompOnUses(CompExec: TPSScript);
begin
  // do nothing
end;

procedure TPSPlugin.ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
  // do nothing
end;

procedure TPSPlugin.ExecImport2(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
  // do nothing
end;

procedure TPSPlugin.ExecOnUses(CompExec: TPSScript);
begin
  // do nothing
end;


{ TPSScript }

function TPSScript.AddFunction(Ptr: Pointer; const Decl: tbtstring): boolean;
begin
  Result := AddFunctionEx(Ptr, Decl, cdRegister);
end;

function TPSScript.AddFunctionEx(Ptr: Pointer; const Decl: tbtstring;
  CallingConv: TDelphiCallingConvention): boolean;
var
  P: TPSRegProc;
begin
  if not FCanAdd then
  begin
    Result := False;
    exit;
  end;
  p := comp.AddDelphiFunction(Decl);
  if p <> nil then
  begin
    Exec.RegisterDelphiFunction(Ptr, p.Name, CallingConv);
    Result := True;
  end
  else
    Result := False;
end;

function TPSScript.AddRegisteredVariable(const VarName, VarType: tbtstring): boolean;
var
  FVar: TPSVar;
begin
  if not FCanAdd then
  begin
    Result := False;
    exit;
  end;
  FVar := FComp.AddUsedVariableN(varname, vartype);
  if fvar = nil then
    Result := False
  else
  begin
    fvar.exportname := fvar.Name;
    Result := True;
  end;
end;

function CENeedFile(Sender: TPSPreProcessor; const callingfilename: tbtstring;
  var FileName, Output: tbtstring): boolean;
begin
  Result := TPSScript(Sender.ID).DoOnNeedFile(Sender.ID, CallingFileName,
    FileName, Output);
end;

procedure CompTranslateLineInfo(Sender: TPSPascalCompiler;
  var Pos, Row, Col: cardinal; var Name: tbtstring);
var
  res: TPSLineInfoResults;
begin
  if TPSScript(Sender.ID).FPP.CurrentLineInfo.GetLineInfo(Name, Pos, Res) then
  begin
    Pos  := Res.Pos;
    Row  := Res.Row;
    Col  := Res.Col;
    Name := Res.Name;
  end;
end;

function TPSScript.Compile: boolean;
var
  i:   longint;
  dta: tbtstring;
begin
  FExec.Clear;
  FExec.CMD_Err(erNoError);
  FExec.ClearspecialProcImports;
  FExec.ClearFunctionList;
  if ri <> nil then
  begin
    RI.Free;
    RI := nil;
  end;
  RI := TPSRuntimeClassImporter.Create;
  for i := 0 to FPlugins.Count - 1 do
  begin
    if (TPSPluginItem(FPlugins.Items[i]) <> nil) and
      (TPSPluginItem(FPlugins.Items[i]).Plugin <> nil) then
      TPSPluginItem(FPlugins.Items[i]).Plugin.ExecImport1(Self, ri);
  end;

  DoOnExecImport(RI);

  for i := 0 to FPlugins.Count - 1 do
  begin
    if (TPSPluginItem(FPlugins.Items[i]) <> nil) and
      (TPSPluginItem(FPlugins.Items[i]).Plugin <> nil) then
      TPSPluginItem(FPlugins.Items[i]).Plugin.ExecImport2(Self, ri);
  end;
  RegisterClassLibraryRuntime(Exec, RI);
  for i := 0 to FPlugins.Count - 1 do
  begin
    if (TPSPluginItem(FPlugins.Items[i]) <> nil) and
      (TPSPluginItem(FPlugins.Items[i]).Plugin <> nil) then
      TPSPluginItem(FPlugins.Items[i]).Plugin.ExecOnUses(Self);
  end;
  FCanAdd := True;
  FComp.BooleanShortCircuit := icBooleanShortCircuit in FCompOptions;
  FComp.AllowNoBegin := icAllowNoBegin in FCompOptions;
  FComp.AllowUnit := icAllowUnit in FCompOptions;
  FComp.AllowNoEnd := icAllowNoEnd in FCompOptions;
  if FUsePreProcessor then
  begin
    FPP.Clear;
    FPP.Defines.Assign(FDefines);
    FComp.OnTranslateLineInfo := CompTranslateLineInfo;
    Fpp.OnProcessDirective := callObjectOnProcessDirective;
    Fpp.OnProcessUnknowDirective := callObjectOnProcessUnknowDirective;
    Fpp.MainFile     := FScript.Text;
    Fpp.MainFileName := FMainFileName;
    Fpp.PreProcess(FMainFileName, dta);
    if FComp.Compile(dta) then
    begin
      FCanAdd := False;
      if (not SuppressLoadData) and (not LoadExec) then
      begin
        Result := False;
      end
      else
        Result := True;
    end
    else
      Result := False;
    Fpp.AdjustMessages(comp);
  end
  else
  begin
    FComp.OnTranslateLineInfo := nil;
    if FComp.Compile(FScript.Text) then
    begin
      FCanAdd := False;
      if not LoadExec then
      begin
        Result := False;
      end
      else
        Result := True;
    end
    else
      Result := False;
  end;
end;

function TPSScript.CompilerErrorToStr(I: integer): tbtstring;
begin
  Result := CompilerMessages[i].MessageToString;
end;

constructor TPSScript.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComp    := TPSPascalCompiler.Create;
  FExec    := TPSDebugExec.Create;
  FScript  := TStringList.Create;
  FPlugins := TPSPlugins.Create(self);

  FComp.ID     := Self;
  FComp.OnUses := CompScriptUses;
  FComp.OnExportCheck := CompExportCheck;
  FExec.Id     := Self;
  FExec.OnRunLine := ExecOnLine;
  FExec.OnGetNVariant := MyGetVariant;
  FExec.OnSetNVariant := MySetVariant;

  FUseDebugInfo := True;

  FPP    := TPSPreProcessor.Create;
  FPP.Id := Self;
  FPP.OnNeedFile := CENeedFile;

  FDefines := TStringList.Create;
end;

destructor TPSScript.Destroy;
begin
  FDefines.Free;

  FPP.Free;
  RI.Free;
  FPlugins.Free;
  FPlugins := nil;
  FScript.Free;
  FExec.Free;
  FComp.Free;
  inherited Destroy;
end;

function TPSScript.Execute: boolean;
begin
  if Running then
    raise Exception.Create(RPS_ScripEngineAlreadyRunning);
  if SuppressLoadData then
    LoadExec;

  DoOnExecute(RI);

  FExec.DebugEnabled := FUseDebugInfo;
  Result := FExec.RunScript and (FExec.ExceptionCode = erNoError);

  DoAfterExecute;
end;

function TPSScript.GetAbout: tbtstring;
begin
  Result := TPSExec.About;
end;

procedure TPSScript.GetCompiled(var Data: tbtstring);
begin
  if not FComp.GetOutput(Data) then
    raise Exception.Create(RPS_ScriptNotCompiled);
end;

function TPSScript.GetCompMsg(i: integer): TPSPascalCompilerMessage;
begin
  Result := FComp.Msg[i];
end;

function TPSScript.GetCompMsgCount: longint;
begin
  Result := FComp.MsgCount;
end;

function TPSScript.GetExecErrorByteCodePosition: cardinal;
begin
  Result := Exec.ExceptionPos;
end;

function TPSScript.GetExecErrorCode: TIFError;
begin
  Result := Exec.ExceptionCode;
end;

function TPSScript.GetExecErrorParam: tbtstring;
begin
  Result := Exec.ExceptionString;
end;

function TPSScript.GetExecErrorPosition: cardinal;
begin
  Result := FExec.TranslatePosition(Exec.ExceptionProcNo, Exec.ExceptionPos);
end;

function TPSScript.GetExecErrorProcNo: cardinal;
begin
  Result := Exec.ExceptionProcNo;
end;

function TPSScript.GetExecErrorString: tbtstring;
begin
  Result := TIFErrorToString(Exec.ExceptionCode, Exec.ExceptionString);
end;

function TPSScript.GetVariable(const Name: tbtstring): PIFVariant;
begin
  Result := FExec.GetVar2(Name);
end;

function TPSScript.LoadExec: boolean;
var
  s: tbtstring;
begin
  if (not FComp.GetOutput(s)) or (not FExec.LoadData(s)) then
  begin
    Result := False;
    exit;
  end;
  if FUseDebugInfo then
  begin
    FComp.GetDebugOutput(s);
    FExec.LoadDebugData(s);
  end;
  Result := True;
end;

function TPSScript.ScriptUses(Sender: TPSPascalCompiler;
  const Name: tbtstring): boolean;
var
  i: longint;
begin
  if Name = 'SYSTEM' then
  begin
    for i := 0 to FPlugins.Count - 1 do
    begin
      if (TPSPluginItem(FPlugins.Items[i]) <> nil) and
        (TPSPluginItem(FPlugins.Items[i]).Plugin <> nil) then
        TPSPluginItem(FPlugins.Items[i]).Plugin.CompOnUses(Self);
    end;
    for i := 0 to FPlugins.Count - 1 do
    begin
      if (TPSPluginItem(FPlugins.Items[i]) <> nil) and
        (TPSPluginItem(FPlugins.Items[i]).Plugin <> nil) then
        TPSPluginItem(FPlugins.Items[i]).Plugin.CompileImport1(self);
    end;

    DoOnCompImport;

    for i := 0 to FPlugins.Count - 1 do
    begin
      if (TPSPluginItem(FPlugins.Items[i]) <> nil) and
        (TPSPluginItem(FPlugins.Items[i]).Plugin <> nil) then
        TPSPluginItem(FPlugins.Items[i]).Plugin.CompileImport2(Self);
    end;

    DoOnCompile;

    Result := True;
    for i := 0 to Sender.MsgCount - 1 do
    begin
      if Sender.Msg[i] is TPSPascalCompilerError then
        Result := False;
    end;
  end
  else
  begin
    Result := DoOnUnknowUses(Sender, Name);
{    If Not Result then
      Sender.MakeError('', ecUnknownIdentifier, Name);}
  end;
end;

procedure TPSScript.SetCompiled(const Data: tbtstring);
var
  i: integer;
begin
  FExec.Clear;
  FExec.ClearspecialProcImports;
  FExec.ClearFunctionList;
  if ri <> nil then
  begin
    RI.Free;
    RI := nil;
  end;
  RI := TPSRuntimeClassImporter.Create;
  for i := 0 to FPlugins.Count - 1 do
  begin
    if (TPSPluginItem(FPlugins.Items[i]) <> nil) and
      (TPSPluginItem(FPlugins.Items[i]).Plugin <> nil) then
      TPSPluginItem(FPlugins.Items[i]).Plugin.ExecImport1(Self, ri);
  end;

  DoOnExecImport(RI);

  for i := 0 to FPlugins.Count - 1 do
  begin
    if (TPSPluginItem(FPlugins.Items[i]) <> nil) and
      (TPSPluginItem(FPlugins.Items[i]).Plugin <> nil) then
      TPSPluginItem(FPlugins.Items[i]).Plugin.ExecImport2(Self, ri);
  end;
  RegisterClassLibraryRuntime(Exec, RI);
  for i := 0 to FPlugins.Count - 1 do
  begin
    if (TPSPluginItem(FPlugins.Items[i]) <> nil) and
      (TPSPluginItem(FPlugins.Items[i]).Plugin <> nil) then
      TPSPluginItem(FPlugins.Items[i]).Plugin.ExecOnUses(Self);
  end;
  if not FExec.LoadData(Data) then
    raise Exception.Create(GetExecErrorString);
end;

function TPSScript.SetVarToInstance(const VarName: tbtstring; cl: TObject): boolean;
var
  p: PIFVariant;
begin
  p := GetVariable(VarName);
  if p <> nil then
  begin
    SetVariantToClass(p, cl);
    Result := True;
  end
  else
    Result := False;
end;

procedure TPSScript.SetScript(const Value: TStrings);
begin
  FScript.Assign(Value);
end;


function TPSScript.AddMethod(Slf, Ptr: Pointer; const Decl: tbtstring): boolean;
begin
  Result := AddMethodEx(Slf, Ptr, Decl, cdRegister);
end;

function TPSScript.AddMethodEx(Slf, Ptr: Pointer; const Decl: tbtstring;
  CallingConv: TDelphiCallingConvention): boolean;
var
  P: TPSRegProc;
begin
  if not FCanAdd then
  begin
    Result := False;
    exit;
  end;
  p := comp.AddDelphiFunction(Decl);
  if p <> nil then
  begin
    Exec.RegisterDelphiMethod(Slf, Ptr, p.Name, CallingConv);
    Result := True;
  end
  else
    Result := False;
end;

procedure TPSScript.OnLineEvent;
begin
  if @FOnLine <> nil then
    FOnLine(Self);
end;

function TPSScript.GetRunning: boolean;
begin
  Result := FExec.Status = isRunning;
end;

function TPSScript.GetExecErrorCol: cardinal;
var
  s:  tbtstring;
  D1: cardinal;
begin
  if not TranslatePositionRC(Exec.ExceptionProcNo, Exec.ExceptionPos, D1, Result, s) then
    Result := 0;
end;

function TPSScript.TranslatePositionPos(Proc, Position: cardinal;
  var Pos: cardinal; var fn: tbtstring): boolean;
var
  D1, D2: cardinal;
begin
  Result := Exec.TranslatePositionEx(Exec.ExceptionProcNo, Exec.ExceptionPos,
    Pos, D1, D2, fn);
end;

function TPSScript.TranslatePositionRC(Proc, Position: cardinal;
  var Row, Col: cardinal; var fn: tbtstring): boolean;
var
  d1: cardinal;
begin
  Result := Exec.TranslatePositionEx(Proc, Position, d1, Row, Col, fn);
end;


function TPSScript.GetExecErrorRow: cardinal;
var
  D1: cardinal;
  s:  tbtstring;
begin
  if not TranslatePositionRC(Exec.ExceptionProcNo, Exec.ExceptionPos, Result, D1, s) then
    Result := 0;
end;

procedure TPSScript.Stop;
begin
  if (FExec.Status = isRunning) or (Fexec.Status = isPaused) then
    FExec.Stop
  else
    raise Exception.Create(RPS_NotRunning);
end;

function TPSScript.GetProcMethod(const ProcName: tbtstring): TMethod;
begin
  Result := FExec.GetProcAsMethodN(ProcName);
end;

procedure TPSScript.SetMainFileName(const Value: tbtstring);
begin
  FMainFileName := Value;
end;

function TPSScript.GetExecErrorFileName: tbtstring;
var
  D1, D2: cardinal;
begin
  if not TranslatePositionRC(Exec.ExceptionProcNo, Exec.ExceptionPos,
    D1, D2, Result) then
    Result := '';
end;

procedure TPSScript.SetPointerToData(const VarName: tbtstring;
  Data: Pointer; aType: TIFTypeRec);
var
  v: PIFVariant;
  t: TPSVariantIFC;
begin
  v := GetVariable(VarName);
  if (Atype = nil) or (v = nil) then
    raise Exception.Create(RPS_UnableToFindVariable);
  t.Dta      := @PPSVariantData(v).Data;
  t.aType    := v.FType;
  t.VarParam := False;
  VNSetPointerTo(t, Data, aType);
end;

function TPSScript.AddRegisteredPTRVariable(const VarName, VarType: tbtstring): boolean;
var
  FVar: TPSVar;
begin
  if not FCanAdd then
  begin
    Result := False;
    exit;
  end;
  FVar := FComp.AddUsedVariableN(varname, vartype);
  if fvar = nil then
    Result := False
  else
  begin
    fvar.exportname := fvar.Name;
    fvar.SaveAsPointer := True;
    Result := True;
  end;
end;

procedure TPSScript.SetDefines(const Value: TStrings);
begin
  FDefines.Assign(Value);
end;

function TPSScript.ExecuteFunction(const Params: array of variant;
  const ProcName: tbtstring): variant;
begin
  if SuppressLoadData then
    LoadExec;

  DoOnExecute(RI);

  FExec.DebugEnabled := FUseDebugInfo;

  Result := Exec.RunProcPN(Params, ProcName);

  DoAfterExecute;
end;

function TPSScript.FindBaseType(Bt: TPSBaseType): TPSTypeRec;
begin
  Result := Exec.FindType2(Bt);
end;

function TPSScript.FindNamedType(const Name: tbtstring): TPSTypeRec;
begin
  Result := Exec.GetTypeNo(Exec.GetType(Name));
end;

procedure TPSScript.Notification(AComponent: TComponent; Operation: TOperation);
var
  i: longint;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (aComponent is TPSPlugin) then
  begin
    for i := Plugins.Count - 1 downto 0 do
    begin
      if (Plugins.Items[i] as TPSPluginItem).Plugin = aComponent then
        {$IFDEF FPC_COL_NODELETE}
        TCollectionItem(Plugins.Items[i]).Free;
        {$ELSE}
      Plugins.Delete(i);
        {$ENDIF}
    end;
  end;
end;

function TPSScript.AddNotificationVariant(const VarName: tbtstring): boolean;
begin
  Result := AddRegisteredVariable(VarName, '!NOTIFICATIONVARIANT');
end;

procedure TPSScript.DoOnProcessDirective(Sender: TPSPreProcessor;
  Parser: TPSPascalPreProcessorParser; const Active: boolean;
  const DirectiveName, DirectiveParam: tbtstring; var Continue: boolean);
begin
  if Assigned(OnProcessDirective) then
    OnProcessDirective(Sender, Parser, Active, DirectiveName, DirectiveParam, Continue);
end;

procedure TPSScript.DoOnProcessUnknowDirective(Sender: TPSPreProcessor;
  Parser: TPSPascalPreProcessorParser; const Active: boolean;
  const DirectiveName, DirectiveParam: tbtstring; var Continue: boolean);
begin
  if Assigned(OnProcessUnknowDirective) then
    OnProcessUnknowDirective(Sender, Parser, Active, DirectiveName,
      DirectiveParam, Continue);
end;

function TPSScript.DoOnNeedFile(Sender: TObject; const OrginFileName: tbtstring;
  var FileName, Output: tbtstring): boolean;
begin
  if Assigned(OnNeedFile) then
    Result := OnNeedFile(Sender, OrginFileName, FileName, Output)
  else
    Result := False;
end;

function TPSScript.DoOnUnknowUses(Sender: TPSPascalCompiler;
  const Name: tbtstring): boolean;
var
  lPrevAllowUnit: boolean;
  lData, lName:   tbtstring;
begin
  if assigned(FOnFindUnknownFile) then
  begin
    lName := Name;
    if FOnFindUnknownFile(self, '', lName, lData) then
    begin
      lPrevAllowUnit  := FComp.AllowUnit;
      FComp.AllowUnit := True;
      if FUsePreProcessor then
      begin
        FPP.Defines.Assign(FDefines);
        Fpp.MainFile     := lData;
        Fpp.MainFileName := lName;
        Fpp.PreProcess(lName, lData);
        Result := FComp.Compile(lData);
        Fpp.AdjustMessages(FComp);
      end
      else
      begin
        FComp.OnTranslateLineInfo := nil;
        Result := FComp.Compile(lData);
      end;
      FComp.AllowUnit := lPrevAllowUnit;
    end
    else
    begin
      FComp.MakeError(FComp.UnitName, ecUnknownIdentifier, lName);
      Result := False;
    end;
  end
  else
  begin
    FComp.MakeError(FComp.UnitName, ecUnknownIdentifier, lName);
    Result := False;
  end;
end;

procedure TPSScript.DoOnCompImport;
begin
  if assigned(OnCompImport) then
    OnCompImport(Self, comp);
end;

procedure TPSScript.DoOnCompile;
begin
  if assigned(OnCompile) then
    OnCompile(Self);
end;

procedure TPSScript.DoOnExecute;
begin
  if Assigned(OnExecute) then
    OnExecute(Self);
end;

procedure TPSScript.DoAfterExecute;
begin
  if Assigned(OnAfterExecute) then
    OnAfterExecute(Self);
end;

function TPSScript.DoVerifyProc(Sender: TPSScript; Proc: TPSInternalProcedure;
  const Decl: tbtstring): boolean;
begin
  if Assigned(OnVerifyProc) then
  begin
    Result := False;
    OnVerifyProc(Sender, Proc, Decl, Result);
    Result := not Result;
  end
  else
    Result := True;
end;

procedure TPSScript.DoOnExecImport(RunTimeImporter: TPSRuntimeClassImporter);
begin
  if assigned(OnExecImport) then
    OnExecImport(Self, FExec, RunTimeImporter);
end;

function TPSScript.DoOnGetNotificationVariant(const Name: tbtstring): variant;
begin
  if not Assigned(OnGetNotificationVariant) then
    raise Exception.Create(RPS_UnableToReadVariant);
  Result := OnGetNotificationVariant(Self, Name);
end;

procedure TPSScript.DoOnSetNotificationVariant(const Name: tbtstring; V: variant);
begin
  if not Assigned(OnSetNotificationVariant) then
    raise Exception.Create(RPS_UnableToWriteVariant);
  OnSetNotificationVariant(Self, Name, v);
end;

{ TPSDllPlugin }

procedure TPSDllPlugin.CompOnUses;
begin
  CompExec.comp.OnExternalProc := DllExternalProc;
end;

procedure TPSDllPlugin.ExecOnUses;
begin
  RegisterDLLRuntime(CompExec.Exec);
end;



{ TPS3DebugCompExec }

procedure LineInfo(Sender: TPSDebugExec; const FileName: tbtstring;
  Position, Row, Col: cardinal);
var
  Dc:   TPSScriptDebugger;
  h, i: longint;
  bi:   TPSBreakPointInfo;
  lFileName: tbtstring;
begin
  Dc := Sender.Id;
  if FileName = '' then
    lFileName := dc.MainFileName
  else
    lFileName := FileName;

  if @dc.FOnLineInfo <> nil then
    dc.FOnLineInfo(dc, lFileName, Position, Row, Col);
  if row = dc.FLastRow then
    exit;
  dc.FLastRow := row;
  h  := MakeHash(lFileName);
  bi := nil;
  for i := DC.FBreakPoints.Count - 1 downto 0 do
  begin
    bi := Dc.FBreakpoints[i];
    if (h = bi.FileNameHash) and (lFileName = bi.FileName) and
      (cardinal(bi.Line) = Row) then
    begin
      Break;
    end;
    Bi := nil;
  end;
  if bi <> nil then
  begin
    if @dc.FOnBreakpoint <> nil then
      dc.FOnBreakpoint(dc, lFileName, Position, Row, Col);
    dc.Pause;
  end;
end;

procedure IdleCall(Sender: TPSDebugExec);
var
  Dc: TPSScriptDebugger;
begin
  Dc := Sender.Id;
  if @dc.FOnIdle <> nil then
    dc.FOnIdle(DC)
  else
    dc.Exec.Run;
end;

procedure TPSScriptDebugger.ClearBreakPoint(const Fn: tbtstring; Line: integer);
var
  h, i: longint;
  bi:   TPSBreakPointInfo;
begin
  h := MakeHash(Fn);
  for i := FBreakPoints.Count - 1 downto 0 do
  begin
    bi := FBreakpoints[i];
    if (h = bi.FileNameHash) and (Fn = bi.FileName) and (bi.Line = Line) then
    begin
      FBreakPoints.Delete(i);
      bi.Free;
      Break;
    end;
  end;
end;

procedure TPSScriptDebugger.ClearBreakPoints;
var
  i: longint;
begin
  for i := FBreakPoints.Count - 1 downto 0 do
    TPSBreakPointInfo(FBreakPoints[i]).Free;
  FBreakPoints.Clear;
  ;
end;

constructor TPSScriptDebugger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBreakPoints     := TIFList.Create;
  FExec.OnSourceLine := LineInfo;
  FExec.OnIdleCall := IdleCall;
end;

destructor TPSScriptDebugger.Destroy;
var
  i: longint;
begin
  for i := FBreakPoints.Count - 1 downto 0 do
  begin
    TPSBreakPointInfo(FBreakPoints[i]).Free;
  end;
  FBreakPoints.Free;
  inherited Destroy;
end;

function TPSScriptDebugger.GetBreakPoint(I: integer): TPSBreakPointInfo;
begin
  Result := FBreakPoints[i];
end;

function TPSScriptDebugger.GetBreakPointCount: longint;
begin
  Result := FBreakPoints.Count;
end;

function TPSScriptDebugger.GetVarContents(const Name: tbtstring): tbtstring;
var
  i:     longint;
  pv:    PIFVariant;
  s1, s: tbtstring;
begin
  s := Uppercase(Name);
  if pos('.', s) > 0 then
  begin
    s1 := copy(s, 1, pos('.', s) - 1);
    Delete(s, 1, pos('.', Name));
  end
  else
  begin
    s1 := s;
    s  := '';
  end;
  pv := nil;
  for i := 0 to Exec.CurrentProcVars.Count - 1 do
  begin
    if Uppercase(Exec.CurrentProcVars[i]) = s1 then
    begin
      pv := Exec.GetProcVar(i);
      break;
    end;
  end;
  if pv = nil then
  begin
    for i := 0 to Exec.CurrentProcParams.Count - 1 do
    begin
      if Uppercase(Exec.CurrentProcParams[i]) = s1 then
      begin
        pv := Exec.GetProcParam(i);
        break;
      end;
    end;
  end;
  if pv = nil then
  begin
    for i := 0 to Exec.GlobalVarNames.Count - 1 do
    begin
      if Uppercase(Exec.GlobalVarNames[i]) = s1 then
      begin
        pv := Exec.GetGlobalVar(i);
        break;
      end;
    end;
  end;
  if pv = nil then
    Result := RPS_UnknownIdentifier
  else
    Result := PSVariantToString(NewTPSVariantIFC(pv, False), s);
end;

function TPSScriptDebugger.HasBreakPoint(const Fn: tbtstring; Line: integer): boolean;
var
  h, i: longint;
  bi:   TPSBreakPointInfo;
begin
  h := MakeHash(Fn);
  for i := FBreakPoints.Count - 1 downto 0 do
  begin
    bi := FBreakpoints[i];
    if (h = bi.FileNameHash) and (Fn = bi.FileName) and (bi.Line = Line) then
    begin
      Result := True;
      exit;
    end;
  end;
  Result := False;
end;

procedure TPSScriptDebugger.Pause;
begin
  if FExec.Status = isRunning then
    FExec.Pause
  else
    raise Exception.Create(RPS_NotRunning);
end;

procedure TPSScriptDebugger.Resume;
begin
  if FExec.Status = isRunning then
    FExec.Run
  else
    raise Exception.Create(RPS_NotRunning);
end;

procedure TPSScriptDebugger.SetBreakPoint(const fn: tbtstring; Line: integer);
var
  i, h: longint;
  BI:   TPSBreakPointInfo;
begin
  h := MakeHash(fn);
  for i := FBreakPoints.Count - 1 downto 0 do
  begin
    bi := FBreakpoints[i];
    if (h = bi.FileNameHash) and (fn = bi.FileName) and (bi.Line = Line) then
      exit;
  end;
  bi := TPSBreakPointInfo.Create;
  FBreakPoints.Add(bi);
  bi.FileName := fn;
  bi.Line     := Line;
end;

procedure TPSScriptDebugger.SetMainFileName(const Value: tbtstring);
var
  OldFn: tbtstring;
  h1, h2, i: longint;
  bi: TPSBreakPointInfo;
begin
  OldFn := FMainFileName;
  inherited SetMainFileName(Value);
  h1 := MakeHash(OldFn);
  h2 := MakeHash(Value);
  if OldFn <> Value then
  begin
    for i := FBreakPoints.Count - 1 downto 0 do
    begin
      bi := FBreakPoints[i];
      if (bi.FileNameHash = h1) and (bi.FileName = OldFn) then
      begin
        bi.FFileNameHash := h2;
        bi.FFileName     := Value;
      end
      else if (bi.FileNameHash = h2) and (bi.FileName = Value) then
      begin
        // It's already the new filename, that can't be right, so remove all the breakpoints there
        FBreakPoints.Delete(i);
        bi.Free;
      end;
    end;
  end;
end;

procedure TPSScriptDebugger.StepInto;
begin
  if (FExec.Status = isRunning) or (FExec.Status = isLoaded) then
    FExec.StepInto
  else
    raise Exception.Create(RPS_NoScript);
end;

procedure TPSScriptDebugger.StepOver;
begin
  if (FExec.Status = isRunning) or (FExec.Status = isLoaded) then
    FExec.StepOver
  else
    raise Exception.Create(RPS_NoScript);
end;



{ TPSPluginItem }

procedure TPSPluginItem.Assign(Source: TPersistent); //Birb
begin
  if Source is TPSPluginItem then
    plugin := ((Source as TPSPluginItem).plugin)
  else
    inherited;
end;

function TPSPluginItem.GetDisplayName: string;
begin
  if FPlugin <> nil then
    Result := string(FPlugin.Name)
  else
    Result := '<nil>';
end;

procedure TPSPluginItem.SetPlugin(const Value: TPSPlugin);
begin
  FPlugin := Value;
  if Value <> nil then
    Value.FreeNotification(TPSPlugins(Collection).FCompExec);
  Changed(False);
end;

{ TPSPlugins }

constructor TPSPlugins.Create(CE: TPSScript);
begin
  inherited Create(TPSPluginItem);
  FCompExec := CE;
end;

function TPSPlugins.GetOwner: TPersistent;
begin
  Result := FCompExec;
end;

{ TPSBreakPointInfo }

procedure TPSBreakPointInfo.SetFileName(const Value: tbtstring);
begin
  FFileName     := Value;
  FFileNameHash := MakeHash(Value);
end;

{ TPSCustomPlugin }
procedure TPSCustumPlugin.CompileImport1(CompExec: TPSScript);
begin
  if @FOnCompileImport1 <> nil then
    FOnCompileImport1(CompExec)
  else
    inherited;
end;

procedure TPSCustumPlugin.CompileImport2(CompExec: TPSScript);
begin
  if @FOnCompileImport2 <> nil then
    FOnCompileImport2(CompExec)
  else
    inherited;
end;

procedure TPSCustumPlugin.CompOnUses(CompExec: TPSScript);
begin
  if @FOnCompOnUses <> nil then
    FOnCompOnUses(CompExec)
  else
    inherited;
end;

procedure TPSCustumPlugin.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  if @FOnExecImport1 <> nil then
    FOnExecImport1(CompExec, compExec.Exec, ri)
  else
    inherited;
end;

procedure TPSCustumPlugin.ExecImport2(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  if @FOnExecImport2 <> nil then
    FOnExecImport1(CompExec, compExec.Exec, ri)
  else
    inherited;
end;

procedure TPSCustumPlugin.ExecOnUses(CompExec: TPSScript);
begin
  if @FOnExecOnUses <> nil then
    FOnExecOnUses(CompExec)
  else
    inherited;
end;

end.

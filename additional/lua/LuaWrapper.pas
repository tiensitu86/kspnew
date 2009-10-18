unit LuaWrapper;

interface

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

uses
  Classes,
  LUA,
  LuaUtils,
  ProfileFunc;{,
  Lauxlib,
  LuaLib;}

type
  TVariantArray =array of Variant;
  PVariantArray =^TVariantArray;

  { TLUA }
  TLUA=class(TComponent)
  private
    L : Plua_State;
    FScript,
    FLibFile,
    FLibName: String;
    FUseDebugDLL: Boolean;
    FMethods : TStringList;
    function GetLuaPath: AnsiString;
    procedure SetLibName(const Value: String);
    function GetValue(ValueName: String): Variant;
    procedure SetLuaPath(const AValue: AnsiString);
    procedure SetValue(ValueName: String; const Value: Variant);
    procedure OpenLibs;
    procedure SetUseDebugDLL(const Value: Boolean);
    function GetUseDebugDLL: Boolean;
  public
    constructor Create(Owner : TComponent); override;
    destructor Destroy; override;

    procedure Close;
    procedure Open;

    procedure LoadScript(Script : String);
    procedure LoadFile(FileName:String);
    procedure Execute;
    procedure ExecuteCmd(Script:String);
    procedure RegisterLUAMethod(MethodName: String; Func: lua_CFunction);
    function  FunctionExists(MethodName:String) : Boolean;
    function  CallFunction( FunctionName :String; const Args: array of Variant;
                            Results : PVariantArray = nil; NResults : Integer = LUA_MULTRET):Integer;
    function  TableFunctionExists(TableName, FunctionName : String) : Boolean;
    function  CallTableFunction( TableName, FunctionName :String;
                               const Args: array of Variant;
                               Results : PVariantArray = nil):Integer;

    property LibName : String read FLibName write SetLibName;
    property Value[ValueName:String]:Variant read GetValue write SetValue;
    property LuaState : Plua_State read L;
    property UseDebugDLL : Boolean read GetUseDebugDLL write SetUseDebugDLL;
    property LuaPath    : AnsiString read GetLuaPath write SetLuaPath;
  end;

  { TLUAThread }

  TLUAThread=class
  private
    FMaster : TLUA;
    FMethodName: AnsiString;
    FTableName: AnsiString;
    L : PLua_State;
    FThreadName : AnsiString;
    function GetIsValid: Boolean;
  public
    constructor Create(LUAInstance: TLUA; ThreadName : AnsiString);
    destructor Destroy; override;

    function Start(TableName : AnsiString; AMethodName : AnsiString; const ArgNames: array of AnsiString; var ErrorString : AnsiString) : Boolean;
    function Resume(EllapsedTime : lua_Number; Args : array of Variant; var ErrorString : AnsiString) : Boolean;

    property LuaState : Plua_State read L;
  published
    property IsValid : Boolean read GetIsValid;
    property ThreadName : AnsiString read FThreadName;
    property MethodName : AnsiString read FMethodName;
    property TableName  : AnsiString read FTableName;
  end;

  { TLUAThreadList }

  TLUAThreadList=class
  private
    FThreads : TList;
    FLUAInstance : TLUA;
    function GetCount: Integer;
    function GetThread(index: integer): TLUAThread;
  public
    constructor Create(LUAInstance: TLUA);
    destructor Destroy; override;

    procedure Process(EllapsedTime : lua_Number; Args : array of Variant; var ErrorString : AnsiString);

    function SpinUp(TableName, AMethodName, ThreadName : AnsiString; var ErrorString : AnsiString) : Boolean;
    function IndexOf(ThreadName : AnsiString): Integer;
    procedure Release(ThreadIndex : Integer);

    property Thread[index:integer]: TLUAThread read GetThread;
  published
    property Count : Integer read GetCount;
  end;

implementation

uses
  Variants,
  SysUtils,
  Multilog;

constructor TLUA.Create(Owner: TComponent);
begin
  inherited;
  FMethods := TStringList.Create;
  Open;
end;

destructor TLUA.Destroy;
begin
  lua_close(L);
  FMethods.Free;
  inherited;
end;

procedure TLUA.Execute;
begin
  if L = nil then
    Open;
  if FScript <> '' then
    LuaLoadBuffer(L, FScript, LibName)
  else
    if FLibFile <> '' then
      luaL_loadfile(L, PChar(FLibFile))
    else
      exit;
  LuaPCall(L, 0, 0, 0);
end;

procedure TLUA.ExecuteCmd(Script: String);
begin
  luaL_loadbuffer(L, PChar(Script), Length(Script), PChar(LibName));
  lua_pcall(L, 0, 0, 0);
end;

function TLUA.GetValue(ValueName: String): Variant;
begin
  lua_pushstring(L, PChar(ValueName));
  lua_rawget(L, LUA_GLOBALSINDEX);
  result := LuaToVariant(L, -1);
  lua_pop(L, 1);
end;

procedure TLUA.SetLuaPath(const AValue: AnsiString);
var
  av: string;
begin
  av:=AValue;
  FixFolderNames(av);
  hLog.Send('LUA_PATH: '+av);
  if LUA_VERSION_NUM = 501 then
    begin
      luaPushString(L, 'package');
      lua_gettable(L, LUA_GLOBALSINDEX);
      luaPushString(L, 'PATH');
      luaPushString(L, av);
      //lua_SetTable(L, -3);
      lua_settable(L, LUA_GLOBALSINDEX);
    end
  else
    begin
      LuaSetTableString(L, LUA_GLOBALSINDEX, 'LUA_PATH', av);
    end;
end;

procedure TLUA.LoadFile(FileName: String);
var
  Res: integer;
begin
  FixFolderNames(FileName);
  if L = nil then
    Open;
  FLibFile := FileName;
  FScript := '';
  hLog.Send('LUA: LOADING FILE: '+FileName);
  if not FileExists(FileName) then
    hLog.Send('LUA: file does not exist');
  Res:=luaL_loadfile(L, PChar(FileName));
  if Res<>0 then
    hLog.Send('LUA ERROR WHILE LOADING SCRIPT: '+IntToStr(Res));
end;

procedure TLUA.LoadScript(Script: String);
begin
  if FScript <> Script then
    Close;
  if L = nil then
    Open;
  FScript := Trim(Script);
  FLibFile := '';
  if FScript <> '' then
    LuaLoadBuffer(L, Script, LibName);
end;

function TLUA.FunctionExists(MethodName: String): Boolean;
begin
  lua_pushstring(L, PChar(MethodName));
  lua_rawget(L, LUA_GLOBALSINDEX);
  result := lua_isfunction(L, -1);
  lua_pop(L, 1);
end;

procedure TLUA.RegisterLUAMethod(MethodName: String; Func: lua_CFunction);
begin
  if L = nil then
    Open;
  lua_register(L, PChar(MethodName), Func);
  if FMethods.IndexOf(MethodName) = -1 then
    FMethods.AddObject(MethodName, @Func)
  else
    FMethods.Objects[FMethods.IndexOf(MethodName)] := @Func;
end;

procedure TLUA.SetLibName(const Value: String);
begin
  FLibName := Value;
end;

function TLUA.GetLuaPath: AnsiString;
begin
  if LUA_VERSION_NUM = 501 then
    begin
     luaPushString(L, 'package');
     lua_gettable(L, LUA_GLOBALSINDEX);
     luaPushString(L, 'path');
     lua_rawget(L, -2);
     result := LuaToString(L, -1);
    end
  else
    begin
      result := LuaGetTableString(L, LUA_GLOBALSINDEX, 'LUA_PATH');
    end;
end;

procedure TLUA.SetValue(ValueName: String; const Value: Variant);
begin
  if VarIsType(Value, varString) then
    LuaSetIdentValue(L, ValueName, quote(Value))
  else
    LuaSetIdentValue(L, ValueName, Value);
end;

function TLUA.CallFunction(FunctionName: String;
  const Args: array of Variant; Results: PVariantArray = nil; NResults : Integer = LUA_MULTRET): Integer;
begin
  if FunctionExists(FunctionName) then
    result := LuaPCallFunction(L, FunctionName, Args, Results, 0, NResults)
  else
    result := -1;
end;

procedure TLUA.Close;
begin
  if L <> nil then
    lua_close(L);
  L := nil;
end;

procedure TLUA.Open;
begin
  if L <> nil then
    Close;
  L := lua_open;
  OpenLibs;
end;

procedure TLUA.OpenLibs;
var
  I : Integer;
begin
  //WriteLn('Loading Common Libraries...');
  luaL_openlibs(L);
  if UseDebugDLL then
    begin
      //WriteLn('Loading Debug Libraries...');
      luaopen_debug(L);
    end;
  lua_settop(L, 0);

  for I := 0 to FMethods.Count -1 do
    begin
      RegisterLUAMethod(FMethods[I], Pointer(FMethods.Objects[I]));
    end;
end;

procedure TLUA.SetUseDebugDLL(const Value: Boolean);
begin
  FUseDebugDLL := Value;
end;

function TLUA.GetUseDebugDLL: Boolean;
begin
  result := FUseDebugDLL and (FileExists(ExtractFilePath(ParamStr(0))+'LuaEditDebug.dll'));
end;

function TLUA.CallTableFunction(TableName, FunctionName: String;
  const Args: array of Variant; Results: PVariantArray): Integer;
var
   NArgs,
   NResults,
   i :Integer;
begin
  if TableFunctionExists(TableName, FunctionName) then
    begin
     //Put Function To Call on the Stack
     luaPushString(L, TableName);
     lua_gettable(L, LUA_GLOBALSINDEX);
     luaPushString(L, FunctionName);
     lua_rawget(L, -2);

     //Put Parameters on the Stack
     NArgs := High(Args)+1;
     for i:=0 to (NArgs-1) do
       LuaPushVariant(L, Args[i]);

     NResults := LUA_MULTRET;
     //Call the Function
     LuaPcall(L, NArgs, NResults, 0);
     Result :=lua_gettop(L);   //Get Number of Results

     if (Results<>Nil) then
       begin
         //Get Results in the right order
         SetLength(Results^, Result);
         for i:=0 to Result-1 do
           Results^[Result-(i+1)] :=LuaToVariant(L, -(i+1));
       end;
    end
  else
    result := -1;
end;

function TLUA.TableFunctionExists(TableName,
  FunctionName: String): Boolean;
begin
  lua_pushstring(L, PChar(TableName));
  lua_rawget(L, LUA_GLOBALSINDEX);
  result := lua_istable(L, -1);
  if result then
    begin
      lua_pushstring(L, PChar(FunctionName));
      lua_rawget(L, -2);
      result := lua_isfunction(L, -1);
      lua_pop(L, 1);
      lua_pop(L, 1);
    end
  else
    begin
      lua_pop(L, 1);
    end;
end;

{ TLUAThread }

function TLUAThread.GetIsValid: Boolean;
begin
  lua_getglobal(L, PChar(FThreadName));
  result := not lua_isnil(L, 1);
  lua_pop(L, 1);
end;

constructor TLUAThread.Create(LUAInstance: TLUA; ThreadName: AnsiString);
begin
  L := lua_newthread(LUAInstance.LuaState);
  FThreadName := ThreadName;
  lua_setglobal(LUAInstance.LuaState, PChar(ThreadName));
  FMaster := LUAInstance;
end;

destructor TLUAThread.Destroy;
begin
  lua_pushnil(FMaster.LuaState);
  lua_setglobal(FMaster.LuaState, PChar(FThreadName));
  inherited;
end;

function luaResume(L : PLua_State; NArgs:Integer; var Res : Integer) : Boolean;
begin
  Res := lua_resume(L, NArgs);
  result := Res <> 0;
end;

function TLUAThread.Start(TableName : AnsiString; AMethodName : AnsiString; const ArgNames: array of AnsiString; var ErrorString : AnsiString) : Boolean;
var
  i,
  rres : Integer;
begin
  FTableName := TableName;
  FMethodName := AMethodName;
  if TableName <> '' then
    begin
      lua_pushstring(L, PChar(TableName));
      lua_gettable(L, LUA_GLOBALSINDEX);
      luaPushString(L, PChar(AMethodName));
      lua_rawget(L, -2);
    end
  else
    lua_getglobal(L, PChar(AMethodName));

  for i := 0 to Length(ArgNames)-1 do
    lua_getglobal(L, PChar(ArgNames[i]));

  if luaResume(L, Length(ArgNames), rres) then
    begin
      ErrorString := lua_tostring(L, -1);
      result := false;
      exit;
    end
  else
    result := true;
end;

function TLUAThread.Resume(EllapsedTime : lua_Number; Args : array of Variant; var ErrorString : AnsiString) : Boolean;
var
  rres,
  i : Integer;
begin
  lua_pushnumber(L, EllapsedTime);
  for i := 0 to Length(Args)-1 do
    LuaPushVariant(L, Args[i]);
  if luaResume(L, Length(Args)+1, rres) then
    begin
      ErrorString := lua_tostring(L, -1);
      WriteLn('Dump: '+ErrorString + IntToStr(rres));
      WriteLn('Stack Dump:');
      for i := lua_gettop(L) downto 1 do
        WriteLn('  ' + LuaStackToStr(L, -1));
      result := false;
    end
  else
    result := true;
end;

{ TLUAThreadList }

function TLUAThreadList.GetCount: Integer;
begin
  result := FThreads.Count;
end;

function TLUAThreadList.GetThread(index: integer): TLUAThread;
begin
  result := TLUAThread(FThreads[index]);
end;

constructor TLUAThreadList.Create(LUAInstance: TLUA);
begin
  FLUAInstance := LUAInstance;
  FThreads := TList.Create;
end;

destructor TLUAThreadList.Destroy;
var
  T : TLUAThread;
begin
  while FThreads.Count > 0 do
    begin
      T := TLUAThread(FThreads[FThreads.Count-1]);
      FThreads.Remove(T);
      T.Free;
    end;
  FThreads.Free;
  inherited;
end;

procedure TLUAThreadList.Process(EllapsedTime: lua_Number; Args : array of Variant;
  var ErrorString: AnsiString);
var
  i : Integer;
begin
  i := 0;
  while i < Count do
    begin
      if not TLUAThread(FThreads[I]).Resume(EllapsedTime, Args, ErrorString) then
        Release(i)
      else
        inc(i);
    end;
end;

function TLUAThreadList.SpinUp(TableName, AMethodName, ThreadName: AnsiString; var ErrorString : AnsiString) : Boolean;
var
  T : TLUAThread;
begin
  T := TLUAThread.Create(FLUAInstance, ThreadName);
  FThreads.Add(T);
  result := T.Start(TableName, AMethodName, [], ErrorString);
end;

function TLUAThreadList.IndexOf(ThreadName: AnsiString): Integer;
var
  i : Integer;
begin
  result := -1;
  i := 0;
  while (result = -1) and (i<FThreads.Count) do
    begin
      if CompareText(ThreadName, TLUAThread(FThreads[i]).ThreadName) = 0 then
        result := i;
      inc(i);
    end;
end;

procedure TLUAThreadList.Release(ThreadIndex: Integer);
var
  T : TLUAThread;
begin
  if (ThreadIndex < Count) and (ThreadIndex > -1) then
    begin
      T := TLUAThread(FThreads[ThreadIndex]);
      FThreads.Delete(ThreadIndex);
      T.Free;
    end;
end;

initialization

finalization

end.

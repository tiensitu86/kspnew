Unit LuaObjects;
{$IFDEF FPC}
    {$MODE Delphi}
    {$H+}
{$ENDIF}
{.......................................................}
//  created by Paul Nicholls
//  Copyright 2006
//  Use for anything that you want, but
//  please email me with any improvements that you may make :)
//
//  Email:
//  paulfnicholls@gmail.com
{.......................................................}

Interface

Uses
    SysUtils,
    Lua, lauxlib, lualib,
    Classes;

Const
    LUA_OPEN_IOLIB    = 1;
    LUA_OPEN_DEBUGLIB = 2;
    
Type
    TLuaVarType   = (eLuaNone,eLuaNumber,eLuaBoolean,eLuaString,eLuaTable,eLuaData,eLuaFunction,eLuaNull);

    ILuaTable = Interface;

    TLuaVar = Packed Record
        VarType    : TLuaVarType;
        LuaNumber  : lua_Number;
        LuaBoolean : Boolean;
        LuaString  : AnsiString;
        LuaData    : Pointer;
        LuaTable   : ILuaTable;
        LuaFunction: Pointer;
    End;

    ILuaTable = Interface
        ['{8AC799DB-93D1-4C67-AA85-FE353D6065EE}']
        Procedure Clear;
        Procedure AddItem(Key: AnsiString; Value: TLuaVar); Overload;
        Procedure AddItem(Value: TLuaVar); Overload;

        Function  Count: LongInt;
        Function  GetCapacity: LongInt;
        Procedure SetCapacity(Capacity: LongInt);
        Function  GetHasKeys: Boolean;
        Function  GetValueByKey(Key: AnsiString): TLuaVar;
        Function  GetRefByKey(Key: AnsiString): Integer;
        Procedure SetValueByKey(Key: AnsiString; Value: TLuaVar);
        Function  GetKey(Index: LongInt): AnsiString;
        Function  GetValue(Index: LongInt): TLuaVar;
        Function  GetKeyIndex(Key: AnsiString): Integer;

        Property  Capacity                    : LongInt    Read GetCapacity Write SetCapacity;
        Property  Key  [Index: LongInt]       : AnsiString Read GetKey;
        Property  Value[Index: LongInt]       : TLuaVar    Read GetValue;
        Property  ValueByKey[Key: AnsiString] : TLuaVar    Read GetValueByKey Write SetValueByKey; Default;
        Property  RefByKey[Key: AnsiString]   : Integer    Read GetRefByKey; 
        Property  HasKeys                     : Boolean    Read GetHasKeys;
    End;

    TLuaTableItem = Packed Record
        Key   : AnsiString;
        Value : TLuaVar;
    End;

    TLuaTable = Class(TInterfacedObject,ILuaTable)
    Private
        FCount        : LongInt;
        FCapacity     : LongInt;
        FGrowthAmount : LongInt;
        FHasKeys      : Boolean;
        FItems        : Array Of TLuaTableItem;
    Public
        Constructor Create(InitialCapacity: LongWord = 10; GrowthAmount: LongWord = 10);
        Destructor  Destroy; Override;

        Procedure Clear;
        Procedure AddItem(Key: AnsiString; Value: TLuaVar); Overload;
        Procedure AddItem(Value: TLuaVar); Overload;

        Function  Count: LongInt;
        Function  GetCapacity: LongInt;
        Procedure SetCapacity(Capacity: LongInt);
        Function  GetHasKeys: Boolean;
        Function  GetValueByKey(Key: AnsiString): TLuaVar;
        Procedure SetValueByKey(Key: AnsiString; Value: TLuaVar);
        Function  GetRefByKey(Key: AnsiString): Integer;
        Function  GetKey(Index: LongInt): AnsiString;
        Function  GetValue(Index: LongInt): TLuaVar;
        Function  GetKeyIndex(Key: AnsiString): Integer;

        Property  Capacity                    : LongInt    Read GetCapacity Write SetCapacity;
        Property  Key  [Index: LongInt]       : AnsiString Read GetKey;
        Property  Value[Index: LongInt]       : TLuaVar    Read GetValue;
        Property  ValueByKey[Key: AnsiString] : TLuaVar    Read GetValueByKey Write SetValueByKey; Default;
        Property  HasKeys                     : Boolean    Read GetHasKeys;
    End;

    TLuaVarArray = Array Of TLuaVar;
    TOnError = Procedure(ErrorMsg: AnsiString; ErrorCode: Integer) Of Object;

    TLuaScript = Class
    Private
        FState          : Plua_State;
        FSelfContained  : Boolean;
        FErrorMessage   : String;
        FResults        : TLuaVarArray;

        Function  GetGlobal(Index: String): TLuaVar;
        Procedure SetGlobal(Index: String; v: TLuaVar);
    Public
        Constructor Create(AState: Plua_State; AOpenLibFlags: Cardinal = 0);
        Destructor  Destroy; Override;

        Function  DoString(s: AnsiString): Integer;
        Function  DoFile(s: AnsiString): Integer;
        Function  DoBuffer(const buff : PChar; sz : size_t): Integer;

        Procedure RegisterFunction(n: AnsiString; f: lua_CFunction);

        Function  GetGlobalInteger(s: AnsiString; ADefault: Integer): Integer;
        Function  GetGlobalBoolean(s: AnsiString; ADefault: Boolean): Boolean;
        Function  GetGlobalNumber(s: AnsiString; ADefault: Double): lua_Number;
        Function  GetGlobalString(s: AnsiString; ADefault: AnsiString): AnsiString;

        Procedure SetGlobalBoolean(s: AnsiString; v: Boolean);
        Procedure SetGlobalNumber(s: AnsiString; v: lua_Number);
        Procedure SetGlobalString(s,v: AnsiString);

        Procedure PushNumber(n: lua_Number);
        Procedure PushString(s: AnsiString);
        Procedure PushBoolean(b: Boolean);
        Procedure PushCFunction(f: lua_CFunction);
        Procedure PushRef(Ref: Integer);
        Procedure DeleteRef(Ref: Integer);
        
        Function  Call(Const Args: Array Of TLuaVar;
                       FuncName: String): Integer; Overload;
        Function  Call(Const Args: Array Of TLuaVar;
                       FuncRef: Integer): Integer; Overload;

        Function  ResultsCount: Integer;
        Function  Results(Index: Integer): TLuaVar;

        Property  State                 : Plua_State   Read FState;
        Property  ErrorMessage          : String      Read FErrorMessage;
        Property  Global[Index: String] : TLuaVar     Read GetGlobal      Write SetGlobal;
    End;

Function  LuaVar(Const Value: lua_Number): TLuaVar; Overload;
Function  LuaVar(Const Value: Boolean): TLuaVar; Overload;
Function  LuaVar(Const Value: String): TLuaVar; Overload;
Function  LuaVarData(Const Value: Pointer): TLuaVar;
Function  LuaVarFunc(Const Value: Pointer): TLuaVar;
Function  LuaTableItem(Const Key: String; Const Value: TLuaVar): TLuaTableItem; Overload;
Function  LuaTableItem(Const Value: TLuaVar): TLuaTableItem; Overload;
Function  LuaVarTable(Const Items: Array Of TLuaTableItem): TLuaVar; Overload;
Function  ParseLuaTable(L: Plua_State; idx: Integer): TLuaVar;
Procedure PushLuaTable(L: Plua_State; Const Table: ILuaTable);
Function  LuaVarNull: TLuaVar; Overload;
Function  LuaVarToStr(Const Value: TLuaVar; QuoteStrings: Boolean = False): AnsiString;
Function  LuaVarAsInt(Const Value: TLuaVar): Integer;
Function  LuaVarAsReal(Const Value: TLuaVar): lua_Number;

Procedure LuaExpectNArgs(L: Plua_State; nargs: Integer; ErrorMsg: String);
Procedure LuaExpectNumberArg(L: Plua_State; idx: Integer; ErrorMsg: String);
Procedure LuaExpectStringArg(L: Plua_State; idx: Integer; ErrorMsg: String);
Procedure LuaExpectBooleanArg(L: Plua_State; idx: Integer; ErrorMsg: String);
Procedure LuaExpectUserDataArg(L: Plua_State; idx: Integer; ErrorMsg: String);
Procedure LuaDoError(L: Plua_State; ErrorMsg: String);

Function  LuaIsInteger(L: Plua_State; Idx: Integer; ErrorMsg: String): Boolean;
Function  LuaIsNumber(L: Plua_State; Idx: Integer; ErrorMsg: String): Boolean;
Function  LuaIsString(L: Plua_State; Idx: Integer; ErrorMsg: String): Boolean;
Function  LuaIsBoolean(L: Plua_State; Idx: Integer; ErrorMsg: String): Boolean;

Implementation

function luaL_dofile(L : Plua_State; fn : PChar) : Integer;
begin
  result := luaL_loadfile(L, fn);
  if result = 0 then
    result := lua_pcall(L, 0, 0, 0);
end;

function luaL_dostring(L : Plua_State; s : PChar) : Integer;
begin
  result := luaL_loadstring(L, s);
  if result = 0 then
    result := lua_pcall(L, 0, 0, 0);
end;

function luaL_dobuffer(L : Plua_State; const buff : PChar; sz : size_t) : Integer;
begin
  result := luaL_loadbuffer(L, buff,sz,Nil);
  if result = 0 then
    result := lua_pcall(L, 0, 0, 0);
end;

{.......................................................}

{.......................................................}
Function LuaVar(Const Value: lua_Number): TLuaVar;
Begin
    Result.VarType := eLuaNumber;
    Result.LuaNumber := Value;
End;
{.......................................................}

{.......................................................}
Function LuaVar(Const Value: Boolean): TLuaVar;
Begin
    Result.VarType := eLuaBoolean;
    Result.LuaBoolean := Value;
End;
{.......................................................}

{.......................................................}
Function LuaVar(Const Value: String): TLuaVar;
Begin
    Result.VarType := eLuaString;
    Result.LuaString := Value;
End;
{.......................................................}

{.......................................................}
Function LuaVarData(Const Value: Pointer): TLuaVar;
Begin
    Result.VarType := eLuaData;
    Result.LuaData := Value;
End;
{.......................................................}

{.......................................................}
Function LuaVarFunc(Const Value: Pointer): TLuaVar;
Begin
    Result.VarType := eLuaFunction;
    Result.LuaFunction := Value;
End;
{.......................................................}

{.......................................................}
Function  LuaTableItem(Const Key: String; Const Value: TLuaVar): TLuaTableItem;
Begin
    Result.Key    := Key;
    Result.Value  := Value;
End;
{.......................................................}

{.......................................................}
Function  LuaTableItem(Const Value: TLuaVar): TLuaTableItem;
Begin
    Result := LuaTableItem('',Value);
End;
{.......................................................}

{.......................................................}
Function LuaVarTable(Const Items: Array Of TLuaTableItem): TLuaVar;
Var
    i: Integer;
Begin
    Result.VarType := eLuaTable;
    Result.LuaTable := TLuaTable.Create;

    For i := 0 To High(Items) Do
        Result.LuaTable.AddItem(Items[i].Key,Items[i].Value);
End;
{.......................................................}

{.......................................................}
Function ParseLuaTable(L: Plua_State; idx: Integer): TLuaVar;
Var
    Key   : AnsiString;
    Value : TLuaVar;
    Count : Integer;
Begin
    Result := LuaVarNull;

    If (Not lua_istable(L,idx)) Then
        Exit;

    Result := LuaVarTable([]);

    Count := 0;

    // table is in the stack at index idx

    //  count the number of items
    lua_pushnil(L);  // first key

    While (lua_next(L,idx) <> 0) Do
    Begin
        Inc(Count);

        lua_pop(L, 1);  // removes `value'; keeps `key' for next iteration
    End;

    If (Count = 0) Then
        Exit;

    Result.LuaTable.Capacity := Count;

    //  parse the items
    lua_pushnil(L);  // first key

    While (lua_next(L,idx) <> 0) Do
    Begin
        //  get key
        Case lua_type(L,-2) Of
//            LUA_TNUMBER   : Writeln(Round(lua_tonumber(L,-1)));
            LUA_TSTRING   : Key := lua_tostring(L,-2);
        Else
            Key := '';
        End;
        //  get value
        Case lua_type(L,-1) Of
            LUA_TBOOLEAN  : Value := LuaVar(lua_toboolean(L,-1));
            LUA_TNUMBER   : Value := LuaVar(lua_tonumber(L,-1));
            LUA_TSTRING   : Value := LuaVar(String(lua_tostring(L,-1)));
            LUA_TTABLE    : Value := ParseLuaTable(L,lua_gettop(L));
            LUA_TFUNCTION : Value := LuaVarFunc(lua_topointer(L,-1));//Pointer(lua_tocfunction(L,-1)));
        Else
            Value := LuaVarNull;
        End;

        Result.LuaTable.AddItem(Key,Value);

        lua_pop(L, 1);  // removes `value'; keeps `key' for next iteration
    End;
End;
{.......................................................}

{.......................................................}
Function LuaTableToStr(Const t: ILuaTable): AnsiString;
Const
    cCRLF = #13+#10;
Var
  i     : Integer;
  Key   : AnsiString;
  Value : TLuaVar;
  Table : ILuaTable;
Begin
    Result := '{}';

    If (t = Nil) Then
        Exit;

    If (Not Supports(t,ILuaTable,Table)) Then
        Exit;

    Result := cCRLF + '{' + cCRLF;

    If (Table.HasKeys) Then
    Begin
        For i := 1 To Table.Count Do
        Begin
            Key   := Table.Key[i];
            Value := Table.Value[i];

            If (Value.VarType <> eLuaTable) Then
            Begin
                Result := Result + '''' + Key + '''=' + LuaVarToStr(Value,True);
            End
            Else
                Result := Result + '''' + Key + '''=' + LuaTableToStr(Value.LuaTable);

            If (i < Table.Count) Then
                Result := Result + ',';

            If (Value.VarType <> eLuaTable) Then
                Result := Result + cCRLF;
        End;
    End
    Else
    Begin
        For i := 1 To Table.Count Do
        Begin
            Value := Table.Value[i];

            If (Value.VarType <> eLuaTable) Then
                Result := Result + LuaVarToStr(Value,True)
            Else
                Result := Result + LuaTableToStr(Value.LuaTable);

            If (i < Table.Count) Then
                Result := Result + ',';

            If (Value.VarType <> eLuaTable) Then
                Result := Result + cCRLF;
        End;
    End;

    Table := Nil;

    Result := Result + '}' + cCRLF;
End;
{.......................................................}

{.......................................................}
Procedure PushLuaTable(L: Plua_State; Const Table: ILuaTable);
Var
    i         : Integer;
    Value     : TLuaVar;
    TableIndex: Integer;
Begin
    //  if the table class is nil the exit
    If (Table = Nil) Then
        Exit;

    //  create the table on the Lua stack
    lua_newtable(L);

    //  take a note of the table position in the stack
    TableIndex := lua_gettop(L);

    //  iterate through each key,value in the table class
    For i := 1 To Table.Count Do
    Begin
        //  push the key onto the stack
        If (Table.HasKeys) Then
        Begin
            lua_pushstring(L,PChar(Table.Key[i]));
        End
        Else
            lua_pushnumber(L,i);

        //  push the value onto the stack
        Value := Table.Value[i];

        Case Value.VarType Of
            eLuaNumber  : lua_pushnumber  (L,Value.LuaNumber);
            eLuaBoolean : lua_pushboolean (L,Value.LuaBoolean);
            eLuaString  : lua_pushstring  (L,PChar(Value.LuaString));
            eLuaTable   : PushLuaTable    (L,Value.LuaTable);
        Else
            lua_pushnil(L);
        End;

        //  set the key,value pair into the table
        lua_settable(L,TableIndex);
    End;
End;
{.......................................................}

{.......................................................}
Function LuaVarNull: TLuaVar; Overload;
Begin
    Result.VarType := eLuaNull;
End;
{.......................................................}

{.......................................................}
Function LuaVarToStr(Const Value: TLuaVar; QuoteStrings: Boolean = False): AnsiString;
Const
    BooleanArray: Array[Boolean] Of String = ('False','True');
    QuotesArray : Array[Boolean] Of String = ('','"');
Begin
    Case Value.VarType Of
        eluaNumber    : Result := FLoatToStr(Value.LuaNumber);
        eluaBoolean   : Result := BooleanArray[Value.LuaBoolean];
        eLuaString    : Result := QuotesArray[QuoteStrings] + Value.LuaString + QuotesArray[QuoteStrings];
        eLuaNone      : Result := 'None';
        eLuaNull      : Result := 'Null';
        eLuaTable     : Result := LuaTableToStr(Value.LuaTable);
        eLuaFunction  : Result := 'Func($'+IntToHex(Integer(Value.LuaFunction),8)+')';
    Else
        Result := 'Not supported';
    End;
End;
{.......................................................}

{.......................................................}
Function LuaVarAsInt(Const Value: TLuaVar): Integer;
Var
    v,c: Integer;
Begin
    Case Value.VarType Of
        eluaNumber  : Result := Round(Value.LuaNumber);
        eluaBoolean : Result := Ord(Value.LuaBoolean);
        eLuaString  : Begin
            Val(Value.LuaString,v,c);

            If (c = 0) Then
                Result := v
            Else
                Result := -1;
        End;
    Else
        Result := -1;
    End;
End;
{.......................................................}

{.......................................................}
Function LuaVarAsReal(Const Value: TLuaVar): lua_Number;
Var
    v: lua_Number;
    c: Integer;
Begin
    Case Value.VarType Of
        eluaNumber  : Result := Value.LuaNumber;
        eluaBoolean : Result := Ord(Value.LuaBoolean);
        eLuaString  : Begin
            Val(Value.LuaString,v,c);

            If (c = 0) Then
                Result := v
            Else
                Result := 0;
        End;
    Else
        Result := 0;
    End;
End;
{.......................................................}

{.......................................................}
Procedure LuaDoError(L: Plua_State; ErrorMsg: String);
Begin
    lua_pushstring(L,PChar(ErrorMsg));
    lua_error(L);
End;
{.......................................................}

{.......................................................}
Function  LuaIsInteger(L: Plua_State; Idx: Integer; ErrorMsg: String): Boolean;
Var
    Value: Double;
Begin
    Result := True;

    If (lua_type(L,idx) = LUA_TNUMBER) Then
        Value := lua_tonumber(L,Idx)
    Else
    Begin
        Result := False;
        If (ErrorMsg <> '') Then
            LuaDoError(L,ErrorMsg + ': "integer" expected, received "' + lua_typename(L,lua_type(L,Idx)) + '"')
        Else
            LuaDoError(L,'"integer" expected, received "' + lua_typename(L,lua_type(L,Idx)) + '"');
        Exit;
    End;

    If (Abs(Value - Trunc(Value)) > 0.0001) Then
    Begin
        Result := False;
        If (ErrorMsg <> '') Then
            LuaDoError(L,ErrorMsg + ': "integer" expected, received "' + lua_typename(L,lua_type(L,Idx)) + '"')
        Else
            LuaDoError(L,'"integer" expected, received "' + lua_typename(L,lua_type(L,Idx)) + '"');
    End;
End;
{.......................................................}

{.......................................................}
Function  LuaIsNumber(L: Plua_State; Idx: Integer; ErrorMsg: String): Boolean;
Begin
    Result := True;
    
    If (lua_type(L,idx) <> LUA_TNUMBER) Then
    Begin
        Result := False;
        If (ErrorMsg <> '') Then
            LuaDoError(L,ErrorMsg + ': "number" expected, received "' + lua_typename(L,lua_type(L,Idx)) + '"')
        Else
            LuaDoError(L,'"number" expected, received "' + lua_typename(L,lua_type(L,Idx)) + '"');
    End;
End;
{.......................................................}

{.......................................................}
Function  LuaIsString(L: Plua_State; Idx: Integer; ErrorMsg: String): Boolean;
Begin
    Result := True;
    
    If (lua_type(L,idx) <> LUA_TSTRING) Then
    Begin
        Result := False;
        If (ErrorMsg <> '') Then
            LuaDoError(L,ErrorMsg + ': "string" expected, received "' + lua_typename(L,lua_type(L,Idx)) + '"')
        Else
            LuaDoError(L,'"string" expected, received "' + lua_typename(L,lua_type(L,Idx)) + '"');
    End;
End;
{.......................................................}

{.......................................................}
Function  LuaIsBoolean(L: Plua_State; Idx: Integer; ErrorMsg: String): Boolean;
Begin
    Result := True;
    
    If (lua_type(L,idx) <> LUA_TBOOLEAN) Then
    Begin
        Result := False;
        If (ErrorMsg <> '') Then
            LuaDoError(L,ErrorMsg + ': "boolean" expected, received "' + lua_typename(L,lua_type(L,Idx)) + '"')
        Else
            LuaDoError(L,'"boolean" expected, received "' + lua_typename(L,lua_type(L,Idx)) + '"');
    End;
End;
{.......................................................}

{.......................................................}
Procedure LuaExpectNArgs(L: Plua_State; nargs: Integer; ErrorMsg: String);
Begin
    If (lua_gettop(L) <> nargs) Then
    Begin
        lua_pushstring(L,PChar(ErrorMsg + ' expected '+IntToStr(nargs) + ' argument(s)'));
        lua_error(L);
    End;
End;
{.......................................................}

{.......................................................}
Procedure LuaExpectNumberArg(L: Plua_State; idx: Integer; ErrorMsg: String);
Begin
    If (Not lua_isnumber(L,idx)) Then
    Begin
        lua_pushstring(L,PChar(ErrorMsg + ' expected number for argument #'+IntToStr(idx)));
        lua_error(L);
    End;
End;
{.......................................................}

{.......................................................}
Procedure LuaExpectStringArg(L: Plua_State; idx: Integer; ErrorMsg: String);
Begin
    If (Not lua_isstring(L,idx)) Then
    Begin
        lua_pushstring(L,PChar(ErrorMsg + ' expected string for argument #'+IntToStr(idx)));
        lua_error(L);
    End;
End;
{.......................................................}

{.......................................................}
Procedure LuaExpectBooleanArg(L: Plua_State; idx: Integer; ErrorMsg: String);
Begin
    If (Not lua_isboolean(L,idx)) Then
    Begin
        lua_pushstring(L,PChar(ErrorMsg + ' expected boolean for argument #'+IntToStr(idx)));
        lua_error(L);
    End;
End;
{.......................................................}

{.......................................................}
Procedure LuaExpectUserDataArg(L: Plua_State; idx: Integer; ErrorMsg: String);
Begin
    If (Not lua_islightuserdata(L,idx)) Then
    Begin
        lua_pushstring(L,PChar(ErrorMsg + ' expected UserData for argument #'+IntToStr(idx)));
        lua_error(L);
    End;
End;
{.......................................................}

{.......................................................}
Constructor TLuaScript.Create(AState: Plua_State; AOpenLibFlags: Cardinal = 0);
Begin
    Inherited Create;

    FState := Nil;
    FErrorMessage := '';
    FSelfContained := True;

    If (AState = Nil) Then
    Begin
        FState := lua_open;

        If (FState = Nil) Then
        Begin
            FErrorMessage := 'Error: lua_open';

            Exit;
        End;

        luaopen_base(FState);
        luaopen_table(FState);
        luaopen_string(FState);
        luaopen_math(FState);

        If (AOpenLibFlags And LUA_OPEN_IOLIB <> 0) Then
            luaopen_io(FState);

        If (AOpenLibFlags And LUA_OPEN_DEBUGLIB <> 0) Then
            luaopen_debug(FState);

{        lua_baselibopen(FState);
        lua_tablibopen(FState);
        lua_strlibopen(FState);
        lua_mathlibopen(FState);

        If (AOpenLibFlags And LUA_OPEN_IOLIB <> 0) Then
            lua_iolibopen(FState);

        If (AOpenLibFlags And LUA_OPEN_DEBUGLIB <> 0) Then
            lua_dblibopen(FState);}
    End
    Else
    Begin
        FSelfContained := False;
        FState := AState;
    End;
End;
{.......................................................}

{.......................................................}
Destructor  TLuaScript.Destroy;
Begin
    If (Not FSelfContained) And (FState <> Nil) Then
        lua_close(FState);

    Inherited Destroy;
End;
{.......................................................}

{.......................................................}
Function  TLuaScript.DoString(s: AnsiString): Integer;
Begin
    Result := luaL_dostring(FState,PChar(s));
End;
{.......................................................}

{.......................................................}
Function  TLuaScript.DoFile(s: AnsiString): Integer;
Begin
    Result := luaL_dofile(FState,PChar(s));
End;
{.......................................................}

{.......................................................}
Function  TLuaScript.DoBuffer(const buff : PChar; sz : size_t): Integer;
Begin
    Result := luaL_dobuffer(FState,buff,sz);
End;
{.......................................................}

{.......................................................}
Procedure TLuaScript.RegisterFunction(n: AnsiString; f: lua_CFunction);
Begin
    lua_register(FState,PChar(n),f);
End;
{.......................................................}

{.......................................................}
Procedure TLuaScript.PushNumber(n: lua_Number);
Begin
    lua_pushnumber(FState,n);
End;
{.......................................................}

{.......................................................}
Procedure TLuaScript.PushString(s: AnsiString);
Begin
    lua_pushstring(FState,PChar(s));
End;
{.......................................................}

{.......................................................}
Procedure TLuaScript.PushBoolean(b: Boolean);
Begin
    lua_pushboolean(FState,b);
End;
{.......................................................}

{.......................................................}
Procedure TLuaScript.PushRef(Ref: Integer);
Begin
    lua_rawgeti(FState, LUA_REGISTRYINDEX, Ref);
End;
{.......................................................}

{.......................................................}
Procedure TLuaScript.DeleteRef(Ref: Integer);
Begin
    lua_unref(FState,Ref);
End;
{.......................................................}

{.......................................................}
Procedure TLuaScript.PushCFunction(f: lua_CFunction);
Begin
    lua_pushcfunction(FState,f);
End;
{.......................................................}

{.......................................................}
Const
    cIndex = -1;
Function  TLuaScript.GetGlobalInteger(s: AnsiString; ADefault: Integer): Integer;
Begin
    lua_GetGlobal(FState,PChar(s));

    Case lua_Type(FState,cIndex) Of
        LUA_TNUMBER  : Result := Round(lua_tonumber(FState,cIndex));
    Else
        Result := ADefault;
    End;

    lua_pop(FState,1);
End;
{.......................................................}

{.......................................................}
Function  TLuaScript.GetGlobalBoolean(s: AnsiString; ADefault: Boolean): Boolean;
Begin
    lua_GetGlobal(FState,PChar(s));

    Case lua_Type(FState,cIndex) Of
        LUA_TBOOLEAN : Result := lua_toboolean(FState,cIndex);
    Else
        Result := ADefault;
    End;

    lua_pop(FState,1);
End;
{.......................................................}

{.......................................................}
Function  TLuaScript.GetGlobalNumber(s: AnsiString; ADefault: Double): lua_Number;
Begin
    lua_GetGlobal(FState,PChar(s));

    Case lua_Type(FState,cIndex) Of
        LUA_TNUMBER  : Result := lua_tonumber(FState,cIndex);
    Else
        Result := ADefault;
    End;

    lua_pop(FState,1);
End;
{.......................................................}

{.......................................................}
Function  TLuaScript.GetGlobalString(s: AnsiString; ADefault: AnsiString): AnsiString;
Begin
    lua_GetGlobal(FState,PChar(s));

    Case lua_Type(FState,cIndex) Of
        LUA_TSTRING  : Result := lua_tostring(FState,cIndex);
    Else
        Result := ADefault;
    End;

    lua_pop(FState,1);
End;
{.......................................................}

{.......................................................}
Procedure TLuaScript.SetGlobalBoolean(s: AnsiString; v: Boolean);
Begin
    lua_pushboolean(FState,v);

    lua_SetGlobal(FState,PChar(s));
End;
{.......................................................}

{.......................................................}
Procedure TLuaScript.SetGlobalNumber(s: AnsiString; v: lua_Number);
Begin
    lua_pushnumber(FState,v);

    lua_SetGlobal(FState,PChar(s));
End;
{.......................................................}

{.......................................................}
Procedure TLuaScript.SetGlobalString(s,v: AnsiString);
Begin
    lua_pushstring(FState,PChar(v));
    lua_SetGlobal(FState,PChar(s));
End;
{.......................................................}

{.......................................................}
Function  TLuaScript.GetGlobal(Index: String): TLuaVar;
Begin
    lua_GetGlobal(FState,PChar(Index));

    Case lua_Type(FState,cIndex) Of
        LUA_TBOOLEAN : Result := LuaVar(lua_toboolean(FState,lua_gettop(FState)));
        LUA_TNUMBER  : Result := LuaVar(lua_tonumber(FState,lua_gettop(FState)));
        LUA_TSTRING  : Result := LuaVar(String(lua_tostring(FState,lua_gettop(FState))));
        LUA_TTABLE   : Result := ParseLuaTable(FState,lua_gettop(FState));
    Else
        Result := LuaVarNull;
    End;

    If (Result.VarType <> eLuaNone) Then
        lua_pop(FState,1);
End;
{.......................................................}

{.......................................................}
Procedure TLuaScript.SetGlobal(Index: String; v: TLuaVar);
Begin
    Case v.VarType Of
        eLuaNumber  : lua_pushnumber(FState,v.LuaNumber);
        eLuaBoolean : lua_pushboolean(FState,v.LuaBoolean);
        eLuaString  : lua_pushstring(FState,PChar(v.LuaString));
    Else
        lua_pushnil(FState);
    End;

    lua_SetGlobal(FState,PChar(Index));
End;
{.......................................................}

{.......................................................}
Function  TLuaScript.ResultsCount: Integer;
Begin
    Result := Length(FResults);
End;
{.......................................................}

{.......................................................}
Function  TLuaScript.Results(Index: Integer): TLuaVar;
Begin
    Result := LuaVarNull;

    If (Index >= 0) And (Index <= High(FResults)) Then
        Result := FResults[Index];
End;
{.......................................................}

{.......................................................}
Function  TLuaScript.Call(Const Args: Array Of TLuaVar;
                          FuncName: String): Integer;
Var
    i           : Integer;
    top1,top2   : Integer;
    ResultsCount: Integer;
Begin
    SetLength(FResults,0);

    top1 := lua_gettop(FState);

    lua_getglobal(FState,PChar(FuncName));

    For i := 0 To High(Args) Do
    Begin
        Case Args[i].VarType Of
            eLuaNumber  : lua_pushnumber(FState,Args[i].LuaNumber);
            eLuaBoolean : lua_pushboolean(FState,Args[i].LuaBoolean);
            eLuaString  : lua_pushstring(FState,PChar(Args[i].LuaString));
            eLuaData    : lua_pushlightuserdata(FState,Args[i].LuaData);
            eLuaTable   : PushLuaTable(FState,Args[i].LuaTable);
        Else
            lua_pushnil(FState);
        End;
    End;

    Result := lua_PCall(FState,Length(Args),LUA_MULTRET,0);

    If (Result = 0) Then
    Begin
        top2 := lua_gettop(FState);

        ResultsCount := top2 - top1;

        SetLength(FResults,ResultsCount);

        For i := ResultsCount Downto 1 Do
        Begin
            Case lua_type(FState, -i) Of
                LUA_TBOOLEAN  : FResults[ResultsCount - i] := LuaVar(lua_toboolean(FState,-i));
                LUA_TNUMBER   : FResults[ResultsCount - i] := LuaVar(lua_tonumber(FState,-i));
                LUA_TSTRING   : FResults[ResultsCount - i] := LuaVar(String(lua_tostring(FState,-i)));
                LUA_TTABLE    : FResults[ResultsCount - i] := ParseLuaTable(FState,top2-i+1);
                LUA_TFUNCTION : FResults[ResultsCount - i] := LuaVarFunc(lua_topointer(FState,-1));//lua_tocfunction(FState,-i));
            Else
                FResults[ResultsCount - i] := LuaVarNull;
            End;
        End;

        For i := 1 To ResultsCount Do
            lua_pop(FState,1);

        FErrorMessage := '';
    End
    Else
    Begin
        If (lua_gettop(FState) >= 1) Then
        Begin
            FErrorMessage := lua_tostring(FState,-1);
            lua_pop(FState,-1);
        End
        Else
            FErrorMessage := 'lua_pcall: ' + IntToStr(Result);
    End;
End;
{.......................................................}

{.......................................................}
Function  TLuaScript.Call(Const Args: Array Of TLuaVar;
                          FuncRef: Integer): Integer;
Var
    i           : Integer;
    top1,top2   : Integer;
    ResultsCount: Integer;
Begin
    SetLength(FResults,0);

    top1 := lua_gettop(FState);

    PushRef(FuncRef);

    For i := 0 To High(Args) Do
    Begin
        Case Args[i].VarType Of
            eLuaNumber  : lua_pushnumber(FState,Args[i].LuaNumber);
            eLuaBoolean : lua_pushboolean(FState,Args[i].LuaBoolean);
            eLuaString  : lua_pushstring(FState,PChar(Args[i].LuaString));
            eLuaData    : lua_pushlightuserdata(FState,Args[i].LuaData);
            eLuaTable   : PushLuaTable(FState,Args[i].LuaTable);
        Else
            lua_pushnil(FState);
        End;
    End;

    Result := lua_PCall(FState,Length(Args),LUA_MULTRET,0);

    If (Result = 0) Then
    Begin
        top2 := lua_gettop(FState);

        ResultsCount := top2 - top1;

        SetLength(FResults,ResultsCount);

        For i := ResultsCount Downto 1 Do
        Begin
            Case lua_type(FState, -i) Of
                LUA_TBOOLEAN  : FResults[ResultsCount - i] := LuaVar(lua_toboolean(FState,-i));
                LUA_TNUMBER   : FResults[ResultsCount - i] := LuaVar(lua_tonumber(FState,-i));
                LUA_TSTRING   : FResults[ResultsCount - i] := LuaVar(String(lua_tostring(FState,-i)));
                LUA_TTABLE    : FResults[ResultsCount - i] := ParseLuaTable(FState,top2-i+1);
//                LUA_TFUNCTION : FResults[ResultsCount - i] := LuaVarFunc(luaL_ref(FState,LUA_REGISTRYINDEX));
            Else
                FResults[ResultsCount - i] := LuaVarNull;
            End;
        End;

        For i := 1 To ResultsCount Do
            lua_pop(FState,1);

        FErrorMessage := '';
    End
    Else
    Begin
        If (lua_gettop(FState) >= 1) Then
        Begin
            FErrorMessage := lua_tostring(FState,-1);
            lua_pop(FState,-1);
        End
        Else
            FErrorMessage := 'lua_pcall: ' + IntToStr(Result);
    End;
End;
{.......................................................}

{.......................................................}
Constructor TLuaTable.Create(InitialCapacity: LongWord = 10; GrowthAmount: LongWord = 10);
Begin
    Inherited Create;

    FHasKeys := False;

    FGrowthAmount := GrowthAmount;
    FCount := 0;

    SetCapacity(InitialCapacity);
End;
{.......................................................}

{.......................................................}
Destructor  TLuaTable.Destroy;
Begin
    Clear;

    Inherited Destroy;
End;
{.......................................................}

{.......................................................}
Procedure TLuaTable.Clear;
Var
    i: Integer;
Begin
    For i := 0 To High(FItems) Do
    Begin
        If (FItems[i].Value.VarType = eLuaTable) Then
        Begin
            If (FItems[i].Value.LuaTable <> Nil) Then
            Begin
                FItems[i].Value.LuaTable.Clear;
                FItems[i].Value.LuaTable := Nil;
            End;
        End
        Else
            FItems[i].Value := LuaVarNull;
    End;

    FHasKeys := False;

    FCount := 0;
End;
{.......................................................}

{.......................................................}
Procedure TLuaTable.AddItem(Key: AnsiString; Value: TLuaVar);
Var
    Index: Integer;
Begin
    If (Key <> '') Then
        FHasKeys := True;

    Index := GetKeyIndex(Key);

    If (Index = -1) Then
    Begin
        Inc(FCount);

        If (FCount > FCapacity) Then
        Begin
            SetCapacity(FCapacity + FGrowthAmount);
        End;

        FItems[FCount - 1].Key := Key;
        FItems[FCount - 1].Value := Value;
    End
    Else
    Begin
        FItems[Index - 1].Value := Value;
    End;
End;
{.......................................................}

{.......................................................}
Procedure TLuaTable.AddItem(Value: TLuaVar);
Begin
    Inc(FCount);

    If (FCount > FCapacity) Then
    Begin
        SetCapacity(FCapacity + FGrowthAmount);
    End;

    FItems[FCount - 1].Key := '';
    FItems[FCount - 1].Value := Value;
End;
{.......................................................}

{.......................................................}
Function  TLuaTable.Count: LongInt;
Begin
    Result := FCount;
End;
{.......................................................}

{.......................................................}
Function  TLuaTable.GetCapacity: LongInt;
Begin
    Result := FCapacity;
End;
{.......................................................}

{.......................................................}
Procedure TLuaTable.SetCapacity(Capacity: LongInt);
Begin
    If (Capacity < 0) Then
        Exit;

    If (Capacity < FCapacity) Then
        Clear;

    FCapacity := Capacity;

    SetLength(FItems,FCapacity);
End;
{.......................................................}

{.......................................................}
Function  TLuaTable.GetHasKeys: Boolean;
Begin
    Result := FHasKeys;
End;
{.......................................................}

{.......................................................}
Function  TLuaTable.GetValueByKey(Key: AnsiString): TLuaVar;
Var
    i: Integer;
Begin
    Result := LuaVarNull;

    If (Key = '') Then
        Exit;

    If (Not FHasKeys) Then
        Exit;

    For i := 0 To High(FItems) Do
        If (FItems[i].Key = Key) Then
        Begin
            Result := FItems[i].Value;
            Exit;
        End;
End;
{.......................................................}

{.......................................................}
Function  TLuaTable.GetRefByKey(Key: AnsiString): Integer;
Var
    i: Integer;
Begin
    Result := 0;

    If (Key = '') Then
        Exit;

    If (Not FHasKeys) Then
        Exit;

    For i := 0 To High(FItems) Do
        If (FItems[i].Key = Key) Then
        Begin
            Exit;
        End;
End;
{.......................................................}

{.......................................................}
Procedure TLuaTable.SetValueByKey(Key: AnsiString; Value: TLuaVar);
Var
    i: Integer;
Begin
    If (Key = '') Then
        Exit;

    i := GetKeyIndex(Key);

    If (i = -1) Then
        AddItem(Key,Value)
    Else
        FItems[i - 1].Value := Value;
End;
{.......................................................}

{.......................................................}
Function  TLuaTable.GetValue(Index: LongInt): TLuaVar;
Begin
    Result := LuaVarNull;

    If (Index < 1) Or (Index > FCount) Then
        Exit;

    Result := FItems[Index - 1].Value;
End;
{.......................................................}

{.......................................................}
Function  TLuaTable.GetKey(Index: LongInt): AnsiString;
Begin
    Result := '';

    If (Index < 1) Or (Index > FCount) Then
        Exit;

    Result := FItems[Index - 1].Key;
End;
{.......................................................}

{.......................................................}
Function  TLuaTable.GetKeyIndex(Key: AnsiString): Integer;
Var
    i: Integer;
Begin
    Result := -1;

    If (Key = '') Then
        Exit;

    If (Not FHasKeys) Then
        Exit;

    For i := 0 To FCount - 1 Do
        If (FItems[i].Key = Key) Then
        Begin
            Result := i + 1;
            Exit;
        End;
End;
{.......................................................}

{.......................................................}
End.

unit ksplua;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Lua;

function LuaShowMessage(L: Plua_State): Integer; cdecl;
function LuaLogEntry(L: Plua_State): Integer; cdecl;

procedure SetupLua;
procedure FreeLua;

implementation

uses kspfiles, multilog, KSPConstsVars, LuaWrapper;

function LuaShowMessage(L: Plua_State): Integer; cdecl;
var
  p: pChar;
begin
  p:=lua_tostring(L, -1);
  hLog.Send('MSG FROM LUA: '+p);
  KSPShowMessageP(p);
  Result:=0;
end;

function LuaLogEntry(L: Plua_State): Integer; cdecl;
var
  p: pChar;
begin
  p:=lua_tostring(L, -1);
  hLog.Send('LUA LOG: '+p);
  Result:=0;
end;

procedure SetupLua;
begin
  ScriptedAddons:=TLUA.Create;
  hLog.Send('LUA DEF PATH: '+ScriptedAddons.LuaPath);
  ScriptedAddons.LuaPath:=ScriptedAddons.LuaPath+';'+KSPDataFolder+'lua/?.lua';
  hLog.Send('LUA DEF PATH: '+ScriptedAddons.LuaPath);
  ScriptedAddons.RegisterLUAMethod('ShowMessage', @LuaShowMessage);
  ScriptedAddons.RegisterLUAMethod('AddLog', @LuaLogEntry);
  //ScriptedAddons.LoadFile(KSPDataFolder+'lua\test.lua');
  //
  DefaultScript:='AddLog("LUA_CPATH=", os.getenv("LUA_CPATH"))';
  //DefaultScript:=DefaultScript+#13+'AddLog(os.getenv("LUA_PATH"))';
  //DefaultScript:=DefaultScript+#13+'AddLog(os.getenv("HOME"))';
  //DefaultScript:=DefaultScript+#13+'AddLog(os.getenv("USERNAME"))';
  //DefaultScript:='os.getenv("LUA_CPATH")';
  ScriptedAddons.LoadScript(DefaultScript);
  ScriptedAddons.Execute;
end;

procedure FreeLua;
begin
  ScriptedAddons.Free;
end;

end.


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

uses kspfiles, multilog, KSPConstsVars, LuaWrapper, ProfileFunc;

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
  DefaultScript:=KSPDataFolder+'lua/test.lua';
  FixFolderNames(DefaultScript);
  if FileExists(DefaultScript) then begin
    ScriptedAddons.LoadFile();
    ScriptedAddons.Execute;
  end;
end;

procedure FreeLua;
begin
  ScriptedAddons.Free;
end;

end.


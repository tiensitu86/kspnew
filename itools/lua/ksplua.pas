unit ksplua;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Lua;

function LuaShowMessage(L: Plua_State): Integer; cdecl;
function LuaLogEntry(L: Plua_State): Integer; cdecl;

implementation

uses kspfiles, multilog;

function LuaShowMessage(L: Plua_State): Integer; cdecl;
var
  p: pChar;
begin
  p:=lua_tostring(L, -1);
  hLog.Send('MSG FROM LUA: '+p);
  KSPShowMessage(p);
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

end.


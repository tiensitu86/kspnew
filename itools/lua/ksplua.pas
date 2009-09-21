unit ksplua;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Lua;

function LuaShowMessage(L: Plua_State): Integer; cdecl;

implementation

uses kspfiles, multilog;

function LuaShowMessage(L: Plua_State): Integer; cdecl;
var
  p: pChar;
begin
  p:=L;
  hLog.Send('MSG FROM LUA: '+p);
  KSPShowMessage(p);
end;

end.


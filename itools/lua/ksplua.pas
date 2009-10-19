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
  Classes, SysUtils, Lua, luamenu;

function LuaShowMessage(L: Plua_State): Integer; cdecl;
function LuaLogEntry(L: Plua_State): Integer; cdecl;
function LuaLoadInterface(L: Plua_State): Integer; cdecl;

procedure SetupLua;
procedure FreeLua;

implementation

uses kspfiles, multilog, KSPConstsVars, LuaWrapper, ProfileFunc;

function LuaLoadInterface(L: Plua_State): Integer; cdecl;
var
  p: pChar;
begin
  p:=lua_tostring(L, -1);
  hLog.SendLua('Loading addon interface: '+p);
end;

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
  hLog.SendLua(p);
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
  ScriptedAddons.RegisterLUAMethod('LoadUI', @LuaLoadInterface);
  DefaultScript:=KSPDataFolder+'lua/runaddons.lua';
  FixFolderNames(DefaultScript);
  if FileExists(DefaultScript) then begin
    ScriptedAddons.LoadFile(DefaultScript);
    ScriptedAddons.Execute;
  end;
end;

procedure FreeLua;
begin
  ScriptedAddons.Free;
end;

end.


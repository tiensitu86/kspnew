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

unit profilefunc;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

function GetUserDataFolder: string;
procedure FixFolderNames(var FolderName: string);
function ForceDirectoriesKSP(const Dir: string): boolean;
procedure FixFileNameDB(var Name: string);
procedure FixFileNameDB2(var Name: string);
function ReplaceStr(strSource, strFind, strReplace: string): string;


implementation

function ForceDirectoriesKSP(const Dir: string): boolean;
var
  t: string;
begin
  t := Dir;
  FixFolderNames(t);
  Result := ForceDirectories(t);
end;

function ReplaceStr(strSource, strFind, strReplace: string): string;
var
  p: integer;
begin
  Result := '';
  p      := pos(uppercase(strFind), uppercase(strSource));
  while p > 0 do
  begin
    Result := Result + Copy(strSource, 1, p - 1) + strReplace;
    Delete(strSource, 1, p + Length(strFind) - 1);
    p := pos(uppercase(strFind), uppercase(strSource));
  end;
  Result := Result + strSource;
end;

procedure FixFolderNames(var FolderName: string);
begin
  //{$ifdef mswindows}
  //  FolderName:=ReplaceStr(FolderName, '/', '\');
  //{$ELSE}
  //  FolderName:=ReplaceStr(FolderName, '\', '/');
  //{$endif}
  FolderName := SetDirSeparators(FolderName);
end;

procedure FixFileNameDB(var Name: string);
begin
  FixFileNameDB2(Name);
{$ifdef mswindows}
  Name := ReplaceStr(Name, '\', '/');
  Name := ReplaceStr(Name, '/', '\\');
{$else}
  Name := ReplaceStr(Name, '/', '\');
  Name := ReplaceStr(Name, '\', '//');
{$endif}
end;

procedure FixFileNameDB2(var Name: string);
begin
{$ifdef mswindows}
  Name := ReplaceStr(Name, '\\', '/');
  Name := ReplaceStr(Name, '/', '\');
{$else}
  Name := ReplaceStr(Name, '//', '\');
  Name := ReplaceStr(Name, '\', '/');
{$endif}
end;

function GetUserDataFolder: string;
begin
  Result := GetUserDir;
end;


end.

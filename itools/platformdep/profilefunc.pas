unit profilefunc;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

function GetUserDataFolder: string;
procedure FixFolderNames(var FolderName: string);
Function ForceDirectoriesKSP(Const Dir: string): Boolean;
procedure FixFileNameDB(var Name: string);
procedure FixFileNameDB2(var Name: string);


implementation

Function ForceDirectoriesKSP(Const Dir: string): Boolean;
var
  t: string;
begin
  t:=Dir;
  FixFolderNames(t);
  Result:=ForceDirectories(t);
end;

function ReplaceStr(strSource, strFind, strReplace: string):string;
  var
    p :integer;
  begin
    result:='';
    p:=pos(uppercase(strFind),uppercase(strSource));
    while p>0 do begin
      result:=result+Copy(strSource,1,p-1)+strReplace;
      Delete(strSource,1,p+Length(strFind)-1);
      p:=pos(uppercase(strFind), uppercase(strSource));
    end;
    Result:=Result+strSource;
  end;

procedure FixFolderNames(var FolderName: string);
begin
{$ifdef mswindows}
  FolderName:=ReplaceStr(FolderName, '/', '\');
{$ELSE}
  FolderName:=ReplaceStr(FolderName, '\', '/');
{$endif}
end;

procedure FixFileNameDB(var Name: string);
begin
  FixFileNameDB2(Name);
{$ifdef mswindows}
  Name:=ReplaceStr(Name, '\', '/');
  Name:=ReplaceStr(Name, '/', '\\');
{$else}
  Name:=ReplaceStr(Name, '/', '\');
  Name:=ReplaceStr(Name, '\', '//');
{$endif}
end;

procedure FixFileNameDB2(var Name: string);
begin
{$ifdef mswindows}
  Name:=ReplaceStr(Name, '\\', '/');
  Name:=ReplaceStr(Name, '/', '\');
{$else}
  Name:=ReplaceStr(Name, '//', '\');
  Name:=ReplaceStr(Name, '\', '/');
{$endif}
end;

function GetUserDataFolder: string;
begin
{$ifdef mswindows}
//   SHGetFolderPathW(0,CSIDL_PERSONAL+CSIDL_FLAG_CREATE,0,0,PATH);
   Result:=GetUserDir;
{$else}
   Result:=GetEnvironmentVariableUTF8('HOME')+'/';
{$endif}
end;


end.


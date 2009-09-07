{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library                                                         }
{ Global Unicode functions                                                    }
{                                                                             }
{ http://mac.sourceforge.net/atl/                                             }
{ e-mail: macteam@users.sourceforge.net                                       }
{                                                                             }
{ Copyright (c) 2003-2005 by The MAC Team                                     }
{                                                                             }
{ Version 1.0 (24 March 2005)                                                 }
{                                                                             }
{ This library is free software; you can redistribute it and/or               }
{ modify it under the terms of the GNU Lesser General Public                  }
{ License as published by the Free Software Foundation; either                }
{ version 2.1 of the License, or (at your option) any later version.          }
{                                                                             }
{ This library is distributed in the hope that it will be useful,             }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of              }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           }
{ Lesser General Public License for more details.                             }
{                                                                             }
{ You should have received a copy of the GNU Lesser General Public            }
{ License along with this library; if not, write to the Free Software         }
{ Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA   }
{                                                                             }
{ *************************************************************************** }

unit CommonATL;

interface

uses
  Classes;

function UTF8ToWString(const S: UTF8String): WideString;
function WStringToUTF8(const S: WideString): UTF8String;
function UpCaseW(const C: WideChar): WideChar;
function LowCaseW(const C: WideChar): WideChar;
function UpperCaseW(const S: WideString): WideString;
function LowerCaseW(const S: WideString): WideString;
function GetCurDir: WideString;
procedure SetCurDir(const Dir: WideString);
function FileCreateW(const FileName: WideString): Integer;
function FileOpenW(const FileName: WideString; Mode: LongWord): Integer;
function WideFileSetDate(const FileName: WideString; Age: LongInt): LongInt;

{type
  TFileStreamW = class(THandleStream)
  public
    constructor Create(const FileName: WideString; Mode: Word); overload;
    destructor Destroy; override;
  end; }

implementation

uses
  SysUtils, FileUtil;

(* -------------------------------------------------------------------------- *)

function UTF8ToWString(const S: UTF8String): WideString;
begin
  Result:=UTF8Decode(S);
end;


(* -------------------------------------------------------------------------- *)

function WStringToUTF8(const S: WideString): UTF8String;
begin
  Result:=UTF8Encode(S);
end;


(* -------------------------------------------------------------------------- *)

function UpCaseW(const C: WideChar): WideChar;
var
  w: widestring;
begin
  w:=C;
  W := UpCase(W);
  Result:=w[1];
end;

(* -------------------------------------------------------------------------- *)

function LowCaseW(const C: WideChar): WideChar;
var
  w: widestring;
begin
  w:=C;
  W := LowerCase(W);
  Result:=w[1];
end;

(* -------------------------------------------------------------------------- *)

function UpperCaseW(const S: WideString): WideString;
begin
  Result:=UpCase(S);
end;

(* -------------------------------------------------------------------------- *)

function LowerCaseW(const S: WideString): WideString;
begin
  Result:=LowerCase(S);
end;

(* -------------------------------------------------------------------------- *)

function GetCurDir: WideString;
begin
  Result:=UTF8Decode(GetCurrentDirUTF8);
end;

(* -------------------------------------------------------------------------- *)

procedure SetCurDir(const Dir: WideString);
begin
  if not SetCurrentDirUTF8(UTF8Encode(Dir)) then
  begin
    raise EInOutError.Create('Cannot change to directory "%s". %s');
  end;
end;

(* -------------------------------------------------------------------------- *)

function FileCreateW(const FileName: WideString): Integer;
begin
  Result := Integer(FileCreate(FileName, fmOpenReadWrite));
end;

(* -------------------------------------------------------------------------- *)

function FileOpenW(const FileName: WideString; Mode: LongWord): Integer;
begin
  Result:=Integer(FileOpen(FileName, Mode))
end;

(* -------------------------------------------------------------------------- *)

function WideFileSetDate(const FileName: WideString; Age: LongInt): LongInt;
begin
  Result:=FileSetDateUTF8(FileName, Age);
end;

(* -------------------------------------------------------------------------- *)
{ TFileStreamW }

{constructor TFileStreamW.Create(const FileName: WideString; Mode: Word);
begin
  if Mode = fmCreate then
  begin
    inherited Create(FileCreateW(FileName));
    if FHandle < 0 then
      raise EFCreateError.CreateResFmt(@SFCreateErrorEx,
        [ExpandFileName(FileName), SysErrorMessage(GetLastError)]);
  end
  else
  begin
    inherited Create(FileOpenW(FileName, Mode));
    if FHandle < 0 then
      raise EFOpenError.CreateResFmt(@SFOpenErrorEx, [ExpandFileName(FileName),
        SysErrorMessage(GetLastError)]);
  end;
end;

(* -------------------------------------------------------------------------- *)

destructor TFileStreamW.Destroy;
begin
  if FHandle >= 0 then
    FileClose(FHandle);
  inherited Destroy;
end; }

(* -------------------------------------------------------------------------- *)

end.


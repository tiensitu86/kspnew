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

unit app_sql;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function TB_ARTISTS: string;
function TB_META: string;
function TB_LYRICS: string;

implementation

const
  TB_LYRICS_C_LITE = 'CREATE TABLE `lyrics` ('+
  '`lyric` TEXT,'+
  '`item_id` INTEGER NOT NULL DEFAULT ''-1'''+
  ')';

  TB_ARTISTS_C_LITE = 'CREATE TABLE `artists` ('+
  '`ArtistRelated` VARCHAR(%s) NOT NULL,'+
  '`ArtistTop` VARCHAR(%s) NOT NULL,'+
  '`LastRefreshData` VARCHAR(127) NOT NULL,'+
  '`LastRefreshRelated` VARCHAR(127) NOT NULL,'+
  '`Artist` VARCHAR(127) NOT NULL'+
  ')';

  TB_META_C_LITE = 'CREATE TABLE `meta` ('+
  '`I_NAME` INTEGER PRIMARY KEY AUTOINCREMENT,'+
  '`Genre` VARCHAR(127),'+
  '`FileName` VARCHAR(%s),'+
  '`GID` INTEGER UNSIGNED,'+
  '`Track` INTEGER UNSIGNED,'+
  '`Comment` TEXT,'+
  '`MetaYear` VARCHAR(32),'+
  '`Album` VARCHAR(127),'+
  '`Artist` VARCHAR(127),'+
  '`Title` VARCHAR(127),'+
  '`PlayedEver` BOOLEAN,'+
  '`Meta` SMALlINT,'+
  '`PlayCount` INTEGER UNSIGNED NOT NULL,'+
  '`Fav` FLOAT NOT NULL,'+
  '`LastPlay` VARCHAR(127) NOT NULL,'+
  '`FirstPlay` VARCHAR(127) NOT NULL'+
  ')';


function TB_ARTISTS: string;
begin
  Result:=(Format(TB_ARTISTS_C_LITE, [IntToStr(MAX_PATH), IntToStr(MAX_PATH)]))
end;

function TB_LYRICS: string;
begin
  Result:=TB_LYRICS_C_LITE
end;

function TB_META: string;
begin
  Result:=(Format(TB_META_C_LITE, [IntToStr(MAX_PATH)]))
end;

end.


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


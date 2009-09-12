unit app_sql;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function TB_ARTISTS(sqlite: boolean): string;
function TB_META(sqlite: boolean): string;
function TB_LYRICS(sqlite: boolean): string;

implementation

const  TB_ARTISTS_C = 'CREATE TABLE `artists` ('+
  '`ArtistRelated` VARCHAR(%s) NOT NULL,'+
  '`ArtistTop` VARCHAR(%s) NOT NULL,'+
  '`LastRefreshData` VARCHAR(127) NOT NULL,'+
  '`LastRefreshRelated` VARCHAR(127) NOT NULL,'+
  '`Artist` VARCHAR(127) NOT NULL'+
  ') ENGINE = InnoDB;';

  TB_META_C = 'CREATE TABLE `meta` ('+
  '`I_NAME` INTEGER UNSIGNED NOT NULL AUTO_INCREMENT,'+
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
  '`FirstPlay` VARCHAR(127) NOT NULL,'+
  'PRIMARY KEY (`I_NAME`)'+
  ') ENGINE = InnoDB;';


  TB_LYRICS_C_LITE = 'CREATE TABLE `lyrics` ('+
  '`lyric` MEMO,'+
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


function TB_ARTISTS(sqlite: boolean): string;
begin
  if sqlite then
    Result:=(Format(TB_ARTISTS_C_LITE, [IntToStr(MAX_PATH), IntToStr(MAX_PATH)]))
  else
    Result:=(Format(TB_ARTISTS_C, [IntToStr(MAX_PATH), IntToStr(MAX_PATH)]))
end;

function TB_LYRICS(sqlite: boolean): string;
begin
  if sqlite then
    Result:=TB_LYRICS_C_LITE
  else
    Result:='';
end;

function TB_META(sqlite: boolean): string;
begin
  if sqlite then
    Result:=(Format(TB_META_C_LITE, [IntToStr(MAX_PATH)]))
  else
    Result:=(Format(TB_META_C, [IntToStr(MAX_PATH)]))
end;

end.


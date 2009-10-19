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

unit FileSupport;

interface

uses MPEGAudio, OggVorbis, {$IFDEF WINDOWS}WMAFile, {$ENDIF}WAVFile, Classes,
    SysUtils, Dialogs, AACfile, ID3v1, ID3v2, KSPMessages, Musepack;

const MAXCDDRIVES = 10;

//type TSupportedBy = (Both, BASSNative, WinampPlugin, None);
type TSupportedBy = (Both, BASSNative, None);
   TStreamInfo = record
     FileName : string;
     FileSize : DWORD;        // File size in byte
     SampleRate : DWORD;      // Sampling rate in Hz
     BitRate : DWORD;         // Bit Rate in KBPS
     BitsPerSample : Word;    // Bits per sample
     Duration : DWORD;        // playback duration in mili second
     Channels : Word;         // 1- Mono, 2 - Stereo
     Format   : DWORD;        // Stream format   // * Added at Ver 1.44
     Title : string;
     Artist : string;
     Album : string;
     Year : string;
     Genre : string;
     GenreID : byte;
     Track : byte;
     Comment : string;
  end;

type TCDDriveList     = array[0..MAXCDDRIVES-1] of string[255];
{$IFNDEF KSP_PLUGINS}
function FileInfoBox(StreamName : string): Boolean;
{$ENDIF}

function GetStreamInfo2(StreamName : string;
                       var StreamInfo : TStreamInfo;
                       var SupportedBy : TSupportedBy) : boolean;

function GetDuration(FileName: WideString): integer; overload;
function GetDuration(Stream: TStreamInfo): integer; overload;
{$IFNDEF KSP_PLUGINS}
function IsSupported(FileName: string): boolean;
{$ENDIF}

implementation

uses {$IFNDEF KSP_PLUGINS} KSPConstsVars, {$ENDIF}KSPFiles;

{$IFNDEF KSP_PLUGINS}
function IsSupported(FileName: string): boolean;
begin
  Result:=FileSupportList.FindExtension(ExtractFileExt(FileName), false)>-1
end;
{$ENDIF}

procedure FillMPCTag(var StreamInfo: TStreamInfo; MPC: TMPEGplus);
var
  i: integer;
begin
  for i:=0 to Length(MPC.APEtag.Fields)-1 do begin
      if UpperCase(MPC.APEtag.Fields[i].Name)='TRACK' then
        StreamInfo.Track:=StrToInt(UTF8Decode(MPC.APEtag.Fields[i].Value));
      if UpperCase(MPC.APEtag.Fields[i].Name)='ALBUM' then
        StreamInfo.Album:=UTF8Decode(MPC.APEtag.Fields[i].Value);
      if UpperCase(MPC.APEtag.Fields[i].Name)='ARTIST' then
        StreamInfo.Artist:=UTF8Decode(MPC.APEtag.Fields[i].Value);
      if UpperCase(MPC.APEtag.Fields[i].Name)='COMMENT' then
        StreamInfo.Comment:=UTF8Decode(MPC.APEtag.Fields[i].Value);
      if UpperCase(MPC.APEtag.Fields[i].Name)='GENRE' then
        StreamInfo.Genre:=UTF8Decode(MPC.APEtag.Fields[i].Value);
      if UpperCase(MPC.APEtag.Fields[i].Name)='TITLE' then
        StreamInfo.Title:=UTF8Decode(MPC.APEtag.Fields[i].Value);
      if UpperCase(MPC.APEtag.Fields[i].Name)='YEAR' then
        StreamInfo.Year:=UTF8Decode(MPC.APEtag.Fields[i].Value);
    end;

  for i:=0 to MAX_MUSIC_GENRES-1 do
    if UpperCase(aTAG_MusicGenre[i])=UpperCase(StreamInfo.Genre) then
      StreamInfo.GenreID:=i;
end;

function GetStreamInfo2(StreamName : string;
                       var StreamInfo : TStreamInfo;
                       var SupportedBy : TSupportedBy) : boolean;
begin
   Result:=Player.GetStreamInfo(StreamName, StreamInfo, SupportedBy);
end;

function GetDuration(FileName: WideString): integer;
var
  Sup: TSupportedBy;
  StreamInfo2 : TStreamInfo;
  Pc: TPathChar;
begin
  Result:=0;
  StrPCopy(Pc, FileName);
  if not FileExists(FileName) and (not IsCD(Pc)) then Exit;
  if GetStreamInfo2(FileName, StreamInfo2, Sup) then
    Result:=StreamInfo2.Duration;
end;

{$IFNDEF KSP_PLUGINS}
function FileInfoBox(StreamName : string): Boolean;
begin
  Result:=Player.FileInfoBox(StreamName);
end;
{$ENDIF}

function GetDuration(Stream: TStreamInfo): integer; overload;
begin
  Result:=Stream.Duration;
end;

end.
 

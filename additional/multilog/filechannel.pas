unit filechannel;

{ Copyright (C) 2006 Luiz Américo Pereira Câmara

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  {$ifndef fpc}fpccompat,{$endif} Classes, SysUtils, multilog, KSPThreadUtils;//{$IFDEF KSPDEBUG}{$ENDIF};

type

  { TFileChannel }

  TFileChannel = class (TLogChannel)
  private
//    FFileHandle: Text;
    FCritical: TRTLCriticalSection;
    FFileName: String;
    FRelativeIdent: Integer;
    FBaseIdent: Integer;
    FShowHeader: Boolean;
    FShowTime: Boolean;
    FShowPrefix: Boolean;
    FShowStrings: Boolean;
    FQueueStr: string;
    procedure SetShowTime(const AValue: Boolean);
    procedure UpdateIdentation;
  public
    constructor Create (const AFileName: String);
    destructor Destroy; override;
    procedure Deliver(const AMsg: TLogMessage);override;
    procedure Init; override;
    property ShowHeader: Boolean read FShowHeader write FShowHeader;
    property ShowPrefix: Boolean read FShowPrefix write FShowPrefix;
    property ShowTime: Boolean read FShowTime write SetShowTime;
  end;

implementation

const
  LogPrefixes: array [ltInfo..ltCounter] of String = (
    'INFO',
    'ERROR',
    'WARNING',
    'VALUE',
    '>>ENTER METHOD',
    '<<EXIT METHOD',
    'CONDITIONAL',
    'CHECKPOINT',
    'STRINGS',
    'CALL STACK',
    'OBJECT',
    'EXCEPTION',
    'BITMAP',
    'HEAP INFO',
    'MEMORY',
    '','','','','',
    'WATCH',
    'COUNTER');

{ TFileChannel }

procedure TFileChannel.UpdateIdentation;
var
  S:String;
begin
  S:='';
  if FShowTime then
    S:=FormatDateTime('hh:nn:ss:zzz',Time);
  FBaseIdent:=Length(S)+3;
end;

procedure TFileChannel.SetShowTime(const AValue: Boolean);
begin
  FShowTime:=AValue;
  UpdateIdentation;
end;

constructor TFileChannel.Create(const AFileName: String);
begin
  FShowPrefix := True;
  FShowTime := True;
  FShowStrings := True;
  Active := True;
  FFileName := AFileName;
  InitCriticalSection(FCritical);
end;

destructor TFileChannel.Destroy;
begin
  //remove it?
  DoneCriticalsection(FCritical);
end;

procedure TFileChannel.Deliver(const AMsg: TLogMessage);
var
  Text: string;
begin
  if FShowTime then
    Text:=FormatDateTime('hh:nn:ss:zzz',AMsg.MsgTime)+' ';
  Text:=Text+Space(FRelativeIdent);
  if FShowPrefix then
    Text:=Text+LogPrefixes[AMsg.MsgType]+': ';

  Text:=Text+AMsg.MsgText;
  DebuglnThreadLog([Text]);
end;

procedure TFileChannel.Init;
begin
  FQueueStr:='';
  UpdateIdentation;
end;

end.


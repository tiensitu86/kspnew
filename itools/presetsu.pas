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

unit PresetsU;

interface

{$mode objfpc}{$H+}

uses SysUtils, Classes;

const
  NumEQBands = 10;

  MaxChannels = 10;

  EQFreq: array[0..NumEQBands] of word = (
    0, 3, 9, 16, 29, 48, 100, 141, 280, 559, 1024
    );

type
  TPreset = record
    vals: array[0..NumEQBands - 1] of single;
    Name: string;
    FromDefault: boolean;
  end;

  TPresetInfo = class(TObject)
  public
    Entry: TPreset;
  end;

  TEqList = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Entry: TPreset);
    procedure Remove(Index: integer);
    function GetItem(Index: integer): TPreset;
    procedure LoadFromFile(FileName: string; SetAsFromDef: boolean = False);
    procedure SaveToFile(FileName: string);
    procedure ChangeVals(Index: integer; Vals: TPreset);
  end;

implementation

uses Multilog;

constructor TEqList.Create;
begin
  inherited Create;
end;

{The Items should be freed here but it isn't. Doesn't matter.
TPlayList is created only once and destroyed only while KSP is
to be closed}

destructor TEqList.Destroy;
var
  i: integer;
begin
  for I := 0 to Count - 1 do
    TPresetInfo(Items[I]).Free;
  inherited Destroy;
end;

procedure TEqList.Add(Entry: TPreset);
var
  T: TPresetInfo;
begin
  T := TPresetInfo.Create;
  T.Entry := Entry;
  inherited Add(T);
end;

procedure TEqList.Remove(Index: integer);
begin
  TPresetInfo(Items[Index]).Free;
  Delete(Index);
end;

function TEqList.GetItem(Index: integer): TPreset;
begin
  Result := TPresetInfo(Items[Index]).Entry;
end;

procedure TEqList.LoadFromFile(FileName: string; SetAsFromDef: boolean = False);
var
  f:    textfile;
  s, sname, sval: string;
  Vals: TPreset;
  aposition: integer;
  i:    integer;
begin
  hLog.Send('Loading presets');
  AssignFile(f, FileName);
  if FileExists(FileName) then
  begin
    Reset(f);
    while not EOF(f) do
    begin
      Readln(f, s);
      sname := Copy(s, 0, Pos('=', s) - 1);
      hLog.Send(sname);
      sval := Copy(s, Pos('=', s) + 1, Length(s));
      //ShowMessage(sval);

      vals.Name := sname;
      //i:=0;
      //aposition:=0;
      for aposition := 0 to NumEQBands - 2 do
      begin
        //pi:=i;
        i := Pos(',', sval);
        if i = 0 then
          Break;
        s := Copy(sval, 0, i - 1);
        Vals.vals[aposition] := StrToFloat(s);
        //ShowMessage(IntToStr(vals[aposition]));
        System.Delete(sval, 1, Pos(',', sval));
      end;

      Vals.vals[NumEQBands - 1] := StrToFloat(sval);
      Vals.FromDefault := SetAsFromDef;

      Add(Vals);
      //EqPresets.Items.Add(vals.name);
      //T:=TMenuItem.Create(Self);
      //T.Caption:=Vals.name;
      //T.Tag:=EqPresets.Items.Count-1;
      //T.RadioItem:=true;
      //T.OnClick:=EqClick;
      //EqualizerMenu.Add(T);

    end;
    CloseFile(f);
  end;
  hLog.Send('Loading presets finished');
end;

procedure TEqList.SaveToFile(FileName: string);
var
  s:   TStringList;
  i:   integer;
  str: string;
begin
  if FileExists(FileName) then
    DeleteFile(FileName);
  if Count = 0 then
    Exit;
  s := TStringList.Create;

  for i := 0 to Count - 1 do
  begin
    if GetItem(i).FromDefault then
      Continue;
    str := GetItem(i).Name + '=';
    //for x := 0 to NumEQBands - 1 do
    //  str:=str+IntToStr(GetItem(i).vals[x])+',';

    System.Delete(str, Length(str), 1);
    s.Add(str);
  end;
  if s.Count > 0 then
    s.SaveToFile(FileName);
  s.Free;
end;

procedure TEqList.ChangeVals(Index: integer; Vals: TPreset);
begin
  if Index < 0 then
    Exit;
  if Index >= Count then
    Exit;
  TPresetInfo(Items[Index]).Entry := Vals;
end;

end.

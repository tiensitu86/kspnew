unit PresetsU;

interface

{$mode objfpc}{$H+}

uses SysUtils, Classes;

const
  NumEQBands = 10;

  MaxChannels = 10;

  EQFreq : array[0..NumEQBands] of Word = (
    0, 3, 9, 16, 29, 48, 100, 141, 280, 559, 1024
  );

type TPreset = record
        vals: array[0..NumEQBands -1] of ShortInt;
        name: string;
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
    procedure Remove(Index: Integer);
    function GetItem(Index: Integer): TPreset;
    procedure LoadFromFile(FileName: string; SetAsFromDef: boolean = false);
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
  for I := 0 to Count-1 do
    TPresetInfo(Items[I]).Free;
  inherited Destroy;
end;

procedure TEqList.Add(Entry: TPreset);
var
  T: TPresetInfo;
begin
  T:=TPresetInfo.Create;
  T.Entry:=Entry;
  inherited Add(T);
end;

procedure TEqList.Remove(Index: Integer);
begin
  TPresetInfo(Items[Index]).Free;
  Delete(Index);
end;

function TEqList.GetItem(Index: Integer): TPreset;
begin
  Result:=TPresetInfo(Items[Index]).Entry;
end;

procedure TEqList.LoadFromFile(FileName: string; SetAsFromDef: boolean = false);
var
  f: textfile;
  s, sname, sval: string;
  Vals: TPreset;
  aposition: integer;
  i: Integer;
begin
    hLog.Send('Loading presets');
    AssignFile(f, FileName);
    if FileExists(FileName) then begin
        Reset(f);
        while not Eof(f) do begin
            Readln(f, s);
            sname:=Copy(s, 0, Pos('=', s)-1);
            hLog.Send(sname);
            sval:=Copy(s, Pos('=', s)+1, Length(s));
            //ShowMessage(sval);

            vals.name:=sname;
           //i:=0;
           //aposition:=0;
           for aposition:=0 to NumEQBands-2 do begin
                //pi:=i;
                i:=Pos(',', sval);
                if i=0 then Break;
                s:=Copy(sval, 0, i-1);
                Vals.vals[aposition]:=StrToInt(s);
                //ShowMessage(IntToStr(vals[aposition]));
                System.Delete(sval,1, Pos(',', sval));
              end;

              Vals.vals[NumEQBands-1]:=StrToInt(sval);
           Vals.FromDefault:=SetAsFromDef;

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
  s: TStringList;
  i, x: integer;
  str: string;
begin
  if FileExists(FileName) then
    DeleteFile(FileName);
  if Count=0 then Exit;
  s:=TStringList.Create;

  for i := 0 to Count - 1 do
    begin
      if GetItem(i).FromDefault then Continue;      
      str:=GetItem(i).name+'=';
      for x := 0 to NumEQBands - 1 do
        str:=str+IntToStr(GetItem(i).vals[x])+',';

      System.Delete(str, Length(str), 1);
      s.Add(str);
    end;
  if s.Count>0 then
    s.SaveToFile(FileName);
  s.Free;
end;

procedure TEqList.ChangeVals(Index: integer; Vals: TPreset);
begin
  if Index<0 then Exit;
  if Index>=Count then Exit;
  TPresetInfo(Items[Index]).Entry:=Vals;
end;

end.

unit KSPThreadUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DateUtils, profilefunc;

function DbgS(const c: cardinal): string; overload;
function DbgS(const i: longint): string; overload;
function DbgS(const i: int64): string; overload;
function DbgS(const q: qword): string; overload;
function DbgS(const r: TRect): string; overload;
function DbgS(const p: TPoint): string; overload;
function DbgS(const p: pointer): string; overload;
function DbgS(const e: extended; MaxDecimals: integer = 999): string; overload;
function DbgS(const b: boolean): string; overload;
function DbgS(const s: TComponentState): string; overload;
function DbgSName(const p: TObject): string; overload;
function DbgSName(const p: TClass): string; overload;

procedure DebuglnThreadLog(Args: array of const);

implementation

uses KSPConstsVars;

{$HINTS OFF}

function DbgS(const c: cardinal): string;
begin
  Result:=IntToStr(c);
end;

function DbgS(const i: longint): string;
begin
  Result:=IntToStr(i);
end;

function DbgS(const i: int64): string;
begin
  Result:=IntToStr(i);
end;

function DbgS(const q: qword): string;
begin
  Result:=IntToStr(q);
end;

function DbgS(const r: TRect): string;
begin
  Result:=' x1='+IntToStr(r.Left)+',y1='+IntToStr(r.Top)
         +',x2='+IntToStr(r.Right)+',y2='+IntToStr(r.Bottom);
end;

function DbgS(const p: TPoint): string;
begin
  Result:='(x='+IntToStr(p.x)+',y='+IntToStr(p.y)+')';
end;

function DbgS(const p: pointer): string;
begin
  Result:=HexStr(PtrUInt(p),2*sizeof(PtrInt));
end;

function DbgS(const e: extended; MaxDecimals: integer): string;
begin
  Result:=copy(FloatToStr(e),1,MaxDecimals);
end;

function DbgS(const b: boolean): string;
begin
  if b then Result:='True' else Result:='False';
end;

function DbgS(const s: TComponentState): string;

  procedure Add(const a: string);
  begin
    if Result<>'' then
      Result:=Result+',';
    Result:=Result+a;
  end;

begin
  Result:='';
  if csLoading in s then Add('csLoading');
  if csReading in s then Add('csReading');
  if csWriting in s then Add('csWriting');
  if csDestroying in s then Add('csDestroying');
  if csDesigning in s then Add('csDesigning');
  if csAncestor in s then Add('csAncestor');
  if csUpdating in s then Add('csUpdating');
  if csFixups in s then Add('csFixups');
  if csFreeNotification in s then Add('csFreeNotification');
  if csInline in s then Add('csInline');
  if csDesignInstance in s then Add('csDesignInstance');
  Result:='['+Result+']';
end;

function DbgSName(const p: TObject): string;
begin
  if p=nil then
    Result:='nil'
  else if p is TComponent then
    Result:=TComponent(p).Name+':'+p.ClassName
  else
    Result:=p.ClassName;
end;

function DbgSName(const p: TClass): string;
begin
  if p=nil then
    Result:='nil'
  else
    Result:=p.ClassName;
end;

procedure DbgOutThreadLog(const Msg: string);
var
  PID: PtrInt;
  fs: TFileStream;
  Filename: string;
begin
  PID:=PtrInt(GetThreadID);
  Filename:=KSPLogFilename+'\Log'+IntToStr(PID);
  FixFolderNames(FileName);
  if FileExistsUTF8(Filename) then
    fs:=TFileStream.Create(UTF8ToSys(Filename),fmOpenWrite)
  else
    fs:=TFileStream.Create(UTF8ToSys(Filename),fmCreate);
  fs.Position:=fs.Size;
  fs.Write(Msg[1], length(Msg));
  fs.Free;
end;

procedure DebuglnThreadLogS(const Msg: string);
var
  PID: PtrInt;
begin
  PID:=PtrInt(GetThreadID);
  DbgOutThreadLog(IntToStr(PtrInt(PID))+' : '+Msg+LineEnding);
end;

procedure DebuglnThreadLog(Args: array of const);
var
  i: Integer;
  s: String;
begin
  s:='';
  for i:=Low(Args) to High(Args) do begin
    case Args[i].VType of
    vtInteger: s:=s+dbgs(Args[i].vinteger);
    vtInt64: s:=s+dbgs(Args[i].VInt64^);
    vtQWord: s:=s+dbgs(Args[i].VQWord^);
    vtBoolean: s:=s+dbgs(Args[i].vboolean);
    vtExtended: s:=s+dbgs(Args[i].VExtended^);
{$ifdef FPC_CURRENCY_IS_INT64}
    // MWE:
    // ppcppc 2.0.2 has troubles in choosing the right dbgs()
    // so we convert here (i don't know about other versions
    vtCurrency: s:=s+dbgs(int64(Args[i].vCurrency^)/10000, 4);
{$else}
    vtCurrency: s:=s+dbgs(Args[i].vCurrency^);
{$endif}
    vtString: s:=s+Args[i].VString^;
    vtAnsiString: s:=s+AnsiString(Args[i].VAnsiString);
    vtChar: s:=s+Args[i].VChar;
    vtPChar: s:=s+Args[i].VPChar;
    vtPWideChar: s:=s+Args[i].VPWideChar;
    vtWideChar: s:=s+Args[i].VWideChar;
    vtWidestring: s:=s+WideString(Args[i].VWideString);
    vtObject: s:=s+DbgSName(Args[i].VObject);
    vtClass: s:=s+DbgSName(Args[i].VClass);
    vtPointer: s:=s+Dbgs(Args[i].VPointer);
    else
      DbgOutThreadLog('?unknown variant?');
    end;
  end;
  DebuglnThreadLogS(s);
end;

{$HINTS ON}

end.


unit MultiLog;

{
  Main unit of the Multilog logging system

  Copyright (C) 2006 Luiz Américo Pereira Câmara
  pascalive@bol.com.br

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

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
  Forms, Classes, SysUtils, Support, KSPFiles, Dialogs, KSPThreadUtils;

const
  //MessageTypes
  //mt (Message Type) and lt (Log Type) prefixes are used elsewhere
  //but mt is worse because there's already mtWarning and mtInformation
  //the existing lt* do not makes confusion
  ltInfo    = 0;
  ltError   = 1;
  ltWarning = 2;
  ltValue   = 3;
  ltEnterMethod = 4;
  ltExitMethod  = 5;
  ltConditional = 6;
  ltCheckpoint = 7;
  ltStrings = 8;
  ltCallStack = 9;
  ltObject = 10;
  ltException = 11;
  ltBitmap = 12;
  ltHeapInfo = 13;
  ltMemory = 14;
  ltCustomData = 15;
  ltWatch = 20;
  ltCounter = 21;


  ltClear=100;

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
  
type
  TLogger = class;

  TLogMessage = record
    MsgType: Integer;
    MsgTime: TDateTime;
    MsgText: String;
    Data: TStream;
  end;

  { TLogger }

  TLogger = class
  private
    FMaxStackCount: Integer;
    FLogStack: TStrings;
    FCheckList: TStringList;
    FCounterList: TStringList;
    FEnabled: boolean;
    procedure GetCallStack(AStream:TStream);
    procedure SetEnabled(AValue: Boolean);
    function GetEnabled: Boolean;
    procedure Deliver(const AMsg: TLogMessage);
    procedure SetMaxStackCount(const AValue: Integer);
  protected
    procedure SendStream(AMsgType: Integer;const AText:String; AStream: TStream);
    procedure SendBuffer(AMsgType: Integer;const AText:String;
      var Buffer; Count: LongWord);
  public
    constructor Create;
    destructor Destroy; override;
    function CalledBy(const AMethodName: String): Boolean;
    //Helper functions
    function RectToStr(const ARect: TRect): String; //inline
    function PointToStr(const APoint: TPoint): String; //inline
    //Send functions
    procedure Send(const AText: String); inline;
    procedure SendPointer(const AText: String; APointer: Pointer); overload; {$ifdef fpc}inline;{$endif}
    procedure SendCallStack(const AText: String); overload; {$ifdef fpc}inline;{$endif}
    procedure SendException(const AText: String; AException: Exception);overload; {$ifdef fpc}inline;{$endif}
    procedure SendHeapInfo(const AText: String); overload; {$ifdef fpc}inline;{$endif}
    procedure SendMemory(const AText: String; Address: Pointer; Size: LongWord); overload; {$ifdef fpc}inline;{$endif}
    procedure SendIf(const AText: String; Expression: Boolean); overload; {$ifdef fpc}inline;{$endif}
    procedure SendIf(const AText: String; Expression, IsTrue: Boolean); overload; {$ifdef fpc}inline;{$endif}
    procedure SendWarning(const AText: String); overload; {$ifdef fpc}inline;{$endif}
    procedure SendError(const AText: String); overload; {$ifdef fpc}inline;{$endif}
    procedure AddCheckPoint;overload; {$ifdef fpc}inline;{$endif}
    procedure AddCheckPoint(const CheckName: String);overload; {$ifdef fpc}inline;{$endif}
    procedure IncCounter(const CounterName: String);overload; {$ifdef fpc}inline;{$endif}
    procedure DecCounter(const CounterName: String);overload; {$ifdef fpc}inline;{$endif}
    procedure ResetCounter(const CounterName: String);overload; {$ifdef fpc}inline;{$endif}
    function GetCounter(const CounterName: String): Integer;
    procedure ResetCheckPoint;overload; {$ifdef fpc}inline;{$endif}
    procedure ResetCheckPoint(const CheckName: String);overload; {$ifdef fpc}inline;{$endif}
    procedure EnterMethod(const AMethodName: String); overload; {$ifdef fpc}inline;{$endif}
    procedure EnterMethod(Sender: TObject; const AMethodName: String); overload; {$ifdef fpc}inline;{$endif}
    procedure ExitMethod(const AMethodName: String); overload; {$ifdef fpc}inline;{$endif}
    procedure ExitMethod(Sender: TObject; const AMethodName: String); overload; {$ifdef fpc}inline;{$endif}
    procedure Watch(const AText, AValue: String); overload; {$ifdef fpc}inline;{$endif}
    procedure Watch(const AText: String; AValue: Integer); overload; {$ifdef fpc}inline;{$endif}
    {$ifdef fpc}
    procedure Watch(const AText: String; AValue: Cardinal); overload; {$ifdef fpc}inline;{$endif}
    {$endif}
    procedure Watch(const AText: String; AValue: Double); overload; {$ifdef fpc}inline;{$endif}
    procedure Watch(const AText: String; AValue: Boolean); overload; {$ifdef fpc}inline;{$endif}
    procedure AppException(Sender: TObject; E: Exception);
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property LogStack: TStrings read FLogStack;
    property MaxStackCount: Integer read FMaxStackCount write SetMaxStackCount;
  end;

var
  hLog: TLogger;

implementation

uses KSPConstsVars;

const
  DefaultCheckName = 'CheckPoint';

function FormatNumber (Value: Integer):String;
var
  TempStr:String;
  i,Digits:Integer;
begin
  Digits:=0;
  Result:='';
  TempStr:=IntToStr(Value);
  for i := length(TempStr) downto 1 do
  begin
    //todo: implement using mod() -> get rids of digits
    if Digits = 3 then
    begin
      Digits:=0;
      Result:=ThousandSeparator+Result;
    end;
    Result:=TempStr[i]+Result;
    Inc(Digits);
  end;
end;

{ TLogger }

procedure TLogger.GetCallStack(AStream: TStream);
{$ifdef fpc}
var
  i : Longint;
  prevbp : Pointer;
  caller_frame,
  caller_addr,
  bp : Pointer;
  S:String;
{$endif}
begin
  {$ifdef fpc}
  //routine adapted from fpc source

  //This trick skip SendCallstack item
  //bp:=get_frame;
  bp:= get_caller_frame(get_frame);
  try
    prevbp:=bp-1;
    i:=0;
    //is_dev:=do_isdevice(textrec(f).Handle);
    while bp > prevbp Do
     begin
       caller_addr := get_caller_addr(bp);
       caller_frame := get_caller_frame(bp);
       if (caller_addr=nil) then
         break;
       //todo: see what is faster concatenate string and use writebuffer or current
       S:=BackTraceStrFunc(caller_addr)+LineEnding;
       AStream.WriteBuffer(S[1],Length(S));
       Inc(i);
       if (i>=FMaxStackCount) or (caller_frame=nil) then
         break;
       prevbp:=bp;
       bp:=caller_frame;
     end;
   except
     { prevent endless dump if an exception occured }
   end;
  {$endif} 
end;

procedure TLogger.SetEnabled(AValue: Boolean);
begin
  FEnabled:=AValue;
end;

function TLogger.GetEnabled: Boolean;
begin
  Result:=FEnabled;
end;

procedure TLogger.Deliver(const AMsg: TLogMessage);
var
  Text: string;
begin
  Text:=FormatDateTime('hh:nn:ss:zzz',AMsg.MsgTime)+' ';
  Text:=Text+Space(1);

  Text:=Text+LogPrefixes[AMsg.MsgType]+': ';

  Text:=Text+AMsg.MsgText;
  DebuglnThreadLog([Text]);
end;

procedure TLogger.SendStream(AMsgType: Integer; const AText: String;
  AStream: TStream);
var
  MsgRec: TLogMessage;
begin
  with MsgRec do
  begin
    MsgType:=AMsgType;
    MsgTime:=Now;
    MsgText:=AText;
    Data:=AStream;
  end;
  Deliver(MsgRec);
  AStream.Free;
end;

procedure TLogger.SendBuffer(AMsgType: Integer; const AText: String;
  var Buffer; Count: LongWord);
var
  AStream: TStream;
begin
  if Count > 0 then
  begin
    AStream:=TMemoryStream.Create;
    AStream.Write(Buffer,Count);
  end
  else
    AStream:=nil;
  //SendStream free AStream
  SendStream(AMsgType,AText,AStream);
end;

procedure TLogger.SetMaxStackCount(const AValue: Integer);
begin
  if AValue < 256 then
    FMaxStackCount := AValue
  else
    FMaxStackCount := 256;
end;

constructor TLogger.Create;
begin
  FMaxStackCount := 20;
  FLogStack := TStringList.Create;
  FCheckList := TStringList.Create;
  with FCheckList do
  begin
    CaseSensitive := False;
    Sorted := True; //Faster IndexOf?
  end;
  FCounterList := TStringList.Create;
  with FCounterList do
  begin
    CaseSensitive := False;
    Sorted := True; //Faster IndexOf?
  end;

  Application.OnException:=@AppException;
end;

destructor TLogger.Destroy;
begin
  Application.OnException:=nil;
  FLogStack.Destroy;
  FCheckList.Destroy;
  FCounterList.Destroy;
end;

function TLogger.CalledBy(const AMethodName: String): Boolean;
begin
  Result:=FLogStack.IndexOf(UpperCase(AMethodName)) <> -1;
end;

function TLogger.RectToStr(const ARect: TRect): String;
begin
  with ARect do
    Result:=Format('(Left: %d; Top: %d; Right: %d; Bottom: %d)',[Left,Top,Right,Bottom]);
end;

function TLogger.PointToStr(const APoint: TPoint): String;
begin
  with APoint do
    Result:=Format('(X: %d; Y: %d)',[X,Y]);
end;

procedure TLogger.Send(const AText: String);
begin
  SendStream(ltInfo,AText,nil);
end;

procedure TLogger.SendPointer(const AText: String; APointer: Pointer);
begin
  SendStream(ltValue,AText+' = '+IntToHex(Integer(APointer),8),nil);
end;

procedure TLogger.SendCallStack(const AText: String);
var
  AStream: TStream;
begin
  AStream:=TMemoryStream.Create;
  GetCallStack(AStream);
  //SendStream free AStream
  SendStream(ltCallStack,AText,AStream);
end;

procedure TLogger.SendException(const AText: String; AException: Exception);
var
  i: Integer;
  Frames: PPointer;
  S:String;
begin
  if AException <> nil then
    S:=AException.ClassName+' - '+AException.Message+LineEnding;
  S:= S + BackTraceStrFunc(ExceptAddr);
  Frames:=ExceptFrames;
  for i:= 0 to ExceptFrameCount - 1 do
    S:= S + (LineEnding+BackTraceStrFunc(Frames[i]));
  SendBuffer(ltException,AText,S[1],Length(S));
end;

procedure TLogger.SendHeapInfo(const AText: String);
var
  S: String;
begin
  with GetFPCHeapStatus do
  begin
    S:='MaxHeapSize: '+FormatNumber(MaxHeapSize)+LineEnding
      +'MaxHeapUsed: '+FormatNumber(MaxHeapUsed)+LineEnding
      +'CurrHeapSize: '+FormatNumber(CurrHeapSize)+LineEnding
      +'CurrHeapUsed: '+FormatNumber(CurrHeapUsed)+LineEnding
      +'CurrHeapFree: '+FormatNumber(CurrHeapFree);
  end;
  SendBuffer(ltHeapInfo,AText,S[1],Length(S));
end;

procedure TLogger.SendMemory(const AText: String; Address: Pointer;
  Size: LongWord);
begin
  SendBuffer(ltMemory,AText,Address^,Size);
end;

procedure TLogger.SendIf(const AText: String; Expression: Boolean);
begin
  SendIf(AText,Expression,True);
end;

procedure TLogger.SendIf(const AText: String; Expression, IsTrue: Boolean);
begin
  if (Expression <> IsTrue) then Exit;
  SendStream(ltConditional,AText,nil);
end;

procedure TLogger.SendWarning(const AText: String);
begin
  SendStream(ltWarning,AText,nil);
end;

procedure TLogger.SendError(const AText: String);
begin
  SendStream(ltError,AText,nil);
end;

procedure TLogger.AddCheckPoint;
begin
  AddCheckPoint(DefaultCheckName);
end;

procedure TLogger.AddCheckPoint(const CheckName: String);
var
  i,j: Integer;
begin
  i:=FCheckList.IndexOf(CheckName);
  if i <> -1 then
  begin
    //Add a custom CheckList
    j:=PtrInt(FCheckList.Objects[i])+1;
//    FCheckList.Objects[i]:=TObject(j);
  end
  else
  begin
    FCheckList.AddObject(CheckName,TObject(0));
    j:=0;
  end;
  SendStream(ltCheckpoint,CheckName+' #'+IntToStr(j),nil);
end;

procedure TLogger.IncCounter(const CounterName: String);
var
  i, j: Integer;
begin
  i := FCounterList.IndexOf(CounterName);
  if i <> -1 then
  begin
    j := PtrInt(FCounterList.Objects[i]) + 1;
//    FCounterList.Objects[i] := TObject(j);
  end
  else
  begin
    FCounterList.AddObject(CounterName, TObject(1));
    j := 1;
  end;
  SendStream(ltCounter,CounterName+'='+IntToStr(j),nil);
end;

procedure TLogger.DecCounter(const CounterName: String);
var
  i, j: Integer;
begin
  i := FCounterList.IndexOf(CounterName);
  if i <> -1 then
  begin
    j := PtrInt(FCounterList.Objects[i]) - 1;
//    FCounterList.Objects[i] := TObject(j);
  end
  else
  begin
    FCounterList.AddObject(CounterName, TObject(-1));
    j := -1;
  end;
  SendStream(ltCounter,CounterName+'='+IntToStr(j),nil);
end;

procedure TLogger.ResetCounter(const CounterName: String);
var
  i: Integer;
begin
  i := FCounterList.IndexOf(CounterName);
  if i <> -1 then
  begin
    FCounterList.Objects[i] := TObject(0);
    SendStream(ltCounter, FCounterList[i] + '=0', nil);
  end;
end;

function TLogger.GetCounter(const CounterName: String): Integer;
var
  i: Integer;
begin
  i := FCounterList.IndexOf(CounterName);
  if i <> -1 then
    Result := PtrInt(FCounterList.Objects[i])
  else
    Result := 0;
end;

procedure TLogger.ResetCheckPoint;
begin
  ResetCheckPoint(DefaultCheckName);
end;

procedure TLogger.ResetCheckPoint(const CheckName: String);
var
  i: Integer;
begin
  i:=FCheckList.IndexOf(CheckName);
  if i <> -1 then
  begin
    FCheckList.Objects[i] := TObject(0);
    SendStream(ltCheckpoint, CheckName+' #0',nil);
  end;
end;

procedure TLogger.EnterMethod(const AMethodName: String);
begin
  EnterMethod(nil,AMethodName);
end;

procedure TLogger.EnterMethod(Sender: TObject; const AMethodName: String);
begin
  FLogStack.Insert(0,UpperCase(AMethodName));
  if Sender <> nil then
  begin
    if Sender is TComponent then
      SendStream(ltEnterMethod,TComponent(Sender).Name+'.'+AMethodName,nil)
    else
      SendStream(ltEnterMethod,Sender.ClassName+'.'+AMethodName,nil);
  end
  else
    SendStream(ltEnterMethod,AMethodName,nil);
end;

procedure TLogger.ExitMethod(const AMethodName: String);
begin
  ExitMethod(nil,AMethodName);
end;

procedure TLogger.ExitMethod(Sender: TObject; const AMethodName: String);
var
  i:Integer;
begin
  //ensure that ExitMethod will be called allways if there's a unpaired Entermethod
  //even if Classes is not Active
  if FLogStack.Count = 0 then Exit;
  //todo: see if is necessary to do Uppercase (set case sensitive to false?)
  i:=FLogStack.IndexOf(UpperCase(AMethodName));
  if i <> -1 then
    FLogStack.Delete(i)
  else
    Exit;
  if Sender <> nil then
  begin
    if Sender is TComponent then
      SendStream(ltExitMethod,TComponent(Sender).Name+'.'+AMethodName,nil)
    else
      SendStream(ltExitMethod,Sender.ClassName+'.'+AMethodName,nil);
  end
  else
    SendStream(ltExitMethod,AMethodName,nil);
end;

procedure TLogger.Watch(const AText, AValue: String);
begin
  SendStream(ltWatch,AText+'='+AValue,nil);
end;

procedure TLogger.Watch(const AText: String; AValue: Integer);
begin
  SendStream(ltWatch,AText+'='+IntToStr(AValue),nil);
end;

{$ifdef fpc}
procedure TLogger.Watch(const AText: String; AValue: Cardinal);
begin
  SendStream(ltWatch,AText+'='+IntToStr(AValue),nil);
end;
{$endif}

procedure TLogger.Watch(const AText: String; AValue: Double);
begin
  SendStream(ltWatch,AText+'='+FloatToStr(AValue),nil);
end;

procedure TLogger.Watch(const AText: String; AValue: Boolean);
begin
  SendStream(ltWatch,AText+'='+BoolToStr(AValue),nil);
end;

procedure TLogger.AppException(Sender: TObject; E: Exception);
var
  f: TSupportForm;
  s, s2: TStringList;
  i: integer;
begin
  //hLog.Add('!!!EXCEPTION HANDLED: '+E.Message);
  Self.SendException('EXCEPTION:'+E.Message, E);

  s:= TStringList.Create;
  s2:=TStringList.Create;
  f:=TSupportForm.Create(nil);
  f.Memo1.Lines.Clear;

  f.Memo1.Lines.Add('Log folder: '+KSPLogFilename);

  SearchForFilesFS(KSPLogFilename, false, s);
  for i:=0 to s.Count-1 do begin
    s2.LoadFromFile(s.Strings[i]);
;
    f.Memo1.Lines.Add(s.Strings[i]);
    f.Memo1.Lines.Add('');
    f.Memo1.Lines.AddStrings(s2);
    f.Memo1.Lines.Add('');
  end;

  f.ShowModal;

  s.Free;
  s2.Free;
  f.Free;
end;

end.


unit FileSupport;

interface

uses Windows, PluginCtrl, Classes,
    SysUtils, RT_basscd, Dialogs;

const MAXCDDRIVES = 8;

type TSupportedBy = (Both, BASSNative, WinampPlugin, None);

TCDDriveList     = array[0..MAXCDDRIVES-1] of string[255];

type TFileDesc = record
        Extensions: array[0..15]of string;
        name: string;
        Description: string;
      end;

    TFileInfo = class(TObject)
  public
    Entry: TFileDesc;
  end;

TFileSupportList = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Entry: TFileDesc);
    procedure Remove(Index: Integer);
    function GetItem(Index: Integer): TFileDesc;
    function FindExtension(Ext: string; CharSize: boolean): integer;
  end;

implementation

{$IFDEF KSP_MAIN}
uses uMain;
{$ENDIF}

constructor TFileSupportList.Create;
begin
  inherited Create;
end;

{The Items should be freed here but it isn't. Doesn't matter.
TPlayList is created only once and destroyed only while KSP is
to be closed}

destructor TFileSupportList.Destroy;
var
  i: integer;
begin
  for I := 0 to Count-1 do
    TFileInfo(Items[I]).Free;
  inherited Destroy;
end;

procedure TFileSupportList.Add(Entry: TFileDesc);
var
  T: TFileInfo;
begin
  T:=TFileInfo.Create;
  T.Entry:=Entry;
  inherited Add(T);
end;

procedure TFileSupportList.Remove(Index: Integer);
begin
  Delete(Index);
end;

function TFileSupportList.GetItem(Index: Integer): TFileDesc;
begin
  Result:=TFileInfo(Items[Index]).Entry;
end;

function TFileSupportList.FindExtension(Ext: string; CharSize: boolean): integer;
var
  i, x: integer;
  found: boolean;
begin
  Result:=-1;
  if not CharSize then Ext:=UpperCase(Ext);
  if Count>0 then begin
      for i:=0 to Count-1 do begin
          found:=false;
          for x:=0 to 15 do
            if UpperCase(TFileInfo(Items[i]).Entry.Extensions[x])=Ext then begin
                found:=true;
              end;

          if found then begin Result:=i; Break; end;
        end;
    end;
end;

end.
 
{
  $Project$
  $Workfile$
  $Revision$
  $DateUTC$
  $Id$

  This file is part of the Indy (Internet Direct) project, and is offered
  under the dual-licensing agreement described on the Indy website.
  (http://www.indyproject.org/)

  Copyright:
   (c) 1993-2005, Chad Z. Hower and the Indy Pit Crew. All rights reserved.
}
{
  $Log$
}

unit IdObjs;



interface
{$I IdCompilerDefines.inc}
uses
{$IFDEF DotNet}
  {$IFDEF DotNetDistro}
  IdObjsFCL
  {$ELSE}
  Classes,
  SysUtils
  {$ENDIF}
{$ELSE}
  Classes,
  {$IFDEF NO_TMultiReadExclusiveWriteSynchronizer}
  SyncObjs,
  {$ENDIF}
  SysUtils
{$ENDIF}
  ;

type

{$IFDEF DotNetDistro}
  TIdBaseObject = &Object;
  TIdPersistent = TIdNetPersistent;
  TIdPersistentHelper = class helper (TIdNetPersistentHelper) for TIdPersistent
  public
    constructor Create; override;
  end;
  TIdNativeComponent = TIdNetNativeComponent;
  TIdNativeComponentHelper = class helper (TIdNetNativeComponentHelper) for TIdNativeComponent
  end;
  TIdNativeComponentState = TIdNetNativeComponentState;
  TIdOperation = TIdNetNativeOperation;
  TIdStrings = TIdStringsFCL;
  TIdStringList = TIdStringListFCL;
  TIdStream = TIdNetStream;
  TIdMemoryStream = TIdNetMemoryStream;
  TIdStringStream = TIdNetStringStream;
  TIdFileStream = TIdNetFileStream;
  TIdComponentName = TIdNetComponentName;
  TIdSeekOrigin = TIdNetSeekOrigin;
  TIdList = TIdNetList;
  TIdCollection = TIdNetCollection;
  TIdCollectionItem = TIdNetCollectionItem;
  TIdNativeThread = TIdNetThread;
  TIdThreadMethod = TIdNetThreadMethod;
  TIdNotifyEvent = TIdNetNotifyEvent;
  TIdThreadList = TIdNetThreadList;
  TidOwnedCollection = TIdNetOwnedCollection;
  TIdMultiReadExclusiveWriteSynchronizer = TIdNetMultiReadExclusiveWriteSynchronizer;
{$ELSE}
  {$IFDEF DELPHI5}
  TSeekOrigin = Word;
  {$ENDIF}
  {$IFDEF DOTNET}
  TIdNativeComponent = TComponent;
  TIdNativeComponentState = TComponentState;
  TIdNativeComponentHelper = class helper (TComponentHelper) for TIdNativeComponent
  end;
   TIdPersistent = TPersistent;
   TIdPersistantHelper = class helper(TPersistentHelper) for TIdPersistent
   end;
  {$ELSE}
     TIdPersistent = TPersistent;
    TIdNativeComponent = TComponent;
  {$ENDIF}
  TIdOperation = TOperation;
  TIdBaseObject = TObject;

  TIdStrings = Classes.TStrings;
  TIdStringList = Classes.TStringList;
  TIdStream = TStream;
  TIdComponentName = TComponentName;
  TIdMemoryStream = TMemoryStream;
  TIdStringStream = TStringStream;
  TIdFileStream = TFileStream;
  TIdSeekOrigin = TSeekOrigin;
  TIdList = TList;
  TIdCollection = TCollection;
  TIdCollectionItem = TCollectionItem;
  TIdNativeThread = TThread;
  TIdThreadMethod = TThreadMethod;
  TIdNotifyEvent = TNotifyEvent;
  TIdThreadList = TThreadList;
  {$IFDEF FPC_REINTRODUCE_BUG}
  TIdOwnedCollection = class(TCollection)
  private
    fOwner: TPersistent;

  protected
    function GetOwner: TPersistent; override;
    constructor Create(AOwner: TPersistent; aItemClass: TCollectionItemClass);
  public

  end;
  {$ELSE}
  TIdOwnedCollection = TOwnedCollection;
  {$ENDIF}
  {$IFDEF NO_TMultiReadExclusiveWriteSynchronizer}
  TIdMultiReadExclusiveWriteSynchronizer = class(TObject)
  protected
    FCrit : TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginWrite;
    procedure BeginRead;
    procedure EndWrite;
    procedure EndRead;
  end;
  {$ELSE}
  TIdMultiReadExclusiveWriteSynchronizer = TMultiReadExclusiveWriteSynchronizer;
  {$ENDIF}
{$ENDIF}
  TIdComponentClass = class of TIdNativeComponent;

const
{$IFDEF DOTNET}
  IdFromBeginning   = TIdSeekOrigin.soBeginning;
  IdFromCurrent     = TIdSeekOrigin.soCurrent;
  IdFromEnd         = TIdSeekOrigin.soEnd;

  fmCreate          = $FFFF;
  fmOpenRead        = $0000;
  fmOpenWrite       = $0001;
  fmOpenReadWrite   = $0002;

  fmShareExclusive  = $0010;
  fmShareDenyWrite  = $0020;
  fmShareDenyNone   = $0040;
{$ELSE}
  {$IFDEF DELPHI5}
  soBeginning = soFromBeginning;
  soCurrent = soFromCurrent;
  soEnd = soFromEnd;
  {$ENDIF}

  IdFromBeginning   = TIdSeekOrigin(soBeginning);
  IdFromCurrent     = TIdSeekOrigin(soCurrent);
  IdFromEnd         = TIdSeekOrigin(soEnd);

  fmCreate          = $FFFF;
  {$IFDEF KYLIX}
  fmOpenRead        = O_RDONLY;
  fmOpenWrite       = O_WRONLY;
  fmOpenReadWrite   = O_RDWR;

  fmShareExclusive  = $0010;
  fmShareDenyWrite  = $0020;
  fmShareDenyNone   = $0030;
  {$ENDIF}
  {$IFDEF FPC}
//for FPC, we just wnt to expose what's in sysutils so that this code
//ismore portable tan usual.  Remember that this can be used on systems
//such as OS/2, MacOS, Mac OS/X, Novell Netare, and who knows what else.
//We can't assume that this will be usd only on Unix systems.
  fmOpenRead        = sysutils.fmOpenRead;
  fmOpenWrite       = sysutils.fmOpenWrite;
  fmOpenReadWrite   = sysutils.fmOpenReadWrite;

  fmShareExclusive  = sysutils.fmShareExclusive;
  fmShareDenyWrite  = sysutils.fmShareDenyWrite;
  fmShareDenyNone   = sysutils.fmShareDenyNone;
  {$ELSE}
    {$ifdef win32_or_win64_or_winCE}
  fmOpenRead        = $0000;
  fmOpenWrite       = $0001;
  fmOpenReadWrite   = $0002;

  fmShareExclusive  = $0010;
  fmShareDenyWrite  = $0020;
  fmShareDenyNone   = $0040;
     {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$IFDEF DotNetDistro}

  csLoading = IdObjsFCL.csLoading;
  csDesigning = IdObjsFCL.csDesigning;
  opRemove = IdObjsFCL.opRemove;
{$ELSE}

  csLoading = Classes.csLoading;
  csDesigning = Classes.csDesigning;
  opRemove = Classes.opRemove;
{$ENDIF}

  iddupIgnore = dupIgnore;
  iddupAccept = dupAccept;
  iddupError = dupError;


implementation
uses IdGlobal;
{$IFDEF DotNetDistro}
{ TIdPersistentHelper }

constructor TIdPersistentHelper.Create;
begin
  inherited Create;
end;

{$ENDIF}

{$IFDEF NO_TMultiReadExclusiveWriteSynchronizer}
constructor TIdMultiReadExclusiveWriteSynchronizer.Create;
begin
 inherited Create;
 FCrit := TCriticalSection.Create;
end;

destructor TIdMultiReadExclusiveWriteSynchronizer.Destroy;
begin
  FreeANdNil(FCrit);
  inherited Destroy;
end;
procedure TIdMultiReadExclusiveWriteSynchronizer.BeginWrite;
begin
  FCrit.Enter;
end;
procedure TIdMultiReadExclusiveWriteSynchronizer.BeginRead;
begin
  FCrit.Enter;
end;

procedure TIdMultiReadExclusiveWriteSynchronizer.EndWrite;
begin
  FCrit.Leave;
end;

procedure TIdMultiReadExclusiveWriteSynchronizer.EndRead;
begin
  FCrit.Leave;
end;
{$ENDIF}

{$IFDEF FPC}
function TIdOwnedCollection.GetOwner: TPersistent;
begin
  Result := fOwner;
end;

constructor TIdOwnedCollection.Create(AOwner: TPersistent; aItemClass: TCollectionItemClass);
begin
  FOwner := AOwner;
  inherited Create(AItemClass);
end;

{$ENDIF}

end.



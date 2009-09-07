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

unit IdSysLinux;

interface
{$i IdCompilerDefines.inc}

uses
  IdSysNativeVCL,
  IdSysVCL;

type
  TIdDateTimeBase = TDateTime;

  TIdSysLinux = class(TIdSysNativeVCL)
  public
    class function OffsetFromUTC: TIdDateTimeBase;
  end;

var
  GOffsetFromUTC: TIdDateTimeBase = 0;

implementation

class function TIdSysLinux.OffsetFromUTC: TIdDateTimeBase;
begin
  //TODO: Fix OffsetFromUTC for Linux to be automatic from OS
  Result := GOffsetFromUTC;
end;

end.

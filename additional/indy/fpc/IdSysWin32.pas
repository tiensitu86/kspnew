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

unit IdSysWin32;

interface
{$i IdCompilerDefines.inc}

uses
  IdSysNativeVCL,
  SysUtils;

type
 // TIdDateTimeBase = TDateTime;
  TIdSysWin32 = class(TIdSysNativeVCL)
  public
    class function Win32Platform: Integer;
    class function Win32MajorVersion : Integer;
    class function Win32MinorVersion : Integer;
    class function Win32BuildNumber : Integer;
    class function OffsetFromUTC: TIdDateTimeBase;// override;
  end;


implementation
uses  IdException, IdResourceStrings, Windows;
//EIdException is only in IdSys and that causes a circular reference
//if IdException is the interface section so we have to move the declaration
//of our exception type down here.

type
  //This is called whenever there is a failure to retreive the time zone information
  EIdFailedToRetreiveTimeZoneInfo = class(EIdException);

class function TIdSysWin32.OffsetFromUTC: TIdDateTimeBase;
var
  iBias: Integer;
  tmez: TTimeZoneInformation;
begin
  {$IFDEF WINCE}
  Case GetTimeZoneInformation(@tmez) of
  {$ELSE}
  Case GetTimeZoneInformation(tmez) of
  {$ENDIF}
    TIME_ZONE_ID_INVALID:
    begin
      raise EIdFailedToRetreiveTimeZoneInfo.Create(RSFailedTimeZoneInfo);
    end;
    TIME_ZONE_ID_UNKNOWN  :
       iBias := tmez.Bias;
    TIME_ZONE_ID_DAYLIGHT :
      iBias := tmez.Bias + tmez.DaylightBias;
    TIME_ZONE_ID_STANDARD :
      iBias := tmez.Bias + tmez.StandardBias;
    else
    begin
      raise EIdFailedToRetreiveTimeZoneInfo.Create(RSFailedTimeZoneInfo);
    end;
  end;
  {We use ABS because EncodeTime will only accept positve values}
  Result := EncodeTime(Abs(iBias) div 60, Abs(iBias) mod 60, 0, 0);
  {The GetTimeZone function returns values oriented towards convertin
   a GMT time into a local time.  We wish to do the do the opposit by returning
   the difference between the local time and GMT.  So I just make a positive
   value negative and leave a negative value as positive}
  if iBias > 0 then begin
    Result := 0 - Result;
  end;
end;

class function TIdSysWin32.Win32MinorVersion: Integer;
begin
  {$IFDEF WINCE}
  Result := SysUtils.WinCEMinorVersion;
  {$ELSE}
  Result := SysUtils.Win32MinorVersion;
  {$ENDIF}
end;

class function TIdSysWin32.Win32BuildNumber: Integer;
begin
 {$IFDEF WINCE}
  Result := SysUtils.WinCEBuildNumber and $FFFF;
  {$ELSE}
//  for this, you need to strip off some junk to do comparisons
   Result := SysUtils.Win32BuildNumber and $FFFF;
   {$ENDIF}
end;

class function TIdSysWin32.Win32Platform: Integer;
begin
 {$IFDEF WINCE}
  Result := SysUtils.WinCEPlatform;
  {$ELSE}
  Result := SysUtils.Win32Platform;
  {$ENDIF}
end;

class function TIdSysWin32.Win32MajorVersion: Integer;
begin
 {$IFDEF WINCE}
  Result := SysUtils.WinCEMajorVersion;
  {$ELSE}
  Result := SysUtils.Win32MajorVersion;
  {$ENDIF}
end;

end.

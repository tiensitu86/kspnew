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

unit IdSys;


interface
{$I IdCompilerDefines.inc}
{
The model we are using is this:

 IdSysBase - base class for everything
   IdSysFCL - stuff for DotNET with NO VCL dependancies for Visual Studio and Mono
   IdSysVCL - stuff that uses the VCL including packages for Borland Delphi VCL NET
     IdSysNativeVCL - stuff that is shared betwen Linux and Win32 versions that uses pointers
             and would make NO sense in DotNET.
        IdSysLinux - Linux specific stuff
        IdSysWindows - Windows specific stuff

}
uses
  {$IFDEF DotNetDistro}
  IdSysNet;
  {$ELSE}
    SysUtils,  //SysUtils has to be here for non-Dot NET stuff

    {$IFDEF win32_or_win64_or_winCE}
    IdSysNativeVCL,
    IdSysWin32;
    {$ELSE}
      {$IFDEF Unix} // Linux adds so little....
      IdSysLinux;
      {$ELSE}
        {$IFDEF DOTNET}
        IdSysVCL,
        IdSysVCLNET;
        {$ELSE}
        IdSysVCL;
      {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

type
  {$IFDEF DotNetDistro}
  Sys = class(TIdSysNet);
  {$ELSE}
    {$IFDEF win32_or_win64_or_winCE}
  Sys = TIdSysWin32;
    {$ELSE}
      {$IFDEF Unix} // linux unit current adds nothing
  Sys = TIdSysLinux;
      {$ELSE}
        {$IFDEF DOTNET}
        Sys = TIdSysVCLNET;
        {$ELSE}
        Sys = TIdSysVCL;
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  TIdDateTime = TIdDateTimeBase;

  // Exceptions
  //
  // ALL Indy exceptions must descend from EIdException or descendants of it and not directly
  // from EIdExceptionBase. This is the class that differentiates Indy exceptions from non Indy
  // exceptions in a cross platform way
  //
  // Do NOT use the class keyword, we do not want a new class, we just want an alias
  // so that it actually IS the base.
  //
  {$IFDEF DotNetDistro}
  EIdExceptionBase = System.Exception;
  EAbort = IdSysNET.EAbort;
  {$ELSE}
  {$IFNDEF FPC_CircularBug}
  EIdExceptionBase = Exception;
  {$ENDIF}
  {$IFDEF DOTNET}
    Exception = System.Exception;
  {$ELSE}
    Exception = SysUtils.Exception;
  {$ENDIF}
  EAbort = SysUtils.EAbort;
  {$ENDIF}

implementation

end.

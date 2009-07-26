library ksp;

{$MODE Delphi}

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
//  LCLIntf,
//  Windows,
  SysUtils;
//  KSPMessages in '..\..\itools\kspmessages.pas';

{$DEFINE KSP_SPECIAL_BUILD}

{$IFDEF KSP_SPECIAL_BUILD}
const KSPSpecialInfo = 'R2 pre';
{$ENDIF}
const KSPMajorVersion = '2009';

const Version = 0;
  Major = 2;
  Minor = 8;
  Build = 0;

type TPathChar = array[0..MAX_PATH] of Char;


function GetKSPVersion(AppPath: TPathChar): ShortString;
begin
  result := KSPMajorVersion;
{$IFDEF KSP_SPECIAL_BUILD}
  Result:=Result+' '+KSPSpecialInfo;
{$ENDIF}
end;

function GetKSPVersion2(AppPath: TPathChar): ShortString;
begin
  Result:=IntToStr(Version)+'.'+
    IntToStr(Major)+'.'+
    IntToStr(Minor)+'.'+
    IntToStr(Build);//GetKSPVersion(AppPath);
end;

exports
  GetKSPVersion,
  GetKSPVersion2;

{$IFDEF WINDOWS}{$R ksp.rc}{$ENDIF}

begin
end.

library ksp;

{$MODE Delphi}

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
  Minor = 21;
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

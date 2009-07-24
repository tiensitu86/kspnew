library ksp;

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
  Windows,
  SysUtils,
  KSPMessages in '..\..\itools\KSPMessages.pas';

{$R *.res}

{$DEFINE KSP_SPECIAL_BUILD}

{$IFDEF KSP_SPECIAL_BUILD}
const KSPSpecialInfo = 'R4';
{$ENDIF}
const KSPMajorVersion = '2006';

procedure GetFileVersionEx(FileName: PChar; var Major1, Major2, Minor1,
Minor2: Integer );
{ Helper function to get the actual file version information }
var
  Info: Pointer;
  InfoSize: DWORD;
  FileInfo: PVSFixedFileInfo;
  FileInfoSize: DWORD;
  Tmp: DWORD;
begin
  // Get the size of the FileVersionInformatioin
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Tmp);
  // If InfoSize = 0, then the file may not exist, or
  // it may not have file version information in it.
  if InfoSize = 0 then
  begin
    Major1 := 0;
    Major2 := 0;
    Minor1 := 0;
    Minor2 := 0;
  end else
  begin
    // Allocate memory for the file version information
    GetMem(Info, InfoSize);
    try      // Get the information
      GetFileVersionInfo(PChar(FileName), 0, InfoSize, Info);
      // Query the information for the version
      VerQueryValue(Info, '\', Pointer(FileInfo), FileInfoSize);
      // Now fill in the version information
      Major1 := FileInfo.dwFileVersionMS shr 16;
      Major2 := FileInfo.dwFileVersionMS and $FFFF;
      Minor1 := FileInfo.dwFileVersionLS shr 16;
      Minor2 := FileInfo.dwFileVersionLS and $FFFF;
    finally
      FreeMem(Info, FileInfoSize);
    end;
  end;
end;

function GetKSPVersion2(AppPath: TPathChar): ShortString;
var
  M1, M2, M3, M4: Integer;
  s: string;
begin
  s:=AppPath+'ksp.dll';
  GetFileVersionEx( PChar(s), M1, M2, M3, M4 );
  result := InttoStr(M1) + '.' + InttoStr(M2) + '.' + InttoStr(M3)+'.'+IntToStr(M4);
{$IFDEF KSP_SPECIAL_BUILD}
  Result:=Result+' '+KSPSpecialInfo;
{$ENDIF}
end;

function GetKSPVersion(AppPath: TPathChar): ShortString;
begin
  result := KSPMajorVersion;
{$IFDEF KSP_SPECIAL_BUILD}
  Result:=Result+' '+KSPSpecialInfo;
{$ENDIF}
end;

exports
  GetKSPVersion,
  GetKSPVersion2;

begin
end.

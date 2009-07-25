library kspinet;

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

//{$Include FastMM4Options.inc}

{%TogetherDiagram 'ModelSupport_kspinet\default.txaPackage'}

uses
//  ShareMem,
//  FastMM4,
//  elCodeHook,
//  FastCodeMoveHook,
//  FastcodeCompareTextHook,
//  FastCodePosHook,
//  FastcodeFillCharHook,
  SysUtils,
//  LCLIntf,
  Classes,
  WinInet;

const
  INTERNET_CONNECTION_OFFLINE           = 20;
  INTERNET_CONNECTION_CONFIGURED        = 40;

function IsConnectedToInternet: Boolean;
var
  dwConnectionTypes: DWORD;
begin
  dwConnectionTypes :=
    INTERNET_CONNECTION_MODEM +
    INTERNET_CONNECTION_LAN +
    INTERNET_CONNECTION_PROXY;
  Result := InternetGetConnectedState(@dwConnectionTypes, 0);
end;

{function PutInfoOnServer(URL: string; KSPLangID: LangID; UseInet: boolean): boolean;
var
  p: PChar;
begin
  StrPCopy(p, USRL+'?langid='+KSPLangID);
  if not IsConnectedToInternet then begin
      if UseInet then begin
          if InternetAutodial(INTERNET_AUTODIAL_FORCE_ONLINE or
            INTERNET_AUTODIAL_FORCE_UNATTENDED, 0) then
              begin
              end;
    end else begin

    end;

end;   }

function DownloadURL(const aUrl: PChar; var Output: TStringList): Boolean;
var
  hSession: HINTERNET;
  hService: HINTERNET;
  lpBuffer: array[0..1024 + 1] of Char;
  dwBytesRead: DWORD;
begin
  Result := False;
  // hSession := InternetOpen( 'MyApp', INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
  hSession := InternetOpen('KSP', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  try
    if Assigned(hSession) then
    begin
      hService := InternetOpenUrl(hSession, PChar(aUrl), nil, 0, 0, 0);
      if Assigned(hService) then
        try
          while True do
          begin
            dwBytesRead := 1024;
            InternetReadFile(hService, @lpBuffer, 1024, dwBytesRead);
            if dwBytesRead = 0 then break;
            lpBuffer[dwBytesRead] := #0;
            Output.Add(lpBuffer);
          end;
          Result := True;
        finally
          InternetCloseHandle(hService);
        end;
    end;
  finally
    InternetCloseHandle(hSession);
  end;
end;

exports
  IsConnectedToInternet,
  DownloadURL;

begin
end.


library kspfiles;

{$MODE objfpc}{$H+}

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

{%TogetherDiagram 'ModelSupport_kspfiles\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_kspfiles\FileSupportLst\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_kspfiles\kspfiles\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_kspfiles\KSPMessages\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_kspfiles\AdditFiles\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_kspfiles\FileUtils\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_kspfiles\default.txvpck'}

uses
{$IFNDEF WINDOWS}
  cmem,
{$ENDIF}
//  LCLIntf,
//  Windows,
  SysUtils;//,
//  Classes,
//  Dialogs,
//  DateUtils,
//  FileSupportLst in '..\..\itools\filesupportlst.pas',
  KSPDLLFileUtils in 'KSPDLLFileUtils.pas',
//  KSPMessages in '..\..\itools\kspmessages.pas'{,
//  AdditFiles in 'C:\medialib\AdditFiles.pas'};


exports
  //SearchFiles,
  RemoveForbiddenChars,
  ProduceFormatedString,
//  GetFav2,
//  GetFav,
  IsStream,
  IsCD;

begin
end.

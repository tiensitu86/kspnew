unit IdCTypes;
interface
{$i IdCompilerDefines.inc}

{This unit should not contain ANY program code.  It is meant to be extremely 
thin.  The idea is that the unit will contain type mappings that used for headers
and API calls using the headers.  The unit is here because in cross-platform
headers, the types may not always be the same as they would for Win32 on x86
Intel architecture.  We also want to be completely compatiable with Borland
Delphi for Win32.}
{$IFDEF FPC}
uses
  ctypes;
{$ENDIF}
  
{
IMPORTANT!!!

The types below are defined to hide architecture differences for various C++
types while also making this header compile with Borland Delphi.

}
type 
  {$IFDEF FPC}
  TIdC_ULONG = cuLong;
  PIdC_ULONG = pculong;
  TIdC_INT   = cInt;
  PIdC_INT   = pcInt;
  TIdC_UINT  = cUInt;
  PIdC_UINT  = pcUInt;
  TIdC_UNSIGNED = cunsigned;
  PIdC_UNSIGNED = ^TIdC_UNSIGNED;
  {$ENDIF}
  {$IFNDEF FPC}
  TIdC_ULONG = LongWord;
  PIdC_ULONG = ^TIdC_ULONG;
  TIdC_INT   = LongInt;
  PIdC_INT   = ^TIdC_INT;
  TIdC_UINT  = LongWord;
  PIdC_UINT  = ^TIdC_UINT;
  TIdC_UNSIGNED = LongWord;
  PIdC_UNSIGNED = ^TIdC_UNSIGNED;
  {$ENDIF}
    
implementation

end.

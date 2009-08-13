////////////////////////////////////////////////////////////////////////////////
//
//
//
// Winamp i/o plugin header adaption for Delphi/Pascal by Snake
//
//
//
////////////////////////////////////////////////////////////////////////////////
//
// (Based on the mini-SDK from Justin Frankel/Nullsoft Inc.)
//
// This ioplug.pas unit contains the follwing converted C-headers:
// in2.h, out.h
//
// Download:
// E-Mail: <snakers@gmx.net>
//
//
// This unit has 4 managment-functions:
//
// function InitInputDLL(dll:string) : Boolean;
// Loads the proc-address for getInModule from <dll> at runtime.
//
// function InitOutputDLL(dll:string) : Boolean;
// Loads the proc-address for getOutModule from <dll> at runtime.
//
// procedure CloseInputDLL;
// Releases the runtime-link to the input DLL and sets all proc-addresses to nil.
// You don't have to call this procedure, because the Finalization section do it for you.
//
// procedure CloseOutputDLL;
// Releases the runtime-link to the Output DLL and sets all proc-addresses to nil.
// You don't have to call this procedure, because the Finalization section do it for you.
//
// Modyfied: 8. Dec. 1999
//
////////////////////////////////////////////////////////////////////////////////

// Ver 1.1                 20 Apr 2009
//   - Modified for Delphi 2009
//
// Ver 1.0                 9 Sep 2008
//  Added 5 functions for Winamp General Purpose Plug-in, at
//    LoadGPPDLL(DLL: string): integer;     // Loads a Winamp GPP
//    UnloadGPPDLL(DLL: string): boolean;   // Unloads a Winamp GPP
//    UnloadAllGPPDLL;                      // Unloads all loaded Winamp GPP's
//    NumOfLoadedGPP : integer;             // Gets the number of loaded Winamp GPP
//    GetGPPInfo(index : integer; ...) : boolean;  // Gets the info. of a loaded Winamp GPP
//
//  Added types and functions for DSP, Visualization and Gernal purpose Plug-in **
//      by Silhwan Hyun


unit ioplug;

{$MINENUMSIZE 4}
{$ALIGN ON}

interface

uses
  Forms, SysUtils;


(*
** -----------------------------------------------------------------------
** MODULE PROTOTYPES
** -----------------------------------------------------------------------
*)


type TData=array[1..10] of byte;

//---------------------------------------------------------------------------------------------------------

//-------------------------------------------------------------------------------------------

//------------------------- For General Purpose Plug-in ---------------------------
const
  GPPHDR_VER = $10;
  MaxGPPNo = 8;    // Max. number of GPP


implementation


end.

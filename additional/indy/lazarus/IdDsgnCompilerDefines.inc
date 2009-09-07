// General

// Make this $DEFINE to use the 16 color icons required by Borland
// or DEFINE to use the 256 color Indy versions
{DEFINE Borland}

//Widget defines are for the design-time GUI and TIdAntiFreeze
// S.G. 4/9/2002: IPv4/IPv6 general switch (for defaults only)

// Delphi 4
{$IFDEF VER120}
  {$define WidgetVCLLike} // LCL included.
  {$define WidgetVCL}
{$ENDIF}

// C++ Builder 4
{$IFDEF VER125}

  {$define WidgetVCLLike} // LCL included.
  {$define WidgetVCL}
  {$define WidgetVCLLikeOrKylix}
{$ENDIF}

// Delphi 5 & CBuilder 5
{$IFDEF VER130}
  {$define WidgetVCLLike} // LCL included.
  {$define WidgetVCL}
  {$define WidgetVCLLikeOrKylix}
{$ENDIF}

//Delphi 6
{$IFDEF VER140}
  {$define WidgetVCLLike} // LCL included.
  {$define WidgetVCL}
  {$define VCL60plus}
  {$define WidgetVCLLikeOrKylix}
{$ENDIF}

//Delphi 7
{$IFDEF VER150}
  {$define WidgetVCLLike} // LCL included.
  {$define WidgetVCLLikeOrKylix}
  {$define WidgetVCL}
  {$define VCL60plus}
{$ENDIF}

//Delphi 8
{$IFDEF VER160}
//Widget defines are omitted since D8 only supports NET
{$ENDIF}

//Delphi 9
{$IFDEF VER170}
  {$ifndef CIL}
    {$define WidgetVCLLike} // LCL included.
    {$define WidgetVCLLikeOrKylix}
    {$define WidgetVCL}
    {$define VCL60plus}
  {$endif}
{$ENDIF}

//Delphi 10
{$IFDEF VER180}
  {$ifndef CIL}
    {$define Wi dgetVCLLike} // LCL included.
    {$define WidgetVCL}
    {$define WidgetVCLLikeOrKylix}
    {$define VCL60plus}
  {$endif}
{$ENDIF}

//Delphi 11
{$IFDEF VER190}
  {$ifndef CIL}
    {$define WidgetVCLLike} // LCL included.
    {$define WidgetVCLLikeOrKylix}
    {$define WidgetVCL}
    {$define VCL60plus}
  {$endif}
{$ENDIF}

//Delphi.NET
// Covers D8+
{$IFDEF CIL}
  {$define WidgetWinForms}
{$ENDIF}

// FPC (2+)

{$IFDEF FPC}
  {$define WidgetVCLLike} // LCL included.
  {$define WidgetVCLLikeOrKylix}
  {$define UseTBitBtn} //use Bit Buttons instead of Buttons
  {$define WidgetLCL}
  //note that we may need further defines for widget types depending on
  //what we do and what platforms we support in FPC.
  //I'll let Marco think about that one.
{$ENDIF}


// Kylix
//
//Important:  Don't use CompilerVersion here as
// $ IF's are evaluated before $ IFDEF's
//and Kylix 1 does not have CompilerVersion defined at all
{$IFDEF KYLIX}
  {$define WidgetKylix}
  {$define WidgetVCLLikeOrKylix}
{$ENDIF}

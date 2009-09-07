// General

// Make this $DEFINE to use the 16 color icons required by Borland
// or DEFINE to use the 256 color Indy versions
{DEFINE Borland}

//Widget defines are for the design-time GUI and TIdAntiFreeze
// S.G. 4/9/2002: IPv4/IPv6 general switch (for defaults only)
{$DEFINE IdIPv4}

{$DEFINE INDY100}

{$IFDEF BCB}
  {$DEFINE CBUILDER}
{$ELSE}
  {$DEFINE DELPHI}
{$ENDIF}

// Delphi 4
{$IFDEF VER120}
   {$define win32_or_win64}
  {$DEFINE VCL4ORABOVE}
  {$DEFINE VCL40}
  {$DEFINE DELPHI4}
  {$DEFINE OPTIONALPARAMS}
  {$DEFINE MSWINDOWS}
  {$DEFINE WIN32}
  {$define i386}
  {$define ENDIAN_LITTLE}
  {$define CPU32}
{$ENDIF}

// C++ Builder 4
{$IFDEF VER125}
   {$define win32_or_win64}
  {$DEFINE VCL4ORABOVE}
  {$DEFINE VCL40}
  {$DEFINE CBUILDER4}
  {$DEFINE OPTIONALPARAMS}
  {$DEFINE MSWINDOWS}
  {$DEFINE WIN32}
  {$define i386}
  {$define ENDIAN_LITTLE}
  {$define CPU32}
{$ENDIF}

// Delphi 5 & CBuilder 5
{$IFDEF VER130}
   {$define win32_or_win64}
  {$DEFINE VCL4ORABOVE}
  {$DEFINE VCL5ORABOVE}
  {$DEFINE VCL5}
  {$IFDEF BCB}
    {$DEFINE CBUILDER5}
  {$ELSE}
    {$DEFINE DELPHI5}
  {$ENDIF}
  {$DEFINE OPTIONALPARAMS}
  {$DEFINE SAMETEXT}
  {$DEFINE MSWINDOWS}
  {$DEFINE WIN32}
  {$define i386}
  {$define ENDIAN_LITTLE}
  {$define CPU32}
{$ENDIF}

//Delphi 6
{$IFDEF VER140}
   {$define win32_or_win64}
  {$DEFINE VCL4ORABOVE}
  {$DEFINE VCL5ORABOVE}
  {$IFDEF BCB}
    {$DEFINE CBUILDER6}
    {$DEFINE ALLOW_NAMED_THREADS}
  {$ELSE}
    {$DEFINE DELPHI6}
  {$ENDIF}
  {$DEFINE OPTIONALPARAMS}
  {$DEFINE SAMETEXT}
  {$DEFINE VCL6ORABOVE}
  {$DEFINE VCL60}
  {$DEFINE TFormatSettings}
  {$DEFINE WIN32}
  {$define i386}
  {$define ENDIAN_LITTLE}
  {$define CPU32}
{$ENDIF}

//Delphi 7
{$IFDEF VER150}
   {$define win32_or_win64}
  {$DEFINE VCL4ORABOVE}
  {$DEFINE VCL5ORABOVE}
  {$DEFINE VCL6ORABOVE}
  {$DEFINE VCL7ORABOVE}
  {$DEFINE VCL70}
  {$IFDEF BCB}
    {$DEFINE CBUILDER7}
  {$ELSE}
    {$DEFINE DELPHI7}
  {$ENDIF}
  {$DEFINE OPTIONALPARAMS}
  {$DEFINE SAMETEXT}
  {$DEFINE ALLOW_NAMED_THREADS}
  {$DEFINE WIN32}
  {$DEFINE TFormatSettings}
  {$define i386}
  {$define ENDIAN_LITTLE}
  {$define CPU32}
{$ENDIF}

//Delphi 8
{$IFDEF VER160}
  {$DEFINE VCL4ORABOVE}
  {$DEFINE VCL5ORABOVE}
  {$DEFINE VCL6ORABOVE}
  {$DEFINE VCL7ORABOVE}
  {$DEFINE VCL8ORABOVE}
  {$DEFINE VCL80}
  {$DEFINE DELPHI8}
  {$DEFINE OPTIONALPARAMS}
  {$DEFINE SAMETEXT}
  {$DEFINE ALLOW_NAMED_THREADS}
  {$DEFINE TFormatSettings}
  {$define i386}
  {$define ENDIAN_LITTLE}
  {$define CPU32}
//Widget defines are omitted since D8 only supports NET
{$ENDIF}

//Delphi 9
{$IFDEF VER170}
   {$define win32_or_win64}
  {$DEFINE VCL4ORABOVE}
  {$DEFINE VCL5ORABOVE}
  {$DEFINE VCL6ORABOVE}
  {$DEFINE VCL7ORABOVE}
  {$DEFINE VCL8ORABOVE}
  {$DEFINE VCL9ORABOVE}
  {$DEFINE VCL90}
  {$DEFINE DELPHI9}
  {$DEFINE OPTIONALPARAMS}
  {$DEFINE SAMETEXT}
  {$DEFINE ALLOW_NAMED_THREADS}
  {$DEFINE TFormatSettings}
  {$define i386}
  {$define ENDIAN_LITTLE}
  {$define CPU32}
{$ENDIF}

//Delphi 10
{$IFDEF VER180}
   {$define win32_or_win64}
  {$DEFINE VCL4ORABOVE}
  {$DEFINE VCL5ORABOVE}
  {$DEFINE VCL6ORABOVE}
  {$DEFINE VCL7ORABOVE}
  {$DEFINE VCL8ORABOVE}
  {$DEFINE VCL9ORABOVE}
  {$DEFINE VCL10ORABOVE}
  {$DEFINE VCL10}
  {$DEFINE DELPHI10}
  {$DEFINE OPTIONALPARAMS}
  {$DEFINE SAMETEXT}
  {$DEFINE ALLOW_NAMED_THREADS}
  {$DEFINE TFormatSettings}
  {$define i386}
  {$define ENDIAN_LITTLE}
  {$define CPU32}

{$ENDIF}

//Delphi 11
{$IFDEF VER190}
   {$define win32_or_win64}
  {$DEFINE VCL4ORABOVE}
  {$DEFINE VCL5ORABOVE}
  {$DEFINE VCL6ORABOVE}
  {$DEFINE VCL7ORABOVE}
  {$DEFINE VCL8ORABOVE}
  {$DEFINE VCL9ORABOVE}
  {$DEFINE VCL10ORABOVE}
  {$DEFINE VCL11ORABOVE}
  {$DEFINE VCL11}
  {$DEFINE DELPHI11}
  {$DEFINE OPTIONALPARAMS}
  {$DEFINE SAMETEXT}
  {$DEFINE ALLOW_NAMED_THREADS}
  {$DEFINE TFormatSettings}
  {$DEFINE FPC_CircularBug}
  {$define i386}
  {$define ENDIAN_LITTLE}
  {$define CPU32}
{$ENDIF}

//Delphi.NET
// Covers D8+
{$IFDEF CIL}
  // Platform specific conditional. Used for platform specific code.
  {$DEFINE DotNet}
  // Extra include used in D7 for testing. Remove later when all comps are
  // ported. Used to selectively exclude non ported parts. Allowed in places
  // IFDEFs are otherwise not permitted.
  {$DEFINE DotNetExclude}
  {$define i386}
  {$define ENDIAN_LITTLE}
  {$define CPU32}
  {$define WidgetWinForms}
{$ENDIF}

// FPC (2+)

{$IFDEF FPC}
  {$MODE Delphi}
  {$IFDEF UNIX}
     {$IFDEF LINUX}
     //In Linux, you can either use USELIBC or USEBASEUNIX.
     //If you choose USELIBC, you need to modify the Makefile.fpc in
     //in the System folder and generate the Makefile (preferably with the
     //complete source tree.
        {.$DEFINE USELIBC}
        {$DEFINE USEBASEUNIX}
     {$ELSE}
        {$DEFINE USEBASEUNIX}
     {$ENDIF}
  {$ENDIF}
  {$IF defined(win32) or defined(win64)}
    {$define win32_or_win64}
  {$ifend}
  //so we can use IdWinsock2 in WinCE, maybe in other places in Indy as well -CYA
  {$IF defined(wince)} 
    {$DEFINE UNICODE} //CE is UNICODE ONLY
    {$define win32_or_win64_or_winCE}
  {$ENDIF}
  {$DEFINE VCL4ORABOVE}
  {$DEFINE VCL5ORABOVE}
  {$DEFINE VCL6ORABOVE}
  {$DEFINE VCL7ORABOVE}
  {$DEFINE VCL70}
  {$DEFINE DELPHI7}
  {$DEFINE OPTIONALPARAMS}
  {$DEFINE SAMETEXT}
  {$DEFINE ALLOW_NAMED_THREADS}
  {$DEFINE NO_TMultiReadExclusiveWriteSynchronizer}
   {$DEFINE FPC_REINTRODUCE_BUG}
   {$DEFINE FPC_CircularBug}
   {$DEFINE NoRedeclare}
   {$DEFINE ByteCompareSets}
{$ENDIF}


// Kylix
//
//Important:  Don't use CompilerVersion here as
// $ IF's are evaluated before $ IFDEF's
//and Kylix 1 does not have CompilerVersion defined at all.
{$IFNDEF FPC}
  {$IFDEF LINUX}
    {$DEFINE USELIBC}
    {$DEFINE UNIX}
    {$IFDEF CONDITIONALEXPRESSIONS}
      {$IF RTLVersion = 14.0}
        {$DEFINE KYLIX1}
        {$DEFINE KYLIX}
        {$DEFINE KYLIX1ORABOVE}
      {$IFEND}
      {$IF RTLVersion = 14.2}
        {$DEFINE KYLIX2}
        {$DEFINE KYLIX}
        {$DEFINE USEZLIBUNIT}
        {$DEFINE KYLIX1ORABOVE}
        {$DEFINE KYLIX2ORABOVE}
      {$IFEND}
      {$IF RTLVersion = 14.5}
        {$DEFINE KYLIX3}
        {$DEFINE KYLIX}
        {$DEFINE USEZLIBUNIT}
        {$DEFINE KYLIX1ORABOVE}
        {$DEFINE KYLIX2ORABOVE}
        {$DEFINE KYLIX3ORABOVE}
      {$IFEND}
    
    {$ENDIF}

    {$IFDEF WIN32}


      {$DEFINE VCL4ORABOVE}
      {$DEFINE VCL5ORABOVE}
      {$DEFINE OPTIONALPARAMS}
      {$DEFINE SAMETEXT}
      {$DEFINE VCL6ORABOVE}
      {$DEFINE VCL60}
      {$define i386}
      {$define ENDIAN_LITTLE}
      {$define CPU32}
      {$define IntThreadPriority} //it's an integer, not enum
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$ifdef win32_or_win64}
   {$define win32_or_win64_or_winCE}
{$endif}

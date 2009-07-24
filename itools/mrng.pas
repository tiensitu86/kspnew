// The "Mother-of-all Pseudo Random Number Generators"
// Invented by Dr. George Marsaglia, Florida St. Univ., Dept. of Statistics
// Assembly implementation by Agner Fog
// Delphi BASM conversion, v1.0, (c)1999, EFD Systems
// Excellent statistical properties and extremely long
// cycle length (approx. 3*10^47)


unit MRNG;

interface

function  MRandom:Double;
function MRandInt(Low,High:Cardinal):Cardinal;
procedure MRandSeed(Seed:Integer);

implementation

var
  M0    :Integer = 0;
  M1    :Integer = 0;
  M2    :Integer = 0;
  M3    :Integer = 0;
  MC    :Integer = 0;
  MF3   :Integer = 2111111111;
  MF2   :Integer = 1492;
  MF1   :Integer = 1776;
  MF0   :Integer = 5115;
  F2M32 :Integer = $2F800000;
  EXTEND:Comp    = 0;


function MRandom:Double;
begin
  {Generate random decimal in the range [0..1]}
{$IFDEF KSP64}
//  Randomize;
{$ELSE}
asm
  PUSH    EDI

  MOV     EAX, MF3
  MUL     M3
  MOV     ECX,EAX
  MOV     EAX, M2
  MOV     EDI,EDX
  MOV     M3, EAX
  MUL     MF2
  ADD     ECX,EAX
  MOV     EAX, M1
  ADC     EDI,EDX
  MOV     M2,EAX
  MUL     MF1
  ADD     ECX,EAX
  MOV     EAX,M0
  ADC     EDI,EDX
  MOV     M1,EAX
  MUL     MF0
  ADD     EAX,ECX
  ADC     EDX,EDI
  ADD     EAX,MC
  ADC     EDX,0
  MOV     M0,EAX
  MOV     MC,EDX
  LEA     EDI,EXTEND
  MOV     [EDI],EAX
  FILD    EXTEND

  POP     EDI

  FMUL    F2M32
{$ENDIF}
end;


procedure MRandSeed(Seed:Integer);
begin
  {Initialize generator; use Seed := GetTickCount to initialize from system clock}
{$IFDEF KSP64}
  Randomize;
{$ELSE}
asm
  PUSH    EDI

  CMP     EAX, 1
  SBB     EAX, 0
  XOR     ECX, ECX
@R80:
  MOV     EDX, EAX
  SHL     EAX, 13
  XOR     EDX, EAX
  MOV     EAX, EDX
  SHR     EDX, 17
  XOR     EAX, EDX
  MOV     EDX, EAX
  SHL     EDX, 5
  XOR     EAX, EDX
  MOV     M0[ECX*4], EAX
  INC     ECX
  CMP     ECX, 5
  JB      @R80
  MOV     EDI, 19
@R90:
  CALL    MRandom
  FSTP    ST(0)
  DEC     EDI
  JNZ     @R90

  POP     EDI
{$ENDIF}
end;


function MRandInt(Low,High:Cardinal):Cardinal;
  {Generate random integer in the range [Low..High]}
{$IFNDEF KSP64}
var
  d: double;
{$ENDIF}
begin
{$IFDEF KSP64}
  Result:=Low+Random(High-Low);
{$ELSE}
  d:=MRandom;
  Result := Low + Trunc(d*(High-Low));
{$ENDIF}
end;

initialization
Randomize;

end.

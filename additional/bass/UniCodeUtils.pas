// unit UniCodeUtils

//    written by Silhwan Hyun  (hyunsh@hanafos.com)
//
//
// Ver 1.0                         3 Sep 2008
//   - Initial release


unit UniCodeUtils;

interface

uses LResources;

 // function DupeString is defined in StrUtils.pas of Delphi 7

 function ToWideString(s: AnsiString): WideString;

implementation

function ToWideString(s: AnsiString): WideString;
begin
   Result:=UTF8Decode(AnsiToUtf8(s));
end;

end.



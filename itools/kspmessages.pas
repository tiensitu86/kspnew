unit KSPMessages;

{$MODE Delphi}

interface

uses Classes, SysUtils;

type TPathChar = array[0..MAX_PATH] of Char;

type TFileRenamed = record
        Old: TPathChar;
        New: TPathChar;
      end;


implementation

end.

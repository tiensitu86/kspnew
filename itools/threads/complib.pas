unit complib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type  TCompactlibThread = class(TThread)
  public
  protected
    procedure Execute; override;
  end;

implementation

uses KSPConstsVars;

procedure TCompactlibThread.Execute;
begin
//  AllSongs.CompactLib;
end;

end.


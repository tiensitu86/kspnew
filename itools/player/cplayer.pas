unit cplayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type TCorePlayer = class(TComponent)
  public
    function PlayerEnabled: boolean; virtual;
  end;


implementation

function TCorePlayer.PlayerEnabled: boolean;
begin
  Result:=false;
end;

end.


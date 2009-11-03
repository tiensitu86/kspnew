unit MRNG;

interface

function MRandInt(Low, High: cardinal): cardinal;

implementation

function MRandInt(Low, High: cardinal): cardinal;
  {Generate random integer in the range [Low..High]}
begin
  Result := Low + Random(High - Low);
end;

end.

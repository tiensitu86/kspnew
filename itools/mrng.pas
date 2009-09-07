unit MRNG;

interface

function MRandInt(Low,High:Cardinal):Cardinal;

implementation

function MRandInt(Low,High:Cardinal):Cardinal;
  {Generate random integer in the range [Low..High]}
begin
  Result:=Low+Random(High-Low);
end;

end.

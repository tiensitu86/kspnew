unit MediaItemsInfo;

interface

uses Classes, Playlists, SysUtils, ID3Mgmnt;

type TMediaItemInfo = class(TObject)
    //f: TXMLEntryFile;
  public
    Entry: TPLEntry;
    constructor Create;// overload;
    //constructor Create(Song: string); overload;
    destructor Destroy;
  end;

implementation

constructor TMediaItemInfo.Create;
begin
  inherited Create;
end;

destructor TMediaItemInfo.Destroy;
begin
  inherited Destroy
end;

end.

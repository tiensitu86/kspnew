{ *************************************************************************** }

 { Audio Tools Library (Freeware)                                              }
 { Class TWAVfile - for extracting information from WAV file header            }

 { Copyright (c) 2001 by Jurgen Faul                                           }
 { E-mail: jfaul@gmx.de                                                        }
 { http://jfaul.de/atl                                                         }

 { Version 1.1 (9 October 2001)                                                }
 {   - Fixed bug with WAV header detection                                     }

 { Version 1.0 (31 July 2001)                                                  }
 {   - Info: channel mode, sample rate, bits per sample, file size, duration   }

{ *************************************************************************** }

//  - Modified for Delphi 2009  (08 May 2009)

unit WAVfile;

interface

uses
  Classes, SysUtils;

const
  { Used with ChannelModeID property }
  WAV_CM_MONO   = 1;                                      { Index for mono mode }
  WAV_CM_STEREO = 2;                                  { Index for stereo mode }

  { Channel mode names }
  WAV_MODE: array [0..2] of string = ('Unknown', 'Mono', 'Stereo');

type
  { Class TWAVfile }
  TWAVfile = class(TObject)
  private
    { Private declarations }
    FValid:      boolean;
    FChannelModeID: byte;
    FSampleRate: word;
    FBitsPerSample: byte;
    FFileSize:   cardinal;
    procedure FResetData;
    function FGetChannelMode: string;
    function FGetDuration: double;
  public
    { Public declarations }
    constructor Create;                                     { Create object }
    function ReadFromFile(const FileName: string): boolean;   { Load header }
    property Valid: boolean Read FValid;             { True if header valid }
    property ChannelModeID: byte Read FChannelModeID;   { Channel mode code }
    property ChannelMode: string Read FGetChannelMode;  { Channel mode name }
    property SampleRate: word Read FSampleRate;          { Sample rate (hz) }
    property BitsPerSample: byte Read FBitsPerSample;     { Bits per sample }
    property FileSize: cardinal Read FFileSize;         { File size (bytes) }
    property Duration: double Read FGetDuration;       { Duration (seconds) }
  end;

implementation

type
  { Real structure of WAV file header }
  WAVRecord = record
    { RIFF file header }
    RIFFHeader:    array [1..4] of AnsiChar;        { Must be "RIFF" }
    FileSize:      integer;                           { Must be "RealFileSize - 8" }
    WAVEHeader:    array [1..4] of AnsiChar;        { Must be "WAVE" }
    { Format information }
    FormatHeader:  array [1..4] of AnsiChar;      { Must be "fmt " }
    FormatSize:    integer;                         { Must be 16 (decimal) }
    FormatCode:    word;                            { Must be 1 }
    ChannelNumber: word;                         { Number of channels }
    SampleRate:    integer;                         { Sample rate (hz) }
    BytesPerSecond: integer;                     { Bytes per second }
    BytesPerSample: word;                        { Bytes per sample }
    BitsPerSample: word;                         { Bits per sample }
    { Data area }
    DataHeader:    array [1..4] of AnsiChar;        { Must be "data" }
    DataSize:      integer;                           { Data size }
  end;

{ ********************* Auxiliary functions & procedures ******************** }

function ReadWAV(const FileName: string; var WAVData: WAVRecord): boolean;
var
  SourceFile:  file;
  Transferred: integer;
begin
  try
    Result := True;
    { Set read-access and open file }
    AssignFile(SourceFile, FileName);
    FileMode := 0;
    Reset(SourceFile, 1);
    { Read header }
    BlockRead(SourceFile, WAVData, 44, Transferred);
    CloseFile(SourceFile);
    { if transfer is not complete }
    if Transferred < 44 then
      Result := False;
  except
    { Error }
    Result := False;
  end;
end;

{ --------------------------------------------------------------------------- }

function HeaderIsValid(const WAVData: WAVRecord): boolean;
begin
  Result := True;   // Assume

  { Validation }
  if WAVData.RIFFHeader <> 'RIFF' then
  begin
    Result := False;
    exit;
  end;
  if WAVData.WAVEHeader <> 'WAVE' then
  begin
    Result := False;
    exit;
  end;
  if WAVData.FormatHeader <> 'fmt ' then
  begin
    Result := False;
    exit;
  end;
  if (WAVData.ChannelNumber <> WAV_CM_MONO) and
    (WAVData.ChannelNumber <> WAV_CM_STEREO) then
    Result := False;
end;

{ ********************** Private functions & procedures ********************* }

procedure TWAVfile.FResetData;
begin
  FValid      := False;
  FChannelModeID := 0;
  FSampleRate := 0;
  FBitsPerSample := 0;
  FFileSize   := 0;
end;

{ --------------------------------------------------------------------------- }

function TWAVfile.FGetChannelMode: string;
begin
  Result := WAV_MODE[FChannelModeID];
end;

{ --------------------------------------------------------------------------- }

function TWAVfile.FGetDuration: double;
begin
  if FValid then
    Result := (FFileSize - 44) * 8 / FSampleRate / FBitsPerSample / FChannelModeID
  else
    Result := 0;
end;

{ ********************** Public functions & procedures ********************** }

constructor TWAVfile.Create;
begin
  inherited;
  FResetData;
end;

{ --------------------------------------------------------------------------- }

function TWAVfile.ReadFromFile(const FileName: string): boolean;
var
  WAVData: WAVRecord;
begin
  { Reset and load header data from file to variable }
  FResetData;
  Result := ReadWAV(FileName, WAVData);
  { Process data if loaded and header valid }
  if (Result) and (HeaderIsValid(WAVData)) then
  begin
    FValid      := True;
    { Fill properties with header data }
    FChannelModeID := WAVData.ChannelNumber;
    FSampleRate := WAVData.SampleRate;
    FBitsPerSample := WAVData.BitsPerSample;
    FFileSize   := WAVData.FileSize + 8;
  end;
end;

end.

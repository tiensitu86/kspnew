{ *************************************************************************** }

 { Audio Tools Library                                                         }
 { Class TMPEGaudio - for manipulating with MPEG audio file information        }

 { http://mac.sourceforge.net/atl/                                             }
 { e-mail: macteam@users.sourceforge.net                                       }

 { Copyright (c) 2000-2002 by Jurgen Faul                                      }
 { Copyright (c) 2003-2005 by The MAC Team                                     }

 { Version 2.1 (April 2005) by Gambit                                          }
 {   - updated to unicode file access                                          }

 { Version 2.00 (December 2004) by e-w@re                                      }
 {   - returns the correct MPEG data position in file                          }
 {   - added property MPEGstart -> returns start of MPEG data in file          }
 {   - added property MPEGend   -> returns end of MPEG data in file            }

 { Version 1.99 (April 2004) by Gambit                                         }
 {   - Improved LAME detection                                                 }
 {      (checks for the LAME string in the padding)                            }

 { Version 1.91 (April 2004) by Gambit                                         }
 {   - Added Ratio property                                                    }

 { Version 1.9 (22 February 2004) by Gambit                                    }
 {   - Added Samples property                                                  }

 { Version 1.8 (29 June 2003) by Gambit                                        }
 {   - Reads ape tags in mp3 files                                             }

 { Version 1.7 (4 November 2002)                                               }
 {   - Ability to recognize QDesign MPEG audio encoder                         }
 {   - Fixed bug with MPEG Layer II                                            }
 {   - Fixed bug with very big files                                           }

 { Version 1.6 (23 May 2002)                                                   }
 {   - Improved reading performance (up to 50% faster)                         }

 { Version 1.1 (11 September 2001)                                             }
 {   - Improved encoder guessing for CBR files                                 }

 { Version 1.0 (31 August 2001)                                                }
 {   - Support for MPEG audio (versions 1, 2, 2.5, layers I, II, III)          }
 {   - Support for Xing & FhG VBR                                              }
 {   - Ability to guess audio encoder (Xing, FhG, LAME, Blade, GoGo, Shine)    }
 {   - Class TID3v1: reading & writing support for ID3v1 tags                  }
 {   - Class TID3v2: reading & writing support for ID3v2 tags                  }

 { This library is free software; you can redistribute it and/or               }
 { modify it under the terms of the GNU Lesser General Public                  }
 { License as published by the Free Software Foundation; either                }
 { version 2.1 of the License, or (at your option) any later version.          }

 { This library is distributed in the hope that it will be useful,             }
 { but WITHOUT ANY WARRANTY; without even the implied warranty of              }
 { MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           }
 { Lesser General Public License for more details.                             }

 { You should have received a copy of the GNU Lesser General Public            }
 { License along with this library; if not, write to the Free Software         }
 { Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA   }

{ *************************************************************************** }

// Modified for Delphi 2009   (08 May 2009)

unit MPEGaudio;

interface


uses
  Classes, SysUtils, ID3v1, ID3v2, APEtag, FileUtil, Dialogs;

const
  { Table for bit rates }
  MPEG_BIT_RATE: array [0..3, 0..3, 0..15] of word =
    (
    { For MPEG 2.5 }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256, 0)),
    { Reserved }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
    { For MPEG 2 }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256, 0)),
    { For MPEG 1 }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 0),
    (0, 32, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 384, 0),
    (0, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448, 0))
    );

  { Sample rate codes }
  MPEG_SAMPLE_RATE_LEVEL_3 = 0;                                     { Level 3 }
  MPEG_SAMPLE_RATE_LEVEL_2 = 1;                                     { Level 2 }
  MPEG_SAMPLE_RATE_LEVEL_1 = 2;                                     { Level 1 }
  MPEG_SAMPLE_RATE_UNKNOWN = 3;                               { Unknown value }

  { Table for sample rates }
  MPEG_SAMPLE_RATE: array [0..3, 0..3] of word =
    (
    (11025, 12000, 8000, 0),                                   { For MPEG 2.5 }
    (0, 0, 0, 0),                                                  { Reserved }
    (22050, 24000, 16000, 0),                                    { For MPEG 2 }
    (44100, 48000, 32000, 0)                                     { For MPEG 1 }
    );

  { VBR header ID for Xing/FhG }
  VBR_ID_XING = 'Xing';                                         { Xing VBR ID }
  VBR_ID_FHG  = 'VBRI';                                           { FhG VBR ID }

  { MPEG version codes }
  MPEG_VERSION_2_5 = 0;                                            { MPEG 2.5 }
  MPEG_VERSION_UNKNOWN = 1;                                 { Unknown version }
  MPEG_VERSION_2 = 2;                                                { MPEG 2 }
  MPEG_VERSION_1 = 3;                                                { MPEG 1 }

  { MPEG version names }
  MPEG_VERSION: array [0..3] of string =
    ('MPEG 2.5', 'MPEG ?', 'MPEG 2', 'MPEG 1');

  { MPEG layer codes }
  MPEG_LAYER_UNKNOWN = 0;                                     { Unknown layer }
  MPEG_LAYER_III = 1;                                             { Layer III }
  MPEG_LAYER_II = 2;                                               { Layer II }
  MPEG_LAYER_I = 3;                                                 { Layer I }

  { MPEG layer names }
  MPEG_LAYER: array [0..3] of string =
    ('Layer ?', 'Layer III', 'Layer II', 'Layer I');

  { Channel mode codes }
  MPEG_CM_STEREO  = 0;                                                { Stereo }
  MPEG_CM_JOINT_STEREO = 1;                                    { Joint Stereo }
  MPEG_CM_DUAL_CHANNEL = 2;                                    { Dual Channel }
  MPEG_CM_MONO    = 3;                                                    { Mono }
  MPEG_CM_UNKNOWN = 4;                                         { Unknown mode }

  { Channel mode names }
  MPEG_CM_MODE: array [0..4] of string =
    ('Stereo', 'Joint Stereo', 'Dual Channel', 'Mono', 'Unknown');

  { Extension mode codes (for Joint Stereo) }
  MPEG_CM_EXTENSION_OFF     = 0;                        { IS and MS modes set off }
  MPEG_CM_EXTENSION_IS      = 1;                             { Only IS mode set on }
  MPEG_CM_EXTENSION_MS      = 2;                             { Only MS mode set on }
  MPEG_CM_EXTENSION_ON      = 3;                          { IS and MS modes set on }
  MPEG_CM_EXTENSION_UNKNOWN = 4;                     { Unknown extension mode }

  { Emphasis mode codes }
  MPEG_EMPHASIS_NONE    = 0;                                              { None }
  MPEG_EMPHASIS_5015    = 1;                                          { 50/15 ms }
  MPEG_EMPHASIS_UNKNOWN = 2;                               { Unknown emphasis }
  MPEG_EMPHASIS_CCIT    = 3;                                         { CCIT J.17 }

  { Emphasis names }
  MPEG_EMPHASIS: array [0..3] of string =
    ('None', '50/15 ms', 'Unknown', 'CCIT J.17');

  { Encoder codes }
  MPEG_ENCODER_UNKNOWN = 0;                                 { Unknown encoder }
  MPEG_ENCODER_XING    = 1;                                               { Xing }
  MPEG_ENCODER_FHG     = 2;                                                 { FhG }
  MPEG_ENCODER_LAME    = 3;                                               { LAME }
  MPEG_ENCODER_BLADE   = 4;                                             { Blade }
  MPEG_ENCODER_GOGO    = 5;                                               { GoGo }
  MPEG_ENCODER_SHINE   = 6;                                             { Shine }
  MPEG_ENCODER_QDESIGN = 7;                                         { QDesign }

  { Encoder names }
  MPEG_ENCODER: array [0..7] of string =
    ('Unknown', 'Xing', 'FhG', 'LAME', 'Blade', 'GoGo', 'Shine', 'QDesign');

type
  hFileInt = integer;

  { Xing/FhG VBR header data }
  VBRData = record
    Found:  boolean;                                    { True if VBR header found }
    ID:     array [1..4] of AnsiChar;                   { Header ID: "Xing" or "VBRI" }
    Frames: integer;                                    { Total number of frames }
    Bytes:  integer;                                    { Total number of bytes }
    Scale:  byte;                                         { VBR scale (1..100) }
    VendorID: string;                                   { Vendor ID (if present) }
  end;

  { MPEG frame header data}
  FrameData = record
    Found:     boolean;                                     { True if frame found }
    Position:  integer;                           { Frame position in the file }
    Size:      word;                                          { Frame size (bytes) }
    Xing:      boolean;                                     { True if Xing encoder }
    Data:      array [1..4] of byte;                 { The whole frame header data }
    VersionID: byte;                                        { MPEG version ID }
    LayerID:   byte;                                            { MPEG layer ID }
    ProtectionBit: boolean;                        { True if protected by CRC }
    BitRateID: word;                                            { Bit rate ID }
    SampleRateID: word;                                      { Sample rate ID }
    PaddingBit: boolean;                               { True if frame padded }
    PrivateBit: boolean;                                  { Extra information }
    ModeID:    byte;                                           { Channel mode ID }
    ModeExtensionID: byte;             { Mode extension ID (for Joint Stereo) }
    CopyrightBit: boolean;                        { True if audio copyrighted }
    OriginalBit: boolean;                            { True if original media }
    EmphasisID: byte;                                           { Emphasis ID }
  end;

  MP3TagRec = TMP3TagInfo;

  { Class TMPEGaudio }
  TMPEGaudio = class(TObject)
  private
    { Private declarations }
    FFileLength: integer;
    FVendorID: string;
    FVBR:     VBRData;
    FFrame:   FrameData;
    FMPEGStart: int64;
    FMPEGEnd: int64;
    FAudioSizeTag: int64;
    FID3v1:   TID3v1;
    FID3v2:   TID3v2;
    FAPEtag:  TAPEtag;
    procedure FResetData;
    function FGetVersion: string;
    function FGetLayer: string;
    function FGetBitRate: word;
    function FGetSampleRate: word;
    function FGetChannelMode: string;
    function FGetEmphasis: string;
    function FGetFrames: integer;
    function FGetDuration: double;
    function FGetVBREncoderID: byte;
    function FGetCBREncoderID: byte;
    function FGetEncoderID: byte;
    function FGetEncoder: string;
    function FGetValid: boolean;
    function FGetSamples: cardinal;
    function FGetRatio: double;
  public
    { Public declarations }
    constructor Create;                                     { Create object }
    destructor Destroy; override;                          { Destroy object }
    function ReadFromFile(const FileName: WideString): boolean; { Load data }
    property FileLength: integer Read FFileLength;    { File length (bytes) }
    property VBR: VBRData Read FVBR;                      { VBR header data }
    property Frame: FrameData Read FFrame;              { Frame header data }
    property ID3v1: TID3v1 Read FID3v1;                    { ID3v1 tag data }
    property ID3v2: TID3v2 Read FID3v2;                    { ID3v2 tag data }
    property APEtag: TAPEtag Read FAPEtag;                   { APE tag data }
    property Version: string Read FGetVersion;          { MPEG version name }
    property Layer: string Read FGetLayer;                { MPEG layer name }
    property BitRate: word Read FGetBitRate;            { Bit rate (kbit/s) }
    property SampleRate: word Read FGetSampleRate;       { Sample rate (hz) }
    property ChannelMode: string Read FGetChannelMode;  { Channel mode name }
    property Emphasis: string Read FGetEmphasis;            { Emphasis name }
    property Frames: integer Read FGetFrames;      { Total number of frames }
    property Duration: double Read FGetDuration;      { Song duration (sec) }
    property EncoderID: byte Read FGetEncoderID;       { Guessed encoder ID }
    property Encoder: string Read FGetEncoder;       { Guessed encoder name }
    property Valid: boolean Read FGetValid;       { True if MPEG file valid }
    property Samples: cardinal Read FGetSamples;
    property Ratio: double Read FGetRatio;          { Compression ratio (%) }
    property MPEGStart: int64 Read FMPEGStart;{Returns start pos of MPEG data}
    property MPEGEnd: int64 Read FMPEGEnd;    { Returns end pos of MPEG data }
    property AudioSizeTag: int64 Read FAudioSizeTag;  { Returns ID3v2 TSIZ value or 0 }
  end;

// Load tag from stream: added for TBASSPlayer by Silhwan Hyun
function ReadFromTagStream(TagStream: PAnsiChar; StreamSize: longword;
  TagVersion: word; var MP3TagInfo: TMP3TagInfo): boolean;

implementation

const
  { Limitation constants }
  MAX_MPEG_FRAME_LENGTH = 1729;                      { Max. MPEG frame length }
  MIN_MPEG_BIT_RATE     = 8;                                { Min. bit rate value }
  MAX_MPEG_BIT_RATE     = 448;                              { Max. bit rate value }
  MIN_ALLOWED_DURATION  = 0.1;                      { Min. song duration value }

  { VBR Vendor ID strings }
  VENDOR_ID_LAME     = 'LAME';                                         { For LAME }
  VENDOR_ID_GOGO_NEW = 'GOGO';                               { For GoGo (New) }
  VENDOR_ID_GOGO_OLD = 'MPGE';                               { For GoGo (Old) }

  hINVALID_HANDLE_VALUE = hFileInt(-1);

{ ********************* Auxiliary functions & procedures ******************** }

function IsFrameHeader(const HeaderData: array of byte): boolean;
begin
  { Check for valid frame header }
  if ((HeaderData[0] and $FF) <> $FF) or ((HeaderData[1] and $E0) <> $E0) or
    (((HeaderData[1] shr 3) and 3) = 1) or (((HeaderData[1] shr 1) and 3) = 0) or
    ((HeaderData[2] and $F0) = $F0) or ((HeaderData[2] and $F0) = 0) or
    (((HeaderData[2] shr 2) and 3) = 3) or ((HeaderData[3] and 3) = 2) then
    Result := False
  else
    Result := True;
end;

{ --------------------------------------------------------------------------- }

procedure DecodeHeader(const HeaderData: array of byte; var Frame: FrameData);
begin
  { Decode frame header data }
  Move(HeaderData, Frame.Data, SizeOf(Frame.Data));
  Frame.VersionID   := (HeaderData[1] shr 3) and 3;
  Frame.LayerID     := (HeaderData[1] shr 1) and 3;
  Frame.ProtectionBit := (HeaderData[1] and 1) <> 1;
  Frame.BitRateID   := HeaderData[2] shr 4;
  Frame.SampleRateID := (HeaderData[2] shr 2) and 3;
  Frame.PaddingBit  := ((HeaderData[2] shr 1) and 1) = 1;
  Frame.PrivateBit  := (HeaderData[2] and 1) = 1;
  Frame.ModeID      := (HeaderData[3] shr 6) and 3;
  Frame.ModeExtensionID := (HeaderData[3] shr 4) and 3;
  Frame.CopyrightBit := ((HeaderData[3] shr 3) and 1) = 1;
  Frame.OriginalBit := ((HeaderData[3] shr 2) and 1) = 1;
  Frame.EmphasisID  := HeaderData[3] and 3;
end;

{ --------------------------------------------------------------------------- }

function ValidFrameAt(const Index: word; Data: array of byte): boolean;
var
  HeaderData: array [1..4] of byte;
begin
  { Check for frame at given position }
  HeaderData[1] := Data[Index];
  HeaderData[2] := Data[Index + 1];
  HeaderData[3] := Data[Index + 2];
  HeaderData[4] := Data[Index + 3];
  if IsFrameHeader(HeaderData) then
    Result := True
  else
    Result := False;
end;

{ --------------------------------------------------------------------------- }

function GetCoefficient(const Frame: FrameData): byte;
begin
  { Get frame size coefficient }
  if Frame.VersionID = MPEG_VERSION_1 then
    if Frame.LayerID = MPEG_LAYER_I then
      Result := 48
    else
      Result := 144
  else
  if Frame.LayerID = MPEG_LAYER_I then
    Result := 24
  else if Frame.LayerID = MPEG_LAYER_II then
    Result := 144
  else
    Result := 72;
end;

{ --------------------------------------------------------------------------- }

function GetBitRate(const Frame: FrameData): word;
begin
  { Get bit rate }
  Result := MPEG_BIT_RATE[Frame.VersionID, Frame.LayerID, Frame.BitRateID];
end;

{ --------------------------------------------------------------------------- }

function GetSampleRate(const Frame: FrameData): word;
begin
  { Get sample rate }
  Result := MPEG_SAMPLE_RATE[Frame.VersionID, Frame.SampleRateID];
end;

{ --------------------------------------------------------------------------- }

function GetPadding(const Frame: FrameData): byte;
begin
  { Get frame padding }
  if Frame.PaddingBit then
    if Frame.LayerID = MPEG_LAYER_I then
      Result := 4
    else
      Result := 1
  else
    Result := 0;
end;

{ --------------------------------------------------------------------------- }

function GetFrameLength(const Frame: FrameData): word;
var
  Coefficient, BitRate, SampleRate, Padding: word;
begin
  { Calculate MPEG frame length }
  Coefficient := GetCoefficient(Frame);
  BitRate     := GetBitRate(Frame);
  SampleRate  := GetSampleRate(Frame);
  Padding     := GetPadding(Frame);
  Result      := Trunc(Coefficient * BitRate * 1000 / SampleRate) + Padding;
end;

{ --------------------------------------------------------------------------- }

function IsXing(const Index: word; Data: array of byte): boolean;
begin
  { Get true if Xing encoder }
  Result :=
    (Data[Index] = 0) and (Data[Index + 1] = 0) and (Data[Index + 2] = 0) and
    (Data[Index + 3] = 0) and (Data[Index + 4] = 0) and (Data[Index + 5] = 0);
end;

{ --------------------------------------------------------------------------- }

function GetXingInfo(const Index: word; Data: array of byte): VBRData;
begin
  { Extract Xing VBR info at given position }
  FillChar(Result, SizeOf(Result), 0);
  Result.Found  := True;
  Result.ID     := VBR_ID_XING;
  Result.Frames :=
    Data[Index + 8] * $1000000 + Data[Index + 9] * $10000 +
    Data[Index + 10] * $100 + Data[Index + 11];
  Result.Bytes  :=
    Data[Index + 12] * $1000000 + Data[Index + 13] * $10000 +
    Data[Index + 14] * $100 + Data[Index + 15];
  Result.Scale  := Data[Index + 119];
  { Vendor ID can be not present }
  Result.VendorID :=
    AnsiChar(Data[Index + 120]) + AnsiChar(Data[Index + 121]) +
    AnsiChar(Data[Index + 122]) + AnsiChar(Data[Index + 123]) +
    AnsiChar(Data[Index + 124]) + AnsiChar(Data[Index + 125]) +
    AnsiChar(Data[Index + 126]) + AnsiChar(Data[Index + 127]);
end;

{ --------------------------------------------------------------------------- }

function GetFhGInfo(const Index: word; Data: array of byte): VBRData;
begin
  { Extract FhG VBR info at given position }
  FillChar(Result, SizeOf(Result), 0);
  Result.Found  := True;
  Result.ID     := VBR_ID_FHG;
  Result.Scale  := Data[Index + 9];
  Result.Bytes  :=
    Data[Index + 10] * $1000000 + Data[Index + 11] * $10000 +
    Data[Index + 12] * $100 + Data[Index + 13];
  Result.Frames :=
    Data[Index + 14] * $1000000 + Data[Index + 15] * $10000 +
    Data[Index + 16] * $100 + Data[Index + 17];
end;

{ --------------------------------------------------------------------------- }

function FindVBR(const Index: word; Data: array of byte): VBRData;
begin
  { Check for VBR header at given position }
  FillChar(Result, SizeOf(Result), 0);
  if Chr(Data[Index]) + Chr(Data[Index + 1]) + Chr(Data[Index + 2]) +
  Chr(Data[Index + 3]) = VBR_ID_XING then
    Result := GetXingInfo(Index, Data);
  if Chr(Data[Index]) + Chr(Data[Index + 1]) + Chr(Data[Index + 2]) +
  Chr(Data[Index + 3]) = VBR_ID_FHG then
    Result := GetFhGInfo(Index, Data);
end;

{ --------------------------------------------------------------------------- }

function GetVBRDeviation(const Frame: FrameData): byte;
begin
  { Calculate VBR deviation }
  if Frame.VersionID = MPEG_VERSION_1 then
    if Frame.ModeID <> MPEG_CM_MONO then
      Result := 36
    else
      Result := 21
  else
  if Frame.ModeID <> MPEG_CM_MONO then
    Result := 21
  else
    Result := 13;
end;

{ --------------------------------------------------------------------------- }

function FindFrame(const Data: array of byte; var VBR: VBRData): FrameData;
var
  HeaderData: array [1..4] of byte;
  Iterator, VBRIdx: integer;
begin
  { Search for valid frame }
  FillChar(Result, SizeOf(Result), 0);
  Move(Data, HeaderData, SizeOf(HeaderData));
  for Iterator := 0 to SizeOf(Data) - MAX_MPEG_FRAME_LENGTH do
  begin
    { Decode data if frame header found }
    if IsFrameHeader(HeaderData) then
    begin
      DecodeHeader(HeaderData, Result);
      { Check for next frame and try to find VBR header }
      VBRIdx := Iterator + GetFrameLength(Result);
      if (VBRIdx < SizeOf(Data)) and ValidFrameAt(VBRIdx, Data) then
      begin
        Result.Found := True;
        Result.Position := Iterator;
        Result.Size := GetFrameLength(Result);
        Result.Xing := IsXing(Iterator + SizeOf(HeaderData), Data);
        VBR := FindVBR(Iterator + GetVBRDeviation(Result), Data);
        break;
      end;
    end;
    { Prepare next data block }
    HeaderData[1] := HeaderData[2];
    HeaderData[2] := HeaderData[3];
    HeaderData[3] := HeaderData[4];
    HeaderData[4] := Data[Iterator + SizeOf(HeaderData)];
  end;
end;

{ --------------------------------------------------------------------------- }

function FindVendorID(const Data: array of byte; Size: word): string;
var
  Iterator: integer;
  VendorID: string;
begin
  { Search for vendor ID }
  Result := '';

  if (SizeOf(Data) - Size - 8) < 0 then
    Size := SizeOf(Data) - 8;
  for Iterator := 0 to Size do
  begin
    VendorID :=
      Chr(Data[SizeOf(Data) - Iterator - 8]) +
      Chr(Data[SizeOf(Data) - Iterator - 7]) +
      Chr(Data[SizeOf(Data) - Iterator - 6]) +
      Chr(Data[SizeOf(Data) - Iterator - 5]);
    if VendorID = VENDOR_ID_LAME then
    begin
      Result := VendorID + Chr(Data[SizeOf(Data) - Iterator - 4]) +
        Chr(Data[SizeOf(Data) - Iterator - 3]) +
        Chr(Data[SizeOf(Data) - Iterator - 2]) +
        Chr(Data[SizeOf(Data) - Iterator - 1]);
      break;
    end;
    if VendorID = VENDOR_ID_GOGO_NEW then
    begin
      Result := VendorID;
      break;
    end;
  end;
end;

{ ********************** Private functions & procedures ********************* }

procedure TMPEGaudio.FResetData;
begin
  { Reset all variables }
  FFileLength   := 0;
  FMPEGStart    := 0;
  FMPEGEnd      := 0;
  FAudioSizeTag := 0;
  FVendorID     := '';
  FillChar(FVBR, SizeOf(FVBR), 0);
  FillChar(FFrame, SizeOf(FFrame), 0);
  FFrame.VersionID  := MPEG_VERSION_UNKNOWN;
  FFrame.SampleRateID := MPEG_SAMPLE_RATE_UNKNOWN;
  FFrame.ModeID     := MPEG_CM_UNKNOWN;
  FFrame.ModeExtensionID := MPEG_CM_EXTENSION_UNKNOWN;
  FFrame.EmphasisID := MPEG_EMPHASIS_UNKNOWN;
  FID3v1.ResetData;
  FID3v2.ResetData;
  FAPEtag.ResetData;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetVersion: string;
begin
  { Get MPEG version name }
  Result := MPEG_VERSION[FFrame.VersionID];
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetLayer: string;
begin
  { Get MPEG layer name }
  Result := MPEG_LAYER[FFrame.LayerID];
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetBitRate: word;
begin
  { Get bit rate, calculate average bit rate if VBR header found }
  if (FVBR.Found) and (FVBR.Frames > 0) then
    Result := Round((FVBR.Bytes / FVBR.Frames - GetPadding(FFrame)) *
      GetSampleRate(FFrame) / GetCoefficient(FFrame) / 1000)
  else
    Result := GetBitRate(FFrame);
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetSampleRate: word;
begin
  { Get sample rate }
  Result := GetSampleRate(FFrame);
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetChannelMode: string;
begin
  { Get channel mode name }
  Result := MPEG_CM_MODE[FFrame.ModeID];
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetEmphasis: string;
begin
  { Get emphasis name }
  Result := MPEG_EMPHASIS[FFrame.EmphasisID];
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetFrames: integer;
var
  MPEGSize: integer;
begin
  { Get total number of frames, calculate if VBR header not found }
  if FVBR.Found then
    Result := FVBR.Frames
  else
  begin
    MPEGSize := FMPEGEnd - FMPEGStart;
    //  Result := (MPEGSize - FFrame.Position) div GetFrameLength(FFrame);
    Result   := MPEGSize div GetFrameLength(FFrame);
    // ** Changed by Silhwan Hyun (2009-05-11)
  end;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetDuration: double;
var
  MPEGSize: int64;
begin
  { Calculate song duration }
  if FFrame.Found then
    if (FVBR.Found) and (FVBR.Frames > 0) then
      Result := FVBR.Frames * GetCoefficient(FFrame) * 8 / GetSampleRate(FFrame)
    else
    begin
      MPEGSize := FMPEGEnd - FMPEGStart;
      //  Result := (MPEGSize - FFrame.Position) / GetBitRate(FFrame) / 1000 * 8;
      Result   := (MPEGSize / GetBitRate(FFrame)) / 1000 * 8;
      // ** Changed by Silhwan Hyun (2009-05-11)
    end
  else
    Result := 0;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetVBREncoderID: byte;
var
  Vendor_ID: string;
begin
  { Guess VBR encoder and get ID }
  Result := 0;

  Vendor_ID := Copy(FVBR.VendorID, 1, 4);
  if Vendor_ID = VENDOR_ID_LAME then
    Result := MPEG_ENCODER_LAME;
  if Vendor_ID = VENDOR_ID_GOGO_NEW then
    Result := MPEG_ENCODER_GOGO;
  if Vendor_ID = VENDOR_ID_GOGO_OLD then
    Result := MPEG_ENCODER_GOGO;

  if (FVBR.ID = VBR_ID_XING) and (Vendor_ID <> VENDOR_ID_LAME) and
    (Vendor_ID <> VENDOR_ID_GOGO_NEW) and (Vendor_ID <> VENDOR_ID_GOGO_OLD) then
    Result := MPEG_ENCODER_XING;

  if FVBR.ID = VBR_ID_FHG then
    Result := MPEG_ENCODER_FHG;
  if Vendor_ID = VENDOR_ID_LAME then
    Result := MPEG_ENCODER_LAME;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetCBREncoderID: byte;
begin
  { Guess CBR encoder and get ID }
  Result := MPEG_ENCODER_FHG;

  if (FFrame.OriginalBit) and (FFrame.ProtectionBit) then
    Result := MPEG_ENCODER_LAME;

  if (GetBitRate(FFrame) <= 160) and (FFrame.ModeID = MPEG_CM_STEREO) then
    Result := MPEG_ENCODER_BLADE;

  if (FFrame.CopyrightBit) and (FFrame.OriginalBit) and
    (not FFrame.ProtectionBit) then
    Result := MPEG_ENCODER_XING;

  if (FFrame.Xing) and (FFrame.OriginalBit) then
    Result := MPEG_ENCODER_XING;

  if FFrame.LayerID = MPEG_LAYER_II then
    Result := MPEG_ENCODER_QDESIGN;

  if (FFrame.ModeID = MPEG_CM_DUAL_CHANNEL) and (FFrame.ProtectionBit) then
    Result := MPEG_ENCODER_SHINE;

  if Copy(FVendorID, 1, 4) = VENDOR_ID_LAME then
    Result := MPEG_ENCODER_LAME;

  if Copy(FVendorID, 1, 4) = VENDOR_ID_GOGO_NEW then
    Result := MPEG_ENCODER_GOGO;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetEncoderID: byte;
begin
  { Get guessed encoder ID }
  if FFrame.Found then
    if FVBR.Found then
      Result := FGetVBREncoderID
    else
      Result := FGetCBREncoderID
  else
    Result := 0;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetEncoder: string;
var
  VendorID: string;
begin
  { Get guessed encoder name and encoder version for LAME }
  Result := MPEG_ENCODER[FGetEncoderID];
  if FVBR.VendorID <> '' then
    VendorID := FVBR.VendorID;
  if FVendorID <> '' then
    VendorID := FVendorID;
  if (FGetEncoderID = MPEG_ENCODER_LAME) and (Length(VendorID) >= 8) and
    (VendorID[5] in ['0'..'9']) and (VendorID[6] = '.') and
    (VendorID[7] in ['0'..'9']) and (VendorID[8] in ['0'..'9']) then
    Result :=
      Result + #32 + VendorID[5] + VendorID[6] + VendorID[7] +
      VendorID[8];
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetValid: boolean;
begin
  { Check for right MPEG file data }
  Result :=
    (FFrame.Found) and (FGetBitRate >= MIN_MPEG_BIT_RATE) and
    (FGetBitRate <= MAX_MPEG_BIT_RATE) and (FGetDuration >= MIN_ALLOWED_DURATION);
end;

{ ********************** Public functions & procedures ********************** }

constructor TMPEGaudio.Create;
begin
  { Object constructor }
  inherited;

  FID3v1  := TID3v1.Create;
  FID3v2  := TID3v2.Create;
  FAPEtag := TAPEtag.Create;
  FResetData;
end;

{ --------------------------------------------------------------------------- }

destructor TMPEGaudio.Destroy;
begin
  { Object destructor }
  FID3v1.Free;
  FID3v2.Free;
  FAPEtag.Free;

  inherited;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.ReadFromFile(const FileName: WideString): boolean;
var
  SourceFile: hFileInt;
  Data:     array [1..MAX_MPEG_FRAME_LENGTH * 2] of byte;
  Transferred: DWORD;
  Position: int64;
  tmp:      integer;
  str:      string;
  Value:    int64;
  Code:     integer;
begin
  FResetData;
  SourceFile := hINVALID_HANDLE_VALUE;
  try
    SourceFile := FileOpen(FileName, fmOpenRead or fmShareDenyWrite);
    if (SourceFile = hINVALID_HANDLE_VALUE) then
    begin
      Result := False;
      Exit;
    end;

    { At first search for tags & Lyrics3 then search for a MPEG frame and VBR data }
    if (FID3v2.ReadFromFile(FileName)) and
      (FID3v1.ReadFromFile(FileName)) then
    begin
      FFileLength := FileSize(FileName);//, nil);
      Position    := FID3v2.Size;
      FileSeek(SourceFile, Position, 0);
      Transferred := FileRead(SourceFile, Data, SizeOf(Data));//, Transferred, nil);
      FFrame      := FindFrame(Data, FVBR);
      // Search for vendor ID at the beginning
      FVendorID   := FindVendorID(Data, FFrame.Size * 5);

      { patched by e-w@re }
      { Try to find the first frame if no frame at the beginning found ]}
      if (not FFrame.Found) and (Transferred = SizeOf(Data)) then
        repeat
          Transferred := FileRead(SourceFile, Data, SizeOf(Data));
          //, Transferred, nil);
          Inc(Position, Transferred);
          FFrame := FindFrame(Data, FVBR);
        until (FFrame.Found) or (Transferred < SizeOf(Data));

      if FFrame.Found then
      begin
        FFrame.Position := Position + FFrame.Position;
        FMPEGStart := FFrame.Position;
        tmp      := FID3v1.TagSize;
        FMPEGEnd := FFileLength - tmp;
      end;

      if FID3v2.Exists then
      begin
        str := FID3v2.TSIZ;
        if Length(str) > 0 then
          try
            Val(str, Value, Code);
            if (Code = 0) then
              FAudioSizeTag := Value;
          except
            // ignore
          end;
      end;

      { Search for vendor ID at the end if CBR encoded }
      if (FFrame.Found) and (FVendorID = '') then
      begin
        if not FID3v1.Exists then
          Position := FFileLength - SizeOf(Data)
        else
          Position := FFileLength - SizeOf(Data) - 128;
        FileSeek(SourceFile, Position, 0);
        Transferred := FileRead(SourceFile, Data, SizeOf(Data));
        //, Transferred, nil);
        FVendorID   := FindVendorID(Data, FFrame.Size * 5);
      end;
    end;
    FileClose(SourceFile);
    Result := True;
  except
    if (SourceFile <> hINVALID_HANDLE_VALUE) then
      FileClose(SourceFile);
    Result := False;
  end;
  if not FFrame.Found then
    FResetData;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetSamples: cardinal;
begin
  Result := 0;

  if FFrame.Found then
    // rework, it's the same
    if (FVBR.Found) and (FVBR.Frames > 0) then
      Result := FVBR.Frames * GetCoefficient(FFrame) * 8
    else
      Result := FGetFrames * GetCoefficient(FFrame) * 8;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetRatio: double;
begin
  { Get compression ratio }
  if FGetValid then
  begin
    //Result := FFileSize / (FGetSamples * FChannels * FBits / 8 + 44) * 100
    if ChannelMode = 'Mono' then
      Result := FFileLength / (FGetSamples * (1 * 16 / 8) + 44) * 100
    else
      Result := FFileLength / (FGetSamples * (2 * 16 / 8) + 44) * 100;
  end
  else
    Result := 0;
end;

{ --------------------------------------------------------------------------- }

function ReadFromTagStream(TagStream: PAnsiChar; StreamSize: longword;
  TagVersion: word; var MP3TagInfo: TMP3TagInfo): boolean;
begin
  Result := False;

  if TagVersion = 1 then
  begin
    if ReadID3v1TagFromStream(TagStream, MP3TagInfo) then
      Result := True;
  end
  else
  if TagVersion = 2 then
    if ReadID3v2TagFromStream(TagStream, StreamSize, MP3TagInfo) then
      Result := True;
end;


end.

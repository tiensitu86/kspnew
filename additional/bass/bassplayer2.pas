unit BassPlayer2;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, Dynamic_Bass, RT_basscd, RT_BASSWMA, Dialogs,
  MPEGAudio, OggVorbis, AACfile, WMAFile, WAVFile, BASS_AAC, RT_bassmidi;

const
  ChannelLimit = 8;           // Allows maximum 8 channels in a stream
  MaxChannels = ChannelLimit;
  MAXCDDRIVES = 4;

  InformPlayerMode = 81;
  InformStreamInfo = 82;

type
  TPlayerMode = (plmStandby, plmReady, plmStopped, plmPlaying, plmPaused);

  TStreamInfo = record
     FileName : string;
     FileSize : DWORD;        // File size in byte
     SampleRate : DWORD;      // Sampling rate in Hz
     BitRate : DWORD;         // Bit Rate in KBPS
     BitsPerSample : Word;    // Bits per sample
     Duration : DWORD;        // playback duration in mili second
     Channels : Word;         // 1- Mono, 2 - Stereo
     Format   : DWORD;        // Stream format   // * Added at Ver 1.44
     Title : string;
     Artist : string;
     Album : string;
     Year : string;
     Genre : string;
     GenreID : byte;
     Track : byte;
     Comment : string;
  end;

  TChannelType = (Channel_NotOpened, Channel_Stream, Channel_CD, Channel_WMA,
                   Channel_Music, Channel_MIDI, Channel_Plugin);

  TSupportedBy = (BASSNative, None);

type TBassPlayer = class
  private
    FVersionStr: string;
    FPlayerMode : TPlayerMode;
    BASSDLLLoaded : boolean;
    FBASSReady      : boolean;
    BassInfoParam : BASS_INFO;
    MPEG        : TMPEGaudio;
    Vorbis      : TOggVorbis;
    AAC         : TAACfile;
    WMA         : TWMAfile;
    WAV         : TWAVFile;
    FBASSWMAReady   : boolean;
    FBASSAACReady   : boolean;   // * Added at Ver 2.00
    FBASSCDReady    : boolean;
    FBASSMIDIReady  : boolean;
    FMIDISoundReady : boolean;
    FNumCDDrives    : integer;
    CDDriveList     : array[0..MAXCDDRIVES-1] of string;
    ChannelType   : TChannelType;
    BassChannelInfo : BASS_CHANNELINFO;
    DecodeChannel : DWORD;
    PlayChannel   : DWORD;
    FGetHTTPHeader  : boolean;
    FDownMixToStereo: boolean;   // * Added at Ver 2.00
    FMixerReady     : boolean;   // * Added at Ver 2.00
    FMixerPlugged   : boolean;   // * Added at Ver 2.00

    FDecoderName    : string;
    FStreamName     : string;
    FStreamInfo     : TStreamInfo;
    FSupportedBy    : TSupportedBy;    // * Added at Ver 2.00
    procedure PausePlay;
    procedure ResumePlay;
    procedure InformPlayerStatus(StatusIndex : integer);
    procedure SetPlayerMode(Mode : TPlayerMode);
    function OpenURL(URL : string;
                             var StreamInfo : TStreamInfo;
                             var SupportedBy : TSupportedBy;
                             Temp_Paused : boolean;
                             CheckOnly : boolean) : boolean;
    function MutedTemporary : boolean;
    procedure RestoreFromMutedState;
  public
    property PlayerReady : boolean read FBASSReady;
    function Open(StreamName : string) : boolean;
    procedure Close;
    function GetNativeFileExts : string;
    function  GetStreamInfo2(StreamName : string;
                            var StreamInfo : TStreamInfo;
                            var SupportedBy : TSupportedBy;
                            var PluginNum : integer;
                            var PluginName : string) : boolean;
    function PlayLength : DWORD;  // get playback length in mili seconds
    constructor Create;
    destructor Destroy;
  end;

implementation

const
   MajorVer = '0';
   MinorVer = '1';
   RevVer = '0';
   BuildDate = '14 July 2009';

   MusicFileExt = '*.MO3;*.IT;*.XM;*.S3M;*.MTM;*.MOD;*.UMX;';
   MIDIFileExt = '*.MID;*.MIDI;*.RMI;*.KAR;';

function GetProgDir : string;
begin
   result := ExtractFilePath(ParamStr(0));
end;

constructor TBassPlayer.Create;
begin
  inherited Create;
  FVersionStr := MajorVer + '.' + MinorVer + '.' + RevVer;
  FPlayerMode := plmStandby;

  BASSDLLLoaded := Load_BASSDLL(GetProgDir + 'bass.dll');
  if not BASSDLLLoaded then
   begin
      exit;
   end;

//  if (HIWORD(BASS_GetVersion)<> BASSVERSION) or (LOWORD(BASS_GetVersion) < 1) then
//   begin
//     ShowErrorMsgBox('BASS version is not ' + BASSVERSIONTEXT + ' !');
//     exit;
//   end;

 //  BASS_SetConfig(BASS_CONFIG_MAXVOL, MaxVolume);  // Set maximum volume range
   BASS_SetConfig(BASS_CONFIG_CD_FREEOLD, 1);   // Automatically free an existing stream
                                                // when creating a new one on the same drive
   BASS_SetConfig(BASS_CONFIG_NET_PLAYLIST, 1);   // * Added at Ver 2.00
   BASS_SetConfig(BASS_CONFIG_WMA_BASSFILE, 1);   // * Added at Ver 2.00
   BASS_SetConfig(BASS_CONFIG_WMA_PREBUF, 1);     // * Added at Ver 2.00

   if not BASS_Init(1, 44100, 0, 0, nil) then
   begin
      ShowMessage('Can''t initialize device');
      exit;
   end else
      FBASSReady := true;

 //  BassInfoParam.size := SizeOf(BassInfoParam);
   BASS_GetInfo(BassInfoParam);

   MPEG        := TMPEGaudio.Create;
    Vorbis      := TOggVorbis.Create;
    AAC         := TAACfile.Create;
    WMA         := TWMAfile.Create;
    WAV         := TWAVFile.Create;
end;

destructor TBassPlayer.Destroy;
begin
 inherited Destroy;
end;

function TBASSPlayer.Open(StreamName : string) : boolean;
var
   IsMusicFile : boolean;
   FromNet : boolean;
   StreamInfo : TStreamInfo;
   OpenFlag : DWORD;
   tmpChannel : DWORD;
   tmpChannelType : TChannelType;
   SupportedBy : TSupportedBy;
   PluginNum : integer;
   StreamName_ : string;
   PluginName : string;
   NameHeader1, NameHeader2 : string;
   ExtCode : string;
   tmpPaused : boolean;
   Using_BASS_AAC : boolean;

begin
  result := false;

   if not FBASSReady then
   begin
      ShowMessage('Player is not ready !');
      exit;
   end;

   FromNet := false;

   StreamName_ := StreamName;
   FillChar(StreamInfo, sizeOf(StreamInfo), 0);

 // remove preceding and trailing '"'.
   if StreamName_[1] = '"' then
      StreamName_ := copy(StreamName_, 2, length(StreamName_) - 1);
   if StreamName_[length(StreamName_)] = '"' then
      StreamName_ := copy(StreamName_, 1, length(StreamName_) - 1);

   NameHeader1 := copy(StreamName_, 1, 7);
   NameHeader2 := copy(StreamName_, 1, 6);
   if (NameHeader1 = 'http://') or (NameHeader2 = 'mms://') then
   begin
      FromNet := true;
      StreamInfo.FileName := StreamName_;
      StreamInfo.BitRate := 0;
   end
   else if not GetStreamInfo2(StreamName_, StreamInfo, SupportedBy, PluginNum, PluginName) then
   begin
      ShowMessage('Invalid or unsupported stream file.'#10' -> ' + StreamName);
      exit;
   end;
   if StreamInfo.Channels > MaxChannels then  // Will this case happen ?
   begin
      ShowMessage('Channel count exceeds. (Max Channels : ' + intToStr(MaxChannels)
                        + ')'#10 + ' -> ' + StreamName);
      exit;
   end;

 //  StreamInfo.Format := 0;  // not determined yet.

   ExtCode := UpperCase(ExtractFileExt(StreamName_));
   if length(ExtCode) = 1 then  // ExtCode = '.' ; the last character of StreamName_ is '.'
      ExtCode := '';            // to prevent mis decision
   if length(ExtCode) >= 3 then
      if pos(ExtCode, MusicFileExt) > 0 then
         IsMusicFile := true
      else
         IsMusicFile := false
   else
      IsMusicFile := false;

  tmpPaused := false;
   if ChannelType <> Channel_NotOpened then
   //   if NetRadio then
         if FPlayerMode = plmPlaying then
         begin
            tmpPaused := true;
            PausePlay;
         end;

   tmpChannelType := Channel_NotOpened;
   Using_BASS_AAC := false;

   if FromNet then   // The streams from internet.
   begin
      if not OpenURL(StreamName_, StreamInfo, SupportedBy,
                     tmpPaused, false{CheckOnly}) then
         exit;
   end;

 // for local stream files
   begin
      if (ExtCode = '.CDA') then
      begin   // (ExtCode = '.CDA')
         if FBASSCDReady then
             {$IFDEF DELPHI_2007_BELOW}
               tmpChannel := BASS_CD_StreamCreateFile(PChar(StreamName_), 0)
             {$ELSE}
               tmpChannel := BASS_CD_StreamCreateFile(PChar(StreamName_), BASS_UNICODE)
             {$ENDIF}
         else
            tmpChannel := 0;
         if (tmpChannel <> 0) then
            tmpChannelType := Channel_CD;
      end else
      begin
         if IsMusicFile then
         begin
               OpenFlag := BASS_MUSIC_PRESCAN + BASS_MUSIC_POSRESET + BASS_MUSIC_RAMPS;
          {$IFDEF DELPHI_2007_BELOW}
            tmpChannel := BASS_MusicLoad(FALSE, PChar(StreamName_), 0, 0, OpenFlag, 0);
          {$ELSE}
            tmpChannel := BASS_MusicLoad(FALSE, PChar(StreamName_), 0, 0, OpenFlag or BASS_UNICODE, 0);
          {$ENDIF}
            if (tmpChannel <> 0) then
            begin
               tmpChannelType := Channel_Music;
               StreamInfo.Duration := PlayLength;
             //  RestartPos := 0;
            end;
         end else if pos(ExtCode, MIDIFileExt) <> 0 then  // is a MIDI file ?
         begin
            if FMIDISoundReady then
                 {$IFDEF DELPHI_2007_BELOW}
                  tmpChannel := BASS_MIDI_StreamCreateFile(FALSE, pChar(StreamName_), 0, 0, 0, 44100)
                 {$ELSE}
                  tmpChannel := BASS_MIDI_StreamCreateFile(FALSE, pChar(StreamName_), 0, 0, BASS_UNICODE, 44100)
                 {$ENDIF}
            else begin
               tmpChannel := 0;
               if FBASSMIDIReady then
                  ShowMessage('None of sound font is loaded.')
            end;

            if tmpChannel <> 0 then
            begin
               tmpChannelType := Channel_MIDI;
               StreamInfo.Duration := PlayLength;
            end;
         end else
         begin
              {$IFDEF DELPHI_2007_BELOW}
               tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName_), 0, 0, 0);
              {$ELSE}
               tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName_), 0, 0, BASS_UNICODE);
              {$ENDIF}
            if (tmpChannel <> 0) then
            begin
               BASS_ChannelGetInfo(tmpChannel, BassChannelInfo);
               if (BassChannelInfo.ctype = BASS_CTYPE_STREAM_WMA) or
                  (BassChannelInfo.ctype = BASS_CTYPE_STREAM_WMA_MP3) then
                   tmpChannelType := Channel_WMA
               else begin
                  if (BassChannelInfo.ctype = BASS_CTYPE_STREAM_AAC) or
                     (BassChannelInfo.ctype = BASS_CTYPE_STREAM_MP4) then
                     Using_BASS_AAC := true;
                  tmpChannelType := Channel_Stream;
               end;
            end;
         end;
      end;

      if (tmpChannel = 0) then  // not a playable file
      begin
         ShowMessage('Invalid or unsupported stream file.'#10' -> ' + StreamName);
         if tmpPaused then
            ResumePlay;
         exit;
      end else
      begin
         Close;
         DecodeChannel := tmpChannel;
         ChannelType := tmpChannelType;
      end;

      BASS_ChannelGetInfo(DecodeChannel, BassChannelInfo);
      StreamInfo.SampleRate := BassChannelInfo.freq;
      StreamInfo.Channels := BassChannelInfo.chans;
      StreamInfo.Format := BassChannelInfo.ctype;

    // Following codes are not necessary in most cases
      if (StreamInfo.BitsPerSample = 32) or
         ((StreamInfo.BitsPerSample = 16) and
         ((BassChannelInfo.flags and BASS_SAMPLE_8BITS) = BASS_SAMPLE_8BITS)) then
      begin
         BASS_MusicFree(DecodeChannel);
         BASS_StreamFree(DecodeChannel);
         if (StreamInfo.BitsPerSample = 32) then
            ShowMessage('Player does not support 32-bit sources.')
         else
            ShowMessage('Sound card does not support 16-bit sources.');
         if tmpPaused then
            ResumePlay;
         exit;
      end;

      if pos(LowerCase(ExtCode), GetNativeFileExts) > 0 then
      begin
         case ChannelType of
           Channel_CD : FDecoderName := 'basscd.dll';
           Channel_MIDI : FDecoderName := 'bassmidi.dll';
           Channel_WMA : FDecoderName := 'basswma.dll';
           else
             if Using_BASS_AAC then
                FDecoderName := 'bass_aac.dll'
              else
                FDecoderName := 'bass.dll';
           end;
      end

   end;   // end of for 'local stream'

   FStreamName := StreamName_;
   FSupportedBy := SupportedBy;
   FStreamInfo := StreamInfo;
   InformPlayerStatus(InformStreamInfo);
   SetPlayerMode(plmReady);
   result := true;
end;

function TBASSPlayer.GetStreamInfo2(StreamName : string;
                       var StreamInfo : TStreamInfo;
                       var SupportedBy : TSupportedBy;
                       var PluginNum : integer;
                       var PluginName : string) : boolean;
var
   i, n : integer;
   f : file;
   tmpTitle : array [1..28] of Char;
   s, s2, ExtCode : string;
   DriveLetter : string[2];

   tmpChannel : DWORD;
   ChannelInfo : BASS_CHANNELINFO;
   ByteLen : int64;

   {$IFDEF DELPHI_2007_BELOW}
   _file : array[0..255] of ansichar;
   {$ENDIF}
   _title : array[0..255] of ansichar;
   _length_in_ms : integer;

begin
   result := false;

   with StreamInfo do
   begin
      FileName := '';
      FileSize := 0;
      SampleRate := 0;
      BitRate := 0;
      BitsPerSample := 16;  // default value
      Duration := 0;
      Channels := 0;
      Format   := 0;        // Unknown format
      Title := '';
      Artist := '';
      Album := '';
      Year := '';
      Genre := '';
      GenreID := 0;
      Track := 0;
      Comment := '';
   end;

   tmpChannel := 0;

   StreamInfo.FileName := StreamName;
   SupportedBy := None;
   PluginNum := -1;
   PluginName := '';

   ExtCode := UpperCase(ExtractFileExt(StreamName));
   if (ExtCode = '') or (ExtCode = '.') or (length(ExtCode) > 5) or
      (length(ExtCode) < 3) then
      exit;

   if (copy(StreamName, 1, 5) = 'http:') or (copy(StreamName, 1, 4) = 'mms:')
      or (copy(StreamName, 1, 4) = 'ftp:') then
      exit;

 // Check if native file types
   if ExtCode = '.WAV' then
   begin
      WAV.ReadFromFile(StreamName);
      if WAV.Valid then
      begin
         with StreamInfo do
         begin
            FileSize := WAV.FileSize;
            SampleRate := WAV.SampleRate;
            BitsPerSample := WAV.BitsPerSample;
            BitRate := (SampleRate * BitsPerSample) div 1000;  // Why not 1024 ?
            Duration := round(WAV.Duration * 1000);
            Channels := WAV.ChannelModeID;
           {$IFDEF DELPHI_2007_BELOW}
            tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName), 0, 0, 0);
           {$ELSE}
            tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName), 0, 0, BASS_UNICODE);
           {$ENDIF}
            if tmpChannel <> 0 then
            begin
               BASS_ChannelGetInfo(tmpChannel, ChannelInfo);
               Format := ChannelInfo.ctype;
               BASS_StreamFree(tmpChannel);
            end;
         end;
         if FBASSReady then
            SupportedBy := BASSNative;
         result := true;
      end;
   end else if (ExtCode = '.MP1') or (ExtCode = '.MP2') or (ExtCode = '.MP3') then
   begin
      MPEG.ReadFromFile(StreamName);
      if MPEG.Valid then
      begin
         with StreamInfo do
         begin
            FileSize := MPEG.FileLength;
            SampleRate := MPEG.SampleRate;
            BitRate := MPEG.BitRate;
            Duration := round(MPEG.Duration * 1000);

            if MPEG.ID3v1.Exists then
            begin
               Title := MPEG.ID3v1.Title;
               Artist := MPEG.ID3v1.Artist;
               Album := MPEG.ID3v1.Album;
               Year := MPEG.ID3v1.Year;
               Genre := MPEG.ID3v1.Genre;
               GenreID := MPEG.ID3v1.GenreID;
               Track := MPEG.ID3v1.Track;
               Comment := MPEG.ID3v1.Comment;
            end else if MPEG.ID3v2.Exists then
            begin
               Title := MPEG.ID3v2.Title;
               Artist := MPEG.ID3v2.Artist;
               Album := MPEG.ID3v2.Album;
               Year := MPEG.ID3v2.Year;
               Genre := MPEG.ID3v2.Genre;
               Track := MPEG.ID3v2.Track;
               Comment := MPEG.ID3v2.Comment;
            end;

            if MPEG.ChannelMode = 'Mono' {MPEG_CM_MONO} then
               Channels := 1
            else
               Channels := 2;
            if MPEG.Layer = MPEG_LAYER[1] then
               Format := BASS_CTYPE_STREAM_MP3
            else if MPEG.Layer = MPEG_LAYER[2] then
               Format := BASS_CTYPE_STREAM_MP2
            else if MPEG.Layer = MPEG_LAYER[3] then
               Format := BASS_CTYPE_STREAM_MP1;

         end;

         if FBASSReady then
            SupportedBy := BASSNative;
         result := true;
      end;
   end else if ExtCode = '.OGG' then
   begin
      Vorbis.ReadFromFile(StreamName);
      if Vorbis.Valid then
      begin
         with StreamInfo do
         begin
            FileSize := Vorbis.FileSize;
            SampleRate := Vorbis.SampleRate;
            BitRate := Vorbis.BitRate;
            Duration := round(Vorbis.Duration * 1000);
            Channels := Vorbis.ChannelModeID;
            Title := Vorbis.Title;
            Artist := Vorbis.Artist;
            Album := Vorbis.Album;
            Year := Vorbis.Date;
            Genre := Vorbis.Genre;
            Track := Vorbis.Track;
            Comment := Vorbis.Comment;
            Format := BASS_CTYPE_STREAM_OGG;
         end;
         if FBASSReady then
            SupportedBy := BASSNative;
         result := true;
      end;
   end else if (ExtCode = '.AAC') then
   begin
      AAC.ReadFromFile(StreamName);
      if AAC.Valid then
      begin
         with StreamInfo do
         begin
            FileSize := AAC.FileSize;
            SampleRate := AAC.SampleRate;
            BitRate := AAC.BitRate div 1000;
            Duration := round(AAC.Duration * 1000);
            Channels := AAC.Channels;
            if AAC.ID3v1.Exists then
            begin
               Title := AAC.ID3v1.Title;
               Artist := AAC.ID3v1.Artist;
               Album := AAC.ID3v1.Album;
               Year := AAC.ID3v1.Year;
               Genre := AAC.ID3v1.Genre;
               GenreID := AAC.ID3v1.GenreID;
               Track := AAC.ID3v1.Track;
               Comment := AAC.ID3v1.Comment;
            end else if AAC.ID3v2.Exists then
            begin
               Title := AAC.ID3v2.Title;
               Artist := AAC.ID3v2.Artist;
               Album := AAC.ID3v2.Album;
               Year := AAC.ID3v2.Year;
               Genre := AAC.ID3v2.Genre;
               Track := AAC.ID3v2.Track;
               Comment := AAC.ID3v2.Comment;
            end;
            Format := BASS_CTYPE_STREAM_AAC;
         end;
         if FBASSAACReady then
         begin
        // AACfile.pas Version 1.2 reports wrong BitRate & Duration for MPEG-4 files.
            if AAC.MPEGVersionID = AAC_MPEG_VERSION_4 then
            begin
              {$IFDEF DELPHI_2007_BELOW}
               tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName), 0, 0, 0);
              {$ELSE}
               tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName), 0, 0, BASS_UNICODE);
              {$ENDIF}
               if tmpChannel <> 0 then
               begin
                  ByteLen := BASS_ChannelGetLength(tmpChannel, BASS_POS_BYTE);
                  StreamInfo.Duration := round(BASS_ChannelBytes2Seconds(tmpChannel, ByteLen) * 1000.0);
                  ByteLen := BASS_StreamGetFilePosition(tmpChannel, BASS_FILEPOS_END);
                  StreamInfo.BitRate := round(ByteLen / (0.125 * StreamInfo.Duration));   // bitrate (Kbps)
                  BASS_StreamFree(tmpChannel);
               end;
            end;
            SupportedBy := BASSNative;
         end;
         result := true;
      end;
   end else if (ExtCode = '.WMA') or (ExtCode = '.ASF') then  // * Changed at Ver 2.00
   begin                                                      //   (add for 'ASF' files)
      WMA.ReadFromFile(StreamName);
      if WMA.Valid then
      begin
         with StreamInfo do
         begin
            FileSize := WMA.FileSize;
            SampleRate := WMA.SampleRate;
            BitRate := WMA.BitRate;
            Duration := round(WMA.Duration * 1000);
            Title := WMA.Title;
            Artist := WMA.Artist;
            Album := WMA.Album;
            Year := WMA.Year;
            Genre := WMA.Genre;
            Channels := WMA.ChannelModeID;
            Track := WMA.Track;
            Comment := WMA.Comment;
            Format := BASS_CTYPE_STREAM_WMA;
         end;
         if FBASSWMAReady then
            SupportedBy := BASSNative;
         result := true;
      end;
   end else if ExtCode = '.CDA' then
   begin
      if FBASSCDReady then
      begin
         DriveLetter := copy(UpperCase(StreamName), 1, 1);
         for i := 0 to (FNumCDDrives - 1) do
         begin
          // Check if the specified stream file is in a Audio CD.
            if DriveLetter = copy(CDDriveList[i], 1, 1) then
            begin
               n := strToint(copy(StreamName, length(StreamName) - 5, 2)) - 1;
               with StreamInfo do
               begin
                  FileSize := BASS_CD_GetTrackLength(i, n);
                  SampleRate := 44100;
                  BitRate := 1411;
                  Duration := (FileSize * 10) div 1764;  // in mili seconds
                  Channels := 2;
                  Track := n;
                  Format := BASS_CTYPE_STREAM_CD;
               end;
               SupportedBy := BASSNative;
               result := true;
            end;
         end;
      end;

   end else if (ExtCode = '.MO3') or (ExtCode = '.IT') or (ExtCode = '.XM') or
      (ExtCode = '.S3M') or (ExtCode = '.MTM') or (ExtCode = '.MOD') or
      (ExtCode = '.UMX') then
   begin
  // I do not have full technical information on MOD music, so only basic information
  // is given to StreamInfo record.
      AssignFile(f, StreamName);
      Reset(f, 1);
      StreamInfo.FileSize := FileSize(f);

      with StreamInfo do
      begin
     // Get the title for some types of music files.
     // I am not sure it always works correctly.
         FillChar(tmpTitle, SizeOf(tmpTitle), ' ');
         if (ExtCode = '.MOD') or (ExtCode = '.MTM') or (ExtCode = '.XM') or
            (ExtCode = '.IT') then
         begin
            if (ExtCode = '.XM') then
                Seek(f, $11)
            else if (ExtCode = '.IT') or (ExtCode = '.MTM') then
                Seek(f, 4);
            BlockRead(f, tmpTitle, 20);
            Title := TrimRight(tmpTitle);
         end else if (ExtCode = '.S3M') then
         begin
            BlockRead(f, tmpTitle, 28);
            Title := TrimRight(tmpTitle);
         end else    // for MO3 and UMX file ( I cannot get any documentation on these files )
            Title := ExtractFileName(StreamName);  // Not the title given to music file

         CloseFile(f);
        {$IFDEF DELPHI_2007_BELOW}
         tmpChannel := BASS_MusicLoad(FALSE, PChar(StreamName), 0, 0, BASS_MUSIC_PRESCAN, 0);
        {$ELSE}
         tmpChannel := BASS_MusicLoad(FALSE, PChar(StreamName), 0, 0, BASS_MUSIC_PRESCAN or BASS_UNICODE, 0);
        {$ENDIF}
         if tmpChannel <> 0 then
         begin
            ByteLen := BASS_ChannelGetLength(tmpChannel, BASS_POS_BYTE);
            Duration := round(BASS_ChannelBytes2Seconds(tmpChannel, ByteLen) * 1000.0);
            BASS_ChannelGetInfo(tmpChannel, ChannelInfo);
            SampleRate := ChannelInfo.freq;
            if (ChannelInfo.flags and BASS_SAMPLE_8BITS) = BASS_SAMPLE_8BITS then
               BitsPerSample := 8
            else if (ChannelInfo.flags and BASS_SAMPLE_FLOAT) = BASS_SAMPLE_FLOAT then
               BitsPerSample := 32;
            Channels := ChannelInfo.chans;
            Format := ChannelInfo.ctype;
            BitRate := 0;  // Meaningless value for music files
            BASS_MusicFree(tmpChannel);
            SupportedBy := BASSNative;
            result := true;
         end;
      end;
   end else if pos(ExtCode, MIDIFileExt) > 0 then
   begin
      if FMIDISoundReady then
      begin
         AssignFile(f, StreamName);
         Reset(f, 1);
         StreamInfo.FileSize := FileSize(f);
         CloseFile(f);

         StreamInfo.FileName := StreamName;

         if FMIDISoundReady then
          {$IFDEF DELPHI_2007_BELOW}
            tmpChannel := BASS_MIDI_StreamCreateFile(FALSE, pChar(StreamName), 0, 0, 0, 44100);
          {$ELSE}
            tmpChannel := BASS_MIDI_StreamCreateFile(FALSE, pChar(StreamName), 0, 0, BASS_UNICODE, 44100);
          {$ENDIF}
         if tmpChannel <> 0 then
         begin
            ByteLen := BASS_ChannelGetLength(tmpChannel, BASS_POS_BYTE);
            StreamInfo.Duration := round(BASS_ChannelBytes2Seconds(tmpChannel, ByteLen) * 1000.0);
            BASS_StreamFree(tmpChannel);
         end;

         StreamInfo.SampleRate := 44100;
         StreamInfo.BitRate := 0;
         StreamInfo.Format := BASS_CTYPE_STREAM_MIDI;
         SupportedBy := BASSNative;
         result := true;
      end;
   end else
   begin   // Check if the opening stream is playable by an add-on.
      AssignFile(f, StreamName);
      Reset(f, 1);
      StreamInfo.FileSize := FileSize(f);
      CloseFile(f);

      S := UpperCase('.AIFF;');
      if FBASSAACReady then
         S := S + '.M4A;.MP4;'; // acceptable by bass_aac.dll (*.AAC files are checked independently)
    //  s2 := copy(ExtCode, 2, length(ExtCode) - 1);
      s2 := s2 + ';';
      if pos(s2, s) > 0 then
      begin
        {$IFDEF DELPHI_2007_BELOW}
         tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName), 0, 0, 0);
        {$ELSE}
         tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName), 0, 0, BASS_UNICODE);
        {$ENDIF}
         if tmpChannel <> 0 then
         begin
            ByteLen := BASS_ChannelGetLength(tmpChannel, BASS_POS_BYTE);
            StreamInfo.Duration := round(BASS_ChannelBytes2Seconds(tmpChannel, ByteLen) * 1000.0);
            BASS_ChannelGetInfo(tmpChannel, ChannelInfo);
            StreamInfo.SampleRate := ChannelInfo.freq;
            if (ChannelInfo.flags and BASS_SAMPLE_8BITS) = BASS_SAMPLE_8BITS then
               StreamInfo.BitsPerSample := 8
            else if (ChannelInfo.flags and BASS_SAMPLE_FLOAT) = BASS_SAMPLE_FLOAT then
               StreamInfo.BitsPerSample := 32;
            StreamInfo.Channels := ChannelInfo.chans;
            StreamInfo.Format := ChannelInfo.ctype;

            ByteLen := BASS_StreamGetFilePosition(tmpChannel, BASS_FILEPOS_END);
            StreamInfo.BitRate := round(ByteLen / (0.125 * StreamInfo.Duration));   // bitrate (Kbps)

            BASS_StreamFree(tmpChannel);
            SupportedBy := BASSNative;
            result := true;
         end;
      end;
   end;

   if (result = true) and (StreamInfo.Title = '') then
      StreamInfo.Title := ExtractFileName(StreamName);
end;

procedure TBASSPlayer.PausePlay;
var
   tmpMuted : boolean;
begin
   if FPlayerMode <> plmPlaying then
      exit;

   if MutedTemporary then
      tmpMuted := true
   else
      tmpMuted := false;

   BASS_ChannelPause(PlayChannel);

 //  if ChannelType = Channel_Music then
 //     MusicPauseTime := timeGetTime - MusicStartTime;
   if tmpMuted then
      RestoreFromMutedState;

   SetPlayerMode(plmPaused);
end;

function TBASSPlayer.OpenURL(URL : string;
                             var StreamInfo : TStreamInfo;
                             var SupportedBy : TSupportedBy;
                             Temp_Paused : boolean;
                             CheckOnly : boolean) : boolean;
var
  PluginNum : integer;
  OpenFlag : DWORD;
  ExtCode, LoExtCode : string;
  TagP : pAnsiChar;
  tmpChannel : DWORD;
  tmpChannelType : TChannelType;
  tmpStr : string;
  tmpStr1 : ansistring;
  tmpStr2 : wideString;
  tmpPChar : pAnsiChar;
  RepeatCounter : integer;
  PlaybackSec : float;
  FileLen : integer;
  TitlePos, DelimeterPos : integer;
  Using_BASS_AAC : boolean;

  function ExtractFileName2(NetStream : string) : string;
  var
    pos, i : integer;
  begin
     pos := 0;
     for i := (length(NetStream) - 6)  downto 6 do
        if NetStream[i] = '/' then
        begin
           pos := i;
           break;
        end;

     if pos > 0 then
        result := copy(NetStream, pos + 1, length(NetStream) - pos - 4)
     else
        result := '';
  end;

begin
   result := false;
   tmpChannel := 0;
   tmpChannelType := Channel_NotOpened;
   Using_BASS_AAC := false;

   ExtCode := UpperCase(ExtractFileExt(URL));
   LoExtCode := LowerCase(ExtCode);

 //  NameHeader1 := copy(URL, 1, 7);
 //  NameHeader2 := copy(URL, 1, 6);


 //----------------------------- Check validity of URL -------------------------------

  // ASF and WMA files are streamed with Winamp input plug-in for the file types,
  //  only if basswma.dll is not loaded.
   begin
      OpenFlag := 0;

      if pos(ExtCode, MIDIFileExt) = 0 then  // not a MIDI file ?
      begin
        {$IFDEF DELPHI_2007_BELOW}
         tmpChannel := BASS_StreamCreateURL(pChar(URL), 0, OpenFlag, nil, nil);
        {$ELSE}
         tmpChannel := BASS_StreamCreateURL(ToPMultiByte(pWideChar(URL)), 0, OpenFlag, nil, nil);
        {$ENDIF}
         if tmpChannel <> 0 then
         begin
            BASS_ChannelGetInfo(tmpChannel, BassChannelInfo);
            if (BassChannelInfo.ctype = BASS_CTYPE_STREAM_WMA) or
               (BassChannelInfo.ctype = BASS_CTYPE_STREAM_WMA_MP3) then
                tmpChannelType := Channel_WMA
            else begin
               tmpChannelType := Channel_Stream;
               if (BassChannelInfo.ctype = BASS_CTYPE_STREAM_AAC) or
                  (BassChannelInfo.ctype = BASS_CTYPE_STREAM_MP4) then
                  Using_BASS_AAC := true;
            end;
         end;

      end else if FMIDISoundReady then
      begin
         FGetHTTPHeader := false;
        {$IFDEF DELPHI_2007_BELOW}
         tmpChannel := BASS_MIDI_StreamCreateURL(pChar(URL), 0,
                                                 OpenFlag + BASS_STREAM_STATUS,
                                                 @DownProc, @MessageHandle, 44100);
        {$ELSE}
         tmpChannel := BASS_MIDI_StreamCreateURL(ToPMultiByte(pWideChar(URL)), 0,
                                                 OpenFlag + BASS_STREAM_STATUS,
                                                 @DownProc, @MessageHandle, 44100);
        {$ENDIF}
         if tmpChannel <> 0 then
            tmpChannelType := Channel_MIDI;
      end else if FBASSMIDIReady then
         if not CheckOnly then
            ShowErrorMsgBox('None of sound font is loaded.');

      if tmpChannel <> 0 then
      begin
         if not CheckOnly then
         begin
            Close;
            DecodeChannel := tmpChannel;
            ChannelType := tmpChannelType;

       // BASSMIDI pre-downloads the entire MIDI file, and closes the file/connection
       // before the stream creation function(BASS_MIDI_StreamCreateURL) returns.
           if ChannelType = Channel_MIDI then
              FDownLoaded := true;
         end else
         begin
            case ChannelType of
               Channel_Music : BASS_MusicFree(tmpChannel);
               Channel_CD, Channel_Stream, Channel_MIDI, Channel_WMA :
                         BASS_StreamFree(tmpChannel);
            end;
         end;
      end else
      begin     // for tmpChannel = 0
      // Check if the file type of URL stream is playable file type by Winamp input plug-in.
      // note) Not all the Winamp input plug-ins support URL stream.
      //       So, Following code should be modified. (But I do not know the method to
      //       classify them.)
         if FPluginReady and (pos(LoExtCode, GetPluginFileExts) > 0) then
         begin
            if length(ExtCode) >= 3 then
               PluginNum := GetPluginNumber(copy(ExtCode, 2, length(ExtCode) - 1))
            else
               PluginNum := -1;
            if PluginNum = - 1 then
            begin
              if not CheckOnly then
              begin
                 ShowErrorMsgBox('Invalid or unsupported URL stream.'#10' -> ' + URL);
                 if Temp_Paused then
                    ResumePlay;
               end;
               exit;
            end;
            if not CheckOnly then
               SetupWinampPlugin(PluginNum);
            StreamInfo.SampleRate := 0;
            StreamInfo.Duration := 0;
         end else
         begin
            if not CheckOnly then
            begin
              ShowErrorMsgBox('Invalid or unsupported URL stream.'#10' -> ' + URL);
              if Temp_Paused then
                 ResumePlay;
            end;
            exit;
         end;
      end;
   end;

 //----------------------- End of checking validity of URL ----------------------------

   result := true;
   if CheckOnly then
      exit;

   if (pos(LoExtCode, GetNativeFileExts) > 0) or
      (pos(LoExtCode, GetBASSAddOnExts) > 0) or
      (pos(LoExtCode, GetPluginFileExts) > 0) then
   begin
      NetStream := true;

      if (ChannelType <> Channel_Plugin) and (ExtCode = '.OGG') then
      begin
         TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_OGG);
         if TagP <> nil then
         while StrLen(TagP) > 0 do
         begin
          {$IFDEF DELPHI_2007_BELOW}
            tmpStr := ansistring(TagP);
          {$ELSE}
            tmpStr := ToWideString(ansistring(TagP));
          {$ENDIF}
            if pos('TITLE=', tmpStr) <> 0 then
            begin
               StreamInfo.Title := copy(tmpStr, 7, length(tmpStr) - 6);
               break;
            end else if pos('ARTIST=', tmpStr) <> 0 then
               StreamInfo.Artist := copy(tmpStr, 8, length(tmpStr) - 7)
            else if pos('GENRE=', tmpStr) <> 0 then
               StreamInfo.Genre := copy(tmpStr, 7, length(tmpStr) - 6)
            else if pos('ALBUM=', tmpStr) <> 0 then
               StreamInfo.Album := copy(tmpStr, 7, length(tmpStr) - 6);

            inc(TagP, StrLen(TagP) + 1);
         end;

   // <-
      end else if (ChannelType = Channel_WMA) then
      begin
         TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_WMA);
         ICYTag := TagP;
         if TagP <> nil then
         while StrLen(TagP) > 0 do
         begin
            tmpStr1 := ansistring(TagP);
           {$IFDEF DELPHI_2007_BELOW}
            if pos('FileSize=', tmpStr1) <> 0 then
           {$ELSE}
            if posEX('FileSize=', tmpStr1, 1) <> 0 then
           {$ENDIF}
               StreamInfo.FileSize := strToInt(copy(tmpStr1, 10, length(tmpStr1) - 9));
            if copy(tmpStr1, 1, 8) = 'Bitrate=' then   // to exclude 'CurrentBitrate=', 'OptimalBitrate='
            begin
             //  SepPos := pos('=', tmpStr1);
               StreamInfo.Bitrate := strToInt(copy(tmpStr1, 9, length(tmpStr1) - 8)) div 1000;
            end;
           {$IFDEF DELPHI_2007_BELOW}
            if pos('Title=', tmpStr1) <> 0 then
           {$ELSE}
            if posEX('Title=', tmpStr1, 1) <> 0 then
           {$ENDIF}
            begin
              {$IFDEF DELPHI_2007_BELOW}
               tmpStr2 := UTF8Decode(tmpStr1);
              {$ELSE}
               tmpStr2 := UTF8ToWideString(tmpStr1);
              {$ENDIF}
               tmpPChar := ToPMultiByte(PWideChar(tmpStr2));
               tmpStr1 := ansistring(tmpPChar);
               StreamInfo.Title := copy(tmpStr1, 7, length(tmpStr1) - 6);
            end;
           {$IFDEF DELPHI_2007_BELOW}
            if pos('Author=', tmpStr1) <> 0 then
           {$ELSE}
            if posEX('Author=', tmpStr1, 1) <> 0 then
           {$ENDIF}
            begin
              {$IFDEF DELPHI_2007_BELOW}
               tmpStr2 := UTF8Decode(tmpStr1);
              {$ELSE}
               tmpStr2 := UTF8ToWideString(tmpStr1);
              {$ENDIF}

               tmpPChar := ToPMultiByte(PWideChar(tmpStr2));
               tmpStr1 := ansistring(tmpPChar);
               StreamInfo.Artist := copy(tmpStr1, 8, length(tmpStr1) - 7);
            end;
            inc(TagP, StrLen(TagP) + 1);
         end;  // <-

         if StreamInfo.Title = '' then
            StreamInfo.Title := ExtractFileName2(URL);

   // We can get the TITLE & Playback length of the stream files from internet
   // at just before the starting playback from Winamp input plug-in if decoded
   // by Winamp input plug-in.
      end else if (ChannelType <> Channel_Plugin) then
         StreamInfo.Title := ExtractFileName2(URL);

      if ChannelType <> Channel_Plugin then
      begin
         if ChannelType <> Channel_MIDI then
         begin
            TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_HTTP);
            HTTPTag := TagP;
         end else
         begin
            RepeatCounter := 0;
            repeat
               sleep(30);
               inc(RepeatCounter);
               WinProcessMessages;
            until FGetHTTPHeader or (RepeatCounter = 100);

            if FGetHTTPHeader then
               HTTPTag := tmpHTTPTag;
         end;

      // BASS_StreamGetLength may return a very rough estimation until the whole file
      // has been downloaded.
      // So it is needed to re-estimate after finishing download.
         PlaybackSec := BASS_ChannelBytes2Seconds(DecodeChannel, BASS_ChannelGetLength(DecodeChannel,
                                                     BASS_POS_BYTE));
         StreamInfo.Duration := round(1000 * PlaybackSec);  // in mili seconds
         FileLen := BASS_StreamGetFilePosition(DecodeChannel, BASS_FILEPOS_END); // file length
         if FileLen <> -1 then
         begin
            if ChannelType <> Channel_WMA then  // obtained at getting tag info for WMA stream
            begin
              StreamInfo.FileSize := FileLen;
              if PlaybackSec > 0 then
                 StreamInfo.BitRate := round(FileLen / (125 * PlaybackSec))   // bitrate (Kbps)
              else
                 StreamInfo.BitRate := 0;
            end;

          // BASS add-on "bass_spx.dll" does not issue Sync signal when downloading of the stream
          //  file from internet is done.
          // So, I use following compare clause to check if download process is done.
            if FileLen = BASS_StreamGetFilePosition(DecodeChannel, BASS_FILEPOS_DOWNLOAD) then
               FDownloaded := true;
         end;

         if not FDownloaded then
            BASS_ChannelSetSync(DecodeChannel, BASS_SYNC_DOWNLOAD, 0, @DownloadSync, @MessageHandle);
      end;
   end else   // = Shoutcast/Icecast server
   begin
      NetRadio := true;

      if (ChannelType = Channel_WMA) then
      begin
         TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_WMA);
         if TagP <> nil then
         while StrLen(TagP) > 0 do
         begin
            tmpStr1 := ansistring(TagP);
            if copy(tmpStr1, 1, 8) = 'Bitrate=' then   // to exclude 'CurrentBitrate=', 'OptimalBitrate='
            begin
             //  SepPos := pos('=', tmpStr1);
               StreamInfo.Bitrate := strToInt(copy(tmpStr1, 9, length(tmpStr1) - 8)) div 1000;
               break;
            end;

            inc(TagP, StrLen(TagP) + 1);
         end;

         TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_WMA_META);
         if TagP <> nil then
         begin
            tmpStr1 := ansistring(TagP);
           {$IFDEF DELPHI_2007_BELOW}
            if pos('Title=', tmpStr1) <> 0 then
           {$ELSE}
            if posEX('Title=', tmpStr1, 1) <> 0 then
           {$ENDIF}
               StreamInfo.Title := copy(tmpStr1, 7, length(tmpStr1) - 6);
         end;
      end else
      begin    // for ChannelType <> Channel_WMA
        TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_ICY);  // for Shoutcast
        if TagP = nil then
           TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_HTTP);  // for Icecast
        ICYTag := TagP;
        if TagP <> nil then
           while StrLen(TagP) > 0 do
           begin
            {$IFDEF DELPHI_2007_BELOW}
             if pos('icy-br:', ansistring(TagP)) <> 0 then
            {$ELSE}
             if posEX('icy-br:', ansistring(TagP), 1) <> 0 then
            {$ENDIF}
             begin
               inc(TagP, 7);
               StreamInfo.BitRate := strToInt(ansistring(TagP));
               break;
             end else
            {$IFDEF DELPHI_2007_BELOW}
             if pos('ice-bitrate:', ansistring(TagP)) <> 0 then
            {$ELSE}
             if posEX('ice-bitrate:', ansistring(TagP), 1) <> 0 then
            {$ENDIF}
             begin
               inc(TagP, 12);
               StreamInfo.BitRate := strToInt(ansistring(TagP));
               break;
             end;

             inc(TagP, StrLen(TagP) + 1);
           end;

        TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_META);
        if TagP <> nil then
        begin
          tmpStr1 := ansistring(TagP);
         {$IFDEF DELPHI_2007_BELOW}
          TitlePos := Pos('StreamTitle=', tmpStr1);
         {$ELSE}
          TitlePos := PosEX('StreamTitle=', tmpStr1, 1);
         {$ENDIF}
        end else
          TitlePos := 0;

        if TitlePos <> 0 then
        begin
          {$IFDEF DELPHI_2007_BELOW}
          DelimeterPos := Pos(';', tmpStr1);
          {$ELSE}
          DelimeterPos := PosEx(';', tmpStr1);
          {$ENDIF}
          if DelimeterPos = 0 then
             StreamInfo.Title := copy(tmpStr1, TitlePos + 13, length(tmpStr1) - TitlePos - 13)
          else
             StreamInfo.Title := copy(tmpStr1, TitlePos + 13, DelimeterPos - TitlePos - 14);
        end else
          StreamInfo.Title := '';

      end;

      MetaSyncParam.ChannelType := ChannelType;
      if (ChannelType = Channel_WMA) then
      begin
         BASS_ChannelSetSync(DecodeChannel, BASS_SYNC_WMA_META, 0, @MetaSync, @MetaSyncParam);
         BASS_ChannelSetSync(DecodeChannel, BASS_SYNC_WMA_CHANGE, 0, @MetaSync, @MetaSyncParam);
      end else
         BASS_ChannelSetSync(DecodeChannel, BASS_SYNC_META, 0, @MetaSync, @MetaSyncParam);
   end;

   if (ChannelType <> Channel_Plugin) then
   begin
      BASS_ChannelGetInfo(DecodeChannel, BassChannelInfo);
      StreamInfo.SampleRate := BassChannelInfo.freq;
      StreamInfo.Channels := BassChannelInfo.chans;
      StreamInfo.Format := BassChannelInfo.ctype;

   // Apply mixer if source has multi channels.   (* Added at Ver 2.00)
      if (StreamInfo.Channels > 2) and FDownMixToStereo then
         if FMixerReady and (not FSingleChannel) then
         begin
         // create stereo mixer
            if HMixer = 0 then
               HMixer := BASS_Mixer_StreamCreate(BassChannelInfo.freq, 2, BASS_MIXER_END or BASS_STREAM_DECODE);
            if HMixer <> 0 then
             // plug in the decoder
               FMixerPlugged := BASS_Mixer_StreamAddChannel(HMixer, DecodeChannel, BASS_MIXER_DOWNMIX);
         end;

      if (pos(LoExtCode, GetNativeFileExts) > 0) or NetRadio then
      begin
         case ChannelType of
           Channel_CD : FDecoderName := 'basscd.dll';
           Channel_MIDI : FDecoderName := 'bassmidi.dll';
           Channel_WMA : FDecoderName := 'basswma.dll';
           else
             if Using_BASS_AAC then
                FDecoderName := 'bass_aac.dll'
              else
                FDecoderName := 'bass.dll';
           end;
      end else
         FDecoderName := GetDecoderName(lowercase(ExtCode));

      SupportedBy := BASSNative;
   end else
      SupportedBy := WinampPlugin;
end;

function TBASSPlayer.PlayLength : DWORD;  // get playback length in mili seconds
var
   SongLength : int64;
   FloatLen : FLOAT;
begin
   result := 0;
   case ChannelType of
      Channel_NotOpened : exit;
      Channel_Stream..Channel_MIDI :
                 SongLength := BASS_ChannelGetLength(DecodeChannel, BASS_POS_BYTE);
      Channel_Plugin : begin
                         result := FStreamInfo.Duration;
                         exit;
                       end;
       else
          SongLength := 0;
   end;

   if SongLength <= 0 then  // can be -1 if DecodeChannel is not available (ex. BASS add-on was freed.)
   begin
      result := 0;
      exit;
   end;

   FloatLen := BASS_ChannelBytes2Seconds(DecodeChannel, SongLength);
   result := round(1000 * FloatLen);   // sec -> milli sec
end;

procedure TBASSPlayer.ResumePlay;
var
   tmpMuted : boolean;
   i : integer;
   ResumePos : DWORD;
begin
   if FPlayerMode <> plmPaused then
      exit;

   if not FMute then
   begin
     SetMuteState(true, 0);   // Mute instantly - no fade-out time is needed (not playing state)
     tmpMuted := true;
   end else
     tmpMuted := false;

 // Compensate non-played sound data in buffer, unheard time during fade-in, fade-out
   ResumePos := CurrentPosition;
   if ResumePos > 500 then
      ResumePos := ResumePos - 500    // backward 0.5 Sec
   else
      ResumePos := 0;

   SetPosition(ResumePos);

 // It is not possible to resume the paused stream from NetRadio. (-> required real time processing.)
 // So, we should restart it.
 // note) Normal restart (= resume playing the previously opened stream) causes problem.
   if NetRadio then   // * Added at Ver 2.00
   begin
      if Open(FStreamName) then    // re-open for clean restart
         Play;                     // then play
      if tmpMuted then
         SetMuteState(false, 500);
      exit;
   end;

   for i := 0 to (NumFFTBands - 1) do
      BandOut[i] := 0;

 // * Fadeout, Fadein ±â´ÉŔ» Ăß°ˇÇŃ ČÄ Pause ČÄ Resume˝Ăżˇ °ŁČ¤ ż¬ÁÖŔ˝ ´ë˝Ĺ ŔâŔ˝ŔĚ Ăâ·ÂµÇ´Â
 //    ą®Á¦°ˇ ąß»ýµĘ.
 //   ŔĚ ą®Á¦´Â Resume˝Ă ąöĆŰ¸¦ Ĺ¬¸®ľî ÇĎ¸é łŞĹ¸łŞÁö ľĘŔ¸ąÇ·Î NeedFlush °ŞŔ» True·Î ĽĽĆ®ÇÔ.
 //    2009-01-25
   NeedFlush := true;

   if ChannelType = Channel_Plugin then
      APlugin.UnPause
   else if NeedFlush then
   begin
      if not FSingleChannel2 then
         ClearBuffer;
      BASS_ChannelPlay(PlayChannel, true);
      NeedFlush := false;
   end else
      BASS_ChannelPlay(PlayChannel, false);

 //  if ChannelType = Channel_Music then
 //     MusicStartTime := timeGetTime - MusicPauseTime;
   if tmpMuted then
      SetMuteState(false, 500);
   SetPlayerMode(plmPlaying);

   if Assigned(FOnNewFFTData) then
      if ChannelType <> Channel_Plugin then
      begin
       //  if VisWindowHandle = 0 then  // vis plug-in is not active ?
            TimerFFT.Enabled := true
      end else if UsesOutputPlugin then
       //  if VisWindowHandle = 0 then  // vis plug-in is not active ?
            TimerFFT.Enabled := true;
end;

procedure TBASSPlayer.Close;
begin
   if ChannelType = Channel_NotOpened then
      exit;

   if ChannelType <> Channel_Plugin then
      if not FSingleChannel2 then
         omodClose2;   // close playing channel

   if FMixerPlugged then
   begin
      BASS_Mixer_ChannelRemove(DecodeChannel);
      FMixerPlugged := false;
   end;

   case ChannelType of
      Channel_Music : BASS_MusicFree(DecodeChannel);
      Channel_CD, Channel_Stream, Channel_MIDI, Channel_WMA :
                       BASS_StreamFree(DecodeChannel);
      Channel_Plugin : begin
                          if PlayChannel <> 0 then
                             APlugin.Stop;
                          FPluginNum := -1;
                          APlugin := nil;
                          SelectInputPlugin(-1);
                          PluginConfigForm.SetInUsePlugin('');
                       end;
   end;

   PlayChannel := 0;
   ChannelType := Channel_NotOpened;
   FStreamName := '';
   FStreamInfo.FileName := '';
   FStreamInfo.FileSize := 0;
   FStreamInfo.SampleRate := 0;
   ClearEffectHandle;
   FDecodingByPlugin := false;
 //  FUsing_BASS_AAC := false;
   FDecoderName := '';
   FDownLoaded := false;
   NetStream := false;
   NetRadio  := false;
   ICYTag    := nil;
   HTTPTag := nil;

   if FPlayerMode <> plmStandby then
      SetPlayerMode(plmStandby);
end;

function TBASSPlayer.GetNativeFileExts : string;
begin
   if not FBASSReady then
   begin
      result := '';
      exit;
   end;

   result := StreamFileExt + Lowercase(MusicFileExt);
   if FBASSCDReady then
      result := result +  '*.cda;';
   if FBASSWMAReady then
      result := result + '*.wma;*.asf;';  // * Changed at Ver 2.00
   if HBASSAAC <> 0 then         // * Added at Ver 2.00
      result := result + '*.aac;*.m4a;*.mp4;';
   if FMIDISoundReady then
      result := result + Lowercase(MIDIFileExt);
end;

procedure TBASSPlayer.InformPlayerStatus(StatusIndex : integer);
var
   p1 : PDWORD;
   p2 : PBYTE;
//   ModeId : DWORD;
   TitleBuf : array[0..MaxTitleLen] of char;

// The DataBuffer defined in PluginCtrl.pas is used as follows
//    byte offset       usage
//      0 -    3     Player Mode
//      4 -  275     Stream Information
//    276 -  279     SyncWindows flag / EMBED window Handle
//    280 -  283     Flag to notify that new FFT data is given
//    284 -  287     Sample rate
//    288 -  291     number of channels
//    292 -  295     playback position;
//    296 - 2599     FFT Data
//   2600 - 2855     Stream file path

begin
   if StatusIndex = InformStreamInfo then
      SetStreamInfo2(FStreamInfo);

   if not GoVisOut then
      exit;

   p1 := ShareMemPointer;

   case StatusIndex of
     // following sentences for "stPlayerMode" are useless (executed in procedure "SetPlayerMode")
     { stPlayerMode : begin
           ModeId := ord(FPlayerMode);
           p1^ := ModeId;
           VisDataThread.InformVisDriver(InformPlayerMode, 4);
         end; }

      InformStreamInfo : begin
           inc(p1, 1);
           p1^ := FStreamInfo.SampleRate;
           inc(p1, 1);
           p1^ := FStreamInfo.BitRate;
           inc(p1, 1);
           if FMixerPlugged then
              p1^ := 2
           else
              p1^ := FStreamInfo.Channels;
           inc(p1, 1);
           p1^ := FStreamInfo.Duration;
           inc(p1, 1);
           FillChar(TitleBuf, MaxTitleLen+1, 0);
           StrPLCopy(TitleBuf, FStreamInfo.Title, MaxTitleLen);
           p2 := pointer(p1);
           move(TitleBuf, p2^, MaxTitleLen+1);
           p2 := ShareMemPointer;
           inc(p2, 2600);   // skip 2600 byte
           FillChar(TitleBuf, MaxTitleLen+1, 0);
           StrPLCopy(TitleBuf, FStreamInfo.FileName, MaxTitleLen);
           move(TitleBuf, p2^, MaxTitleLen+1);

           VisDataThread.InformVisDriver(InformStreamInfo, 528);
         end;
     { stSyncWindows : begin
           inc(p1, 69);
           if FSyncVisWindow then
              p1^ := 1
           else
              p1^ := 0;
           VisDataThread.InformVisDriver(InformSyncWindows, 4);
         end; }
     { stEMBEDHandle : begin
           inc(p1, 69);
           p1^ := FEMBEDHandle;
           VisDataThread.InformVisDriver(InformEMBEDHandle, 4);
         end; }
     { stUseVisDrawer : begin
           inc(p1, 69);
           p1^ := ord(FUseVisDrawer);
           VisDataThread.InformVisDriver(InformUseVisDrawer, 4);
         end; }
   end;

end;

procedure TBASSPlayer.SetPlayerMode(Mode : TPlayerMode);
var
   p1 : PDWORD;
   ModeId : DWORD;
   OldMode : TPlayerMode;
begin
   OldMode := FPlayerMode;
   FPlayerMode := Mode;

   if Mode = plmStandby then
   begin
      FStreamName := '';
      FStreamInfo.FileName := '';
      FStreamInfo.Title := '';
      FStreamInfo.FileSize := 0;
      FStreamInfo.Duration := 0;
      FStreamInfo.Channels := 0;
      FStreamInfo.SampleRate := 0;
      FStreamInfo.BitRate := 0;
      SetStreamInfo2(FStreamInfo);
   end;

   VisDataThread.SetPlayerMode1(FPlayerMode);   // inform to vis plug-in driver
   SetPlayerMode2(FPlayerMode);   // inform to DSP plug-in driver

   if Mode <> plmPlaying then
   begin
      if not VisDataThread.Suspended then
         VisDataThread.Suspend;
     //  TimerFFT.Enabled := false;
   end else
   begin
      if Assigned(FOnNewFFTData) then
       //  if VisWindowHandle = 0 then
            TimerFFT.Enabled := true;
   end;

   if GoVisOut then
   begin
      p1 := ShareMemPointer;
      ModeId := ord(FPlayerMode);
      p1^ := ModeId;
      VisDataThread.InformVisDriver(InformPlayerMode, 4);
      if (Mode = plmPlaying) and VisDataThread.Suspended then
         VisDataThread.ResumeVis(RestartDelay);
   end;

   if Assigned(FOnModeChange) then
      FOnModeChange(Self, OldMode, FPlayerMode);
end;

function TBASSPlayer.MutedTemporary : boolean;
begin
   if not FMute then
   begin
     SetMuteState(true, 500);   // Mute
     repeat
        Sleep(20);
        WinProcessMessages;
     until (not AttribSliding);

     result := true;
   end else
     result := false;
end;

procedure TBASSPlayer.RestoreFromMutedState;
begin
   BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, FOutputVolume * 39);
   BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, FOutputVolume * 39);
   BASS_ChannelSetAttribute(PlayChannel, BASS_ATTRIB_VOL, 1);
   FMute := false;
end;

end.


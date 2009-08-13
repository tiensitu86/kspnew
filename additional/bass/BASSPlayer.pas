unit BASSPlayer;

interface

{$INCLUDE Delphi_Ver.inc}

uses
  Messages, SysUtils, Classes, Forms, Controls, StdCtrls, ExtCtrls,
  Dynamic_BASS, RT_BASSWMA, RT_basscd, RT_bassmidi, bass_aac, RT_bassmix, ioplug,
  MPEGAudio, OggVorbis, AACfile, WMAFile, WAVFile,
  MPEGInfoBox, OGGInfoBox, WMAInfoBox, Dialogs, FileSupportLst,
  TntCollection, UniCodeUtils, LMessages
  {$IFNDEF DELPHI_2007_BELOW}, AnsiStrings{$ENDIF}
  {, EmbededBrowser, TestMB};


const
   MaxPluginNum = 8;                  // the maximum number of plug-ins, simultaneously loadable
   Channellimit = 8;

   WM_WA_MPEG_EOF = WM_USER + 2;      // message from Winamp input plug-in, sent when stream reaches the end
   WM_StartPlay   = WM_USER + 101;    // message from output plug-in emulator at starting playback
   WM_BASS_StreamCreate = WM_USER + 105; // message from output plug-in emulator at executing BASS_StreamCreate
//   WM_BASS_StreamFree = WM_USER + 106;   // message from output plug-in emulator at executing BASS_StreamFree
   WM_GetToEnd    = WM_USER + 107;    // message to notify that BASS reaches the end of a stream
   WM_PluginFirst_Changed = WM_USER + 109; // message from PluginConfigForm(=Input plug-in Configuration Form)

   WM_GetMeta     = WM_USER + 110;    // message to notify that new Shoutcast Metadata are ready
   WM_DownLoaded  = WM_USER + 111;    // message to notify that downloading of an stream file from internet is done.
   WM_GetChannelInfo = WM_USER + 112; // message from Winamp input plug-in at getting stream file's properties (sampling rate, ..)
   WM_GetChannelData = WM_USER + 114; // message for repeated sample data extraction from BASS decoding channel.
   WM_RequestFromVis = WM_USER + 115; // message to notify that a request from Vis plug-in received
 //  WM_PlayListConfig = WM_USER + 116;  // defined in unit PlayListConfig.pas
   WM_ChannelUnavailable = WM_USER + 120;  // message from the PlayThread (Decode channel is not available)
   WM_GetLyric = WM_USER + 121;       // message to notify that a lyric event is encountered
   WM_GetHTTPHeaders = WM_USER + 122; // message to notify that BASS got the HTTP header
   WM_SlideEnded  = WM_USER + 123;    // message to notify that an attribute slide has ended   // * Added at Ver 1.44.4

   MaxVolume = 255;          // Winamp input plug-in volume range : 0 ~ 255


 // error types related loading & unloading of Winamp input plug-ins
   ERROR_OMOD_NOTREADY  = 1;   // output plug-in emulator is not ready
   ERROR_LOADED_BEFORE  = 2;   // already loaded before
   ERROR_LOADED_FULL    = 3;   // no space to load
   ERROR_CANNOT_LOAD    = 4;   // uncertain error at loading
   ERROR_INVALID_PLUGIN = 5;   // not a Winamp input plug-in
   ERROR_CANNOT_UNLOAD  = 6;   // uncertain error at unloading
   ERROR_NOT_LOADED     = 7;   // not loaded yet
   ERROR_IN_USE         = 8;   // specified plug-in is in use

   maxDSPmodNum = 8;

 // constants for the communication between threads related to driving vis plug-in
   DataReady = 10;
   QuitProg = 20;
 //  PlayerStatus = 30;
   MinimizeWindow = 40;
   ChangeEmbedWindow = 45;
   ChangeEMBEDSwitchMode = 46;
 //  CheckTitleBar = 47;     // * New at Ver 1.44
   RestoreWindow = 50;
   UnloadVisPlugin = 56;
   RequestRestFlag = 57;
   PlayListChange  = 58;    // * New at Ver 1.44

   InformPlayerMode = 81;
   InformStreamInfo = 82;
 //  InformSyncWindows = 83;

   CLASSNAME_WINAMP : pChar = 'Winamp v1.x';
   CLASSNAME_VIS_DRAWER = 'Vis Drawer';

type
  TChannelType = (Channel_NotOpened, Channel_Stream, Channel_CD, Channel_WMA,
                   Channel_Music, Channel_MIDI);

  TChannelInfo = record
      BitsPerSample : word;     // Bits per sample
      BitRate   : LongInt;
      SampleRate : LongInt;
      Channels : Word;
   end;

  PChannelInfo = ^TChannelInfo;

  TPlayerMode = (plmStandby, plmReady, plmStopped, plmPlaying, plmPaused);

   TSupportedBy = (Both, BASSNative, None);
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

const
   MaxChannels = ChannelLimit;
   MAXCDDRIVES = 4;
   MaxDX8Effect = 32;    // DX8 Effect range : 0 ~ 32
   MaxTitleLen = 255;
   NumFFTBands = 25;     // Number of bands for spectrum visualization
   NumEQBands  = 10;     // Number of bands for equalizer
   EQBandWidth  : array[0..NumEQBands-1] of float =
                  (4, 4, 4, 4, 5, 6, 5, 4, 3, 3);
   EQFreq : array[0..NumEQBands-1] of float =
                  (80, 170, 310, 600, 1000, 3000, 6000, 10000, 12000, 14000);
//   MaxLoadableAddons = 255;       // maximum number of add-on simultaneously loadable

type
  TSoundEffects = set of (Flanger, Equalizer, Echo, Reverb);
  TFFTData = array [0..512] of Single;
  TBandOut = array[0..NumFFTBands-1] of word;
  TEQGains = array [0..NumEQBands-1] of float;  // -15.0 ~ +15.0
  TBandData = record
     CenterFreq : float;   // 80 ~ 16,000 (cannot exceed one-third of the sampling frequency)
     BandWidth  : float;   // Bandwidth, in semitones, in the range from 1 to 36
  end;
  TEQBands = record
     Bands : word;     // Number of equalizer bands (0 ~ 10)
     BandPara : array[0..NumEQBands-1] of TBandData;
  end;

  TMetaSyncParam = record
     MsgHandle : HWND;
     ChannelType : TChannelType;  // * Added at Ver 2.00
     TitleP : array[0..255] of ansichar;
  end;
  TLyricSyncParam = record
     MsgHandle : HWND;
     Channel : DWORD;
  end;
  TSlideSyncParam = record
     MsgHandle : HWND;
     NextAction : DWORD;
  end;
  TBASSAddOnInfo = record
     Handle : HPLUGIN;
     Name   : string;
     Version : DWORD;
     NumFormat  : DWORD;
     FormatP : PBASS_PLUGINFORMS;
  end;
//  TBASSAddOnList = TList;//array[1..MaxLoadableAddons] of TBASSAddOnInfo;


  TMIDI_FONTINFO = record
     FontName : string;
     Copyright : string;
     Comment : string;
     Presets : DWORD;
     SampleSize : DWORD;
     SampleLoaded : DWORD;
  end;

  TMIDITrackInfo = record
     TrackCount : integer;
     TrackText : array[0..255] of string;
  end;

  TWinampGPPInfo = TGPPInfo;
 { TSubClass_Proc = function(lng_hWnd: HWND; uMsg: Integer;
                            var Msg: TMessage; var bHandled: Boolean) : boolean; }

  TNotifyEvent = procedure(Sender: TObject) of object;
  TNotifyEvent2 = procedure(Sender: TObject; GenParam : DWORD) of object;
  TNotifyNetEvent = procedure(Sender: TObject; Content : ansistring) of object;
  TNotifyMIDIEvent = procedure(Sender: TObject; TextP : pAnsiChar) of object;
  TNotifyModeChangeEvent = procedure(Sender: TObject; OldMode, NewMode : TPlayerMode) of object;

  TBASSPlayer = class(TComponent)
  private
    { Private declarations }
    AppHWND     : THandle;
    ParentHWND  : HWND;
  //  TimerTitleBar : TTimer;  // * Added at Ver 2.00

    ChannelType   : TChannelType;
    DecodeChannel : DWORD;
    PlayChannel   : DWORD;
    NeedFlush     : boolean;

    BassChannelInfo : BASS_CHANNELINFO;
    MessageHandle : hwnd;

    NowStarting : boolean;
 // Some input plug-ins do not need output plug-in. i.e., some input plug-ins
 // sound out decoded data directly.
 // UsesOutputPlugin is used to know if input plug-in needs output plug-in.
 // Use this instead of input plug-in's UsesOutPlug property because some
 // input plug-ins report wrong UsesOutPlug value.

    BASSDLLLoaded : boolean;
    BassInfoParam : BASS_INFO;
    EQParam       : BASS_DX8_PARAMEQ;
    EchoParam     : BASS_DX8_ECHO;
    ReverbParam   : BASS_DX8_REVERB;
    FlangerParam  : BASS_DX8_FLANGER;
    FSoundEffects : TSoundEffects;

    HBASSWMA      : HPLUGIN;   // * Added at Ver 2.00

    HSlideSync    : HSYNC;     // * Added at Ver 2.01
    AttribSliding : boolean;   // * Added at Ver 2.01

 //   DSPHandle     : HDSP;
 //   fladspHandle  : HDSP;
    EQHandle      : array [0..NumEQBands-1] of HFX;
    EchoHandle    : HFX;
    ReverbHandle  : HFX;
    FlangerHandle : HFX;

    BandOut: TBandOut;

    NetStream  : boolean;
    NetRadio   : boolean;
    ICYTag     : pAnsiChar;
    HTTPTag    : pAnsiChar;
    tmpHTTPTag : pAnsiChar;
    MetaSyncParam : TMetaSyncParam;
    LyricSyncParam : TLyricSyncParam;
    SlideSyncParam : TSlideSyncParam;   // * Added at Ver 2.01

    MPEG        : TMPEGaudio;
    Vorbis      : TOggVorbis;
    AAC         : TAACfile;
    WMA         : TWMAfile;
    WAV         : TWAVFile;

  //  MusicStartTime : DWORD;
  //  MusicPauseTime : DWORD;
  //  RestartPos : DWORD;

    //FMBHandle  : HWND;  // Handle to Mini Browser window

  // variables for vis plug-in
    DataReadyMsg : hwnd;

    MPEGFileInfoForm: TMPEGFileInfoForm;
    OggVorbisInfoForm : TOggVorbisInfoForm;
    WMAInfoForm : TWMAInfoForm;

    FBASSReady      : boolean;
    FBASSWMAReady   : boolean;
    FBASSAACReady   : boolean;   // * Added at Ver 2.00
    FBASSCDReady    : boolean;
    FBASSMIDIReady  : boolean;
    FMIDISoundReady : boolean;
    FPan: float;

    FMixerReady     : boolean;   // * Added at Ver 2.00
    FMixerPlugged   : boolean;   // * Added at Ver 2.00
    FDownMixToStereo: boolean;   // * Added at Ver 2.00

    FNumCDDrives    : integer;
    CDDriveList     : array[0..MAXCDDRIVES-1] of string;

    FVersionStr     : string;
    FDLLVersionStr  : string;
    FStreamName     : string;
    FDecodingByPlugin : boolean;
  //  FUsing_BASS_AAC : boolean;
    FDecoderName    : string;
    FDownLoaded     : boolean;
    FGetHTTPHeader  : boolean;
    FStreamInfo     : TStreamInfo;
    FSupportedBy    : TSupportedBy;    // * Added at Ver 2.00
//    BASSAddOnList   : TBASSAddOnList;

    FOrgWndProc     : pointer;

    defaultFontHandle : HSOUNDFONT;
    defaultMIDI_FONTINFO : TMIDI_FONTINFO;

    FOutputVolume   : DWORD;
    FMute           : boolean;
    FDX8EffectReady : boolean;
    FEchoLevel      : word;
    FReverbLevel    : word;
    FFlangerLevel   : word;
    FEQGains        : TEQGains;
    FEQBands        : TEQBands;
    FPlayerMode     : TPlayerMode;

    FOnPlayEnd         : TNotifyEvent;
    FOnGetMeta         : TNotifyNetEvent;
    FOnDownloaded      : TNotifyEvent2;
    FOnModeChange      : TNotifyModeChangeEvent;
    FOnGetLyric        : TNotifyMIDIEvent;
    FOnPluginRequest   : TNotifyEvent2;
    FOnUpdatePlayList  : TNotifyEvent2;
    FOnGenWindowShow   : TNotifyEvent2;

    procedure ClearEffectHandle;
    function  GetStreamInfo2(StreamName : string;
                            var StreamInfo : TStreamInfo;
                            var SupportedBy : TSupportedBy;
                            var PluginNum : integer;
                            var PluginName : string) : boolean;
    procedure SetVolume(Value : DWORD);
    procedure SetEchoLevel(Value : word);
    procedure SetReverbLevel(Value : word);
    procedure SetFlangerLevel(Value : word);
    procedure SetSoundEffect(Value : TSoundEffects);
    procedure SetEQGains(Value : TEQGains);
    procedure SetEQBands(Value : TEQBands);
  //  procedure SetSyncVisWindow(Value : boolean);
    procedure SetPlayerMode(Mode : TPlayerMode);
    function  GetDLLVersionStr : string;
    function  GetNativeFileExts : string;
    function  IsSeekable : boolean;
    function  GetDownloadProgress : DWORD;
    procedure PausePlay;
    procedure ResumePlay;
    procedure Restart;
 //   procedure TimerTitleBarTimer(Sender: TObject);  // * Added at Ver 2.00
    function  CurrentPosition : DWORD;
    procedure SetPosition(Value : DWORD);
    procedure Error(msg: string);
    function  GetBASSAddonExts : string; overload;
    function  GetDecoderName(ExtCode : string) : string;
    function  OpenURL(URL : string;
                             var StreamInfo : TStreamInfo;
                          //   var tmpChannel : DWORD;
                          //   var tmpChannelType : TChannelType;
                             var SupportedBy : TSupportedBy;
                             Temp_Paused : boolean;
                             CheckOnly : boolean) : boolean;
    procedure SetDownMixToStereo(Value : boolean);  // * Added at Ver 2.00
    function  MutedTemporary : boolean;             // * Added at Ver 2.01
    procedure RestoreFromMutedState;                // * Added at Ver 2.01
  //  function  GetGenWinHandles : TGenHandles;        // * Added at Ver 2.00.1
    function  GetBuildDate : string;
    procedure WinProcessMessages;   // * Added at Ver 2.00
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; NoInit: boolean = false);
      // Allocates memory and constructs an instance of TBASSPlayer.
      // This Create method should be executed first to use TBASSPlayer.
      // Parameter :
      //  - AOwner : an owner component, normally the main form of application

    destructor Destroy; override;
     // Disposes the TBASSPlayer component and its owned components.
     // Do not call Destroy directly. Call Free instead.  Free verifies that the component is not already
     // freed, and only then calls Destroy.

    function  GetBASSAddonExts(i: integer) : string; overload;

    procedure ProcMessage(var Msg: TLMessage);
     // Gets the handle of vis window.
     // Gets 0 if vis plug-in is not running.

    function GetStreamInfo(StreamName: string; var StreamInfo: TStreamInfo; var SupportedBy: TSupportedBy)
                           : boolean;
     // Gets the information of a stream file.
     // Parameters :
     //  - StreamName : the file path of a stream file
     //  - StreamInfo : TStreamInfo record where the information of a stream file is given
     //  - SupportedBy : Gets one of following values
     //      Both : The stream file can be decoded by BASS or Winamp input plug-in.
     //             If property "PluginFirst" is true then the stream file will be decoded by Winamp
     //             input plug-in.
     //      BASSNative : The stream file can be decoded only by BASS.
     //      WinampPlugin : The stream file can be decoded only by Winamp input plug-in.
     //      None : The stream file can not be decoded. (may be an invalid stream file or an unsupported
     //             stream file)
     // Return value : True if valid stream file, else False.
     // note) This function is not available for URL streams.
     //       There may be unrecorded features in the stream file, some items of StreamInfo
     //       may be filled with default value.
     //       The default value is 0 for numeric item, null for string item.

    function Open(StreamName : string) : boolean;
      //  Prepares to play a local stream file, a MIDI file, a music file, a CD Audio file or an URL
      //   stream.
      //  Local MPx(MP1, MP2, MP3, MP4), AAC, OGG, WAV, WMA, ASF, MIDI(MID, RMI, KAR), AIFF, MO3, IT,
      //   XM, S3M, MTM, MOD, UMX and CD Audio files are decoded by BASS library if property "PluginFirst"
      //   is not true or appropriate Winamp input plug-in(s) is not loaded.
      //  You should load the appropriate BASS add-on(s) or Winamp input plug-in(s) prior to opening a
      //   local stream file or a local music file other than above file formats.
      //  TBASSPlayer can play MPx, AAC, OGG, WAV, MIDI, WMA and ASF files from internet and the
      //   stream from net radio such as Shoutcast/Icecast server.
      //  Parameter :
      //   - StreamName : file path, URL of a stream file from internet, URL of a net radio.
      //  Return value : True on success, False on failure.
      //  note) TBASSPlayer is programmed to load several BASS extension modules to support various kind of
      //        sound files and net streams metioned above, as follows,
      //         - BASSWMA.DLL : WMA, ASF files and the stream from WMA broadcaster
      //         - BASS_AAC.DLL : MP4, AAC files and AAC+ Shoutcast streams
      //         - BASSCD.DLL : CD Audio files
      //         - BASSMIDI.DLL : MIDI files
      //        So, you must load appropriate Winamp input plug-in if the required BASS extension module is
      //        not loaded.

    function PlayLength : DWORD;
      //  Gets the playback length of opened file in mili seconds.
      //  Gets 0 if there is no opened stream.

    function GetChannelFFTData(PFFTData : pointer; FFTFlag : DWORD) : boolean;
      //  Gets FFT data.
      //  TBASSPlayer provides you the processed FFT data (intensity per frequency band data) to help you
      //  to drive visual display.
      //  Use this function only if you need raw FFT data.
      //  Parameters :
      //   - PFFTData : Buffer pointer where FFT data are given.
      //   - FFTFlag : Number of samples to get FFT data. One of followings,
      //         BASS_DATA_FFT512 : 512 samples (returns 256 floating-point values)
      //         BASS_DATA_FFT1024 : 1024 samples (returns 512 floating-point values)
      //         BASS_DATA_FFT2048 : 2048 samples (returns 1024 floating-point values)
      //       in addition, you can add BASS_DATA_FFT_INDIVIDUAL for multi-channel streams.
      //  Return value : True on success, False on failure.

    procedure Play;
      //  Plays an opened stream.
      //  If TBASSPlayer is in paused state then resumes playing.
      //  If TBASSPlayer is in stopped state then restarts from the beginning.
      //  If there is no opened stream or TBASSPlayer is in playing state then
      //  it takes no effects.

    procedure Stop;
      //  Stops playing.
      //  If TBASSPlayer is in paused state(Mode=plmPaused) then TBASSPlayer switches its
      //  state to Stopped state(Mode=plmStopped).
      //  Else if TBASSPlayer is not playing a stream then it takes no effects.

    procedure Pause(pAction : boolean);
      //  Pauses or resumes playing.
      //  Parameters :
      //   - pAction : Set true if you want to pause palying, false to resume.

    procedure Close;
      //  Closes an opened stream.

    procedure SetMuteState(MuteState : boolean; FadeInOutMS : DWORD);  // ** New at Ver 2.01
      //  Mutes or restores from mute
      //  Parameters :
      //   - MuteState : Set true if you want to mute, false to restore from mute
      //   - FadeInOutMS : Fade-in, Fade-out period in mili seconds (min  : 0, max : 3000)
      //     note) If the value of FadeInOutMS is less than 50 then it is assumed 0.
      //            (= no fade-in, fade-out period).
      //           If the value of FadeInOutMS is greater than 3000 then it is assumed 3000.

    property Mute : boolean read FMute;
      //  Indicates mute state, true if sound output is mute, else false.

    procedure SetPan(Pan: float);

    function BASSAddonLoad(FilePath : string) : TFileDesc;
      //  Plugs a BASS add-on into the standard stream and sample creation functions.
      //  Support for additional file formats are available via BASS add-ons, which can be downloaded
      //  from the BASS website: http://www.un4seen.com/
      //  Parameters :
      //   - FilePath : The file path of the BASS add-on to be loaded.
      //  Return value : The elements of TBASSAddOnInfo is filled with the information on the BASS add-on
      //                 loaded on success, the Handle element of TBASSAddOnInfo is 0 on failure.

    function BASSAddonFree(AddonHandle : HPLUGIN) : integer;
      //  Unplugs a BASS add-on or all BASS add-ons.
      //  Parameters :
      //   - AddonHandle : The handle to the BASS add-on to be unloaded or 0 for all BASS add-ons.
      //  Return value : The number of BASS add-ons released.

//    function GetBASSAddonList : TBASSAddonList;
      //  Gets the BASSAddonList which holds the information on BASS add-ons plugged.
      //  Return value : BASSAddonList.

    function MIDIFontInit(FontFilePath : string;
                            var MIDI_FONTINFO : TMIDI_FONTINFO) : boolean;
      //  Initializes a MIDI soundfont which is used as default soundfont.
      //  If there is a previously initialized soundfont, it is freed, i.e, TBASSPlayer does
      //  not permit multiple soundfonts.
      //  Parameters :
      //   - FontFilePath : the file path of a SF2 soundfont.
      //  Return value : True on success, False on failure.
      //                 MIDI_FONTINFO holds information on the initialized soundfont
      //  note) You should initializes a SF2 sound font to play MIDI files using BASS audio
      //        library "bassmidi.dl".

    function MIDIGetTrackInfo(MIDIFilePath : string;
                              var MIDITrackInfo : TMIDITrackInfo) : boolean;
      //  Retreives track information on a given MIDI file.
      //  Parameters :
      //   - MIDIFilePath : the file path of a MIDI file.
      //  Return value : True on success, False on failure.
      //                 MIDITrackInfo holds track information.

   { function MIDIFontGetInfo : TMIDI_FONTINFO;
      //  Retrieves information on the initialized soundfont.
      //  Return value : MIDI_FONTINFO
      //  note) if there is no initialized soundfont then the value of SampleSize and SampleLoaded
      //         of MIDI_FONTINFO is 0. }

    function SetAEQGain(BandNum : integer; EQGain : float) : boolean;
      //  Sets the gain of a equalizer's band.
      //  Parameters :
      //   - BandNum : Band number. ( 0 ~ FEQBands.Bands - 1 )
      //   - EQGain : Gain of the designated equalizer's band. ( -15.0 ~ +15.0 )
      //  Return value : True on success, False on failure.

    property EQGains : TEQGains read FEQGains write SetEQGains;
      //  Specifies equalizer's gain of all bands.
      //  note) The equalizer is not effective if the property "DX8EffectReady" is not true(= the system
      //        does not support DirectX version 8 or higher).

    property EQBands : TEQBands read FEQBands write SetEQBands;
      //  Specifies band parameters of all bands.

    property PlayerReady : boolean read FBASSReady;
      //  Indicates whether BASS.DLL is operational.  It is true if BASS.DLL has been loaded and
      //  initialized successfully.
      //  If FBASSReady is false then none of functions or procedures to play a stream are available.

    property BASSWMAReady : boolean read FBASSWMAReady;
      //  Indicates whether BASSWMA.DLL is operational.  It is true if FBASSReady is true and BASSWMA.DLL
      //  has been loaded.
      //  If FBASSReady is true and FBASSWMAReady is false then you should load a Winamp input plug-in for
      //  WMA file(ex: in_WM.dll) to play WMA files.

    property BASSAACReady : boolean read FBASSAACReady;  // * New at Ver 2.00
      //  Indicates whether bass_aac.dll is operational.  It is true if FBASSReady is true and bass_aac.dll
      //  has been loaded.
      //  If FBASSReady is true and FBASSAACReady is false then you should load a Winamp input plug-in for
      //  AAC, M4A, MP4 files and AAC+ stream from net.

    property BASSCDReady : boolean read FBASSCDReady;
      //  Indicates whether BASSCD.DLL is operational.  It is true if FBASSReady is true and BASSCD.DLL
      //  has been loaded.
      //  If FBASSReady is true and FBASSCDReady is false then you should load a Winamp input plug-in for
      //  CD Audio file(ex: in_cdda.dll) to play CD Audio tracks.

    property BASSMIDIReady : boolean read FBASSMIDIReady;
      //  Indicates whether BASSMIDI.DLL is operational.  It is true if FBASSReady is true and BASSMIDI.DLL
      //  has been loaded.
      //  If FBASSReady is true and FBASSMIDIReady is false then you should load a Winamp input plug-in for
      //  MIDI file(ex: in_midi.dll) to play MIDI files.

    property MIDISoundReady : boolean read FMIDISoundReady;
      //  Indicates whether the functions to play MIDI files is operational or not.
      //  FMIDISoundReady is true when FBASSMIDIReady is true and a MIDI sound font is loaded.

  {  property NumCDDrives : integer read FNumCDDrives;
      // Indicates the number of CDROM drives installed.
      // note) NumCDDrives is available only if BASSCDReady is true. (Always will be zero if not)

    function CDDescription(DriveNum : integer) : string;
      // Gets "drive_letter: description" string for specified CDROM drive.
      //  (0 : 1st drive, 1 : 2nd drive ... )  }

    property DecodingByPlugin : boolean read FDecodingByPlugin;
      //  Indicates whether the opened stream is decoded by Winamp input plug-in.

    property DecoderName : string read FDecoderName;
      //  Indicates the name of decoder responsible for the currently opened stream such as
      //   "BASS_Native" : The Currently opened stream is decoded by BASS sound library.
      //   "in_(Any name).dll" : The Currently opened stream is decoded by a Winamp input
      //                           plug-in. (ex. in_WM.dll)
      //   "bass*.dll          : The Currently opened stream is decoded by a BASS audio library
      //   "bass_(Any name).dll" : The Currently opened stream is decoded by a BASS add-on.
      //                           (ex. bass_spx.dll)

    property Seekable : boolean read IsSeekable;
      //  Indicates whether the playback position is changeable.

    property StreamInfo : TStreamInfo read FStreamInfo;
      //  Gets features of an opened stream.
      //  note) Because there may be unrecorded features in an opened stream file, some items of StreamInfo
      //        may be filled with default value.
      //        The default value is 0 for numeric item, null for string item.

    property Mode : TPlayerMode read FPlayerMode;
      //  Indicates the state of TBASSPlayer, It can be one of followings.
      //   plmStandby : TBASSPlayer is in standby state. (= Not opened a stream to play yet)
      //   plmReady   : TBASSPlayer is ready to play a stream. (= Opened a stream to play)
      //   plmStopped : TBASSPlayer is in stopped state.
      //                Subsequent playback is started from the beginning.
      //   plmPlaying : TBASSPlayer is playing a stream.
      //   plmPaused  : TBASSPlayer is in paused state.
      //                Subsequent playback is started from the position paused.

    property Position : DWORD read CurrentPosition write SetPosition;
      //  Specifies the playback position in mili seconds.
      //  Altering Position, which causes to perform SetPosition, takes no effects if the opened stream
      //  is not seekable stream.

    property StreamPath : string read FStreamName;
      //  Indicates the file path of an opened stream

    property ChannelId : DWORD read DecodeChannel;
      //  Indicates the handle which is given at creating a sample stream by BASS.
      //  You can implement most of BASS functions in your own program if you know the handle of channel.

    property Volume : DWORD read FOutputVolume write SetVolume;
      //  Specifies the output volume.
      //  The output volume range is from 0 to 255.

    property SoundEffects : TSoundEffects read FSoundEffects write SetSoundEffect;
      //  Specifies sound effects to be applied.
      //  The 4 different sound effects can be applied according to the value of SoundEffects.
      //  ex) BASSPlayer1.SoundEffects := [Echo] + [Reverb];  {apply echo and reverb}

    property EchoLevel : word read FEchoLevel write SetEchoLevel;
      //  Specifies the echo level.
      //  The range of echo level is from 0 to 32.

    property ReverbLevel : word read FReverbLevel write SetReverbLevel;
      //  Specifies the reverb level.
      //  The range of reverb level is from 0 to 32.

    property FlangerLevel : word read FFlangerLevel write SetFlangerLevel;  // ** New at Ver 2.01
      //  Specifies the Flanger level.
      //  The range of flanger level is from 0 to 32.

    property NativeFileExts : string read GetNativeFileExts;
      //  Reports playable file types which can be decoded by BASS or BASS extension moules,
      //  i.e., without Winamp input plug-ins and BASS add-ons.
      //  This property reflects the currently loaded BASS extension moules such as basscd.dll,
      //  basswma.dll, bassmidi.dll.

    property BASSAddonExts : string read GetBASSAddonExts;
      //  Reports the file types which can be decoded by BASS add-on plugins.
      //  This BASSAddonExts changes according to the currently loaded BASS add-on plug-ins.

    function IsValidURL(URL : string) : boolean;   // * New at Ver 2.01
      // Checks if the URL is a playable stream source.
      // Return value : True on success, False on failure.

    property IsNetStream : boolean read NetStream;
      //  Indicates whether the opened stream is a stream file from internet.

    property IsNetRadio : boolean read NetRadio;
      //  Indicates whether the opened stream comes from net radio station such as Shoutcast/
      //   Icecast server.

    property ICYInfo : pAnsiChar read ICYTag;
      //  Reports ICY (Shoutcast) tags. A pointer to a series of null-terminated strings is returned,
      //  the final string ending with a double null.
      //  ICYInfo will be nil if the opened stream has no ICY (Shoutcast) tags.

    property HTTPInfo : pAnsiChar read HTTPTag;
      //  Reports HTTP headers, only available when streaming from a HTTP server.
      //  A pointer to a series of null-terminated strings is returned, the final string ending with a
      //  double null.
      //  HTTPInfo will be nil if the opened stream is not from a HTTP server.

    property DownloadProgress : DWORD read GetDownloadProgress;
      //  Reports download progress of the opened stream file from internet, in bytes.

    function FileInfoBox(StreamName : string): Boolean;
      //  Shows dialog box which presents some information on a specified stream file.
      //  The MPEG, OGG Vorbis and WMA files are supported by internal code.
      //  You can also edit TAG of these files.
      //  For the stream files other than MPEG, OGG Vorbis and WMA files, this function is effective
      //  only if the specified stream file is supported by one of currently loaded Winamp input plug-ins.
      //  Return value : True on success, False on failure.
      //  note) This function is valid only for local stream files.

    function GetMessageHandle : HWND;   // debug purpose (to debug VisOut program)
      //  Gets the message handle of TBASSPlayer.


  // Followings are for Play List handling.

    procedure SetPlayEnd;
    procedure SetPlayEndA(Data: PtrInt);

    property MixerReady : boolean read FMixerReady;        // * New at Ver 2.00
      //  Indicates whether BASSmix is ready to operation.

    property DownMixToStereo : boolean read FDownMixToStereo write SetDownMixToStereo;  // * New at Ver 2.00
      //  Determines whether the multi channel source data are to be downmixed to stereo.
      //  This property is applied from new stream to be opened.
      //  This property is valid only if the opened stream is decoded by BASS library(= not by
      //   Winamp input plug-in), BASSmix is ready to operational(MixerReady = true) and
      //   BASS is operating in Dual channel mode(SingleChannel = false).

    property BuildDateStr : string read GetBuildDate;          // * New at Ver 2.01
      //  Indicates the Build date of BASS.DLL in string.

  published
    { Published declarations }
    property Version : string read FVersionStr;
      //  Indicates the version of TBASSPlayer in string.
      //  note) Altering Version, which causes to perform SetVersionStr, takes no effects.
      //        SetVersionStr is needed only to show FVersionStr at form design.

    property BASSDLLVer : string read FDLLVersionStr;
      //  Indicates the version of BASS.DLL in string.
      //  note) Altering BASSDLLVer, which causes to perform SetDLLVersionStr, takes no effects.
      //        SetDLLVersionStr is needed only to show FDLLVersionStr at form design.

    property DX8EffectReady : boolean read FDX8EffectReady;
      //  Indicates whether TBASSPlayer is ready to use sound effects which are supported
      //  by Direct-X.
      //  It is set True if the DirectX version 8 or higher is properly installed in your system.
      //  note) Altering DX8EffectReady, which causes to perform SetDX8EffectReady, takes no effects.
      //        SetDX8EffectReady is needed only to show FDX8EffectReady at form design.

    property OnGetMeta : TNotifyNetEvent read FOnGetMeta write FOnGetMeta;
      //  Occurs when metadata is received in a Shoutcast stream.

    property OnDownloaded : TNotifyEvent2 read FOnDownloaded write FOnDownloaded;
      //  Occurs when downloading of a stream file from internet is done.
      //  note) The downloading process is automatically started when you play an URL stream.
      //        This OnDownloaded event does not occur if the stream file from internet is played
      //        in buffered mode(= download and play in smaller chunks).

    property OnPlayEnd : TNotifyEvent read FOnPlayEnd write FOnPlayEnd;
      //  Occurs when stream reaches the end.

    property OnModeChange : TNotifyModeChangeEvent read FOnModeChange write FOnModeChange;
      //  Occurs when player's state is changed.

    property OnGetLyric : TNotifyMIDIEvent read FOnGetLyric write FOnGetLyric;
      //  Occurs when a lyric event is encountered.

    property OnPluginRequest : TNotifyEvent2 read FOnPluginRequest write FOnPluginRequest;
      //  Occurs at receiving a request from Winamp plug-in such as volume up/down, seek forward/backward.
      // ( e.g. user pressed a specified control key when vis window is the focused window and
      //   the vis plug-in in use transfers such activities to it's parent window. )
      //  The activities defined are as follows.
      //    Volume up(REQ_VOLUMEUP), Volume down(REQ_VOLUMEDOWN)
      //    Seek forward(REQ_FFWD5S), Seek backward(REQ_REW5S)
      //    Previous title(REQ_PREV), Next title(REQ_NEXT)
      //    Play(REQ_PLAY), Pause(REQ_PAUSE), Stop(REQ_STOP)

    property OnUpdatePlayList : TNotifyEvent2 read FOnUpdatePlayList write FOnUpdatePlayList; // * New at Ver 2.00
      // Occurs at receiving request for changing of play list from vis plug-in.

    property OnGenWindowShow : TNotifyEvent2 read FOnGenWindowShow write FOnGenWindowShow;  // * New at Ver 2.00.1
      //  Occurs at Showing up or Closing the window for Winamp GPP.

  end;


 // A Window API function "SetWaitableTimer" may not be executed normally if your
 // program is compiled with old Delphi versions (V4 and may be V5).
 // Because one of it's parameters is incorrectly declared in Delphi RTL.
 // ("lpDueTime" is declared as const instead of var)
 // So, I decided to use an alternate wrapper function to avoid this RTL bug.
  function MySetWaitableTimer(hTimer: THandle; var lpDueTime: TLargeInteger;
                              lPeriod: Longint; pfnCompletionRoutine: TFNTimerAPCRoutine;
                              lpArgToCompletionRoutine: Pointer; fResume: BOOL): BOOL;
                              stdcall; external 'kernel32.dll' name 'SetWaitableTimer';

  procedure Register;


implementation

uses Main, KSPConstsVars, MultiLog;


const
   MajorVer = '2';
   MinorVer = '10';
   RevVer = '0';
   BuildDate = '14 May 2009';

   NoDX8Msg : pchar = 'DirectX Ver 8 or higher is not installed.' + #13#10 +
                      'Sound effects except rotate are disabled.';
   StreamFileExt = '*.wav;*.mp1;*.mp2;*.mp3;*.ogg;*.aiff';
   MIDIFileExt = '*.MID;*.MIDI;*.RMI;*.KAR;';
   MusicFileExt = '*.MO3;*.IT;*.XM;*.S3M;*.MTM;*.MOD;*.UMX;';

var
   msgs_org : array of string;
   msgs_local : array of String;
   msgCount : integer = 0;
   typeStr : pchar;
   Entries_Msg_Str : integer;
   CheckedLocalString : boolean = false;

function GetProgDir : string;
begin
   result := ExtractFilePath(ParamStr(0));
end;

procedure ShowErrorMsgBox(ErrorStr : string);  // * Modified at Ver 2.00
var
   F: TextFile;
   SearchRec: TSearchRec;
   MyEntry : boolean;
   CommonEntry : boolean;
   S : string;
   tmpStr : string;
   localStr : string;
   i : integer;
   SepPos : integer;
   ErrorStr_ : string;

begin
  if not CheckedLocalString then
  begin
    CheckedLocalString := true;
    if FindFirst(ExtractFilePath(ParamStr(0)) + 'lang_*.txt', faAnyFile, SearchRec) = 0 then
    begin

      SetLength(msgs_org, 16);
      SetLength(msgs_local, 16);
      Entries_Msg_Str := 16;
      MyEntry := false;
      CommonEntry := false;

      AssignFile(F, ExtractFilePath(ParamStr(0)) + SearchRec.Name);
      Reset(F);
      FindClose(SearchRec);

      while not Eof(F) do
      begin
        Readln(F, S);
        S := trim(S);

        if S = '' then
           continue;
        if copy(S, 1, 2) = '//' then
           continue;

        if (S[1] = '[') and (S[length(S)] = ']') then
        begin
           tmpStr := copy(S, 2, length(S) - 2);
           if uppercase(tmpStr) = 'TBASSPLAYER' then
           begin
              MyEntry := true;
              CommonEntry := false;
              continue;
           end else if uppercase(tmpStr) = 'COMMON' then
           begin
              MyEntry := false;
              CommonEntry := true;
              continue;
           end else if MyEntry then
              break
           else begin
              MyEntry := false;
              CommonEntry := false;
              continue;
           end
        end
        else if (not MyEntry) and (not CommonEntry) then
           continue;

        if ((S[1] = '&') or (S[1] = '$') or (S[1] = '*') or (S[1] = '^')) then
           continue;

      // Store message strings (format : Original=Local)
        SepPos := pos('=', S);
        if SepPos = 0 then
           continue;

        localStr := trim(copy(S, SepPos + 1, length(S) - SepPos));
        if localStr = '' then
           continue;
        if localStr[1] = '"' then
           localStr := copy(localStr, 2, length(localStr) - 1);
        if localStr[length(localStr)] = '"' then
           localStr := copy(localStr, 1, length(localStr) - 1);
        if localStr = '' then
           continue;

        msgs_org[msgCount] := trim(copy(S, 1, SepPos - 1));  // original message string
        msgs_local[msgCount] := localStr;
        inc(msgCount);
        if msgCount = Entries_Msg_Str then
        begin
           Entries_Msg_Str := Entries_Msg_Str + 16;
           SetLength(msgs_org, Entries_Msg_Str);
           SetLength(msgs_local, Entries_Msg_Str);
        end;
      end;

      CloseFile(F);
      SetLength(msgs_org, msgCount);
      SetLength(msgs_local, msgCount);

    end else
      FindClose(SearchRec);

  // following sentences are to get the local text for the title of messagebox
    typeStr := 'Error';
    tmpStr := string(typeStr);
    if msgCount > 0 then
    begin
       for i := 1 to msgCount do
       begin
         if msgs_org[i - 1] = tmpStr then
         begin
            typeStr := pChar(msgs_local[i - 1]);
            break;
         end;
       end;
    end;
  end;

   localStr := ErrorStr;

   if msgCount > 0 then
   begin
   // I programmed to apply the localization for only the 1st line of message string.
   // (=> message string may include variable contents in it, ex : file name to open )
      SepPos := pos(chr(10), ErrorStr);
      if SepPos > 0 then
         ErrorStr_ := copy(ErrorStr, 1, SepPos - 1)
      else
         ErrorStr_ := ErrorStr;

      for i := 1 to msgCount do
      begin
         if ErrorStr_ = msgs_org[i - 1] then
         begin
            localStr := msgs_local[i - 1];
            if SepPos > 0 then
               localStr := localStr + chr(10) + copy(ErrorStr, SepPos + 1, length(ErrorStr) - Seppos);
            break;
         end;
      end;
   end;

   Application.MessageBox(pChar(localStr), typeStr, MB_OK + MB_ICONERROR);
end;

// -------------------------------- Event Handlers ------------------------------

// This procedure is called when metadata are received in a Shoutcast stream.
procedure MetaSync(SyncHandle : HSYNC; Channel, data : DWORD; user : pointer); stdcall;
var
   TagP : pAnsiChar;
   tmpStr, StreamTitle : ansistring;
   TitlePos, DelimeterPos : integer;
   PMetaSyncParam : ^TMetaSyncParam;
   tmpPChar : pAnsiChar;
   tmpStr2 : WideString;
begin
   PMetaSyncParam := user;
   StreamTitle := '';
 //  TagP := pAnsiChar(data);
   if PMetaSyncParam^.ChannelType = Channel_WMA then
      TagP := BASS_ChannelGetTags(channel, BASS_TAG_WMA_META) // get metadata
   else
      TagP := BASS_ChannelGetTags(channel, BASS_TAG_META); // get metadata
   if TagP <> nil then
   begin
      tmpStr := ansistring(TagP);
      if PMetaSyncParam^.ChannelType = Channel_WMA then
        {$IFDEF DELPHI_2007_BELOW}
         TitlePos := Pos('Title=', tmpStr)
        {$Else}
         TitlePos := PosEx('Title=', tmpStr, 1)
        {$ENDIF}
      else
        {$IFDEF DELPHI_2007_BELOW}
         TitlePos := Pos('StreamTitle=', tmpStr);
        {$Else}
         TitlePos := PosEx('StreamTitle=', tmpStr, 1);
        {$ENDIF}
   end else
   begin
      tmpStr := '';
      TitlePos := 0;
   end;

   if TitlePos <> 0 then
   begin
     {$IFDEF DELPHI_2007_BELOW}
      DelimeterPos := Pos(';', tmpStr);
     {$Else}
      DelimeterPos := PosEx(';', tmpStr);
     {$ENDIF}
      if PMetaSyncParam^.ChannelType = Channel_WMA then
      begin
         if DelimeterPos = 0 then
            StreamTitle := copy(tmpStr, TitlePos + 7, length(tmpStr) - TitlePos - 7)
         else
            StreamTitle := copy(tmpStr, TitlePos + 7, DelimeterPos - TitlePos - 8);
        {$IFDEF DELPHI_2007_BELOW}
         tmpStr2 := UTF8Decode(StreamTitle);
        {$ELSE}
         tmpStr2 := UTF8ToWideString(StreamTitle);
        {$ENDIF}
         tmpPChar := ToPMultiByte(PWideChar(tmpStr2));
         StreamTitle := ansistring(tmpPChar);
      end else
         if DelimeterPos = 0 then
            StreamTitle := copy(tmpStr, TitlePos + 13, length(tmpStr) - TitlePos - 13)
         else
            StreamTitle := copy(tmpStr, TitlePos + 13, DelimeterPos - TitlePos - 14);
   end else
   begin   // May be a chained OGG stream
      while StrLen(TagP) > 0 do
      begin
        {$IFNDEF DELPHI_2007_BELOW}
         if PosEX('TITLE=', Ansistrings.upperCase(ansistring(TagP)), 1) <> 0 then
        {$ELSE}
         if pos('TITLE=', upperCase(ansistring(TagP))) <> 0 then
        {$ENDIF}
         begin
            inc(TagP, 6);
            StreamTitle := ansistring(TagP);
            break;

            inc(TagP, StrLen(TagP) + 1);
         end;
      end;
   end;

 {  if StreamTitle = '' then
      StreamTitle := tmpStr;  }

   StrPLCopy(PMetaSyncParam^.TitleP, StreamTitle, MaxTitleLen);
   PostMessage(PMetaSyncParam^.MsgHandle, WM_GetMeta, 0, longint(@PMetaSyncParam^.TitleP));
end;


// This procedure is called when a stream reaches the end.
procedure PlayEndSync(SyncHandle : HSYNC; Channel, data : DWORD; user : pointer); stdcall;
var
   f: PtrInt;
begin
  Application.QueueAsyncCall(Player.SetPlayEndA, f);
end;

// This procedure is called when downloading of an URL stream is done.
procedure DownloadSync(SyncHandle : HSYNC; Channel, data: DWORD; user : pointer); stdcall;
var
   Msg_Handle : DWORD;
begin
   Msg_Handle := PDWORD(user)^;
   PostMessage(Msg_Handle, WM_DownLoaded, 0, 0);
end;

// This procedure is called when a lyric event is encountered.
procedure LyricSync(SyncHandle : HSYNC; Channel, data : DWORD; user : pointer); stdcall;
var
   PLyricSyncParam : ^TLyricSyncParam;
   MarkP : BASS_MIDI_MARK;
begin
   PLyricSyncParam := user;
   if BASS_MIDI_StreamGetMark(PLyricSyncParam^.Channel, BASS_MIDI_MARK_LYRIC, data, MarkP) then
      PostMessage(PLyricSyncParam^.MsgHandle, WM_GetLyric, longint(MarkP.text), 0);
end;

// This procedure is called when an attribute slide has ended
// * Added at Ver 2.01
procedure SlideEndSync(SyncHandle : HSYNC; Channel, data : DWORD; user : pointer); stdcall;
var
   PSlideSyncParam : ^TSlideSyncParam;
begin
   PSlideSyncParam := user;
   PostMessage(PSlideSyncParam^.MsgHandle, WM_SlideEnded, PSlideSyncParam^.NextAction, 0);
end;

// This procedure is called before the BASS_MIDI_StreamCreateURL call returns.
// This procedure is used to get HTTP or ICY tags from the server.
procedure DownProc(buffer : pchar; length : DWORD; user : pointer); stdcall;
var
   Msg_Handle : DWORD;
   bufferStr : string;
   p : pointer;
begin
   Msg_Handle := PDWORD(user)^;

   if length = 0 then
   begin
      bufferStr := string(buffer);
      if copy(bufferStr, 1, 4) = 'HTTP' then
      begin
         p := buffer + 4;
         PostMessage(Msg_Handle, WM_GetHTTPHeaders, DWORD(p), 0);
      end else
      if copy(bufferStr, 1, 3) = 'ICY' then
      begin
         p := buffer + 3;
         PostMessage(Msg_Handle, WM_GetHTTPHeaders, DWORD(p), 0);
      end
   end else
   if buffer = nil then
      PostMessage(Msg_Handle, WM_GetHTTPHeaders, 0, 0);  // no HTTP or ICY tags from the server
                                                         // but finished downloading process.
end;

// ----------------------------- end of Event Handlers ---------------------------

// ------------------------------ Class TBassPlayer ------------------------------

procedure TBASSPlayer.Error(msg: string);
var
  s: string;
begin
  s := msg + #13#10 + '(error code: ' + IntToStr(BASS_ErrorGetCode) + ')';
  ShowErrorMsgBox(s);
end;

procedure TBASSPlayer.WinProcessMessages;  // * Added at Ver 2.00
// Allow Windows to process other system messages
var
    ProcMsg  :  TMsg;
begin
    while PeekMessage(ProcMsg, 0, 0, 0, PM_REMOVE) do begin
      if (ProcMsg.Message = WM_QUIT) then Exit;
      TranslateMessage(ProcMsg);
      DispatchMessage(ProcMsg);
    end;  
end;

{ function TBASSPlayer.CDDescription(DriveNum : integer) : string;
begin
   if not FBASSCDReady then
      result := ''
   else if DriveNum >= FNumCDDrives then
      result := ''
   else
      result := CDDriveList[DriveNum];
end; }

procedure TBASSPlayer.ClearEffectHandle;
var
   i : integer;
begin
 //  fladspHandle := 0;
   if FDX8EffectReady then
   begin
      for i := 0 to (FEQBands.Bands - 1) do
          EQHandle[i] := 0;
      EchoHandle := 0;
      ReverbHandle := 0;
      FlangerHandle := 0;
   end;
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


function TBASSPlayer.CurrentPosition : DWORD;  // get playback position in mili seconds
var
   SongPos : int64;
   FloatPos : FLOAT;
begin
   case ChannelType of
      Channel_NotOpened : SongPos := 0;
      Channel_Stream..Channel_MIDI : begin
                         SongPos := BASS_ChannelGetPosition(DecodeChannel, BASS_POS_BYTE);
                       end;
      else
         SongPos := 0;
   end;

   if SongPos <= 0 then  // can be -1 if DecodeChannel is not available (ex. BASS add-on was freed.)
   begin
      result := 0;
      exit;
   end;

   FloatPos := BASS_ChannelBytes2Seconds(DecodeChannel, SongPos);
   result := round(1000 * FloatPos);     // sec -> milli sec
end;


procedure TBASSPlayer.SetPosition(Value : DWORD); // set playback position in mili seconds
var
   SongPos : int64;
begin
   if not IsSeekable then
      exit;

   if Value > PlayLength {= FStreamInfo.Duration} then
      exit;

   if NowStarting then
      exit;

   if not FMute then
   begin
     //SetMuteState(true, 500);   // Mute
     repeat
        Sleep(20);
        WinProcessMessages;
     until (not AttribSliding);
   end;

   case ChannelType of
      Channel_NotOpened : exit;
    {  Channel_Music   : begin   
                           if (BASS_ChannelSetPosition(DecodeChannel, MAKELONG((Value div 1000), $ffff))) then
                              if FSingleChannel2 then
                              begin
                                 MusicStartTime := timeGetTime - Value;
                                 exit;
                              end else
                              begin
                                 RestartPos := Value;
                                 if PlayerStat = BASS_ACTIVE_PLAYING then
                                 begin
                                    BASS_ChannelPlay(PlayChannel, true);
                                    if Assigned(FOnNewFFTData) then
                                      if VisWindowHandle = 0 then  // vis plug-in is not active ?
                                         TimerFFT.Enabled := true;
                                 end;
                              end;
                           exit;
                        end; }
      Channel_Stream..Channel_MIDI  : begin
                           SongPos := BASS_ChannelSeconds2Bytes(DecodeChannel, Value / 1000);
                           BASS_ChannelSetPosition(DecodeChannel, SongPos, BASS_POS_BYTE);
                        end;
   end;

   //if tmpMuted then
      //SetMuteState(false, 500);
end;

function TBASSPlayer.GetDownloadProgress : DWORD;  // get downloaded bytes
begin
   if ChannelType = Channel_NotOpened then
      result := 0
   else if (not NetStream) then
      result := 0
   else
      result := BASS_StreamGetFilePosition(DecodeChannel, BASS_FILEPOS_DOWNLOAD);
end;

function TBASSPlayer.IsSeekable : boolean;
begin
   if (ChannelType = Channel_NotOpened) {or (ChannelType = Channel_Music)} then
      result := false
   else if NetRadio then
      result := false
   else if NetStream then
   begin
      if (ChannelType = Channel_WMA) then   // * Added at Ver 2.00
         result := false
      else
         result := FDownLoaded
   end else
      result := true;
end;


procedure TBASSPlayer.SetVolume(Value : DWORD);
var
   tmpValue : DWORD;
begin
   if not FBASSReady then
      exit;
      
   if Value > MaxVolume then
      tmpValue := MaxVolume
   else
      tmpValue := Value;

   FOutputVolume := tmpValue;
   if not FMute then
   begin
      BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, FOutputVolume * 39);
      BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, FOutputVolume * 39);
   end;
end;

procedure TBASSPlayer.SetMuteState(MuteState : boolean; FadeInOutMS : DWORD);  // ** New at Ver 2.01
begin
   if AttribSliding then
      exit;

   if FadeInOutMS > 3000 then
      FadeInOutMS := 3000;

   if MuteState <> FMute then
   begin
      FMute := MuteState;
      if FMute then
      begin
         if (PlayChannel <> 0) and (FadeInOutMS >= 50) then
         begin
            AttribSliding := true;
            BASS_ChannelSlideAttribute(PlayChannel, BASS_ATTRIB_VOL, 0, 500);
            SlideSyncParam.NextAction := 0;  // 0 : Mute
            HSlideSync := BASS_ChannelSetSync(PlayChannel, BASS_SYNC_SLIDE, 0, @SlideEndSync, @SlideSyncParam);
         end else
         begin
            BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, 0);
            BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, 0);
            BASS_ChannelSetAttribute(PlayChannel, BASS_ATTRIB_VOL, 0);
         end;
      end else
      begin
         BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, FOutputVolume * 39);
         BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, FOutputVolume * 39);
         if (PlayChannel <> 0) and (FadeInOutMS >= 50)then
         begin
            AttribSliding := true;
            BASS_ChannelSetAttribute(PlayChannel, BASS_ATTRIB_VOL, 0);
            BASS_ChannelSlideAttribute(PlayChannel, BASS_ATTRIB_VOL, 1, 500);
            SlideSyncParam.NextAction := 1;  // 1 : Restore
            HSlideSync := BASS_ChannelSetSync(PlayChannel, BASS_SYNC_SLIDE, 0, @SlideEndSync, @SlideSyncParam);
         end else
            BASS_ChannelSetAttribute(PlayChannel, BASS_ATTRIB_VOL, 1);
      end;
   end;
end;

function TBASSPlayer.SetAEQGain(BandNum : integer; EQGain : float) : boolean;  
begin
   result := false;

   if not FDX8EffectReady then
      exit;
   if BandNum >= FEQBands.Bands then
      exit;

   if EQGain > 15.0 then
      FEQGains[BandNum] := 15.0
   else if EQGain < -15.0 then
      FEQGains[BandNum] := -15.0
   else
      FEQGains[BandNum] := EQGain;

   if (Equalizer in FSoundEffects) then
   begin
      if EQHandle[BandNum] = 0 then
         exit;

      if BASS_FXGetParameters(EQHandle[BandNum], @EQParam) then
      begin
      // Avoid redundant operation
         if abs(EQParam.fGain - FEQGains[BandNum]) > 0.001 then   // consider round-off error
         begin
            EQParam.fBandWidth := FEQBands.BandPara[BandNum].BandWidth;
            EQParam.fCenter := FEQBands.BandPara[BandNum].CenterFreq;
            EQParam.fGain:= FEQGains[BandNum];
            if BASS_FXSetParameters(EQHandle[BandNum], @EQParam) then
               result := true;
         end else
            result := true;
      end;
   end;
end;

procedure TBASSPlayer.SetEQGains(Value : TEQGains);
var
   i : integer;
begin
   if not FDX8EffectReady then
      exit;

   for i := 0 to (FEQBands.Bands - 1) do
   begin
      if Value[i] > 15.0 then
         FEQGains[i] := 15.0
      else if Value[i] < -15.0 then
         FEQGains[i] := -15.0
      else
         FEQGains[i] := Value[i];

      if not (Equalizer in FSoundEffects) then
         Continue;

      if EQHandle[i] = 0 then
         Continue;

      if BASS_FXGetParameters(EQHandle[i], @EQParam) then
      begin
      // Avoid redundant operation
         if abs(EQParam.fGain - FEQGains[i]) < 0.001 then   // consider round-off error
            Continue;

         EQParam.fBandWidth := FEQBands.BandPara[i].BandWidth;
         EQParam.fCenter := FEQBands.BandPara[i].CenterFreq;
         EQParam.fGain:= FEQGains[i];
         BASS_FXSetParameters(EQHandle[i], @EQParam);
      end;
   end;
end;

procedure TBASSPlayer.SetEchoLevel(Value : word);
begin
   if Value > MaxDX8Effect then
      exit;

   FEchoLevel := Value;
   if Echo in FSoundEffects then
   begin
       if EchoHandle <> 0 then
       begin
         if BASS_FXGetParameters(EchoHandle, @EchoParam) then
         begin
            EchoParam.fWetDryMix := FEchoLevel * 1.2{30.0};
            EchoParam.fFeedBack := 30.0;
            BASS_FXSetParameters(EchoHandle, @EchoParam)
         end;
       end;
   end;
end;

procedure TBASSPlayer.SetReverbLevel(Value : word);
begin
   if Value > MaxDX8Effect then
      exit;

   FReverbLevel := Value;
   if Reverb in FSoundEffects then
   begin
      if ReverbHandle <> 0 then
       begin
         if BASS_FXGetParameters(ReverbHandle, @ReverbParam) then
         begin
            ReverbParam.fInGain := 0.0;
            ReverbParam.fReverbMix := FReverbLevel * 0.5 - 16.0;
            ReverbParam.fReverbTime := 1000.0;
            ReverbParam.fHighFreqRTRatio := 0.1;
            BASS_FXSetParameters(ReverbHandle, @ReverbParam)
         end;
       end;
   end;
end;

procedure TBASSPlayer.SetFlangerLevel(Value : word);
begin
   if Value > MaxDX8Effect then
      exit;

   FFlangerLevel := Value;
   if Flanger in FSoundEffects then
   begin
      if FlangerHandle <> 0 then
       begin
         if BASS_FXGetParameters(FlangerHandle, @FlangerParam) then
         begin
            FlangerParam.fWetDryMix := FFlangerLevel * 1.5;
            FlangerParam.fDepth     := 75;
            FlangerParam.fFeedback  := -60;
            FlangerParam.fFrequency := 5;
            FlangerParam.lWaveform  := 0;
            FlangerParam.fDelay     := 3;
            FlangerParam.lPhase     := BASS_DX8_PHASE_NEG_180;
            BASS_FXSetParameters(FlangerHandle, @FlangerParam)
         end;
       end;
   end;
end;

procedure TBASSPlayer.SetEQBands(Value : TEQBands);
var
   i : integer;
begin
   if Value.Bands > NumEQBands then
      exit;

   FEQBands.Bands := Value.Bands;
   if FEQBands.Bands > NumEQBands then
      FEQBands.Bands := NumEQBands;

   for i := 0 to (FEQBands.Bands-1) do
   begin
      FEQBands.BandPara[i].CenterFreq := Value.BandPara[i].CenterFreq;
      FEQBands.BandPara[i].BandWidth := Value.BandPara[i].BandWidth;
   end;

   if Equalizer in FSoundEffects then
     for i := 0 to (FEQBands.Bands-1) do
     begin
        if EQHandle[i] = 0 then
           EQHandle[i] := BASS_ChannelSetFX(PlayChannel, BASS_FX_DX8_PARAMEQ, i+1);
        if EQHandle[i] <> 0 then
        begin
           EQParam.fGain := FEQGains[i];
           EQParam.fBandWidth := FEQBands.BandPara[i].BandWidth;
           EQParam.fCenter := FEQBands.BandPara[i].CenterFreq;
           BASS_FXSetParameters(EQHandle[i], @EQParam);
        end;
     end;

   for i := FEQBands.Bands to (NumEQBands-1) do
   begin
      if EQHandle[i] <> 0 then
         if BASS_ChannelRemoveFX(PlayChannel, EQHandle[i]) then
            EQHandle[i] := 0;
   end;
end;

procedure TBASSPlayer.SetSoundEffect(Value : TSoundEffects);
var
   i : integer;
begin
   FSoundEffects := Value;

   if ChannelType = Channel_NotOpened then
      exit;

  { if Flanger in Value then
   begin
      if fladspHandle = 0 then
      begin
         SetFlangeParams;
         fladspHandle := BASS_ChannelSetDSP(PlayChannel, @Flange, nil, 0);
      end;
   end else
      if fladspHandle <> 0 then
         if BASS_ChannelRemoveDSP(PlayChannel, fladspHandle) then
            fladspHandle := 0;  }

   if not FDX8EffectReady then
      exit;

   if Equalizer in Value then
   begin
      for i := 0 to (FEQBands.Bands-1) do
      begin
         if EQHandle[i] = 0 then
            EQHandle[i] := BASS_ChannelSetFX(PlayChannel, BASS_FX_DX8_PARAMEQ, i+1);

         if EQHandle[i] <> 0 then
         begin
            EQParam.fGain := FEQGains[i];
            EQParam.fBandWidth := FEQBands.BandPara[i].BandWidth;
            EQParam.fCenter := FEQBands.BandPara[i].CenterFreq;
            BASS_FXSetParameters(EQHandle[i], @EQParam);
         end;
      end;
   end else
      for i := 0 to (FEQBands.Bands - 1) do
      begin
         if EQHandle[i] <> 0 then
            if BASS_ChannelRemoveFX(PlayChannel, EQHandle[i]) then
               EQHandle[i] := 0;
      end;

   if Echo in Value then
   begin
       if EchoHandle = 0 then
          EchoHandle := BASS_ChannelSetFX(PlayChannel, BASS_FX_DX8_ECHO, NumEQBands+1);
       if EchoHandle <> 0 then
       begin
         if BASS_FXGetParameters(EchoHandle, @EchoParam) then
         begin
            EchoParam.fWetDryMix := FEchoLevel * 1.2{30.0};
            EchoParam.fFeedBack := 30.0;
            BASS_FXSetParameters(EchoHandle, @EchoParam)
         end;
       end;
   end else
      if EchoHandle <> 0 then
         if BASS_ChannelRemoveFX(PlayChannel, EchoHandle) then
            EchoHandle := 0;

   if Reverb in Value then
   begin
       if ReverbHandle = 0 then
          ReverbHandle := BASS_ChannelSetFX(PlayChannel, BASS_FX_DX8_REVERB, NumEQBands+2);
       if ReverbHandle <> 0 then
       begin
         if BASS_FXGetParameters(ReverbHandle, @ReverbParam) then
         begin
            ReverbParam.fInGain := 0.0;
            ReverbParam.fReverbMix := FReverbLevel * 1.5 - 16.0;
            ReverbParam.fReverbTime := 1000.0;
            ReverbParam.fHighFreqRTRatio := 0.1;
            BASS_FXSetParameters(ReverbHandle, @ReverbParam)
         end;
       end;
   end else
      if ReverbHandle <> 0 then
         if BASS_ChannelRemoveFX(PlayChannel, ReverbHandle) then
            ReverbHandle := 0;

   if Flanger in Value then
   begin
      if FlangerHandle = 0 then
          FlangerHandle := BASS_ChannelSetFX(PlayChannel, BASS_FX_DX8_FLANGER, NumEQBands+3);
       if FlangerHandle <> 0 then
       begin
         if BASS_FXGetParameters(FlangerHandle, @FlangerParam) then
         begin
            FlangerParam.fWetDryMix := FFlangerLevel * 1.5;
            FlangerParam.fDepth     := 75;
            FlangerParam.fFeedback  := -60;
            FlangerParam.fFrequency := 5;
            FlangerParam.lWaveform  := 0;
            FlangerParam.fDelay     := 3;
            FlangerParam.lPhase     := BASS_DX8_PHASE_NEG_180;
            BASS_FXSetParameters(FlangerHandle, @FlangerParam)
         end;
       end;
   end else
      if FlangerHandle <> 0 then
         if BASS_ChannelRemoveFX(PlayChannel, FlangerHandle) then
            FlangerHandle := 0;
end;


procedure TBASSPlayer.SetPlayerMode(Mode : TPlayerMode);
var
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
   end;

   if Assigned(FOnModeChange) then
      FOnModeChange(Self, OldMode, FPlayerMode);
end;

constructor TBASSPlayer.Create(AOwner: TComponent; NoInit: boolean = false);
var
   i : integer;
 //  BASSVersion : DWORD;
 //  d: PChar;
   t : string;
   n: Integer;
   CD_Info : BASS_CD_INFO;
   fn : string;

begin
   inherited Create(AOwner);

   FVersionStr := MajorVer + '.' + MinorVer + '.' + RevVer;
 {  FBASSReady := false;
   FBASSWMAReady := false;
   FBASSAACReady := false;
   FBASSCDReady := false;
   FBASSMIDIReady := false;
   FMIDISoundReady := false;
   FGPPluginReady := false;
   FMiniBrowserReady := false;
   FUseVisDrawer := false;
   FSingleChannel := false;
   NeedFlush   := false;

   HBASSWMA := 0;
   HBASSAAC := 0;
   FNumCDDrives := 0;
   defaultFontHandle := 0;
   DecodeChannel := 0;
   PlayChannel := 0;  }

   ChannelType := Channel_NotOpened;
   FSoundEffects := [];
   FPlayerMode := plmStandby;
   FPan:=0;

   if not (csDesigning in ComponentState) then
   begin
     if AOwner<>nil then
       ParentHWND := (AOwner as TWinControl).Handle else ParentHWND:=FindWindow('KSPMainWindow',nil);;
     AppHWND := ParentHWND;

     MessageHandle := 0;//AllocateHWnd(ProcMessage);
     //MessageHandle := MakeObjectInstance(ProcMessage);

     FOrgWndProc := Pointer(GetWindowLong(MessageHandle, GWL_WNDPROC));
     MetaSyncParam.MsgHandle := MessageHandle;
     LyricSyncParam.MsgHandle := MessageHandle;
     SlideSyncParam.MsgHandle := MessageHandle;
     DataReadyMsg := registerWindowMessage('WM_BASSSendingData');

   end;

   BASSDLLLoaded := Load_BASSDLL(GetProgDir + 'bass.dll');
   if not BASSDLLLoaded then
   begin
      if (csDesigning in ComponentState) then
         FDLLVersionStr := 'N.A.(Copy bass.dll in <directory Delphi installed>\Bin to get version)'
      else
         FDLLVersionStr := '';
      exit;
   end;

 //  BASSVERSION & BASSVERSIONTEXT are defined in Dynamic_Bass.pas
   if (HIWORD(BASS_GetVersion)<> BASSVERSION) or (LOWORD(BASS_GetVersion) < 1) then
   begin
     if not (csDesigning in ComponentState) then
        ShowErrorMsgBox('BASS version is not ' + BASSVERSIONTEXT + ' !');
     exit;
   end;

   if not NoInit then begin

 //  BASS_SetConfig(BASS_CONFIG_MAXVOL, MaxVolume);  // Set maximum volume range
   BASS_SetConfig(BASS_CONFIG_CD_FREEOLD, 1);   // Automatically free an existing stream
                                                // when creating a new one on the same drive
   BASS_SetConfig(BASS_CONFIG_NET_PLAYLIST, 1);   // * Added at Ver 2.00
   BASS_SetConfig(BASS_CONFIG_WMA_BASSFILE, 1);   // * Added at Ver 2.00
   BASS_SetConfig(BASS_CONFIG_WMA_PREBUF, 1);     // * Added at Ver 2.00

 // setup output - default device, 44100hz, stereo, 16 bits
   if not BASS_Init(1, 44100, 0, AppHWND, nil) then
   begin
      if not (csDesigning in ComponentState) then
         Error('Can''t initialize device');
      exit;
   end else
      FBASSReady := true;

 //  BassInfoParam.size := SizeOf(BassInfoParam);
   BASS_GetInfo(BassInfoParam);

 // BASS_ChannelSetFX requires DirectX version 8 or higher
   if BassInfoParam.dsver >= 8 then
   begin
      FDX8EffectReady := true;
   end else begin
      if not (csDesigning in ComponentState) then
         MessageBox(AppHWND, NoDX8Msg, 'Warning', MB_ICONWARNING or MB_OK);
      FDX8EffectReady := false;
   end;

   FEQBands.Bands := NumEQBands;
   for i := 0 to (NumEQBands-1) do
   begin
      FEQGains[i] := 0;
      FEQBands.BandPara[i].CenterFreq := EQFreq[i];
      FEQBands.BandPara[i].BandWidth := EQBandWidth[i];
   end;
   FEchoLevel := MaxDX8Effect div 2{16};
   FReverbLevel := MaxDX8Effect div 2{16};
   FFlangerLevel := MaxDX8Effect div 2{16};

   FOutputVolume := MaxVolume;
   BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, FOutputVolume * 39);
   BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, FOutputVolume * 39); 

   FDLLVersionStr := GetDLLVersionStr;

   end else FBASSReady:=true;

   if not (csDesigning in ComponentState) then
   begin
      fn := GetProgDir + 'basswma.dll';
      if FileExists(fn) then  // * Changed at Ver 2.00
      begin
        {$IFDEF DELPHI_2007_BELOW}
         HBASSWMA := BASS_PluginLoad(pChar(fn), 0);
        {$ELSE}
         HBASSWMA := BASS_PluginLoad(pChar(fn), BASS_UNICODE);
        {$ENDIF}
         if HBASSWMA <> 0 then
            FBASSWMAReady := true;
      end;
      if FileExists(GetProgDir + 'bassmix.dll') then
      begin
         if Load_BASSMIXDLL(GetProgDir + 'bassmix.dll') then
            if (HIWORD(BASS_Mixer_GetVersion) = BASSVERSION) then
               FMixerReady := true
            else
               ShowErrorMsgBox('BASS Mixer version is not ' + BASSVERSIONTEXT + ' !');
      end;

      if Load_BASSCDDLL(GetProgDir + 'basscd.dll') then
      begin
         FBASSCDReady := true;
      // Get list of available CDROM drives
         n := 0;
         while (n < MAXCDDRIVES) do
         begin
            if BASS_CD_GetInfo(n, CD_Info) then
            begin
               t := Format('%s: %s', [Char(CD_Info.letter + Ord('A')), CD_Info.product]); // "letter: description"
               CDDriveList[n] := t;
            end else
               break;

            n := n + 1;
         end;
         FNumCDDrives := n;
      end;

      if Load_BASSMIDIDLL(GetProgDir + 'bassmidi.dll') then
         FBASSMIDIReady := true;

      MPEG := TMPEGaudio.Create;
      Vorbis := TOggVorbis.Create;
      AAC := TAACfile.Create;
      WMA := TWMAfile.Create;
      WAV := TWAVFile.Create;

   // * Changed at Ver 2.00 ( the owner of the forms : Self -> AOwner )
      MPEGFileInfoForm := TMPEGFileInfoForm.Create(AOwner);
      OggVorbisInfoForm := TOggVorbisInfoForm.Create(AOwner);
      WMAInfoForm := TWMAInfoForm.Create(AOwner);

     { for i := 1 to MaxLoadableAddons do
         BASSAddonList[i].Handle := 0; }

   end;
end;


destructor  TBASSPlayer.Destroy;
var
   i : integer;
begin
   if ChannelType <> Channel_NotOpened then
   begin
      BASS_ChannelStop(PlayChannel);
   end;

   if not (csDesigning in ComponentState) then
   begin
     MPEG.Free;
     Vorbis.Free;
     AAC.Free;
     WMA.Free;
     WAV.Free;
   //  QuitPluginCtrl;
     MPEGFileInfoForm.Free;
     OggVorbisInfoForm.Free;
     WMAInfoForm.Free;

    { if MessageHandle <> 0 then
        DeallocateHWnd(MessageHandle);   }
   end;

   if BASSDLLLoaded then
   begin
      BASS_PluginFree(0);  // Unplugs all plugins
      BASS_Free;
      Unload_BASSDLL;
   end;
  { if FBASSWMAReady then
      Unload_BASSWMADLL; }
   if HBASSWMA <> 0 then
      BASS_PluginFree(HBASSWMA);
   if FBASSCDReady then
      Unload_BASSCDDLL;
   if FBASSMIDIReady then
   begin
      if defaultFontHandle <> 0 then
         BASS_MIDI_FontFree(defaultFontHandle);
      Unload_BASSMIDIDLL;
   end;
   if FMixerReady then
      Unload_BASSMIXDLL;

   if msgCount > 0 then
   begin
      SetLength(msgs_org, 0);
      SetLength(msgs_local, 0);
   end;

 // Release subclassed window procedure before deallocating MessageHandle.
   if MessageHandle <> 0 then      // * Added at Ver 2.00.1
   begin
      if GetWindowLong(MessageHandle, GWL_WNDPROC) <> LongInt(FOrgWndProc) then
         SetWindowLong(MessageHandle, GWL_WNDPROC, LongInt(FOrgWndProc));
      DeallocateHWnd(MessageHandle);
    //  FreeObjectInstance(MessageHandle);
   end;

   inherited Destroy;
end;

// Get the version information as string
function TBASSPlayer.GetDLLVersionStr : string;
var
   VersionNum : DWORD;
begin
   if not BASSDLLLoaded then
   begin
      result := '';
      exit;
   end;

   VersionNum := BASS_GetVersion;
   result := intToStr(HIBYTE(HiWord(VersionNum))) + '.' +
             intToStr(LOBYTE(HiWord(VersionNum))) + '.' +
             intToStr(HIBYTE(LoWord(VersionNum))) + '.' +
             intToStr(LOBYTE(LoWord(VersionNum)));
   // intToStr(LoWord(VersionNum)) + '.' + intToStr(HiWord(VersionNum));
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
   if FMIDISoundReady then
      result := result + Lowercase(MIDIFileExt);
end;

function TBASSPlayer.FileInfoBox(StreamName : string): Boolean;
var
   i : integer;
   tmpChannel : DWORD;
   ByteLen : int64;
   ExtCode : string;
   {$IFDEF DELPHI_2007_BELOW}
   _file : array[0..255] of ansichar;
   {$ENDIF}
   Duration, BitRate : DWORD;
   UseGivenData : boolean;

   dumNum : integer;
   dumStr : string;
begin
   result := false;

   if (StreamName = '') or (not FileExists(StreamName)) then
   begin
    //  ShowMessage('Invalid file name.');
     { if StreamName = '' then
         exit;

      if (StreamName <> FStreamName) or (not NetStream) or
         (FSupportedBy <> WinampPlugin) then   // to continue for stream from net }
         exit;
   end;

   ExtCode := UpperCase(ExtractFileExt(StreamName));
   if (ExtCode = '') or (ExtCode = '.') or
       ((length(ExtCode) <> 4) and (length(ExtCode) <> 3)) then
   begin
    //  ShowMessage('Invalid file name.');
      exit;
   end;

 // Check if it is the file type that can be analyzed by native code.
   if (ExtCode = '.MP1') or (ExtCode = '.MP2') or (ExtCode = '.MP3') then
   begin
      MPEG.ReadFromFile(StreamName);
      if MPEG.Valid then
      begin
         MPEGFileInfoForm.SetAttributes(StreamName, MPEG, AAC, false, 0, 0, false);
         MPEGFileInfoForm.ShowModal;
         result := true;
      end {else
         ShowMessage('Not a valid MPEG file - ' + StreamName)};
   end else if (ExtCode = '.AAC') then
   begin
      AAC.ReadFromFile(StreamName);
      if AAC.Valid then
      begin
         UseGivenData := false;
         Duration := 0;
         BitRate := 0;

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
                  Duration := round(BASS_ChannelBytes2Seconds(tmpChannel, ByteLen) * 1000);
                  ByteLen := BASS_StreamGetFilePosition(tmpChannel, BASS_FILEPOS_END);
                  BitRate := round(ByteLen / (0.125 * Duration));   // bitrate (Kbps)
                  BASS_StreamFree(tmpChannel);
                  UseGivenData := true;
               end;
            end;
         end;
         MPEGFileInfoForm.SetAttributes(StreamName, MPEG, AAC, true, Duration, BitRate, UseGivenData);
         MPEGFileInfoForm.ShowModal;
         result := true;
      end
   end else if (ExtCode = '.OGG') then
   begin
      Vorbis.ReadFromFile(StreamName);
      if Vorbis.Valid then
      begin
         OggVorbisInfoForm.SetAttributes(StreamName, Vorbis);
         OggVorbisInfoForm.ShowModal;
         result := true;
      end {else
         ShowMessage('Not a valid OGG Vorbis file - ' + StreamName)};
   end else if (ExtCode = '.WMA') or (ExtCode = '.ASF') then
   begin
      WMA.ReadFromFile(StreamName);
      if WMA.Valid then
      begin
         WMAInfoForm.SetAttributes(StreamName, WMA);
         WMAInfoForm.ShowModal;
         result := true;
      end {else
         ShowMessage('Not a valid WMA file - ' + StreamName)};
   end;

   if StreamName = FStreamName then
   // Reload information of the stream file if it is currently opened stream file.
   // (may be altered some items of them.)
      GetStreamInfo2(StreamName, FStreamInfo, FSupportedBy, dumNum, dumStr);
   if result = true then
      exit; 

   if StreamName = FStreamName then
   // Reload information of the stream file if it is currently opened stream file.
   // (may be altered some items of them.)
      GetStreamInfo2(StreamName, FStreamInfo, FSupportedBy, dumNum, dumStr);
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
//   hLog.Send(StreamName);
   hLog.Send(copy(StreamName, 1, 5));
   if (copy(StreamName, 1, 5) = 'http:') or (copy(StreamName, 1, 4) = 'mms:')
      or (copy(StreamName, 1, 4) = 'ftp:') then begin
        StreamInfo.Duration:=0;
        exit;
      end;

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

      S := UpperCase('.AIFF;' + GetBASSAddonExts);
      if FBASSAACReady then
         S := S + '.M4A;.MP4;'; // acceptable by bass_aac.dll (*.AAC files are checked independently)
    //  s2 := copy(ExtCode, 2, length(ExtCode) - 1);
      s2 := s2 + ';';
      if pos(s2, s) > 0 then
      begin
        {$IFDEF DELPHI_2007_BELOW}
         tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName), 0, 0, 0);
        {$ELSE}
         tmpChannel := BASS_StreamCreateFile(FALSE, PChar(StreamName), 0, 0, BASS_UNICODE);    egeg
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

function TBASSPlayer.GetStreamInfo(StreamName : string;
                       var StreamInfo : TStreamInfo;
                       var SupportedBy : TSupportedBy) : boolean;
var
   dumNum : integer;
   dumStr : string;
begin
   if StreamName = FStreamName then
   begin
      StreamInfo := FStreamInfo;
      SupportedBy := FSupportedBy;
      result := true;
   end else
      result := GetStreamInfo2(StreamName, StreamInfo, SupportedBy, dumNum, dumStr);
end;


procedure TBASSPlayer.ProcMessage(var Msg: TLMessage);
var
   PTitle : PAnsiChar;
   ExtCode : string;
   TagP : pAnsiChar;
   TagVer : word;
   MP3Tag : MP3TagRec;
   PlaybackSec : float;
begin
   if Msg.Msg = DataReadyMsg then
   begin
      if Msg.wParam = PlayListChange then  // * Added at Ver 2.00
      begin
         if Assigned(FOnUpdatePlayList) then
            FOnUpdatePlayList(Self, Msg.lParam);
      end else
         exit;
   end else if Msg.Msg = WM_RequestFromVis then
   begin
      if Assigned(FOnPluginRequest) then
         FOnPluginRequest(Self, Msg.WParam);
   end;

   case Msg.Msg of
      WM_GetMeta       : begin     // BASS received Metadata in a Shoutcast stream
                            PTitle := pointer(Msg.lParam);
                            FStreamInfo.Title := ansistring(PTitle);
                            if Assigned(FOnGetMeta) then
                               FOnGetMeta(Self, ansistring(PTitle));
                         end;
      WM_GetLyric      : if Assigned(FOnGetLyric) then  // BASS encountered lyric event
                            FOnGetLyric(Self, pointer(Msg.WParam));

      WM_SlideEnded    : begin
                           if Msg.WParam = 0 then    // Mute
                           begin
                             BASS_SetConfig(BASS_CONFIG_GVOL_MUSIC, 0);
                             BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, 0);
                           end;

                           AttribSliding := false;
                           if HSlideSync <> 0 then
                           begin
                             BASS_ChannelRemoveSync(PlayChannel, HSlideSync);
                             HSlideSync := 0;
                           end;
                         end;

      WM_GetHTTPHeaders : begin
                             tmpHTTPTag := pAnsiChar(Msg.WParam);
                             FGetHTTPHeader := true;
                          end;
      WM_DownLoaded    : begin     // BASS finished download of an URL stream
                            FDownloaded := true;
                            ExtCode := UpperCase(ExtractFileExt(FStreamName));
                            if (ExtCode = '.MP3') or (ExtCode = '.MP2') or (ExtCode = '.MP1') or
                               (FBASSAACReady and (ExtCode = '.AAC')) then
                            begin
                               TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_ID3);
                               if TagP = nil then
                               begin
                                  TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_ID3V2);
                                  if TagP = nil then
                                     TagVer := 0
                                  else
                                     TagVer := 2;
                               end else
                                  TagVer := 1;
                            end else
                          {  if (ExtCode = '.WMA') then  // ** Tag info is obtained at opening
                            begin
                               TagP := BASS_ChannelGetTags(DecodeChannel, BASS_TAG_WMA);
                               if TagP = nil then
                                  TagVer := 0
                                else
                                   TagVer := 3;
                            end else }
                            begin
                               TagP := nil;
                               TagVer := 0;
                            end;

                            FStreamInfo.FileSize := BASS_StreamGetFilePosition(DecodeChannel, BASS_FILEPOS_END);

                            if (TagVer = 1) or (TagVer = 2) then
                              if ReadFromTagStream(TagP, FStreamInfo.FileSize, TagVer, MP3Tag) then
                                 with FStreamInfo do
                                 begin
                                    Title := MP3Tag.Title;
                                    Artist := MP3Tag.Artist;
                                    Album := MP3Tag.Album;
                                    Year := MP3Tag.Year;
                                    Genre := MP3Tag.Genre;
                                    Track := MP3Tag.Track;
                                    Comment := MP3Tag.Comment;
                                 end;

                         // Reestimate playback length of an URL stream after finishing download.
                         // ** Following method also shows inaccurate result.
                          {  if (ExtCode = '.AAC') then
                            begin
                            // The returned value of BASS_ChannelGetLength is not accurate for AAC files.
                            // So, I used BASS_ChannelGetPosition instead of BASS_ChannelGetLength.
                              ElapsedByte := BASS_StreamGetFilePosition(DecodeChannel, BASS_FILEPOS_CURRENT);
                              ElapsedTime := BASS_ChannelBytes2Seconds(DecodeChannel,
                                                        BASS_ChannelGetPosition(DecodeChannel, BASS_POS_BYTE));
                            // I assumed that about 60% of buffer is filled with data which are accounted in
                            //  ElapsedByte, and not accounted in ElapsedTime.
                              ElapsedTime := ElapsedTime + (BASS_GetConfig(BASS_CONFIG_BUFFER) / 1000) * 0.60;
                              if (ElapsedByte > 0) and (ElapsedTime > 0) then
                              begin
                                 FStreamInfo.Duration := round(ElapsedTime * FStreamInfo.FileSize * 1000 / ElapsedByte);
                                 FStreamInfo.BitRate := round(ElapsedByte / (125 * ElapsedTime));
                              end;
                            end else
                            begin   }
                              PlaybackSec := BASS_ChannelBytes2Seconds(DecodeChannel,
                                                           BASS_ChannelGetLength(DecodeChannel, BASS_POS_BYTE));
                              FStreamInfo.Duration := round(1000 * PlaybackSec);  // in mili seconds
                           // end;

                            if Assigned(FOnDownloaded) then
                               FOnDownloaded(Self, FStreamInfo.Duration);
                         end;

      WM_BASS_StreamCreate : PlayChannel := DWORD(Msg.WParam);  // Message from output plug-in emulator

    //  Use of WM_BASS_StreamFree may cause problem because of reset PlayChannel after set by opening playing channel
    //  WM_BASS_StreamFree   : PlayChannel := 0;        // Message from output plug-in emulator

    //  WM_GetChannelData : omodWrite2;                // message requesting sample data for BASS playing channel
      WM_ChannelUnavailable : begin   // Message from PlayThread (DecodeChannel is not available)
                            ShowErrorMsgBox('Decoding Channel is unavailable by any reasons.'
                                              + chr(10) + 'Opened stream is beging closed.');
                            Close;
                          end;

      WM_QueryEndSession : begin
                              Msg.Result := 1;    // Allow system termination
                           end;
   end;
end;

function TBASSPlayer.OpenURL(URL : string;
                             var StreamInfo : TStreamInfo;
                             var SupportedBy : TSupportedBy;
                             Temp_Paused : boolean;
                             CheckOnly : boolean) : boolean;
var
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
   if ((ExtCode = '.ASF') or (ExtCode = '.WMA')) and (not FBASSWMAReady) then
   begin
         if not CheckOnly then
         begin
           ShowErrorMsgBox('Load BASSWMA.DLL or Winamp input plug-in for ASF/WMA file.');
           if Temp_Paused then
              ResumePlay;
         end;
         exit;
   end else
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
            if not CheckOnly then
            begin
              ShowErrorMsgBox('Invalid or unsupported URL stream.'#10' -> ' + URL);
              if Temp_Paused then
                 ResumePlay;
            end;
            exit;
      end;
   end;

 //----------------------- End of checking validity of URL ----------------------------

   result := true;
   if CheckOnly then
      exit;

   if (pos(LoExtCode, GetNativeFileExts) > 0) or
      (pos(LoExtCode, GetBASSAddOnExts) > 0) then
   begin
      NetStream := true;

      if (ExtCode = '.OGG') then
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
      end else StreamInfo.Title := ExtractFileName2(URL);

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

   begin
      BASS_ChannelGetInfo(DecodeChannel, BassChannelInfo);
      StreamInfo.SampleRate := BassChannelInfo.freq;
      StreamInfo.Channels := BassChannelInfo.chans;
      StreamInfo.Format := BassChannelInfo.ctype;

   // Apply mixer if source has multi channels.   (* Added at Ver 2.00)

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
   end;
end;

function TBASSPlayer.IsValidURL(URL : string) : boolean;
var
   StreamInfo : TStreamInfo;
   SupportedBy : TSupportedBy;
begin
   result := OpenURL(URL, StreamInfo, SupportedBy, false, true);
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

 // get the file name from the name of an URL stream
 // (ex: http://.../.../file_name.mp3 -> file_name)
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

   if not FBASSReady then
   begin
      ShowErrorMsgBox('Player is not ready !');
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
      ShowErrorMsgBox('Invalid or unsupported stream file.'#10' -> ' + StreamName);
      exit;
   end;
   if StreamInfo.Channels > MaxChannels then  // Will this case happen ?
   begin
      ShowErrorMsgBox('Channel count exceeds. (Max Channels : ' + intToStr(MaxChannels)
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

  // Pause previously assigned channel only if it is for net radio station.
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
   end

 // for local stream files
   else begin
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
                  ShowErrorMsgBox('None of sound font is loaded.')
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
         ShowErrorMsgBox('Error during file open ('+InttoStr(BASS_ErrorGetCode)+').'#10' -> ' + StreamName);
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
            ShowErrorMsgBox('TBASSPlayer does not support 32-bit sources.')
         else
            ShowErrorMsgBox('Sound card does not support 16-bit sources.');
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
      end else
         FDecoderName := GetDecoderName(lowercase(ExtCode));

   end;   // end of for 'local stream'

   FStreamName := StreamName_;
   FSupportedBy := SupportedBy;
   FStreamInfo := StreamInfo;
   SetPlayerMode(plmReady);
   result := true;
end;


procedure TBASSPlayer.Play;
var
   StartOK : boolean;
   i : integer;

  function WaitBuffering(Channel : DWORD) : boolean;   // * Added at Ver 2.00
  var
    progress, len : DWORD;
    ExitBuffering : boolean;

  begin
    ExitBuffering := false;
    if (BASS_StreamGetFilePosition(Channel, BASS_FILEPOS_WMA_BUFFER) <> -1) then // it's a WMA stream
      repeat
     // progress = percentage of buffer filled
        progress := BASS_StreamGetFilePosition(Channel, BASS_FILEPOS_WMA_BUFFER);
        if (progress = dword(-1)) then
           ExitBuffering := true; // something's gone wrong! (eg. BASS_Free called)
        if (progress = 100) then
           break; // full
        WinProcessMessages;
        Sleep(20);
      until ExitBuffering
    else  // Shoutcast/Icecast stream
      repeat
        len := BASS_StreamGetFilePosition(Channel, BASS_FILEPOS_END);
        if (len = dword(-1)) then
            ExitBuffering := true; // something's gone wrong! (eg. BASS_Free called)
     // progress = percentage of buffer filled
        progress := round((BASS_StreamGetFilePosition(Channel, BASS_FILEPOS_DOWNLOAD) -
                          BASS_StreamGetFilePosition(Channel, BASS_FILEPOS_CURRENT)) * 100 / len);
        if (progress > 75) then
           break; // over 75% full, enough
        WinProcessMessages;
        Sleep(20);
      until ExitBuffering;

    if ExitBuffering then
    begin
       result := false;
       ShowErrorMsgBox('Error at buffering process.');
       exit;
    end else
       result := true;
  end;

begin
   if NowStarting then   // avoid duplicate Play operation
      exit
   else if ChannelType = Channel_NotOpened then
      exit
   else if FPlayerMode = plmPlaying then
      exit
   else if FPlayerMode = plmPaused then
   begin
      ResumePlay;
      exit;
   end else if FPlayerMode = plmStopped then
   begin
      begin
         SetPosition(0);   // Set playback position to starting point
         if CurrentPosition = PlayLength then
         begin
            ShowErrorMsgBox('Playback reposition of opened stream is not allowed.');
            exit;
         end;
      end;
      Restart;
      exit;
   end;

   StartOK := false;
   for i := 0 to (NumFFTBands - 1) do
      BandOut[i] := 0;

 // play an opened channel
   begin
      if PlayChannel <> 0 then
         BASS_StreamFree(PlayChannel);
      ClearEffectHandle;

      PlayChannel := DecodeChannel;

      if PlayChannel = 0 then
      begin
         ShowErrorMsgBox('Could not open the channel for play.');
         FDecoderName := '';
         exit;
      end;

      SetSoundEffect(FSoundEffects);  // Apply equalyzation and sound effects

      StartOK := BASS_ChannelPlay(PlayChannel, true);
   end;

   if StartOK then
   begin
      BASS_ChannelSetSync(DecodeChannel, BASS_SYNC_END, 0, @PlayEndSync, @Self);
      if ChannelType = Channel_MIDI then
      begin
         LyricSyncParam.Channel := DecodeChannel;
         BASS_ChannelSetSync(DecodeChannel, BASS_SYNC_MIDI_LYRIC, 0, @LyricSync, @LyricSyncParam);
      end;
    //  if ChannelType = Channel_Music then
    //     MusicStartTime := timeGetTime;

      SetPlayerMode(plmPlaying);
      Self.SetPan(FPan);
   end;
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

procedure TBASSPlayer.PausePlay;
begin
      if FPlayerMode <> plmPlaying then
      exit;

   BASS_ChannelPause(PlayChannel);

   SetPlayerMode(plmPaused);
end;

procedure TBASSPlayer.ResumePlay;
begin
   if FPlayerMode <> plmPaused then
      exit;

   if NeedFlush then
   begin
      //BASS_ChannelPreBuf(PlayChannel, DefChannelBuffer);
      BASS_ChannelPlay(PlayChannel, true);
      NeedFlush := false;
   end else begin
      //BASS_ChannelPreBuf(PlayChannel, DefChannelBuffer);
      BASS_ChannelPlay(PlayChannel, false);
   end;


   SetPlayerMode(plmPlaying);
end;

procedure TBASSPlayer.Pause(pAction : boolean);
begin
   if pAction then
      PausePlay
   else
      ResumePlay;
end;

procedure TBASSPlayer.Stop;
begin
   if (FPlayerMode <> plmPlaying) and (FPlayerMode <> plmPaused) then  // * Changed at 2007-07-25
      exit;

   BASS_ChannelStop(PlayChannel);

   SetPlayerMode(plmStopped);
end;

procedure TBASSPlayer.Restart;
var
   StartOK : boolean;
 //  wCycle : integer;
   i : integer;
   tmpMuted : boolean;
begin
   if (FPlayerMode <> plmStopped) then
       exit;

   if NowStarting then
      exit;

 // If Stopped or Paused stream is the type of NetRadio then we should re-open the stream.
 // note) Normal restart (= resume playing the previously opened stream) causes problem.
   if NetRadio then  // * Added at Ver 2.00
   begin
      if not FMute then
      begin
        SetMuteState(true, 0);   // Mute instantly - no fade-out time is needed (not playing state)
        tmpMuted := true;
      end else
        tmpMuted := false;

      if Open(FStreamName) then  // re-open for clean restart
         Play;                   // then re-start

      if tmpMuted then
         SetMuteState(false, 500);
      exit;
   end;

   StartOK := false;

   for i := 0 to (NumFFTBands - 1) do
      BandOut[i] := 0;

  // play a opened channel
   StartOK := BASS_ChannelPlay(PlayChannel, true);

   if StartOK then
   begin
      SetPlayerMode(plmPlaying);
   end;
end;

procedure TBASSPlayer.Close;
begin
   if ChannelType = Channel_NotOpened then
      exit;

   if FMixerPlugged then
   begin
      BASS_Mixer_ChannelRemove(DecodeChannel);
      FMixerPlugged := false;
   end;

   case ChannelType of
      Channel_Music : BASS_MusicFree(DecodeChannel);
      Channel_CD, Channel_Stream, Channel_MIDI, Channel_WMA :
                       BASS_StreamFree(DecodeChannel);
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


function TBASSPlayer.GetChannelFFTData(PFFTData : pointer; FFTFlag : DWORD) : boolean;
begin
   result := false;

   if BASS_ChannelIsActive(PlayChannel) <> BASS_ACTIVE_PLAYING then
      exit;

   if (FFTFlag = BASS_DATA_FFT512) or
      (FFTFlag = BASS_DATA_FFT512 + BASS_DATA_FFT_INDIVIDUAL) or
      (FFTFlag = BASS_DATA_FFT1024) or
      (FFTFlag = BASS_DATA_FFT1024 + BASS_DATA_FFT_INDIVIDUAL) or
      (FFTFlag = BASS_DATA_FFT2048) or
      (FFTFlag = BASS_DATA_FFT2048 + BASS_DATA_FFT_INDIVIDUAL) then
      if BASS_ChannelGetData(PlayChannel, PFFTData, FFTFlag) <> DW_ERROR {-1} then
         result := true;
end;


function TBASSPlayer.GetMessageHandle : HWND;
begin
   result := MessageHandle;
end;

function TBASSPlayer.BASSAddonLoad(FilePath : string) : TFileDesc;
var
   FoundPreloaded : boolean;
   FileName : string;
   AddonHandle : HPLUGIN;
   AddonInfoP : PBASS_PLUGININFO;
   fd: TFileDesc;
begin
   result.Handle := 0;

   if not BASSDLLLoaded then   
      exit;

   if not FileExists(FilePath) then
      exit;

   FoundPreloaded := false;
   FileName := Lowercase(ExtractFileName(FilePath));

   {for i := 1 to FileSupportList.Count-1 do
      if BASSAddonList[i].Handle <> 0 then
         if BASSAddonList[i].Name = FileName then
         begin
            FoundPreloaded := true;
            break;
         end;}
   FoundPreloaded:=FileSupportList.FindName(FileName)>-1;

   if FoundPreloaded then
      exit;

  {$IFDEF DELPHI_2007_BELOW}
   AddonHandle := BASS_PluginLoad(pChar(FilePath), 0);
  {$ELSE}
   AddonHandle := BASS_PluginLoad(pChar(FilePath), BASS_UNICODE);
  {$ENDIF}
   if AddonHandle <> 0 then
   begin
      fd.Handle := AddonHandle;
      fd.Name := FileName;
      AddonInfoP := BASS_PluginGetInfo(AddonHandle);
      if AddonInfoP <> nil then
      begin
         fd.Version := AddonInfoP^.Version;
         fd.NumFormat := AddonInfoP^.formatc;
         fd.FormatP := AddonInfoP^.formats;
      end;

      if FileName = 'bass_aac.dll' then
         FBASSAACReady := true;
      FileSupportList.Add(fd);
      result := fd;
   end;

end;

function TBASSPlayer.BASSAddonFree(AddonHandle : HPLUGIN) : integer;
var
   i : integer;
   Loaded: boolean;
begin
   result := 0;

   if not BASSDLLLoaded then
      exit;

  for i:=FilesupportList.Count-1 downto 0 do begin
    Loaded:=FileSupportList.GetItem(i).Handle=AddonHandle;
    if Loaded then if BASS_PluginFree(AddonHandle) then
      begin
         if FileSupportList.GetItem(i).Name = 'bass_aac.dll' then
            FBASSAACReady := false;
         inc(result);
         FileSupportList.Remove(i);
      end;
  end;
end;

function TBASSPlayer.GetBASSAddonExts : string;
var
   i, j : integer;
   fd: TFileDesc;
begin
   result := '';

   for i := 0 to FileSupportList.Count-1 do begin
      fd:=FileSupportList.GetItem(i);
         for j := 1 to fd.NumFormat do
            {$IFDEF DELPHI_2007_BELOW}
             result := result + LowerCase(fd.FormatP[j-1].exts) + ';';
            {$ELSE}
             result := result + AnsiStrings.LowerCase(fd.FormatP[j-1].exts) + ';';
            {$ENDIF}
   end;

end;

function  TBASSPlayer.GetBASSAddonExts(i: integer) : string; overload;
var
   j : integer;
   fd: TFileDesc;
begin
   result := '';

      fd:=FileSupportList.GetItem(i);
         for j := 1 to fd.NumFormat do
            {$IFDEF DELPHI_2007_BELOW}
             result := result + LowerCase(fd.FormatP[j-1].exts) + ';';
            {$ELSE}
             result := result + AnsiStrings.LowerCase(fd.FormatP[j-1].exts) + ';';
            {$ENDIF}
end;

function TBASSPlayer.GetDecoderName(ExtCode : string) : string;
var
   s : string;
   i, j : integer;
begin
   result := '';

   for i := 0 to FileSupportList.Count-1 do
      if FileSupportList.GetItem(i).Handle <> 0 then
      begin
         s := '';
         for j := 1 to FileSupportList.GetItem(i).NumFormat do
           {$IFDEF DELPHI_2007_BELOW}
            s := s + LowerCase(FileSupportList.GetItem(i).FormatP[j-1].exts);
           {$ELSE}
            s := s + AnsiStrings.LowerCase(FileSupportList.GetItem(i).FormatP[j-1].exts);
           {$ENDIF}

         if pos(ExtCode, s) > 0 then
         begin
            result := FileSupportList.GetItem(i).Name;
            break;
         end;
      end;
end;

function TBASSPlayer.MIDIFontInit(FontFilePath : string;
                            var MIDI_FONTINFO : TMIDI_FONTINFO) : boolean;
var
   aFontHandle : HSOUNDFONT;
   MIDIFont : BASS_MIDI_FONT;
   aMIDI_FONTINFO : BASS_MIDI_FONTINFO;
begin
   result := false;

   if not FBASSMIDIReady then
      exit;

   if FileExists(FontFilePath) then
    {$IFDEF DELPHI_2007_BELOW}
      aFontHandle := BASS_MIDI_FontInit(pChar(FontFilePath), 0)
    {$ELSE}
      aFontHandle := BASS_MIDI_FontInit(pChar(FontFilePath), BASS_UNICODE)
    {$ENDIF}
   else
      exit;

   if aFontHandle <> 0 then
   begin
      if BASS_MIDI_FontGetInfo(aFontHandle, aMIDI_FONTINFO) then
         with MIDI_FONTINFO do
         begin
            FontName := string(aMIDI_FONTINFO.name);
            Copyright := string(aMIDI_FONTINFO.copyright);
            Comment := string(aMIDI_FONTINFO.comment);
            Presets := aMIDI_FONTINFO.presets;
            SampleSize := aMIDI_FONTINFO.samsize;
            SampleLoaded := aMIDI_FONTINFO.samload;
         end;

      if defaultFontHandle <> 0 then
         BASS_MIDI_FontFree(defaultFontHandle);   // free previously initialized default font

      defaultFontHandle := 0;
      FMIDISoundReady := false;

      MIDIFont.font := aFontHandle;
      MIDIFont.preset := -1; // use all presets
      MIDIFont.bank := 0;    // use default bank(s)

    // set default soundfont configuration
      if BASS_MIDI_StreamSetFonts(0, MIDIFont, 1) then
      begin
         defaultFontHandle := aFontHandle;
         defaultMIDI_FONTINFO := MIDI_FONTINFO;
         FMIDISoundReady := true;
         result := true;
      end;

   end;
end;

{ function TBASSPlayer.MIDIFontGetInfo : TMIDI_FONTINFO;
begin
   if MIDIFontHandle = 0 then
   begin
      result.FontName := '';
      result.SampleSize := 0;
      result.SampleLoaded := 0;
   end else
      result := MIDI_FONTINFO;
end; }

function TBASSPlayer.MIDIGetTrackInfo(MIDIFilePath : string;
                                      var MIDITrackInfo : TMIDITrackInfo) : boolean;
var
   pTrackText : pAnsiChar;
   tmpChannel : DWORD;
   NameHeader1 : string[7];
   NameHeader2 : string[6];
   Tracks : integer;
   i : integer;

begin
   result := false;
   MIDITrackInfo.TrackCount := 0;

   if not FBASSMIDIReady then
      exit;

   Tracks := 0;

   if MIDIFilePath = StreamInfo.FileName then
   begin
      if ChannelType <> Channel_MIDI then
         exit;

      tmpChannel := DecodeChannel;
   end else
   begin
      if pos(UpperCase(ExtractFileExt(FStreamName)), MIDIFileExt) = 0 then
         exit;

      NameHeader1 := copy(FStreamName, 1, 7);
      NameHeader2 := copy(FStreamName, 1, 6);
      if (NameHeader1 = 'http://') or (NameHeader2 = 'mms://') then
        {$IFDEF DELPHI_2007_BELOW}
         tmpChannel := BASS_MIDI_StreamCreateURL(pChar(FStreamName), 0,
                                                    BASS_STREAM_DECODE,
                                                    nil, nil, 44100)
        {$ELSE}
         tmpChannel := BASS_MIDI_StreamCreateURL(ToPMultiByte(pWideChar(FStreamName)), 0,
                                                    BASS_STREAM_DECODE,
                                                    nil, nil, 44100)
        {$ENDIF}
      else
        {$IFDEF DELPHI_2007_BELOW}
         tmpChannel := BASS_MIDI_StreamCreateFile(FALSE, pChar(FStreamName), 0, 0,
                                                  BASS_STREAM_DECODE, 44100);
        {$ELSE}
         tmpChannel := BASS_MIDI_StreamCreateFile(FALSE, ToPMultiByte(pWideChar(FStreamName)), 0, 0,
                                                  BASS_STREAM_DECODE or BASS_UNICODE, 44100);
        {$ENDIF}
   end;

   if tmpChannel <> 0 then
   begin
      for i := 0 to 255 do
      begin
         pTrackText := BASS_ChannelGetTags(tmpChannel, BASS_TAG_MIDI_TRACK + i);
         if pTrackText <> nil then
         begin
            inc(Tracks);
            MIDITrackInfo.TrackText[i] := string(pTrackText);
         end else
            break;
      end;

      if tmpChannel <> DecodeChannel then
         BASS_StreamFree(tmpChannel);
   end;

   if Tracks > 0 then
   begin
      MIDITrackInfo.TrackCount := Tracks;
      result := true;
   end;

end;


// ------------------------ Winamp General purpose plug-in support -----------------

// ----------------------------------------------------------------------------------------

procedure TBASSPlayer.SetDownMixToStereo(Value : boolean);   // * New at Ver 2.00
begin
   if not FMixerReady then
      exit;

   FDownMixToStereo := Value;

end;

function TBASSPlayer.GetBuildDate : string;       // * New at Ver 2.00
begin
   Result := BuildDate;
end;

procedure TBASSPlayer.SetPlayEnd;
var
  pStat: integer;
begin
                          //  WaitCycle := 0;
                            pStat:=BASS_ChannelIsActive(PlayChannel);
                            while pStat = BASS_ACTIVE_PLAYING do
                            begin
                               pStat:=BASS_ChannelIsActive(PlayChannel);
                               WinProcessMessages;
                               sleep(50);
                              { inc(WaitCycle);
                               if WaitCycle = 100 then
                                  break; }
                            end;

                            SetPlayerMode(plmStopped);
                            if Assigned(FOnPlayEnd) then
                               FOnPlayEnd(Self);
end;

procedure TBASSPlayer.SetPlayEndA(Data: PtrInt);
begin
  SetPlayEnd;
end;

procedure TBassPlayer.SetPan(Pan: float);
begin
  if Pan<>fPan then fPan:=Pan;

  if Self.Mode<>plmStopped then
    BASS_ChannelSetAttribute(PlayChannel, BASS_ATTRIB_PAN, fPan);
end;

// ------------------------------ end of TBassPlayer ------------------------------


procedure Register;
begin
   RegisterComponents('Samples', [TBASSPlayer]);
end;

end.


unit KSPTypes;

interface

uses Forms, ExtCtrls, Types, Classes, Graphics;

const
  DBT_DEVTYP_OEM              = $0000; // oem-defined device type
  DBTF_MEDIA                  = $0001; // devnode number
  DBT_DEVTYP_VOLUME           = $0002;
  DBT_DEVTYP_PORT             = $0003; // serial, parallel
  DBT_DEVTYP_NET              = $0004; // network resource
  DBT_DEVICEARRIVAL           = $8000;
  DBT_DEVICEQUERYREMOVE       = $8001; // wants to remove, may fail
  DBT_DEVICEQUERYREMOVEFAILED = $8002; // removal aborted
  DBT_DEVICEREMOVECOMPLETE    = $8004;
  DBT_DEVICEREMOVEPENDING     = $8003; // about to remove, still avail.
  DBT_DEVICETYPESPECIFIC      = $8005; // type specific event


type TLoadKSPPluginWindow = function(App: TApplication; Scr: TScreen; RealAtom: Word): TPanel;
  TGetPluginForms = function: TStringList;

type
  TTimeFormat = (tfElapsed,tfRemain);

  TCoverSize = (csSmall, csMedium, csLarge);

  TDoRipJobs = packed record
    Rip, Encode, Coding: boolean;
    Track: Integer;
  end;

  TDockedItems = packed record
      Playlist: boolean;
      SuggOptions: boolean;
      Info: boolean;
      StreamInfo: boolean;
    end;

  TKSPEqualizer = packed record
    Enabled: Boolean;
    e0, e1, e2, e3, e4, e5, e6, e7, e8, e9: integer;
    Visible: boolean;
  end;

  TKSPOptions = packed record
    Equalizer: TKSPEqualizer;
  end;

  TKSPSetup = packed record
      //DockedItems: TDockedItems;
      KSPOptions: TKSPOptions;
  end;

  TSortType = (stByArtist, stByAlbum, stByYear, stByGenre, stCDArtist);
  TKSPAlertType = (atNone, atNewSong, atMinimizeToTray);

  TVisualType = (vtNone, vtSpectrum, vtWaveform);
  TRepeatType = (rtNone, rtOne, rtAll);

  TMP3Settings = packed record
    Bitrate: Word;
    MaxBitrate: Word;
    VBR: boolean;
    VBRHeader: boolean;
    Quality: Byte;
    RateFromTrack: boolean;
    Rate: Integer;
    Mode: Byte;
    Description: string;
    EncodeOGG: boolean;
  end;

  THKPlayControls = packed record
      Play: Word;
      Stop: Word;
      Next: Word;
      Prev: Word;
      StopAfter: Word;
    end;

  THKPlayList = packed record
      Add: Word;
      Delete: Word;
      Edit: Word;
      RepeatPls: Word;
      Shuffle: Word;
    end;

  THotKeyTypes = packed record
    PlayControls: THKPlayControls;
    Playlist: THKPlaylist;
  end;

  PHotKeyRegistration = ^THotKeyRegistration;
  THotKeyRegistration = packed record
    HotKey: Cardinal;
    KeyIndex: Word;
  end;
type
 PDevBroadcastHdr = ^TDevBroadcastHdr;
 {$EXTERNALSYM DEV_BROADCAST_HDR}
 DEV_BROADCAST_HDR = packed record
   dbch_size: DWORD;
   dbch_devicetype: DWORD;
   dbch_reserved: DWORD;
 end;
 TDevBroadcastHdr = DEV_BROADCAST_HDR;
 PDevBroadcastVolume = ^TDevBroadcastVolume;
 {$EXTERNALSYM DEV_BROADCAST_VOLUME}
 DEV_BROADCAST_VOLUME = packed record
   dbcv_size: DWORD;
   dbcv_devicetype: DWORD;
   dbcv_reserved: DWORD;
   dbcv_unitmask: DWORD;
   dbcv_flags: Word;
 end;

 TDevBroadcastVolume = DEV_BROADCAST_VOLUME;

implementation

end.

// User file stream callback functions
type
FILECLOSEPROC = procedure(user: Pointer); cdecl;
FILELENPROC = function(user: Pointer): QWORD; cdecl;
FILEREADPROC = function(buffer: Pointer; length: DWORD; user: Pointer): DWORD; cdecl;
FILESEEKPROC = function(offset: QWORD; user: Pointer): BOOL; cdecl;

  BASS_FILEPROCS = record
    close: FILECLOSEPROC;
    length: FILELENPROC;
    read: FILEREADPROC;
    seek: FILESEEKPROC;
  end;

  STREAMPROC = function(handle: HSTREAM; buffer: Pointer; length: DWORD; user: Pointer): DWORD; cdecl;
  {
    User stream callback function. NOTE: A stream function should obviously be as
    quick as possible, other streams (and MOD musics) can't be mixed until
    it's finished.
    handle : The stream that needs writing
    buffer : Buffer to write the samples in
    length : Number of bytes to write
    user   : The 'user' parameter value given when calling BASS_StreamCreate
    RETURN : Number of bytes written. Set the BASS_STREAMPROC_END flag to end
             the stream.
  }

const
  // special STREAMPROCs
  STREAMPROC_DUMMY : STREAMPROC = nil;  // "dummy" stream
  STREAMPROC_PUSH  : STREAMPROC = nil; // push stream

type

  DOWNLOADPROC = procedure(buffer: Pointer; length: DWORD; user: Pointer); cdecl;
  {
    Internet stream download callback function.
    buffer : Buffer containing the downloaded data... NULL=end of download
    length : Number of bytes in the buffer
    user   : The 'user' parameter value given when calling BASS_StreamCreateURL
  }

  SYNCPROC = procedure(handle: HSYNC; channel, data: DWORD; user: Pointer); cdecl;
  {
    Sync callback function. NOTE: a sync callback function should be very
    quick as other syncs cannot be processed until it has finished. If the
    sync is a "mixtime" sync, then other streams and MOD musics can not be
    mixed until it's finished either.
    handle : The sync that has occured
    channel: Channel that the sync occured in
    data   : Additional data associated with the sync's occurance
    user   : The 'user' parameter given when calling BASS_ChannelSetSync
  }

  DSPPROC = procedure(handle: HDSP; channel: DWORD; buffer: Pointer; length: DWORD; user: Pointer); cdecl;
  {
    DSP callback function. NOTE: A DSP function should obviously be as quick
    as possible... other DSP functions, streams and MOD musics can not be
    processed until it's finished.
    handle : The DSP handle
    channel: Channel that the DSP is being applied to
    buffer : Buffer to apply the DSP to
    length : Number of bytes in the buffer
    user   : The 'user' parameter given when calling BASS_ChannelSetDSP
  }

  RECORDPROC = function(handle: HRECORD; buffer: Pointer; length: DWORD; user: Pointer): BOOL; cdecl;
  {
    Recording callback function.
    handle : The recording handle
    buffer : Buffer containing the recorded sample data
    length : Number of bytes
    user   : The 'user' parameter value given when calling BASS_RecordStart
    RETURN : TRUE = continue recording, FALSE = stop
  }

function BASS_SetConfig(option, value: DWORD): BOOL; cdecl; external bassdll;
function BASS_GetConfig(option: DWORD): DWORD; cdecl; external bassdll;
function BASS_SetConfigPtr(option: DWORD; value: Pointer): BOOL; cdecl; external bassdll;
function BASS_GetConfigPtr(option: DWORD): Pointer; cdecl; external bassdll;
function BASS_GetVersion: DWORD; cdecl; external bassdll;
function BASS_ErrorGetCode: LongInt; cdecl; external bassdll;
function BASS_GetDeviceInfo(device: DWORD; var info: BASS_DEVICEINFO): BOOL; cdecl; external bassdll;
function BASS_Init(device: LongInt; freq, flags: DWORD; win: HWND; clsid: PGUID): BOOL; cdecl; external bassdll;
function BASS_SetDevice(device: DWORD): BOOL; cdecl; external bassdll;
function BASS_GetDevice: DWORD; cdecl; external bassdll;
function BASS_Free: BOOL; cdecl; external bassdll;
function BASS_GetDSoundObject(obj: DWORD): Pointer; cdecl; external bassdll;
function BASS_GetInfo(var info: BASS_INFO): BOOL; cdecl; external bassdll;
function BASS_Update(length: DWORD): BOOL; cdecl; external bassdll;
function BASS_GetCPU: FLOAT; cdecl; external bassdll;
function BASS_Start: BOOL; cdecl; external bassdll;
function BASS_Stop: BOOL; cdecl; external bassdll;
function BASS_Pause: BOOL; cdecl; external bassdll;
function BASS_SetVolume(volume: FLOAT): BOOL; cdecl; external bassdll;
function BASS_GetVolume: FLOAT; cdecl; external bassdll;

function BASS_PluginLoad(filename: PChar; flags: DWORD): HPLUGIN; cdecl; external bassdll;
function BASS_PluginFree(handle: HPLUGIN): BOOL; cdecl; external bassdll;
function BASS_PluginGetInfo(handle: HPLUGIN): PBASS_PLUGININFO; cdecl; external bassdll;

function BASS_Set3DFactors(distf, rollf, doppf: FLOAT): BOOL; cdecl; external bassdll;
function BASS_Get3DFactors(var distf, rollf, doppf: FLOAT): BOOL; cdecl; external bassdll;
function BASS_Set3DPosition(var pos, vel, front, top: BASS_3DVECTOR): BOOL; cdecl; external bassdll;
function BASS_Get3DPosition(var pos, vel, front, top: BASS_3DVECTOR): BOOL; cdecl; external bassdll;
procedure BASS_Apply3D; cdecl; external bassdll;
function BASS_SetEAXParameters(env: LongInt; vol, decay, damp: FLOAT): BOOL; cdecl; external bassdll;
function BASS_GetEAXParameters(var env: DWORD; var vol, decay, damp: FLOAT): BOOL; cdecl; external bassdll;

function BASS_MusicLoad(mem: BOOL; f: Pointer; offset: QWORD; length, flags, freq: DWORD): HMUSIC; cdecl; external bassdll;
function BASS_MusicFree(handle: HMUSIC): BOOL; cdecl; external bassdll;

function BASS_SampleLoad(mem: BOOL; f: Pointer; offset: QWORD; length, max, flags: DWORD): HSAMPLE; cdecl; external bassdll;
function BASS_SampleCreate(length, freq, chans, max, flags: DWORD): HSAMPLE; cdecl; external bassdll;
function BASS_SampleFree(handle: HSAMPLE): BOOL; cdecl; external bassdll;
function BASS_SampleSetData(handle: HSAMPLE; buffer: Pointer): BOOL; cdecl; external bassdll;
function BASS_SampleGetData(handle: HSAMPLE; buffer: Pointer): BOOL; cdecl; external bassdll;
function BASS_SampleGetInfo(handle: HSAMPLE; var info: BASS_SAMPLE): BOOL; cdecl; external bassdll;
function BASS_SampleSetInfo(handle: HSAMPLE; var info: BASS_SAMPLE): BOOL; cdecl; external bassdll;
function BASS_SampleGetChannel(handle: HSAMPLE; onlynew: BOOL): HCHANNEL; cdecl; external bassdll;
function BASS_SampleGetChannels(handle: HSAMPLE; channels: Pointer): DWORD; cdecl; external bassdll;
function BASS_SampleStop(handle: HSAMPLE): BOOL; cdecl; external bassdll;

function BASS_StreamCreate(freq, chans, flags: DWORD; proc: STREAMPROC; user: Pointer): HSTREAM; cdecl; external bassdll;
function BASS_StreamCreateFile(mem: BOOL; f: Pointer; offset, length: QWORD; flags: DWORD): HSTREAM; cdecl; external bassdll;
function BASS_StreamCreateURL(url: PAnsiChar; offset: DWORD; flags: DWORD; proc: DOWNLOADPROC; user: Pointer):HSTREAM; cdecl; external bassdll;
function BASS_StreamCreateFileUser(system, flags: DWORD; var procs: BASS_FILEPROCS; user: Pointer): HSTREAM; cdecl; external bassdll;
function BASS_StreamFree(handle: HSTREAM): BOOL; cdecl; external bassdll;
function BASS_StreamGetFilePosition(handle: HSTREAM; mode: DWORD): QWORD; cdecl; external bassdll;
function BASS_StreamPutData(handle: HSTREAM; buffer: Pointer; length: DWORD): DWORD; cdecl; external bassdll;
function BASS_StreamPutFileData(handle: HSTREAM; buffer: Pointer; length: DWORD): DWORD; cdecl; external bassdll;

function BASS_RecordGetDeviceInfo(device: DWORD; var info: BASS_DEVICEINFO): BOOL; cdecl; external bassdll;
function BASS_RecordInit(device: LongInt):BOOL; cdecl; external bassdll;
function BASS_RecordSetDevice(device: DWORD): BOOL; cdecl; external bassdll;
function BASS_RecordGetDevice: DWORD; cdecl; external bassdll;
function BASS_RecordFree: BOOL; cdecl; external bassdll;
function BASS_RecordGetInfo(var info: BASS_RECORDINFO): BOOL; cdecl; external bassdll;
function BASS_RecordGetInputName(input: LongInt): PAnsiChar; cdecl; external bassdll;
function BASS_RecordSetInput(input: LongInt; flags: DWORD; volume: FLOAT): BOOL; cdecl; external bassdll;
function BASS_RecordGetInput(input: LongInt; var volume: FLOAT): DWORD; cdecl; external bassdll;
function BASS_RecordStart(freq, chans, flags: DWORD; proc: RECORDPROC; user: Pointer): HRECORD; cdecl; external bassdll;

function BASS_ChannelBytes2Seconds(handle: DWORD; pos: QWORD): Double; cdecl;external bassdll;
function BASS_ChannelSeconds2Bytes(handle: DWORD; pos: Double): QWORD; cdecl;external bassdll;
function BASS_ChannelGetDevice(handle: DWORD): DWORD; cdecl; external bassdll;
function BASS_ChannelSetDevice(handle, device: DWORD): BOOL; cdecl; external bassdll;
function BASS_ChannelIsActive(handle: DWORD): DWORD; cdecl;external bassdll;
function BASS_ChannelGetInfo(handle: DWORD; var info: BASS_CHANNELINFO):BOOL;cdecl;external bassdll;
function BASS_ChannelGetTags(handle: HSTREAM; tags: DWORD): PAnsiChar; cdecl; external bassdll;
function BASS_ChannelFlags(handle, flags, mask: DWORD): DWORD; cdecl; external bassdll;
function BASS_ChannelUpdate(handle, length: DWORD): BOOL; cdecl; external bassdll;
function BASS_ChannelLock(handle: DWORD; lock: BOOL): BOOL; cdecl; external bassdll;
function BASS_ChannelPlay(handle: DWORD; restart: BOOL): BOOL; cdecl; external bassdll;
function BASS_ChannelStop(handle: DWORD): BOOL; cdecl; external bassdll;
function BASS_ChannelPause(handle: DWORD): BOOL; cdecl; external bassdll;
function BASS_ChannelSetAttribute(handle, attrib: DWORD; value: FLOAT): BOOL; cdecl; external bassdll;
function BASS_ChannelGetAttribute(handle, attrib: DWORD; var value: FLOAT): BOOL; cdecl; external bassdll;
function BASS_ChannelSlideAttribute(handle, attrib: DWORD; value: FLOAT; time: DWORD): BOOL; cdecl; external bassdll;
function BASS_ChannelIsSliding(handle, attrib: DWORD): BOOL; cdecl;external bassdll;
function BASS_ChannelSet3DAttributes(handle: DWORD; mode: LongInt; min, max: FLOAT; iangle, oangle, outvol: LongInt): BOOL; cdecl; external bassdll;
function BASS_ChannelGet3DAttributes(handle: DWORD; var mode: DWORD; var min, max: FLOAT; var iangle, oangle, outvol: DWORD): BOOL; cdecl; external bassdll;
function BASS_ChannelSet3DPosition(handle: DWORD; var pos, orient, vel: BASS_3DVECTOR): BOOL; cdecl; external bassdll;
function BASS_ChannelGet3DPosition(handle: DWORD; var pos, orient, vel: BASS_3DVECTOR): BOOL; cdecl; external bassdll;
function BASS_ChannelGetLength(handle, mode: DWORD): QWORD; cdecl; external bassdll;
function BASS_ChannelSetPosition(handle: DWORD; pos: QWORD; mode: DWORD): BOOL; cdecl; external bassdll;
function BASS_ChannelGetPosition(handle, mode: DWORD): QWORD; cdecl; external bassdll;
function BASS_ChannelGetLevel(handle: DWORD): DWORD; cdecl; external bassdll;
function BASS_ChannelGetData(handle: DWORD; buffer: Pointer; length: DWORD): DWORD; cdecl; external bassdll;
function BASS_ChannelSetSync(handle: DWORD; type_: DWORD; param: QWORD; proc: SYNCPROC; user: Pointer): HSYNC; cdecl; external bassdll;
function BASS_ChannelRemoveSync(handle: DWORD; sync: HSYNC): BOOL; cdecl; external bassdll;
function BASS_ChannelSetDSP(handle: DWORD; proc: DSPPROC; user: Pointer; priority: LongInt): HDSP; cdecl; external bassdll;
function BASS_ChannelRemoveDSP(handle: DWORD; dsp: HDSP): BOOL; cdecl; external bassdll;
function BASS_ChannelSetLink(handle, chan: DWORD): BOOL; cdecl; external bassdll;
function BASS_ChannelRemoveLink(handle, chan: DWORD): BOOL; cdecl; external bassdll;
function BASS_ChannelSetFX(handle, type_: DWORD; priority: LongInt): HFX; cdecl; external bassdll;
function BASS_ChannelRemoveFX(handle: DWORD; fx: HFX): BOOL; cdecl; external bassdll;

function BASS_FXSetParameters(handle: HFX; par: Pointer): BOOL; cdecl; external bassdll;
function BASS_FXGetParameters(handle: HFX; par: Pointer): BOOL; cdecl; external bassdll;
function BASS_FXReset(handle: HFX): BOOL; cdecl; external bassdll;


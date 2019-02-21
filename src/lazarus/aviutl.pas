unit AviUtl;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Windows;

const
  AVIUTL_TRUE = 1; // -1 is not True in AviUtl world
  AVIUTL_FALSE = 0;

  FILTER_FLAG_ACTIVE = 1;
  FILTER_FLAG_ALWAYS_ACTIVE = 4;
  FILTER_FLAG_CONFIG_POPUP = 8;
  FILTER_FLAG_CONFIG_CHECK = 16;
  FILTER_FLAG_CONFIG_RADIO = 32;
  FILTER_FLAG_EX_DATA = 1024;
  FILTER_FLAG_PRIORITY_HIGHEST = 2048;
  FILTER_FLAG_PRIORITY_LOWEST = 4096;
  FILTER_FLAG_WINDOW_THICKFRAME = 8192;
  FILTER_FLAG_WINDOW_SIZE = 16384;
  FILTER_FLAG_DISP_FILTER = 32768;
  FILTER_FLAG_REDRAW = $20000;
  FILTER_FLAG_EX_INFORMATION = $40000;
  FILTER_FLAG_INFORMATION = $80000;
  FILTER_FLAG_NO_CONFIG = $100000;
  FILTER_FLAG_AUDIO_FILTER = $200000;
  FILTER_FLAG_RADIO_BUTTON = $400000;
  FILTER_FLAG_WINDOW_HSCROLL = $800000;
  FILTER_FLAG_WINDOW_VSCROLL = $1000000;
  FILTER_FLAG_INTERLACE_FILTER = $4000000;
  FILTER_FLAG_NO_INIT_DATA = $8000000;
  FILTER_FLAG_IMPORT = $10000000;
  FILTER_FLAG_EXPORT = $20000000;
  FILTER_FLAG_MAIN_MESSAGE = $40000000;
  WM_FILTER_UPDATE = (WM_USER + 100);
  WM_FILTER_FILE_OPEN = (WM_USER + 101);
  WM_FILTER_FILE_CLOSE = (WM_USER + 102);
  WM_FILTER_INIT = (WM_USER + 103);
  WM_FILTER_EXIT = (WM_USER + 104);
  WM_FILTER_SAVE_START = (WM_USER + 105);
  WM_FILTER_SAVE_END = (WM_USER + 106);
  WM_FILTER_IMPORT = (WM_USER + 107);
  WM_FILTER_EXPORT = (WM_USER + 108);
  WM_FILTER_CHANGE_ACTIVE = (WM_USER + 109);
  WM_FILTER_CHANGE_WINDOW = (WM_USER + 110);
  WM_FILTER_CHANGE_PARAM = (WM_USER + 111);
  WM_FILTER_CHANGE_EDIT = (WM_USER + 112);
  WM_FILTER_COMMAND = (WM_USER + 113);
  WM_FILTER_FILE_UPDATE = (WM_USER + 114);
  WM_FILTER_MAIN_MOUSE_DOWN = (WM_USER + 120);
  WM_FILTER_MAIN_MOUSE_UP = (WM_USER + 121);
  WM_FILTER_MAIN_MOUSE_MOVE = (WM_USER + 122);
  WM_FILTER_MAIN_KEY_DOWN = (WM_USER + 123);
  WM_FILTER_MAIN_KEY_UP = (WM_USER + 124);
  WM_FILTER_MAIN_MOVESIZE = (WM_USER + 125);
  WM_FILTER_MAIN_MOUSE_DBLCLK = (WM_USER + 126);
  WM_FILTER_MAIN_MOUSE_R_DOWN = (WM_USER + 127);
  WM_FILTER_MAIN_MOUSE_R_UP = (WM_USER + 128);
  WM_FILTER_MAIN_MOUSE_WHEEL = (WM_USER + 129);
  WM_FILTER_MAIN_CONTEXTMENU = (WM_USER + 130);
  FILTER_UPDATE_STATUS_ALL = 0;
  FILTER_UPDATE_STATUS_TRACK = $10000;
  FILTER_UPDATE_STATUS_CHECK = $20000;
  FILTER_WINDOW_SIZE_CLIENT = $10000000;
  FILTER_WINDOW_SIZE_ADD = $30000000;
  ADD_MENU_ITEM_FLAG_KEY_SHIFT = 1;
  ADD_MENU_ITEM_FLAG_KEY_CTRL = 2;
  ADD_MENU_ITEM_FLAG_KEY_ALT = 4;
  EDIT_OUTPUT_FLAG_NO_DIALOG = 2;
  EDIT_OUTPUT_FLAG_WAV = 4;


  FOURCC_YC48 = $38344359;

type
  AviUtlBool = integer;
  TPixelYC = record
    Y,Cb,Cr: SmallInt;
  end;
  PPixelYC = ^TPixelYC;

  PFilter = ^TFilter;
  PFilterProcInfo = ^TFilterProcInfo;
  PFileInfo = ^TFileInfo;
  PSysInfo = ^TSysInfo;
  PExFunc = ^TExFunc;

  // BOOL    (*func_proc)( FILTER *fp,FILTER_PROC_INFO *fpip );
  TProcFunc = function(fp: PFilter; fpip: PFilterProcInfo): AviUtlBool; cdecl;

  // BOOL    (*func_init)( FILTER *fp );
  TInitFunc = function(fp: PFilter): AviUtlBool; cdecl;

  // BOOL    (*func_exit)( FILTER *fp );
  TExitFunc = function(fp: PFilter): AviUtlBool; cdecl;

  TWndProcFunc = function(Hwnd: HWND; Message: UINT; wp: WPARAM;
    lp: LPARAM; edit: Pointer; filter: PFilter): LRESULT; cdecl;

  // BOOL    (*func_project_load)( FILTER *fp,void *editp,void *data,int size );
  TProjectLoadFunc = function(fp: PFilter; edit: Pointer; Data: Pointer;
    size: integer): AviUtlBool; cdecl;
  // BOOL    (*func_project_save)( FILTER *fp,void *editp,void *data,int *size );
  TProjectSaveFunc = function(fp: PFilter; edit: Pointer; Data: Pointer;
    var size: integer): AviUtlBool; cdecl;

  TIsEditingFunc = function(edit: Pointer): AviUtlBool; cdecl;
  TIsSavingFunc = function(edit: Pointer): AviUtlBool; cdecl;
  TGetFrameFunc = function(edit: Pointer): integer; cdecl;
  TGetFrameNFunc = function(edit: Pointer): integer; cdecl;
  TSetFrame = function(edit: Pointer; n: integer): integer; cdecl;
  TGetSysInfo = function(edit: Pointer; sip: PSysInfo): AviUtlBool; cdecl;
  TGetFileInfoFunc = function(edit: Pointer; fip: PFileInfo): AviUtlBool; cdecl;
  TAviFileOpenFunc = function(filename: PChar; fip: PFileInfo; Flag: integer): Pointer; cdecl;
  TAviFileCloseFunc = procedure(h: Pointer); cdecl;
  TAVIFileSetAudioSampleRateFunc = function(h: Pointer; rate: integer; ch: integer): integer; cdecl;
  TGetSelectFrame = function(edit: Pointer; out s: integer; out e: integer): AviUtlBool; cdecl;
  TGetAudioFilteredFunc = function(edit: Pointer; N: integer;
    Buf: Pointer): integer; cdecl;
  TGetAudioFilteringFunc = function(fp: PFilter; edit: Pointer; N: integer; Buf: Pointer): integer; cdecl;
  TGetPixelFilteredExFunc = function(edit: Pointer; N: integer; pixel: Pointer; w: PInteger; h: PInteger; format: DWORD): AviUtlBool; cdecl;
  TSetYCPFilteringCacheSizeFunc = function(fp: PFilter; W: integer; H: integer; D: integer; Flag: integer): AviUtlBool; cdecl;
  TGetYCPFilteringCacheExFunc = function(fp: PFilter; edit: Pointer; N: integer; w: PInteger; h: PInteger): PPixelYC; cdecl;
  TGetFilterPFunc = function(filterId: integer): PFilter; cdecl;
  TIniLoadIntFunc = function(fp: PFilter; key: PChar; default: integer): integer; cdecl;
  TIniLoadStrFunc = function(fp: PFilter; key: PChar; str: PChar; default: PChar): AviUtlBool; cdecl;
  TAddMenuItemFunc = function(fp: PFilter; Name: PChar; h: THandle;
    id: integer; def_key: integer; flag: integer): integer; cdecl;
  TEditOutputFunc = function(edit: Pointer; FileName: PChar; Flag: integer; Typ: PChar): AviUtlBool; cdecl;
  TDrawTextFunc = procedure(ycp: Pointer; x: integer; y: integer; text: PChar; r: integer; g: integer; b: integer; tr: integer; font: THandle; w: PInteger; h: PInteger); cdecl;

  TExFunc = record
    GetYCPOfs: Pointer;
    GetYCP: Pointer;
    GetPixelP: Pointer;
    GetAudio: Pointer;
    IsEditing: TIsEditingFunc;
    IsSaving: TIsSavingFunc;
    GetFrame: TGetFrameFunc;
    GetFrameN: TGetFrameNFunc;
    GetFrameSize: Pointer;
    SetFrame: TSetFrame;
    SetFrameN: Pointer;
    CopyFrame: Pointer;
    CopyVideo: Pointer;
    CopyAudio: Pointer;
    CopyClip: Pointer;
    PasteClip: Pointer;
    GetFrameStatus: Pointer;
    SetFrameStatus: Pointer;
    IsSaveFrame: Pointer;
    IsKeyFrame: Pointer;
    IsRecompress: Pointer;
    FilterWindowUpdate: Pointer;
    IsFilterWindowDisp: Pointer;
    GetFileInfo: TGetFileInfoFunc;
    GetConfigName: Pointer;
    IsFilterActive: Pointer;
    GetPixelFiltered: Pointer;
    GetAudioFiltered: TGetAudioFilteredFunc;
    GetSelectFrame: TGetSelectFrame;
    SetSelectFrame: Pointer;
    RGB2YC: Pointer;
    YC2RGB: Pointer;
    DlgGetLoadName: Pointer;
    DlgGetSaveName: Pointer;
    IniLoadInt: TIniLoadIntFunc;
    IniSaveInt: Pointer;
    IniLoadStr: TIniLoadStrFunc;
    IniSaveStr: Pointer;
    GetSourceFileInfo: Pointer;
    GetSourceVideoNumber: Pointer;
    GetSysInfo: TGetSysInfo;
    GetFilterP: TGetFilterPFunc;
    GetYCPFiltering: Pointer;
    GetAudioFiltering: TGetAudioFilteringFunc;
    SetYCPFilteringCacheSize: TSetYCPFilteringCacheSizeFunc;
    GetYCPFilteringCache: Pointer;
    GetYCPSourceCache: Pointer;
    GetDispPixelP: Pointer;
    GetPixelSource: Pointer;
    GetPixelFilteredEx: TGetPixelFilteredExFunc;
    GetYCPFilteringCacheEx: TGetYCPFilteringCacheExFunc;
    ExecMultiThreadFunc: Pointer;
    CreateYC: Pointer;
    DeleteYC: Pointer;
    LoadImage: Pointer;
    ResizeYC: Pointer;
    CopyYC: Pointer;
    DrawText: TDrawTextFunc;
    AVIFileOpen: TAviFileOpenFunc;
    AVIFileClose: TAviFileCloseFunc;
    AVIFileReadVideo: Pointer;
    AVIFileReadAudio: Pointer;
    AVIFileGetVideoPixelP: Pointer;
    GetAVIFileFilter: Pointer;
    AVIFileReadAudioSample: Pointer;
    AVIFileSetAudioSampleRate: TAVIFileSetAudioSampleRateFunc;
    GetFrameStatusTable: Pointer;
    SetUndo: Pointer;
    AddMenuItem: TAddMenuItemFunc;
    EditOpen: Pointer;
    EditClose: Pointer;
    EditOutput: TEditOutputFunc;
    SetConfig: Pointer;
    Reserved: array[0..6] of integer;
  end;

  TFileInfo = record
    Flag: integer;
    Name: PChar;
    Width, Height: integer;
    VideoRate, VideoScale: integer;
    AudioRate: integer;
    AudioCh: integer;
    FrameN: integer;
    VideoDecodeFormat: DWORD;
    VideoDecodeBit: integer;
    AudioN: integer;
    Reserved: array[0..3] of integer;
  end;

  TSysInfo = record
    Flag: integer;
    Info: PChar;
    FilterN: integer;
    MinW, MinH: integer;
    MaxW, MaxH: integer;
    MaxFrame: integer;
    EditName: PChar;
    ProjectName: PChar;
    OutputName: PChar;
    VRamW, VRamH: integer;
    VRamYCSize: integer;
    VRamLineSize: integer;
    Font: THandle;
    Build: integer;
    Reserved: array[0..1] of integer;
  end;

  TFilterProcInfo = record
    Flag: integer;
    YCPEdit: Pointer;
    YCPTemp: Pointer;
    X, Y: integer;
    MaxW, MaxH: integer;
    Frame: integer;
    FrameN: integer;
    OrgW, OrgH: integer;
    AudioP: PSmallint;
    AudioN: integer;
    AudioCh: integer;
    PixelP: Pointer;
    EditP: Pointer;
    YCSize: integer;
    LineSize: integer;
    Reserved: array[0..7] of integer;
  end;

  TFilter = record
    Flag: integer;
    X: integer;
    Y: integer;
    Name: PChar;
    TrackN: integer;
    TrackName: PPChar;
    TrackDefault: PInteger;
    TrackS: PInteger;
    TrackE: PInteger;
    CheckN: integer;
    CheckName: PPChar;
    CheckDefault: PInteger;
    // BOOL  (*func_proc)( void *fp,FILTER_PROC_INFO *fpip );
    FuncProc: TProcFunc;
    // BOOL  (*func_init)( void *fp );
    FuncInit: TInitFunc;
    // BOOL  (*func_exit)( void *fp );
    FuncExit: TExitFunc;
    // BOOL  (*func_update)( void *fp,int status );
    FuncUpdate: Pointer;
    // BOOL   (*func_WndProc)( HWND hwnd,UINT message,WPARAM wparam,LPARAM lparam,void *editp,void *fp );
    FuncWndProc: TWndProcFunc;
    Track: PInteger;
    Check: PInteger;
    ExDataPtr: Pointer;
    ExDataSize: integer;
    Information: PChar;
    // BOOL  (*func_save_start)( void *fp,int s,int e,void *editp );
    FuncSaveStart: Pointer;
    // BOOL  (*func_save_end)( void *fp,void *editp );
    FuncSaveEnd: Pointer;
    ExFunc: PExFunc;
    Hwnd: THandle;
    DLLHInst: THandle;
    ExDataDef: Pointer;
    // BOOL  (*func_is_saveframe)( void *fp,void *editp,int saveno,int frame,int fps,int edit_flag,int inter );
    FuncIsSaveFrame: Pointer;
    // BOOL  (*func_project_load)( void *fp,void *editp,void *data,int size );
    FuncProjectLoad: TProjectLoadFunc;
    // BOOL  (*func_project_save)( void *fp,void *editp,void *data,int *size );
    FuncProjectSave: TProjectSaveFunc;
    // BOOL  (*func_modify_title)( void *fp,void *editp,int frame,LPSTR title,int max_title );
    FuncModifyTitle: Pointer;
    DLLPath: PChar;
    Reserved: array[0..1] of integer;
  end;

  TFilterDLL = record
    // int      flag;
    Flag: integer;
    // int      x,y;
    X: integer;
    Y: integer;
    // TCHAR    *name;
    Name: PChar;
    // int      track_n;
    TrackN: integer;
    // TCHAR    **track_name;
    TrackName: PPChar;
    // int      *track_default;
    TrackDefault: PInteger;
    // int      *track_s,*track_e;
    TrackS: PInteger;
    TrackE: PInteger;
    // int      check_n;
    CheckN: integer;
    // TCHAR    **check_name;
    CheckName: PPChar;
    // int      *check_default;
    CheckDefault: PInteger;
    // BOOL    (*func_proc)( FILTER *fp,FILTER_PROC_INFO *fpip );
    FuncProc: TProcFunc;
    // BOOL    (*func_init)( FILTER *fp );
    FuncInit: TInitFunc;
    // BOOL    (*func_exit)( FILTER *fp );
    FuncExit: TExitFunc;
    // BOOL    (*func_update)( FILTER *fp,int status );
    FuncUpdate: Pointer;
    // BOOL     (*func_WndProc)( HWND hwnd,UINT message,WPARAM wparam,LPARAM lparam,void *editp,FILTER *fp );
    FuncWndProc: TWndProcFunc;
    // int      *track,*check;
    Track: PInteger;
    Check: PInteger;
    // void    *ex_data_ptr;
    ExDataPtr: Pointer;
    // int      ex_data_size;
    ExDataSize: integer;
    // TCHAR    *information;
    Information: PChar;
    // BOOL    (*func_save_start)( FILTER *fp,int s,int e,void *editp );
    FuncSaveStart: Pointer;
    // BOOL    (*func_save_end)( FILTER *fp,void *editp );
    FuncSaveEnd: Pointer;
    // EXFUNC    *exfunc;
    ExFunc: PExFunc;
    // HWND    hwnd;
    HWND: THandle;
    // HINSTANCE  dll_hinst;
    DLLHInst: THandle;
    // void    *ex_data_def;
    ExDataDef: Pointer;
    // BOOL    (*func_is_saveframe)( FILTER *fp,void *editp,int saveno,int frame,int fps,int edit_flag,int inter );
    FuncIsSaveFrame: Pointer;
    // BOOL    (*func_project_load)( FILTER *fp,void *editp,void *data,int size );
    FuncProjectLoad: TProjectLoadFunc;
    // BOOL    (*func_project_save)( FILTER *fp,void *editp,void *data,int *size );
    FuncProjectSave: TProjectSaveFunc;
    // BOOL    (*func_modify_title)( FILTER *fp,void *editp,int frame,LPSTR title,int max_title );
    FuncModifyTitle: Pointer;
    // TCHAR    *dll_path;
    DLLPath: PChar;
    // int      reserve[2];
    Reserved: array[0..1] of integer;
  end;
  PFilterDLL = ^TFilterDLL;
  PPFilterDLL = ^PFilterDLL;

  TGetVideoFunc = function(frame: integer): Pointer; cdecl;
  TGetAudioFunc = function(start: integer; length: integer; readed: PInteger): Pointer; cdecl;
  TIsAbortFunc = function(): AviUtlBool; cdecl;
  TRestTimeDispFunc = function(now: integer; total: integer): AviUtlBool; cdecl;
  TUpdatePreviewFunc = function(): AviUtlBool; cdecl;
  TGetVideoExFunc = function(frame: integer; format: DWORD): Pointer; cdecl;
  TOutputInfo = record
    Flag: integer;
    W, H: integer;
    Rate, Scale: integer;
    N: integer;
    Size: integer;
    AudioRate: integer;
    AudioChannel: integer;
    AudioN: integer;
    AudioSize: integer;
    SaveFile: PChar;
    GetVideo: TGetVideoFunc;
    GetAudio: TGetAudioFunc;
    IsAbort: TIsAbortFunc;
    RestTimeDisp: TRestTimeDispFunc;
    GetFlag: Pointer;
    UpdatePreview: TUpdatePreviewFunc;
    GetVideoEx: TGetVideoExFunc;
  end;
  POutputInfo = ^TOutputInfo;

  TOutputInitFunc = function(): AviUtlBool; cdecl;
  TOutputExitFunc = function(): AviUtlBool; cdecl;
  TOutputFunc = function(OI: POutputInfo): AviUtlBool; cdecl;
  TOutputConfigFunc = function(hwnd: THandle; dll_hinst: THandle): AviUtlBool; cdecl;
  TOutputConfigSetFunc = function(data: Pointer; size: Integer): Integer; cdecl;
  TOutputPluginTable = record
    Flag: integer;
    Name: PChar;
    FileFilter: PChar;
    Information: PChar;
    FuncInit: TOutputInitFunc;
    FuncExit: TOutputExitFunc;
    FuncOutput: TOutputFunc;
    FuncConfig: TOutputConfigFunc;
    FuncConfigSet: TOutputConfigSetFunc;
    FuncConfigGet: Pointer;
    Reserved: array[0..15] of integer;
  end;
  POutputPluginTable = ^TOutputPluginTable;

implementation

end.

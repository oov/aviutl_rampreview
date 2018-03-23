unit Main;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Windows, SysUtils, Classes, Process, Remote, AviUtl, Encoder, Parallel, StorageAPI;

type
  { TRamPreview }

  TRamPreview = class
  private
    FRemoteProcess: TProcess;
    FReceiver: TReceiver;
    FVideoEncoder, FAudioEncoder: TEncoder;
    FParallel: TParallel;
    FCS, FFMOCS: TRTLCriticalSection;

    FEntry, FEntryAudio, FEntryExtram: TFilterDLL;
    FMainWindow: THandle;
    FFilters: array of PFilter;
    FOrigProcs: array of TProcFunc;
    FWindow, FFont, FPlayModeList, FResolutionComboBox, FCacheCreateButton,
    FCacheClearButton, FDrawFrameCheckBox, FStatusLabel: THandle;
    FFontHeight: integer;

    FMappedFile: THandle;
    FMappedViewHeader: PViewHeader;
    FMappedViewData: Pointer;

    FDrawFrame: boolean;
    FResolution: integer;

    FCapturing: boolean;
    FPlaying: boolean;
    FCacheSizeUpdatedAt: cardinal;
    FErrorMessage: WideString;

    FStartFrame, FEndFrame, FCurrentFrame: integer;
    {$IFDEF BENCH_ENCODE}
    FTransferTime, FCompressTime, FVideoPushTime, FAudioPushTime: single;
    {$ENDIF}

    {$IFDEF BENCH_UPSCALE}
    FUpScaleTime: single;
    FUpScaleCount: integer;
    {$ENDIF}

    procedure PrepareIPC();
    procedure OnRequest(Sender: TObject; const Command: UTF8String);
    procedure EnterCS(CommandName: string);
    procedure LeaveCS(CommandName: string);

    procedure Clear();
    procedure SetDrawFrame(AValue: boolean);
    procedure SetPlaying(AValue: boolean);
    procedure SetResolution(AValue: integer);
    function Stat(): QWord;
    procedure Put(const Key: integer; const Size: integer);
    function Get(const Key: integer): integer;

    procedure ClearS();
    procedure PutS(const Key: string; const Size: integer);
    function GetS(const Key: string): integer;
    function DelS(const Key: string): integer;

    procedure EncodeVideo(const UserData, Buffer, TempBuffer: Pointer);
    procedure EncodeAudio(const UserData, Buffer, TempBuffer: Pointer);
    procedure PutVideo(const Buffer: Pointer;
      const Frame, Width, Height, Mode, Len: integer);
    procedure PutAudio(const Buffer: Pointer; const Frame, Samples, Channels: integer);

    procedure CaptureRange(Edit: Pointer; Filter: PFilter);
    procedure ClearCache(Edit: Pointer; Filter: PFilter);

    procedure ClearStorageCache(Edit: Pointer; Filter: PFilter);

    function GetEntry: PFilterDLL;
    function GetEntryAudio: PFilterDLL;
    function GetEntryExtram: PFilterDLL;
    function InitProc(Filter: PFilter): boolean;
    function ExitProc(Filter: PFilter): boolean;
    function MainProc(Window: HWND; Message: UINT; WP: WPARAM;
      LP: LPARAM; Edit: Pointer; Filter: PFilter): integer;
    function FilterProc(fp: PFilter; fpip: PFilterProcInfo): boolean;
    function FilterAudioProc(fp: PFilter; fpip: PFilterProcInfo): boolean;
    function ProjectLoadProc(Filter: PFilter; Edit: Pointer; Data: Pointer;
      Size: integer): boolean;
    function ProjectSaveProc(Filter: PFilter; Edit: Pointer; Data: Pointer;
      var Size: integer): boolean;
    procedure UpdateStatusLabel();
  public
    constructor Create();
    destructor Destroy(); override;
    property Entry: PFilterDLL read GetEntry;
    property EntryAudio: PFilterDLL read GetEntryAudio;
    property EntryExtram: PFilterDLL read GetEntryExtram;
    property Playing: boolean read FPlaying write SetPlaying;
    property Resolution: integer read FResolution write SetResolution;
    property DrawFrame: boolean read FDrawFrame write SetDrawFrame;
  end;

function GetFilterTableList(): PPFilterDLL; stdcall;
function GetOutputPluginTable(): POutputPluginTable; stdcall;
function GetStorageAPI(): PStorageAPI; cdecl;

var
  RamPreview: TRamPreview;

implementation

uses
  Hook, NV12, Util, Ver;

const
  PluginName = '拡張編集RAMプレビュー';
  PluginNameANSI = #$8a#$67#$92#$a3#$95#$d2#$8f#$57#$52#$41#$4d#$83#$76#$83#$8c#$83#$72#$83#$85#$81#$5b;
  PluginInfoANSI = PluginNameANSI + ' ' + Version;
  PluginNameAudioANSI = #$8a#$67#$92#$a3#$95#$d2#$8f#$57#$52#$41#$4d#$83#$76#$83#$8c#$83#$72#$83#$85#$81#$5b#$28#$89#$b9#$90#$ba#$29;
  PluginInfoAudioANSI = PluginNameAudioANSI + ' ' + Version;
  OutputPluginNameANSI = PluginNameANSI;
  OutputPluginInfoANSI = PluginNameANSI + ' ' + Version;
  PluginNameExtramANSI = 'Extram';
  PluginInfoExtramANSI = PluginNameExtramANSI + ' ' + Version;

const
  OutputFilter = #$8E#$E8#$93#$AE#$82#$C5#$8E#$67#$82#$A4#$82#$B1#$82#$C6#$82#$CD#$82#$C5#$82#$AB#$82#$DC#$82#$B9#$82#$F1#$00#$44#$4F#$20#$4E#$4F#$54#$20#$55#$53#$45#$20#$54#$48#$49#$53#$00#$00;
  BoolConv: array[boolean] of AviUtlBool = (AVIUTL_FALSE, AVIUTL_TRUE);
  ScaleMap: array[0..3] of integer = (1, 1, 2, 4);

type
  TVideoFrame = record
    Frame, Width, Height, Mode: integer;
  end;
  PVideoFrame = ^TVideoFrame;

  TAudioFrame = record
    Frame, Samples, Channels: integer;
  end;
  PAudioFrame = ^TAudioFrame;

var
  FilterDLLList: array of PFilterDLL;
  OutputPluginTable: TOutputPluginTable;
  Storage: TStorageAPI;

function GetFilterTableList(): PPFilterDLL; stdcall;
begin
  Result := @FilterDLLList[0];
end;

function GetOutputPluginTable(): POutputPluginTable; stdcall;
begin
  Result := @OutputPluginTable;
end;

function StorageGet(Key: PChar): integer; cdecl;
begin
  EnterCriticalSection(RamPreview.FFMOCS);
  Result := RamPreview.GetS(Key) - SizeOf(TViewHeader);
end;

procedure StorageGetFinish(); cdecl;
begin
  LeaveCriticalSection(RamPreview.FFMOCS);
end;

procedure StoragePutStart(); cdecl;
begin
  EnterCriticalSection(RamPreview.FFMOCS);
end;

function StoragePut(Key: PChar; Len: integer): integer; cdecl;
begin
  try
    if Len > Storage.ViewLen then
    begin
      Result := 0;
      Exit;
    end;
    RamPreview.PutS(Key, Len + SizeOf(TViewHeader));
    Result := Len;
  finally
    LeaveCriticalSection(RamPreview.FFMOCS);
  end;
end;

procedure StorageDel(Key: PChar); cdecl;
begin
  RamPreview.DelS(Key);
end;

function GetStorageAPI(): PStorageAPI; cdecl;
begin
  Result := @Storage;
end;

function FilterFuncInit(fp: PFilter): AviUtlBool; cdecl;
begin
  Result := BoolConv[RamPreview.InitProc(fp)];
end;

function FilterFuncProjectLoad(Filter: PFilter; Edit: Pointer;
  Data: Pointer; Size: integer): AviUtlBool; cdecl;
begin
  Result := BoolConv[RamPreview.ProjectLoadProc(Filter, Edit, Data, Size)];
end;

function FilterFuncProjectSave(Filter: PFilter; Edit: Pointer;
  Data: Pointer; var Size: integer): AviUtlBool; cdecl;
begin
  Result := BoolConv[RamPreview.ProjectSaveProc(Filter, Edit, Data, Size)];
end;

function FilterExtramFuncInit(fp: PFilter): AviUtlBool; cdecl;
const
  ClearCacheCaption = #$83#$4c#$83#$83#$83#$62#$83#$56#$83#$85#$8f#$c1#$8b#$8e;
  // キャッシュ消去
begin
  Result := AVIUTL_TRUE;
  fp^.ExFunc^.AddMenuItem(fp, ClearCacheCaption, RamPreview.FWindow, 103, 0, 0);
end;

function DummyFuncProc(fp: PFilter; fpip: PFilterProcInfo): AviUtlBool; cdecl;
begin
  Result := AVIUTL_TRUE;
end;

function FilterFuncProc(fp: PFilter; fpip: PFilterProcInfo): AviUtlBool; cdecl;
begin
  Result := BoolConv[RamPreview.FilterProc(fp, fpip)];
end;

function FilterAudioFuncProc(fp: PFilter; fpip: PFilterProcInfo): AviUtlBool; cdecl;
begin
  Result := BoolConv[RamPreview.FilterAudioProc(fp, fpip)];
end;

function FilterFuncExit(fp: PFilter): AviUtlBool; cdecl;
begin
  Result := BoolConv[RamPreview.ExitProc(fp)];
end;

function FilterFuncWndProc(Window: HWND; Message: UINT; WP: WPARAM;
  LP: LPARAM; Edit: Pointer; Filter: PFilter): LRESULT; cdecl;
begin
  Result := RamPreview.MainProc(Window, Message, WP, LP, Edit, Filter);
end;

function OutputFuncOutput(OI: POutputInfo): AviUtlBool; cdecl;
var
  I, Frame, Read: integer;
  Sec, SamplePos, NextSamplePos, SampleOffset: QWORD;
  aborted: AviUtlBool;
  P: Pointer;
  // VideoFrame: TVideoFrame;
  AudioFrame: TAudioFrame;
  {$IFDEF BENCH_ENCODE}
  Freq, Start, Finish: int64;
  {$ENDIF}
begin
  if not RamPreview.FCapturing then
  begin
    Result := AVIUTL_FALSE;
    Exit;
  end;
  Sec := OI^.Scale * RamPreview.FStartFrame;
  Frame := RamPreview.FStartFrame;
  NextSamplePos := (Sec * OI^.AudioRate) div OI^.Rate;
  SampleOffset := NextSamplePos;
  for I := 0 to OI^.N - 1 do
  begin
    DisableMessageBox(True);
    aborted := OI^.IsAbort();
    DisableMessageBox(False);
    if aborted <> AVIUTL_FALSE then
      break;
    if Length(RamPreview.FErrorMessage) > 0 then
    begin
      Result := AVIUTL_TRUE;
      Exit;
    end;
    OI^.RestTimeDisp(I, OI^.N);

    P := OI^.GetVideoEx(I, FOURCC_YC48);
    // This route is slow than capture on filter.
    // Because we can not avoid memory copy at aviutl side in this route.
    // VideoFrame.Frame := Frame;
    // VideoFrame.Width := OI^.W;
    // VideoFrame.Height := OI^.H;
    // VideoFrame.Mode := RamPreview.FResolution;
    // {$IFDEF BENCH_ENCODE}
    // QueryPerformanceFrequency(Freq);
    // QueryPerformanceCounter(Start);
    // {$ENDIF}
    // RamPreview.FVideoEncoder.WaitPush();
    // Move(P^, RamPreview.FVideoEncoder.Buffer^, OI^.W * OI^.H * SizeOf(TPixelYC));
    // RamPreview.FVideoEncoder.Push(@VideoFrame, SizeOf(TVideoFrame));
    // {$IFDEF BENCH_ENCODE}
    // QueryPerformanceCounter(Finish);
    // RamPreview.FVideoPushTime := RamPreview.FVideoPushTime + (Finish - Start) * 1000 / Freq;
    // {$ENDIF}

    SamplePos := NextSamplePos;
    Inc(Sec, OI^.Scale);
    NextSamplePos := (Sec * OI^.AudioRate) div OI^.Rate;
    P := OI^.GetAudio(SamplePos - SampleOffset, NextSamplePos - SamplePos, @Read);
    AudioFrame.Frame := Frame;
    AudioFrame.Samples := NextSamplePos - SamplePos;
    AudioFrame.Channels := OI^.AudioChannel;
    {$IFDEF BENCH_ENCODE}
    QueryPerformanceFrequency(Freq);
    QueryPerformanceCounter(Start);
    {$ENDIF}
    RamPreview.FAudioEncoder.WaitPush();
    Move(P^, RamPreview.FAudioEncoder.Buffer^, AudioFrame.Samples *
      SizeOf(smallint) * AudioFrame.Channels);
    RamPreview.FAudioEncoder.Push(@AudioFrame, SizeOf(TAudioFrame));
    {$IFDEF BENCH_ENCODE}
    QueryPerformanceCounter(Finish);
    RamPreview.FAudioPushTime :=
      RamPreview.FAudioPushTime + (Finish - Start) * 1000 / Freq;
    {$ENDIF}

    if (I and 15) = 0 then
      OI^.UpdatePreview();

    Inc(Frame);
  end;
  RamPreview.FCapturing := False;
  Result := AVIUTL_TRUE;
end;

{ TRamPreview }

function TRamPreview.MainProc(Window: HWND; Message: UINT; WP: WPARAM;
  LP: LPARAM; Edit: Pointer; Filter: PFilter): integer;
const
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
  ToggleModeCaption = #$93#$ae#$8d#$ec#$83#$82#$81#$5b#$83#$68#$90#$d8#$82#$e8#$91#$d6#$82#$a6#$81#$69#$92#$ca#$8f#$ed#$81#$5e#$52#$41#$4d#$83#$76#$83#$8c#$83#$72#$83#$85#$81#$5b#$81#$6a;
  // 動作モード切り替え（通常／RAMプレビュー）
  CaptureCaption = #$91#$49#$91#$f0#$94#$cd#$88#$cd#$82#$a9#$82#$e7#$83#$4c#$83#$83#$83#$62#$83#$56#$83#$85#$8d#$ec#$90#$ac;
  // 選択範囲からキャッシュ作成
  ClearCacheCaption = #$83#$4c#$83#$83#$83#$62#$83#$56#$83#$85#$8f#$c1#$8b#$8e;
  // キャッシュ消去
var
  Y, Height: integer;
  NCM: TNonClientMetrics;
  DC: THandle;
  sinfo: TSysInfo;
  si: SYSTEM_INFO;
begin
  case Message of
    WM_FILTER_INIT:
    begin
      if Filter^.ExFunc^.GetSysInfo(Edit, @sinfo) <> AVIUTL_FALSE then
      begin
        SetLength(FFilters, sinfo.FilterN);
        for Y := 0 to sinfo.FilterN - 1 do
          FFilters[Y] := Filter^.ExFunc^.GetFilterP(Y);
      end;

      NCM.cbSize := SizeOf(NCM);
      SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NCM), @NCM, 0);
      FFont := CreateFontIndirect(NCM.lfMessageFont);
      DC := GetDC(Window);
      try
        FFontHeight := -MulDiv(NCM.lfMessageFont.lfHeight,
          GetDeviceCaps(DC, LOGPIXELSY), 72);
      finally
        ReleaseDC(Window, DC);
      end;

      Y := 8;
      FPlayModeList := CreateWindowW('LISTBOX', nil, WS_BORDER or
        WS_CHILD or WS_VISIBLE or LBS_NOTIFY, 8, Y, 160, 160,
        Window, 1, Filter^.DLLHInst, nil);
      SendMessageW(FPlayModeList, LB_ADDSTRING, 0, LPARAM(PWideChar('通常')));
      SendMessageW(FPlayModeList, LB_ADDSTRING, 0,
        LPARAM(PWideChar('RAMプレビュー')));
      SendMessageW(FPlayModeList, LB_SETCURSEL, 0, 0);
      SendMessageW(FPlayModeList, WM_SETFONT, WPARAM(FFont), 0);
      Height := SendMessage(FPlayModeList, LB_GETITEMHEIGHT, 0, 0) *
        2 + GetSystemMetrics(SM_CYBORDER) * 2;
      SetWindowPos(FPlayModeList, 0, 0, 0, 160, Height, SWP_NOMOVE or SWP_NOZORDER);
      Inc(Y, Height + 8);

      Height := FFontHeight + GetSystemMetrics(SM_CYEDGE) * 2;
      FCacheCreateButton := CreateWindowW('BUTTON',
        '選択範囲からキャッシュ作成', WS_CHILD or
        WS_VISIBLE, 8, Y, 160, Height, Window, 2, Filter^.DLLHInst, nil);
      SendMessageW(FCacheCreateButton, WM_SETFONT, WPARAM(FFont), 0);
      Inc(Y, Height);

      Height := FFontHeight + GetSystemMetrics(SM_CYFIXEDFRAME) * 2;
      FResolutionComboBox := CreateWindowW('COMBOBOX', nil, WS_CHILD or
        WS_VISIBLE or CBS_DROPDOWNLIST, 8, Y, 160, Height + 300,
        Window, 3, Filter^.DLLHInst, nil);
      SendMessageW(FResolutionComboBox, CB_ADDSTRING, 0,
        {%H-}LPARAM(PWideChar('無圧縮')));
      SendMessageW(FResolutionComboBox, CB_ADDSTRING, 0,
        {%H-}LPARAM(PWideChar('通常品質'))); // 1/4
      SendMessageW(FResolutionComboBox, CB_ADDSTRING, 0,
        {%H-}LPARAM(PWideChar('解像度1/2'))); // 1/16
      SendMessageW(FResolutionComboBox, CB_ADDSTRING, 0,
        {%H-}LPARAM(PWideChar('解像度1/4'))); // 1/64
      SendMessageW(FResolutionComboBox, WM_SETFONT, WPARAM(FFont), 0);
      SendMessageW(FResolutionComboBox, CB_SETCURSEL, 1, 0);
      Inc(Y, Height);

      Height := FFontHeight + GetSystemMetrics(SM_CYEDGE) * 2;
      FDrawFrameCheckBox := CreateWindowW('BUTTON',
        'プレビュー中は赤枠を描画', BS_CHECKBOX or
        WS_CHILD or WS_VISIBLE, 8, Y, 160, Height, Window, 5, Filter^.DLLHInst, nil);
      SendMessageW(FDrawFrameCheckBox, WM_SETFONT, WPARAM(FFont), 0);
      Inc(Y, Height + 8);
      DrawFrame := True;

      Height := FFontHeight + GetSystemMetrics(SM_CYEDGE) * 2;
      FCacheClearButton := CreateWindowW('BUTTON', PWideChar('キャッシュ消去'),
        WS_CHILD or WS_VISIBLE, 8, Y, 160, Height, Window, 4,
        Filter^.DLLHInst, nil);
      SendMessageW(FCacheClearButton, WM_SETFONT, WPARAM(FFont), 0);
      Inc(Y, Height);

      Height := FFontHeight;
      FStatusLabel := CreateWindowW('STATIC', PWideChar(''),
        WS_CHILD or WS_VISIBLE or ES_RIGHT, 8, Y, 160, Height,
        Window, 3, Filter^.DLLHInst, nil);
      SendMessageW(FStatusLabel, WM_SETFONT, WPARAM(FFont), 0);
      Inc(Y, Height + 8);

      Inc(Y, GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYFIXEDFRAME) * 2);
      SetWindowPos(Window, 0, 0, 0, 8 + 160 + 8 + GetSystemMetrics(SM_CXFIXEDFRAME) *
        2, Y, SWP_NOMOVE or SWP_NOZORDER);

      try
        GetNativeSystemInfo(@si);
        if si.wProcessorArchitecture <> PROCESSOR_ARCHITECTURE_AMD64 then
          raise Exception.Create(PluginName +
            ' をを使うには 64bit 版の Windows が必要です。');
        if sinfo.Build < 10000 then
          raise Exception.Create(PluginName +
            ' を使うには AviUtl version 1.00 以降が必要です。');

        Filter^.ExFunc^.AddMenuItem(Filter, CaptureCaption, Window,
          100, VK_R, ADD_MENU_ITEM_FLAG_KEY_CTRL);
        Filter^.ExFunc^.AddMenuItem(Filter, ClearCacheCaption, Window,
          101, VK_E, ADD_MENU_ITEM_FLAG_KEY_CTRL);
        Filter^.ExFunc^.AddMenuItem(Filter, ToggleModeCaption, Window,
          102, VK_R, ADD_MENU_ITEM_FLAG_KEY_CTRL or ADD_MENU_ITEM_FLAG_KEY_SHIFT);
      except
        on E: Exception do
        begin
          SetWindowTextW(Window,
            PWideChar(WideString(PluginName +
            ' - 初期化に失敗したため使用できません')));
          MessageBoxW(FMainWindow,
            PWideChar(PluginName +
            ' の初期化中にエラーが発生しました。'#13#10#13#10 +
            WideString(E.Message)),
            PWideChar('初期化エラー - ' + PluginName), MB_ICONERROR);
        end;
      end;
      Result := AVIUTL_FALSE;
    end;
    WM_FILTER_FILE_OPEN:
    begin
      ClearCache(Edit, Filter);
      Result := AVIUTL_TRUE;
    end;
    WM_FILTER_FILE_CLOSE:
    begin
      ClearCache(Edit, Filter);
      Result := AVIUTL_TRUE;
    end;
    WM_FILTER_EXIT:
    begin
      DeleteObject(FFont);
      FFont := 0;
      Result := AVIUTL_TRUE;
    end;
    WM_COMMAND:
    begin
      try
        case LOWORD(WP) of
          1:
          begin
            if HIWORD(WP) = LBN_SELCHANGE then
            begin
              SetFocus(FMainWindow);
              case SendMessageW(FPlayModeList, LB_GETCURSEL, 0, 0) of
                0: Playing := False;
                1: Playing := True;
                else
                  raise Exception.Create('unexpected play mode value');
              end;
            end;
            Result := AVIUTL_TRUE;
          end;
          2:
          begin
            SetFocus(FMainWindow);
            CaptureRange(Edit, Filter);
            Result := AVIUTL_TRUE;
          end;
          3:
          begin
            if HIWORD(WP) = LBN_SELCHANGE then
              SetFocus(FMainWindow);
            Resolution := SendMessageW(FResolutionComboBox, CB_GETCURSEL, 0, 0);
            Result := AVIUTL_FALSE;
          end;
          4:
          begin
            SetFocus(FMainWindow);
            ClearCache(Edit, Filter);
            Result := AVIUTL_TRUE;
          end;
          5:
          begin
            DrawFrame := not DrawFrame;
            if FPlaying then
              Result := AVIUTL_TRUE
            else
              Result := AVIUTL_FALSE;
          end;
        end;
      except
        on E: Exception do
          MessageBoxW(FWindow, PWideChar(
            WideString('処理中にエラーが発生しました。'#13#10#13#10 +
            WideString(E.Message))),
            PluginName, MB_ICONERROR);
      end;
    end;
    WM_FILTER_COMMAND:
    begin
      try
        case LOWORD(WP) of
          100: CaptureRange(Edit, Filter);
          101: ClearCache(Edit, Filter);
          102: Playing := not Playing;
          103: ClearStorageCache(Edit, Filter);
        end;
        Result := AVIUTL_TRUE;
      except
        on E: Exception do
          MessageBoxW(FWindow, PWideChar(
            WideString('処理中にエラーが発生しました。'#13#10#13#10 +
            WideString(E.Message))),
            PluginName, MB_ICONERROR);
      end;

    end;
    else
    begin
      Result := AVIUTL_FALSE;
    end;
  end;
end;

function TRamPreview.FilterProc(fp: PFilter; fpip: PFilterProcInfo): boolean;
var
  S: string;
  X, Y, Len: integer;
  VideoFrame: TVideoFrame;
  Freq, Start, Finish: int64;
  Color: TPixelYC;
begin
  Result := True;
  try
    if FCapturing then
    begin
      VideoFrame.Frame := fpip^.Frame;
      VideoFrame.Width := fpip^.X;
      VideoFrame.Height := fpip^.Y;
      VideoFrame.Mode := FResolution;

      {$IFDEF BENCH_ENCODE}
      QueryPerformanceFrequency(Freq);
      QueryPerformanceCounter(Start);
      {$ENDIF}
      FVideoEncoder.WaitPush();
      CopyYC48(FParallel, FVideoEncoder.Buffer, fpip^.YCPEdit, fpip^.X, fpip^.Y,
        fpip^.LineSize, fpip^.X * SizeOf(TPixelYC));
      FVideoEncoder.Push(@VideoFrame, SizeOf(TVideoFrame));
      {$IFDEF BENCH_ENCODE}
      QueryPerformanceCounter(Finish);
      FVideoPushTime := FVideoPushTime + (Finish - Start) * 1000 / Freq;
      {$ENDIF}

      FCurrentFrame := fpip^.Frame;
      if (FStartFrame - FCurrentFrame) and 15 <> 0 then
      begin
        fpip^.X := 4;
        fpip^.Y := 4;
      end;

      if GetTickCount() > FCacheSizeUpdatedAt + 500 then
      begin
        FCacheSizeUpdatedAt := GetTickCount();
        UpdateStatusLabel();
      end;

      Exit;
    end;
    if not FPlaying then
      Exit;
    EnterCriticalSection(FFMOCS);
    try
      Len := Get(fpip^.Frame + 1);
      if Len > SizeOf(TViewHeader) then
      begin
        fpip^.X := FMappedViewHeader^.A;
        fpip^.Y := FMappedViewHeader^.B;
        case FMappedViewHeader^.C of
          0:
          begin // Full
            CopyYC48(FParallel, fpip^.YCPEdit, FMappedViewData, fpip^.X, fpip^.Y,
              fpip^.X * fpip^.YCSize, fpip^.LineSize);
          end;
          1:
          begin // 1/4
            DecodeNV12ToYC48(FParallel, fpip^.YCPEdit, FMappedViewData,
              FMappedViewHeader^.A,
              FMappedViewHeader^.B, fpip^.LineSize);
          end;
          2, 3:
          begin // 1/16, 1/64
            X := FMappedViewHeader^.A;
            Y := FMappedViewHeader^.B;
            CalcDownScaledSize(X, Y, ScaleMap[FMappedViewHeader^.C]);
            DecodeNV12ToYC48(FParallel, fpip^.YCPTemp, FMappedViewData,
              X, Y, X * SizeOf(TPixelYC));
            {$IFDEF BENCH_UPSCALE}
            QueryPerformanceFrequency(Freq);
            QueryPerformanceCounter(Start);
            {$ENDIF}
            UpScaleYC48(FParallel, fpip^.YCPEdit, fpip^.YCPTemp, FMappedViewHeader^.A,
              FMappedViewHeader^.B, fpip^.LineSize, ScaleMap[FMappedViewHeader^.C]);
            {$IFDEF BENCH_UPSCALE}
            QueryPerformanceCounter(Finish);
            FUpScaleTime := FUpScaleTime + (Finish - Start) * 1000 / Freq;
            Inc(FUpScaleCount);
            if (FUpScaleCount and 31) = 0 then
              OutputDebugString(PChar(Format('UpScaleTime: Avg %0.3fms',
                [FUpScaleTime / FUpScaleCount])));
            {$ENDIF}
          end;
        end;
        if FDrawFrame and (fp^.ExFunc^.IsSaving(fpip^.EditP) = AVIUTL_FALSE) then
        begin
          Color.Y := 1225;
          Color.Cb := -691;
          Color.Cr := 2048;
          DrawFrameYC48(fpip^.YCPEdit, fpip^.X, fpip^.Y, fpip^.LineSize, 4, Color);
        end;
      end
      else
      begin
        FillChar(fpip^.YCPEdit^, fpip^.LineSize * fpip^.Y, 0);
        S := Format('Frame %d: no cache', [fpip^.Frame]);
        fp^.ExFunc^.DrawText(fpip^.YCPEdit, 0, 0, PChar(S), 255, 255,
          255, 0, 0, nil, nil);
      end;
    finally
      LeaveCriticalSection(FFMOCS);
    end;
  except
    on E: Exception do
    begin
      if FErrorMessage = '' then
      begin
        FErrorMessage := WideString(
          'ビデオ処理中にエラーが発生しました。'#13#10#13#10 +
          E.Message);
        if not FCapturing then
          MessageBoxW(FWindow,
            PWideChar(FErrorMessage), PluginName, MB_ICONERROR);
      end;
      Result := False;
    end;
  end;
end;

function TRamPreview.FilterAudioProc(fp: PFilter; fpip: PFilterProcInfo): boolean;
var
  Len: integer;
begin
  Result := True;
  try
    if FCapturing then
      Exit;
    if not FPlaying then
      Exit;
    EnterCriticalSection(FFMOCS);
    try
      Len := Get(-fpip^.Frame - 1);
      if (Len > SizeOf(TViewHeader)) and (FMappedViewHeader^.A = fpip^.AudioN) and
        (FMappedViewHeader^.B = fpip^.AudioCh) then
        Move(FMappedViewData^, fpip^.AudioP^, fpip^.AudioCh *
          fpip^.AudioN * SizeOf(smallint))
      else
        FillChar(fpip^.AudioP^, fpip^.AudioCh * fpip^.AudioN * SizeOf(smallint), 0);
    finally
      LeaveCriticalSection(FFMOCS);
    end;
  except
    on E: Exception do
    begin
      if FErrorMessage = '' then
      begin
        FErrorMessage := WideString(
          'オーディオ処理中にエラーが発生しました。'#13#10#13#10 +
          E.Message);
        if not FCapturing then
          MessageBoxW(FWindow,
            PWideChar(FErrorMessage), PluginName, MB_ICONERROR);
      end;
      Result := False;
    end;
  end;
end;

function TRamPreview.ProjectLoadProc(Filter: PFilter; Edit: Pointer;
  Data: Pointer; Size: integer): boolean;
var
  SL: TStringList;
  MS: TMemoryStream;
  I: integer;
begin
  SL := TStringList.Create();
  try
    MS := TMemoryStream.Create();
    try
      MS.Write(Data^, Size);
      MS.Position := 0;
      SL.LoadFromStream(MS);
    finally
      MS.Free();
    end;
    I := SL.IndexOfName('resolution');
    if I <> -1 then
      Resolution := StrToIntDef(SL.ValueFromIndex[I], 0);
    I := SL.IndexOfName('drawframe');
    if I <> -1 then
      DrawFrame := StrToIntDef(SL.ValueFromIndex[I], 0) <> 0;
    Result := True;
  finally
    SL.Free();
  end;
end;

function TRamPreview.ProjectSaveProc(Filter: PFilter; Edit: Pointer;
  Data: Pointer; var Size: integer): boolean;
var
  SL: TStringList;
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create();
  try
    SL := TStringList.Create();
    try
      SL.Add('resolution=' + IntToStr(Resolution));
      SL.Add('drawframe=' + IntToStr(Ord(DrawFrame)));
      SL.SaveToStream(MS);
    finally
      SL.Free();
    end;
    Size := MS.Size;
    if Assigned(Data) then
      Move(MS.Memory^, Data^, MS.Size);
    Result := True;
  finally
    MS.Free();
  end;
end;

function TRamPreview.InitProc(Filter: PFilter): boolean;
var
  SI: TSysInfo;
  Len: integer;
  P: Pointer;
begin
  Result := True;
  try
    FParallel := TParallel.Create(TThread.ProcessorCount);

    if Filter^.ExFunc^.GetSysInfo(nil, @SI) = AVIUTL_FALSE then
      raise Exception.Create('AviUtl のシステム情報取得に失敗しました');

    Len := Max(SI.MaxW, 1280) * Max(SI.MaxH, 720) * SizeOf(TPixelYC);
    FMappedFile := CreateFileMappingW(INVALID_HANDLE_VALUE, nil,
      PAGE_READWRITE, 0, DWORD((Len + SizeOf(TViewHeader)) and $ffffffff), nil);
    if FMappedFile = 0 then
      raise Exception.Create('CreateFileMapping に失敗しました');

    P := MapViewOfFile(FMappedFile, FILE_MAP_WRITE, 0, 0, 0);
    if P = nil then
      raise Exception.Create('MapViewOfFile に失敗しました');
    FMappedViewHeader := P;
    FMappedViewData := P;
    Inc(FMappedViewData, SizeOf(TViewHeader));

    Storage.ViewHeader := FMappedViewHeader;
    Storage.View := FMappedViewData;
    Storage.ViewLen := Len;

    FWindow := Filter^.Hwnd;
  except
    on E: Exception do
      MessageBoxW(Filter^.Hwnd,
        PWideChar(WideString('初期化中にエラーが発生しました。'#13#10#13#10 + WideString(E.Message))), PluginName, MB_ICONERROR);
  end;
end;

function TRamPreview.ExitProc(Filter: PFilter): boolean;
begin
  Result := True;
  try
    if FMappedViewHeader <> nil then
    begin
      UnmapViewOfFile(FMappedViewHeader);
      FMappedViewHeader := nil;
      FMappedViewData := nil;
    end;
    if FMappedFile <> 0 then
    begin
      CloseHandle(FMappedFile);
      FMappedFile := 0;
    end;
    FreeAndNil(FParallel);
  except
    on E: Exception do
      MessageBoxW(Filter^.Hwnd,
        PWideChar(WideString('初期化中にエラーが発生しました。'#13#10#13#10 + WideString(E.Message))), PluginName, MB_ICONERROR);
  end;
end;

function TRamPreview.GetEntry: PFilterDLL;
begin
  Result := @FEntry;
end;

function TRamPreview.GetEntryAudio: PFilterDLL;
begin
  Result := @FEntryAudio;
end;

constructor TRamPreview.Create();
var
  ws: WideString;
begin
  inherited Create;
  InitCriticalSection(FCS);
  InitCriticalSection(FFMOCS);
  FRemoteProcess := TProcess.Create(nil);
  ws := GetDLLName();
  ws[Length(ws) - 2] := 'e';
  ws[Length(ws) - 1] := 'x';
  ws[Length(ws) - 0] := 'e';
  FRemoteProcess.ApplicationName := string(ws);
  FRemoteProcess.Options := [poUsePipes, poNoConsole];
  FReceiver := nil;

  FillChar(FEntry, SizeOf(FEntry), 0);
  FEntry.Flag := FILTER_FLAG_ALWAYS_ACTIVE or FILTER_FLAG_EX_INFORMATION or
    FILTER_FLAG_PRIORITY_LOWEST or FILTER_FLAG_RADIO_BUTTON;
  FEntry.Name := PluginNameANSI;
  FEntry.Information := PluginInfoANSI;

  FillChar(FEntryAudio, SizeOf(FEntryAudio), 0);
  FEntryAudio.Flag := FILTER_FLAG_ALWAYS_ACTIVE or FILTER_FLAG_EX_INFORMATION or
    FILTER_FLAG_PRIORITY_LOWEST or FILTER_FLAG_AUDIO_FILTER or
    FILTER_FLAG_NO_CONFIG or FILTER_FLAG_RADIO_BUTTON;
  FEntryAudio.Name := PluginNameAudioANSI;
  FEntryAudio.Information := PluginInfoAudioANSI;

  FillChar(FEntryExtram, SizeOf(FEntryExtram), 0);
  FEntryExtram.Flag := FILTER_FLAG_ALWAYS_ACTIVE or FILTER_FLAG_EX_INFORMATION or
    FILTER_FLAG_DISP_FILTER or FILTER_FLAG_NO_CONFIG or FILTER_FLAG_RADIO_BUTTON;
  FEntryExtram.Name := PluginNameExtramANSI;
  FEntryExtram.Information := PluginInfoExtramANSI;

  FMainWindow := FindAviUtlWindow();
end;

destructor TRamPreview.Destroy();
begin
  if FReceiver <> nil then
    FReceiver.Terminate;

  if FRemoteProcess.Running then
  begin
    FRemoteProcess.CloseInput;
    FRemoteProcess.CloseOutput;
  end;
  FreeAndNil(FRemoteProcess);

  if FReceiver <> nil then
  begin
    while not FReceiver.Finished do
      ThreadSwitch();
    FreeAndNil(FReceiver);
  end;

  DoneCriticalSection(FFMOCS);
  DoneCriticalSection(FCS);
  inherited Destroy;
end;

procedure TRamPreview.OnRequest(Sender: TObject; const Command: UTF8String);
const
  UnknownCommandErr = 'Unknown Command';
begin
  WriteUInt32(FRemoteProcess.Input, Length(UnknownCommandErr) or $80000000);
  WriteString(FRemoteProcess.Input, UnknownCommandErr);
end;

procedure TRamPreview.EnterCS(CommandName: string);
begin
  EnterCriticalSection(FCS);
  ODS('%s BEGIN', [CommandName]);
end;

procedure TRamPreview.LeaveCS(CommandName: string);
begin
  ODS('%s END', [CommandName]);
  LeaveCriticalSection(FCS);
end;

procedure TRamPreview.Clear();
begin
  EnterCS('CLR ');
  try
    PrepareIPC();
    FRemoteProcess.Input.WriteBuffer('CLR ', 4);
  finally
    LeaveCS('CLR ');
  end;
  FReceiver.WaitResult();
  FReceiver.Done();
end;

procedure TRamPreview.SetDrawFrame(AValue: boolean);
const
  CheckState: array[boolean] of WPARAM = (BST_UNCHECKED, BST_CHECKED);
begin
  if FDrawFrame = AValue then
    Exit;
  SendMessage(FDrawFrameCheckBox, BM_SETCHECK, CheckState[AValue], 0);
  FDrawFrame := AValue;
end;

procedure TRamPreview.SetPlaying(AValue: boolean);
const
  V: array[boolean] of WPARAM = (0, 1);
var
  I: integer;
begin
  if FPlaying = AValue then
    Exit;
  FPlaying := AValue;
  SendMessageW(FPlayModeList, LB_SETCURSEL, V[AValue], 0);

  if not FPlaying then
  begin
    for I := Low(FFilters) to High(FFilters) do
      if FFilters[I]^.FuncProc = @DummyFuncProc then
        FFilters[I]^.FuncProc := FOrigProcs[I];
    Exit;
  end;

  SetLength(FOrigProcs, Length(FFilters));
  for I := Low(FFilters) to High(FFilters) do
  begin
    FOrigProcs[I] := FFilters[I]^.FuncProc;
    if (FOrigProcs[I] <> nil) and (FOrigProcs[I] <> @FilterFuncProc) and
      (FOrigProcs[I] <> @FilterAudioFuncProc) then
      FFilters[I]^.FuncProc := @DummyFuncProc;
  end;
end;

procedure TRamPreview.SetResolution(AValue: integer);
begin
  if FResolution = AValue then
    Exit;
  SendMessageW(FResolutionComboBox, CB_SETCURSEL, AValue, 0);
  FResolution := AValue;
end;

function TRamPreview.Stat(): QWord;
begin
  EnterCS('STAT');
  try
    PrepareIPC();
    FRemoteProcess.Input.WriteBuffer('STAT', 4);
  finally
    LeaveCS('STAT');
  end;
  FReceiver.WaitResult();
  try
    Result := FReceiver.ReadUInt64();
  finally
    FReceiver.Done();
  end;
end;

procedure TRamPreview.Put(const Key: integer; const Size: integer);
begin
  EnterCS('PUT ');
  try
    PrepareIPC();
    FRemoteProcess.Input.WriteBuffer('PUT ', 4);
    WriteInt32(FRemoteProcess.Input, Key);
    WriteInt32(FRemoteProcess.Input, Size);
  finally
    LeaveCS('PUT ');
  end;
  FReceiver.WaitResult();
  FReceiver.Done();
end;

function TRamPreview.Get(const Key: integer): integer;
begin
  EnterCS('GET ');
  try
    PrepareIPC();
    FRemoteProcess.Input.WriteBuffer('GET ', 4);
    WriteInt32(FRemoteProcess.Input, Key);
  finally
    LeaveCS('GET ');
  end;
  FReceiver.WaitResult();
  try
    Result := FReceiver.ReadInt32();
  finally
    FReceiver.Done();
  end;
end;

procedure TRamPreview.ClearS();
begin
  EnterCS('CLRS');
  try
    PrepareIPC();
    FRemoteProcess.Input.WriteBuffer('CLRS', 4);
  finally
    LeaveCS('CLRS');
  end;
  FReceiver.WaitResult();
  FReceiver.Done();
end;

procedure TRamPreview.PutS(const Key: string; const Size: integer);
begin
  EnterCS('PUTS');
  try
    PrepareIPC();
    FRemoteProcess.Input.WriteBuffer('PUTS', 4);
    WriteString(FRemoteProcess.Input, Key);
    WriteInt32(FRemoteProcess.Input, Size);
  finally
    LeaveCS('PUTS');
  end;
  FReceiver.WaitResult();
  FReceiver.Done();
end;

function TRamPreview.GetS(const Key: string): integer;
begin
  EnterCS('GETS');
  try
    PrepareIPC();
    FRemoteProcess.Input.WriteBuffer('GETS', 4);
    WriteString(FRemoteProcess.Input, Key);
  finally
    LeaveCS('GETS');
  end;
  FReceiver.WaitResult();
  try
    Result := FReceiver.ReadInt32();
  finally
    FReceiver.Done();
  end;
end;

function TRamPreview.DelS(const Key: string): integer;
begin
  EnterCS('DELS');
  try
    PrepareIPC();
    FRemoteProcess.Input.WriteBuffer('DELS', 4);
    WriteString(FRemoteProcess.Input, Key);
  finally
    LeaveCS('DELS');
  end;
  FReceiver.WaitResult();
  FReceiver.Done();
end;

procedure TRamPreview.EncodeVideo(const UserData, Buffer, TempBuffer: Pointer);
var
  UD: PVideoFrame absolute UserData;
  W, H, Len: integer;
  Freq, Start, Finish: int64;
begin
  case UD^.Mode of
    0:
    begin // Full
      PutVideo(Buffer, UD^.Frame, UD^.Width, UD^.Height, UD^.Mode,
        UD^.Width * SizeOf(TPixelYC) * UD^.Height);
    end;
    1:
    begin // 1/4
      {$IFDEF BENCH_ENCODE}
      QueryPerformanceFrequency(Freq);
      QueryPerformanceCounter(Start);
      {$ENDIF}
      Len := EncodeYC48ToNV12(FParallel, TempBuffer, Buffer, UD^.Width,
        UD^.Height, UD^.Width * SizeOf(TPixelYC));
      {$IFDEF BENCH_ENCODE}
      QueryPerformanceCounter(Finish);
      FCompressTime := FCompressTime + (Finish - Start) * 1000 / Freq;
      {$ENDIF}
      PutVideo(TempBuffer, UD^.Frame, UD^.Width, UD^.Height, UD^.Mode, Len);
    end;
    2, 3:
    begin // 1/16, 1/64
      {$IFDEF BENCH_ENCODE}
      QueryPerformanceFrequency(Freq);
      QueryPerformanceCounter(Start);
      {$ENDIF}
      W := UD^.Width;
      H := UD^.Height;
      DownScaleYC48(FParallel, TempBuffer, Buffer, W, H, UD^.Width * SizeOf(TPixelYC),
        ScaleMap[UD^.Mode]);
      Len := EncodeYC48ToNV12(FParallel, Buffer, TempBuffer, W, H, W * SizeOf(TPixelYC));
      {$IFDEF BENCH_ENCODE}
      QueryPerformanceCounter(Finish);
      FCompressTime := FCompressTime + (Finish - Start) * 1000 / Freq;
      {$ENDIF}
      PutVideo(Buffer, UD^.Frame, UD^.Width, UD^.Height, UD^.Mode, Len);
    end;
  end;
end;

procedure TRamPreview.EncodeAudio(const UserData, Buffer, TempBuffer: Pointer);
var
  UD: PAudioFrame absolute UserData;
begin
  PutAudio(Buffer, UD^.Frame, UD^.Samples, UD^.Channels);
end;

procedure TRamPreview.PutVideo(const Buffer: Pointer;
  const Frame, Width, Height, Mode, Len: integer);
var
  Freq, Start, Finish: int64;
begin
  {$IFDEF BENCH_ENCODE}
  QueryPerformanceFrequency(Freq);
  QueryPerformanceCounter(Start);
  {$ENDIF}
  EnterCriticalSection(FFMOCS);
  try
    FMappedViewHeader^.A := Width;
    FMappedViewHeader^.B := Height;
    FMappedViewHeader^.C := Mode;
    Move(Buffer^, FMappedViewData^, Len);
    Put(Frame + 1, Len + SizeOf(TViewHeader));
  finally
    LeaveCriticalSection(FFMOCS);
  end;
  {$IFDEF BENCH_ENCODE}
  QueryPerformanceCounter(Finish);
  FTransferTime := FTransferTime + (Finish - Start) * 1000 / Freq;
  {$ENDIF}
end;

procedure TRamPreview.PutAudio(const Buffer: Pointer;
  const Frame, Samples, Channels: integer);
var
  Len: integer;
begin
  EnterCriticalSection(FFMOCS);
  try
    FMappedViewHeader^.A := Samples;
    FMappedViewHeader^.B := Channels;
    Len := Samples * SizeOf(smallint) * Channels;
    Move(Buffer^, FMappedViewData^, Len);
    Put(-Frame - 1, Len + SizeOf(TViewHeader));
  finally
    LeaveCriticalSection(FFMOCS);
  end;
end;

function SetFrameRateChangerToNone(const Window: THandle): integer;
var
  MainMenu, Settings, Changer: THandle;
  I, N, ID: integer;
  MII: TMenuItemInfo;
begin
  MainMenu := GetMenu(Window);
  if MainMenu = 0 then
    raise Exception.Create('Main menu not found');
  N := GetMenuItemCount(MainMenu);
  if N <> 7 then
    raise Exception.Create('unexpected Main menu item count');

  Settings := GetSubMenu(MainMenu, 2);
  N := GetMenuItemCount(Settings);
  if N < 8 then
    raise Exception.Create('unexpected Settings menu item count');

  Changer := GetSubMenu(Settings, N - 6);
  N := GetMenuItemCount(Changer);
  if N <> 8 then
    raise Exception.Create('unexpected FrameRateChanger menu item count');

  FillChar(MII, SizeOf(MII), 0);
  MII.cbSize := SizeOf(TMenuItemInfo);
  MII.fMask := MIIM_STATE or MIIM_ID;
  for I := 0 to N - 1 do
  begin
    if not GetMenuItemInfo(Changer, I, True, MII) then
      raise Exception.Create('failed to GetMenuItemInfo');
    if (MII.fState and MF_CHECKED) = MF_CHECKED then
    begin
      Result := MII.wID;
      break;
    end;
    if I = 0 then
      ID := MII.wID;
  end;
  SendMessage(Window, WM_COMMAND, LOWORD(ID), 0);
end;

procedure TRamPreview.CaptureRange(Edit: Pointer; Filter: PFilter);
var
  FI: TFileInfo;
  SI: TSysInfo;
  SelectedFrameRateChangerID, Frames: integer;
  Freq, Start, Finish: int64;
begin
  if Filter^.ExFunc^.GetSysInfo(nil, @SI) = AVIUTL_FALSE then
    raise Exception.Create('AviUtl のシステム情報取得に失敗しました');

  FillChar(FI, SizeOf(TFileInfo), 0);
  if Filter^.ExFunc^.GetFileInfo(Edit, @FI) = AVIUTL_FALSE then
    raise Exception.Create(
      '編集中のファイルの情報取得に失敗しました');
  if (FI.Width = 0) or (FI.Height = 0) then
    Exit;
  if Filter^.ExFunc^.GetSelectFrame(Edit, FStartFrame, FEndFrame) = AVIUTL_FALSE then
    raise Exception.Create('選択範囲を取得できませんでした');

  Clear();

  SelectedFrameRateChangerID := SetFrameRateChangerToNone(FMainWindow);
  try
    FErrorMessage := '';
{$IFDEF BENCH_ENCODE}
    FTransferTime := 0;
    FCompressTime := 0;
    FVideoPushTime := 0;
    FAudioPushTime := 0;
{$ENDIF}
    Playing := False;
    FCapturing := True;
    DisableGetSaveFileName(True);
    DisablePlaySound(True);

    FVideoEncoder := TEncoder.Create(Max(SI.MaxW, 1280) * Max(SI.MaxH, 720) *
      SizeOf(TPixelYC), True);
    FAudioEncoder := TEncoder.Create(FI.AudioRate * SizeOf(smallint) *
      FI.AudioCh, False);
    try
      FVideoEncoder.OnEncode := @EncodeVideo;
      FAudioEncoder.OnEncode := @EncodeAudio;

      QueryPerformanceFrequency(Freq);
      QueryPerformanceCounter(Start);

      Filter^.ExFunc^.EditOutput(Edit, 'RAM', 0, OutputPluginNameANSI);

      QueryPerformanceCounter(Finish);
      Frames := FEndFrame - FStartFrame + 1;
      {$IFDEF BENCH_ENCODE}
      OutputDebugString(PChar(Format(
        'TotalTime: %0.3fms / Compress: Avg %0.3fms / Transfer: Avg %0.3fms / VideoPushTime: Avg %0.3fms / AudioPushTime: Avg %0.3fms',
        [(Finish - Start) * 1000 / Freq, FCompressTime / Frames,
        FTransferTime / Frames, FVideoPushTime / Frames, FAudioPushTime / Frames])));
      {$ELSE}
      OutputDebugString(PChar(Format('TotalTime: %0.3fms',
        [(Finish - Start) * 1000 / Freq])));
      {$ENDIF}
    finally
      FVideoEncoder.Terminate;
      FVideoEncoder.Push(nil, 0);
      while not FVideoEncoder.Finished do
        ThreadSwitch();
      FreeAndNil(FVideoEncoder);

      FAudioEncoder.Terminate;
      FAudioEncoder.Push(nil, 0);
      while not FAudioEncoder.Finished do
        ThreadSwitch();
      FreeAndNil(FAudioEncoder);
    end;

  finally
    DisablePlaySound(False);
    DisableGetSaveFileName(False);
    FCapturing := False;
    UpdateStatusLabel();

    SendMessage(FMainWindow, WM_COMMAND, LOWORD(SelectedFrameRateChangerID), 0);

    if FErrorMessage = '' then
    begin
      Playing := True;
      Filter^.ExFunc^.SetFrame(Edit, FStartFrame);
    end
    else
    begin
      MessageBoxW(FWindow, PWideChar(
        'キャッシュデータの作成中にエラーが発生しました。'#13#10#13#10 + FErrorMessage),
        PluginName, MB_ICONERROR);
    end;
  end;
end;

procedure TRamPreview.ClearCache(Edit: Pointer; Filter: PFilter);
begin
  if not FRemoteProcess.Running then
    Exit;
  Clear();
  UpdateStatusLabel();
  Playing := False;
end;

procedure TRamPreview.UpdateStatusLabel();
begin
  if FRemoteProcess.Running then
  begin
    if FCapturing then
      SetWindowText(FStatusLabel,
        PChar(Format('%d%% [%d/%d] %s', [(FCurrentFrame - FStartFrame + 1) *
        100 div (FEndFrame - FStartFrame + 1), FCurrentFrame -
        FStartFrame + 1, FEndFrame - FStartFrame + 1, BytesToStr(Stat())])))
    else
      SetWindowText(FStatusLabel, PChar(BytesToStr(Stat())));
  end
  else
    SetWindowTextW(FStatusLabel, '');
  EnableWindow(FStatusLabel, True);
end;

procedure TRamPreview.ClearStorageCache(Edit: Pointer; Filter: PFilter);
begin
  if not FRemoteProcess.Running then
    Exit;
  ClearS();
  UpdateStatusLabel();
end;

procedure TRamPreview.PrepareIPC();
var
  h: THandle;
begin
  if FRemoteProcess.Running then
    Exit;
  try
    FRemoteProcess.Execute;
  except
    on E: EProcess do
    begin
      raise Exception.Create('failed to execute: ZRamPreview.exe'#13#10 +
        WideString(E.Message) + #13#10#13#10 +
        'アンチウィルスソフトがプログラム実行を阻害していないか確認してください。'#13#10 + 'また、AviUtl が日本語やスペースが含まれている場所にある場合は "C:\AviUtl\AviUtl.exe" などに移動してみてください。');
    end;
  end;
  FRemoteProcess.CloseStderr;
  FRemoteProcess.Input.WriteBuffer('HELO', 4);

  if FReceiver <> nil then
    FreeAndNil(FReceiver);
  FReceiver := TReceiver.Create(FRemoteProcess.Output);
  FReceiver.OnRequest := @OnRequest;
  FReceiver.WaitResult();
  FReceiver.Done();

  try
    if not DuplicateHandle(GetCurrentProcess(), FMappedFile,
      FRemoteProcess.ProcessHandle, @h, FILE_MAP_ALL_ACCESS, False, 0) then
      RaiseLastOSError();
  except
    on E: Exception do
      MessageBoxW(FWindow, PWideChar(
        WideString('DuplicateHandle でエラーが発生しました。'#13#10#13#10 +
        WideString(E.Message))),
        PluginName, MB_ICONERROR);
  end;

  FRemoteProcess.Input.WriteBuffer('FMO ', 4);
  WriteUInt64(FRemoteProcess.Input, h);
  FReceiver.WaitResult();
  FReceiver.Done();
end;

function TRamPreview.GetEntryExtram: PFilterDLL;
begin
  Result := @FEntryExtram;
end;

initialization
  RamPreview := TRamPreview.Create();
  RamPreview.Entry^.FuncInit := @FilterFuncInit;
  RamPreview.Entry^.FuncExit := @FilterFuncExit;
  RamPreview.Entry^.FuncWndProc := @FilterFuncWndProc;
  RamPreview.Entry^.FuncProc := @FilterFuncProc;
  RamPreview.Entry^.FuncProjectLoad := @FilterFuncProjectLoad;
  RamPreview.Entry^.FuncProjectSave := @FilterFuncProjectSave;
  RamPreview.EntryAudio^.FuncProc := @FilterAudioFuncProc;
  RamPreview.EntryExtram^.FuncInit := @FilterExtramFuncInit;

  SetLength(FilterDLLList, 4);
  FilterDLLList[0] := RamPreview.Entry;
  FilterDLLList[1] := RamPreview.EntryAudio;
  FilterDLLList[2] := RamPreview.EntryExtram;
  FilterDLLList[3] := nil;

  FillChar(OutputPluginTable, SizeOf(TOutputPluginTable), 0);
  OutputPluginTable.Name := OutputPluginNameANSI;
  OutputPluginTable.Information := OutputPluginInfoANSI;
  OutputPluginTable.FileFilter := OutputFilter;
  OutputPluginTable.FuncOutput := @OutputFuncOutput;

  Storage.Get := @StorageGet;
  Storage.GetFinish := @StorageGetFinish;
  Storage.PutStart := @StoragePutStart;
  Storage.Put := @StoragePut;
  Storage.Del := @StorageDel;

  InitHook();

finalization
  RamPreview.Free();
  FreeHook();

end.

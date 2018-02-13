unit Main;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Windows, SysUtils, Classes, Process, Remote, AviUtl, StorageAPI;

type
  { TRamPreview }

  TRamPreview = class
  private
    FRemoteProcess: TProcess;
    FReceiver: TReceiver;
    FCS: TRTLCriticalSection;

    FEntry, FEntryAudio, FEntryExtram: TFilterDLL;
    FMainWindow: THandle;
    FFilters: array of PFilter;
    FOrigProcs: array of TProcFunc;
    FWindow, FFont, FPlayModeList, FResolution, FCacheCreateButton,
    FCacheClearButton, FCacheSizeLabel: THandle;
    FFontHeight: integer;

    FMappedFile: THandle;
    FMappedViewHeader: PViewHeader;
    FMappedViewData: Pointer;

    FCapturing: boolean;
    FPlaying: boolean;
    FCacheSizeUpdatedAt: cardinal;

    function GetResolution: integer;
    procedure PrepareIPC();
    procedure OnRequest(Sender: TObject; const Command: UTF8String);
    procedure EnterCS(CommandName: string);
    procedure LeaveCS(CommandName: string);

    procedure Clear();
    procedure SetPlaying(AValue: boolean);
    function Stat(): QWord;
    procedure Put(const Key: integer; const Size: integer);
    function Get(const Key: integer): integer;

    procedure ClearS();
    procedure PutS(const Key: string; const Size: integer);
    function GetS(const Key: string): integer;
    function DelS(const Key: string): integer;

    procedure CaptureRange(Edit: Pointer; Filter: PFilter);
    procedure ClearCache(Edit: Pointer; Filter: PFilter);

    procedure ClearStorageCache(Edit: Pointer; Filter: PFilter);

    function GetPlayModeComboBox: boolean;
    procedure SetPlayModeComboBox(AValue: boolean);
    function GetEntry: PFilterDLL;
    function GetEntryAudio: PFilterDLL;
    function GetEntryExtram: PFilterDLL;
    function InitProc(Filter: PFilter): boolean;
    function ExitProc(Filter: PFilter): boolean;
    function MainProc(Window: HWND; Message: UINT; WP: WPARAM;
      LP: LPARAM; Edit: Pointer; Filter: PFilter): integer;
    function FilterProc(fp: PFilter; fpip: PFilterProcInfo): boolean;
    function FilterAudioProc(fp: PFilter; fpip: PFilterProcInfo): boolean;
    procedure UpdateCacheSize();
  public
    constructor Create();
    destructor Destroy(); override;
    property Entry: PFilterDLL read GetEntry;
    property EntryAudio: PFilterDLL read GetEntryAudio;
    property EntryExtram: PFilterDLL read GetEntryExtram;
    property Playing: boolean read FPlaying write SetPlaying;
    property PlayModeComboBox: boolean read GetPlayModeComboBox
      write SetPlayModeComboBox;
    property Resolution: integer read GetResolution;
  end;

function GetFilterTableList(): PPFilterDLL; stdcall;
function GetOutputPluginTable(): POutputPluginTable; stdcall;
function GetStorageAPI(): PStorageAPI; cdecl;

var
  RamPreview: TRamPreview;

implementation

uses
  Util, Ver;

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
  Result := RamPreview.GetS(Key) - SizeOf(TViewHeader);
end;

function StoragePut(Key: PChar; Len: integer): integer; cdecl;
begin
  if Len > Storage.ViewLen then
  begin
    Result := 0;
    Exit;
  end;
  RamPreview.PutS(Key, Len + SizeOf(TViewHeader));
  Result := Len;
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
  I, Sec, SamplePos, NextSamplePos, Read: integer;
begin
  if not RamPreview.FCapturing then begin
    Result := AVIUTL_FALSE;
    Exit;
  end;
  Sec := 0;
  NextSamplePos := 0;
  for I := 0 to OI^.N - 1 do begin
    if OI^.IsAbort() <> AVIUTL_FALSE then break;
    OI^.RestTimeDisp(I, OI^.N);
    SamplePos := NextSamplePos;
    Inc(Sec, OI^.Scale);
    NextSamplePos := (Sec * OI^.AudioRate) div OI^.Rate;
    OI^.GetVideoEx(I, FOURCC_YC48);
    OI^.GetAudio(SamplePos, NextSamplePos - SamplePos, @Read);
    OI^.UpdatePreview();
  end;
  RamPreview.FCapturing := False;
  Result := AVIUTL_TRUE;
end;

{ TRamPreview }

function TRamPreview.MainProc(Window: HWND; Message: UINT; WP: WPARAM;
  LP: LPARAM; Edit: Pointer; Filter: PFilter): integer;
const
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
  ExEditVersion = ' version 0.92 ';
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
      FCacheCreateButton := CreateWindowW('BUTTON', '選択範囲からキャッシュ作成',
        WS_CHILD or WS_VISIBLE, 8, Y, 160, Height, Window, 2, Filter^.DLLHInst, nil);
      SendMessageW(FCacheCreateButton, WM_SETFONT, WPARAM(FFont), 0);
      Inc(Y, Height);

      Height := FFontHeight + GetSystemMetrics(SM_CYFIXEDFRAME) * 2;
      FResolution := CreateWindowW('COMBOBOX', nil, WS_CHILD or
        WS_VISIBLE or CBS_DROPDOWNLIST, 8, Y, 160, Height + 300,
        Window, 3, Filter^.DLLHInst, nil);
      SendMessageW(FResolution, CB_ADDSTRING, 0,
        {%H-}LPARAM(PWideChar('メモリー節約なし')));
      SendMessageW(FResolution, CB_ADDSTRING, 0,
        {%H-}LPARAM(PWideChar('1/4')));
      SendMessageW(FResolution, CB_ADDSTRING, 0,
        {%H-}LPARAM(PWideChar('1/9')));
      SendMessageW(FResolution, CB_ADDSTRING, 0,
        {%H-}LPARAM(PWideChar('1/16')));
      SendMessageW(FResolution, CB_ADDSTRING, 0,
        {%H-}LPARAM(PWideChar('1/25')));
      SendMessageW(FResolution, WM_SETFONT, WPARAM(FFont), 0);
      SendMessageW(FResolution, CB_SETCURSEL, 0, 0);
      Inc(Y, Height + 8);

      Height := FFontHeight + GetSystemMetrics(SM_CYEDGE) * 2;
      FCacheClearButton := CreateWindowW('BUTTON', PWideChar('キャッシュ消去'),
        WS_CHILD or WS_VISIBLE, 8, Y, 160, Height, Window, 4,
        Filter^.DLLHInst, nil);
      SendMessageW(FCacheClearButton, WM_SETFONT, WPARAM(FFont), 0);
      Inc(Y, Height);

      Height := FFontHeight;
      FCacheSizeLabel := CreateWindowW('STATIC', PWideChar(''),
        WS_CHILD or WS_VISIBLE or ES_RIGHT, 8, Y, 160, Height,
        Window, 3, Filter^.DLLHInst, nil);
      SendMessageW(FCacheSizeLabel, WM_SETFONT, WPARAM(FFont), 0);
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
            if HIWORD(WP) = LBN_SELCHANGE then begin
              SetFocus(FMainWindow);
              Playing := PlayModeComboBox;
            end;
            Result := AVIUTL_TRUE;
          end;
          2: begin
            SetFocus(FMainWindow);
            CaptureRange(Edit, Filter);
            Result := AVIUTL_TRUE;
          end;
          3:
          begin
            if HIWORD(WP) = LBN_SELCHANGE then
              SetFocus(FMainWindow);
            Result := AVIUTL_FALSE;
          end;
          4: begin
            SetFocus(FMainWindow);
            ClearCache(Edit, Filter);
            Result := AVIUTL_TRUE;
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
  X, Y, LineSize, Width, Len: integer;
  PYC: PPixelYC;
  SrcLine, DestLine: PByte;
begin
  Result := True;
  if FCapturing then begin
    FMappedViewHeader^.A := fpip^.X;
    FMappedViewHeader^.B := fpip^.Y;
    FMappedViewHeader^.C := fpip^.YCSize;
    FMappedViewHeader^.D := Resolution;
    SrcLine := fpip^.YCPEdit;
    DestLine := FMappedViewData;
    LineSize := fpip^.LineSize;
    case FMappedViewHeader^.D of
      0:
      begin // Full
        Width := fpip^.X * fpip^.YCSize;
        for Y := 0 to fpip^.Y - 1 do
        begin
          Move(SrcLine^, DestLine^, Width);
          Inc(SrcLine, LineSize);
          Inc(DestLine, Width);
        end;
        Put(fpip^.Frame + 1, Width * fpip^.Y + SizeOf(TViewHeader));
      end;
      1:
      begin // 1/4
        Width := (fpip^.X div 2) * fpip^.YCSize;
        for Y := 0 to (fpip^.Y div 2) - 1 do
        begin
          PYC := PPixelYC(SrcLine);
          for X := 0 to (fpip^.X div 2) - 1 do
          begin
            PPixelYC(DestLine)^ := PYC^;
            Inc(DestLine, fpip^.YCSize);
            Inc(PYC, 2);
          end;
          Inc(SrcLine, LineSize * 2);
        end;
        Put(fpip^.Frame + 1, Width * (fpip^.Y div 2) + SizeOf(TViewHeader));
      end;
      2:
      begin
        Width := (fpip^.X div 3) * fpip^.YCSize;
        for Y := 0 to (fpip^.Y div 3) - 1 do
        begin
          PYC := PPixelYC(SrcLine);
          for X := 0 to (fpip^.X div 3) - 1 do
          begin
            PPixelYC(DestLine)^ := PYC^;
            Inc(DestLine, fpip^.YCSize);
            Inc(PYC, 3);
          end;
          Inc(SrcLine, LineSize * 3);
        end;
        Put(fpip^.Frame + 1, Width * (fpip^.Y div 3) + SizeOf(TViewHeader));
      end;
      3:
      begin
        Width := (fpip^.X div 4) * fpip^.YCSize;
        for Y := 0 to (fpip^.Y div 4) - 1 do
        begin
          PYC := PPixelYC(SrcLine);
          for X := 0 to (fpip^.X div 4) - 1 do
          begin
            PPixelYC(DestLine)^ := PYC^;
            Inc(DestLine, fpip^.YCSize);
            Inc(PYC, 4);
          end;
          Inc(SrcLine, LineSize * 4);
        end;
        Put(fpip^.Frame + 1, Width * (fpip^.Y div 4) + SizeOf(TViewHeader));
      end;
      4:
      begin
        Width := (fpip^.X div 5) * fpip^.YCSize;
        for Y := 0 to (fpip^.Y div 5) - 1 do
        begin
          PYC := PPixelYC(SrcLine);
          for X := 0 to (fpip^.X div 5) - 1 do
          begin
            PPixelYC(DestLine)^ := PYC^;
            Inc(DestLine, fpip^.YCSize);
            Inc(PYC, 5);
          end;
          Inc(SrcLine, LineSize * 5);
        end;
        Put(fpip^.Frame + 1, Width * (fpip^.Y div 5) + SizeOf(TViewHeader));
      end;
    end;

    if GetTickCount() > FCacheSizeUpdatedAt + 500 then
    begin
      FCacheSizeUpdatedAt := GetTickCount();
      UpdateCacheSize();
    end;

    Exit;
  end;
  if not FPlaying then Exit;
  try
    Len := Get(fpip^.Frame + 1);
    if (Len > SizeOf(TViewHeader)) and (FMappedViewHeader^.C = fpip^.YCSize) then
    begin
      fpip^.X := FMappedViewHeader^.A;
      fpip^.Y := FMappedViewHeader^.B;
      SrcLine := FMappedViewData;
      DestLine := fpip^.YCPEdit;
      LineSize := fpip^.LineSize;
      case FMappedViewHeader^.D of
        0:
        begin // Full
          Width := fpip^.X * fpip^.YCSize;
          for Y := 0 to fpip^.Y - 1 do
          begin
            Move(SrcLine^, DestLine^, Width);
            Inc(DestLine, LineSize);
            Inc(SrcLine, Width);
          end;
        end;
        1:
        begin // 1/4
          Width := (fpip^.X div 2) * fpip^.YCSize;
          for Y := 0 to (fpip^.Y div 2) - 1 do
          begin
            PYC := PPixelYC(DestLine);
            for X := 0 to (fpip^.X div 2) - 1 do
            begin
              PYC^ := PPixelYC(SrcLine)^;
              Inc(PYC);
              PYC^ := PPixelYC(SrcLine)^;
              Inc(PYC);
              Inc(SrcLine, SizeOf(TPixelYC));
            end;
            Inc(DestLine, LineSize);
            Move((DestLine - LineSize)^, DestLine^, LineSize);
            Inc(DestLine, LineSize);
          end;
        end;
        2:
        begin // 1/9
          Width := (fpip^.X div 3) * fpip^.YCSize;
          for Y := 0 to (fpip^.Y div 3) - 1 do
          begin
            PYC := PPixelYC(DestLine);
            for X := 0 to (fpip^.X div 3) - 1 do
            begin
              PYC^ := PPixelYC(SrcLine)^;
              Inc(PYC);
              PYC^ := PPixelYC(SrcLine)^;
              Inc(PYC);
              PYC^ := PPixelYC(SrcLine)^;
              Inc(PYC);
              Inc(SrcLine, SizeOf(TPixelYC));
            end;
            Inc(DestLine, LineSize);
            Move((DestLine - LineSize)^, DestLine^, LineSize);
            Inc(DestLine, LineSize);
            Move((DestLine - LineSize)^, DestLine^, LineSize);
            Inc(DestLine, LineSize);
          end;
        end;
        3:
        begin // 1/16
          Width := (fpip^.X div 4) * fpip^.YCSize;
          for Y := 0 to (fpip^.Y div 4) - 1 do
          begin
            PYC := PPixelYC(DestLine);
            for X := 0 to (fpip^.X div 4) - 1 do
            begin
              PYC^ := PPixelYC(SrcLine)^;
              Inc(PYC);
              PYC^ := PPixelYC(SrcLine)^;
              Inc(PYC);
              PYC^ := PPixelYC(SrcLine)^;
              Inc(PYC);
              PYC^ := PPixelYC(SrcLine)^;
              Inc(PYC);
              Inc(SrcLine, SizeOf(TPixelYC));
            end;
            Inc(DestLine, LineSize);
            Move((DestLine - LineSize)^, DestLine^, LineSize);
            Inc(DestLine, LineSize);
            Move((DestLine - LineSize)^, DestLine^, LineSize);
            Inc(DestLine, LineSize);
            Move((DestLine - LineSize)^, DestLine^, LineSize);
            Inc(DestLine, LineSize);
          end;
        end;
        4:
        begin // 1/25
          Width := (fpip^.X div 5) * fpip^.YCSize;
          for Y := 0 to (fpip^.Y div 5) - 1 do
          begin
            PYC := PPixelYC(DestLine);
            for X := 0 to (fpip^.X div 5) - 1 do
            begin
              PYC^ := PPixelYC(SrcLine)^;
              Inc(PYC);
              PYC^ := PPixelYC(SrcLine)^;
              Inc(PYC);
              PYC^ := PPixelYC(SrcLine)^;
              Inc(PYC);
              PYC^ := PPixelYC(SrcLine)^;
              Inc(PYC);
              PYC^ := PPixelYC(SrcLine)^;
              Inc(PYC);
              Inc(SrcLine, SizeOf(TPixelYC));
            end;
            Inc(DestLine, LineSize);
            Move((DestLine - LineSize)^, DestLine^, LineSize);
            Inc(DestLine, LineSize);
            Move((DestLine - LineSize)^, DestLine^, LineSize);
            Inc(DestLine, LineSize);
            Move((DestLine - LineSize)^, DestLine^, LineSize);
            Inc(DestLine, LineSize);
            Move((DestLine - LineSize)^, DestLine^, LineSize);
            Inc(DestLine, LineSize);
          end;
        end;
      end;
    end
    else
    begin
      FillChar(fpip^.YCPEdit^, fpip^.LineSize * fpip^.Y, 0);
      S := Format('Frame %d: no cache', [fpip^.Frame]);
      fp^.ExFunc^.DrawText(fpip^.YCPEdit, 0, 0, PChar(S), 255, 255,
        255, 0, 0, nil, nil);
    end;
  except
    on E: Exception do
      MessageBoxW(FWindow, PWideChar(
        WideString('処理中にエラーが発生しました。'#13#10#13#10 +
        E.Message)),
        PluginName, MB_ICONERROR);
  end;
end;

function TRamPreview.FilterAudioProc(fp: PFilter; fpip: PFilterProcInfo): boolean;
var
  Len: integer;
begin
  Result := True;
  if FCapturing then begin
    Len := fpip^.AudioCh * fpip^.AudioN * SizeOf(smallint);
    FMappedViewHeader^.A := fpip^.AudioN;
    FMappedViewHeader^.B := fpip^.AudioCh;
    Move(fpip^.AudioP^, FMappedViewData^, Len);
    Put(-fpip^.Frame - 1, Len + SizeOf(TViewHeader));
    Exit;
  end;
  if not FPlaying then Exit;
  try
    Len := Get(-fpip^.Frame - 1);
    if (Len > SizeOf(TViewHeader)) and (FMappedViewHeader^.A = fpip^.AudioN) and
      (FMappedViewHeader^.B = fpip^.AudioCh) then
      Move(FMappedViewData^, fpip^.AudioP^, fpip^.AudioCh *
        fpip^.AudioN * SizeOf(smallint))
    else
      FillChar(fpip^.AudioP^, fpip^.AudioCh *
        fpip^.AudioN * SizeOf(smallint), 0);
  except
    on E: Exception do
      MessageBoxW(FWindow, PWideChar(
        WideString('処理中にエラーが発生しました。'#13#10#13#10 +
        WideString(E.Message))),
        PluginName, MB_ICONERROR);
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
  FRemoteProcess := TProcess.Create(nil);
  ws := GetDLLName();
  ws[Length(ws) - 2] := 'e';
  ws[Length(ws) - 1] := 'x';
  ws[Length(ws) - 0] := 'e';
  FRemoteProcess.Executable := ws;
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

procedure TRamPreview.SetPlaying(AValue: boolean);
var
  I: integer;
begin
  if FPlaying = AValue then Exit;
  FPlaying := AValue;
  PlayModeComboBox := AValue;

  if not FPlaying then begin
    for I := Low(FFilters) to High(FFilters) do
        FFilters[I]^.FuncProc := FOrigProcs[I];
    Exit;
  end;

  SetLength(FOrigProcs, Length(FFilters));
  for I := Low(FFilters) to High(FFilters) do begin
    FOrigProcs[I] := FFilters[I]^.FuncProc;
    if (FOrigProcs[I] <> nil) and (FOrigProcs[I] <> @FilterFuncProc) and (FOrigProcs[I] <> @FilterAudioFuncProc) then
      FFilters[I]^.FuncProc := @DummyFuncProc;
  end;
end;

procedure TRamPreview.SetPlayModeComboBox(AValue: boolean);
const
  V: array[boolean] of WPARAM = (0, 1);
begin
  SendMessageW(FPlayModeList, LB_SETCURSEL, V[AValue], 0);
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

procedure TRamPreview.CaptureRange(Edit: Pointer; Filter: PFilter);
var
  StartFrame, EndFrame: integer;
  FI: TFileInfo;
begin
  FillChar(FI, SizeOf(TFileInfo), 0);
  if Filter^.ExFunc^.GetFileInfo(Edit, @FI) = AVIUTL_FALSE then
    raise Exception.Create(
      '編集中のファイルの情報取得に失敗しました');
  if (FI.Width = 0) or (FI.Height = 0) then
    Exit;
  if Filter^.ExFunc^.GetSelectFrame(Edit, StartFrame, EndFrame) = AVIUTL_FALSE then
    raise Exception.Create('選択範囲を取得できませんでした');

  Playing := False;
  FCapturing := True;
  Filter^.ExFunc^.EditOutput(Edit, 'RAM', EDIT_OUTPUT_FLAG_NO_DIALOG, OutputPluginNameANSI);
  FCapturing := False;
  Playing := True;

  UpdateCacheSize();
  Filter^.ExFunc^.SetFrame(Edit, StartFrame);
end;

procedure TRamPreview.ClearCache(Edit: Pointer; Filter: PFilter);
begin
  if not FRemoteProcess.Running then
    Exit;
  Clear();
  UpdateCacheSize();
  Playing := False;
end;

procedure TRamPreview.UpdateCacheSize();
begin
  if FRemoteProcess.Running then
    SetWindowTextW(FCacheSizeLabel,
      PWideChar(WideString(BytesToStr(Stat()))))
  else
    SetWindowTextW(FCacheSizeLabel, '');
  EnableWindow(FCacheSizeLabel, True);
end;

procedure TRamPreview.ClearStorageCache(Edit: Pointer; Filter: PFilter);
begin
  if not FRemoteProcess.Running then
    Exit;
  ClearS();
  UpdateCacheSize();
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
        'Please check whether the antivirus software is blocking program execution.'#13#10
        + 'アンチウィルスソフトがプログラム実行を阻害していないか確認してください。');
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

  EnterCS('FMO ');
  try
    FRemoteProcess.Input.WriteBuffer('FMO ', 4);
    WriteUInt64(FRemoteProcess.Input, h);
  finally
    LeaveCS('FMO ');
  end;
  FReceiver.WaitResult();
  FReceiver.Done();
end;

function TRamPreview.GetResolution: integer;
begin
  Result := SendMessageW(FResolution, CB_GETCURSEL, 0, 0);
end;

function TRamPreview.GetEntryExtram: PFilterDLL;
begin
  Result := @FEntryExtram;
end;

function TRamPreview.GetPlayModeComboBox: boolean;
begin
  case SendMessageW(FPlayModeList, LB_GETCURSEL, 0, 0) of
    0: Result := False;
    1: Result := True;
    else
      raise Exception.Create('unexpected play mode value');
  end;
end;

initialization
  RamPreview := TRamPreview.Create();
  RamPreview.Entry^.FuncInit := @FilterFuncInit;
  RamPreview.Entry^.FuncExit := @FilterFuncExit;
  RamPreview.Entry^.FuncWndProc := @FilterFuncWndProc;
  RamPreview.Entry^.FuncProc := @FilterFuncProc;
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
  Storage.Put := @StoragePut;
  Storage.Del := @StorageDel;

finalization
  RamPreview.Free();

end.

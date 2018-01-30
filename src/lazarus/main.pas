unit Main;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Windows, SysUtils, Classes, Process, Remote, AviUtl, StorageAPI;

type
  TDataHeader = record
    A, B, C, D: integer;
  end;
  PDataHeader = ^TDataHeader;

  { TRamPreview }

  TRamPreview = class
  private
    FRemoteProcess: TProcess;
    FReceiver: TReceiver;
    FCS: TRTLCriticalSection;

    FEntry, FEntryAudio: TFilterDLL;
    FExEdit, FExEditAudio: PFilter;
    FWindow, FFont, FPlayModeList, FCacheCreateButton, FCacheClearButton,
    FCacheSizeLabel, FStatusLabel: THandle;
    FFontHeight: integer;
    FOriginalExEditProc, FOriginalExEditAudioProc: TProcFunc;

    FMappedFile: THandle;
    FMappedViewHeader: PDataHeader;
    FMappedViewData: PByte;
    FMappedViewSize: integer;

    FCapturing: boolean;
    FPlaying: boolean;
    FStartFrame: integer;
    FCurrentFrame: integer;
    FEndFrame: integer;
    FTimer: THandle;
    FCacheWidth, FCacheHeight: integer;
    FAudioChannels: integer;
    FAudioBuffer: Pointer;
    FCacheSizeUpdatedAt: Cardinal;

    procedure PrepareIPC();
    procedure OnRequest(Sender: TObject; const Command: UTF8String);
    procedure EnterCS(CommandName: string);
    procedure LeaveCS(CommandName: string);

    procedure Clear();
    function Stat(): QWord;
    procedure Put(const Key: integer; const Size: integer);
    function Get(const Key: integer): integer;

    procedure ClearS();
    procedure PutS(const Key: string; const Size: integer);
    function GetS(const Key: string): integer;
    function DelS(const Key: string): integer;

    procedure CaptureRange(Edit: Pointer; Filter: PFilter);
    procedure Capturing(Edit: Pointer; Filter: PFilter);
    procedure ClearCache(Edit: Pointer; Filter: PFilter);
    procedure UpdateCacheSize(Edit: Pointer; Filter: PFilter);

    procedure UpdateMode();
    function GetPlayModeComboBox: boolean;
    procedure SetPlayModeComboBox(AValue: boolean);
    function GetEntry: PFilterDLL;
    function GetEntryAudio: PFilterDLL;
    function InitProc(Filter: PFilter): boolean;
    function ExitProc(Filter: PFilter): boolean;
    function MainProc(Window: HWND; Message: UINT; WP: WPARAM;
      LP: LPARAM; Edit: Pointer; Filter: PFilter): integer;
    function FilterProc(fp: PFilter; fpip: PFilterProcInfo): boolean;
    function FilterAudioProc(fp: PFilter; fpip: PFilterProcInfo): boolean;
    procedure UpdateStatus(ws: WideString);
  public
    constructor Create();
    destructor Destroy(); override;
    property Entry: PFilterDLL read GetEntry;
    property EntryAudio: PFilterDLL read GetEntryAudio;
    property PlayModeComboBox: boolean read GetPlayModeComboBox write SetPlayModeComboBox;
  end;

function GetFilterTableList(): PPFilterDLL; stdcall;
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
  ExEditNameANSI = #$8a#$67#$92#$a3#$95#$d2#$8f#$57; // '拡張編集'
  ExEditAudioNameANSI = #$8a#$67#$92#$a3#$95#$d2#$8f#$57#$28#$89#$b9#$90#$ba#$29;
// '拡張編集(音声)'

const
  BoolConv: array[boolean] of AviUtlBool = (AVIUTL_FALSE, AVIUTL_TRUE);
  CreateButtonCaption: array[boolean] of PWideChar =
    ('選択範囲からキャッシュ作成', '中止');

var
  FilterDLLList: array of PFilterDLL;
  Storage: TStorageAPI;

function GetFilterTableList(): PPFilterDLL; stdcall;
begin
  Result := @FilterDLLList[0];
end;

function StorageGetMaxBufferSize(): integer; cdecl;
begin
  Result := RamPreview.FMappedViewSize;
end;

function StorageGet(Key: PChar; Dest: Pointer): integer; cdecl;
begin
  Result := RamPreview.GetS(Key);
  Move(RamPreview.FMappedViewData^, Dest^, Result);
end;

function StoragePut(Key: PChar; Src: Pointer; Len: integer): integer; cdecl;
begin
  if Len > RamPreview.FMappedViewSize then begin
    Result := 0;
    Exit;
  end;
  if Len = 0 then begin
    RamPreview.DelS(Key);
    Result := 0;
    Exit;
  end;
  Move(Src^, RamPreview.FMappedViewData^, Len);
  RamPreview.PutS(Key, Len);
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

{ TRamPreview }

function TRamPreview.MainProc(Window: HWND; Message: UINT; WP: WPARAM;
  LP: LPARAM; Edit: Pointer; Filter: PFilter): integer;
const
  ExEditVersion = ' version 0.92 ';
  ToggleModeCaption = #$93#$ae#$8d#$ec#$83#$82#$81#$5b#$83#$68#$90#$d8#$82#$e8#$91#$d6#$82#$a6#$81#$69#$92#$ca#$8f#$ed#$81#$5e#$52#$41#$4d#$83#$76#$83#$8c#$83#$72#$83#$85#$81#$5b#$81#$6a; // 動作モード切り替え（通常／RAMプレビュー）
  CaptureCaption = #$91#$49#$91#$f0#$94#$cd#$88#$cd#$82#$a9#$82#$e7#$83#$4c#$83#$83#$83#$62#$83#$56#$83#$85#$8d#$ec#$90#$ac#$81#$5E#$92#$86#$8E#$7E; // 選択範囲からキャッシュ作成／中止
  ClearCacheCaption = #$83#$4c#$83#$83#$83#$62#$83#$56#$83#$85#$8f#$c1#$8b#$8e; // キャッシュ消去
var
  Y, Height: integer;
  NCM: TNonClientMetrics;
  DC: THandle;
  sinfo: TSysInfo;
  fp: PFilter;
begin
  case Message of
    WM_FILTER_INIT:
    begin
      FWindow := Window;
      if Filter^.ExFunc^.GetSysInfo(Edit, @sinfo) <> AVIUTL_FALSE then
      begin
        for Y := 0 to sinfo.FilterN - 1 do
        begin
          fp := Filter^.ExFunc^.GetFilterP(Y);
          if fp = nil then
            continue;
          if fp^.Name = ExEditNameANSI then
            FExEdit := fp
          else if fp^.Name = ExEditAudioNameANSI then
            FExEditAudio := fp
          else
            continue;
        end;
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
      FCacheCreateButton := CreateWindowW('BUTTON', CreateButtonCaption[False],
        WS_CHILD or WS_VISIBLE, 8, Y, 160, Height, Window, 2, Filter^.DLLHInst, nil);
      SendMessageW(FCacheCreateButton, WM_SETFONT, WPARAM(FFont), 0);
      Inc(Y, Height);

      Height := FFontHeight;
      FStatusLabel := CreateWindowW('STATIC', PWideChar(''),
        WS_CHILD or WS_VISIBLE or ES_RIGHT, 8, Y, 160, Height,
        Window, 3, Filter^.DLLHInst, nil);
      SendMessageW(FStatusLabel, WM_SETFONT, WPARAM(FFont), 0);
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
        if (not Assigned(FExEdit)) or (not Assigned(FExEditAudio)) then
          raise Exception.Create(
            '拡張編集プラグインが見つかりません。');
        if StrPos(FExEdit^.Information, ExEditVersion) = nil then
          raise Exception.Create(PluginName + ' を使うには拡張編集' +
            ExEditVersion + 'が必要です。');
        if sinfo.Build < 10000 then
          raise Exception.Create(PluginName +
            ' を使うには AviUtl version 1.00 以降が必要です。');

        FOriginalExEditProc := FExEdit^.FuncProc;
        FOriginalExEditAudioProc := FExEditAudio^.FuncProc;

        Filter^.ExFunc^.AddMenuItem(Filter, CaptureCaption, Window, 100, VK_R, ADD_MENU_ITEM_FLAG_KEY_CTRL);
        Filter^.ExFunc^.AddMenuItem(Filter, ClearCacheCaption, Window, 101, VK_E, ADD_MENU_ITEM_FLAG_KEY_CTRL);
        Filter^.ExFunc^.AddMenuItem(Filter, ToggleModeCaption, Window, 102, VK_R, ADD_MENU_ITEM_FLAG_KEY_CTRL or ADD_MENU_ITEM_FLAG_KEY_SHIFT);
      except
        on E: Exception do
        begin
          SetWindowTextW(Window,
            PWideChar(WideString(PluginName +
            ' - 初期化に失敗したため使用できません')));
          MessageBoxW(FExEdit^.Hwnd,
            PWideChar(PluginName +
            ' の初期化中にエラーが発生しました。'#13#10#13#10 +
            WideString(E.Message)),
            PWideChar('初期化エラー - ' + PluginName), MB_ICONERROR);
        end;
      end;
      Result := 1;
    end;
    WM_FILTER_FILE_OPEN:
    begin
      ClearCache(Edit, Filter);
    end;
    WM_FILTER_FILE_CLOSE:
    begin
      ClearCache(Edit, Filter);
    end;
    WM_FILTER_EXIT:
    begin
      DeleteObject(FFont);
      FFont := 0;
    end;
    WM_COMMAND:
    begin
      try
        case LOWORD(WP) of
          1:
          begin
            if HIWORD(WP) = LBN_SELCHANGE then
              FPlaying := PlayModeComboBox;
            UpdateMode();
          end;
          2: CaptureRange(Edit, Filter);
          4: ClearCache(Edit, Filter);
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
          102: PlayModeComboBox := not PlayModeComboBox;
        end;
      except
        on E: Exception do
          MessageBoxW(FWindow, PWideChar(
            WideString('処理中にエラーが発生しました。'#13#10#13#10 +
            WideString(E.Message))),
            PluginName, MB_ICONERROR);
      end;

    end;
    WM_TIMER:
    begin
      try
        case WP of
          3:
          begin
            KillTimer(Filter^.Hwnd, FTimer);
            Capturing(Edit, Filter);
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
    else
    begin
      Result := 0;
    end;
  end;
end;

function TRamPreview.FilterProc(fp: PFilter; fpip: PFilterProcInfo): boolean;
var
  S: string;
  Y, LineSize, Width, Len: integer;
  SrcLine, DestLine: PByte;
begin
  Result := True;
  if FCapturing or (not FPlaying) or (FStartFrame <> FEndFrame) then Exit;
  try
    Len := Get(fpip^.Frame);
    if (Len > SizeOf(TDataHeader)) and (FMappedViewHeader^.C = fpip^.YCSize) then
    begin
      fpip^.X := FMappedViewHeader^.A;
      fpip^.Y := FMappedViewHeader^.B;
      Width := fpip^.X * fpip^.YCSize;
      SrcLine := Self.FMappedViewData;
      DestLine := fpip^.YCPEdit;
      LineSize := fpip^.LineSize;
      for Y := 0 to fpip^.Y - 1 do
      begin
        Move(SrcLine^, DestLine^, Width);
        Inc(SrcLine, Width);
        Inc(DestLine, LineSize);
      end;
    end
    else
    begin
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
  if FCapturing or (not FPlaying) or (FStartFrame <> FEndFrame) then Exit;
  try
    Len := Get(-fpip^.Frame);
    if (Len > SizeOf(TDataHeader)) and (FMappedViewHeader^.A = fpip^.AudioN) and
      (FMappedViewHeader^.B = fpip^.AudioCh) then
      Move(FMappedViewData^, fpip^.AudioP^, fpip^.AudioCh *
        fpip^.AudioN * SizeOf(smallint));
  except
    on E: Exception do
      MessageBoxW(FWindow, PWideChar(
        WideString('処理中にエラーが発生しました。'#13#10#13#10 +
        WideString(E.Message))),
        PluginName, MB_ICONERROR);
  end;
end;

procedure TRamPreview.UpdateStatus(ws: WideString);
begin
  SetWindowTextW(FStatusLabel, PWideChar(ws));
end;

function TRamPreview.InitProc(Filter: PFilter): boolean;
var
  SI: TSysInfo;
  W, H: integer;
  P: Pointer;
begin
  Result := True;
  try
    if Filter^.ExFunc^.GetSysInfo(nil, @SI) = AVIUTL_FALSE then
      raise Exception.Create('AviUtl のシステム情報取得に失敗しました');

    W := Max(SI.MaxW, 1280);
    H := Max(SI.MaxH, 720);
    FMappedViewSize := W * H * SizeOf(TPixelYC);
    FMappedFile := CreateFileMappingW(INVALID_HANDLE_VALUE, nil,
      PAGE_READWRITE, 0, DWORD((FMappedViewSize + SizeOf(TDataHeader)) and $ffffffff), nil);
    if FMappedFile = 0 then
      raise Exception.Create('CreateFileMapping に失敗しました');

    P := MapViewOfFile(FMappedFile, FILE_MAP_WRITE, 0, 0, 0);
    if P = nil then
      raise Exception.Create('MapViewOfFile に失敗しました');
    FMappedViewHeader := P;
    FMappedViewData := P;
    Inc(FMappedViewData, SizeOf(TDataHeader));
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
end;

destructor TRamPreview.Destroy();
begin
  if FAudioBuffer <> nil then
    FreeMem(FAudioBuffer);

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

procedure TRamPreview.SetPlayModeComboBox(AValue: boolean);
const
  V: array[boolean] of WPARAM = (0, 1);
begin
  SendMessageW(FPlayModeList, LB_SETCURSEL, V[AValue], 0);
  if FPlaying = AValue then Exit;
  FPlaying := AValue;
  UpdateMode();
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
  StartFrame, EndFrame, Size: integer;
  FI: TFileInfo;
begin
  if FStartFrame <> FEndFrame then
  begin
    FCurrentFrame := FEndFrame;
    Exit;
  end;
  FillChar(FI, SizeOf(TFileInfo), 0);
  if Filter^.ExFunc^.GetFileInfo(Edit, @FI) = AVIUTL_FALSE then
    raise Exception.Create(
      '編集中のファイルの情報取得に失敗しました');
  if (FI.Width = 0)or(FI.Height = 0) then Exit;
  if Filter^.ExFunc^.GetSelectFrame(Edit, StartFrame, EndFrame) = AVIUTL_FALSE then
    raise Exception.Create('選択範囲を取得できませんでした');

  if FAudioBuffer <> nil then
  begin
    FreeMem(FAudioBuffer);
    FAudioBuffer := nil;
  end;
  FAudioChannels := FI.AudioCh;
  Size := Filter^.ExFunc^.GetAudioFiltered(Edit, StartFrame, nil) *
    FAudioChannels * SizeOf(smallint);
  FAudioBuffer := GetMem(Size * 3);

  FCacheWidth := FI.Width;
  FCacheHeight := FI.Height;
  FStartFrame := StartFrame;
  FEndFrame := EndFrame;
  FCurrentFrame := StartFrame;

  FTimer := SetTimer(Filter^.Hwnd, 3, 0, nil);
  SetWindowTextW(FCacheCreateButton, CreateButtonCaption[True]);
end;

procedure TRamPreview.Capturing(Edit: Pointer; Filter: PFilter);
var
  Samples, W, H, Y, SrcW, DestW: integer;
  Src, Dest: PByte;
begin
  try
    FCapturing := True;
    UpdateMode();
    try
      Samples := Filter^.ExFunc^.GetAudioFiltering(Filter, Edit,
        FCurrentFrame, FAudioBuffer);
      if Samples > 0 then
      begin
        W := FAudioChannels * Samples * SizeOf(smallint);
        FMappedViewHeader^.A := Samples;
        FMappedViewHeader^.B := FAudioChannels;
        Move(FAudioBuffer^, FMappedViewData^, W);
        Put(-FCurrentFrame, W + SizeOf(TDataHeader));
      end;

      if Filter^.ExFunc^.SetYCPFilteringCacheSize(Filter, FCacheWidth,
        FCacheHeight, 1, 0) = AVIUTL_FALSE then
        raise Exception.Create('SetYCPFilteringCacheSize に失敗しました');

      Src := PByte(Filter^.ExFunc^.GetYCPFilteringCacheEx(Filter, Edit, FCurrentFrame, @W, @H));
      if Src <> nil then
      begin
        SrcW := FCacheWidth * SizeOf(TPixelYC);
        DestW := W * SizeOf(TPixelYC);
        Dest := FMappedViewData;

        FMappedViewHeader^.A := W;
        FMappedViewHeader^.B := H;
        FMappedViewHeader^.C := SizeOf(TPixelYC);
        for Y := 0 to H - 1 do
        begin
          Move(Src^, Dest^, DestW);
          Inc(Src, SrcW);
          Inc(Dest, DestW);
        end;
        Put(FCurrentFrame, DestW * H + SizeOf(TDataHeader));
      end;
    finally
      FCapturing := False;
      UpdateMode();
    end;

    if FCurrentFrame < FEndFrame then
    begin
      if GetTickCount() > FCacheSizeUpdatedAt + 500 then begin
        FCacheSizeUpdatedAt := GetTickCount();
        UpdateCacheSize(Edit, Filter);
      end;

      Filter^.ExFunc^.SetFrame(Edit, FCurrentFrame);
      Inc(FCurrentFrame);
      UpdateStatus(WideString(Format('%d / %d', [FCurrentFrame -
        FStartFrame, FEndFrame - FStartFrame])));
      FTimer := SetTimer(Filter^.Hwnd, 3, 0, nil);
    end
    else
    begin
      Filter^.ExFunc^.SetFrame(Edit, FStartFrame);
      SetWindowTextW(FCacheCreateButton, CreateButtonCaption[False]);
      UpdateCacheSize(Edit, Filter);
      UpdateStatus('');
      FStartFrame := 0;
      FEndFrame := 0;
      FCurrentFrame := 0;
      PlayModeComboBox := True;
      UpdateMode();
    end;
  except
    on E: Exception do begin
      MessageBoxW(FWindow, PWideChar(
        WideString('処理中にエラーが発生しました。'#13#10#13#10 +
        WideString(E.Message))),
        PluginName, MB_ICONERROR);

      SetWindowTextW(FCacheCreateButton, CreateButtonCaption[False]);
      UpdateStatus('');
      Filter^.ExFunc^.SetFrame(Edit, FStartFrame);
      FStartFrame := 0;
      FEndFrame := 0;
      FCurrentFrame := 0;
      UpdateMode();
    end;
  end;
end;

procedure TRamPreview.ClearCache(Edit: Pointer; Filter: PFilter);
begin
  if not FRemoteProcess.Running then Exit;
  Clear();
  UpdateCacheSize(Edit, Filter);
  PlayModeComboBox := False;
end;

procedure TRamPreview.UpdateCacheSize(Edit: Pointer; Filter: PFilter);
begin
  if FRemoteProcess.Running then
    SetWindowTextW(FCacheSizeLabel,
      PWideChar(WideString(BytesToStr(Stat()))))
  else
    SetWindowTextW(FCacheSizeLabel, '');
end;

procedure TRamPreview.UpdateMode();
begin
  if FPlaying and (not FCapturing) and (FStartFrame = FEndFrame) then begin
    FExEdit^.FuncProc := @DummyFuncProc;
    FExEditAudio^.FuncProc := @DummyFuncProc;
  end else begin
    FExEdit^.FuncProc := FOriginalExEditProc;
    FExEditAudio^.FuncProc:= FOriginalExEditAudioProc;
  end;
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

function TRamPreview.GetPlayModeComboBox: boolean;
begin
  case SendMessageW(FPlayModeList, LB_GETCURSEL, 0, 0) of
    0: Result := False;
    1: Result := True;
    else raise Exception.Create('unexpected play mode value');
  end;
end;

initialization
  RamPreview := TRamPreview.Create();
  RamPreview.Entry^.FuncInit := @FilterFuncInit;
  RamPreview.Entry^.FuncExit := @FilterFuncExit;
  RamPreview.Entry^.FuncWndProc := @FilterFuncWndProc;
  RamPreview.Entry^.FuncProc := @FilterFuncProc;
  RamPreview.EntryAudio^.FuncProc := @FilterAudioFuncProc;

  SetLength(FilterDLLList, 3);
  FilterDLLList[0] := RamPreview.Entry;
  FilterDLLList[1] := RamPreview.EntryAudio;
  FilterDLLList[2] := nil;

  Storage.GetMaxBufferSize := @StorageGetMaxBufferSize;
  Storage.Get := @StorageGet;
  Storage.Put := @StoragePut;
  Storage.Del := @StorageDel;

finalization
  RamPreview.Free();

end.

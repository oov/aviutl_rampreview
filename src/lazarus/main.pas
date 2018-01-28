unit Main;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Windows, SysUtils, Classes, Process, Remote, AviUtl;

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

    FCapturing: boolean;
    FPlaying: boolean;
    FStartFrame: integer;
    FCurrentFrame: integer;
    FEndFrame: integer;
    FTimer: THandle;
    FCacheWidth, FCacheHeight: integer;
    FAudioChannels: integer;
    FAudioBuffer: Pointer;

    procedure PrepareIPC();
    procedure OnRequest(Sender: TObject; const Command: UTF8String);
    procedure EnterCS(CommandName: string);
    procedure LeaveCS(CommandName: string);

    procedure Clear();
    function Stat(): QWord;
    procedure Put(const Key: integer; const Size: integer);
    function Get(const Key: integer): integer;

    procedure CaptureRange(Edit: Pointer; Filter: PFilter);
    procedure Capturing(Edit: Pointer; Filter: PFilter);
    procedure UpdateCacheSize();

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
  end;

function GetFilterTableList(): PPFilterDLL; stdcall;

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

function GetFilterTableList(): PPFilterDLL; stdcall;
begin
  Result := @FilterDLLList[0];
end;

function FilterFuncInit(fp: PFilter): AviUtlBool; cdecl;
begin
  Result := BoolConv[RamPreview.InitProc(fp)];
end;

function ExEditDummyFuncProc(fp: PFilter; fpip: PFilterProcInfo): AviUtlBool; cdecl;
begin
  if RamPreview.FPlaying and (not RamPreview.FCapturing) and
    (RamPreview.FStartFrame = RamPreview.FEndFrame) then
    Result := BoolConv[True]
  else
    Result := RamPreview.FOriginalExEditProc(fp, fpip);
end;

function ExEditAudioDummyFuncProc(fp: PFilter; fpip: PFilterProcInfo): AviUtlBool; cdecl;
begin
  if RamPreview.FPlaying and (not RamPreview.FCapturing) and
    (RamPreview.FStartFrame = RamPreview.FEndFrame) then
    Result := BoolConv[True]
  else
    Result := RamPreview.FOriginalExEditAudioProc(fp, fpip);
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
        FExEdit^.FuncProc := @ExEditDummyFuncProc;
        FOriginalExEditAudioProc := FExEditAudio^.FuncProc;
        FExEditAudio^.FuncProc := @ExEditAudioDummyFuncProc;

        SetTimer(Filter^.Hwnd, 100, 3000, nil);
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
      if FRemoteProcess.Running then Clear();
    end;
    WM_FILTER_FILE_CLOSE:
    begin
      if FRemoteProcess.Running then Clear();
    end;
    WM_FILTER_EXIT:
    begin
      DeleteObject(FFont);
      FFont := 0;
    end;
    WM_COMMAND:
    begin
      case LOWORD(WP) of
        1:
        begin
          if HIWORD(WP) = LBN_SELCHANGE then
            case SendMessageW(FPlayModeList, LB_GETCURSEL, 0, 0) of
              0: FPlaying := False;
              1: FPlaying := True;
            end;
        end;
        2: CaptureRange(Edit, Filter);
        4: if FRemoteProcess.Running then Clear();
      end;
    end;
    WM_TIMER:
    begin
      case WP of
        3:
        begin
          KillTimer(Filter^.Hwnd, FTimer);
          Capturing(Edit, Filter);
        end;
        100:
        begin
          UpdateCacheSize();
        end;
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
  try
    if FPlaying and (not FCapturing) and (RamPreview.FStartFrame =
      RamPreview.FEndFrame) then
    begin
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
  len: integer;
begin
  Result := True;
  try
    if FPlaying and (not FCapturing) and (RamPreview.FStartFrame =
      RamPreview.FEndFrame) then
    begin
      len := Get(-fpip^.Frame);
      if (len > SizeOf(TDataHeader)) and (FMappedViewHeader^.A = fpip^.AudioN) and
        (FMappedViewHeader^.B = fpip^.AudioCh) then
        Move(FMappedViewData^, fpip^.AudioP^, fpip^.AudioCh *
          fpip^.AudioN * SizeOf(smallint));
    end;
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
  Size: QWord;
  P: Pointer;
begin
  Result := True;
  try
    if Filter^.ExFunc^.GetSysInfo(nil, @SI) = AVIUTL_FALSE then
      raise Exception.Create('AviUtl のシステム情報取得に失敗しました');

    W := Max(SI.MaxW, 1280);
    H := Max(SI.MaxH, 720);
    Size := W * H * SizeOf(TPixelYC) + SizeOf(TDataHeader);
    FMappedFile := CreateFileMappingW(INVALID_HANDLE_VALUE, nil,
      PAGE_READWRITE, DWORD(Size shr 32), DWORD(Size and $ffffffff), nil);
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
  try
    EnterCS('CLR ');
    try
      PrepareIPC();
      FRemoteProcess.Input.WriteBuffer('CLR ', 4);
    finally
      LeaveCS('CLR ');
    end;
    FReceiver.WaitResult();
    FReceiver.Done();
  except
    on E: Exception do
      MessageBoxW(FWindow, PWideChar(
        WideString('処理中にエラーが発生しました。'#13#10#13#10 +
        WideString(E.Message))),
        PluginName, MB_ICONERROR);
  end;
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

procedure TRamPreview.CaptureRange(Edit: Pointer; Filter: PFilter);
var
  StartFrame, EndFrame, Size: integer;
  FI: TFileInfo;
begin
  try
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
  except
    on E: Exception do
      MessageBoxW(FWindow, PWideChar(
        WideString('処理中にエラーが発生しました。'#13#10#13#10 +
        WideString(E.Message))),
        PluginName, MB_ICONERROR);
  end;
end;

procedure TRamPreview.Capturing(Edit: Pointer; Filter: PFilter);
var
  Samples, Bytes, W, H: integer;
  Pix: PPixelYC;
begin
  try
    FCapturing := True;
    try
      Samples := Filter^.ExFunc^.GetAudioFiltering(Filter, Edit,
        FCurrentFrame, FAudioBuffer);
      if Samples > 0 then
      begin
        Bytes := FAudioChannels * Samples * SizeOf(smallint);
        FMappedViewHeader^.A := Samples;
        FMappedViewHeader^.B := FAudioChannels;
        Move(FAudioBuffer^, FMappedViewData^, Bytes);
        Put(-FCurrentFrame, Bytes + SizeOf(TDataHeader));
      end;

      if Filter^.ExFunc^.SetYCPFilteringCacheSize(Filter, FCacheWidth,
        FCacheHeight, 1, 0) = AVIUTL_FALSE then
        raise Exception.Create('SetYCPFilteringCacheSize に失敗しました');

      Pix := Filter^.ExFunc^.GetYCPFilteringCacheEx(Filter, Edit, FCurrentFrame, @W, @H);
      if Pix <> nil then
      begin
        Bytes := W * SizeOf(TPixelYC) * H;
        FMappedViewHeader^.A := W;
        FMappedViewHeader^.B := H;
        FMappedViewHeader^.C := SizeOf(TPixelYC);
        Move(Pix^, FMappedViewData^, Bytes);
        Put(FCurrentFrame, Bytes + SizeOf(TDataHeader));
      end;
    finally
      FCapturing := False;
    end;

    Filter^.ExFunc^.SetFrame(Edit, FCurrentFrame);
    if FCurrentFrame < FEndFrame then
    begin
      Inc(FCurrentFrame);
      UpdateStatus(WideString(Format('%d / %d', [FCurrentFrame -
        FStartFrame, FEndFrame - FStartFrame])));
      FTimer := SetTimer(Filter^.Hwnd, 3, 0, nil);
    end
    else
    begin
      SetWindowTextW(FCacheCreateButton, CreateButtonCaption[False]);
      UpdateStatus('');
      Filter^.ExFunc^.SetFrame(Edit, FStartFrame);
      FStartFrame := 0;
      FEndFrame := 0;
      FCurrentFrame := 0;
    end;
  except
    on E: Exception do
      MessageBoxW(FWindow, PWideChar(
        WideString('処理中にエラーが発生しました。'#13#10#13#10 +
        WideString(E.Message))),
        PluginName, MB_ICONERROR);
  end;
end;

procedure TRamPreview.UpdateCacheSize();
begin
  if FRemoteProcess.Running then
    SetWindowTextW(FCacheSizeLabel,
      PWideChar(WideString(BytesToStr(Stat()))))
  else
    SetWindowTextW(FCacheSizeLabel, '');
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

finalization
  RamPreview.Free();

end.

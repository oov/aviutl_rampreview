unit Hook;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

procedure InitHook();
procedure FreeHook();
procedure DisablePlaySound(const b: boolean);
procedure DisableGetSaveFileName(const b: boolean);
procedure DisableMessageBox(const b: boolean);

implementation

uses
  Windows;

type
  LPGETSAVEFILENAMEA = function(var OFN: OPENFILENAME): HResult; stdcall;
  LPPLAYSOUNDA =  function(pszSound: PChar; hMod: HMODULE; fdwSound: DWORD): WINBOOL; stdcall;
  LPMESSAGEBOXA = function(hWnd: HWND; lpText: LPCSTR; lpCaption: LPCSTR;
    uType: UINT): longint; stdcall;

  PImageImportByName = ^TImageImportByName;

  _IMAGE_IMPORT_BY_NAME = packed record
    HInst: word;
    Name: byte;
  end;
  TImageImportByName = _IMAGE_IMPORT_BY_NAME;

  _IMAGE_THUNK_DATA = packed record
    case integer of
      0: (ForwarderString: PByte);
      1: (thFunction: PDWORD);
      2: (Ordinal: DWORD);
      3: (AddressOfData: PImageImportByName);
  end;
  TImageThunkData = _IMAGE_THUNK_DATA;
  IMAGE_THUNK_DATA = _IMAGE_THUNK_DATA;
  PImageThunkData = ^TImageThunkData;

  TCharcteristics = record
    case integer of
      0: (Characteristics: DWORD);
      1: (OriginalFirstThunk: PImageThunkData);
  end;

  PImageImportDescriptor = ^TImageImportDescriptor;

  _IMAGE_IMPORT_DESCRIPTOR = packed record
    c: TCharcteristics;
    TimeDateStamp: DWord;
    ForwarderChain: DWORD;
    Name: DWORD;
    FirstThunk: PImageThunkData;
  end;
  TImageImportDescriptor = _IMAGE_IMPORT_DESCRIPTOR;
  IMAGE_IMPORT_DESCRIPTOR = _IMAGE_IMPORT_DESCRIPTOR;

  TAPIHookEntry = packed record
    ProcName: PChar;
    Module: THandle;
    OriginalProc: Pointer;
    HookProc: Pointer;
  end;

var
  hCOMDLG32DLL, hWINMMDLL: THandle;
  HookEntries: array of TAPIHookEntry;

  pGetSaveFileNameA: LPGETSAVEFILENAMEA;
  pPlaySoundA: LPPLAYSOUNDA;
  pMessageBoxA: LPMESSAGEBOXA;
  SuppressGSFN: boolean;
  SuppressPlaySound: boolean;
  SuppressMSGBOX: boolean;

procedure DisableGetSaveFileName(const b: boolean);
begin
  SuppressGSFN := b;
end;

procedure DisablePlaySound(const b: boolean);
begin
  SuppressPlaySound := b;
end;

procedure DisableMessageBox(const b: boolean);
begin
  SuppressMSGBOX := b;
end;

function MyGetSaveFileNameA(var OFN: OPENFILENAME): HResult; stdcall;
begin
  if SuppressGSFN then
  begin
    Result := 1;
    Exit;
  end;
  Result := pGetSaveFileNameA(OFN);
end;

function MyPlaySoundA(pszSound: PChar; hMod: HMODULE; fdwSound: DWORD): WINBOOL; stdcall;
begin
  if SuppressPlaySound then
  begin
    Result := True;
    Exit;
  end;
  Result := pPlaySoundA(pszSound, hMod, fdwSound);
end;

function MyMessageBoxA(hWnd: HWND; lpText: LPCSTR; lpCaption: LPCSTR;
  uType: UINT): longint; stdcall;
begin
  if SuppressMSGBOX then
  begin
    Result := idOk;
    Exit;
  end;
  Result := pMessageBoxA(hWnd, lpText, lpCaption, uType);
end;

procedure Hook(hModule: THandle; Install: boolean);
var
  i: integer;

  DOSHeader: PImageDosHeader;
  NTHeader: PImageNtHeaders;
  ImageDataDir: PImageDataDirectory;
  PImports: PImageImportDescriptor;
  OldProtect: DWORD;
  PRVA_Import: PImageThunkData;
begin
  DOSHeader := PImageDosHeader(hModule);
  if DOSHeader^.e_magic <> IMAGE_DOS_SIGNATURE then
    Exit;

  NTHeader := PImageNtHeaders(PtrUInt(DOSHeader) + PtrUInt(DOSHeader^._lfanew));
  if NTHeader^.Signature <> IMAGE_NT_SIGNATURE then
    Exit;

  if NTHeader^.OptionalHeader.NumberOfRvaAndSizes <= IMAGE_DIRECTORY_ENTRY_IMPORT then
    Exit;

  ImageDataDir := @NTHeader^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT];
  if ImageDataDir^.VirtualAddress = 0 then
    Exit;

  PImports := PImageImportDescriptor(PtrUInt(hModule) +
    PtrUInt(ImageDataDir^.VirtualAddress));
  if PImports = Pointer(NTHeader) then
    Exit;

  while PImports^.Name <> 0 do
  begin
    PRVA_Import := PImageThunkData(PtrUInt(PImports^.FirstThunk) + PtrUInt(hModule));
    while PRVA_Import^.thFunction <> nil do
    begin
      if Install then
        for i := 0 to Length(HookEntries) - 1 do
        begin
          if PRVA_Import^.thFunction = HookEntries[i].OriginalProc then
          begin
            VirtualProtect(PRVA_Import, sizeof(Pointer), PAGE_EXECUTE_READWRITE,
              OldProtect);
            PRVA_Import^.thFunction := HookEntries[i].HookProc;
            VirtualProtect(PRVA_Import, sizeof(Pointer), OldProtect, OldProtect);
            break;
          end;
        end
      else
        for i := 0 to Length(HookEntries) - 1 do
        begin
          if PRVA_Import^.thFunction = HookEntries[i].HookProc then
          begin
            VirtualProtect(PRVA_Import, sizeof(Pointer), PAGE_EXECUTE_READWRITE,
              OldProtect);
            PRVA_Import^.thFunction := HookEntries[i].OriginalProc;
            VirtualProtect(PRVA_Import, sizeof(Pointer), OldProtect, OldProtect);
            break;
          end;
        end;
      Inc(PRVA_Import);
    end;
    Inc(PImports);
  end;
end;

procedure InitHook();
var
  dir, S: WideString;
  hUSER32: THandle;
begin
  hUSER32 := GetModuleHandleW(user32);
  SetLength(dir, GetSystemDirectoryW(nil, 0));
  GetSystemDirectoryW(@dir[1], Length(dir));
  dir := WideString(PWideChar(dir));
  S := dir + WideString('\comdlg32.dll');
  hCOMDLG32DLL := LoadLibraryW(PWideChar(S));
  S := dir + WideString('\winmm.dll');
  hWINMMDLL := LoadLibraryW(PWideChar(S));

  SetLength(HookEntries, 3);
  pGetSaveFileNameA := LPGETSAVEFILENAMEA(GetProcAddress(hCOMDLG32DLL,
    'GetSaveFileNameA'));
  with HookEntries[0] do
  begin
    ProcName := 'GetSaveFileNameA';
    Module := hCOMDLG32DLL;
    OriginalProc := pGetSaveFileNameA;
    HookProc := @MyGetSaveFileNameA;
  end;
  pPlaySoundA := LPPLAYSOUNDA(GetProcAddress(hWINMMDLL,
    'PlaySoundA'));
  with HookEntries[1] do
  begin
    ProcName := 'PlaySoundA';
    Module := hWINMMDLL;
    OriginalProc := pPlaySoundA;
    HookProc := @MyPlaySoundA;
  end;
  pMessageBoxA := LPMESSAGEBOXA(GetProcAddress(hUSER32, 'MessageBoxA'));
  with HookEntries[2] do
  begin
    ProcName := 'MessageBoxA';
    Module := hUSER32;
    OriginalProc := pMessageBoxA;
    HookProc := @MyMessageBoxA;
  end;

  SuppressGSFN := False;
  SuppressPlaySound := False;
  SuppressMSGBOX := False;
  Hook(GetModuleHandle(nil), True);
end;

procedure FreeHook();
begin
  Hook(GetModuleHandle(nil), False);
  FreeLibrary(hCOMDLG32DLL);
  FreeLibrary(hWINMMDLL);
end;

end.

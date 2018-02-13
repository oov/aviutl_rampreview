unit Hook;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

procedure InitHook();
procedure FreeHook();
procedure SwitchHook(b: boolean);

implementation

uses
  Windows;

type
  LPGETSAVEFILENAMEA = function(var OFN: OPENFILENAME): HResult; stdcall;

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
  hCOMDLG32DLL: THandle;
  HookEntries: array of TAPIHookEntry;

  pGetSaveFileNameA: LPGETSAVEFILENAMEA;
  Suppress: boolean;

procedure SwitchHook(b: boolean);
begin
  Suppress := b;
end;

function MyGetSaveFileNameA(var OFN: OPENFILENAME): HResult; stdcall;
begin
  if Suppress then begin
    Result := 1;
    Exit;
  end;
  Result := pGetSaveFileNameA(OFN);
end;

procedure Hook(hModule: THandle);
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
      for i := 0 to Length(HookEntries) - 1 do
      begin
        if PRVA_Import^.thFunction = HookEntries[i].OriginalProc then
        begin
          VirtualProtect(PRVA_Import, sizeof(Pointer), PAGE_EXECUTE_READWRITE, OldProtect);
          PRVA_Import^.thFunction := HookEntries[i].HookProc;
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
  dir: WideString;
begin
  SetLength(dir, GetSystemDirectoryW(nil, 0));
  GetSystemDirectoryW(@dir[1], Length(dir));
  dir := WideString(PWideChar(dir)) + WideString('\comdlg32.dll');
  hCOMDLG32DLL := GetModuleHandleW(PWideChar(dir));
  if hCOMDLG32DLL = 0 then
    hCOMDLG32DLL := LoadLibraryW(PWideChar(dir));

  SetLength(HookEntries, 1);
  pGetSaveFileNameA := LPGETSAVEFILENAMEA(GetProcAddress(hCOMDLG32DLL,
    'GetSaveFileNameA'));
  with HookEntries[0] do
  begin
    ProcName := 'GetSaveFileNameA';
    Module := hCOMDLG32DLL;
    OriginalProc := pGetSaveFileNameA;
    HookProc := @MyGetSaveFileNameA;
  end;

  Suppress := False;
  Hook(GetModuleHandle(nil));
end;

procedure FreeHook();
begin
  FreeLibrary(hCOMDLG32DLL);
end;

end.


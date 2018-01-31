unit ExtramMain;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  StorageAPI;

type

  { TExtram }

  TExtram = class
  private
    FDLL: THandle;
    FAPI: PStorageAPI;
  public
    constructor Create();
    destructor Destroy(); override;
    property API: PStorageAPI read FAPI;
  end;

  function GetDLLName(): WideString;

implementation

uses
  Windows, SysUtils;

function GetDLLName(): WideString;
begin
  SetLength(Result, MAX_PATH);
  GetModuleFileNameW(hInstance, @Result[1], MAX_PATH);
  Result := PWideChar(Result);
end;

{ TExtram }

constructor TExtram.Create();
var
  F: TGetStorageAPIFunc;
begin
  inherited Create();
  FDLL := LoadLibraryW(PWideChar(ExtractFileDir(GetDLLName()) + '\..\ZRamPreview.auf'));
  if FDLL = 0 then Exit;
  F := TGetStorageAPIFunc(GetProcAddress(FDLL, 'GetStorageAPI'));
  if not Assigned(F) then Exit;
  FAPI := F();
  if not Assigned(FAPI) then Exit;
end;

destructor TExtram.Destroy();
begin
  if FDLL <> 0 then begin
    FreeLibrary(FDLL);
    FDLL := 0;
    FAPI := nil;
  end;
  inherited Destroy();
end;

end.

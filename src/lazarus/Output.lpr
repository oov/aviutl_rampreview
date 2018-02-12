library Output;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

uses
  Windows, AviUtl;

type
  TGetOutputPluginTableFunc = function(): POutputPluginTable; stdcall;

var
  Auf: THandle;

procedure Init();
var
  S: WideString;
begin
  SetLength(S, MAX_PATH);
  GetModuleFileNameW(hInstance, @S[1], MAX_PATH);
  S := PWideChar(S);
  S[Length(S)] := 'f';
  Auf := LoadLibraryW(PWideChar(S));
end;

procedure Final();
begin
  if Auf <> 0 then
    FreeLibrary(Auf);
end;

function GetOutputPluginTable(): POutputPluginTable; stdcall;
var
  F: TGetOutputPluginTableFunc;
begin
  Result := nil;
  if Auf = 0 then
    Exit;
  F := TGetOutputPluginTableFunc(GetProcAddress(Auf, 'GetOutputPluginTable'));
  if not Assigned(F) then
    Exit;
  Result := F();
end;

exports
  GetOutputPluginTable;

initialization
  Init();

finalization
  Final();

end.


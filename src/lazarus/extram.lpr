library Extram;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

uses
  SysUtils,
  ExtramMain,
  Lua,
  LuaFuncs, StorageAPI;

exports
  luaopen_Extram;

initialization
  SetMultiByteConversionCodePage(CP_UTF8);
  Randomize();
  LoadLua(ExtractFileDir(GetDLLName()) + '\..\lua51.dll');

finalization
  FreeLua();

end.

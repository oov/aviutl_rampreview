library RamPreview;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

uses
  SysUtils,
  AviUtl,
  Main, Remote, Util, StorageAPI, Hook, YV12;

exports
  GetFilterTableList,
  GetOutputPluginTable,
  GetStorageAPI;

initialization
  SetMultiByteConversionCodePage(CP_UTF8);
  Randomize();

finalization

end.

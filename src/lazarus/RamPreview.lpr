library RamPreview;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

uses
  SysUtils,
  AviUtl,
  Main, Remote, Util, StorageAPI, Hook, NV12, Encoder, Parallel;

exports
  GetFilterTableList,
  GetOutputPluginTable,
  GetStorageAPI;

initialization
  SetMultiByteConversionCodePage(CP_UTF8);
  Randomize();

finalization

end.

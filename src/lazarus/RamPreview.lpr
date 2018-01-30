library RamPreview;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

uses
  SysUtils,
  AviUtl,
  Main, Remote, Util, StorageAPI;

exports
  GetFilterTableList,
  GetStorageAPI;

initialization
  SetMultiByteConversionCodePage(CP_UTF8);
  Randomize();

finalization

end.

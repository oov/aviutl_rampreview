unit StorageAPI;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

type
  TGetMaxBufferSizeFunc = function(): integer; cdecl;
  TGetFunc = function(Key: PChar; Dest: Pointer): integer; cdecl;
  TPutFunc = function(Key: PChar; Src: Pointer; Len: integer): integer; cdecl;
  TDelFunc = procedure(Key: PChar); cdecl;

  TStorageAPI = record
    GetMaxBufferSize: TGetMaxBufferSizeFunc;
    Get: TGetFunc;
    Put: TPutFunc;
    Del: TDelFunc;
  end;
  PStorageAPI = ^TStorageAPI;

  TGetStorageAPIFunc = function(): PStorageAPI; cdecl;

implementation

end.


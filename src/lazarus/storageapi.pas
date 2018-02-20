unit StorageAPI;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

type
  TGetFunc = function(Key: PChar): integer; cdecl;
  TPutFunc = function(Key: PChar; Len: integer): integer; cdecl;
  TDelFunc = procedure(Key: PChar); cdecl;
  TSyncFunc = procedure(); cdecl;

  TViewHeader = record
    A, B, C, D: integer;
  end;
  PViewHeader = ^TViewHeader;

  TStorageAPI = record
    ViewHeader: PViewHeader;
    View: Pointer;
    ViewLen: integer;
    Get: TGetFunc;
    GetFinish: TSyncFunc;
    PutStart: TSyncFunc;
    Put: TPutFunc;
    Del: TDelFunc;
  end;
  PStorageAPI = ^TStorageAPI;

  TGetStorageAPIFunc = function(): PStorageAPI; cdecl;

implementation

end.


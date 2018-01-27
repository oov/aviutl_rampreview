unit Util;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Classes, SysUtils;

procedure ODS(const Fmt: string; const Args: array of const);

function GetDLLName(): WideString;
function GetWorkingSetSize(Process: THandle): QWord;
function BytesToStr(const Value: QWord; const InsertSpace: boolean = True): string;
procedure WriteUInt64(const S: TStream; const V: QWord);
procedure WriteUInt32(const S: TStream; const V: DWORD);
procedure WriteInt32(const S: TStream; const V: integer);
procedure WriteSingle(const S: TStream; const V: single);
procedure WriteString(const S: TStream; const V: UTF8String);
procedure WriteRawString(const S: TStream; const V: RawByteString);

implementation

uses
  Windows;

var
  debugging: boolean;

type
  TProcessMemoryCounters = packed record
    cb: DWORD;
    PageFaultCount: DWORD;
    PeakWorkingSetSize: size_t;
    WorkingSetSize: size_t;
    QuotaPeakPagedPoolUsage: size_t;
    QuotaPagedPoolUsage: size_t;
    QuotaPeakNonPagedPoolUsage: size_t;
    QuotaNonPagedPoolUsage: size_t;
    PagefileUsage: size_t;
    PeakPagefileUsage: size_t;
  end;

function GetProcessMemoryInfo(Process: THandle;
  var ppsmemCounters: TProcessMemoryCounters; cb: DWORD): BOOL; stdcall;
  external 'psapi.dll' Name 'GetProcessMemoryInfo';

procedure ODS(const Fmt: string; const Args: array of const);
begin
  if not debugging then
    Exit;
  OutputDebugStringW(PWideChar(WideString(Format('rampreview cli: ' + Fmt, Args))));
end;

function GetDLLName(): WideString;
begin
  SetLength(Result, MAX_PATH);
  GetModuleFileNameW(hInstance, @Result[1], MAX_PATH);
  Result := PWideChar(Result);
end;

function GetWorkingSetSize(Process: THandle): QWord;
var
  PMC: TProcessMemoryCounters;
begin
  Result := 0;
  PMC.cb := SizeOf(TProcessMemoryCounters);
  if not GetProcessMemoryInfo(Process, PMC, SizeOf(TProcessMemoryCounters)) then
    Exit;
  Result := PMC.WorkingSetSize;
end;

function BytesToStr(const Value: QWord; const InsertSpace: boolean = True): string;
const
  byte = 0;
  KILLO = 1024;
  MEGA = KILLO * 1024;
  GIGA = MEGA * 1024;
  TERA = QWord(GIGA) * 1024;

  BYTE1000 = 0;
  KILLO1000 = 1000;
  MEGA1000 = KILLO1000 * 1000;
  GIGA1000 = MEGA1000 * 1000;
  TERA1000 = QWord(GIGA1000) * 1000;
  Formats: array[boolean] of string = ('#,##0.00', '#,##');
var
  f: extended;
  s, Ext: string;
begin
  if InsertSpace then
    s := ' '
  else
    s := '';

  if Value < KILLO1000 then
  begin
    Result := IntToStr(Value) + s + 'バイト';
    Exit;
  end
  else if Value < MEGA1000 then
  begin
    f := Value / KILLO;
    Ext := 'KB';
  end
  else if Value < GIGA1000 then
  begin
    f := Value / MEGA;
    Ext := 'MB';
  end
  else if Value < TERA1000 then
  begin
    f := Value / GIGA;
    Ext := 'GB';
  end
  else
  begin
    f := Value / TERA;
    Ext := 'TB';
  end;
  Result := FormatFloat(Formats[f >= 10], f) + s + Ext;
end;

procedure WriteUInt64(const S: TStream; const V: QWord);
begin
  S.WriteQWord(V);
end;

procedure WriteUInt32(const S: TStream; const V: DWORD);
begin
  S.WriteDWord(V);
end;

procedure WriteInt32(const S: TStream; const V: integer);
begin
  S.WriteDWord(V);
end;

procedure WriteSingle(const S: TStream; const V: single);
begin
  S.WriteDWord(DWORD(V));
end;

procedure WriteString(const S: TStream; const V: UTF8String);
begin
  WriteInt32(S, Length(V));
  S.WriteBuffer(V[1], Length(V));
end;

procedure WriteRawString(const S: TStream; const V: RawByteString);
begin
  S.WriteBuffer(V[1], Length(V));
end;

initialization
  debugging := SysUtils.GetEnvironmentVariable('RAMPREVIEWDEBUG') <> '';

end.

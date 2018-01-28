unit Util;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Classes, SysUtils;

procedure ODS(const Fmt: string; const Args: array of const);

function GetDLLName(): WideString;
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

function BytesToStr(const Value: QWord; const InsertSpace: boolean = True): string;
const
  KILLO = 1024;
  MEGA = KILLO * 1024;
  GIGA = MEGA * 1024;
  TERA = QWord(GIGA) * 1024;

  KILLO1000 = 1000;
  MEGA1000 = KILLO1000 * 1000;
  GIGA1000 = MEGA1000 * 1000;
  TERA1000 = QWord(GIGA1000) * 1000;
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
    Ext := 'KiB';
  end
  else if Value < GIGA1000 then
  begin
    f := Value / MEGA;
    Ext := 'MiB';
  end
  else if Value < TERA1000 then
  begin
    f := Value / GIGA;
    Ext := 'GiB';
  end
  else
  begin
    f := Value / TERA;
    Ext := 'TiB';
  end;
  Result := FormatFloat('#,##0.00', f) + s + Ext;
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

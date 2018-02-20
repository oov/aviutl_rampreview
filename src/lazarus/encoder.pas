unit Encoder;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Classes, SysUtils;

type
  TEncodeEvent = procedure(const UserData, Buffer, TempBuffer: Pointer) of object;

  { TEncoder }

  TEncoder = class(TThread)
  private
    FBuffer, FSubBuffer, FTempBuffer: Pointer;
    FUserData: array of byte;
    FOnEncode: TEncodeEvent;
    FQueued, FDone: PRTLEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(const BufferSize: integer; const AllocTempBuffer: boolean);
    destructor Destroy(); override;
    procedure WaitPush();
    procedure Push(const UserData: Pointer; const UserDataLen: integer);
    property OnEncode: TEncodeEvent read FOnEncode write FOnEncode;
    property Buffer: Pointer read FBuffer;
  end;

implementation

uses
  Windows;

{ TEncoder }

procedure TEncoder.Execute;
var
  UserData: array of byte;
  P: Pointer;
begin
  try
    while not Terminated do
    begin
      RTLEventWaitFor(FQueued);
      RTLEventResetEvent(FQueued);
      if Length(FUserData) > 0 then begin
        SetLength(UserData, Length(FUserData));
        Move(FUserData[0], UserData[0], Length(UserData));
        P := FBuffer;
        FBuffer := FSubBuffer;
        FSubBuffer := P;
        RTLEventSetEvent(FDone);
        FOnEncode(@UserData[0], P, FTempBuffer);
      end else
        RTLEventSetEvent(FDone);
    end;
  except
    on E: Exception do
    begin
      OutputDebugStringW(PWideChar(
        WideString(Format('Exception in TEncoder: %s', [E.Message]))));
      Terminate();
      RTLEventSetEvent(FDone);
    end;
  end;
end;

constructor TEncoder.Create(const BufferSize: integer; const AllocTempBuffer: boolean);
begin
  FQueued := RTLEventCreate();
  FDone := RTLEventCreate();
  FBuffer := GetMem(BufferSize);
  FSubBuffer := GetMem(BufferSize);
  if AllocTempBuffer then
    FTempBuffer := GetMem(BufferSize)
  else
    FTempBuffer := nil;
  RTLEventSetEvent(FDone);
  inherited Create(False);
end;

destructor TEncoder.Destroy();
begin
  FreeMem(FSubBuffer);
  FreeMem(FBuffer);
  if Assigned(FTempBuffer) then
    FreeMem(FTempBuffer);
  RTLEventDestroy(FQueued);
  RTLEventDestroy(FDone);
  inherited Destroy();
end;

procedure TEncoder.WaitPush();
begin
  RTLEventWaitFor(FDone);
  RTLEventResetEvent(FDone);
end;

procedure TEncoder.Push(const UserData: Pointer; const UserDataLen: integer);
begin
  if UserDataLen <> Length(FUserData) then
    SetLength(FUserData, UserDataLen);
  if UserDataLen > 0 then
    Move(UserData^, FUserData[0], UserDataLen);
  RTLEventSetEvent(FQueued);
end;

end.

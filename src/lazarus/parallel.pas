unit Parallel;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Classes, SysUtils;

type
  TProc = procedure(const Index, N: integer; const UserData: Pointer);

  { TWorker }

  TWorker = class(TThread)
  private
    FIndex: integer;
    FN: integer;
    FProc: TProc;
    FQueued, FDone: PRTLEvent;
    FUserData: Pointer;
  protected
    procedure Execute; override;
  public
    constructor Create(const ThreadIndex: integer);
    destructor Destroy(); override;
    property Queued: PRTLEvent read FQueued;
    property Done: PRTLEvent read FDone;
    property Proc: TProc read FProc write FProc;
    property Index: integer read FIndex write FIndex;
    property N: integer read FN write FN;
    property UserData: Pointer read FUserData write FUserData;
  end;

  { TParallel }

  TParallel = class
  private
    FCS: TRTLCriticalSection;
    FWorkers: array of TWorker;
  public
    constructor Create(const Threads: integer);
    destructor Destroy(); override;
    procedure Execute(const Proc: TProc; const UserData: Pointer;
      const StartIndex, N: integer);
  end;

implementation

uses
  Windows;

{ TWorker }

procedure TWorker.Execute;
var
  ThreadIndex: integer;
begin
  ThreadIndex := FIndex;
  SetThreadAffinityMask(Windows.GetCurrentThread(), 1 shl ThreadIndex);
  try
    while not Terminated do
    begin
      RTLEventWaitFor(FQueued);
      RTLEventResetEvent(FQueued);
      if (FProc <> nil) and (FN <> 0) then
        FProc(FIndex, FN, FUserData);
      RTLEventSetEvent(FDone);
    end;
  except
    on E: Exception do
    begin
      OutputDebugStringW(PWideChar(
        WideString(Format('Exception in TWorker #%d: %s',
        [ThreadIndex, E.Message]))));
    end;
  end;
end;

constructor TWorker.Create(const ThreadIndex: integer);
begin
  FQueued := RTLEventCreate();
  FDone := RTLEventCreate();
  FIndex := ThreadIndex;
  inherited Create(False);
end;

destructor TWorker.Destroy();
begin
  RTLEventDestroy(FDone);
  RTLEventDestroy(FQueued);
  inherited Destroy();
end;

{ TParallel }

constructor TParallel.Create(const Threads: integer);
var
  I: integer;
begin
  inherited Create();
  InitCriticalSection(FCS);
  SetLength(FWorkers, Threads);
  for I := Low(FWorkers) to High(FWorkers) do
    FWorkers[I] := TWorker.Create(I);
end;

destructor TParallel.Destroy();
var
  I: integer;
begin
  for I := Low(FWorkers) to High(FWorkers) do
    FWorkers[I].Terminate;
  Execute(nil, nil, 0, 0);
  for I := Low(FWorkers) to High(FWorkers) do
  begin
    while not FWorkers[I].Finished do
      ThreadSwitch();
    FreeAndNil(FWorkers[I]);
  end;
  DoneCriticalSection(FCS);
  inherited Destroy();
end;

procedure TParallel.Execute(const Proc: TProc; const UserData: Pointer;
  const StartIndex, N: integer);
var
  I, D, Index, Remain, WorkersLen: integer;
begin
  WorkersLen := Length(FWorkers);
  EnterCriticalSection(FCS);
  D := (N + WorkersLen - 1) div WorkersLen;
  Index := StartIndex;
  Remain := N;
  for I := Low(FWorkers) to High(FWorkers) do
  begin
    FWorkers[I].Proc := Proc;
    FWorkers[I].UserData := UserData;
    FWorkers[I].Index := Index;
    if Remain - D >= 0 then
    begin
      FWorkers[I].N := D;
      Inc(Index, D);
      Dec(Remain, D);
    end
    else
    begin
      FWorkers[I].N := Remain;
      Inc(Index, Remain);
      Remain := 0;
    end;
    RTLEventSetEvent(FWorkers[I].Queued);
  end;
  for I := Low(FWorkers) to High(FWorkers) do
  begin
    RTLEventWaitFor(FWorkers[I].Done);
    RTLEventResetEvent(FWorkers[I].Done);
  end;
  LeaveCriticalSection(FCS);
end;

end.

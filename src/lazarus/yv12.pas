unit YV12;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  AviUtl;

function EncodeYC48ToYV12(fpip: PFilterProcInfo; Dest: Pointer): integer;
procedure DecodeYV12ToYC48(fpip: PFilterProcInfo; Src: Pointer);

implementation

// Referene: https://makiuchi-d.github.io/mksoft/doc/aviutlyc.html

function EncodeYC48ToYV12(fpip: PFilterProcInfo; Dest: Pointer): integer;
var
  SLine, DLine, X, Y: integer;
  S1, S2: PPixelYC;
  S, DY1, DY2, DU, DV: PByte;
begin
  SLine := fpip^.LineSize;
  S := fpip^.YCPEdit;
  DLine := fpip^.X;
  DY1 := Dest;
  DY2 := DY1 + DLine;
  DU := DY1 + DLine * fpip^.Y;
  DV := DU + (DLine div 2) * (fpip^.Y div 2);
  for Y := 0 to (fpip^.Y div 2) - 1 do
  begin
    S1 := PPixelYC(S);
    S2 := PPixelYC(S + SLine);
    for X := 0 to (fpip^.X div 2) - 1 do
    begin
      DU^ := (((S1^.Cb + 2048)*7 + 66)>>7) + 16;
      DV^ := (((S1^.Cr + 2048)*7 + 66)>>7) + 16;
      (DY1+0)^ := (((S1+0)^.Y*219 + 383)>>12) + 16;
      (DY1+1)^ := (((S1+1)^.Y*219 + 383)>>12) + 16;
      (DY2+0)^ := (((S2+0)^.Y*219 + 383)>>12) + 16;
      (DY2+1)^ := (((S2+1)^.Y*219 + 383)>>12) + 16;
      Inc(DY1, 2);
      Inc(DY2, 2);
      Inc(S1, 2);
      Inc(S2, 2);
      Inc(DU);
      Inc(DV);
    end;
    Inc(S, SLine * 2);
    Inc(DY1, DLine);
    Inc(DY2, DLine);
  end;
  Result := DLine * fpip^.Y + (DLine div 2) * (fpip^.Y div 2) * 2;
end;

procedure DecodeYV12ToYC48(fpip: PFilterProcInfo; Src: Pointer);
var
  SLine, DLine, X, Y, Cb, Cr: integer;
  D1, D2: PPixelYC;
  D, SY1, SY2, SU, SV: PByte;
begin
  DLine := fpip^.LineSize;
  D := fpip^.YCPEdit;
  SLine := fpip^.X;
  SY1 := Src;
  SY2 := SY1 + SLine;
  SU := SY1 + SLine * fpip^.Y;
  SV := SU + (SLine div 2) * (fpip^.Y div 2);
  for Y := 0 to (fpip^.Y div 2) - 1 do
  begin
    D1 := PPixelYC(D);
    D2 := PPixelYC(D + DLine);
    for X := 0 to (fpip^.X div 2) - 1 do
    begin
      // TODO: interpolate Cb/Cr
      Cb := (((SU+0)^ - 128)*4681 + 164) >> 8;
      Cr := (((SV+0)^ - 128)*4681 + 164) >> 8;
      (D1+0)^.Y := (((SY1+0)^ * 1197)>>6) - 299;
      (D1+0)^.Cb := Cb;
      (D1+0)^.Cr := Cr;
      (D1+1)^.Y := (((SY1+1)^ * 1197)>>6) - 299;
      (D1+1)^.Cb := Cb;
      (D1+1)^.Cr := Cr;
      (D2+0)^.Y := (((SY2+0)^ * 1197)>>6) - 299;
      (D2+0)^.Cb := Cb;
      (D2+0)^.Cr := Cr;
      (D2+1)^.Y := (((SY2+1)^ * 1197)>>6) - 299;
      (D2+1)^.Cb := Cb;
      (D2+1)^.Cr := Cr;

      Inc(SY1, 2);
      Inc(SY2, 2);
      Inc(D1, 2);
      Inc(D2, 2);
      Inc(SU);
      Inc(SV);
    end;
    Inc(D, DLine * 2);
    Inc(SY1, SLine);
    Inc(SY2, SLine);
  end;
end;

end.


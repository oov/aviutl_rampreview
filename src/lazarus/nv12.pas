unit NV12;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

procedure CopyYC48(Dest, Src: Pointer; const W, H, SLine, DLine: integer);
procedure CalcDownScaledSize(var W, H: integer; const Factor: integer);
procedure DownScaleYC48(Dest, Src: Pointer; var W, H: integer;
  SLine: integer; const Factor: integer);
procedure UpScaleYC48(Dest, Src: Pointer; const OrigW, OrigH: integer;
  const DLine: integer; const Factor: integer);
function EncodeYC48ToNV12(const Dest, Src: Pointer; const W, H: integer;
  const SLine: integer): integer;
procedure DecodeNV12ToYC48(const Dest, Src: Pointer; const W, H: integer;
  const DLine: integer);

implementation

uses
  AviUtl;

procedure CopyYC48(Dest, Src: Pointer; const W, H, SLine, DLine: integer);
var
  X, Y, Width: integer;
  S: PByte absolute Src;
  D: PByte absolute Dest;
begin
  Width := W * SizeOf(TPixelYC);
  for Y := 0 to H - 1 do
  begin
    Move(S^, D^, Width);
    Inc(S, SLine);
    Inc(D, DLine);
  end;
end;

procedure CalcDownScaledSize(var W, H: integer; const Factor: integer);
begin
  W := (W + Factor - 1) div Factor;
  H := (H + Factor - 1) div Factor;
end;

procedure DownScaleYC48(Dest, Src: Pointer; var W, H: integer;
  SLine: integer; const Factor: integer);
var
  X, Y: integer;
  S: PPixelYC;
  D: PPixelYC absolute Dest;
begin
  W := (W + Factor - 1) div Factor;
  H := (H + Factor - 1) div Factor;
  SLine := SLine * Factor;
  D := Dest;
  for Y := 0 to H - 1 do
  begin
    S := PPixelYC(Src);
    for X := 0 to W - 1 do
    begin
      D^ := S^;
      Inc(D);
      Inc(S, Factor);
    end;
    Inc(Src, SLine);
  end;
end;

procedure UpScaleYC48(Dest, Src: Pointer; const OrigW, OrigH: integer;
  const DLine: integer; const Factor: integer);
var
  SW, SH, FW, FH, X, Y, I, OrigLine: integer;
  Pix: TPixelYC;
  S: PPixelYC absolute Src;
  D: PPixelYC;
begin
  FW := OrigW div Factor;
  FH := OrigH div Factor;
  SW := (OrigW + Factor - 1) div Factor;
  SH := (OrigH + Factor - 1) div Factor;
  OrigLine := OrigW * SizeOf(TPixelYC);
  for Y := 0 to FH - 1 do
  begin
    D := PPixelYC(Dest);
    for X := 0 to FW - 1 do
    begin
      Pix := S^;
      for I := 0 to Factor - 1 do
      begin
        D^ := Pix;
        Inc(D);
      end;
      Inc(S);
    end;
    if SW <> FW then
    begin
      Pix := S^;
      for X := FW * Factor to OrigW - 1 do
      begin
        D^ := Pix;
        Inc(D);
      end;
      Inc(S);
    end;
    for I := 1 to Factor - 1 do
    begin
      Move(Dest^, (Dest + DLine * I)^, OrigLine);
    end;
    Inc(Dest, DLine * Factor);
  end;
  if SH <> FH then
  begin
    D := PPixelYC(Dest);
    for X := 0 to FW - 1 do
    begin
      Pix := S^;
      for I := 0 to Factor - 1 do
      begin
        D^ := Pix;
        Inc(D);
      end;
      Inc(S);
    end;
    if SW <> FW then
    begin
      Pix := S^;
      for X := FW * Factor to OrigW - 1 do
      begin
        D^ := Pix;
        Inc(D);
      end;
      Inc(S);
    end;
    for Y := FH * Factor to OrigH - 1 do
    begin
      Move(Dest^, (Dest + DLine)^, OrigLine);
      Inc(Dest, DLine);
    end;
  end;
end;

// Reference: https://makiuchi-d.github.io/mksoft/doc/aviutlyc.html

type
  TUV = record
    U, V: byte;
  end;
  PUV = ^TUV;

function EncodeYC48ToNV12(const Dest: Pointer; const Src: Pointer;
  const W, H: integer; const SLine: integer): integer;
var
  DLine, X, Y, YWB, YHB, UVW, UVH: integer;
  S1, S2: PPixelYC;
  S, DY1, DY2: PByte;
  DUV: PUV;
begin
  YWB := W div 2;
  YHB := H div 2;
  UVW := (W + 2 - 1) div 2;
  UVH := (H + 2 - 1) div 2;
  S := Src;
  DLine := W;
  DY1 := Dest;
  DY2 := DY1 + DLine;
  DUV := PUV(DY1 + W * H);
  for Y := 0 to YHB - 1 do
  begin
    S1 := PPixelYC(S);
    S2 := PPixelYC(S + SLine);
    for X := 0 to YWB - 1 do
    begin
      DUV^.U := (((S1^.Cb + 2048) * 7 + 66) shr 7) + 16;
      DUV^.V := (((S1^.Cr + 2048) * 7 + 66) shr 7) + 16;
      (DY1 + 0)^ := (((S1 + 0)^.Y * 219 + 383) shr 12) + 16;
      (DY1 + 1)^ := (((S1 + 1)^.Y * 219 + 383) shr 12) + 16;
      (DY2 + 0)^ := (((S2 + 0)^.Y * 219 + 383) shr 12) + 16;
      (DY2 + 1)^ := (((S2 + 1)^.Y * 219 + 383) shr 12) + 16;
      Inc(DY1, 2);
      Inc(DY2, 2);
      Inc(S1, 2);
      Inc(S2, 2);
      Inc(DUV);
    end;
    if YWB <> UVW then
    begin
      DUV^.U := (((S1^.Cb + 2048) * 7 + 66) shr 7) + 16;
      DUV^.V := (((S1^.Cr + 2048) * 7 + 66) shr 7) + 16;
      //for X := YWB * 2 to W - 1 do
      //begin
      DY1^ := ((S1^.Y * 219 + 383) shr 12) + 16;
      DY2^ := ((S2^.Y * 219 + 383) shr 12) + 16;
      Inc(DY1);
      Inc(DY2);
      Inc(S1);
      Inc(S2);
      //end;
      Inc(DUV);
    end;
    Inc(S, SLine * 2);
    Inc(DY1, DLine);
    Inc(DY2, DLine);
  end;
  if YHB <> UVH then
  begin
    S1 := PPixelYC(S);
    //for Y := YHB * 2 to H - 1 do
    //begin
    for X := 0 to YWB - 1 do
    begin
      DUV^.U := (((S1^.Cb + 2048) * 7 + 66) shr 7) + 16;
      DUV^.V := (((S1^.Cr + 2048) * 7 + 66) shr 7) + 16;
      (DY1 + 0)^ := (((S1 + 0)^.Y * 219 + 383) shr 12) + 16;
      (DY1 + 1)^ := (((S1 + 1)^.Y * 219 + 383) shr 12) + 16;
      Inc(DY1, 2);
      Inc(S1, 2);
      Inc(DUV);
    end;
    if YWB <> UVW then
    begin
      DUV^.U := (((S1^.Cb + 2048) * 7 + 66) shr 7) + 16;
      DUV^.V := (((S1^.Cr + 2048) * 7 + 66) shr 7) + 16;
      //for X := YWB * 2 to W - 1 do
      //begin
      DY1^ := ((S1^.Y * 219 + 383) shr 12) + 16;
      //Inc(DY1);
      //Inc(S1);
      //end;
      //Inc(DUV);
    end;
    //Inc(S, SLine);
    //end;
  end;
  Result := W * H + UVW * UVH * SizeOf(TUV);
end;

procedure DecodeNV12ToYC48(const Dest: Pointer; const Src: Pointer;
  const W, H: integer; const DLine: integer);
var
  SLine, X, Y, Cb, Cr, YWB, YHB, UVW, UVH: integer;
  D1, D2: PPixelYC;
  D, SY1, SY2: PByte;
  SUV: PUV;
begin
  YWB := W div 2;
  YHB := H div 2;
  UVW := (W + 2 - 1) div 2;
  UVH := (H + 2 - 1) div 2;

  D := Dest;
  SLine := W;
  SY1 := Src;
  SY2 := SY1 + SLine;
  SUV := PUV(SY1 + W * H);
  for Y := 0 to YHB - 1 do
  begin
    D1 := PPixelYC(D);
    D2 := PPixelYC(D + DLine);
    for X := 0 to YWB - 1 do
    begin
      // TODO: interpolate Cb/Cr
      Cb := ((SUV^.U - 128) * 4681 + 164) shr 8;
      Cr := ((SUV^.V - 128) * 4681 + 164) shr 8;
      (D1 + 0)^.Y := (((SY1 + 0)^ * 1197) shr 6) - 299;
      (D1 + 0)^.Cb := Cb;
      (D1 + 0)^.Cr := Cr;
      (D1 + 1)^.Y := (((SY1 + 1)^ * 1197) shr 6) - 299;
      (D1 + 1)^.Cb := Cb;
      (D1 + 1)^.Cr := Cr;
      (D2 + 0)^.Y := (((SY2 + 0)^ * 1197) shr 6) - 299;
      (D2 + 0)^.Cb := Cb;
      (D2 + 0)^.Cr := Cr;
      (D2 + 1)^.Y := (((SY2 + 1)^ * 1197) shr 6) - 299;
      (D2 + 1)^.Cb := Cb;
      (D2 + 1)^.Cr := Cr;
      Inc(SY1, 2);
      Inc(SY2, 2);
      Inc(D1, 2);
      Inc(D2, 2);
      Inc(SUV);
    end;
    if YWB <> UVW then
    begin
      Cb := ((SUV^.U - 128) * 4681 + 164) shr 8;
      Cr := ((SUV^.V - 128) * 4681 + 164) shr 8;
      //for X := YWB * 2 to W - 1 do
      //begin
      D1^.Y := ((SY1^ * 1197) shr 6) - 299;
      D1^.Cb := Cb;
      D1^.Cr := Cr;
      D2^.Y := ((SY2^ * 1197) shr 6) - 299;
      D2^.Cb := Cb;
      D2^.Cr := Cr;
      Inc(SY1);
      Inc(SY2);
      Inc(D1);
      Inc(D2);
      //end;
      Inc(SUV);
    end;
    Inc(D, DLine * 2);
    Inc(SY1, SLine);
    Inc(SY2, SLine);
  end;
  if YHB <> UVH then
  begin
    D1 := PPixelYC(D);
    //for Y := YHB * 2 to H - 1 do
    //begin
    for X := 0 to YWB - 1 do
    begin
      // TODO: interpolate Cb/Cr
      Cb := ((SUV^.U - 128) * 4681 + 164) shr 8;
      Cr := ((SUV^.V - 128) * 4681 + 164) shr 8;
      (D1 + 0)^.Y := (((SY1 + 0)^ * 1197) shr 6) - 299;
      (D1 + 0)^.Cb := Cb;
      (D1 + 0)^.Cr := Cr;
      (D1 + 1)^.Y := (((SY1 + 1)^ * 1197) shr 6) - 299;
      (D1 + 1)^.Cb := Cb;
      (D1 + 1)^.Cr := Cr;
      Inc(SY1, 2);
      Inc(D1, 2);
      Inc(SUV);
    end;
    if YWB <> UVW then
    begin
      Cb := ((SUV^.U - 128) * 4681 + 164) shr 8;
      Cr := ((SUV^.V - 128) * 4681 + 164) shr 8;
      //for X := YWB * 2 to W - 1 do
      //begin
      D1^.Y := ((SY1^ * 1197) shr 6) - 299;
      D1^.Cb := Cb;
      D1^.Cr := Cr;
      //Inc(SY1);
      //Inc(D1);
      //end;
      //Inc(SUV);
    end;
    //Inc(D, DLine);
    //end;
  end;
end;

end.

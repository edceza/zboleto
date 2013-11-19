(*
  ZBoleto Código de Barras.

  Copyright (C) 2013 Silvio Clecio.

  http://silvioprog.github.io/zboleto

  Todos os contribuidores:
  Por favor, veja o arquivo CONTRIBUIDORES.txt, incluso nesta
  distribuição.

  Veja o arquivo LICENCA.txt, incluso nesta distribuição,
  para detalhes sobre copyright.

  Esta biblioteca é distribuída na esperança de que seja útil,
  mas, SEM NENHUMA GARANTIA, nem mesmo a garantia implícita de
  COMERCIALIZAÇÃO ou ADEQUAÇÃO A UM DETERMINADO FIM.
*)

unit ZBoletoCodBarras;

{$i zboleto.inc}

interface

uses
  Classes, SysUtils, FPimage, FPWritePNG, FPImgCanv, FPCanvas;

function CodBarras2de5ParaStrBin(const ACod: string): string;
procedure CodBarras2de5ParaStream(ABuf: PChar; ACont: Integer;
  AStream: TStream; AAltura: Integer = 50);

implementation

var
  _CodBarras: array of string;

procedure PreparaTabela;
const
  P: Boolean = False;
var
  S: string;
  I, J, K, L: Integer;
begin
  if P then
    Exit;
  SetLength(_CodBarras, 100);
  _CodBarras[0] := '00110';
  _CodBarras[1] := '10001';
  _CodBarras[2] := '01001';
  _CodBarras[3] := '11000';
  _CodBarras[4] := '00101';
  _CodBarras[5] := '10100';
  _CodBarras[6] := '01100';
  _CodBarras[7] := '00011';
  _CodBarras[8] := '10010';
  _CodBarras[9] := '01010';
  for I := 9 downto 0 do
    for J := 9 downto 0 do
    begin
      L := (I * 10) + J;
      S := '';
      for K := 1 to 5 do
        S += _CodBarras[I][K] + _CodBarras[J][K];
      _CodBarras[L] := S;
    end;
  P := True;
end;

function CodBarras2de5ParaStrBin(const ACod: string): string;

  function _Obtem(const AVal, ATipo: Char): string; inline;
  begin
    if AVal = '0' then
      Result := ATipo
    else
      Result := ATipo + ATipo + ATipo;
  end;

var
  S: string;
  P: PString;
  I, R: Integer;
begin
  PreparaTabela;
  S := ACod;
  if Length(S) mod 2 <> 0 then
    S := '0' + S;
  Result := '';
  for I := 1 to Length(S) div 2 do
  begin
    R := Round(StrToFloatDef(Copy(S, I * 2 - 1, 2), 0));
    P := @_CodBarras[R];
    for R := 1 to 5 do
      Result += _Obtem(P^[R * 2 - 1], '0') + _Obtem(P^[R * 2], '1');
  end;
  Result := '0101' + Result + '00010';
end;

procedure CodBarras2de5ParaStream(ABuf: PChar; ACont: Integer;
  AStream: TStream; AAltura: Integer);
var
  I: Integer;
  VImg: TFPCustomImage;
  VWriter: TFPWriterPNG;
  VCanvas: TFPCustomCanvas;
begin
  VCanvas := nil;
  VImg := TFPMemoryImage.Create(ACont, AAltura);
  try
{$WARNINGS OFF}
    VCanvas := TFPImageCanvas.Create(VImg);
{$WARNINGS ON}
    VCanvas.Brush.FPColor := colWhite;
    VCanvas.Brush.Style := bsSolid;
    VCanvas.FillRect(0, 0, ACont, AAltura);
    VCanvas.Pen.FPColor := colBlack;
    VCanvas.Pen.Width := 1;
    for I := 0 to ACont - 1 do
      if ABuf[I] = '0' then
        VCanvas.Line(I, 0, I, AAltura);
    VWriter := TFPWriterPNG.Create;
    try
      VWriter.indexed := False;
      VWriter.wordsized := False;
      VWriter.UseAlpha := False;
      VWriter.GrayScale := False;
      VWriter.UseAlpha := False;
      VImg.SaveToStream(AStream, VWriter);
    finally
      VWriter.Free;
    end;
  finally
    VCanvas.Free;
    VImg.Free;
  end;
end;

end.

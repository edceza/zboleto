(*
  ZBoleto Úteis.

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

unit ZBoletoUteis;

{$i zboleto.inc}

interface

uses
  ZBoletoCodBarras, Classes, StrUtils, SysUtils, Base64;

function DataJuliano(const AData: TDate): string;
function RemoveDiacriticos(const S: string): string;
function RemoveCaractrsEspeciais(const S: string): string;
function StrZero(const S: string; const N: Integer): string;
function CorrigeStr(const S: string; const A: string; const N: string): string;
function Modulo10(const AVal: string): string;
function Modulo11(const AVal: string; const ABase: Integer = 9;
  const AResto: Boolean = False): string;
{ Calculo de Modulo 11 "Invertido" (com pesos de 9 a 2  e não de 2 a 9) }
function Modulo11Invertido(const AVal: string): string;
function CalculaFatorVencimento(const AData: TDateTime): string;
function FormataTexto(ATexto: string; const ATamanho: Integer;
  const ACaractrsDireita: Boolean = True;
  ACaractrsAcrescentar: Char = ' '): string;
function FormataNumero(const V: string; const N: Integer): string;
function GeraStrCodBarras(const ABanco, AMoeda, AAg, AOperacao, ACarteira,
  ANNumero, AValor, AVencto: string): string;
function GeraLinhaDigitavel(const ABanco, AMoeda, AAg, AOperacao,
  ACarteira, ANNumero, AValor, AVencto: string): string; overload;
function GeraLinhaDigitavel(const ACodigo: string): string; overload;
function StrBinParaHml(const ACod: string; const AAltura: Integer = 50): string;
function CodBarras2de5ParaHml(const ACod: string;
  const AAltura: Integer = 50): string;

implementation

function DataJuliano(const AData: TDate): string;
var
  D, M, A: word;
begin
  if AData = 0 then
    Result := '0000'
  else
  begin
    DecodeDate(AData, A, M, D);
    Result := AddChar('0', IntToStr(Trunc((AData -
      EncodeDate(A - 1, 12, 31)))), 3) + IntToStr(A)[4];
  end;
end;

function RemoveDiacriticos(const S: string): string;
var
  F: Boolean;
  I: SizeInt;
  PS, PD: PChar;
begin
  SetLength(Result, Length(S));
  PS := PChar(S);
  PD := PChar(Result);
  I := 0;
  while PS^ <> #0 do
  begin
    F := PS^ = #195;
    if F then
      case PS[1] of
        #128..#134: PD^ := 'A';
        #135: PD^ := 'C';
        #136..#139: PD^ := 'E';
        #140..#143: PD^ := 'I';
        #144: PD^ := 'D';
        #145: PD^ := 'N';
        #146..#150, #152: PD^ := 'O';
        #151: PD^ := 'x';
        #153..#156: PD^ := 'U';
        #157: PD^ := 'Y';
        #158: PD^ := 'P';
        #159: PD^ := 's';
        #160..#166: PD^ := 'a';
        #167: PD^ := 'c';
        #168..#171: PD^ := 'e';
        #172..#175: PD^ := 'i';
        #176: PD^ := 'd';
        #177: PD^ := 'n';
        #178..#182, #184: PD^ := 'o';
        #183: PD^ := '-';
        #185..#188: PD^ := 'u';
        #190: PD^ := 'p';
        #189, #191: PD^ := 'y';
      else
        F := False;
      end;
    if F then
      Inc(PS)
    else
      PD^ := PS^;
    Inc(I);
    Inc(PD);
    Inc(PS);
  end;
  SetLength(Result, I);
end;

function RemoveCaractrsEspeciais(const S: string): string;
var
  F: Boolean;
  I: SizeInt;
  PS, PD: PChar;
begin
  SetLength(Result, Length(S));
  PS := PChar(S);
  PD := PChar(Result);
  I := 0;
  while PS^ <> #0 do
  begin
    F := PS^ = #194;
    if F then
      case PS[1] of
        #161: PD^ := '!';
        #162, #169: PD^ := 'c';
        #163: PD^ := 'l';
        #164: PD^ := 'o';
        #165: PD^ := 'y';
        #166: PD^ := '|';
        #167: PD^ := 's';
        #168: PD^ := '"';
        #170, #172, #173: PD^ := '-';
        #171: PD^ := '<';
        #174: PD^ := 'r';
        #175, #176, #178, #179, #183, #185: PD^ := '^';
        #177: PD^ := '+';
        #180: PD^ := '\';
        #181: PD^ := '/';
        #182: PD^ := 'P';
        #184: PD^ := ',';
        #186: PD^ := '_';
        #187: PD^ := '>';
        #188, #189: PD^ := '1';
        #190: PD^ := '3';
        #191: PD^ := '?';
      else
        F := False;
      end;
    if F then
      Inc(PS)
    else
      PD^ := PS^;
    Inc(I);
    Inc(PD);
    Inc(PS);
  end;
  SetLength(Result, I);
end;

function StrZero(const S: string; const N: Integer): string;
begin
  Result := AddChar('0', S, N);
end;

function CorrigeStr(const S: string; const A: string; const N: string): string;
begin
  Result := StringReplace(S, A, N, [rfReplaceAll]);
end;

function Modulo10(const AVal: string): string;
var
  I, VPeso, VDigito: Integer;
begin
  Result := '';
  VPeso := 2;
  for I := Length(AVal) downto 1 do
  begin
    Result := IntToStr(StrToInt(AVal[I]) * VPeso) + Result;
    if VPeso = 1 then
      VPeso := 2
    else
      VPeso := 1;
  end;
  VDigito := 0;
  for I := 1 to Length(Result) do
    VDigito := VDigito + StrToInt(Result[I]);
  VDigito := 10 - (VDigito mod 10);
  if VDigito > 9 then
    VDigito := 0;
  Result := IntToStr(VDigito);
end;

function Modulo11(const AVal: string; const ABase: Integer;
  const AResto: Boolean): string;
var
  I, VSoma, VPeso, VDigito: Integer;
begin
  VSoma := 0;
  VPeso := 2;
  for I := Length(AVal) downto 1 do
  begin
    VSoma += StrToInt(AVal[I]) * VPeso;
    if VPeso < ABase then
      VPeso := Succ(VPeso)
    else
      VPeso := 2;
  end;
  if AResto then
    Result := IntToStr(VSoma mod 11)
  else
  begin
    VDigito := 11 - (VSoma mod 11);
    if VDigito > 9 then
      VDigito := 0;
    Result := IntToStr(VDigito);
  end;
end;

function Modulo11Invertido(const AVal: string): string;
var
  I, VFtIni, VFtFim, VFator, VSoma, VDigito: Integer;
begin
  VFtIni := 2;
  VFtFim := 9;
  VFator := VFtFim;
  VSoma := 0;
  for I := Length(AVal) downto 1 do
  begin
    VSoma += StrToInt(Copy(AVal, I, 1)) * VFator;
    Dec(VFator);
    if VFator < VFtIni then
      VFator := VFtFim;
  end;
  VDigito := VSoma mod 11;
  if VDigito > 9 then
    VDigito := 0;
  Result := IntToStr(VDigito);
end;

function CalculaFatorVencimento(const AData: TDateTime): string;
begin
  Result := IntToStr(Trunc(AData - EncodeDate(1997, 10, 07)));
end;

function FormataTexto(ATexto: string; const ATamanho: Integer;
  const ACaractrsDireita: Boolean; ACaractrsAcrescentar: Char): string;
var
  VQuantAcrescentar, VTamanhoTexto, VPosicaoInicial: integer;
begin
  case ACaractrsAcrescentar of
    '0'..'9', 'a'..'z', 'A'..'Z': ;
  else
    ACaractrsAcrescentar := ' ';
  end;
  ATexto := Trim(UpperCase(RemoveDiacriticos(ATexto)));
  VTamanhoTexto := Length(ATexto);
  VQuantAcrescentar := ATamanho - VTamanhoTexto;
  if VQuantAcrescentar < 0 then
    VQuantAcrescentar := 0;
  if ACaractrsAcrescentar = '' then
    ACaractrsAcrescentar := ' ';
  if VTamanhoTexto >= ATamanho then
    VPosicaoInicial := VTamanhoTexto - ATamanho + 1
  else
    VPosicaoInicial := 1;
  if ACaractrsDireita then
    ATexto := Copy(ATexto, 1, ATamanho) + StringOfChar(ACaractrsAcrescentar,
      VQuantAcrescentar)
  else
    ATexto := StringOfChar(ACaractrsAcrescentar, VQuantAcrescentar) +
      Copy(ATexto, VPosicaoInicial, ATamanho);
  Result := UpperCase(ATexto);
end;

function FormataNumero(const V: string; const N: Integer): string;
begin
  Result := StrZero(StringReplace(V, ',', '', [rfReplaceAll]), N);
end;

function GeraStrCodBarras(const ABanco, AMoeda, AAg, AOperacao, ACarteira,
  ANNumero, AValor, AVencto: string): string;
var
  VFatorVencto, VCampoLivre, VCodigoBarras, VDigitoCodigoBarras: string;
begin
  VFatorVencto := FormataTexto(CalculaFatorVencimento(StrToDate(AVencto)), 4,
    False, '0');
  VCampoLivre := StrZero(AAg, 4) + StrZero(ACarteira, 3) +
    StrZero(AOperacao, 7) + StrZero(ANNumero + Modulo10(AAg + ACarteira +
    ANNumero), 11);
  VCodigoBarras := StrZero(ABanco, 3) + StrZero(AMoeda, 1) +
    StrZero(VFatorVencto, 4) + StrZero(CorrigeStr(AValor, ',', ''), 10) +
    VCampoLivre;
  VDigitoCodigoBarras := Modulo11(VCodigoBarras, 9);
  if VDigitoCodigoBarras = '0' then
    VDigitoCodigoBarras := '1';
  Result := Copy(VCodigoBarras, 1, 4) + VDigitoCodigoBarras +
    Copy(VCodigoBarras, 5, length(VCodigoBarras) - 4);
end;

function GeraLinhaDigitavel(const ABanco, AMoeda, AAg, AOperacao,
  ACarteira, ANNumero, AValor, AVencto: string): string;
var
  P1, P2, P3, P4, P5, P6, C1, C2, C3, C4, C5, VCod: string;
begin
  VCod := GeraStrCodBarras(ABanco, AMoeda, AAg, AOperacao, ACarteira,
    ANNumero, AValor, AVencto);
  P1 := Copy(VCod, 1, 4);
  P2 := Copy(VCod, 20, 5);
  P3 := Modulo10(P1 + P2);
  P4 := P1 + P2 + P3;
  P5 := Copy(P4, 1, 5);
  P6 := Copy(P4, 6, 5);
  C1 := P5 + '.' + P6;
  P1 := Copy(VCod, 25, 10);
  P2 := Modulo10(P1);
  P3 := P1 + P2;
  P4 := Copy(P3, 1, 5);
  P5 := Copy(P3, 6, 6);
  C2 := P4 + '.' + P5;
  P1 := Copy(VCod, 35, 10);
  P2 := Modulo10(P1);
  P3 := P1 + P2;
  P4 := Copy(P3, 1, 5);
  P5 := Copy(P3, 6, 6);
  C3 := P4 + '.' + P5;
  C4 := Copy(VCod, 5, 1);
  C5 := Copy(VCod, 6, 14);
  Result := C1 + ' ' + C2 + ' ' + C3 + ' ' + C4 + ' ' + C5;
end;

function GeraLinhaDigitavel(const ACodigo: string): string;
var
  P1, P2, P3, P4, P5, P6, C1, C2, C3, C4, C5: string;
begin
  P1 := Copy(ACodigo, 1, 4);
  P2 := Copy(ACodigo, 20, 5);
  P3 := Modulo10(P1 + P2);
  P4 := P1 + P2 + P3;
  P5 := Copy(P4, 1, 5);
  P6 := Copy(P4, 6, MaxInt);
  C1 := P5 + '.' + P6;
  P1 := Copy(ACodigo, 25, 10);
  P2 := Modulo10(P1);
  P3 := P1 + P2;
  P4 := Copy(P3, 1, 5);
  P5 := Copy(P3, 6, MaxInt);
  C2 := P4 + '.' + P5;
  P1 := Copy(ACodigo, 35, 10);
  P2 := Modulo10(P1);
  P3 := P1 + P2;
  P4 := Copy(P3, 1, 5);
  P5 := Copy(P3, 6, MaxInt);
  C3 := P4 + '.' + P5;
  C4 := Copy(ACodigo, 5, 1);
  P1 := Copy(ACodigo, 6, 4);
  P2 := Copy(ACodigo, 10, 10);
  C5 := P1 + P2;
  Result := C1 + ' ' + C2 + ' ' + C3 + ' ' + C4 + ' ' + C5;
end;

function StrBinParaHml(const ACod: string; const AAltura: Integer): string;
var
  M: TStringStream;
begin
  M := TStringStream.Create('');
  try
    CodBarras2de5ParaStream(PChar(ACod), Length(ACod), M, AAltura);
    Result := '<img src="data:image/png;base64,' +
      EncodeStringBase64(M.DataString) + '">';
  finally
    M.Free;
  end;
end;

function CodBarras2de5ParaHml(const ACod: string;
  const AAltura: Integer): string;
begin
  Result := StrBinParaHml(CodBarras2de5ParaStrBin(ACod), AAltura);
end;

end.

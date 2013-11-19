(*
  ZBoleto Modelo SICREDI.

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

unit ZBoletoModelo_SICREDI;

{$i zboleto.inc}

interface

uses
  ZBoleto, ZBoletoUteis, SysUtils;

type

  { TZBoletoModeloSICREDI }

  TZBoletoModeloSICREDI = class(TZBoletoModeloBase)
  private
    FByteIdt: string;
    FCampoLivre: string;
    FCampoLivreDV: string;
    FDVNossoNumero: string;
    FFiller1: string;
    FFiller2: string;
    FNNum: string;
    FNossoNumeroDV: string;
    FPosto: string;
    FTipoCarteira: string;
    FTipoCobranca: string;
  public
    class function NomeModelo: string; override;
    class function TipoModelo: string; override;
    function GeraCodigoBanco(const ANumero: string): string; override;
    function FormataNumero(const V: string; const N: Integer; const I: string;
      const T: string = 'geral'): string;
    function DigitoVerificadorNossoNumero(const ANumero: string): string;
    function DigitoVerificadorCampoLivre(const ANumero: string): string;
    function DigitoVerificadorBarra(const ANumero: string): string;
    procedure Prepara; override;
    property Posto: string read FPosto write FPosto;
    property Filler1: string read FFiller1 write FFiller1;
    property Filler2: string read FFiller2 write FFiller2;
    property ByteIdt: string read FByteIdt write FByteIdt;
    property TipoCobranca: string read FTipoCobranca write FTipoCobranca;
    property TipoCarteira: string read FTipoCarteira write FTipoCarteira;
    property NNum: string read FNNum write FNNum;
    property DVNossoNumero: string read FDVNossoNumero write FDVNossoNumero;
    property NossoNumeroDV: string read FNossoNumeroDV write FNossoNumeroDV;
    property CampoLivre: string read FCampoLivre write FCampoLivre;
    property CampoLivreDV: string read FCampoLivreDV write FCampoLivreDV;
  end;

implementation

{ TZBoletoModeloSICREDI }

class function TZBoletoModeloSICREDI.NomeModelo: string;
begin
  Result := 'modelo_sicredi';
end;

class function TZBoletoModeloSICREDI.TipoModelo: string;
begin
  Result := 'SICREDI';
end;

function TZBoletoModeloSICREDI.GeraCodigoBanco(const ANumero: string): string;
begin
  Result := Copy(ANumero, 1, 3) + '-X';
end;

function TZBoletoModeloSICREDI.FormataNumero(const V: string;
  const N: Integer; const I: string; const T: string): string;
begin
  case T of
    'geral', 'valor':
      begin
        Result := CorrigeStr(V, ',', '');
        while Length(Result) < N do
          Result := I + Result;
      end;
    'convenio':
      begin
        Result := V;
        while Length(Result) < N do
          Result += I;
      end;
  end;
end;

function TZBoletoModeloSICREDI.DigitoVerificadorNossoNumero(
  const ANumero: string): string;
var
  VDigito, VResto2: Integer;
begin
  VResto2 := StrToInt(Modulo11(ANumero, 9, True));
  VDigito := 11 - VResto2;
  if VDigito > 9 then
    Result := '0'
  else
    Result := IntToStr(VDigito);
end;

function TZBoletoModeloSICREDI.DigitoVerificadorCampoLivre(
  const ANumero: string): string;
var
  VResto2: Integer;
begin
  VResto2 := StrToInt(Modulo11(ANumero, 9, True));
  if VResto2 <= 1 then
    Result := '0'
  else
    Result := IntToStr(11 - VResto2);
end;

function TZBoletoModeloSICREDI.DigitoVerificadorBarra(
  const ANumero: string): string;
var
  VResto2, VDigito: Integer;
begin
  VResto2 := StrToInt(Modulo11(ANumero, 9, True));
  VDigito := 11 - VResto2;
  if (VDigito <= 1) or (VDigito >= 10) then
    Result := '1'
  else
    Result := IntToStr(VDigito);
end;

procedure TZBoletoModeloSICREDI.Prepara;
begin
  NomeBanco := 'SICREDI';
  CodigoBanco := '748';
  CodigoBancoComDV := GeraCodigoBanco(CodigoBanco);
  NumMoeda := '9';
  FatorVencimento := CalculaFatorVencimento(StrToDate(
    Campo('data_vencimento').AsString));
  Valor := Self.FormataNumero(Campo('valor_boleto').AsString, 10, '0', 'valor');
  Agencia := Self.FormataNumero(Campo('agencia').AsString, 4, '0');
  FPosto := Self.FormataNumero(Campo('posto').AsString, 2, '0');
  Conta := Self.FormataNumero(Campo('conta').AsString, 5, '0');
  ContaDV := Self.FormataNumero(Campo('conta_dv').AsString, 1, '0');
  Carteira := Campo('carteira').AsString;
  FFiller1 := '1';
  FFiller2 := '0';
  FByteIdt := Campo('byte_idt').AsString;
  FTipoCobranca := '3';
  FTipoCarteira := '1';
  FNNum := Campo('inicio_nosso_numero').AsString + FByteIdt +
    Self.FormataNumero(Campo('nosso_numero').AsString, 5, '0');
  FDVNossoNumero := DigitoVerificadorNossoNumero(Agencia + FPosto + Conta +
    FNNum);
  FNossoNumeroDV := FNNum + FDVNossoNumero;
  FCampoLivre := FTipoCobranca + FTipoCarteira + FNossoNumeroDV + Agencia +
    FPosto + Conta + FFiller1 + FFiller2;
  FCampoLivreDV := FCampoLivre + DigitoVerificadorCampoLivre(FCampoLivre);
  DV := DigitoVerificadorBarra(CodigoBanco + NumMoeda + FatorVencimento +
    Valor + FCampoLivreDV);
  Codigo := CodigoBanco + NumMoeda + DV + FatorVencimento + Valor +
    FCampoLivreDV;
  NossoNumero := Copy(FNossoNumeroDV, 1, 2) + '/' + Copy(FNossoNumeroDV, 3, 6) +
    '-' + Copy(FNossoNumeroDV, 9, 1);
  AgenciaCodigo := Agencia + '.' + FPosto + '.' + Conta;
  Campos.Strings['codigo_barras'] := CodBarras2de5ParaHml(Codigo);
  Campos.Strings['linha_digitavel'] := GeraLinhaDigitavel(Codigo);
  Campos.Strings['agencia_codigo'] := AgenciaCodigo;
  Campos.Strings['nosso_numero'] := NossoNumero;
  Campos.Strings['codigo_banco_com_dv'] := CodigoBancoComDV;
end;

initialization
  TZBoletoModeloSICREDI.Registra;

finalization
  TZBoletoModeloSICREDI.Desregistra;

end.


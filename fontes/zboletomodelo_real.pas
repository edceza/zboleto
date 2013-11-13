(*
  ZBoleto Modelo Banco Real.

  Copyright (C) 2013 Silvio Clecio.

  http://silvioprog.github.io/zboleto/

  Todos os contribuidores:
  Por favor, veja o arquivo CONTRIBUIDORES.txt, incluso nesta
  distribuição.

  Veja o arquivo LICENCA.txt, incluso nesta distribuição,
  para detalhes sobre copyright.

  Esta biblioteca é distribuída na esperança de que seja útil,
  mas, SEM NENHUMA GARANTIA, nem mesmo a garantia implícita de
  COMERCIALIZAÇÃO ou ADEQUAÇÃO A UM DETERMINADO FIM.
*)

unit ZBoletoModelo_Real;

{$i zboleto.inc}

interface

uses
  ZBoleto, ZBoletoUteis, SysUtils;

type

  { TZBoletoModeloReal }

  TZBoletoModeloReal = class(TZBoletoModeloBase)
  private
    FDigitaoCobranca: string;
  public
    class function NomeModelo: string; override;
    class function TipoModelo: string; override;
    function FormataNumero(const V: string; const N: Integer; const I: string;
      const T: string = 'geral'): string;
    function DigitoVerificadorNossoNumero(const ANumero: string): string;
    function DigitoVerificadorBarra(const ANumero: string): string;
    procedure DVBarra(PNumero: PString);
    procedure Executa; override;
    property DigitaoCobranca: string read FDigitaoCobranca write FDigitaoCobranca;
  end;

implementation

{ TZBoletoModeloReal }

class function TZBoletoModeloReal.NomeModelo: string;
begin
  Result := 'modelo_real';
end;

class function TZBoletoModeloReal.TipoModelo: string;
begin
  Result := 'Real';
end;

function TZBoletoModeloReal.FormataNumero(const V: string;
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

function TZBoletoModeloReal.DigitoVerificadorNossoNumero(
  const ANumero: string): string;
var
  VDigito, VResto2: Integer;
begin
  VResto2 := StrToInt(Modulo11(ANumero, 7, True));
  VDigito := 11 - VResto2;
  case VDigito of
    10: Result := 'P';
    11: Result := '0';
  else
    Result := IntToStr(VDigito);
  end;
end;

function TZBoletoModeloReal.DigitoVerificadorBarra(
  const ANumero: string): string;
var
  VResto2: Integer;
begin
  VResto2 := StrToInt(Modulo11(ANumero, 9, True));
  if (VResto2 = 0) or (VResto2 = 1) or (VResto2 = 10) then
    Result := '1'
  else
    Result := IntToStr(11 - VResto2);
end;

procedure TZBoletoModeloReal.DVBarra(PNumero: PString);
const
  PESOS = '43290876543298765432987654329876543298765432';
var
  I, VSoma, VNumTemp: Integer;
begin
  if Length(PNumero^) = 44 then
  begin
    VSoma := 0;
    for I := 1 to Length(PNumero^) do
      VSoma += StrToInt(PNumero^[I]) * StrToInt(PESOS[I]);
    VNumTemp := 11 - (VSoma mod 11);
    if VNumTemp >= 10 then
      VNumTemp := 1;
    PNumero^[5] := IntToStr(VNumTemp)[1];
  end;
end;

procedure TZBoletoModeloReal.Executa;
begin
  NomeBanco := 'Real';
  CodigoBanco := '356';
  CodigoBancoComDV := GeraCodigoBanco(CodigoBanco);
  NumMoeda := '9';
  FatorVencimento := CalculaFatorVencimento(StrToDate(
    ObtemValorCampo('data_vencimento').AsString));
  Valor := Self.FormataNumero(ObtemValorCampo('valor_boleto').AsString, 10,
    '0', 'valor');
  Agencia := Self.FormataNumero(ObtemValorCampo('agencia').AsString, 4, '0');
  Conta := Self.FormataNumero(ObtemValorCampo('conta').AsString, 7, '0');
  Carteira := ObtemValorCampo('carteira').AsString;
  NossoNumero := Self.FormataNumero(ObtemValorCampo('nosso_numero').AsString,
    13, '0');
  FDigitaoCobranca := Modulo10(NossoNumero + Agencia + Conta);
  Codigo := CodigoBanco + NumMoeda + '0' + FatorVencimento + Valor + Agencia +
    Conta + FDigitaoCobranca + NossoNumero;
  DVBarra(@Codigo);
  AgenciaCodigo := Agencia + '/' + Conta + '/' + FDigitaoCobranca;
  Campos.Strings['codigo_barras'] := CodBarras2de5ParaHml(Codigo);
  Campos.Strings['linha_digitavel'] := GeraLinhaDigitavel(Codigo);
  Campos.Strings['agencia_codigo'] := AgenciaCodigo;
  Campos.Strings['nosso_numero'] := NossoNumero;
  Campos.Strings['codigo_banco_com_dv'] := CodigoBancoComDV;
end;

initialization
  TZBoletoModeloReal.Registra;

finalization
  TZBoletoModeloReal.Desregistra;

end.


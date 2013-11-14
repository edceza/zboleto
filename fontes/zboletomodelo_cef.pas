(*
  ZBoleto Modelo Caixa Econômica Federal.

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

unit ZBoletoModelo_CEF;

{$i zboleto.inc}

interface

uses
  ZBoleto, ZBoletoUteis, SysUtils, FPJSON;

type

  { TZBoletoModeloCEF }

  TZBoletoModeloCEF = class(TZBoletoModeloBase)
  private
    FNNum: string;
  public
    class function NomeModelo: string; override;
    class function TipoModelo: string; override;
    function DigitoVerificadorNossoNumero(const ANumero: string): string;
    function DigitoVerificadorBarra(const ANumero: string): string;
    procedure Prepara; override;
    property NNum: string read FNNum write FNNum;
  end;

implementation

{ TZBoletoModeloCEF }

class function TZBoletoModeloCEF.NomeModelo: string;
begin
  Result := 'modelo_cef';
end;

class function TZBoletoModeloCEF.TipoModelo: string;
begin
  Result := 'CEF';
end;

function TZBoletoModeloCEF.DigitoVerificadorNossoNumero(
  const ANumero: string): string;
var
  VDigito, VResto2: Integer;
begin
  VResto2 := StrToInt(Modulo11(ANumero, 9, True));
  VDigito := 11 - VResto2;
  if (VDigito = 10) or (VDigito = 11) then
    Result := '0'
  else
    Result := IntToStr(VDigito);
end;

function TZBoletoModeloCEF.DigitoVerificadorBarra(
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

procedure TZBoletoModeloCEF.Prepara;
var
  VDataVencto: TJSONData;
begin
  NomeBanco := 'Caixa Econômica Federal';
  CodigoBanco := '104';
  CodigoBancoComDV := GeraCodigoBanco(CodigoBanco);
  NumMoeda := '9';
  FatorVencimento := CalculaFatorVencimento(
    StrToDate(ObtemValorCampo('data_vencimento').AsString));
  Valor := ZBoletoUteis.FormataNumero(
    ObtemValorCampo('valor_boleto').AsString, 10);
  Agencia := ZBoletoUteis.FormataNumero(ObtemValorCampo('agencia').AsString, 4);
  Conta := ZBoletoUteis.FormataNumero(ObtemValorCampo('conta').AsString, 5);
  ContaDV := ZBoletoUteis.FormataNumero(ObtemValorCampo('conta_dv').AsString, 1);
  Carteira := ObtemValorCampo('carteira').AsString;
  FNNum := ObtemValorCampo('inicio_nosso_numero').AsString +
    ZBoletoUteis.FormataNumero(ObtemValorCampo('nosso_numero').AsString, 8);
  NossoNumero := FNNum + '-' + DigitoVerificadorNossoNumero(FNNum);
  ContaCedente := ZBoletoUteis.FormataNumero(
    ObtemValorCampo('conta_cedente').AsString, 11);
  ContaCedenteDV := ZBoletoUteis.FormataNumero(
    ObtemValorCampo('conta_cedente_dv').AsString, 1);
  DV := DigitoVerificadorBarra(CodigoBanco + NumMoeda + FatorVencimento +
    Valor + FNNum + Agencia + ContaCedente);
  Codigo := CodigoBanco + NumMoeda + DV + FatorVencimento + Valor + FNNum +
    Agencia + ContaCedente;
  AgenciaCodigo := Agencia + ' / ' + ContaCedente + '-' + ContaCedenteDV;
  Campos.Strings['codigo_barras'] := CodBarras2de5ParaHml(Codigo);
  Campos.Strings['linha_digitavel'] := GeraLinhaDigitavel(Codigo);
  Campos.Strings['agencia_codigo'] := AgenciaCodigo;
  Campos.Strings['nosso_numero'] := NossoNumero;
  Campos.Strings['codigo_banco_com_dv'] := CodigoBancoComDV;
  VDataVencto := ObtemValorCampo('data_vencimento');
  if VDataVencto.AsString = '' then
    VDataVencto.AsString := 'Contra apresentação';
end;

initialization
  TZBoletoModeloCEF.Registra;

finalization
  TZBoletoModeloCEF.Desregistra;

end.

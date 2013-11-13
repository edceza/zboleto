(*
  ZBoleto Modelo Itaú.

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

unit ZBoletoModelo_Itau;

{$i zboleto.inc}

interface

uses
  ZBoleto, ZBoletoUteis, SysUtils, FPJSON;

type

  { TZBoletoModeloItau }

  TZBoletoModeloItau = class(TZBoletoModeloBase)
  private
    FCodigoBarras: string;
    FDVNossoNumero: string;
    FNNum: string;
  public
    class function NomeModelo: string; override;
    class function TipoModelo: string; override;
    function FormataNumero(const V: string; const N: Integer; const I: string;
      const T: string = 'geral'): string;
    function DigitoVerificadorNossoNumero(const ANumero: string): string;
    function DigitoVerificadorBarra(const ANumero: string): string;
    procedure Executa(ACampos: TJSONObject); override;
    property NNum: string read FNNum write FNNum;
    property DVNossoNumero: string read FDVNossoNumero write FDVNossoNumero;
    property CodigoBarras: string read FCodigoBarras write FCodigoBarras;
  end;

implementation

{ TZBoletoModeloItau }

class function TZBoletoModeloItau.NomeModelo: string;
begin
  Result := 'modelo_itau';
end;

class function TZBoletoModeloItau.TipoModelo: string;
begin
  Result := 'Itau';
end;

function TZBoletoModeloItau.FormataNumero(const V: string;
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

function TZBoletoModeloItau.DigitoVerificadorNossoNumero(
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

function TZBoletoModeloItau.DigitoVerificadorBarra(
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

procedure TZBoletoModeloItau.Executa(ACampos: TJSONObject);
begin
  inherited;
  NomeBanco := 'Itaú';
  CodigoBanco := '341';
  CodigoBancoComDV := GeraCodigoBanco(CodigoBanco);
  NumMoeda := '9';
  FatorVencimento := CalculaFatorVencimento(StrToDate(
    ObtemValorCampo('data_vencimento').AsString));
  Valor := Self.FormataNumero(ObtemValorCampo('valor_boleto').AsString, 10,
    '0', 'valor');
  Agencia := Self.FormataNumero(ObtemValorCampo('agencia').AsString, 4, '0');
  Conta := Self.FormataNumero(ObtemValorCampo('conta').AsString, 5, '0');
  ContaDV := Self.FormataNumero(ObtemValorCampo('conta_dv').AsString, 1, '0');
  Carteira := ObtemValorCampo('carteira').AsString;
  FNNum := Self.FormataNumero(ObtemValorCampo('nosso_numero').AsString, 8, '0');
  FCodigoBarras := CodigoBanco + NumMoeda + FatorVencimento + Valor + Carteira +
    FNNum + Modulo10(Agencia + Conta + Carteira + FNNum) + Agencia + Conta +
    Modulo10(Agencia + Conta) + '000';
  DV := DigitoVerificadorBarra(FCodigoBarras);
  Codigo := Copy(FCodigoBarras, 1, 4) + DV + Copy(FCodigoBarras, 5, 43);
  NossoNumero := Carteira + '/' + FNNum + '-' + Modulo10(Agencia + Conta +
    Carteira + FNNum);
  AgenciaCodigo := Agencia + ' / ' + Conta + '-' + Modulo10(Agencia + Conta);
  ACampos.Strings['codigo_barras'] := CodBarras2de5ParaHml(Codigo);
  ACampos.Strings['linha_digitavel'] := GeraLinhaDigitavel(Codigo);
  ACampos.Strings['agencia_codigo'] := AgenciaCodigo;
  ACampos.Strings['nosso_numero'] := NossoNumero;
  ACampos.Strings['codigo_banco_com_dv'] := CodigoBancoComDV;
end;

initialization
  TZBoletoModeloItau.Registra;

finalization
  TZBoletoModeloItau.Desregistra;

end.


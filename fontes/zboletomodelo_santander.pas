(*
  ZBoleto Modelo Santander.

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

unit ZBoletoModelo_Santander;

{$i zboleto.inc}

interface

uses
  ZBoleto, ZBoletoUteis, SysUtils, FPJSON;

type

  { TZBoletoModeloSantander }

  TZBoletoModeloSantander = class(TZBoletoModeloBase)
  private
    FBarra: string;
    FCodigoCliente: string;
    FDVNossoNumero: string;
    FFixo: string;
    FIos: string;
    FNNum: string;
    FVencimento: TDate;
    FVencJuliano: string;
  public
    class function NomeModelo: string; override;
    class function TipoModelo: string; override;
    function FormataNumero(const V: string; const N: Integer; const I: string;
      const T: string = 'geral'): string;
    function DigitoVerificadorBarra(const ANumero: string): string;
    procedure Prepara; override;
    property Fixo: string read FFixo write FFixo;
    property Ios: string read FIos write FIos;
    property CodigoCliente: string read FCodigoCliente write FCodigoCliente;
    property NNum: string read FNNum write FNNum;
    property DVNossoNumero: string read FDVNossoNumero write FDVNossoNumero;
    property Vencimento: TDate read FVencimento write FVencimento;
    property VencJuliano: string read FVencJuliano write FVencJuliano;
    property Barra: string read FBarra write FBarra;
  end;

implementation

{ TZBoletoModeloSantander }

class function TZBoletoModeloSantander.NomeModelo: string;
begin
  Result := 'modelo_santander';
end;

class function TZBoletoModeloSantander.TipoModelo: string;
begin
  Result := 'Santander';
end;

function TZBoletoModeloSantander.FormataNumero(const V: string;
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

function TZBoletoModeloSantander.DigitoVerificadorBarra(
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

procedure TZBoletoModeloSantander.Prepara;
var
  VCodigoCliente, VNossoNumero: TJSONData;
begin
  NomeBanco := 'Santander';
  CodigoBanco := '033';
  CodigoBancoComDV := GeraCodigoBanco(CodigoBanco);
  NumMoeda := '9';
  FFixo := '9';
  FIos := '0';
  FatorVencimento := CalculaFatorVencimento(StrToDate(
    ObtemValorCampo('data_vencimento').AsString));
  Valor := Self.FormataNumero(ObtemValorCampo('valor_boleto').AsString, 10,
    '0', 'valor');
  Carteira := ObtemValorCampo('carteira').AsString;
  FCodigoCliente :=
    Self.FormataNumero(ObtemValorCampo('codigo_cliente').AsString, 7, '0');
  FNNum := Self.FormataNumero(ObtemValorCampo('nosso_numero').AsString, 7, '0');
  DVNossoNumero := Modulo11(FNNum, 9, False);
  NossoNumero := '00000' + FNNum + FDVNossoNumero;
  FVencimento := StrToDate(ObtemValorCampo('data_vencimento').AsString);
  FVencJuliano := DataJuliano(FVencimento);
  FBarra := CodigoBanco + NumMoeda + FatorVencimento + Valor + FFixo +
    FCodigoCliente + NossoNumero + FIos + Carteira;
  DV := DigitoVerificadorBarra(FBarra);
  Codigo := Copy(FBarra, 1, 4) + DV + Copy(FBarra, 5, MaxInt);
  Campos.Strings['codigo_barras'] := CodBarras2de5ParaHml(Codigo);
  Campos.Strings['linha_digitavel'] := GeraLinhaDigitavel(Codigo);
  Campos.Strings['nosso_numero'] := NossoNumero;
  Campos.Strings['codigo_banco_com_dv'] := CodigoBancoComDV;
  VCodigoCliente := ObtemValorCampo('codigo_cliente');
  VCodigoCliente.AsString := Copy(VCodigoCliente.AsString, 1,
    Length(VCodigoCliente.AsString) - 1) + '-' + Copy(VCodigoCliente.AsString,
    Length(VCodigoCliente.AsString), 1);
  VNossoNumero := ObtemValorCampo('nosso_numero');
  VNossoNumero.AsString := Copy(VNossoNumero.AsString, 1,
    Length(VNossoNumero.AsString) - 1) + '-' + Copy(VNossoNumero.AsString,
    Length(VNossoNumero.AsString), 1);
end;

initialization
  TZBoletoModeloSantander.Registra;

finalization
  TZBoletoModeloSantander.Desregistra;

end.

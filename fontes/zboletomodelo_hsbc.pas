(*
  ZBoleto Modelo HSBC.

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

unit ZBoletoModelo_HSBC;

{$i zboleto.inc}

interface

uses
  ZBoleto, ZBoletoUteis, SysUtils, FPJSON;

type

  { TZBoletoModeloHSBC }

  TZBoletoModeloHSBC = class(TZBoletoModeloBase)
  private
    FApp: string;
    FBarra: string;
    FCodigoCedente: string;
    FNDoc: string;
    FNNum: string;
    FVencimento: string;
    FVencJuliano: string;
  public
    class function NomeModelo: string; override;
    class function TipoModelo: string; override;
    function FormataNumero(const V: string; const N: Integer; const I: string;
      const T: string = 'geral'): string;
    function DigitoVerificadorBarra(const ANumero: string): string;
    function GeraNossoNumero(ANDoc, ACedente, AVenc, ATipoId: string): string;
    procedure Executa(ACampos: TJSONObject); override;
    property NNum: string read FNNum write FNNum;
    property NDoc: string read FNDoc write FNDoc;
    property CodigoCedente: string read FCodigoCedente write FCodigoCedente;
    property Vencimento: string read FVencimento write FVencimento;
    property VencJuliano: string read FVencJuliano write FVencJuliano;
    property App: string read FApp write FApp;
    property Barra: string read FBarra write FBarra;
  end;

implementation

{ TZBoletoModeloHSBC }

class function TZBoletoModeloHSBC.NomeModelo: string;
begin
  Result := 'modelo_hsbc';
end;

class function TZBoletoModeloHSBC.TipoModelo: string;
begin
  Result := 'HSBC';
end;

function TZBoletoModeloHSBC.FormataNumero(const V: string; const N: Integer;
  const I: string; const T: string): string;
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

function TZBoletoModeloHSBC.DigitoVerificadorBarra(
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

function TZBoletoModeloHSBC.GeraNossoNumero(ANDoc, ACedente, AVenc,
  ATipoId: string): string;
var
  VRes: Integer;
begin
  ANDoc := ANDoc + Modulo11Invertido(ANDoc) + ATipoId;
  AVenc := Copy(AVenc, 1, 2) + Copy(AVenc, 4, 2) + Copy(AVenc, 9, 2);
  VRes := StrToInt(ANDoc) + StrToInt(ACedente) + StrToInt(AVenc);
  Result := ANDoc + Modulo11Invertido(IntToStr(VRes));
end;

procedure TZBoletoModeloHSBC.Executa(ACampos: TJSONObject);
begin
  inherited;
  NomeBanco := 'HSBC';
  CodigoBanco := '399';
  CodigoBancoComDV := GeraCodigoBanco(CodigoBanco);
  NumMoeda := '9';
  FatorVencimento := CalculaFatorVencimento(
    StrToDate(ObtemValorCampo('data_vencimento').AsString));
  Valor := Self.FormataNumero(ObtemValorCampo('valor_boleto').AsString,
    10, '0', 'valor');
  Carteira := ObtemValorCampo('carteira').AsString;
  FCodigoCedente := Self.FormataNumero(
    ObtemValorCampo('codigo_cedente').AsString, 7, '0');
  FNDoc := ObtemValorCampo('numero_documento').AsString;
  FVencimento := ObtemValorCampo('data_vencimento').AsString;
  FNNum := Self.FormataNumero(ObtemValorCampo('numero_documento').AsString,
    13, '0');
  NossoNumero := GeraNossoNumero(FNNum, FCodigoCedente, FVencimento, '4');
  FVencJuliano := DataJuliano(FVencimento);
  FApp := '2';
  FBarra := CodigoBanco + NumMoeda + FatorVencimento + Valor + FCodigoCedente +
    FNNum + FVencJuliano + FApp;
  DV := DigitoVerificadorBarra(FBarra);
  Codigo := Copy(FBarra, 1, 4) + DV + Copy(FBarra, 5, MaxInt);
  AgenciaCodigo := FCodigoCedente;
  ACampos.Strings['codigo_barras'] := CodBarras2de5ParaHml(Codigo);
  ACampos.Strings['linha_digitavel'] := GeraLinhaDigitavel(Codigo);
  ACampos.Strings['agencia_codigo'] := AgenciaCodigo;
  ACampos.Strings['nosso_numero'] := NossoNumero;
  ACampos.Strings['codigo_banco_com_dv'] := CodigoBancoComDV;
end;

initialization
  TZBoletoModeloHSBC.Registra;

finalization
  TZBoletoModeloHSBC.Desregistra;

end.

(*
  ZBoleto Modelo Bradesco.

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

unit ZBoletoModelo_Bradesco;

{$i zboleto.inc}

interface

uses
  ZBoleto, ZBoletoUteis, SysUtils;

type

  { TZBoletoModeloBradesco }

  TZBoletoModeloBradesco = class(TZBoletoModeloBase)
  private
    FDVNossoNumero: string;
    FNNum: string;
  public
    class function NomeModelo: string; override;
    class function TipoModelo: string; override;
    function FormataNumero(const V: string; const N: Integer; const I: string;
      const T: string = 'geral'): string;
    function DigitoVerificadorNossoNumero(const ANumero: string): string;
    function DigitoVerificadorBarra(const ANumero: string): string;
    procedure Prepara; override;
    property NNum: string read FNNum write FNNum;
    property DVNossoNumero: string read FDVNossoNumero write FDVNossoNumero;
  end;

implementation

{ TZBoletoModeloBradesco }

class function TZBoletoModeloBradesco.NomeModelo: string;
begin
  Result := 'modelo_bradesco';
end;

class function TZBoletoModeloBradesco.TipoModelo: string;
begin
  Result := 'Bradesco';
end;

function TZBoletoModeloBradesco.FormataNumero(const V: string;
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

function TZBoletoModeloBradesco.DigitoVerificadorNossoNumero(
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

function TZBoletoModeloBradesco.DigitoVerificadorBarra(
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

procedure TZBoletoModeloBradesco.Prepara;
begin
  NomeBanco := 'Bradesco';
  CodigoBanco := '237';
  CodigoBancoComDV := GeraCodigoBanco(CodigoBanco);
  NumMoeda := '9';
  FatorVencimento := CalculaFatorVencimento(StrToDate(
    Campo('data_vencimento').AsString));
  Valor := Self.FormataNumero(Campo('valor_boleto').AsString, 10, '0', 'valor');
  Agencia := Self.FormataNumero(Campo('agencia').AsString, 4, '0');
  Conta := Self.FormataNumero(Campo('conta').AsString, 6, '0');
  ContaDV := Self.FormataNumero(Campo('conta_dv').AsString, 1, '0');
  Carteira := Campo('carteira').AsString;
  FNNum := Self.FormataNumero(Campo('carteira').AsString, 2, '0') +
    Self.FormataNumero(Campo('nosso_numero').AsString, 11, '0');
  FDVNossoNumero := DigitoVerificadorNossoNumero(NNum);
  NossoNumero := Copy(FNNum, 1, 2) + '/' + Copy(FNNum, 3, MaxInt) + '-' +
    FDVNossoNumero;
  ContaCedente := Self.FormataNumero(Campo('conta_cedente').AsString,
    7, '0');
  ContaCedenteDV :=
    Self.FormataNumero(Campo('conta_cedente_dv').AsString, 1, '0');
  DV := DigitoVerificadorBarra(CodigoBanco + NumMoeda + FatorVencimento +
    Valor + Agencia + FNNum + ContaCedente + '0');
  Codigo := CodigoBanco + NumMoeda + DV + FatorVencimento + Valor + Agencia +
    FNNum + ContaCedente + '0';
  AgenciaCodigo := Agencia + '-' + Campo('agencia_dv').AsString +
    ' / ' + ContaCedente + '-' + ContaCedenteDV;
  Campos.Strings['codigo_barras'] := CodBarras2de5ParaHml(Codigo);
  Campos.Strings['linha_digitavel'] := GeraLinhaDigitavel(Codigo);
  Campos.Strings['agencia_codigo'] := AgenciaCodigo;
  Campos.Strings['nosso_numero'] := NossoNumero;
  Campos.Strings['codigo_banco_com_dv'] := CodigoBancoComDV;
end;

initialization
  TZBoletoModeloBradesco.Registra;

finalization
  TZBoletoModeloBradesco.Desregistra;

end.


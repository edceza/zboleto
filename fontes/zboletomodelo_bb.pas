(*
  ZBoleto Modelo Banco do Brasil.

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

unit ZBoletoModelo_BB;

{$i zboleto.inc}

interface

uses
  ZBoleto, ZBoletoUteis, SysUtils, FPJSON;

type

  { TZBoletoModeloBB }

  TZBoletoModeloBB = class(TZBoletoModeloBase)
  private
    FConvenio: string;
    FLivreZeros: string;
    FNServico: string;
  public
    class function NomeModelo: string; override;
    class function TipoModelo: string; override;
    function FormataNumero(const V: string; const N: Integer; const I: string;
      const T: string = 'geral'): string;
    procedure Executa(ACampos: TJSONObject); override;
    property LivreZeros: string read FLivreZeros write FLivreZeros;
    property Convenio: string read FConvenio write FConvenio;
    property NServico: string read FNServico write FNServico;
  end;

implementation

{ TZBoletoModeloBB }

class function TZBoletoModeloBB.NomeModelo: string;
begin
  Result := 'modelo_bb';
end;

class function TZBoletoModeloBB.TipoModelo: string;
begin
  Result := 'BB';
end;

function TZBoletoModeloBB.FormataNumero(const V: string; const N: Integer;
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

procedure TZBoletoModeloBB.Executa(ACampos: TJSONObject);
var
  VFormatacaoConvenio, VFormatacaoNossoNumero: TJSONData;
begin
  inherited;
  NomeBanco := 'Banco do Brasil';
  CodigoBanco := '001';
  CodigoBancoComDV := GeraCodigoBanco(CodigoBanco);
  NumMoeda := '9';
  FatorVencimento := CalculaFatorVencimento(StrToDate(
    ObtemValorCampo('data_vencimento').AsString));
  Valor := Self.FormataNumero(ObtemValorCampo('valor_boleto').AsString, 10,
    '0', 'valor');
  Agencia := Self.FormataNumero(ObtemValorCampo('agencia').AsString, 4, '0');
  Conta := Self.FormataNumero(ObtemValorCampo('conta').AsString, 8, '0');
  Carteira := ObtemValorCampo('carteira').AsString;
  AgenciaCodigo := Agencia + '-' + Modulo11(Agencia) + ' / ' + Conta + '-' +
    Modulo11(Conta);
  FLivreZeros := '000000';
  VFormatacaoConvenio := ObtemValorCampo('formatacao_convenio');
  case VFormatacaoConvenio.AsInteger of
    8:
      begin
        FConvenio := Self.FormataNumero(ObtemValorCampo('convenio').AsString, 8,
          '0', 'convenio');
        NossoNumero := Self.FormataNumero(
          ObtemValorCampo('nosso_numero').AsString, 9, '0');
        DV := Modulo11(CodigoBanco + NumMoeda + FatorVencimento + Valor +
          FLivreZeros + FConvenio + NossoNumero + Carteira);
        Codigo := CodigoBanco + NumMoeda + DV + FatorVencimento + Valor +
          FLivreZeros + FConvenio + NossoNumero + Carteira;
        NossoNumero := FConvenio + NossoNumero + '-' + Modulo11(FConvenio +
          NossoNumero);
      end;
    7:
      begin
        FConvenio := Self.FormataNumero(ObtemValorCampo('convenio').AsString, 7,
          '0', 'convenio');
        NossoNumero := Self.FormataNumero(
          ObtemValorCampo('nosso_numero').AsString, 10, '0');
        DV := Modulo11(CodigoBanco + NumMoeda + FatorVencimento + Valor +
          FLivreZeros + FConvenio + NossoNumero + Carteira);
        Codigo := CodigoBanco + NumMoeda + DV + FatorVencimento + Valor +
          LivreZeros + Convenio + NossoNumero + Carteira;
        NossoNumero := FConvenio + NossoNumero;
      end;
    6:
      begin
        FConvenio := Self.FormataNumero(ObtemValorCampo('convenio').AsString, 6,
          '0', 'convenio');
        VFormatacaoNossoNumero := ObtemValorCampo('formatacao_nosso_numero');
        case VFormatacaoNossoNumero.AsInteger of
          1:
            begin
              NossoNumero := Self.FormataNumero(
                ObtemValorCampo('nosso_numero').AsString, 5, '0');
                DV := Modulo11(CodigoBanco + NumMoeda + FatorVencimento +
                  Valor + FConvenio + NossoNumero + Agencia + Conta + Carteira);
                Codigo := CodigoBanco + NumMoeda + DV + FatorVencimento +
                  Valor + FConvenio + NossoNumero + Agencia + Conta + Carteira;
                NossoNumero := FConvenio + NossoNumero + '-' +
                  Modulo11(FConvenio + NossoNumero);
            end;
          2:
            begin
              FNServico := '21';
              NossoNumero := Self.FormataNumero(
                ObtemValorCampo('nosso_numero').AsString, 17, '0');
              DV := Modulo11(CodigoBanco + NumMoeda + FatorVencimento + Valor +
                FConvenio + NossoNumero + FNServico);
              Codigo := CodigoBanco + NumMoeda + DV + FatorVencimento + Valor +
                FConvenio + NossoNumero + FNServico;
            end;
        end;
      end;
  end;
  ACampos.Strings['codigo_barras'] := CodBarras2de5ParaHml(Codigo);
  ACampos.Strings['linha_digitavel'] := GeraLinhaDigitavel(Codigo);
  ACampos.Strings['agencia_codigo'] := AgenciaCodigo;
  ACampos.Strings['nosso_numero'] := NossoNumero;
  ACampos.Strings['codigo_banco_com_dv'] := CodigoBancoComDV;
end;

initialization
  TZBoletoModeloBB.Registra;

finalization
  TZBoletoModeloBB.Desregistra;

end.


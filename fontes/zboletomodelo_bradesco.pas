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
  ZBoleto, ZBoletoUteis, Classes, SysUtils, StrUtils, FPJSON;

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
    function GeraHeaderRemessa(ACampos: TJSONObject;
      const ATipoCNAB: TZBoletoTipoCNAB): string; override;
    function GeraDetalhesRemessa(ACampos: TJSONArray;
      const ATipoCNAB: TZBoletoTipoCNAB): string; override;
    function GeraTraillerRemessa(ACampos: TJSONObject;
      const ATipoCNAB: TZBoletoTipoCNAB): string; override;
    procedure GeraHeaderRetorno(ADados: TStream; ACampos: TJSONObject;
       const ATipoCNAB: TZBoletoTipoCNAB); override;
    procedure GeraDetalhesRetorno(ADados: TStream; AItens: TJSONArray;
       const ATipoCNAB: TZBoletoTipoCNAB); override;
    procedure GeraTraillerRetorno(ADados: TStream; ACampos: TJSONObject;
      const ATipoCNAB: TZBoletoTipoCNAB); override;
    function GeraHeaderRemessaCNAB400(const ACodigoEmpresa,
      ANomeEmpresa: string; ADataGravacaoArquivo: TDate;
      const ANumSequencialRemessa: Integer): string;
    function GeraDetalhesRemessaCNAB400(const AAgenciaDebito: string;
      const AAgenciaDebitoDV: Char; const ARazaoContaCorrente,
      AContaCorrente: string; const AContaCorrenteDV: Char;
      const AEmpresaCedendeBanco, ANumControleParticipante,
      ACodigoBanco: string; const APercentualMulta: Double;
      const ATituloBanco: string; const ANossoNumeroDV: Char;
      const ADescBonificacaoPorDia: Double; const AEmissaoPapeletaCobr,
      AEmitePapeletaDebito, ARateioCredito, AAvisoDebitoContaCorrente: Char;
      const AOcorrencia, ANumDocumento: string; const ADataVenctoTitulo: TDate;
      const AValorTitulo: Currency; const ABancoCobranca, AAgenciaDepositaria,
      AEspecieTitulo: string; const AIdentificacao: Char;
      const ADataEmissaoTitulo: TDate; const APrimeiraInstrucao,
      ASegundaInstrucao: string; const AValorCobradoPorDiaAtraso: Currency;
      const ADataLimiteConcessaoDesc: TDate; const AAValorDesconto, AValorIOF,
      AValorAbatimento: Currency; const ATipoInscricaoSacado,
      ANumInscricaoSacado, ANomeSacado, AEnderecoCompleto, APrimeiraMensagem,
      ACEP, ASufixoCEP, ASacadorAvalistaSegMsg: string;
      const ANumSequencialRegistro: Integer): string;
    function GeraTraillerRemessaCNAB400(
      const ANumSequencialRegistro: Integer): string;
    procedure GeraHeaderRetornoCNAB400(ADados: TStream; ARetorno: TStrings;
      ACampos: TJSONObject);
    procedure GeraDetalhesRetornoCNAB400(ADados: TStream; ARetorno: TStrings;
      AItens: TJSONArray);
    procedure GeraTraillerRetornoCNAB400(ADados: TStream; ARetorno: TStrings;
      ACampos: TJSONObject);
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

function TZBoletoModeloBradesco.GeraHeaderRemessa(ACampos: TJSONObject;
  const ATipoCNAB: TZBoletoTipoCNAB): string;
var
  VDataGravacaoArquivo: TDate;
  VCampoDataGravacaoArquivo: TJSONData;
begin
  case ATipoCNAB of
    tc240: Result := inherited GeraHeaderRemessa(ACampos, ATipoCNAB);
    tc400:
      begin
        inherited GeraHeaderRemessa(ACampos, ATipoCNAB);
        VCampoDataGravacaoArquivo := ACampos.Find('data_gravacao_arquivo');
        if Assigned(VCampoDataGravacaoArquivo) then
          VDataGravacaoArquivo := VCampoDataGravacaoArquivo.AsFloat
        else
          VDataGravacaoArquivo := DataNula;
        Result := GeraHeaderRemessaCNAB400(
          CampoInterno(ACampos, 'codigo_empresa').AsString,
          CampoInterno(ACampos, 'nome_empresa').AsString, VDataGravacaoArquivo,
          CampoInterno(ACampos, 'num_sequencial_remessa').AsInteger);
      end;
  end;
end;

function TZBoletoModeloBradesco.GeraDetalhesRemessa(ACampos: TJSONArray;
  const ATipoCNAB: TZBoletoTipoCNAB): string;

  function _CampoZero(J: TJSONObject; const C: string; const N: Integer): string;
  begin
    Result := FormataComZero(CampoInterno(J, C).AsString, N);
  end;

var
  I: Integer;
  VEmpresa: string;
  VCampo: TJSONData;
  VCampos: TJSONObject;
  VCampoEmpresa: TJSONObject absolute VCampo;
begin
  case ATipoCNAB of
    tc240: Result := inherited GeraDetalhesRemessa(ACampos, ATipoCNAB);
    tc400:
      begin
        inherited GeraDetalhesRemessa(ACampos, ATipoCNAB);
        if ACampos.Count < 1 then
          raise EZBoleto.Create('Sem dados para gerar detalhes de remessa.');
        Result := '';
        for I := 0 to Pred(ACampos.Count) do
        begin
          VCampos := ACampos.Objects[I];
          VCampo := CampoInterno(VCampos, 'empresa');
          case VCampo.JSONType of
            jtString: VEmpresa := VCampo.AsString;
            jtObject: VEmpresa := _CampoZero(VCampoEmpresa, 'carteira', 3) +
              _CampoZero(VCampoEmpresa, 'agencia', 5) +
              _CampoZero(VCampoEmpresa, 'conta', 7) +
              _CampoZero(VCampoEmpresa, 'conta_dv', 1);
          else
            raise EZBoleto.Create('Tipo inválido para o campo "empresa".');
          end;
          Result += GeraDetalhesRemessaCNAB400(
            TextoJSON(VCampos, 'agencia_debito', '00000'),
            CaractereJSON(VCampos, 'agencia_debito_dv', '0'),
            TextoJSON(VCampos, 'razao_conta_corrente', '00000'),
            TextoJSON(VCampos, 'conta_corrente', '0000000'),
            CaractereJSON(VCampos, 'conta_corrente_dv', '0'),
            VEmpresa,
            CampoInterno(VCampos, 'num_controle_participante').AsString,
            TextoJSON(VCampos, 'codigo_banco', '000'),
            FloatJSON(VCampos, 'percentual_multa', 0),
            TextoJSON(VCampos, 'titulo_banco', '00000000000'),
            CaractereJSON(VCampos, 'nosso_numero_dv', '0'),
            FloatJSON(VCampos, 'desc_bonificacao_por_dia', 0),
            CaractereJSON(VCampos, 'emissao_papeleta_cobr', '2'),
            CaractereJSON(VCampos, 'emite_papeleta_debito', ' '),
            CaractereJSON(VCampos, 'rateio_credito', ' '),
            CaractereJSON(VCampos, 'aviso_debito_conta_corrente', '2'),
            CampoInterno(VCampos, 'ocorrencia').AsString,
            CampoInterno(VCampos, 'num_documento').AsString,
            CampoInterno(VCampos, 'data_vencto_titulo').AsFloat,
            CampoInterno(VCampos, 'valor_titulo').AsFloat,
            TextoJSON(VCampos, 'banco_cobranca', '000'),
            TextoJSON(VCampos, 'agencia_depositaria', '00000'),
            CampoInterno(VCampos, 'especie_titulo').AsString,
            CaractereJSON(VCampos, 'identificacao', 'N'),
            CampoInterno(VCampos, 'data_emissao_titulo').AsFloat,
            TextoJSON(VCampos, 'primeira_instrucao', '00'),
            TextoJSON(VCampos, 'segunda_instrucao', '00'),
            CurrJSON(VCampos, 'valor_cobrado_por_dia_atraso', 0),
            DataJSON(VCampos, 'data_limite_concessao_desc', DataNula),
            CurrJSON(VCampos, 'valor_desconto', 0),
            CurrJSON(VCampos, 'valor_iof', 0),
            CurrJSON(VCampos, 'valor_abatimento', 0),
            CampoInterno(VCampos, 'tipo_inscricao_sacado').AsString,
            CampoInterno(VCampos, 'num_inscricao_sacado').AsString,
            CampoInterno(VCampos, 'nome_sacado').AsString,
            CampoInterno(VCampos, 'endereco_completo').AsString,
            CampoInterno(VCampos, 'primeira_mensagem').AsString,
            CampoInterno(VCampos, 'cep').AsString,
            CampoInterno(VCampos, 'sufixo_cep').AsString,
            TextoJSON(VCampos, 'sacador_avalista_seg_msg', ''),
            CampoInterno(VCampos, 'num_sequencial_registro').AsInteger) + CRLF;
        end;
        SetLength(Result, Length(Result) - Length(CRLF));
      end;
  end;
end;

function TZBoletoModeloBradesco.GeraTraillerRemessa(ACampos: TJSONObject;
  const ATipoCNAB: TZBoletoTipoCNAB): string;
begin
  case ATipoCNAB of
    tc240: Result := inherited GeraTraillerRemessa(ACampos, ATipoCNAB);
    tc400:
      begin
        inherited GeraTraillerRemessa(ACampos, ATipoCNAB);
        Result := GeraTraillerRemessaCNAB400(
          CampoInterno(ACampos, 'num_sequencial_registro').AsInteger);
      end;
  end;
end;

procedure TZBoletoModeloBradesco.GeraHeaderRetorno(ADados: TStream;
  ACampos: TJSONObject; const ATipoCNAB: TZBoletoTipoCNAB);
var
  VRetorno: TStrings;
begin
  case ATipoCNAB of
    tc240: raise EZBoleto.Create(SZBNaoImplementado_Info);
    tc400:
      begin
        inherited GeraHeaderRetorno(ADados, ACampos, ATipoCNAB);
        VRetorno := TStringList.Create;
        try
          GeraHeaderRetornoCNAB400(ADados, VRetorno, ACampos);
        finally
          VRetorno.Free;
        end;
      end;
  end;
end;

procedure TZBoletoModeloBradesco.GeraDetalhesRetorno(ADados: TStream;
  AItens: TJSONArray; const ATipoCNAB: TZBoletoTipoCNAB);
var
  VRetorno: TStrings;
begin
  case ATipoCNAB of
    tc240: raise EZBoleto.Create(SZBNaoImplementado_Info);
    tc400:
      begin
        inherited GeraDetalhesRetorno(ADados, AItens, ATipoCNAB);
        VRetorno := TStringList.Create;
        try
          GeraDetalhesRetornoCNAB400(ADados, VRetorno, AItens);
        finally
          VRetorno.Free;
        end;
      end;
  end;
end;

procedure TZBoletoModeloBradesco.GeraTraillerRetorno(ADados: TStream;
  ACampos: TJSONObject; const ATipoCNAB: TZBoletoTipoCNAB);
var
  VRetorno: TStrings;
begin
  case ATipoCNAB of
    tc240: raise EZBoleto.Create(SZBNaoImplementado_Info);
    tc400:
      begin
        inherited GeraTraillerRetorno(ADados, ACampos, ATipoCNAB);
        VRetorno := TStringList.Create;
        try
          GeraTraillerRetornoCNAB400(ADados, VRetorno, ACampos);
        finally
          VRetorno.Free;
        end;
      end;
  end;
end;

function TZBoletoModeloBradesco.GeraHeaderRemessaCNAB400(const ACodigoEmpresa,
  ANomeEmpresa: string; ADataGravacaoArquivo: TDate;
  const ANumSequencialRemessa: Integer): string;
begin
  if ADataGravacaoArquivo = DataNula then
    ADataGravacaoArquivo := Date;
  Result :=
    '0' +                                                // Identificação do registro
    '1' +                                                // Identificação do arquivo remessa
    'REMESSA' +                                          // Literal remessa
    '01' +                                               // Código de serviço
    FormataComEspacoR('COBRANCA', 15) +                  // Literal serviço
    FormataComZero(ACodigoEmpresa, 20) +                 // Código da empresa
    FormataComEspacoR(UpperCase(ANomeEmpresa), 30) +     // Nome da empresa
    '237' +                                              // Número do Bradesco na Câmara de Compensação
    FormataComEspacoR('BRADESCO', 15) +                  // Nome do banco por extenso
    FormataData(ADataGravacaoArquivo) +                  // Data da gravação do arquivo
    Espacos(8) +                                         // Branco
    'MX' +                                               // Identificação do sistema
    FormataComZero(IntToStr(ANumSequencialRemessa), 7) + // Sequencial
    Espacos(277) +                                       // Branco
    '000001';                                            // Nº Sequencial do registro de um em um
end;

function TZBoletoModeloBradesco.GeraDetalhesRemessaCNAB400(
  const AAgenciaDebito: string; const AAgenciaDebitoDV: Char;
  const ARazaoContaCorrente, AContaCorrente: string;
  const AContaCorrenteDV: Char; const AEmpresaCedendeBanco,
  ANumControleParticipante, ACodigoBanco: string;
  const APercentualMulta: Double; const ATituloBanco: string;
  const ANossoNumeroDV: Char; const ADescBonificacaoPorDia: Double;
  const AEmissaoPapeletaCobr, AEmitePapeletaDebito, ARateioCredito,
  AAvisoDebitoContaCorrente: Char; const AOcorrencia, ANumDocumento: string;
  const ADataVenctoTitulo: TDate; const AValorTitulo: Currency;
  const ABancoCobranca, AAgenciaDepositaria, AEspecieTitulo: string;
  const AIdentificacao: Char; const ADataEmissaoTitulo: TDate;
  const APrimeiraInstrucao, ASegundaInstrucao: string;
  const AValorCobradoPorDiaAtraso: Currency;
  const ADataLimiteConcessaoDesc: TDate; const AAValorDesconto, AValorIOF,
  AValorAbatimento: Currency; const ATipoInscricaoSacado, ANumInscricaoSacado,
  ANomeSacado, AEnderecoCompleto, APrimeiraMensagem, ACEP, ASufixoCEP,
  ASacadorAvalistaSegMsg: string; const ANumSequencialRegistro: Integer): string;
var
  VDataVencto: string;
begin
  case Trunc(ADataVenctoTitulo) of
    0: VDataVencto := '000000';
    777777: VDataVencto := '777777';
    888888: VDataVencto := '888888';
    999999: VDataVencto := '999999';
  else
    VDataVencto := FormataData(ADataVenctoTitulo);
  end;
  ValidaTamVal(ACodigoBanco, 3);
  Result :=
    '1' +                                                      // Identificação do registro
    FormataComZero(AAgenciaDebito, 5) +                        // Agência de débito
    AAgenciaDebitoDV +                                         // Dígito da agência de débito
    FormataComZero(ARazaoContaCorrente, 5) +                   // Razão da conta corrente
    FormataComZero(AContaCorrente, 7) +                        // Conta corrente
    AContaCorrenteDV +                                         // Dígito da conta corrente
    FormataComEspacoR('0' + AEmpresaCedendeBanco, 17) +        // Identificação da empresa cedente no banco
    FormataComEspacoR(ANumControleParticipante, 25) +          // Nº Controle do participante
    ACodigoBanco +                                             // Código do banco a ser debitado na câmara de compensação
    IfThen(APercentualMulta > 0, '2', '0') +                   // Se campo 66 = 2 considerar percentual de multa
    FormataValor(APercentualMulta, 4) +                        // Percentual de multa
    FormataComZero(ATituloBanco, 11) +                         // Identificação do título no banco
    ANossoNumeroDV +                                           // Digito de auto conferencia do nosso número
    FormataValor(ADescBonificacaoPorDia, 10) +                 // Desconto Bonificação por dia
    AEmissaoPapeletaCobr +                                     // Condição para emissão da papeleta de cobrança
    AEmitePapeletaDebito +                                     // Ident. se emite papeleta para débito automático
    Espacos(10) +                                              // Identificação da operação do banco
    ARateioCredito +                                           // Indicador rateio crédito
    AAvisoDebitoContaCorrente +                                // Endereçamento para aviso do débito automático em conta corrente
    Espacos(2) +                                               // Branco
    FormataComZero(AOcorrencia, 2) +                           // Identificação ocorrência
    FormataComEspaco(ANumDocumento, 10) +                      // Nº do Documento
    VDataVencto +                                              // Data do vencimento do título
    FormataValor(AValorTitulo, 13) +                           // Valor do título
    FormataComZero(ABancoCobranca, 3) +                        // Banco encarregado da cobrança
    FormataComZero(AAgenciaDepositaria, 5) +                   // Agência depositária
    FormataComZero(AEspecieTitulo, 2) +                        // Espécie de título
    AIdentificacao +                                           // Identificação
    FormataData(ADataEmissaoTitulo) +                          // Data da emissão do título
    FormataComZero(APrimeiraInstrucao, 2) +                    // Vide observação no layout do Bradesco
    FormataComZero(ASegundaInstrucao, 2) +                     // Vide observação no layout do Bradesco
    FormataValor(AValorCobradoPorDiaAtraso, 13) +              // Valor a ser cobrado por dia de atraso
    IfThen(ADataLimiteConcessaoDesc < EncodeDate(2000, 01, 01), '000000',
      FormataData(ADataLimiteConcessaoDesc)) +                 // Data limite para concessão de desconto
    FormataValor(AAValorDesconto, 13) +                        // Valor do desconto
    FormataValor(AValorIOF, 13) +                              // Valor do IOF
    FormataValor(AValorAbatimento, 13) +                       // Valor do abatimento a ser concedido ou cancelado
    FormataComZero(ATipoInscricaoSacado, 2) +                  // Identificação do tipo de inscrição do sacado
    FormataComZero(ANumInscricaoSacado, 14) +                  // Nº Inscrição do sacado
    FormataComEspacoR(UpperCase(ANomeSacado), 40) +            // Nome do sacado
    FormataComEspacoR(UpperCase(AEnderecoCompleto), 40) +      // Endereço completo
    FormataComEspacoR(UpperCase(APrimeiraMensagem), 12) +      // 1ª Mensagem
    FormataComZero(ACEP, 5) +                                  // CEP
    FormataComZero(ASufixoCEP, 3) +                            // Sufixo do CEP
    FormataComEspacoR(UpperCase(ASacadorAvalistaSegMsg), 60) + // Sacador / Avalista ou 2ª mensagem
    FormataComZero(IntToStr(ANumSequencialRegistro), 6);       // Nº sequencial do registro
end;

function TZBoletoModeloBradesco.GeraTraillerRemessaCNAB400(
  const ANumSequencialRegistro: Integer): string;
begin
  Result :=
    '9' +                                                // Identificação Registro
    Espacos(393) +                                       // Branco
    FormataComZero(IntToStr(ANumSequencialRegistro), 6); // Nº sequencial do último registro
end;

procedure TZBoletoModeloBradesco.GeraHeaderRetornoCNAB400(ADados: TStream;
  ARetorno: TStrings; ACampos: TJSONObject);
var
  VDados: string;
begin
  ADados.Seek(0, 0);
  SetLength(VDados, 400);
  if ADados.Read(VDados[1], 400) < 400 then
    raise EZBoleto.Create('Registro Header Label inválido.');
  ARetorno.Text := VDados;
  if Copy(VDados, 77, 3) <> '237' then
    raise EZBoleto.Create('Dados de retorno inválidos.');
  ACampos.Integers['codigo_empresa'] := LeInt(VDados, 27, 20);          // Código da empresa
  ACampos.Strings['nome_empresa'] := LeStr(VDados, 47, 30);             // Nome da empresa por extenso
  ACampos.Floats['data_gravacao_arquivo'] := LeData(VDados, 95);        // Data da gravação do arquivo
  ACampos.Integers['num_aviso_bancario'] := LeInt(VDados, 109, 5);      // Nº aviso bancário
  ACampos.Floats['data_credito'] := LeData(VDados, 380);                // Data do crédito
  ACampos.Integers['num_sequencial_registro'] := LeInt(VDados, 395, 6); // Nº sequencial de registro
end;

procedure TZBoletoModeloBradesco.GeraDetalhesRetornoCNAB400(ADados: TStream;
  ARetorno: TStrings; AItens: TJSONArray);
var
  I: Integer;
  VDados, VEmprCedenteBanco: string;
begin
  ADados.Seek(402, 0);
  SetLength(VDados, ADados.Size - 806);
  if ADados.Read(VDados[1], Length(VDados)) < 400 then
    raise EZBoleto.Create('Registro de Transação inválido.');
  ARetorno.Text := VDados;
  for I := 0 to Pred(ARetorno.Count) do
  begin
    VDados := ARetorno[I];
    if Copy(VDados, 18, 3) <> '000' then
      raise EZBoleto.Create('Dados de retorno inválidos.');
    VEmprCedenteBanco := LeStr(VDados, 21, 17);
    AItens.Add(TJSONObject.Create([
      'tipo_inscricao_empresa', LeInt(VDados, 2, 2),           // Tipo de inscrição empresa
      'num_inscricao_empresa', LeInt64(VDados, 1, 14),         // Nº inscrição da empresa
      'empresa_cedente_banco', TJSONObject.Create([
        'carteira', LeInt(VEmprCedenteBanco, 2, 3),//int,
        'agencia', LeInt(VEmprCedenteBanco, 5, 5),//int,
        'conta', LeInt(VEmprCedenteBanco, 10, 7),//int
        'conta_dv', LeStr(VEmprCedenteBanco, 17, 1)
      ]),                                                      // Identificação da empresa cedente no banco
      'num_controle_participante', LeStr(VDados, 38, 25),      // Nº controle do participante
      'titulo_banco', LeStr(VDados, 71, 12),                   // Identificação do título no banco
      'rateio_credito', LeStr(VDados, 105, 1),                 // Indicador de rateio crédito
      'carteira', LeInt(VDados, 108, 1),                       // Carteira
      'ocorrencia', LeInt(VDados, 109, 2),                     // Identificação de ocorrência
      'data_ocorrencia_banco', LeData(VDados, 111),            // Data ocorrência no banco
      'numero_documento', LeStr(VDados, 117, 10),              // Número do documento
      'titulo_banco_2', LeStr(VDados, 127, 20),                // Identificação do título no banco
      'data_vencimento_titulo', LeData(VDados, 147),           // Data vencimento do título
      'valor_titulo', LeCurr(VDados, 153, 13),                 // Valor do título
      'banco_cobrador', LeInt(VDados, 166, 3),                 // Banco cobrador
      'agencia_cobradora', LeInt(VDados, 169, 5),              // Agência cobradora
      'despesas_cobr_cod_ocorrencia', LeCurr(VDados, 176, 13), // Despesas de cobrança para os códigos de ocorrência: 02 - Entrada confirmada; 28 - Débito de tarifas
      'outras_despesas', LeCurr(VDados, 189, 13),              // Outras despesas custas de protesto
      'juros_operacao_atraso', LeFloat(VDados, 202, 13),       // Juros operação em atraso
      'iof_devido', LeCurr(VDados, 215, 13),                   // IOF devido
      'abatimento_concedido_titulo', LeCurr(VDados, 228, 13),  // Abatimento concedido sobre o título
      'desconto_concedido', LeCurr(VDados, 241, 13),           // Desconto concedido
      'valor_pago', LeCurr(VDados, 254, 13),                   // Valor pago
      'juros_mora', LeCurr(VDados, 267, 13),                   // Juros de mora
      'outros_creditos', LeCurr(VDados, 280, 13),              // Outros créditos
      'motivo_codigo_ocorrencia_19', LeStr(VDados, 295, 1),    // Motivo do código de ocorrência 19 (confirmação de instrução de protesto)
      'data_credito', LeData(VDados, 296),                     // Data do crédito
      'origem_pagamento', LeInt(VDados, 302, 3),               // Origem pagamento
      'quando_cheque_bradesco', LeInt(VDados, 315, 4),         // Quando cheque Bradesco informe 0237
      'motivos_rejeicoes', LeInt64(VDados, 319, 10),           // Motivos das rejeições para os códigos de ocorrência
      'numero_cartorio', LeInt(VDados, 369, 2),                // Número do cartório
      'numero_protocolo', LeStr(VDados, 371, 10),              // Número do protocolo
      'num_sequencial_registro', LeInt(VDados, 395, 6)         // Nº sequencial de registro
    ]));
  end;
end;

procedure TZBoletoModeloBradesco.GeraTraillerRetornoCNAB400(ADados: TStream;
  ARetorno: TStrings; ACampos: TJSONObject);
var
  VDados: string;
begin
  ADados.Seek(ADados.Size - 402, 0);
  SetLength(VDados, 402);
  if ADados.Read(VDados[1], Length(VDados)) < 400 then
    raise EZBoleto.Create('Registro Trailler inválido.');
  ARetorno.Text := VDados;
  if Copy(VDados, 5, 3) <> '237' then
    raise EZBoleto.Create('Dados trailler inválidos.');
  ACampos.Integers['quant_titulos_cobr'] := LeInt(VDados, 18, 8);                // Quantidade de títulos em cobrança
  ACampos.Floats['valor_total_cobranca'] := LeCurr(VDados, 26, 14);              // Valor total em cobrança
  ACampos.Integers['num_aviso_bancario'] := LeInt(VDados, 40, 8);                // Nº do aviso bancário
  ACampos.Integers['quant_reg_confirm_entradas'] := LeInt(VDados, 58, 5);        // Quantidade de registros - Ocorrência 02 – Confirmação de entradas
  ACampos.Floats['valor_reg_confirm_entradas'] := LeCurr(VDados, 63, 12);        // Valor dos registros – Ocorrência 02 - Confirmação de entradas
  ACampos.Floats['valor_reg_liquidacao'] := LeCurr(VDados, 75, 12);              // Valor dos registros – Ocorrência 06 – Liquidação
  ACampos.Integers['quant_reg_liquidacao'] := LeInt(VDados, 87, 5);              // Quantidade dos registros - Ocorrência 06 – Liquidação
  ACampos.Floats['valor_reg_liquidacao'] := LeCurr(VDados, 92, 12);              // Valor dos Registros - Ocorrência 06 - Liquidação
  ACampos.Integers['quant_reg_titulos_baixados'] := LeInt(VDados, 104, 5);       // Quantidade dos Registros - Ocorrência 09 e 10 - Títulos baixados
  ACampos.Floats['valor_reg_titulos_baixados'] := LeCurr(VDados, 109, 12);       // Valor dos Registros – Ocorrência 09 e 10 - Títulos baixados
  ACampos.Integers['quant_reg_abatimento_cancelado'] := LeInt(VDados, 121, 5);   // Quantidade de registros - Ocorrência 13 - Abatimento cancelado
  ACampos.Floats['valor_reg_abatimento_cancelado'] := LeCurr(VDados, 126, 12);   // Valor dos registros – Ocorrência 13 - Abatimento cancelado
  ACampos.Integers['quant_reg_vencto_alterado'] := LeInt(VDados, 138, 5);        // Quantidade dos registros - Ocorrência 14 – Vencimento alterado
  ACampos.Floats['valor_reg_vencto_alterado'] := LeCurr(VDados, 143, 12);        // Valor dos registros - Ocorrência 14 - Vencimento alterado
  ACampos.Integers['quant_reg_abatimento_concedido'] := LeInt(VDados, 155, 5);   // Quantidade dos registros - Ocorrência 12 – Abatimento concedido
  ACampos.Floats['valor_reg_abatimento_concedido'] := LeCurr(VDados, 160, 12);   // Valor dos registros – Ocorrência 12 - Abatimento concedido
  ACampos.Integers['quant_reg_confirm_instr_protesto'] := LeInt(VDados, 172, 5); // Quantidade dos registros - Ocorrência 19 - Confirmação da instrução protesto
  ACampos.Floats['valor_reg_confirm_instr_protesto'] := LeCurr(VDados, 177, 12); // Valor dos registros – Ocorrência 19 - Confirmação da instrução de protesto
  ACampos.Floats['valor_total_rateios_efetuados'] := LeCurr(VDados, 363, 15);    // Valor total dos rateios efetuados
  ACampos.Integers['quant_total_rateios_efetuados'] := LeInt(VDados, 378, 8);    // Quantidade total dos rateios efetuados
  ACampos.Integers['num_sequencial_registro'] := LeInt(VDados, 395, 6);          // Número sequencial do registro
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

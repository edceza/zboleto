program ex_bradesco;

{$mode objfpc}{$H+}

uses
  ZBoleto, ZBoletoModelo_Bradesco, ZBoletoUteis, Classes, SysUtils, FPJSON;

var
  Seq: Integer;
  Dados, Carteira, NossoNumero: string;
  Boleto: TZBoleto;
  Remessa: TMemoryStream;
  Modelo: TZBoletoModeloBase;
  CamposItens: TJSONArray;
  CamposHeader, CamposDetalhe, CamposTrailler: TJSONObject;
begin
  Boleto := TZBoleto.Create(nil);
  Modelo := Boleto.CriaModelo('bradesco');
  Remessa := TMemoryStream.Create;
  CamposHeader := TJSONObject.Create;
  CamposDetalhe := TJSONObject.Create;
  CamposItens := TJSONArray.Create;
  CamposTrailler := TJSONObject.Create;
  try
    // Registro Header Label
    Seq := Modelo.GeraSequenciaArquivo;
    Carteira := FormataComZero('9', 3);
    NossoNumero := FormataComZero(IntToStr(Seq), 11);
    CamposHeader.Add('codigo_empresa', '9999999'); // Será fornecido pelo Bradesco, quando do cadastramento
    CamposHeader.Add('nome_empresa', 'RAZAO SOCIAL'); // Razão social
    CamposHeader.Add('data_gravacao_arquivo', DataNula); // Data da gravação do arquivo
    CamposHeader.Add('num_sequencial_remessa', Seq); // Sequencial
    Dados := Modelo.GeraHeaderRemessa(CamposHeader, tc400) + CRLF;
    Remessa.Write(Dados[1], Length(Dados));

    // Registro de Transação - Tipo 1
    Inc(Seq);
    CamposDetalhe.Add('agencia_debito', ''); // Código da agência do sacado exclusivo para débito em conta
    CamposDetalhe.Add('agencia_debito_dv', ''); // Dígito da agência do sacado
    CamposDetalhe.Add('razao_conta_corrente', ''); // Razão da conta do sacado
    CamposDetalhe.Add('conta_corrente', ''); // Número da conta do sacado
    CamposDetalhe.Add('conta_corrente_dv', ''); // Dígito da conta corrente
    CamposDetalhe.Add('empresa', TJSONObject.Create(['carteira', Carteira,
      'agencia', 9999, 'conta', 9999, 'conta_dv', 9])); // Carteira + Agência + Conta corrente
    CamposDetalhe.Add('num_controle_participante', ''); // Uso da empresa
    CamposDetalhe.Add('codigo_banco', '999'); // Nº do banco "237"
    CamposDetalhe.Add('percentual_multa', 0); // Percentual de multa a ser considerado
    CamposDetalhe.Add('titulo_banco', NossoNumero); // Nosso Número para cobrança com e sem registro
    CamposDetalhe.Add('nosso_numero_dv', TZBoletoModeloBradesco(Modelo
      ).DigitoVerificadorNossoNumero(Carteira + NossoNumero)); // Digito N/N
    CamposDetalhe.Add('desc_bonificacao_por_dia', 0); // Valor do desconto bonificação por dia
    CamposDetalhe.Add('emissao_papeleta_cobr', '2'); // 1 = Banco emite e processa o registro
                                                     // 2 = Cliente emite e o banco somente processa o registro
    CamposDetalhe.Add('emite_papeleta_debito', 'N'); // N = Não registra na cobrança e diferente de N registra e emite papeleta
    CamposDetalhe.Add('rateio_credito', ''); // Indicador Rateio Crédito
    CamposDetalhe.Add('aviso_debito_conta_corrente', ''); // Endereçamento para aviso do débito automático em conta corrente
    CamposDetalhe.Add('ocorrencia', '01'); // Códigos de ocorrência
    CamposDetalhe.Add('num_documento', '1'); // Documento
    CamposDetalhe.Add('data_vencto_titulo', Date + 5); // DDMMAA
    CamposDetalhe.Add('valor_titulo', 2.95); // Valor do título
    CamposDetalhe.Add('banco_cobranca', ''); // Nº do banco na câmara de compensação
    CamposDetalhe.Add('agencia_depositaria', ''); // Código da agência depositária
    CamposDetalhe.Add('especie_titulo', '01'); // 01 - Duplicata
                                               // 02 - Nota Promissória
                                               // 03 - Nota de Seguro
                                               // 04 - Cobrança Seriada
                                               // 05 - Recibo
                                               // 10 - Letras de Câmbio
                                               // 11 - Nota de Débito
                                               // 12 - Duplicata de Serv.
                                               // 99 - Outros
    CamposDetalhe.Add('identificacao', ''); // A – aceito
                                            // N - não aceito
    CamposDetalhe.Add('data_emissao_titulo', Date); // DDMMAA
    CamposDetalhe.Add('primeira_instrucao', ''); // Vide observação no layout do Bradesco
    CamposDetalhe.Add('segunda_instrucao', ''); // Vide observação no layout do Bradesco
    CamposDetalhe.Add('valor_cobrado_por_dia_atraso', 0); // Valor a ser cobrado por dia de atraso
    CamposDetalhe.Add('valor_desconto', 0); // Valor desconto
    CamposDetalhe.Add('valor_iof', 0); // Valor do IOF
    CamposDetalhe.Add('valor_abatimento', 0); // Valor abatimento
    CamposDetalhe.Add('tipo_inscricao_sacado', '01'); // 01 - CPF
                                                      // 02 - CNPJ
                                                      // 03 - PIS/PASEP
                                                      // 98 - Não tem
                                                      // 99 - Outros
    CamposDetalhe.Add('num_inscricao_sacado', '99999999999999'); // CNPJ / CPF
    CamposDetalhe.Add('nome_sacado', 'SACADO SILVA LTDA ME'); // Nome do sacado
    CamposDetalhe.Add('endereco_completo', 'Rua ABC 100 Centro Guarulhos Sao Paulo'); // Endereço do sacado
    CamposDetalhe.Add('primeira_mensagem', ''); // Vide observação no layout do Bradesco
    CamposDetalhe.Add('cep', '99999999'); // CEP sacado
    CamposDetalhe.Add('sufixo_cep', '000'); // Sufixo CEP
    CamposDetalhe.Add('sacador_avalista_seg_msg', ''); // Decomposição
    CamposDetalhe.Add('num_sequencial_registro', Seq); // Nº sequencial do registro
    CamposItens.Add(CamposDetalhe.Clone);
    Dados := Modelo.GeraDetalhesRemessa(CamposItens, tc400) + CRLF;
    Remessa.Write(Dados[1], Length(Dados));

    // Registro Trailler
    Inc(Seq);
    CamposTrailler.Add('num_sequencial_registro', Seq);
    Dados := Modelo.GeraTraillerRemessa(CamposTrailler, tc400);
    Remessa.Write(Dados[1], Length(Dados));

    Remessa.SaveToFile(Modelo.GeraNomeArquivoRemessa);
  finally
    CamposHeader.Free;
    CamposDetalhe.Free;
    CamposItens.Free;
    CamposTrailler.Free;
    Remessa.Free;
    FreeAndNil(Modelo);
    Boleto.Free;
  end;
end.


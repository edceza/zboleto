program ex_bb;

{$mode objfpc}{$H+}

uses
  ZBoleto, ZBoletoAnalisadorHtml, ZBoletoModelo_BB, SysUtils;

var
  Boleto: TZBoleto;
  DiasPrazoPagamento: Byte;
  ValorCobrado, TaxaBoleto: Currency;
  DataVenc, ValorBoleto: string;
begin
  Boleto := TZBoleto.Create(nil);
  try
    ZBoletoAnalisadorHtml.DirModelos := '../../../../modelos/html';
    Boleto.Campos.Add('dir_img', '../../../../imagens/');

    // DADOS DO BOLETO PARA O SEU CLIENTE
    DiasPrazoPagamento := 5;
    TaxaBoleto := 2.95;
    DataVenc := FormatDateTime('dd/mm/yyyy', Date + DiasPrazoPagamento); // Prazo de X dias OU informe data: '13/04/2006';
    ValorCobrado := 2950; // Valor - REGRA: Sem pontos na milhar e tanto faz com "." ou "," ou com 1 ou 2 ou sem casa decimal
    ValorBoleto := FormatFloat('0.00', ValorCobrado + TaxaBoleto);

    Boleto.Campos.Add('nosso_numero', '87654');
    Boleto.Campos.Add('numero_documento', '27.030195.10'); // Num do pedido ou do documento
    Boleto.Campos.Add('data_vencimento', DataVenc); // Data de Vencimento do Boleto - REGRA: Formato DD/MM/AAAA
    Boleto.Campos.Add('data_documento', FormatDateTime('dd/mm/yyyy', Date)); // Data de emissão do Boleto
    Boleto.Campos.Add('data_processamento', FormatDateTime('dd/mm/yyyy', Date)); // Data de processamento do boleto (opcional)
    Boleto.Campos.Add('valor_boleto', ValorBoleto); // Valor do Boleto - REGRA: Com vírgula e sempre com duas casas depois da virgula

    // DADOS DO SEU CLIENTE
    Boleto.Campos.Add('sacado', 'Nome do seu Cliente');
    Boleto.Campos.Add('endereco1', 'Endereço do seu Cliente');
    Boleto.Campos.Add('endereco2', 'Cidade - Estado -  CEP: 00000-000');

    // INFORMACOES PARA O CLIENTE
    Boleto.Campos.Add('demonstrativo1', 'Pagamento de Compra na Loja Nonononono');
    Boleto.Campos.Add('demonstrativo2', 'Mensalidade referente a nonon nonooon nononon<br>Taxa bancária - R$ ' + FormatFloat('0.00', TaxaBoleto));
    Boleto.Campos.Add('demonstrativo3', 'ZBoleto - https://github.com/silvioprog/zboleto');

    // INSTRUÇÕES PARA O CAIXA
    Boleto.Campos.Add('instrucoes1', '- Sr. Caixa, cobrar multa de 2% após o vencimento');
    Boleto.Campos.Add('instrucoes2', '- Receber até 10 dias após o vencimento');
    Boleto.Campos.Add('instrucoes3', '- Em caso de dúvidas entre em contato conosco: xxxx@xxxx.com.br');
    Boleto.Campos.Add('instrucoes4', '&nbsp; Emitido pelo sistema ZBoletoPhp - github.com/silvioprog/zboleto');

    // DADOS OPCIONAIS DE ACORDO COM O BANCO OU CLIENTE
    Boleto.Campos.Add('quantidade', '10');
    Boleto.Campos.Add('valor_unitario', '10');
    Boleto.Campos.Add('aceite', 'N');
    Boleto.Campos.Add('especie', 'R$');
    Boleto.Campos.Add('especie_doc', 'DM');

    // ---------------------- DADOS FIXOS DE CONFIGURAÇÃO DO SEU BOLETO --------------- //

    // DADOS DA SUA CONTA - BANCO DO BRASIL
    Boleto.Campos.Add('agencia', '9999'); // Num da agencia, sem digito
    Boleto.Campos.Add('conta', '99999'); // Num da conta, sem digito

    // DADOS PERSONALIZADOS - BANCO DO BRASIL
    Boleto.Campos.Add('convenio', '7777777'); // Num do convênio - REGRA: 6 ou 7 ou 8 dígitos
    Boleto.Campos.Add('contrato', '999999'); // Num do seu contrato
    Boleto.Campos.Add('carteira', '18');
    Boleto.Campos.Add('variacao_carteira', '-019'); // Variação da Carteira, com traço (opcional)

    // TIPO DO BOLETO
    Boleto.Campos.Add('formatacao_convenio', '7'); // REGRA: 8 p/ Convênio c/ 8 dígitos, 7 p/ Convênio c/ 7 dígitos, ou 6 se Convênio c/ 6 dígitos
    Boleto.Campos.Add('formatacao_nosso_numero', '2'); // REGRA: Usado apenas p/ Convênio c/ 6 dígitos: informe 1 se for NossoNúmero de até 5 dígitos ou 2 para opção de até 17 dígitos

    (*
    #################################################
    DESENVOLVIDO PARA CARTEIRA 18

    - Carteira 18 com Convenio de 8 digitos
      Nosso número: pode ser até 9 dígitos

    - Carteira 18 com Convenio de 7 digitos
      Nosso número: pode ser até 10 dígitos

    - Carteira 18 com Convenio de 6 digitos
      Nosso número:
      de 1 a 99999 para opção de até 5 dígitos
      de 1 a 99999999999999999 para opção de até 17 dígitos
    #################################################
    *)

    // SEUS DADOS
    Boleto.Campos.Add('identificacao', 'ZBoleto - Gerador de boletos bancários');
    Boleto.Campos.Add('cpf_cnpj', '');
    Boleto.Campos.Add('endereco', 'Coloque o endereço da sua empresa aqui');
    Boleto.Campos.Add('cidade_uf', 'Cidade / Estado');
    Boleto.Campos.Add('cedente', 'Coloque a Razão Social da sua empresa aqui');

    Boleto.Executa('bb', 'html', 'boleto_bb.html');
  finally
    Boleto.Free;
  end;
end.


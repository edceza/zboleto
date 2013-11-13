program ex_bradesco;

{$mode objfpc}{$H+}

uses
  ZBoleto, ZBoletoAnalisadorHtml, ZBoletoModelo_Bradesco, Classes, SysUtils;

var
  Boleto: TZBoleto;
  Conteudo: TMemoryStream;
  DiasPrazoPagamento: Byte;
  ValorCobrado, TaxaBoleto: Currency;
  DataVenc, ValorBoleto, Html: string;
begin
  Boleto := TZBoleto.Create(nil);
  Conteudo := TMemoryStream.Create;
  try
    ZBoletoAnalisadorHtml.DirModelos := '../../modelos/html';
    Boleto.Campos.Add('dir_img', '../../imagens/');

    // DADOS DO BOLETO PARA O SEU CLIENTE
    DiasPrazoPagamento := 5;
    TaxaBoleto := 2.95;
    DataVenc := FormatDateTime('dd/mm/yyyy', Date + DiasPrazoPagamento); // Prazo de X dias OU informe data: '13/04/2006';
    ValorCobrado := 2950; // Valor - REGRA: Sem pontos na milhar e tanto faz com "." ou "," ou com 1 ou 2 ou sem casa decimal
    ValorBoleto := FormatFloat('0.00', ValorCobrado + TaxaBoleto);

    Boleto.Campos.Add('nosso_numero', '75896452'); // Nosso numero sem o DV - REGRA: Máximo de 11 caracteres!
    Boleto.Campos.Add('numero_documento', Boleto.Campos['nosso_numero'].AsString); // Num do pedido ou do documento = Nosso numero
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
    Boleto.Campos.Add('quantidade', '001');
    Boleto.Campos.Add('valor_unitario', ValorBoleto);
    Boleto.Campos.Add('aceite', '');
    Boleto.Campos.Add('especie', 'R$');
    Boleto.Campos.Add('especie_doc', 'DS');

    // ---------------------- DADOS FIXOS DE CONFIGURAÇÃO DO SEU BOLETO --------------- //

    // DADOS DA SUA CONTA - Bradesco
    Boleto.Campos.Add('agencia', '1172'); // Num da agencia, sem digito
    Boleto.Campos.Add('agencia_dv', '0'); // Digito do Num da agencia
    Boleto.Campos.Add('conta', '0403005'); // Num da conta, sem digito
    Boleto.Campos.Add('conta_dv', '2'); // Digito do Num da conta

    // DADOS PERSONALIZADOS - Bradesco
    Boleto.Campos.Add('conta_cedente', '0403005'); // ContaCedente do Cliente, sem digito (Somente Números)
    Boleto.Campos.Add('conta_cedente_dv', '2'); // Digito da ContaCedente do Cliente
    Boleto.Campos.Add('carteira', '06'); // Código da Carteira: pode ser 06 ou 03

    // SEUS DADOS
    Boleto.Campos.Add('identificacao', 'ZBoleto - Gerador de boletos bancários');
    Boleto.Campos.Add('cpf_cnpj', '');
    Boleto.Campos.Add('endereco', 'Coloque o endereço da sua empresa aqui');
    Boleto.Campos.Add('cidade_uf', 'Cidade / Estado');
    Boleto.Campos.Add('cedente', 'Coloque a Razão Social da sua empresa aqui');

    Html := Boleto.Executa('bradesco', 'html');
    Conteudo.Write(Html[1], Length(Html));
    Conteudo.SaveToFile('boleto_bradesco.html');
  finally
    Conteudo.Free;
    Boleto.Free;
  end;
end.

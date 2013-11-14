program ex_sicredi;

{$mode objfpc}{$H+}

uses
  ZBoleto, ZBoletoAnalisadorHtml, ZBoletoModelo_SICREDI, Classes, SysUtils;

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

    Boleto.Campos.Add('inicio_nosso_numero', FormatDateTime('yy', Date)); // Ano da geração do título ex: 07 para 2007
    Boleto.Campos.Add('nosso_numero', '13871'); // Nosso numero (máx. 5 digitos) - Numero sequencial de controle
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
    Boleto.Campos.Add('quantidade', '');
    Boleto.Campos.Add('valor_unitario', '');
    Boleto.Campos.Add('aceite', 'N');
    Boleto.Campos.Add('especie', 'R$');
    Boleto.Campos.Add('especie_doc', 'A');

    // ---------------------- DADOS FIXOS DE CONFIGURAÇÃO DO SEU BOLETO --------------- //

    // DADOS DA SUA CONTA - SICREDI
    Boleto.Campos.Add('agencia', '1234'); // Num da agencia (4 digitos), sem Digito Verificador
    Boleto.Campos.Add('conta', '12345'); // Num da conta (5 digitos), sem Digito Verificador
    Boleto.Campos.Add('conta_dv', '6'); // Digito Verificador do Num da conta

    // DADOS PERSONALIZADOS - SICREDI
    Boleto.Campos.Add('posto', '18'); // Código do posto da cooperativa de crédito
    Boleto.Campos.Add('byte_idt', '2'); // Byte de identificação do cedente do bloqueto utilizado para compor o nosso número.
    Boleto.Campos.Add('carteira', 'A'); // Código da Carteira: A (Simples)

    // SEUS DADOS
    Boleto.Campos.Add('identificacao', 'ZBoleto - Gerador de boletos bancários');
    Boleto.Campos.Add('cpf_cnpj', '');
    Boleto.Campos.Add('endereco', 'Coloque o endereço da sua empresa aqui');
    Boleto.Campos.Add('cidade_uf', 'Cidade / Estado');
    Boleto.Campos.Add('cedente', 'Coloque a Razão Social da sua empresa aqui');

    Html := Boleto.Executa('sicredi', 'html');
    Conteudo.Write(Html[1], Length(Html));
    Conteudo.SaveToFile('boleto_sicredi.html');
  finally
    Conteudo.Free;
    Boleto.Free;
  end;
end.

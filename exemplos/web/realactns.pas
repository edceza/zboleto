unit realactns;

{$mode objfpc}{$H+}

interface

uses
  BrookAction, ZBoleto, ZBoletoAnalisadorHtml, ZBoletoModelo_Real, SysUtils;

type
  TBoletoRealAction = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

procedure TBoletoRealAction.Get;
var
  Boleto: TZBoleto;
  DiasPrazoPagamento: Byte;
  DataVenc, ValorBoleto: string;
  ValorCobrado, TaxaBoleto: Currency;
begin
  Boleto := TZBoleto.Create(nil);
  try
    ZBoletoAnalisadorHtml.DirModelos := './modelos/html';
    Boleto.Campos.Add('dir_img', '/imagens/');

    // DADOS DO BOLETO PARA O SEU CLIENTE
    DiasPrazoPagamento := 5;
    TaxaBoleto := 2.95;
    DataVenc := FormatDateTime('dd/mm/yyyy', Date + DiasPrazoPagamento); // Prazo de X dias OU informe data: '13/04/2006';
    ValorCobrado := 2950; // Valor - REGRA: Sem pontos na milhar e tanto faz com "." ou "," ou com 1 ou 2 ou sem casa decimal
    ValorBoleto := FormatFloat('0.00', ValorCobrado + TaxaBoleto);

    Boleto.Campos.Add('nosso_numero', '0000000123456'); // Nosso numero - REGRA: Máximo de 13 caracteres!
    Boleto.Campos.Add('numero_documento', '1234567'); // Num do pedido ou do documento
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
    Boleto.Campos.Add('especie_doc', '');

    // ---------------------- DADOS FIXOS DE CONFIGURAÇÃO DO SEU BOLETO --------------- //

    // DADOS DA SUA CONTA - REAL
    Boleto.Campos.Add('agencia', '1234'); // Num da agencia, sem digito
    Boleto.Campos.Add('conta', '0012345'); // Num da conta, sem digito
    Boleto.Campos.Add('carteira', '57'); // Código da Carteira

    // SEUS DADOS
    Boleto.Campos.Add('identificacao', 'ZBoleto - Gerador de boletos bancários');
    Boleto.Campos.Add('cpf_cnpj', '');
    Boleto.Campos.Add('endereco', 'Coloque o endereço da sua empresa aqui');
    Boleto.Campos.Add('cidade_uf', 'Cidade / Estado');
    Boleto.Campos.Add('cedente', 'Coloque a Razão Social da sua empresa aqui');

    Write(Boleto.Executa('real', 'html'));
  finally
    Boleto.Free;
  end;
end;

initialization
  TBoletoRealAction.Register('*');

end.

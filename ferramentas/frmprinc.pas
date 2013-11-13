unit frmprinc;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, EditBtn, StdCtrls, Classes, Dialogs, RUtils;

type

  { TfrPrinc }

  TfrPrinc = class(TForm)
    btConverter: TButton;
    edModeloBP: TFileNameEdit;
    edModeloZB: TFileNameEdit;
    lbModeloZB: TLabel;
    lbModeloBP: TLabel;
    procedure btConverterClick(Sender: TObject);
    procedure edModeloBPEditingDone(Sender: TObject);
  public
    procedure Converter;
  end;

var
  frPrinc: TfrPrinc;

implementation

{$R *.lfm}

{ TfrPrinc }

procedure TfrPrinc.edModeloBPEditingDone(Sender: TObject);
var
  S: string;
begin
  if edModeloZB.FileName <> '' then
    Exit;
  S := edModeloBP.FileName;
  if S <> '' then
  begin
    S := ExtractFilePath(S) + 'modelo' + Copy(S, Pos('_', S), MaxInt) + '.html';
    Delete(S, Pos('.php', S), 4);
    edModeloZB.FileName := S;
  end;
end;

procedure TfrPrinc.btConverterClick(Sender: TObject);
begin
  Converter;
end;

procedure TfrPrinc.Converter;

  function _SubstPart(const S, A, N: string): string;
  begin
    Result := StringReplace(S, A, N, [rfIgnoreCase, rfReplaceAll]);
  end;

  function _SubstCodigo(const S, A, N: string; const Prefixo: string = ''): string;
  begin
    Result := StringReplace(S, A, Prefixo + '<% ' + N + ' %>', [rfIgnoreCase, rfReplaceAll]);
  end;

  function _SubstImg(const S, A: string): string;
  begin
    Result := StringReplace(S, A, '<% dir_img %>' + ExtractFileName(A), [rfIgnoreCase, rfReplaceAll]);
  end;

var
  _p, S: string;
  C: SizeInt;
  Ori, Dest: TStringList;
begin
  Ori := TStringList.Create;
  Dest := TStringList.Create;
  try
    Ori.LoadFromFile(edModeloBP.FileName);
    S := AnsiToUtf8(Ori.Text);

    _p := '?>' + #13#10 + #13#10;
    Delete(S, Pos('<?php', S), Pos(_p, S) + Length(_p) - 1);

    S := _SubstPart(S, '<meta http-equiv=Content-Type content=text/html charset=ISO-8859-1>', '<meta http-equiv=Content-Type content=text/html charset=UTF-8>');

    S := _SubstPart(S, '<HTML', '<html');
    S := _SubstPart(S, '<HEAD', '<head');
    S := _SubstPart(S, '<TITLE', '<title');
    S := _SubstPart(S, '<META', '<meta');
    S := _SubstPart(S, '<BODY', '<body');
    S := _SubstPart(S, '<TBODY', '<tbody');
    S := _SubstPart(S, '<DIV', '<div');
    S := _SubstPart(S, '<FONT', '<font');
    S := _SubstPart(S, '<TABLE', '<table');
    S := _SubstPart(S, '<TD', '<td');
    S := _SubstPart(S, '<TR', '<tr');

    S := _SubstPart(S, 'HTML>', 'html>');
    S := _SubstPart(S, 'HEAD>', 'head>');
    S := _SubstPart(S, 'TITLE>', 'title>');
    S := _SubstPart(S, 'META>', 'meta>');
    S := _SubstPart(S, 'BODY>', 'body>');
    S := _SubstPart(S, 'TBODY>', 'tbody>');
    S := _SubstPart(S, 'DIV>', 'div>');
    S := _SubstPart(S, 'FONT>', 'font>');
    S := _SubstPart(S, 'TABLE>', 'table>');
    S := _SubstPart(S, 'TD>', 'td>');
    S := _SubstPart(S, 'TR>', 'tr>');

    S := _SubstPart(S, '<IMG', '<img');
    S := _SubstPart(S, '<BR', '<br');
    S := _SubstPart(S, 'SRC=', 'src=');

    S := _SubstPart(S, 'ALIGN=CENTER', 'align="center"');
    S := _SubstPart(S, 'ALIGN="CENTER"', 'align="center"');
    S := _SubstPart(S, 'ALIGN=LEFT', 'align="left"');
    S := _SubstPart(S, 'ALIGN="LEFT"', 'align="left"');
    S := _SubstPart(S, 'ALIGN=RIGHT', 'align="right"');
    S := _SubstPart(S, 'ALIGN="RIGHT"', 'align="right"');

    S := _SubstImg(S, 'imagens/1.png');
    S := _SubstImg(S, 'imagens/2.png');
    S := _SubstImg(S, 'imagens/3.png');
    S := _SubstImg(S, 'imagens/6.png');
    S := _SubstImg(S, 'imagens/imgbrrazu.png');
    S := _SubstImg(S, 'imagens/imgbrrlrj.png');
    S := _SubstImg(S, 'imagens/imgpxlazu.png');

    S := _SubstImg(S, 'imagens/logo_empresa.png');
    S := _SubstImg(S, 'imagens/logobancoob.jpg');
    S := _SubstImg(S, 'imagens/logobanespa.jpg');
    S := _SubstImg(S, 'imagens/logobanestes.jpg');
    S := _SubstImg(S, 'imagens/logobb.jpg');
    S := _SubstImg(S, 'imagens/logobesc.jpg');
    S := _SubstImg(S, 'imagens/logobradesco.jpg');
    S := _SubstImg(S, 'imagens/logocaixa.jpg');
    S := _SubstImg(S, 'imagens/logohsbc.jpg');
    S := _SubstImg(S, 'imagens/logoitau.jpg');
    S := _SubstImg(S, 'imagens/logonossacaixa.jpg');
    S := _SubstImg(S, 'imagens/logoreal.jpg');
    S := _SubstImg(S, 'imagens/logosantander.jpg');
    S := _SubstImg(S, 'imagens/logosicredi.jpg');
    S := _SubstImg(S, 'imagens/logosudameris.jpg');
    S := _SubstImg(S, 'imagens/logounibanco.jpg');

    S := _SubstPart(S, '<meta name="Generator" content="Projeto BoletoPHP - www.boletophp.com.br - Licença GPL" />', '<meta name="generator" content="ZBoleto - https://github.com/silvioprog/zboleto" />');

    S := _SubstCodigo(S, '<?php echo isset($dadosboleto["cpf_cnpj"]) ? "<br>".$dadosboleto["cpf_cnpj"] : ''''?>', 'cpf_cnpj', '<br />');
    S := _SubstCodigo(S, '<?php echo isset($dadosboleto["cpf_cnpj"]) ? $dadosboleto["cpf_cnpj"] : ''''?>', 'cpf_cnpj');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["identificacao"]?>', 'identificacao');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["linha_digitavel"]?>', 'linha_digitavel');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["valor_boleto"]?>', 'valor_boleto');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["identificacao"]?>', 'identificacao');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["endereco"]?>', 'endereco');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["cidade_uf"]?>', 'cidade_uf');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["codigo_banco_com_dv"]?>', 'codigo_banco_com_dv');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["cedente"]?>', 'cedente');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["agencia_codigo"]?>', 'agencia_codigo');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["especie"]?>', 'especie');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["quantidade"]?>', 'quantidade');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["nosso_numero"]?>', 'nosso_numero');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["numero_documento"]?>', 'numero_documento');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["cpf_cnpj"]?>', 'cpf_cnpj');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["data_vencimento"]?>', 'data_vencimento');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["sacado"]?>', 'sacado');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["demonstrativo1"]?>', 'demonstrativo1');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["demonstrativo2"]?>', 'demonstrativo2');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["demonstrativo3"]?>', 'demonstrativo3');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["data_documento"]?>', 'data_documento');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["especie_doc"]?>', 'especie_doc');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["aceite"]?>', 'aceite');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["data_processamento"]?>', 'data_processamento');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["carteira"]?>', 'carteira');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["valor_unitario"]?>', 'valor_unitario');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["instrucoes1"]?>', 'instrucoes1');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["instrucoes2"]?>', 'instrucoes2');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["instrucoes3"]?>', 'instrucoes3');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["instrucoes4"]?>', 'instrucoes4');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["endereco1"]?>', 'endereco1');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["endereco2"]?>', 'endereco2');
    S := _SubstCodigo(S, '<?php echo ($data_venc != "") ? $dadosboleto["data_vencimento"] : "Contra Apresentação"?>', 'data_vencimento');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["contrato"]?>', 'contrato');
    S := _SubstCodigo(S, '<?php echo isset($dadosboleto["variacao_carteira"]) ? $dadosboleto["variacao_carteira"] : ''&nbsp;''?>', 'variacao_carteira');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["carteira_descricao"]?>', 'carteira_descricao');
    S := _SubstCodigo(S, '<?php fbarcode($dadosboleto["codigo_barras"])?>', 'codigo_barras');

    S := _SubstCodigo(S, '<?php echo isset($dadosboleto["cpf_cnpj"]) ? "<br>".$dadosboleto["cpf_cnpj"] : '''';?>', 'cpf_cnpj', '<br />');
    S := _SubstCodigo(S, '<?php echo isset($dadosboleto["cpf_cnpj"]) ? $dadosboleto["cpf_cnpj"] : '''';?>', 'cpf_cnpj');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["identificacao"];?>', 'identificacao');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["linha_digitavel"];?>', 'linha_digitavel');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["valor_boleto"];?>', 'valor_boleto');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["identificacao"];?>', 'identificacao');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["endereco"];?>', 'endereco');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["cidade_uf"];?>', 'cidade_uf');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["codigo_banco_com_dv"];?>', 'codigo_banco_com_dv');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["cedente"];?>', 'cedente');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["agencia_codigo"];?>', 'agencia_codigo');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["especie"];?>', 'especie');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["quantidade"];?>', 'quantidade');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["nosso_numero"];?>', 'nosso_numero');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["numero_documento"];?>', 'numero_documento');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["cpf_cnpj"];?>', 'cpf_cnpj');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["data_vencimento"];?>', 'data_vencimento');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["sacado"];?>', 'sacado');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["demonstrativo1"];?>', 'demonstrativo1');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["demonstrativo2"];?>', 'demonstrativo2');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["demonstrativo3"];?>', 'demonstrativo3');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["data_documento"];?>', 'data_documento');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["especie_doc"];?>', 'especie_doc');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["aceite"];?>', 'aceite');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["data_processamento"];?>', 'data_processamento');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["carteira"];?>', 'carteira');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["valor_unitario"];?>', 'valor_unitario');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["instrucoes1"];?>', 'instrucoes1');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["instrucoes2"];?>', 'instrucoes2');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["instrucoes3"];?>', 'instrucoes3');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["instrucoes4"];?>', 'instrucoes4');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["endereco1"];?>', 'endereco1');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["endereco2"];?>', 'endereco2');
    S := _SubstCodigo(S, '<?php echo ($data_venc != "") ? $dadosboleto["data_vencimento"] : "Contra Apresentação";?>', 'data_vencimento');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["contrato"];?>', 'contrato');
    S := _SubstCodigo(S, '<?php echo isset($dadosboleto["variacao_carteira"]) ? $dadosboleto["variacao_carteira"] : ''&nbsp;'';?>', 'variacao_carteira');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["carteira_descricao"];?>', 'carteira_descricao');
    S := _SubstCodigo(S, '<?php fbarcode($dadosboleto["codigo_barras"]);?>', 'codigo_barras');

    S := _SubstCodigo(S, '<?php echo isset($dadosboleto["cpf_cnpj"]) ? "<br>".$dadosboleto["cpf_cnpj"] : '''' ?>', 'cpf_cnpj', '<br />');
    S := _SubstCodigo(S, '<?php echo isset($dadosboleto["cpf_cnpj"]) ? $dadosboleto["cpf_cnpj"] : '''' ?>', 'cpf_cnpj');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["identificacao"] ?>', 'identificacao');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["linha_digitavel"] ?>', 'linha_digitavel');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["valor_boleto"] ?>', 'valor_boleto');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["identificacao"] ?>', 'identificacao');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["endereco"] ?>', 'endereco');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["cidade_uf"] ?>', 'cidade_uf');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["codigo_banco_com_dv"] ?>', 'codigo_banco_com_dv');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["cedente"] ?>', 'cedente');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["agencia_codigo"] ?>', 'agencia_codigo');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["especie"] ?>', 'especie');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["quantidade"] ?>', 'quantidade');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["nosso_numero"] ?>', 'nosso_numero');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["numero_documento"] ?>', 'numero_documento');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["cpf_cnpj"] ?>', 'cpf_cnpj');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["data_vencimento"] ?>', 'data_vencimento');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["sacado"] ?>', 'sacado');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["demonstrativo1"] ?>', 'demonstrativo1');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["demonstrativo2"] ?>', 'demonstrativo2');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["demonstrativo3"] ?>', 'demonstrativo3');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["data_documento"] ?>', 'data_documento');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["especie_doc"] ?>', 'especie_doc');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["aceite"] ?>', 'aceite');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["data_processamento"] ?>', 'data_processamento');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["carteira"] ?>', 'carteira');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["valor_unitario"] ?>', 'valor_unitario');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["instrucoes1"] ?>', 'instrucoes1');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["instrucoes2"] ?>', 'instrucoes2');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["instrucoes3"] ?>', 'instrucoes3');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["instrucoes4"] ?>', 'instrucoes4');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["endereco1"] ?>', 'endereco1');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["endereco2"] ?>', 'endereco2');
    S := _SubstCodigo(S, '<?php echo ($data_venc != "") ? $dadosboleto["data_vencimento"] : "Contra Apresentação" ?>', 'data_vencimento');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["contrato"] ?>', 'contrato');
    S := _SubstCodigo(S, '<?php echo isset($dadosboleto["variacao_carteira"]) ? $dadosboleto["variacao_carteira"] : ''&nbsp;'' ?>', 'variacao_carteira');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["carteira_descricao"] ?>', 'carteira_descricao');
    S := _SubstCodigo(S, '<?php fbarcode($dadosboleto["codigo_barras"]) ?>', 'codigo_barras');

    S := _SubstCodigo(S, '<?php echo isset($dadosboleto["cpf_cnpj"]) ? "<br>".$dadosboleto["cpf_cnpj"] : ''''; ?>', 'cpf_cnpj', '<br />');
    S := _SubstCodigo(S, '<?php echo isset($dadosboleto["cpf_cnpj"]) ? $dadosboleto["cpf_cnpj"] : ''''; ?>', 'cpf_cnpj');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["identificacao"]; ?>', 'identificacao');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["linha_digitavel"]; ?>', 'linha_digitavel');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["valor_boleto"]; ?>', 'valor_boleto');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["identificacao"]; ?>', 'identificacao');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["endereco"]; ?>', 'endereco');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["cidade_uf"]; ?>', 'cidade_uf');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["codigo_banco_com_dv"]; ?>', 'codigo_banco_com_dv');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["cedente"]; ?>', 'cedente');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["agencia_codigo"]; ?>', 'agencia_codigo');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["especie"]; ?>', 'especie');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["quantidade"]; ?>', 'quantidade');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["nosso_numero"]; ?>', 'nosso_numero');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["numero_documento"]; ?>', 'numero_documento');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["cpf_cnpj"]; ?>', 'cpf_cnpj');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["data_vencimento"]; ?>', 'data_vencimento');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["sacado"]; ?>', 'sacado');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["demonstrativo1"]; ?>', 'demonstrativo1');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["demonstrativo2"]; ?>', 'demonstrativo2');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["demonstrativo3"]; ?>', 'demonstrativo3');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["data_documento"]; ?>', 'data_documento');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["especie_doc"]; ?>', 'especie_doc');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["aceite"]; ?>', 'aceite');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["data_processamento"]; ?>', 'data_processamento');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["carteira"]; ?>', 'carteira');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["valor_unitario"]; ?>', 'valor_unitario');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["instrucoes1"]; ?>', 'instrucoes1');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["instrucoes2"]; ?>', 'instrucoes2');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["instrucoes3"]; ?>', 'instrucoes3');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["instrucoes4"]; ?>', 'instrucoes4');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["endereco1"]; ?>', 'endereco1');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["endereco2"]; ?>', 'endereco2');
    S := _SubstCodigo(S, '<?php echo ($data_venc != "") ? $dadosboleto["data_vencimento"] : "Contra Apresentação"; ?>', 'data_vencimento');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["contrato"]; ?>', 'contrato');
    S := _SubstCodigo(S, '<?php echo isset($dadosboleto["variacao_carteira"]) ? $dadosboleto["variacao_carteira"] : ''&nbsp;''; ?>', 'variacao_carteira');
    S := _SubstCodigo(S, '<?php echo $dadosboleto["carteira_descricao"]; ?>', 'carteira_descricao');
    S := _SubstCodigo(S, '<?php fbarcode($dadosboleto["codigo_barras"]); ?>', 'codigo_barras');

    Dest.Text := S;

    Dest.SaveToFile(edModeloZB.FileName);

    C := Occurs(S, '<?');
    if C > 0 then
      ShowMessageFmt('Há %d ocorrências de código PHP no arquivo convertido.', [C])
    else
      ShowMessage('Sucesso, não há código PHP no arquivo convertido!')
  finally
    Dest.Free;
    Ori.Free;
  end;
end;

end.


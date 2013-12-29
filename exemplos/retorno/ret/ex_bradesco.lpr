program ex_bradesco;

{$mode objfpc}{$H+}

uses
  ZBoleto, ZBoletoModelo_Bradesco, Classes, SysUtils, FPJSON;

  procedure FmtData(ACampos: TJSONObject; const ACampo: array of string);
  var
    C: string;
  begin
    for C in ACampo do
      ACampos.Strings[C] := FormatDateTime('dd/mm/yy', ACampos.Floats[C]);
  end;

  procedure FmtCurr(ACampos: TJSONObject; const ACampo: array of string);
  var
    C: string;
  begin
    for C in ACampo do
      ACampos.Strings[C] := FormatCurr(',0.00', ACampos.Floats[C]);
  end;

  procedure FmtFloat(ACampos: TJSONObject; const ACampo: string);
  begin
    ACampos.Strings[ACampo] := FormatFloat(',0.00', ACampos.Floats[ACampo]);
  end;

var
  I, J, K: Integer;
  NomeCampo: string;
  Boleto: TZBoleto;
  Retorno: TFileStream;
  Modelo: TZBoletoModeloBase;
  CamposItens: TJSONArray;
  CamposHeader, CamposDetalhe, CamposTrailler, EmprCedenteBanco: TJSONObject;
begin
  Boleto := TZBoleto.Create(nil);
  Modelo := Boleto.CriaModelo('bradesco');
  Retorno := TFileStream.Create('CB000000.RST', fmOpenRead or fmShareDenyWrite);
  CamposHeader := TJSONObject.Create;
  CamposItens := TJSONArray.Create;
  CamposTrailler := TJSONObject.Create;
  try
    // Registro Header Label
    Modelo.GeraHeaderRetorno(Retorno, CamposHeader, tc400);
    WriteLn('Registro Header Label ...');
    FmtData(CamposHeader, ['data_gravacao_arquivo', 'data_credito']);
    for I := 0 to Pred(CamposHeader.Count) do
      WriteLn(CamposHeader.Names[I], ': ', CamposHeader.Items[I].AsString);

    WriteLn;

    // Registro de Transação - Tipo 1
    Modelo.GeraDetalhesRetorno(Retorno, CamposItens, tc400);
    WriteLn('Registro de Transação - Tipo 1 ...');
    for I := 0 to Pred(CamposItens.Count) do
    begin
      CamposDetalhe := CamposItens.Objects[I];
      FmtData(CamposDetalhe, ['data_ocorrencia_banco', 'data_vencimento_titulo',
        'data_credito']);
      FmtFloat(CamposDetalhe, 'juros_operacao_atraso');
      FmtCurr(CamposDetalhe, ['valor_titulo', 'despesas_cobr_cod_ocorrencia',
        'outras_despesas', 'iof_devido', 'abatimento_concedido_titulo',
        'desconto_concedido', 'valor_pago', 'juros_mora', 'outros_creditos']);
      for J := 0 to Pred(CamposDetalhe.Count) do
      begin
        NomeCampo := CamposDetalhe.Names[J];
        if NomeCampo = 'empresa_cedente_banco' then
        begin
          WriteLn(NomeCampo, ': ');
          EmprCedenteBanco := CamposDetalhe.Items[J] as TJSONObject;
          for K := 0 to Pred(EmprCedenteBanco.Count) do
            WriteLn('  ', EmprCedenteBanco.Names[K], ': ',
              EmprCedenteBanco.Items[K].AsJSON)
        end
        else
          WriteLn(NomeCampo, ': ', CamposDetalhe.Items[J].AsString);
      end;
      WriteLn('--');
    end;

    WriteLn;

    // Registro Trailler
    Modelo.GeraTraillerRetorno(Retorno, CamposTrailler, tc400);
    WriteLn('Registro Trailler ...');
    FmtCurr(CamposTrailler, ['valor_total_cobranca',
      'valor_reg_confirm_entradas', 'valor_reg_liquidacao',
      'valor_reg_liquidacao', 'valor_reg_titulos_baixados',
      'valor_reg_abatimento_cancelado', 'valor_reg_vencto_alterado',
      'valor_reg_abatimento_concedido', 'valor_reg_confirm_instr_protesto',
      'valor_total_rateios_efetuados']);
    for I := 0 to Pred(CamposTrailler.Count) do
      WriteLn(CamposTrailler.Names[I], ': ', CamposTrailler.Items[I].AsString);
  finally
    CamposHeader.Free;
    CamposItens.Free;
    CamposTrailler.Free;
    FreeAndNil(Modelo);
    Boleto.Free;
  end;
end.


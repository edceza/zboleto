(*
  ZBoleto Registro.

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

unit ZBoletoReg;

{$mode objfpc}{$H+}

interface

uses
  ZBoleto, ZBoletoAnalisadorHtml, ZBoletoModelo_BB, ZBoletoModelo_Bradesco,
  ZBoletoModelo_CEF, ZBoletoModelo_HSBC, ZBoletoModelo_Itau, ZBoletoModelo_Real,
  ZBoletoModelo_Santander, ZBoletoModelo_SICREDI, Classes, LResources;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ZBoleto', [TZBoleto, TZBoletoAnalisadorHtml,
    TZBoletoModeloBB, TZBoletoModeloBradesco, TZBoletoModeloCEF,
    TZBoletoModeloHSBC, TZBoletoModeloItau, TZBoletoModeloReal,
    TZBoletoModeloSantander, TZBoletoModeloSICREDI]);
end;

initialization
  {$i zboletoreg.lrs}

end.


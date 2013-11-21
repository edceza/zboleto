(*
  ZBoleto Analisador WkHtmlToPdf.

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

unit ZBoletoAnalisadorWkHtmlToPdf;

{$i zboleto.inc}

interface

uses
  ZBoleto, ZBoletoAnalisadorHtml, uwkhtmltopdf, Classes, SysUtils, FPJSON;

type

  { TZBoletoAnalisadorPdf }

  TZBoletoAnalisadorPdf = class(TZBoletoAnalisadorBase)
  private
    FDirTmp: string;
    FWkHtmlToPdf: TWkHtmlToPdf;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function TipoAnalisador: string; override;
    procedure Executa(ACampos: TJSONObject; const ATipoModelo,
      ANomeArquivo: string); override; overload;
    property WkHtmlToPdf: TWkHtmlToPdf read FWkHtmlToPdf;
    property DirTmp: string read FDirTmp write FDirTmp;
  end;

implementation

{ TZBoletoAnalisadorPdf }

constructor TZBoletoAnalisadorPdf.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWkHtmlToPdf := TWkHtmlToPdf.Create;
  FDirTmp := ExtractFilePath(ParamStr(0));
end;

destructor TZBoletoAnalisadorPdf.Destroy;
begin
  FWkHtmlToPdf.Free;
  inherited Destroy;
end;

class function TZBoletoAnalisadorPdf.TipoAnalisador: string;
begin
  Result := 'Pdf';
end;

procedure TZBoletoAnalisadorPdf.Executa(ACampos: TJSONObject;
  const ATipoModelo, ANomeArquivo: string);
var
  VArqTmp: TFileName;
  VAnalisador: TZBoletoAnalisadorHtml;
begin
  VAnalisador := TZBoletoAnalisadorHtml.Create(nil);
  try
    VArqTmp := IncludeTrailingPathDelimiter(FDirTmp) +
      ExtractFileName(ChangeFileExt(GetTempFileName(FDirTmp, 'ZB'), '.html'));
    VAnalisador.Executa(ACampos, ATipoModelo, VArqTmp);
    FWkHtmlToPdf.Title := 'ZBoleto - Gerador de boletos bancários.';
    FWkHtmlToPdf.HtmlFile := VArqTmp;
    FWkHtmlToPdf.PdfFile := ANomeArquivo;
    FWkHtmlToPdf.Execute;
  finally
    VAnalisador.Free;
  end;
end;

initialization
  TZBoletoAnalisadorPdf.Registra;

finalization
  TZBoletoAnalisadorPdf.Desregistra;

end.

(*
  ZBoleto Analisador HTML.

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

unit ZBoletoAnalisadorHtml;

{$i zboleto.inc}

interface

uses
  ZBoleto, Classes, SysUtils, FPJSON, JTemplate;

type

  { TZBoletoAnalisadorHtml }

  TZBoletoAnalisadorHtml = class(TZBoletoAnalisadorBase)
  private
    FModelo: TJTemplateStream;
    function ObtemSuportaHtml: Boolean;
    function ObtemTagPrefixo: ShortString;
    function ObtemTagSufixo: ShortString;
    procedure AtribuiSuportaHtml(const AVal: Boolean);
    procedure AtribuiTagPrefixo(const AVal: ShortString);
    procedure AtribuiTagSufixo(const AVal: ShortString);
  protected
    property Modelo: TJTemplateStream read FModelo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function TipoAnalisador: string; override;
    function Executa(ACampos: TJSONObject; const ANomeModelo: string): string; override;
    property SuportaHtml: Boolean read ObtemSuportaHtml write AtribuiSuportaHtml;
    property TagPrefixo: ShortString read ObtemTagPrefixo write AtribuiTagPrefixo;
    property TagSufixo: ShortString read ObtemTagSufixo write AtribuiTagSufixo;
  end;

var
  DirModelos: string;

implementation

{ TZBoletoAnalisadorHtml }

constructor TZBoletoAnalisadorHtml.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FModelo := TJTemplateStream.Create;
  FModelo.Parser.TagPrefix := '<% ';
  FModelo.Parser.TagSuffix := ' %>';
  FModelo.Parser.HtmlSupports := False;
end;

destructor TZBoletoAnalisadorHtml.Destroy;
begin
  FModelo.Free;
  inherited Destroy;
end;

class function TZBoletoAnalisadorHtml.TipoAnalisador: string;
begin
  Result := 'Html';
end;

function TZBoletoAnalisadorHtml.ObtemTagPrefixo: ShortString;
begin
  Result := FModelo.Parser.TagPrefix;
end;

function TZBoletoAnalisadorHtml.ObtemSuportaHtml: Boolean;
begin
  Result := FModelo.Parser.HtmlSupports;
end;

function TZBoletoAnalisadorHtml.ObtemTagSufixo: ShortString;
begin
  Result := FModelo.Parser.TagSuffix;
end;

procedure TZBoletoAnalisadorHtml.AtribuiSuportaHtml(const AVal: Boolean);
begin
  FModelo.Parser.HtmlSupports := AVal;
end;

procedure TZBoletoAnalisadorHtml.AtribuiTagPrefixo(const AVal: ShortString);
begin
  FModelo.Parser.TagPrefix := AVal;
end;

procedure TZBoletoAnalisadorHtml.AtribuiTagSufixo(const AVal: ShortString);
begin
  FModelo.Parser.TagSuffix := AVal;
end;

function TZBoletoAnalisadorHtml.Executa(ACampos: TJSONObject;
  const ANomeModelo: string): string;
var
  I: Integer;
  VArq: string;
begin
  VArq := IncludeTrailingPathDelimiter(DirModelos) +
    LowerCase(ANomeModelo) + '.html';
  if not FileExists(VArq) then
    raise EZBoleto.CreateFmt(SZBArquivoNaoExiste_Erro, [VArq]);
  FModelo.LoadFromFile(VArq);
  for I := 0 to Pred(ACampos.Count) do
    FModelo.Parser.Fields.Add(ACampos.Names[I], ACampos.Items[I].Clone);
  FModelo.Parser.Replace(True);
  Result := FModelo.Parser.Content;
end;

initialization
  TZBoletoAnalisadorHtml.Registra;
  DirModelos := '.' + DirectorySeparator + 'modelos' + DirectorySeparator + 'html';

finalization
  TZBoletoAnalisadorHtml.Desregistra;

end.

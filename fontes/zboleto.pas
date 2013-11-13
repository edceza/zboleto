(*
  ZBoleto.

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

unit ZBoleto;

{$i zboleto.inc}

interface

uses
  ZBoletoUteis, Classes, SysUtils, FPJSON;

type
  EZBoleto = class(Exception);

  TZClasseBoletoModeloBase = class of TZBoletoModeloBase;

  TZClasseBoletoAnalisadorBase = class of TZBoletoAnalisadorBase;

  { TZBoletoModeloBase }

  TZBoletoModeloBase = class(TComponent)
  private
    FAgencia: string;
    FAgenciaCodigo: string;
    FCampos: TJSONObject;
    FCarteira: string;
    FCodigo: string;
    FCodigoBanco: string;
    FCodigoBancoComDV: string;
    FConta: string;
    FContaCedente: string;
    FContaCedenteDV: string;
    FContaDV: string;
    FDV: string;
    FFatorVencimento: string;
    FNomeBanco: string;
    FNossoNumero: string;
    FNumMoeda: string;
    FValor: string;
  public
    constructor Create(ACampos: TJSONObject; AOwner: TComponent); reintroduce;
    class procedure Registra;
    class procedure Desregistra;
    class function NomeModelo: string; virtual;
    class function TipoModelo: string; virtual;
    function ObtemValorCampo(const ANomeCampo: string): TJSONData;
    function GeraCodigoBanco(const ANumero: string): string; virtual;
    procedure Executa; virtual; abstract;
    property Campos: TJSONObject read FCampos write FCampos;
    property CodigoBanco: string read FCodigoBanco write FCodigoBanco;
    property CodigoBancoComDV: string read FCodigoBancoComDV write FCodigoBancoComDV;
    property NomeBanco: string read FNomeBanco write FNomeBanco;
    property NumMoeda: string read FNumMoeda write FNumMoeda;
    property FatorVencimento: string read FFatorVencimento write FFatorVencimento;
    property Valor: string read FValor write FValor;
    property Agencia: string read FAgencia write FAgencia;
    property Conta: string read FConta write FConta;
    property ContaDV: string read FContaDV write FContaDV;
    property Carteira: string read FCarteira write FCarteira;
    property NossoNumero: string read FNossoNumero write FNossoNumero;
    property ContaCedente: string read FContaCedente write FContaCedente;
    property ContaCedenteDV: string read FContaCedenteDV write FContaCedenteDV;
    property DV: string read FDV write FDV;
    property Codigo: string read FCodigo write FCodigo;
    property AgenciaCodigo: string read FAgenciaCodigo write FAgenciaCodigo;
  end;

  { TZBoletoAnalisadorBase }

  TZBoletoAnalisadorBase = class(TComponent)
  public
    class procedure Registra;
    class procedure Desregistra;
    class function TipoAnalisador: string; virtual;
    function Executa({%H-}ACampos: TJSONObject;
      {%H-}const ANomeModelo: string): string; virtual;
  end;

  { TZBoletoBase }

  TZBoletoBase = class(TComponent)
  private
    FCampos: TJSONObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function EncontraClasseModelo(
      const ATipoModelo: string): TZClasseBoletoModeloBase;
    function ObtemClasseModeloPorTipoModelo(
      const ATipoModelo: string): TZClasseBoletoModeloBase;
    function EncontraClasseAnalisador(
      const ATipoAnalisador: string): TZClasseBoletoAnalisadorBase;
    function ObtemClasseAnalisadorPorTipoAnalisador(
      const ATipoAnalisador: string): TZClasseBoletoAnalisadorBase;
    function Executa(const ATipoModelo, ATipoAnalisador: string;
      out AModelo: TZBoletoModeloBase;
      out AAnalisador: TZBoletoAnalisadorBase): string; overload;
    function Executa(const ATipoModelo, ATipoAnalisador: string): string; overload;
    property Campos: TJSONObject read FCampos;
  end;

  { TZBoleto }

  TZBoleto = class(TZBoletoBase)
  end;

resourcestring
  SZBNaoImplementado_Info = 'Não implementado.';
  SZBCampoNaoEncontrado_Erro = 'Campo "%s" não adicionado.';
  SZBArquivoNaoExiste_Erro = 'Arquivo não existe: %s.';
  SZBModeloJaRegistrado_Erro = 'Modelo já registrado: %s.';
  SZBModeloNaoRegistrado_Erro = 'Modelo não registrado: %s.';
  SZBAnalisadorJaRegistrado_Erro = 'Analisador já registrado: %s.';
  SZBAnalisadorNaoRegistrado_Erro = 'Analisador não registrado: %s.';

implementation

var
  _Modelos: TFPList;
  _Analisadores: TFPList;

function Modelos: TFPList;
begin
  Result := _Modelos;
end;

procedure CriaModelos;
begin
  _Modelos := TFPList.Create;
end;

procedure DestroiModelos;
begin
  FreeAndNil(_Modelos);
end;

function Analisadores: TFPList;
begin
  Result := _Analisadores;
end;

procedure CriaAnalisadores;
begin
  _Analisadores := TFPList.Create;
end;

procedure DestroiAnalisadores;
begin
  FreeAndNil(_Analisadores);
end;

{ TZBoletoModeloBase }

constructor TZBoletoModeloBase.Create(ACampos: TJSONObject; AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCampos := ACampos;
  FCodigoBanco := SZBNaoImplementado_Info;
  FCodigoBancoComDV := SZBNaoImplementado_Info;
  FNomeBanco := SZBNaoImplementado_Info;
  FNumMoeda := SZBNaoImplementado_Info;
  FFatorVencimento := SZBNaoImplementado_Info;
  FValor := SZBNaoImplementado_Info;
  FAgencia := SZBNaoImplementado_Info;
  FConta := SZBNaoImplementado_Info;
  FContaDV := SZBNaoImplementado_Info;
  FCarteira := SZBNaoImplementado_Info;
  FNossoNumero := SZBNaoImplementado_Info;
  FContaCedente := SZBNaoImplementado_Info;
  FContaCedenteDV := SZBNaoImplementado_Info;
  FDV := SZBNaoImplementado_Info;
  FCodigo := SZBNaoImplementado_Info;
  FAgenciaCodigo := SZBNaoImplementado_Info;
end;

class procedure TZBoletoModeloBase.Registra;
begin
  if Modelos.IndexOf(Self) > -1 then
    raise EZBoleto.CreateFmt(SZBModeloJaRegistrado_Erro, [ClassName]);
  Modelos.Add(Self);
end;

class procedure TZBoletoModeloBase.Desregistra;
var
  VIdModelo: Integer;
begin
  VIdModelo := Modelos.IndexOf(Self);
  if VIdModelo = -1 then
    raise EZBoleto.CreateFmt(SZBModeloNaoRegistrado_Erro, [ClassName]);
  Modelos.Delete(VIdModelo);
end;

class function TZBoletoModeloBase.NomeModelo: string;
begin
  Result := SZBNaoImplementado_Info;
end;

class function TZBoletoModeloBase.TipoModelo: string;
begin
  Result := SZBNaoImplementado_Info;
end;

function TZBoletoModeloBase.ObtemValorCampo(const ANomeCampo: string): TJSONData;
var
  I: Integer;
begin
  I := Campos.IndexOfName(ANomeCampo);
  if I < 0 then
    raise EZBoleto.CreateFmt(SZBCampoNaoEncontrado_Erro, [ANomeCampo]);
  Result := Campos.Items[I];
end;

function TZBoletoModeloBase.GeraCodigoBanco(const ANumero: string): string;
begin
  Result := Copy(ANumero, 1, 3);
  Result += '-' + Modulo11(Result);
end;

{ TZBoletoAnalisadorBase }

class procedure TZBoletoAnalisadorBase.Registra;
begin
  if Analisadores.IndexOf(Self) > -1 then
    raise EZBoleto.CreateFmt(SZBAnalisadorJaRegistrado_Erro, [ClassName]);
  Analisadores.Add(Self);
end;

class procedure TZBoletoAnalisadorBase.Desregistra;
var
  VIdAnalisador: Integer;
begin
  VIdAnalisador := Analisadores.IndexOf(Self);
  if VIdAnalisador = -1 then
    raise EZBoleto.CreateFmt(SZBAnalisadorNaoRegistrado_Erro, [ClassName]);
  Analisadores.Delete(VIdAnalisador);
end;

class function TZBoletoAnalisadorBase.TipoAnalisador: string;
begin
  Result := SZBNaoImplementado_Info;
end;

function TZBoletoAnalisadorBase.Executa(ACampos: TJSONObject;
  const ANomeModelo: string): string;
begin
  Result := SZBNaoImplementado_Info;
end;

{ TZBoletoBase }

constructor TZBoletoBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCampos := TJSONObject.Create;
end;

destructor TZBoletoBase.Destroy;
begin
  FCampos.Free;
  inherited Destroy;
end;

function TZBoletoBase.EncontraClasseModelo(
  const ATipoModelo: string): TZClasseBoletoModeloBase;
var
  P: Pointer absolute Result;
begin
  for P in Modelos do
    if CompareText(Result.TipoModelo, ATipoModelo) = 0 then
      Exit;
  Result := nil;
end;

function TZBoletoBase.ObtemClasseModeloPorTipoModelo(
  const ATipoModelo: string): TZClasseBoletoModeloBase;
begin
  Result := EncontraClasseModelo(ATipoModelo);
  if not Assigned(Result) then
    raise EZBoleto.CreateFmt(SZBModeloNaoRegistrado_Erro, [ATipoModelo]);
end;

function TZBoletoBase.EncontraClasseAnalisador(
  const ATipoAnalisador: string): TZClasseBoletoAnalisadorBase;
var
  P: Pointer absolute Result;
begin
  for P in Analisadores do
    if CompareText(Result.TipoAnalisador, ATipoAnalisador) = 0 then
      Exit;
  Result := nil;
end;

function TZBoletoBase.ObtemClasseAnalisadorPorTipoAnalisador(
  const ATipoAnalisador: string): TZClasseBoletoAnalisadorBase;
begin
  Result := EncontraClasseAnalisador(ATipoAnalisador);
  if not Assigned(Result) then
    raise EZBoleto.CreateFmt(SZBAnalisadorNaoRegistrado_Erro, [ATipoAnalisador]);
end;

function TZBoletoBase.Executa(const ATipoModelo, ATipoAnalisador: string;
  out AModelo: TZBoletoModeloBase; out AAnalisador: TZBoletoAnalisadorBase): string;
begin
  AModelo := ObtemClasseModeloPorTipoModelo(ATipoModelo).Create(FCampos, nil);
  AAnalisador := ObtemClasseAnalisadorPorTipoAnalisador(ATipoAnalisador).Create(nil);
  AModelo.Executa;
  Result := AAnalisador.Executa(FCampos, AModelo.NomeModelo);
end;

function TZBoletoBase.Executa(const ATipoModelo, ATipoAnalisador: string): string;
var
  VModelo: TZBoletoModeloBase = nil;
  VAnalisador: TZBoletoAnalisadorBase = nil;
begin
  try
    Result := Executa(ATipoModelo, ATipoAnalisador, VModelo, VAnalisador);
  finally
    FreeAndNil(VAnalisador);
    FreeAndNil(VModelo);
  end;
end;

initialization
  CriaModelos;
  CriaAnalisadores;

finalization
  DestroiModelos;
  DestroiAnalisadores;

end.

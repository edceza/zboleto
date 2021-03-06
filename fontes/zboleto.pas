(*
  ZBoleto.

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

unit ZBoleto;

{$i zboleto.inc}

interface

uses
  ZBoletoUteis, Classes, SysUtils, FPJSON;

type
  EZBoleto = class(Exception);

  TZClasseBoletoModeloBase = class of TZBoletoModeloBase;

  TZClasseBoletoAnalisadorBase = class of TZBoletoAnalisadorBase;

  TZBoletoTipoCNAB = (tc240, tc400);

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
    FDirArquivosRemessa: string;
    FDirArquivosRetorno: string;
    FDV: string;
    FFatorVencimento: string;
    FNomeBanco: string;
    FNossoNumero: string;
    FNumMoeda: string;
    FValor: string;
  protected
    procedure ChecaParametroInterno(AObject: TObject);
    function CampoInterno(ACampos: TJSONObject;
      const ANomeCampo: string): TJSONData; overload;
  public
    constructor Create(ACampos: TJSONObject; AOwner: TComponent); reintroduce;
    class procedure Registra;
    class procedure Desregistra;
    class function NomeModelo: string; virtual;
    class function TipoModelo: string; virtual;
    function Campo(const ANomeCampo: string): TJSONData;
    function GeraCodigoBanco(const ANumero: string): string; virtual;
    function GeraNomeArquivoRemessa: string; virtual;
    function GeraSequenciaArquivo: Integer; virtual;
    function GeraHeaderRemessa(ACampos: TJSONObject;
      {%H-}const ATipoCNAB: TZBoletoTipoCNAB): string; virtual;
    function GeraDetalhesRemessa(ACampos: TJSONArray;
      {%H-}const ATipoCNAB: TZBoletoTipoCNAB): string; virtual;
    function GeraTraillerRemessa(ACampos: TJSONObject;
      {%H-}const ATipoCNAB: TZBoletoTipoCNAB): string; virtual;
    procedure GeraHeaderRetorno(ADados: TStream; ACampos: TJSONObject;
      {%H-}const ATipoCNAB: TZBoletoTipoCNAB); virtual;
    procedure GeraDetalhesRetorno(ADados: TStream; AItens: TJSONArray;
      {%H-}const ATipoCNAB: TZBoletoTipoCNAB); virtual;
    procedure GeraTraillerRetorno(ADados: TStream; ACampos: TJSONObject;
      {%H-}const ATipoCNAB: TZBoletoTipoCNAB); virtual;
    procedure Prepara; virtual; abstract;
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
    property DirArquivosRemessa: string read FDirArquivosRemessa write FDirArquivosRemessa;
    property DirArquivosRetorno: string read FDirArquivosRetorno write FDirArquivosRetorno;
  end;

  { TZBoletoAnalisadorBase }

  TZBoletoAnalisadorBase = class(TComponent)
  public
    class procedure Registra;
    class procedure Desregistra;
    class function TipoAnalisador: string; virtual;
    function Executa({%H-}ACampos: TJSONObject;
      {%H-}const ATipoModelo: string): string; virtual; overload;
    procedure Executa({%H-}ACampos: TJSONObject;
      {%H-}const ATipoModelo,{%H-}ANomeArquivo: string); virtual; overload;
  end;

  { TZBoletoBase }

  TZBoletoBase = class(TComponent)
  private
    FCampos: TJSONObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CriaModelo(const ATipo: string): TZBoletoModeloBase; virtual;
    function CriaAnalisador(const ATipo: string): TZBoletoAnalisadorBase; virtual;
    function EncontraClasseModelo(
      const ATipoModelo: string): TZClasseBoletoModeloBase;
    function ObtemClasseModeloPorTipoModelo(
      const ATipoModelo: string): TZClasseBoletoModeloBase;
    function EncontraClasseAnalisador(
      const ATipoAnalisador: string): TZClasseBoletoAnalisadorBase;
    function ObtemClasseAnalisadorPorTipoAnalisador(
      const ATipoAnalisador: string): TZClasseBoletoAnalisadorBase;
    procedure Prepara(const ATipoModelo: string;
      out AModelo: TZBoletoModeloBase); overload;
    procedure Prepara(const ATipoModelo, ATipoAnalisador: string;
      out AModelo: TZBoletoModeloBase;
      out AAnalisador: TZBoletoAnalisadorBase); overload;
    procedure Prepara(const ATipoModelo, ATipoAnalisador: string;
      out AAnalisador: TZBoletoAnalisadorBase); overload;
    function Executa(const ATipoModelo, ATipoAnalisador: string;
      out AModelo: TZBoletoModeloBase;
      out AAnalisador: TZBoletoAnalisadorBase): string; overload;
    function Executa(const ATipoModelo, ATipoAnalisador: string): string; overload;
    function Executa(const ATipoModelo, ATipoAnalisador: string;
      out AAnalisador: TZBoletoAnalisadorBase): string; overload;
    procedure Executa(const ATipoModelo, ATipoAnalisador,
      ANomeArquivo: string); overload;
    property Campos: TJSONObject read FCampos;
  end;

  { TZBoleto }

  TZBoleto = class(TZBoletoBase)
  end;

resourcestring
  SZBParametroNulo_Erro = 'Parâmetro não pode ser nulo.';
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
  if FDirArquivosRemessa = '' then
    FDirArquivosRemessa := ExtractFilePath(ParamStr(0));
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

procedure TZBoletoModeloBase.ChecaParametroInterno(AObject: TObject);
begin
  if not Assigned(AObject) then
    raise EZBoleto.Create(SZBParametroNulo_Erro);
end;

function TZBoletoModeloBase.CampoInterno(ACampos: TJSONObject;
  const ANomeCampo: string): TJSONData;
var
  I: Integer;
begin
  I := ACampos.IndexOfName(ANomeCampo);
  if I < 0 then
    raise EZBoleto.CreateFmt(SZBCampoNaoEncontrado_Erro, [ANomeCampo]);
  Result := ACampos.Items[I];
end;

function TZBoletoModeloBase.Campo(const ANomeCampo: string): TJSONData;
begin
  Result := CampoInterno(Campos, ANomeCampo);
end;

function TZBoletoModeloBase.GeraCodigoBanco(const ANumero: string): string;
begin
  Result := Copy(ANumero, 1, 3);
  Result += '-' + Modulo11(Result);
end;

function TZBoletoModeloBase.GeraNomeArquivoRemessa: string;
var
  S: string;
  I: Integer;
begin
  S := IncludeTrailingPathDelimiter(FDirArquivosRemessa) + 'CB' +
    FormatDateTime('ddmm', Date);
  for I := 0 to 1295 do
  begin
    Result := S + GeraCodigoAlfaNumerico(I) + '.REM';
    if not FileExists(Result) then
      Break;
  end;
end;

function TZBoletoModeloBase.GeraSequenciaArquivo: Integer;
var
  I: Integer;
  R: TSearchRec;
  F: Boolean = False;
begin
  I := 0;
  if FindFirst(IncludeTrailingPathDelimiter(FDirArquivosRemessa) + '*.REM',
    faArchive, R) = 0 then
    try
      repeat
        if ((R.Attr and faDirectory) <> faDirectory) and
          (ExtractFileExt(R.Name) = '.REM') then
        begin
          F := True;
          Inc(I);
        end;
      until FindNext(R) <> 0;
      if F then
        Inc(I);
      Result := I;
    finally
      FindClose(R);
    end;
end;

function TZBoletoModeloBase.GeraHeaderRemessa(ACampos: TJSONObject;
  const ATipoCNAB: TZBoletoTipoCNAB): string;
begin
  ChecaParametroInterno(ACampos);
  Result := SZBNaoImplementado_Info;
end;

function TZBoletoModeloBase.GeraDetalhesRemessa(ACampos: TJSONArray;
  const ATipoCNAB: TZBoletoTipoCNAB): string;
begin
  ChecaParametroInterno(ACampos);
  Result := SZBNaoImplementado_Info;
end;

function TZBoletoModeloBase.GeraTraillerRemessa(ACampos: TJSONObject;
  const ATipoCNAB: TZBoletoTipoCNAB): string;
begin
  ChecaParametroInterno(ACampos);
  Result := SZBNaoImplementado_Info;
end;

procedure TZBoletoModeloBase.GeraHeaderRetorno(ADados: TStream;
  ACampos: TJSONObject; const ATipoCNAB: TZBoletoTipoCNAB);
begin
  ChecaParametroInterno(ADados);
  ChecaParametroInterno(ACampos);
end;

procedure TZBoletoModeloBase.GeraDetalhesRetorno(ADados: TStream;
  AItens: TJSONArray; const ATipoCNAB: TZBoletoTipoCNAB);
begin
  ChecaParametroInterno(ADados);
  ChecaParametroInterno(AItens);
end;

procedure TZBoletoModeloBase.GeraTraillerRetorno(ADados: TStream;
  ACampos: TJSONObject; const ATipoCNAB: TZBoletoTipoCNAB);
begin
  ChecaParametroInterno(ADados);
  ChecaParametroInterno(ACampos);
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
  const ATipoModelo: string): string;
begin
  Result := SZBNaoImplementado_Info;
end;

procedure TZBoletoAnalisadorBase.Executa(ACampos: TJSONObject;
  const ATipoModelo, ANomeArquivo: string);
begin
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

function TZBoletoBase.CriaModelo(const ATipo: string): TZBoletoModeloBase;
begin
  Result := ObtemClasseModeloPorTipoModelo(ATipo).Create(FCampos, Owner);
end;

function TZBoletoBase.CriaAnalisador(const ATipo: string): TZBoletoAnalisadorBase;
begin
  Result := ObtemClasseAnalisadorPorTipoAnalisador(ATipo).Create(Owner);
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

procedure TZBoletoBase.Prepara(const ATipoModelo: string;
  out AModelo: TZBoletoModeloBase);
begin
  AModelo := CriaModelo(ATipoModelo);
  AModelo.Prepara;
end;

procedure TZBoletoBase.Prepara(const ATipoModelo, ATipoAnalisador: string;
  out AModelo: TZBoletoModeloBase; out AAnalisador: TZBoletoAnalisadorBase);
begin
  AAnalisador := CriaAnalisador(ATipoAnalisador);
  Prepara(ATipoModelo, AModelo);
end;

procedure TZBoletoBase.Prepara(const ATipoModelo, ATipoAnalisador: string; out
  AAnalisador: TZBoletoAnalisadorBase);
var
  VModelo: TZBoletoModeloBase = nil;
begin
  try
    Prepara(ATipoModelo, ATipoAnalisador, VModelo, AAnalisador);
  finally
    FreeAndNil(VModelo);
  end;
end;

function TZBoletoBase.Executa(const ATipoModelo, ATipoAnalisador: string;
  out AModelo: TZBoletoModeloBase; out AAnalisador: TZBoletoAnalisadorBase): string;
begin
  Prepara(ATipoModelo, ATipoAnalisador, AModelo, AAnalisador);
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

function TZBoletoBase.Executa(const ATipoModelo, ATipoAnalisador: string; out
  AAnalisador: TZBoletoAnalisadorBase): string;
var
  VModelo: TZBoletoModeloBase;
begin
  try
    Result := Executa(ATipoModelo, ATipoAnalisador, VModelo, AAnalisador);
  finally
    FreeAndNil(VModelo);
  end;
end;

procedure TZBoletoBase.Executa(const ATipoModelo, ATipoAnalisador,
  ANomeArquivo: string);
var
  VModelo: TZBoletoModeloBase = nil;
  VAnalisador: TZBoletoAnalisadorBase = nil;
begin
  try
    Prepara(ATipoModelo, ATipoAnalisador, VModelo, VAnalisador);
    VAnalisador.Executa(FCampos, VModelo.NomeModelo, ANomeArquivo);
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

unit uwkhtmltopdf;

{$mode objfpc}{$H+}

interface

uses
  Process, Classes, SysUtils;

const
  WKHTMLTOPDF_PATH: string = '\wkhtmltopdf\';
  WKHTMLTOPDF_EXE: string = 'wkhtmltopdf';
  WKHTMLTOPDF_FILE_NAME: string = '';

type
  EWkHtmlToPdf = class(Exception);

  { TWkHtmlToPdf }

  TWkHtmlToPdf = class
  private
    FCmd: TFileName;
    FCopies: Integer;
    FDeleteTmpFiles: Boolean;
    FGrayScale: Boolean;
    FHtmlFile: TFileName;
    FOrientation: string;
    FOutput: TStrings;
    FParameters: TStrings;
    FPdfFile: TFileName;
    FProcess: TProcess;
    FPageSize: string;
    FTitle: string;
    FToc: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    property Cmd: TFileName read FCmd write FCmd;
    property DeleteTmpFiles: Boolean read FDeleteTmpFiles write FDeleteTmpFiles;
    property HtmlFile: TFileName read FHtmlFile write FHtmlFile;
    property PdfFile: TFileName read FPdfFile write FPdfFile;
    property Orientation: string read FOrientation write FOrientation;
    property PageSize: string read FPageSize write FPageSize;
    property Toc: Boolean read FToc write FToc;
    property Copies: Integer read FCopies write FCopies;
    property GrayScale: Boolean read FGrayScale write FGrayScale;
    property Title: string read FTitle write FTitle;
    property Parameters: TStrings read FParameters write FParameters;
    property Output: TStrings read FOutput write FOutput;
  end;

implementation

{ TWkHtmlToPdf }

constructor TWkHtmlToPdf.Create;
begin
  inherited Create;
  FProcess := TProcess.Create(nil);
  FParameters := TStringList.Create;
  FOutput := TStringList.Create;
  FOutput.NameValueSeparator := ':';
  FCmd := WKHTMLTOPDF_FILE_NAME;
  FDeleteTmpFiles := True;
  FHtmlFile := 'input.html';
  FPdfFile := 'output.pdf';
  FOrientation := 'Portrait';
  FPageSize := 'A4';
  FCopies := 1;
end;

destructor TWkHtmlToPdf.Destroy;
begin
  FOutput.Free;
  FParameters.Free;
  FProcess.Free;
  inherited Destroy;
end;

procedure TWkHtmlToPdf.Execute;
var
  VError: string;
begin
  try
    FProcess.Options := FProcess.Options + [poUsePipes, poStderrToOutPut,
      poWaitOnExit, poNoConsole];
    FProcess.Executable := FCmd;
    FProcess.Parameters.Assign(FParameters);
    FProcess.Parameters.Add('--quiet');
    FProcess.Parameters.Add('--disable-smart-shrinking');
    FProcess.Parameters.Add('--copies');
    FProcess.Parameters.Add(IntToStr(FCopies));
    FProcess.Parameters.Add('--orientation');
    FProcess.Parameters.Add(FOrientation);
    FProcess.Parameters.Add('--page-size');
    FProcess.Parameters.Add(FPageSize);
    if FToc then
      FProcess.Parameters.Add('--toc');
    if FGrayScale then
      FProcess.Parameters.Add('--grayscale');
    if FTitle <> '' then
    begin
      FProcess.Parameters.Add('--title');
      FProcess.Parameters.Add(
{$IFDEF MSWINDOWS}Utf8ToAnsi({$ENDIF}FTitle{$IFDEF MSWINDOWS}){$ENDIF});
    end;
    if not FileExists(FHtmlFile) then
      raise EWkHtmlToPdf.CreateFmt('Arquivo "%s" não encontrado.', [FHtmlFile]);
    FProcess.Parameters.Add(FHtmlFile);
    if Trim(FPdfFile) = '' then
      raise EWkHtmlToPdf.Create('Informe um nome correto para o aquivo a ser gerado.');
    FProcess.Parameters.Add(FPdfFile);
    FProcess.Execute;
    FOutput.Clear;
    FOutput.LoadFromStream(FProcess.Output);
    VError := Trim(FOutput.Values['Error']);
    if VError <> '' then
      raise Exception.Create(VError);
  finally
    if FDeleteTmpFiles then
      DeleteFile(FHtmlFile);
  end;
end;

initialization
{$IFDEF MSWINDOWS}
  WKHTMLTOPDF_PATH :=
    {$I %PROGRAMFILES%} + WKHTMLTOPDF_PATH
  WKHTMLTOPDF_EXE +=
    '.exe'
{$ENDIF};
  WKHTMLTOPDF_FILE_NAME :=
{$IFDEF MSWINDOWS}
    WKHTMLTOPDF_PATH +
{$ENDIF}
    WKHTMLTOPDF_EXE;

end.

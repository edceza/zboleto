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

  { TWkHtmlToPdf }

  TWkHtmlToPdf = class
  private
    FCmd: TFileName;
    FCopies: Integer;
    FGrayScale: Boolean;
    FHtmlFile: TFileName;
    FOrientation: string;
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
    property HtmlFile: TFileName read FHtmlFile write FHtmlFile;
    property PdfFile: TFileName read FPdfFile write FPdfFile;
    property Orientation: string read FOrientation write FOrientation;
    property PageSize: string read FPageSize write FPageSize;
    property Toc: Boolean read FToc write FToc;
    property Copies: Integer read FCopies write FCopies;
    property GrayScale: Boolean read FGrayScale write FGrayScale;
    property Title: string read FTitle write FTitle;
    property Parameters: TStrings read FParameters write FParameters;
  end;

implementation

{ TWkHtmlToPdf }

constructor TWkHtmlToPdf.Create;
begin
  inherited Create;
  FProcess := TProcess.Create(nil);
  FParameters := TStringList.Create;
  FCmd := WKHTMLTOPDF_FILE_NAME;
  FHtmlFile := 'input.html';
  FPdfFile := 'output.pdf';
  FOrientation := 'Portrait';
  FPageSize := 'A4';
  FCopies := 1;
end;

destructor TWkHtmlToPdf.Destroy;
begin
  FParameters.Free;
  FProcess.Free;
  inherited Destroy;
end;

procedure TWkHtmlToPdf.Execute;
begin
  FProcess.Executable := FCmd;
  FProcess.Parameters.Assign(FParameters);
  FProcess.Parameters.Add('--quiet');
  FProcess.Parameters.Add('--footer-center');
  FProcess.Parameters.Add(
{$IFDEF MSWINDOWS}Utf8ToAnsi({$ENDIF}
    'ZBoleto - Gerador de boletos bancários.'
{$IFDEF MSWINDOWS}){$ENDIF});
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
    FProcess.Parameters.Add(FTitle);
  end;
  FProcess.Parameters.Add('--disable-smart-shrinking');
  FProcess.Parameters.Add(FHtmlFile);
  FProcess.Parameters.Add(FPdfFile);
  FProcess.Execute;
end;

initialization
  WKHTMLTOPDF_PATH :=
{$IFDEF MSWINDOWS}
    {$I %PROGRAMFILES%} + WKHTMLTOPDF_PATH
{$ENDIF};
  WKHTMLTOPDF_EXE +=
{$IFDEF MSWINDOWS}
    '.exe'
{$ENDIF};
  WKHTMLTOPDF_FILE_NAME := WKHTMLTOPDF_PATH + WKHTMLTOPDF_EXE;

end.


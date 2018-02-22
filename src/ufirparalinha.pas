unit ufirparalinha;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls, ButtonPanel;

type

  { TfrmIrParaLinha }

  TfrmIrParaLinha = class(TForm)
    ButtonPanel1 : TButtonPanel;
    edLinha: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblPrimeiraLinha: TLabel;
    lblUltimaLinha: TLabel;
    procedure btnOkClick(Sender: TObject);
    procedure btCancelarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FLinha: Integer;
    FPrimeiraLinha: Integer;
    FUltimaLinha: Integer;
    { Private declarations }
  public
    Property Linha:Integer read FLinha;
    Property PrimeiraLinha:Integer read FPrimeiraLinha write FPrimeiraLinha;
    Property UltimaLinha:Integer read FUltimaLinha write FUltimaLinha;
    { public declarations }
  end;

var
  frmIrParaLinha: TfrmIrParaLinha;

implementation

{$R *.lfm}

{ TfrmIrParaLinha }

procedure TfrmIrParaLinha.btnOkClick(Sender: TObject);
begin
  Try
    FLinha := StrToInt(edLinha.Text);
    if (FLinha < FPrimeiraLinha)
    or (FLinha > FUltimaLinha) then
    begin
      // TODO: gerar um exceção
      ShowMessage('Linha Inválida');
      edLinha.SetFocus;
      exit;
    end;
  except
    ShowMessage('Linha Inválida');
    edLinha.SetFocus;
  end;// Try..Except
  Close;
end;

procedure TfrmIrParaLinha.btCancelarClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmIrParaLinha.FormCreate(Sender: TObject);
begin
  FLinha := -1;
end;

procedure TfrmIrParaLinha.FormShow(Sender: TObject);
begin
  lblPrimeiraLinha.Caption := IntToStr(FPrimeiraLinha);
  lblUltimaLinha.Caption := IntToStr(FUltimaLinha);
end;

end.


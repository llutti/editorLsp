unit ufParametrosFuncoes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, LResources, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Buttons, ButtonPanel, SynEditHighlighter, LCSynEdit;

type

  { TFParametrosFuncoes }

  TFParametrosFuncoes = class(TForm)
    ButtonPanel1: TButtonPanel;

    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    SynEdit1:TLCSynEdit;
    { private declarations }
  public
    Procedure SetHighlighter(SynH : TSynCustomHighlighter);
    { public declarations }
  end;

var
  FParametrosFuncoes: TFParametrosFuncoes;

implementation

{$R *.lfm}

uses
  LazFileUtils, ufmain;

{ TFParametrosFuncoes }

procedure TFParametrosFuncoes.FormCreate(Sender: TObject);
Var
  aFileName : String;
begin
  SynEdit1:= TLCSynEdit.Create(self, 99999);
  SynEdit1.Parent := self;
  SynEdit1.Align:=alClient;

  aFileName := ExtractFilePath(Application.ExeName) + FILECOMPLETIONPROPOSAL;
  if FileExistsUTF8(aFileName) then
  begin
    SynEdit1.LoadFromFile(aFileName);
  end;
end;

procedure TFParametrosFuncoes.OKButtonClick(Sender: TObject);
Var
  aFileName : String;
begin
  aFileName := ExtractFilePath(Application.ExeName) + FILECOMPLETIONPROPOSAL;
  if Trim(SynEdit1.Text) = '' then
  begin
    DeleteFile(aFileName)
  end
  else
  begin
    SynEdit1.SaveAs(aFileName,false);
  end;
end;

procedure TFParametrosFuncoes.SetHighlighter(SynH: TSynCustomHighlighter);
begin
  SynEdit1.Highlighter := SynH;
end;


end.


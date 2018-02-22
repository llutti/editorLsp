unit uFSettingsEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, SynEdit, LResources, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ComCtrls, ExtCtrls, Buttons, StdCtrls, EditBtn, PropEdits, ObjectInspector, ECTabCtrl, uEditorLspSettings,
  SynLCHighlighter, Types, LCLType;

type

  { TFSettingsEditor }

  TFSettingsEditor = class(TForm)
    bbAddArqExc : TBitBtn;
    bbRemoverArqExc : TBitBtn;
    ButtonPanel1: TButtonPanel;
    chkDefault : TCheckBox;
    chkBold : TCheckBox;
    chkItalic : TCheckBox;
    chkUnder : TCheckBox;
    colBackCol : TColorButton;
    colTextCol : TColorButton;
    dePastaRootModulo : TDirectoryEdit;
    fnArqExc : TFileNameEdit;
    Label1 : TLabel;
    Label2 : TLabel;
    Label3 : TLabel;
    Label4 : TLabel;
    Label5 : TLabel;
    Label6 : TLabel;
    lbArqExc : TListBox;
    lbModulos : TListBox;
    lbElementosSintaxe : TListBox;
    PageControl1: TPageControl;
    Splitter1: TSplitter;
    SynEdit1: TSynEdit;
    TabSheet1 : TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3 : TTabSheet;
    TIPropertyGrid1: TTIPropertyGrid;
    procedure bbAddArqExcClick(Sender : TObject);
    procedure bbRemoverArqExcClick(Sender : TObject);
    procedure chkDefaultChange(Sender : TObject);
    procedure dePastaRootModuloChange(Sender : TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbArqExcClick(Sender : TObject);
    procedure lbElementosSintaxeSelectionChange(Sender : TObject; User : boolean);
    procedure lbModulosSelectionChange(Sender : TObject; User : boolean);
    procedure OKButtonClick(Sender: TObject);
  private
    procedure UpdateDisplayElemento(pValue:TLCElementoSintaxe);

    { private declarations }
  public
    { public declarations }
  end;

var
  FSettingsEditor: TFSettingsEditor;

implementation

{$R *.lfm}

uses ufmain, LCSynEdit;

{ TFSettingsEditor }

procedure TFSettingsEditor.OKButtonClick(Sender: TObject);
begin
  if lbModulos.Tag <> -1 then
  begin
    TModuloVetorh(lbModulos.Items.Objects[lbModulos.Tag]).PastaBase := dePastaRootModulo.Directory;
    TModuloVetorh(lbModulos.Items.Objects[lbModulos.Tag]).ArquivosExcluidos.Assign(lbArqExc.Items);
  end;

  if lbElementosSintaxe.Tag <> -1 then
  begin
    TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[lbElementosSintaxe.Tag]).DefaultValues := chkDefault.checked;
    TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[lbElementosSintaxe.Tag]).Atributos.Background := colBackCol.ButtonColor;
    TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[lbElementosSintaxe.Tag]).Atributos.Foreground := colTextCol.ButtonColor;
    TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[lbElementosSintaxe.Tag]).Atributos.Style := [];
    if chkBold.Checked then
    begin
      TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[lbElementosSintaxe.Tag]).Atributos.Style :=
        TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[lbElementosSintaxe.Tag]).Atributos.Style + [fsBold];
    end;
    if chkItalic.Checked then
    begin
      TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[lbElementosSintaxe.Tag]).Atributos.Style :=
        TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[lbElementosSintaxe.Tag]).Atributos.Style + [fsItalic];
    end;
    if chkUnder.Checked then
    begin
      TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[lbElementosSintaxe.Tag]).Atributos.Style :=
        TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[lbElementosSintaxe.Tag]).Atributos.Style + [fsUnderline];
    end;
  end;

  FrmMain.SalvarConfiguracoes;
  Self.ModalResult := mrOK;
end;

procedure TFSettingsEditor.UpdateDisplayElemento(pValue : TLCElementoSintaxe);
begin
  chkDefault.checked := pValue.DefaultValues;

  colBackCol.ButtonColor := pValue.Atributos.Background;
  colBackCol.Hint := '$' + IntToHex(pValue.Atributos.Background, 6);

  colTextCol.ButtonColor := pValue.Atributos.Foreground;
  colTextCol.Hint := '$' + IntToHex(pValue.Atributos.Foreground, 6);

  chkBold.Checked := fsBold in pValue.Atributos.Style;
  chkItalic.Checked := fsItalic in pValue.Atributos.Style;
  chkUnder.Checked := fsUnderline in pValue.Atributos.Style;
end;

procedure TFSettingsEditor.FormCreate(Sender: TObject);
var
  item:TCollectionItem;
  Modulo: TModuloVetorh;
  Elemento: TLCElementoSintaxe;
begin
  FrmMain.AtualizarPreferencias(SynEdit1);
  TIPropertyGrid1.TIObject := FrmMain.EditorSettings;

  // Aba Modulos Vetorh
  dePastaRootModulo.Directory := '';
  lbArqExc.Items.clear;
  fnArqExc.FileName := '';

  lbModulos.Clear;
  lbModulos.Tag := -1;
  for item in FrmMain.EditorSettings.ModulosVetorh do
  begin
    Modulo:= TModuloVetorh(item);
    lbModulos.AddItem(AbreviaturaModuloVetorh[modulo.Sigla], modulo);
  end;

  // Aba Elementos Sintaxe
  lbElementosSintaxe.Clear;
  lbElementosSintaxe.Tag := -1;
  for item in FrmMain.EditorSettings.Editor.ElementosSintaxe do
  begin
    Elemento:= TLCElementoSintaxe(item);
    lbElementosSintaxe.AddItem(Elemento.Description , Elemento);
  end;
  // Selecionar o primeiro item da lista
  lbElementosSintaxe.ItemIndex := 0;
  lbElementosSintaxe.Tag := 0;
  UpdateDisplayElemento(TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[0]));
end;

procedure TFSettingsEditor.lbArqExcClick(Sender : TObject);
begin
  fnArqExc.FileName := lbArqExc.GetSelectedText;
end;

procedure TFSettingsEditor.lbElementosSintaxeSelectionChange(Sender : TObject; User : boolean);
var
  i:Integer;
begin
  if lbElementosSintaxe.Tag <> -1 then
  begin
    TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[lbElementosSintaxe.Tag]).DefaultValues := chkDefault.checked;
    TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[lbElementosSintaxe.Tag]).Atributos.Background := colBackCol.ButtonColor;
    TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[lbElementosSintaxe.Tag]).Atributos.Foreground := colTextCol.ButtonColor;
    TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[lbElementosSintaxe.Tag]).Atributos.Style := [];
    if chkBold.Checked then
    begin
      TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[lbElementosSintaxe.Tag]).Atributos.Style :=
        TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[lbElementosSintaxe.Tag]).Atributos.Style + [fsBold];
    end;
    if chkItalic.Checked then
    begin
      TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[lbElementosSintaxe.Tag]).Atributos.Style :=
        TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[lbElementosSintaxe.Tag]).Atributos.Style + [fsItalic];
    end;
    if chkUnder.Checked then
    begin
      TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[lbElementosSintaxe.Tag]).Atributos.Style :=
        TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[lbElementosSintaxe.Tag]).Atributos.Style + [fsUnderline];
    end;

  end;

  //property FrameColor: TColor read FFrameColor write FFrameColor;
  //property FrameStyle: TSynLineStyle  read FFrameStyle write FFrameStyle;
  //property FrameEdges: TSynFrameEdges read FFrameEdges write FFrameEdges;

  lbElementosSintaxe.Tag := -1;
  colBackCol.Color := clNone;
  colTextCol.Color := clNone;

  if lbElementosSintaxe.SelCount > 0 then
  begin
    for i:= 0 to lbElementosSintaxe.Count - 1 do
    begin
      if lbElementosSintaxe.Selected[i] = true then
      begin
        lbElementosSintaxe.Tag := i;

        UpdateDisplayElemento(TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[i]));
      end;
    end;
  end;
end;

procedure TFSettingsEditor.dePastaRootModuloChange(Sender : TObject);
begin
  fnArqExc.InitialDir := dePastaRootModulo.Directory;
end;

procedure TFSettingsEditor.bbAddArqExcClick(Sender : TObject);
begin
  if (fnArqExc.FileName = '') then
  begin
    showmessage('O Arquivo não foi informado.');
    exit;
  end;

  if (FileExists(fnArqExc.FileName) = false) then
  begin
    showmessage(format('O Arquivo "%s" não existe.',[fnArqExc.FileName]));
    exit;
  end;

  // Verificar se o arquivo já foi incluído
  if lbArqExc.Items.IndexOf(fnArqExc.FileName) < 0 then
  begin
    lbArqExc.Items.Add(fnArqExc.FileName);
  end;

  fnArqExc.FileName := '';
end;

procedure TFSettingsEditor.bbRemoverArqExcClick(Sender : TObject);
begin
  lbArqExc.DeleteSelected();
end;

procedure TFSettingsEditor.chkDefaultChange(Sender : TObject);
var
  Elemento: TLCElementoSintaxe;
  oPadrao: TSynLCHighlighterSettings;
begin
  if chkDefault.checked = true then
  begin
    oPadrao:= TSynLCHighlighterSettings.Create;
    Elemento := TLCElementoSintaxe.Create(nil);
    try
      Elemento.DefaultValues := true;
      Elemento.Kind := TLCElementoSintaxe(lbElementosSintaxe.Items.Objects[lbElementosSintaxe.Tag]).Kind;
      Elemento.Atributos.Assign(FrmMain.GetAttributeSettingsDefault(oPadrao, Elemento.Kind));
      UpdateDisplayElemento(Elemento);
    finally
      FreeAndNil(Elemento);
      FreeAndNil(oPadrao);
    end;
  end;
  colBackCol.Enabled := not chkDefault.checked;
  colTextCol.Enabled := not chkDefault.checked;
  chkBold.Enabled := not chkDefault.checked;
  chkItalic.Enabled := not chkDefault.checked;
  chkUnder.Enabled := not chkDefault.checked;
end;

procedure TFSettingsEditor.lbModulosSelectionChange(Sender : TObject; User : boolean);
var
  i:Integer;
begin
  if lbModulos.Tag <> -1 then
  begin
    TModuloVetorh(lbModulos.Items.Objects[lbModulos.Tag]).PastaBase := dePastaRootModulo.Directory;
    TModuloVetorh(lbModulos.Items.Objects[lbModulos.Tag]).ArquivosExcluidos.Assign(lbArqExc.Items);
  end;

  lbModulos.Tag := -1;
  dePastaRootModulo.Directory := '';
  lbArqExc.Items.clear;
  fnArqExc.FileName := '';

  if lbModulos.SelCount > 0 then
  begin
    for i:= 0 to lbModulos.Count - 1 do
    begin
      if lbModulos.Selected[i] = true then
      begin
        lbModulos.Tag := i;
        dePastaRootModulo.Directory := TModuloVetorh(lbModulos.Items.Objects[i]).PastaBase;
        lbArqExc.Items.Assign(TModuloVetorh(lbModulos.Items.Objects[i]).ArquivosExcluidos);
      end;
    end;
  end;
end;


initialization


end.


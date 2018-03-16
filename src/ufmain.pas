unit ufmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Fgl,
  Dialogs, ComCtrls, Menus, ActnList, StdActns, ECTabCtrl,
  LCLTranslator, LCLIntf, LCLType, ExtCtrls, StdCtrls, EditBtn, Buttons, LConvEncoding,
  uEditorLspSettings, LCSynEdit, syneditmarkupfoldcoloring, SynEditMiscClasses,
  SynLCHighlighter, SynLCCompletion, uCompilador, SynEditHighlighter,
  SynEdit, SynEditTypes, ECTypes, ECAccordion, VirtualTrees;

resourcestring
  rsEditorName = 'BCCS ::. Editor para Linguagem Senior de Programacao .::';
  rsQuestionSaveFile = 'Salvar alteracoes em "%s" ?';
  rsQuestionConfirmExitApp = 'Voce quer Finalizar este Programa ?';
  rsQuestionSearchBeginFile = 'Deseja continuar do Inicio do Arquivo?';
  rsWarningTextNotFound = 'O texto "%s" nao pode ser localizado!';
  rsUnchanged = 'Nao Alterado';
  rsChanged = 'Alterado';
  rsReplace = 'Substituir';
  rsInsert = 'Inserir';

Const
  EndOfLine: shortstring = LineEnding;
  ModifiedStrs: array[boolean] of string = (rsUnchanged, rsChanged);
  InsertModeStrs: array[boolean] of string = (rsReplace, rsInsert);

  CompletionLinesInWindow = 15; // Quantidade de linhas Máxima de linhas se apresentado na lista de funções

  FILEAUTOCOMPLETELIST = 'AutoCompleteList.txt';
  FILECOMPLETIONPROPOSAL = 'CompletionProposal.txt';

  // Identificadores dos Panels do StatusBar
  iPnlCaretXY = 0;
  iPnlMode    = 1;
  iPnlModified= 2;
  iPnlNumLock = 3;
  iPnlCapital = 4;
  iPnlScroll  = 5;
  iPnlSize    = 6;
  iPnlModoSel = 7;
  iPnlReadOnly= 8;
  iPnlFileCode= 9;
  iPnlMessage = 10;

type
  TLCArquivoRegra = Record
    NomeArquivo:String;
    Funcoes: TLCListOfFunction;
  end;

  TLCModuloVetorh = Record
    Id: TLCSiglaModuloVetorh;
    Carregado: Boolean;
    PastaBase: String;
    ArquivosExcluidos:TStrings;
    Regras: array of TLCArquivoRegra;
  end;

  // Record utilizado para apresentar as mensagens da validação de sintaxe
  PLCMessageNode = ^TLCMessageNode;
  TLCMessageNode = record
    Coluna: Integer;
    ImageIndex: Integer;
    Linha: Integer;
    TextoEstatico,
    Texto: String;
    Tipo: TLCTypeOfMessages;
  end;

  // Record Utilizado para os arquivos/diretorios
  PFileNode = ^TFileNode;
  TFileNode = record
    Name,
    FullPath:String;
    IsFolder:Boolean;
    OpenIndex,
    CloseIndex: Integer;
  end;

  { TLCPalavra }

  TLCPalavra = class
  private
    fDescricao : String;
    FParametros : String;
    fTexto : String;
    fTipo : TLCTokenKind;
  protected
  public
    constructor Create;
    //destructor Destroy; virtual;
    function GetSintaxe: String;
  published
    property Texto:String read fTexto write fTexto;
    property Descricao:String read fDescricao write fDescricao;
    Property Parametros:String read FParametros write FParametros;
    Property Sintaxe:String read GetSintaxe;
    property Tipo:TLCTokenKind read fTipo write fTipo;
end;

  { TLCListOfPalavras }

  TLCListOfPalavras = class(specialize TFPGObjectList<TLCPalavra>)
  private
    fIndice: array['a'..'z'] of integer;

    procedure LimparIndices;
  protected

  public
    constructor Create(pFreeObjects: Boolean = True); virtual;
    destructor Destroy; override;

    procedure AtualizarInidices;
    function GetStartIndex(Const pValue:String):Integer;
  published

  end;

  { TCompletionProposal }

  TCompletionProposal = class(TCollectionItem)
  private
    FNome: String;
    FParametros: String;
    FDescricao: String;
    function GetSintaxe: String;
  protected
  public
    Property Nome:String read FNome write FNome;
    Property Parametros:String read FParametros write FParametros;
    Property Sintaxe:String read GetSintaxe;
    Property Descricao:String read FDescricao write FDescricao;
  published
  end;

  { TCompletionProposalList }

  TCompletionProposalList = class(TCollection)
  private
  protected
  public
    function IndexOf(aValue:String):Integer;
  published
  end;

  { TDefComando }

  TDefComando = class(TPersistent)
  private
    FNome: String;
    FDescricao: String;
    FSintaxe: String;
  public
    property Nome:String read FNome write FNome;
    property Descricao:String read FDescricao write FDescricao;
    property Sintaxe:String read FSintaxe write FSintaxe;
  end;

  { TFrmMain }

  TFrmMain = class(TForm)
    aciComandos : TAccordionItem;
    aciFuncoes : TAccordionItem;
    aciVariaveis : TAccordionItem;
    actAbrir: TFileOpen;
    actAbrirTodosRecentes: TAction;
    actComentarSelecao: TAction;
    actConverterInverter: TAction;
    actConverterMaiusculo: TAction;
    actConverterMinusculo: TAction;
    actConverterPrimeiraMaius: TAction;
    actCopiarTudo: TAction;
    actDuplicarTexto: TAction;
    actEditRedo: TAction;
    actFecharAtual: TAction;
    actFecharTodos: TAction;
    actFecharTodosExceto: TAction;
    actFormatarTexto: TAction;
    actIdentarAumentar: TAction;
    actIdentarDiminuir: TAction;
    actInseriCarEspASCII: TAction;
    actInseriCarEspHTML: TAction;
    actInserirData: TAction;
    actInserirDataCompleta: TAction;
    actInserirDataHora: TAction;
    actInserirDirArqAtual: TAction;
    actInserirHora: TAction;
    actInserirNmArqAtual: TAction;
    actAutoCompletar: TAction;
    actCircundarSelecao : TAction;
    actChecarSintaxe : TAction;
    actShowMsg : TAction;
    actMostrarExplorer: TAction;
    actMoverLinhasParaCima: TAction;
    actMoverLinhasParaBaixo: TAction;
    actMostrarComandos : TAction;
    actPreferencias: TAction;
    actParametrosFuncoes: TAction;
    ActionList1: TActionList;
    actIrPara0: TAction;
    actIrPara1: TAction;
    actIrPara2: TAction;
    actIrPara3: TAction;
    actIrPara4: TAction;
    actIrPara5: TAction;
    actIrPara6: TAction;
    actIrPara7: TAction;
    actIrPara8: TAction;
    actIrPara9: TAction;
    actIrParaLinha: TAction;
    actLimparRecentes: TAction;
    actLimparTodos: TAction;
    actLocalizar: TAction;
    actLocalizarProximo: TAction;
    actMarcar0: TAction;
    actMarcar1: TAction;
    actMarcar2: TAction;
    actMarcar3: TAction;
    actMarcar4: TAction;
    actMarcar5: TAction;
    actMarcar6: TAction;
    actMarcar7: TAction;
    actMarcar8: TAction;
    actMarcar9: TAction;
    actModoColuna: TAction;
    actModoLinha: TAction;
    actModoNormal: TAction;
    actMostrarCaractEspeciais: TAction;
    actNovo: TAction;
    actProximaTab: TAction;
    actSair: TFileExit;
    actSalvar: TAction;
    actSalvarComo: TAction;
    actSalvarTodos: TAction;
    actSomenteLeitura: TAction;
    actSubstituir: TAction;
    actTabAnterior: TAction;
    DirectoryEdit1 : TDirectoryEdit;
    ecaLista : TECAccordion;
    ECTabCtrl1 : TECTabCtrl;
    EditCopy1: TEditCopy;
    EditCut1: TEditCut;
    EditDelete1: TEditDelete;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    FindDialog1: TFindDialog;
    IdleTimer1 : TIdleTimer;
    ImageListExplorer: TImageList;
    ImageListActions: TImageList;
    ImageListBookmark: TImageList;
    ImageListStatusBar: TImageList;
    ImageTabs: TImageList;
    ImageMessages : TImageList;
    lbDescricao : TLabel;
    lbSintaxe : TLabel;
    lbxComandos : TListBox;
    lbxFuncoes : TListBox;
    lbxVariaveis : TListBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem100: TMenuItem;
    MenuItem101: TMenuItem;
    MenuItem102: TMenuItem;
    MenuItem103: TMenuItem;
    MenuItem104: TMenuItem;
    MenuItem105: TMenuItem;
    MenuItem106: TMenuItem;
    MenuItem107: TMenuItem;
    MenuItem108: TMenuItem;
    MenuItem109: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem110: TMenuItem;
    MenuItem111: TMenuItem;
    MenuItem112: TMenuItem;
    MenuItem113: TMenuItem;
    MenuItem114: TMenuItem;
    MenuItem115: TMenuItem;
    MenuItem116: TMenuItem;
    MenuItem117: TMenuItem;
    MenuItem118: TMenuItem;
    MenuItem119: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem120: TMenuItem;
    MenuItem121: TMenuItem;
    MenuItem122: TMenuItem;
    MenuItem123: TMenuItem;
    MenuItem124: TMenuItem;
    MenuItem125: TMenuItem;
    MenuItem126: TMenuItem;
    MenuItem127 : TMenuItem;
    MenuItem128 : TMenuItem;
    MenuItem129: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem130: TMenuItem;
    MenuItem131: TMenuItem;
    MenuItem132: TMenuItem;
    MenuItem133: TMenuItem;
    MenuItem136 : TMenuItem;
    MenuItem137 : TMenuItem;
    MenuItem138 : TMenuItem;
    MenuItem139 : TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem58: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem60: TMenuItem;
    MenuItem61: TMenuItem;
    MenuItem62: TMenuItem;
    MenuItem63: TMenuItem;
    MenuItem64: TMenuItem;
    MenuItem65: TMenuItem;
    MenuItem66: TMenuItem;
    MenuItem67: TMenuItem;
    MenuItem68: TMenuItem;
    MenuItem69: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem70: TMenuItem;
    MenuItem71: TMenuItem;
    MenuItem72: TMenuItem;
    MenuItem73: TMenuItem;
    MenuItem74: TMenuItem;
    MenuItem75: TMenuItem;
    MenuItem76: TMenuItem;
    MenuItem77: TMenuItem;
    MenuItem78: TMenuItem;
    MenuItem79: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem80: TMenuItem;
    MenuItem81: TMenuItem;
    MenuItem82: TMenuItem;
    MenuItem83: TMenuItem;
    MenuItem84: TMenuItem;
    MenuItem85: TMenuItem;
    MenuItem86: TMenuItem;
    MenuItem87: TMenuItem;
    MenuItem88: TMenuItem;
    MenuItem89: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem90: TMenuItem;
    MenuItem91: TMenuItem;
    MenuItem92: TMenuItem;
    MenuItem93: TMenuItem;
    MenuItem94: TMenuItem;
    MenuItem95: TMenuItem;
    MenuItem96: TMenuItem;
    MenuItem97: TMenuItem;
    MenuItem98: TMenuItem;
    MenuItem99: TMenuItem;
    Panel1 : TPanel;
    Panel2 : TPanel;
    Panel4 : TPanel;
    pnlExplorer: TPanel;
    pnlComandos : TPanel;
    Panel3 : TPanel;
    pnlEditor : TPanel;
    PopupEditor: TPopupMenu;
    PopupMRU: TPopupMenu;
    PopupTabs: TPopupMenu;
    ReplaceDialog1: TReplaceDialog;
    SaveDialog1: TSaveDialog;
    SpeedButton1 : TSpeedButton;
    Splitter1 : TSplitter;
    Splitter2 : TSplitter;
    Splitter3: TSplitter;
    Splitter4 : TSplitter;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButton27: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    ToolButton3: TToolButton;
    ToolButton30 : TToolButton;
    ToolButton31 : TToolButton;
    ToolButton32 : TToolButton;
    ToolButton33 : TToolButton;
    ToolButton34 : TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    vstExplorer: TVirtualStringTree;
    vstMessages : TVirtualStringTree;
    procedure actAbrirAccept(Sender: TObject);
    procedure actAbrirBeforeExecute(Sender : TObject);
    procedure actAbrirTodosRecentesExecute(Sender: TObject);
    procedure actAutoCompletarExecute(Sender: TObject);
    procedure actChecarSintaxeExecute(Sender : TObject);
    procedure actCircundarSelecaoExecute(Sender : TObject);
    procedure actMostrarComandosExecute(Sender : TObject);
    procedure actComentarSelecaoExecute(Sender: TObject);
    procedure actConverterInverterExecute(Sender: TObject);
    procedure actConverterMaiusculoExecute(Sender: TObject);
    procedure actConverterMinusculoExecute(Sender: TObject);
    procedure actConverterPrimeiraMaiusExecute(Sender: TObject);
    procedure actCopiarTudoExecute(Sender: TObject);
    procedure actDuplicarTextoExecute(Sender: TObject);
    procedure actFecharAtualExecute(Sender: TObject);
    procedure actFecharTodosExcetoExecute(Sender: TObject);
    procedure actFecharTodosExecute(Sender: TObject);
    procedure actIdentarAumentarExecute(Sender: TObject);
    procedure actIdentarDiminuirExecute(Sender: TObject);
    procedure actInseriCarEspASCIIExecute(Sender: TObject);
    procedure actInseriCarEspHTMLExecute(Sender: TObject);
    procedure actInserirDataCompletaExecute(Sender: TObject);
    procedure actInserirDataExecute(Sender: TObject);
    procedure actInserirDataHoraExecute(Sender: TObject);
    procedure actInserirDirArqAtualExecute(Sender: TObject);
    procedure actInserirHoraExecute(Sender: TObject);
    procedure actInserirNmArqAtualExecute(Sender: TObject);
    procedure actIrPara0Execute(Sender: TObject);
    procedure actIrPara0Update(Sender: TObject);
    procedure actIrParaLinhaExecute(Sender: TObject);
    procedure actLimparRecentesExecute(Sender: TObject);
    procedure actLimparTodosExecute(Sender: TObject);
    procedure actLimparTodosUpdate(Sender: TObject);
    procedure actLocalizarExecute(Sender: TObject);
    procedure actLocalizarProximoExecute(Sender: TObject);
    procedure actMarcar0Execute(Sender: TObject);
    procedure actMarcar0Update(Sender: TObject);
    procedure actModoColunaExecute(Sender: TObject);
    procedure actModoLinhaExecute(Sender: TObject);
    procedure actModoNormalExecute(Sender: TObject);
    procedure actMostrarCaractEspeciaisExecute(Sender: TObject);
    procedure actMostrarCaractEspeciaisUpdate(Sender: TObject);
    procedure actMostrarExplorerExecute(Sender: TObject);
    procedure actParametrosFuncoesExecute(Sender: TObject);
    procedure actPreferenciasExecute(Sender: TObject);
    procedure actProximaTabExecute(Sender: TObject);
    procedure actSalvarComoExecute(Sender: TObject);
    procedure actSalvarExecute(Sender: TObject);
    procedure ActionList1Update({%H-}AAction: TBasicAction; var {%H-}Handled: Boolean);
    procedure actNovoExecute(Sender: TObject);
    procedure actSalvarTodosExecute(Sender: TObject);
    procedure actSalvarTodosUpdate(Sender: TObject);
    procedure actShowMsgExecute(Sender : TObject);
    procedure actSomenteLeituraExecute(Sender: TObject);
    procedure actSomenteLeituraUpdate(Sender: TObject);
    procedure actSubstituirExecute(Sender: TObject);
    procedure actTabAnteriorExecute(Sender: TObject);
    procedure actEditRedoExecute(Sender: TObject);
    procedure DirectoryEdit1AcceptDirectory(Sender: TObject; Var Value: String);
    procedure DirectoryEdit1EditingDone(Sender: TObject);
    procedure ECTabCtrl1CloseQuery(Sender: TObject; AIndex: Integer;
      var CanClose: Boolean);
    procedure ECTabCtrl1MouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure FindDialog1Find(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure IdleTimer1Timer(Sender : TObject);
    procedure lbxFuncoesDblClick(Sender : TObject);
    procedure lbxFuncoesSelectionChange(Sender : TObject; {%H-}User : boolean);
    procedure MenuItem29Click(Sender: TObject);
    procedure MenuItemMRUClick(Sender: TObject);
    procedure ReplaceDialog1Find(Sender: TObject);
    procedure ReplaceDialog1Replace(Sender: TObject);
    procedure ECTabCtrl1Change(Sender: TObject);
    procedure SpeedButton1Click(Sender : TObject);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure StatusBar1Hint(Sender: TObject);
    procedure vstExplorerCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; {%H-}Column: TColumnIndex; var Result: Integer);
    procedure vstExplorerFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstExplorerGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      {%H-}Column: TColumnIndex; var {%H-}LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure vstExplorerGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; {%H-}Kind: TVTImageKind; {%H-}Column: TColumnIndex;
      var {%H-}Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstExplorerGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      {%H-}Column: TColumnIndex; {%H-}TextType: TVSTTextType; var CellText: String);
    procedure vstExplorerInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure vstExplorerInitNode(Sender: TBaseVirtualTree; {%H-}ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vstExplorerNodeDblClick(Sender: TBaseVirtualTree;
      const {%H-}HitInfo: THitInfo);
    procedure vstExplorerPaintText(Sender : TBaseVirtualTree; const TargetCanvas : TCanvas; Node : PVirtualNode;
      {%H-}Column : TColumnIndex; {%H-}TextType : TVSTTextType);
    procedure vstMessagesFreeNode(Sender : TBaseVirtualTree; Node : PVirtualNode);
    procedure vstMessagesGetImageIndex(Sender : TBaseVirtualTree; Node : PVirtualNode; {%H-}Kind : TVTImageKind;
      {%H-}Column : TColumnIndex; var {%H-}Ghosted : Boolean; var ImageIndex : Integer);
    procedure vstMessagesGetText(Sender : TBaseVirtualTree; Node : PVirtualNode; {%H-}Column : TColumnIndex;
      TextType : TVSTTextType; var CellText : String);
    procedure vstMessagesNodeClick(Sender : TBaseVirtualTree; const HitInfo : THitInfo);
    procedure vstMessagesPaintText(Sender : TBaseVirtualTree; const TargetCanvas : TCanvas; {%H-}Node : PVirtualNode;
      {%H-}Column : TColumnIndex; TextType : TVSTTextType);

  private
    fSettings: TEditorLspSettings;
    fSynCmp: TSynLCCompletion;
    FSynLsp: TSynLCHighlighter;
    fSynAuC: TSynLCAutoComplete;
    FCompilador: TLCCompilador;
    configFileName:String;
    fPalavras: TLCListOfPalavras;

    Modulos: array[TLCSiglaModuloVetorh] of TLCModuloVetorh;

    function AdicionarRegraAoModulo(var aModulo:TLCModuloVetorh; const sFile: String):integer;
    procedure InicializarListaModulos;
    procedure LimparModulo(var aModulo:TLCModuloVetorh);
    procedure CarregarModulo(var aModulo:TLCModuloVetorh);
    function GetModuloByFileName(const aFileName:String):TLCSiglaModuloVetorh;
    function GetPosicaoRegraByFileName(const Sigla:TLCSiglaModuloVetorh; const aFileName:String):Integer;

    procedure LerDiretorio(Sender: TBaseVirtualTree; Node: PVirtualNode; filtro: TStrings; diretorioBase:string);

    procedure DoIdle(Sender: TObject; var Done: Boolean);
    procedure DoReplaceText(Sender: TObject);
    procedure DoFindText(Sender: TObject);
    procedure DoAutoSave;

    procedure DoExceptionHandler(Sender: TObject; E: Exception);
    procedure DoSpecialLineMarkup(Sender : TObject; Line : integer; var Special : boolean;
      Markup : TSynSelectedColor);

    procedure DoExecuteCompletion(Sender:TObject);
    procedure DoSearchPosition(var APosition: integer);
    procedure DoAddCompletion;

    procedure GetTip(Sender: TObject; aWord: string; const aRow:Integer; var aTip:string);
    procedure GetCustomFunction(Sender: TObject; Const pToken:String; var pIdentKind:TLCTokenKind);
    procedure CarregarCustomFunction(var pModulo:TLCModuloVetorh; pHL:TSynLCHighlighter);

    procedure AddMsg(sMsg: String; sMsgEstatica:String = '');
    procedure AddMsg(pMsg: TLCMessages);

    procedure CriarMenuMRU();
    procedure CriarBackupArquivo(aFileName:String);

    procedure ShowMsgAviso(aShow : Boolean);
    procedure ShowExplorer(aShow : Boolean);
    procedure ShowListOfCommands(aShow : Boolean);
    procedure CarregarConfiguracoes(EhChamadaInterna : Boolean = false);
    function FecharTodasAbas(ExcetoAtiva:Boolean; SaveSession:Boolean = false):Boolean;
    procedure CarregarCompletionProposal;
    procedure CarregarListaComandos;

    procedure CarregarPalavras;

    procedure MostrarArquivosAbertos;
    function DirectoryIsOpen(ADirectory : String) : Boolean;
    function FileIsOpen(pFileName: String; pAtivar:Boolean = true): Boolean;
    function FecharDocumento(editor:TLCSynEdit):Boolean;
    procedure NovoDocumento(NomeArquivo:String; Ativar:Boolean = true);

    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);

    { private declarations }
  public
    LookupList : TCompletionProposalList;

    function GetEditorAtivo:TLCSynEdit;
    function VersionOfApplication:String;

    function GetAttributeSettingsDefault(pPadrao : TSynLCHighlighterSettings; const pValue : TLCTokenKind) : TSynLCAttributeSettings;
    procedure AtualizarPreferencias(editor:TSynEdit);
    procedure SalvarConfiguracoes;

    property EditorSettings:TEditorLspSettings read fSettings;
    { public declarations }
  end;

  function OrdenarPalavras(const Item1, Item2 : TLCPalavra) : Integer;

  procedure EncloseTextSelection(const Template: string; Source: TStrings;
                               SelectionStart, SelectionEnd: TPoint;
                               out NewSelection: string; out NewCursor: TPoint);


var
  FrmMain: TFrmMain;

implementation

uses SynEditKeyCmds
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  , fileinfo
  , winpeimagereader {need this for reading exe info}
  , elfreader {needed for reading ELF executables}
  , machoreader {needed for reading MACH-O executables}
  , Math, LazUTF8, LazFileUtils, ufirparalinha, uFSobre, ufcaracteresespeciais
  , BasicCodeTools, CodeToolManager, SourceChanger, dateutils
  , SynEditMarkupHighAll
  , LazLogger, Masks
  , ufParametrosFuncoes, uFAutoCompletar, uFSettingsEditor;

{$R *.lfm}

function OrdenarPalavras(const Item1, Item2 : TLCPalavra) : Integer;
begin
  result := 0;
  if lowerCase(Item1.Texto) > lowerCase(Item2.Texto) then
  begin
    result := 1;
  end
  else
  if lowerCase(Item1.Texto) < lowerCase(Item2.Texto) then
  begin
    result := -1;
  end;
end;

procedure EncloseTextSelection(const Template: string; Source: TStrings;
  SelectionStart, SelectionEnd: TPoint; out NewSelection: string; out
  NewCursor: TPoint);
var
  TemplateLen: Integer;
  TemplatePos: Integer;
  LastWrittenTemplatePos: Integer;
  NewSelect: TMemoryStream;
  Y: Integer;
  X: Integer;
  OldSelectionIndent: Integer;
  TemplateIndent: Integer;
  CutLastLineBreak: Boolean;
  CutPos: Integer;

  procedure AddBeautified(const s: string);
  var
    NewStr: String;
    LengthOfLastLine: integer;
    LineEndCnt: Integer;
    CurIndent: Integer;
    FirstLineIndent: Integer;
    EndPos: Integer;
  begin
    if s='' then exit;
    NewStr:=s;
    CurIndent:=OldSelectionIndent;
    if NewSelect.Position=0 then
    begin
      FirstLineIndent:=OldSelectionIndent-SelectionStart.X+1;
      if FirstLineIndent<0 then
        FirstLineIndent:=0;
      NewStr:=GetIndentStr(FirstLineIndent)+NewStr;
      dec(CurIndent,FirstLineIndent);
      if CurIndent<0 then
        CurIndent:=0;
    end;
    //debugln('AddBeautified A X=',X,' Y=',Y,' CurIndent=',CurIndent,' NewStr="',NewStr,'"');
    dec(CurIndent,GetLineIndent(NewStr,1));
    if CurIndent<0 then CurIndent:=0;
    NewStr:=CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
                NewStr,CurIndent,
                [bcfIndentExistingLineBreaks,bcfDoNotIndentFirstLine]);
    LineEndCnt:=LineEndCount(NewStr,LengthOfLastLine);
    if (TemplatePos>TemplateLen) then begin
      // cut indent at end of template
      if LineEndCnt>0 then begin
        EndPos:=length(NewStr);
        while (EndPos>=1) and (NewStr[EndPos]=' ') do dec(EndPos);
        NewStr:=copy(NewStr,1,length(NewStr)-CurIndent);
        LineEndCnt:=LineEndCount(NewStr,LengthOfLastLine);
      end;
    end;
    inc(Y,LineEndCnt);
    if LineEndCnt=0 then
      inc(X,LengthOfLastLine)
    else
      X:=LengthOfLastLine+1;
    if (LineEndCnt>0) or (NewSelect.Position=0) then
      TemplateIndent:=GetLineIndent(NewStr,length(NewStr)+1);
    //debugln('AddBeautified B X=',X,' Y=',Y,' TemplateIndent=',TemplateIndent,' LengthOfLastLine=',LengthOfLastLine,' NewStr="',NewSTr,'"');
    NewSelect.Write(NewStr[1],length(NewStr));
  end;

  procedure FlushTemplate;
  var
    FromPos: Integer;
    ToPos: Integer;
  begin
    FromPos:=LastWrittenTemplatePos+1;
    ToPos:=TemplatePos-1;
    if ToPos>TemplateLen then ToPos:=TemplateLen;
    if FromPos<=ToPos then
      AddBeautified(copy(Template,FromPos,ToPos-FromPos+1));
    LastWrittenTemplatePos:=ToPos;
  end;

  procedure CalculateCursorPos;
  begin
    NewCursor:=Point(X,Y);
  end;

  procedure InsertSelection;
  var
    CurY: Integer;
    CurLine: string;
    IndentStr: String;
    MinX: Integer;
    MaxX: Integer;
    l: Integer;
  begin
    IndentStr:=GetIndentStr(TemplateIndent-OldSelectionIndent);
    for CurY:=SelectionStart.Y to SelectionEnd.Y do begin
      CurLine:=Source[CurY-1];
      //debugln(['InsertSelection CurY=',CurY,' CurLine="',dbgstr(CurLine),'"']);
      MinX:=1;
      MaxX:=length(CurLine)+1;
      if (CurY=SelectionStart.Y) then begin
        MinX:=SelectionStart.X;
        if MinX<=OldSelectionIndent then
          MinX:=OldSelectionIndent+1;
        if MinX>MaxX then
          MinX:=MaxX;
      end;
      if (CurY=SelectionEnd.Y) and (MaxX>SelectionEnd.X) then
        MaxX:=SelectionEnd.X;
      //debugln(['InsertSelection CurY=',CurY,' Range=',MinX,'-',MaxX,' Indent="',length(IndentStr),'" "',copy(CurLine,MinX,MaxX-MinX),'"']);
      X:=1;
      // write indent
      if (IndentStr<>'') and (CurY<>SelectionStart.Y) then begin
        NewSelect.Write(IndentStr[1],length(IndentStr));
        inc(X,length(IndentStr));
      end;
      // write line
      l:=MaxX-MinX;
      if l>0 then begin
        NewSelect.Write(CurLine[MinX],l);
        inc(X,l);
      end;
      // write line break and adjust cursor
      if CurY<SelectionEnd.Y then begin
        NewSelect.Write(EndOfLine[1],length(EndOfLine));
        inc(Y);
        X:=1;
      end;
    end;
  end;

  procedure ParseMacro;
  var
    MacroNameStart: Integer;
    MacroNameEnd: Integer;

    function MacroNameIs(const Name: string): boolean;
    begin
      Result:=CompareText(@Template[MacroNameStart],MacroNameEnd-MacroNameStart,
                          @Name[1],length(Name),false)=0;
    end;

  begin
    FlushTemplate;
    inc(TemplatePos);
    MacroNameStart:=TemplatePos;
    while (TemplatePos<=TemplateLen)
    and (Template[TemplatePos] in ['a'..'z','A'..'Z','_','0'..'9']) do
      inc(TemplatePos);
    MacroNameEnd:=TemplatePos;
    if (TemplatePos<=TemplateLen) and (Template[TemplatePos]='>') then begin
      LastWrittenTemplatePos:=TemplatePos;
      inc(TemplatePos);
      if MacroNameIs('Selection') then begin
        InsertSelection;
      end;
    end;
  end;

  procedure GetOldSelectionIndent;
  var
    CurY: Integer;
    CurLine: string;
    CurIndent: Integer;
  begin
    OldSelectionIndent:=0;
    CurY:=SelectionStart.Y;
    while CurY<Source.Count do begin
      CurLine:=Source[CurY-1];
      CurIndent:=GetLineIndent(CurLine,1);
      if CurIndent<length(CurLine) then begin
        OldSelectionIndent:=CurIndent;
        break;
      end;
      inc(CurY);
    end;
  end;

begin
  //debugln(['EncloseTextSelection A ',SelectionStart.X,',',SelectionStart.Y,'-',SelectionEnd.X,',',SelectionEnd.Y,' indent=',Indent,' Template="',Template,'"']);
  NewSelection:='';
  NewCursor:=Point(0,0);
  CutLastLineBreak:=true;
  if (SelectionEnd.X=1) and (SelectionEnd.Y>SelectionStart.Y) then begin
    CutLastLineBreak:=false;
    dec(SelectionEnd.Y);
    if SelectionEnd.Y<Source.Count then
      SelectionEnd.X:=length(Source[SelectionEnd.Y-1])+1;
  end;
  NewSelect:=TMemoryStream.Create;
  NewCursor:=SelectionStart;
  X:=NewCursor.X;
  Y:=NewCursor.Y;
  GetOldSelectionIndent;
  TemplateIndent:=OldSelectionIndent;
  try
    TemplateLen:=length(Template);
    TemplatePos:=1;
    LastWrittenTemplatePos:=TemplatePos-1;
    while TemplatePos<=TemplateLen do begin
      case Template[TemplatePos] of
        '\':
          begin
            FlushTemplate;
            LastWrittenTemplatePos:=TemplatePos;
            inc(TemplatePos,2);
          end;

        '|':
          begin
            FlushTemplate;
            CalculateCursorPos;
            LastWrittenTemplatePos:=TemplatePos;
            inc(TemplatePos);
          end;

        '<':
          ParseMacro;

      else
        inc(TemplatePos);
      end;
    end;
    FlushTemplate;
  finally
    SetLength(NewSelection,NewSelect.Size);
    if NewSelection<>'' then begin
      NewSelect.Position:=0;
      NewSelect.Read(NewSelection[1],length(NewSelection));
      //debugln(['EncloseTextSelection CutLastLineBreak=',CutLastLineBreak,' NewSelection="',NewSelection,'"']);
      if CutLastLineBreak then begin
        CutPos:=length(NewSelection);
        if NewSelection[CutPos] in [#10,#13] then begin
          dec(CutPos);
          if (CutPos>=1) and (NewSelection[CutPos] in [#10,#13])
          and (NewSelection[CutPos]<>NewSelection[CutPos+1]) then begin
            dec(CutPos);
          end;
          NewSelection:=copy(NewSelection,1,CutPos);
        end;
      end;
    end;
    NewSelect.Free;
  end;
end;

{ TLCListOfPalavras }

procedure TLCListOfPalavras.LimparIndices;
var
  i:Char;
begin
  for I := 'a' to 'z' do
  begin
    fIndice[i] := -1;
  end;
end;

constructor TLCListOfPalavras.Create(pFreeObjects : Boolean);
begin
  inherited Create(pFreeObjects);

  LimparIndices;
end;

destructor TLCListOfPalavras.Destroy;
begin
  inherited Destroy;
end;

procedure TLCListOfPalavras.AtualizarInidices;
var
  i: integer;
  letra:char;
begin
  LimparIndices;

  for i:= 0 to Count - 1 do
  begin
    letra := lowerCase(Items[i].Texto.Chars[0]);

    if letra in ['a'..'z'] then
    begin
      if fIndice[letra] = -1 then
      begin
        fIndice[letra] := i;
      end;
    end;
  end;
end;

function TLCListOfPalavras.GetStartIndex(const pValue : String) : Integer;
var
  letra:Char;
begin
  Result := -1;

  letra := lowerCase(pValue.Chars[0]);

  if letra in ['a'..'z'] then
  begin
    Result := fIndice[letra];
  end;

  if Result < 0 then
  begin
    Result := 0;
  end;
end;

{ TLCPalavra }

constructor TLCPalavra.Create;
begin
  fDescricao := '';
  FParametros := '';
  fTexto := '';
  fTipo := tLCUnknown;
end;

function TLCPalavra.GetSintaxe : String;
begin
  if FParametros = '' then
  begin
    Result := '';
    Exit;
  end;

  Result := StringReplace(FParametros, '",',',', [rfReplaceAll]);
  Result := StringReplace(Result, '"','', [rfReplaceAll]);
  Result := fTexto + '(' + Result + ')';
end;


{ TCompletionProposalList }

function TCompletionProposalList.IndexOf(aValue: String): Integer;
var
  i:Integer;
begin
  Result := -1;
  aValue := AnsiUpperCase(aValue);

  for i := 0 to Pred(Count) do
  begin
    if AnsiUpperCase(TCompletionProposal(Items[i]).Nome) = aValue then
    begin
      Result := i;

      exit;
    end;
  end;
end;

{ TCompletionProposal }

function TCompletionProposal.GetSintaxe: String;
Var
  aSintaxe : String;
begin
  aSintaxe := StringReplace(FParametros, '",',',', [rfReplaceAll]);
  aSintaxe := StringReplace(aSintaxe, '"','', [rfReplaceAll]);
  Result := FNome + '(' + aSintaxe + ')';
end;

{ TFrmMain }

procedure TFrmMain.actNovoExecute(Sender: TObject);
begin
  NovoDocumento('');
  GetEditorAtivo.SetFocus;
end;

procedure TFrmMain.actSalvarTodosExecute(Sender: TObject);
var
  i: Integer;
  editor : TLCSynEdit;
begin
  for i := 0 to Pred(ECTabCtrl1.Tabs.Count) do
  begin
    editor := ECTabCtrl1.Tabs[i].control as TLCSynEdit;
    if (editor <> nil) then
    begin
      if editor.Modified = true then
      begin
        CriarBackupArquivo(editor.FileName);
        editor.Save;
      end;
    end;
  end; // For
end;

procedure TFrmMain.actSalvarTodosUpdate(Sender: TObject);
var
  i: integer;
  bOk: Boolean;
  editor : TLCSynEdit;
begin
  bOk := false;
  if (ECTabCtrl1.Tabs.Count > 0) then
  begin
    for i := 0 to Pred(ECTabCtrl1.Tabs.Count) do
    begin
      editor := ECTabCtrl1.Tabs[i].control as TLCSynEdit;
      if (editor <> nil) then
      begin
        if editor.Modified = true then
        begin
          bOk := true;
          break;
        end;
      end;
    end;
  end;
  actSalvarTodos.Enabled := bOK;
end;

procedure TFrmMain.actShowMsgExecute(Sender : TObject);
begin
  ShowMsgAviso(not Panel4.Visible);
end;

procedure TFrmMain.actSomenteLeituraExecute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    editor.ReadOnly := not editor.ReadOnly;
  end;
end;

procedure TFrmMain.actSomenteLeituraUpdate(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    actSomenteLeitura.Enabled := true;
    if editor.ReadOnly then
    begin
      actSomenteLeitura.ImageIndex:=62;
    end
    else
    begin
      actSomenteLeitura.ImageIndex:=61;
    end;
  end
  else
  begin
    actSomenteLeitura.Enabled := false;
  end;
end;

procedure TFrmMain.actSubstituirExecute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor = nil) then
  begin
    exit;
  end;
  if editor.SelAvail then
  begin
    FindDialog1.FindText := editor.SelText;
  end;

  if editor.SelAvail = false then
  begin
    editor.SelectWord;
  end;
  ReplaceDialog1.FindText := editor.SelText;

  ReplaceDialog1.Execute;
end;

procedure TFrmMain.actTabAnteriorExecute(Sender: TObject);
begin
  ECTabCtrl1.SelectPrevious;
end;

procedure TFrmMain.actEditRedoExecute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  if (ECTabCtrl1.Tabs.Count > 0) then
  begin
    editor := GetEditorAtivo;
    if (editor <> nil) then
    begin
      editor.CommandProcessor(ecRedo, '', nil);
    end;
  end;
end;

procedure TFrmMain.DirectoryEdit1AcceptDirectory(Sender : TObject; var Value : String);
begin
  fSettings.PathBase := Value;
  ShowExplorer(true);
end;

procedure TFrmMain.DirectoryEdit1EditingDone(Sender: TObject);
begin
  fSettings.PathBase := DirectoryEdit1.Directory;
  ShowExplorer(true);
end;

procedure TFrmMain.ECTabCtrl1CloseQuery(Sender: TObject; AIndex: Integer;
  var CanClose: Boolean);
begin
  if (ECTabCtrl1.UpdateCount = 0) then
  begin
    CanClose := true;
    if AIndex >= 0 then
    begin
      if (ECTabCtrl1.TabIndex = AIndex) then
      begin
        ECTabCtrl1.SelectPrevious;
      end;
      CanClose := FecharDocumento((ECTabCtrl1.Tabs[AIndex].Control as TLCSynEdit));
    end;
  end;
end;

procedure TFrmMain.ECTabCtrl1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TabIndex : Integer;
begin
  if (Button = mbRight) then
  begin
   //select on right click
   TabIndex := ECTabCtrl1.IndexOfTabAt(x, y);
   if TabIndex >= 0 then
   begin
     ECTabCtrl1.ActivateTab(TabIndex);
   end;
  end;
end;

procedure TFrmMain.FindDialog1Find(Sender: TObject);
begin
  DoFindText(Sender);
end;

procedure TFrmMain.FormActivate(Sender: TObject);
begin
  if (GetEditorAtivo <> nil) then
  begin
    //if fSettings.ShowExplorer = true then
    //begin
    //  MostrarArquivosAbertos;
    //end;

    GetEditorAtivo.BringToFront;
    GetEditorAtivo.SetFocus;
  end;
end;


procedure TFrmMain.actAbrirAccept(Sender: TObject);
begin
  NovoDocumento(actAbrir.Dialog.FileName);
  GetEditorAtivo.SetFocus;

  fSettings.AddMruFiles(actAbrir.Dialog.FileName, GetEditorAtivo);
  CriarMenuMRU;
end;

procedure TFrmMain.actAbrirBeforeExecute(Sender : TObject);
var
   editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    actAbrir.Dialog.InitialDir := ExtractFileDir(editor.FileName);
  end;
end;

procedure TFrmMain.actAbrirTodosRecentesExecute(Sender: TObject);
var
  i : Integer;
begin
  For i:=0 to fSettings.MruFiles.Count - 1 do
  begin
    NovoDocumento(fSettings.getMruFile(i).GetPathName);

    fSettings.getMruFile(i).SetToSynEdit(GetEditorAtivo);
  end;
  if GetEditorAtivo <> nil then
  begin
    GetEditorAtivo.SetFocus;
  end;
end;

procedure TFrmMain.actAutoCompletarExecute(Sender: TObject);
Var
  aFileName:String;
begin
  FAutoCompletar := TFAutoCompletar.Create(Application);
  Try
    aFileName := ExtractFilePath(Application.ExeName) + FILEAUTOCOMPLETELIST;
    FAutoCompletar.SetHighlighter(FSynLsp);
    if FAutoCompletar.ShowModal = MROK then
    begin
      if FileExistsUTF8(aFileName) then
      begin
        fSynAuC.AutoCompleteList.LoadFromFile(aFileName);
      end;
    end;
  Finally
    FAutoCompletar.Release;
  End;
end;

procedure TFrmMain.actChecarSintaxeExecute(Sender : TObject);
var
  sFileName,
  sMsg: String;
  oMsg: TLCMessages;
  start: TDateTime;
  editor: TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    start := now;

    oMsg := nil;
    sFileName := ExtractFileNameOnly(editor.FileName);
    vstMessages.Tag := ECTabCtrl1.TabIndex;
    vstMessages.RootNodeCount := 0;

    AddMsg('Validar arquivo:',sFileName);
    AddMsg(DateTimeToStr(Now));

    FCompilador.HL := TSynLCHighlighter(editor.Highlighter);
    FCompilador.Lines := editor.Lines;
    FCompilador.Initialize;
    FCompilador.ChecarSintaxe;
    For oMsg in  FCompilador.ListOfMessages do
    begin
      AddMsg(oMsg);
    end;
    ShowMsgAviso(true);

    if oMsg <> nil then
    begin
      editor.LineError := oMsg.Linha;
      editor.CaretY := oMsg.Linha;
      editor.LogicalCaretXY := TPoint.Create(oMsg.Coluna, oMsg.Linha);
      editor.Invalidate;
    end;

    AddMsg(DateTimeToStr(Now));

    sMsg := Format('**** Validação da Sintaxe Concluída em [%s ms] ****',[FormatFloat('#,#00', MilliSecondsBetween(start,Now))]);
    AddMsg(sMsg);

    editor.SetFocus;
  end;
end;

procedure TFrmMain.actCircundarSelecaoExecute(Sender : TObject);
var
  editor: TLCSynEdit;
  NewSelection,
  template:string;
  NewCursor: TPoint;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    template := 'Inicio' + LineEnding +
                '  |<selection>' + LineEnding +
                'Fim;' + LineEnding;

    EncloseTextSelection(template, editor.Lines,
                         editor.BlockBegin, editor.BlockEnd, NewSelection,
                         NewCursor);
    editor.SelText := NewSelection;
    editor.CaretXY := NewCursor;
  end;
end;

procedure TFrmMain.actMostrarComandosExecute(Sender : TObject);
begin
  fSettings.ShowListOfCommands := not fSettings.ShowListOfCommands;

  ShowListOfCommands(fSettings.ShowListOfCommands);
end;

procedure TFrmMain.actComentarSelecaoExecute(Sender: TObject);
var
  editor: TLCSynEdit;
  x,y,
  xi,yi:Integer;
  aTxt: String;
  bEhBloco:Boolean;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    editor.BeginUpdate;
    try
      bEhBloco := not (editor.BlockBegin.y = editor.BlockEnd.y);
      xi := editor.BlockBegin.x;
      yi := editor.BlockBegin.y;
      editor.CaretX := editor.BlockEnd.x;
      editor.CaretY := editor.BlockEnd.y;
      if bEhBloco = true then
      begin
        aTxt := '*****/';
        // Se a selecao Terminar no primeiro caracter da linha, adicionar uma quebra de linha
        if (editor.CaretX = 1) then  // TODO: Identificar se for final da linha e também inserir um quebra de linha
        begin
          aTxt := aTxt + LineEnding;
        end;
      end
      else
      begin
        aTxt := ' --@';
      end;

      editor.InsertTextAtCaret(aTxt);

      x := editor.CaretX;
      y := editor.CaretY;

      editor.CaretX := xi;
      editor.CaretY := yi;

      if bEhBloco = true then
      begin
        aTxt := '/*****';

        // Se a selecao Iniciar no primeiro caracter da linha, adicionar uma quebra de linha
        if (xi = 1) then             // TODO: Identificar se for Inicio da linha (idendacao) e também inserir um quebra de linha
        begin
          aTxt := aTxt + LineEnding;
          Inc(y);
        end;
      end
      else
      begin
        aTxt := '@-- ';
      end;
      editor.InsertTextAtCaret(aTxt);

      editor.CaretX := x;
      editor.CaretY := y;
    finally
      editor.EndUpdate;
    end;
  end;
end;

procedure TFrmMain.actConverterInverterExecute(Sender: TObject);
  function ToggleCase(const aStr : string) : string;
  var
    i : Integer;
    s1, s2 : string;
  begin
    Result := '';
    s1 := LazUTF8.UTF8UpperCase(aStr);
    s2 := LazUTF8.UTF8LowerCase(aStr);
    for i := 1 to Length(aStr) do
    begin
      if aStr[i] = s1[i] then
      begin
        Result := Result + s2[i]
      end
      else
      begin
        Result := Result + s1[i];
      end;
    end;
  end;
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    if editor.SelAvail = false then
    begin
      editor.SelectWord;
    end;

    if editor.SelAvail then
    begin
      editor.SelText := ToggleCase(editor.SelText);
    end;

  end;
end;

procedure TFrmMain.actConverterMaiusculoExecute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    if editor.SelAvail = false then
    begin
      editor.SelectWord;
    end;

    if editor.SelAvail then
    begin
      editor.SelText := LazUTF8.UTF8UpperCase(editor.SelText);
    end;
  end;
end;

procedure TFrmMain.actConverterMinusculoExecute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    if editor.SelAvail = false then
    begin
      editor.SelectWord;
    end;

    if editor.SelAvail then
    begin
      editor.SelText := LazUTF8.UTF8LowerCase(editor.SelText);
    end;
  end;
end;

procedure TFrmMain.actConverterPrimeiraMaiusExecute(Sender: TObject);
var
  editor : TLCSynEdit;
  aTxt:String;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    if editor.SelAvail = false then
    begin
      editor.SelectWord;
    end;

    if editor.SelAvail then
    begin
      aTxt := editor.SelText;
      editor.SelText := LazUTF8.UTF8UpperCase( aTxt[1] ) +
                        LazUTF8.UTF8LowerCase(Copy(aTxt, 2, Length(aTxt)));
    end;
  end;
end;

procedure TFrmMain.actCopiarTudoExecute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    editor.DoCopyToClipboard(TrimRight(editor.Text));
  end;
end;

procedure TFrmMain.actDuplicarTextoExecute(Sender: TObject);
var
  BlockIni,
  BlockFim,
  PosAtual:TPoint;
  editor : TLCSynEdit;
  nLin:Integer;
  aTxt: String;
  SelMod: TSynSelectionMode;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    editor.BeginUpdate;
    try
      // Salvar a configuração inicial da seleção
      BlockIni := editor.BlockBegin;
      BlockFim := editor.BlockEnd;
      PosAtual := editor.CaretXY;

      SelMod := editor.SelectionMode;
      editor.SelectionMode := smLine;

      if editor.SelAvail = true then
      begin
        nLin := editor.BlockEnd.y;
        aTxt := editor.SelText;
        editor.CaretX := editor.BlockEnd.x + 1;
      end
      else
      begin
        nLin := editor.CaretY;
        aTxt := editor.LineText + LineEnding;
        editor.CaretX := Length(aTxt) + 1;
      end;
      editor.CaretY := nLin + 1;
      editor.CaretX := 0;
      editor.InsertTextAtCaret(aTxt,scamEnd);
    finally
      editor.SelectionMode := SelMod;

      // Restaurar a configuração inicial da seleção
      editor.BlockBegin := BlockIni;
      editor.BlockEnd := BlockFim;
      editor.CaretXY := PosAtual;

      editor.EndUpdate;
    end;
  end;
end;

procedure TFrmMain.actFecharAtualExecute(Sender: TObject);
begin
  if (ECTabCtrl1.Tabs.Count > 0) then
  begin
    ECTabCtrl1.BeginUpdate;
    try
      FecharDocumento(GetEditorAtivo);
      ECTabCtrl1.DeleteTab(ECTabCtrl1.TabIndex);
    finally
      ECTabCtrl1.EndUpdate;
    end;
  end;
end;

procedure TFrmMain.actFecharTodosExcetoExecute(Sender: TObject);
begin
  FecharTodasAbas(true);
end;

procedure TFrmMain.actFecharTodosExecute(Sender: TObject);
begin
  FecharTodasAbas(false);
end;

procedure TFrmMain.actIdentarAumentarExecute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    editor.CommandProcessor(ecBlockIndent , #0, nil);
  end;
end;

procedure TFrmMain.actIdentarDiminuirExecute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    editor.CommandProcessor(ecBlockUnindent, #0, nil);
  end;
end;

procedure TFrmMain.actInseriCarEspASCIIExecute(Sender: TObject);
begin
  FCaracteresEspeciais := TFCaracteresEspeciais.Create(Application);
  Try
    FCaracteresEspeciais.TipoRetorno := trChar;
    FCaracteresEspeciais.ShowModal;
  Finally
    FCaracteresEspeciais.Release;
  End;
end;

procedure TFrmMain.actInseriCarEspHTMLExecute(Sender: TObject);
begin
  FCaracteresEspeciais := TFCaracteresEspeciais.Create(Application);
  Try
    FCaracteresEspeciais.TipoRetorno := trHTML;
    FCaracteresEspeciais.ShowModal;
  Finally
    FCaracteresEspeciais.Release;
  end;
end;

procedure TFrmMain.actInserirDataCompletaExecute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    editor.SelText:=FormatDateTime('dddddd', Now);
  end;
end;

procedure TFrmMain.actInserirDataExecute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    editor.SelText:=FormatDateTime('ddddd', Date);
  end;
end;

procedure TFrmMain.actInserirDataHoraExecute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    editor.SelText:=FormatDateTime('ddddd tt', Now);
  end;
end;

procedure TFrmMain.actInserirDirArqAtualExecute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    editor.SelText:=ExtractFilePath(editor.FileName);
  end;
end;

procedure TFrmMain.actInserirHoraExecute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    editor.SelText:=FormatDateTime('tt', Time);
  end;
end;

procedure TFrmMain.actInserirNmArqAtualExecute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    editor.SelText:= ExtractFileName(editor.FileName);
  end;
end;

procedure TFrmMain.actIrPara0Execute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    editor.GotoBookMark((Sender as TAction).Tag);
  end;
end;

procedure TFrmMain.actIrPara0Update(Sender: TObject);
var
  bOk: boolean;
  editor : TLCSynEdit;
begin
  bOk := false;
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    bOk := editor.IsBookmark((Sender as TAction).Tag);
  end;

  (Sender as TAction).Enabled := bOk;
end;

procedure TFrmMain.actIrParaLinhaExecute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    frmIrParaLinha := TfrmIrParaLinha.Create(Application);
    frmIrParaLinha.PrimeiraLinha := 1;
    frmIrParaLinha.UltimaLinha:=editor.Lines.Count;
    frmIrParaLinha.ShowModal;
    if frmIrParaLinha.Linha > 0 then
    begin
      editor.CaretY := frmIrParaLinha.Linha;
    end;
    frmIrParaLinha.Free;
  end;
end;

procedure TFrmMain.actLimparRecentesExecute(Sender: TObject);
begin
  fSettings.MruFiles.Clear;
  CriarMenuMRU;
end;

procedure TFrmMain.actLimparTodosExecute(Sender: TObject);
var
  i: integer;
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    for i := Pred(editor.Marks.Count) downto 0 do  // Necessario para nao ocorrer list out of bound
    begin
      editor.ClearBookMark(editor.Marks[i].BookmarkNumber);
    end;
  end;
end;

procedure TFrmMain.actLimparTodosUpdate(Sender: TObject);
var
  bOk : Boolean;
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  bOk := actLimparTodos.Enabled;
  if (editor <> nil) then
  begin
    bOk := editor.Marks.Count > 0;
  end;
  actLimparTodos.Enabled := bOk;
end;

procedure TFrmMain.actLocalizarExecute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor = nil) then
  begin
    exit;
  end;

  if editor.SelAvail = false then
  begin
    editor.SelectWord;
  end;
  FindDialog1.FindText := editor.SelText;
  FindDialog1.Execute;
end;

procedure TFrmMain.actLocalizarProximoExecute(Sender: TObject);
begin
  DoFindText(Sender);
end;

procedure TFrmMain.actMarcar0Execute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    editor.CommandProcessor(ecToggleMarker0 + (Sender as TAction).Tag, #0, nil);
  end;
end;

procedure TFrmMain.actMarcar0Update(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    (Sender as TAction).Enabled := true;
    (Sender as TAction).Checked := editor.IsBookmark((Sender as TAction).Tag);
  end
  else
  begin
    (Sender as TAction).Enabled := false;
  end;
end;

procedure TFrmMain.actModoColunaExecute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    editor.SelectionMode:= smColumn;
  end;
end;

procedure TFrmMain.actModoLinhaExecute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    editor.SelectionMode:= smLine;
  end;
end;

procedure TFrmMain.actModoNormalExecute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    editor.SelectionMode:= smNormal;
  end;
end;

procedure TFrmMain.actMostrarCaractEspeciaisExecute(Sender: TObject);
var
  i: Integer;
  editor : TLCSynEdit;
begin
  for i := 0 to ECTabCtrl1.Tabs.Count - 1 do
  begin
    editor := (ECTabCtrl1.Tabs[i].Control as TLCSynEdit);
    if (editor <> nil) then
    begin
      if (actMostrarCaractEspeciais.Checked = true) then
      begin
        editor.Options := editor.Options - [eoShowSpecialChars];
      end
      else
      begin
        editor.Options := editor.Options + [eoShowSpecialChars];
      end;
    end;
  end; // For
end;

procedure TFrmMain.actMostrarCaractEspeciaisUpdate(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    actMostrarCaractEspeciais.Checked := (eoShowSpecialChars in editor.Options);
  end;
end;

procedure TFrmMain.actMostrarExplorerExecute(Sender: TObject);
begin
  fSettings.ShowExplorer := not fSettings.ShowExplorer;

  ShowExplorer(fSettings.ShowExplorer);
  if fSettings.ShowExplorer = true then
  begin
    MostrarArquivosAbertos;
  end;
end;

procedure TFrmMain.actParametrosFuncoesExecute(Sender: TObject);
begin
  FParametrosFuncoes := TFParametrosFuncoes.Create(Application);
  Try
    FParametrosFuncoes.SetHighlighter(FSynLsp);
    if FParametrosFuncoes.ShowModal = MROK then
    begin
      CarregarCompletionProposal;
      CarregarListaComandos;
    end;
  Finally
    FParametrosFuncoes.Release;
  End;
end;

procedure TFrmMain.actPreferenciasExecute(Sender: TObject);
var
  i : Integer;
begin
  FSettingsEditor := TFSettingsEditor.Create(Application);
  Try
    if FSettingsEditor.ShowModal = MROK then
    begin
      CarregarConfiguracoes(true);
      for i := 0 to ECTabCtrl1.Tabs.Count - 1 do
      begin
        AtualizarPreferencias((ECTabCtrl1.Tabs[i].Control as TLCSynEdit));
      end;// for
    end;
  Finally
    FSettingsEditor.Release;
  End;
end;

procedure TFrmMain.actProximaTabExecute(Sender: TObject);
begin
  ECTabCtrl1.SelectNext;
end;

procedure TFrmMain.actSalvarComoExecute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    if editor.SaveAs('') = true then
    begin
      ECTabCtrl1.Tabs[ECTabCtrl1.TabIndex].Text := ExtractFileName(editor.FileName);

      Editor.ModuloVetorh := GetModuloByFileName(Editor.FileName);
      Editor.PosicaoRegra := GetPosicaoRegraByFileName(Editor.ModuloVetorh, Editor.FileName);

      if Modulos[Editor.ModuloVetorh].Carregado = false then
      begin
        CarregarModulo(Modulos[Editor.ModuloVetorh]);
      end;
    end;
  end;
end;

procedure TFrmMain.actSalvarExecute(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor <> nil) then
  begin
    if (editor.ReadOnly = false) then
    begin
      CriarBackupArquivo(editor.FileName);
      if editor.Save = true then
      begin
        ECTabCtrl1.Tabs[ECTabCtrl1.TabIndex].Text := ExtractFileName(editor.FileName);

        Editor.ModuloVetorh := GetModuloByFileName(Editor.FileName);
        Editor.PosicaoRegra := GetPosicaoRegraByFileName(Editor.ModuloVetorh, Editor.FileName);

        if Modulos[Editor.ModuloVetorh].Carregado = false then
        begin
          CarregarModulo(Modulos[Editor.ModuloVetorh]);
        end;
      end;
      fSettings.AddMruFiles(editor.FileName, editor);
      CriarMenuMRU;
    end;
  end;
end;
procedure TFrmMain.ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
var
  editor:TLCSynEdit;
begin
  actFecharAtual.Enabled := (ECTabCtrl1.Tabs.Count > 0);
  actFecharTodos.Enabled := (ECTabCtrl1.Tabs.Count > 0);
  actFecharTodosExceto.Enabled := (ECTabCtrl1.Tabs.Count > 0);

  actSalvarComo.Enabled := (ECTabCtrl1.Tabs.Count > 0);
  actCopiarTudo.Enabled := (ECTabCtrl1.Tabs.Count > 0);

  actModoColuna.Enabled := (ECTabCtrl1.Tabs.Count > 0);
  actModoLinha.Enabled := (ECTabCtrl1.Tabs.Count > 0);
  actModoNormal.Enabled := (ECTabCtrl1.Tabs.Count > 0);

  actLocalizar.Enabled := (ECTabCtrl1.Tabs.Count > 0);
  actLocalizarProximo.Enabled := (ECTabCtrl1.Tabs.Count > 0);
  actIrParaLinha.Enabled := (ECTabCtrl1.Tabs.Count > 0);

  actLimparRecentes.Enabled := (fSettings.MruFiles.Count > 0);
  actAbrirTodosRecentes.Enabled := (fSettings.MruFiles.Count > 0);

  actChecarSintaxe.Enabled := (ECTabCtrl1.Tabs.Count > 0);
  actShowMsg.Enabled := (ECTabCtrl1.Tabs.Count > 0);

  if ECTabCtrl1.Tabs.Count > 0 then
  begin
    editor := GetEditorAtivo;
    actModoColuna.Checked := (editor.SelectionMode = smColumn);
    actModoLinha.Checked := (editor.SelectionMode = smLine);
    actModoNormal.Checked := (editor.SelectionMode = smNormal);

    actIdentarAumentar.Enabled := not editor.ReadOnly;
    actIdentarDiminuir.Enabled := not editor.ReadOnly;

    actConverterMaiusculo.Enabled := not editor.ReadOnly;
    actConverterMinusculo.Enabled := not editor.ReadOnly;
    actConverterInverter.Enabled := not editor.ReadOnly;
    actConverterPrimeiraMaius.Enabled := not editor.ReadOnly;

    actSubstituir.Enabled := not editor.ReadOnly;
    actDuplicarTexto.Enabled := not editor.ReadOnly;
    actCircundarSelecao.Enabled := not editor.ReadOnly;

    actInserirData.Enabled := not editor.ReadOnly;
    actInserirHora.Enabled := not editor.ReadOnly;
    actInserirDataHora.Enabled := not editor.ReadOnly;
    actInserirDataCompleta.Enabled := not editor.ReadOnly;
    actInserirDirArqAtual.Enabled := not editor.ReadOnly;
    actInseriCarEspASCII.Enabled := not editor.ReadOnly;
    actInseriCarEspHTML.Enabled := not editor.ReadOnly;
    actInserirNmArqAtual.Enabled := not editor.ReadOnly;

    actComentarSelecao.Enabled := (not editor.ReadOnly) and (editor.SelAvail = true);
    actSalvar.Enabled := editor.Modified;
    actEditRedo.Enabled := editor.CanRedo;
    actLimparTodos.Enabled := (editor.Marks.Count > 0);
  end
  else
  begin
    actModoColuna.Enabled := false;
    actModoLinha.Enabled := false;
    actModoNormal.Enabled := false;

    actIdentarAumentar.Enabled := false;
    actIdentarDiminuir.Enabled := false;

    actConverterMaiusculo.Enabled := false;
    actConverterMinusculo.Enabled := false;
    actConverterInverter.Enabled := false;
    actConverterPrimeiraMaius.Enabled := false;

    actSubstituir.Enabled := false;
    actDuplicarTexto.Enabled := false;
    actCircundarSelecao.Enabled := false;

    actInserirData.Enabled := false;
    actInserirHora.Enabled := false;
    actInserirDataHora.Enabled := false;
    actInserirDataCompleta.Enabled := false;
    actInserirDirArqAtual.Enabled := false;
    actInseriCarEspASCII.Enabled := false;
    actInseriCarEspHTML.Enabled := false;
    actInserirNmArqAtual.Enabled := false;

    actComentarSelecao.Enabled := false;
    actSalvar.Enabled := false;
    actEditRedo.Enabled := false;
    actLimparTodos.Enabled := false;
  end;

end;

procedure TFrmMain.ECTabCtrl1Change(Sender: TObject);
begin
  if (ECTabCtrl1.UpdateCount = 0) then
  begin
    if (ECTabCtrl1.TabIndex >= 0) then
    begin
      if (ECTabCtrl1.Tabs[ECTabCtrl1.TabIndex].Control <> nil) then
      begin
        ECTabCtrl1.Tabs[ECTabCtrl1.TabIndex].Control.BringToFront;
        GetEditorAtivo.Invalidate;
        GetEditorAtivo.SetFocus;
      end;
    end;
  end;
end;

procedure TFrmMain.SpeedButton1Click(Sender : TObject);
begin
  ShowExplorer(true);
  MostrarArquivosAbertos;
end;

procedure TFrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i: Integer;
  sigla: TLCSiglaModuloVetorh;
begin
  if  (fSettings.AskBeforeExit = True)
  and (Application.MessageBox(PChar(rsQuestionConfirmExitApp),PChar(Application.Title), MB_YESNO or MB_DEFBUTTON2 or MB_ICONQUESTION) = IDNO) then
  begin
    CanClose := false;
    exit;
  end;

  fSettings.SessionFiles.Clear; // Limpar os dados da Sessão anterior
  if (FecharTodasAbas(false, fSettings.SaveSession) = false) then
  begin
    CanClose := false;
    exit;
  end;

  SalvarConfiguracoes;

  if (LookupList <> nil) then
  begin
    LookupList.Clear;
  end;
  FreeAndNil(LookupList);

  FreeAndNil(FCompilador);

  for sigla in TLCSiglaModuloVetorh do
  begin
    LimparModulo(Modulos[sigla]);
  end;

  For i:= 0 to lbxFuncoes.Count - 1 do
  begin
    TDefComando(lbxFuncoes.Items.Objects[i]).Free;
    lbxFuncoes.Items.Objects[i] := nil;
  end;

  For i:= 0 to lbxComandos.Count - 1 do
  begin
    TDefComando(lbxComandos.Items.Objects[i]).Free;
    lbxComandos.Items.Objects[i] := nil;
  end;

  For i:= 0 to lbxVariaveis.Count - 1 do
  begin
    TDefComando(lbxVariaveis.Items.Objects[i]).Free;
    lbxVariaveis.Items.Objects[i] := nil;
  end;

  lbxFuncoes.Items.Clear;
  lbxComandos.Items.Clear;
  lbxVariaveis.Items.Clear;

  fPalavras.Destroy;
  fSettings.Destroy;

  CanClose := true; // Indicar que a aplicacao deverá ser finalizada
end;

procedure TFrmMain.FormCreate(Sender: TObject);
Var
  bAchouAtivo:Boolean;
  i : Integer;
  oSessionFile: TSessionFile;
  oMruFile: TMRUFile;
begin
  Application.Flags := Application.Flags +  [AppNoExceptionMessages];
  Application.AddOnExceptionHandler(@DoExceptionHandler, true);

  // Limpar panels da Barra de Status
  For i := 0 to StatusBar1.Panels.Count - 1 do
  begin
    StatusBar1.Panels[i].Text := '';
  end;

  // Determinar o tamanho de cada nó
  vstExplorer.NodeDataSize := SizeOf(TFileNode);
  // Inicializar a quantidade nós
  vstExplorer.RootNodeCount := 0;

  // Determinar o tamanho de cada nó
  vstMessages.NodeDataSize := SizeOf(TLCMessageNode);
  // Inicializar a quantidade nós
  vstMessages.RootNodeCount := 0;

  FCompilador := TLCCompilador.Create();
  fPalavras := TLCListOfPalavras.Create(True);

  // Iniciarlizar as variáveis
  fSettings := TEditorLspSettings.Create;
  configFileName := ChangeFileExt(ExtractFileName(Application.ExeName), '.json');

  if DirPathExists(fSettings.PathConfig) = false then
  begin
    CreateDir(fSettings.PathConfig);

    if FileExistsUTF8(fSettings.PathAppRoot + 'SynLCLsp.json') = true then
    begin
      RenameFile(fSettings.PathAppRoot + 'SynLCLsp.json', fSettings.PathConfig + 'SynLCLsp.json');
    end;

    if FileExistsUTF8(fSettings.PathAppRoot + configFileName) = true then
    begin
      RenameFile(fSettings.PathAppRoot + configFileName, fSettings.PathConfig + configFileName);
    end;

    if FileExistsUTF8(fSettings.PathAppRoot + FILEAUTOCOMPLETELIST) = true then
    begin
      RenameFile(fSettings.PathAppRoot + FILEAUTOCOMPLETELIST, fSettings.PathConfig + FILEAUTOCOMPLETELIST);
    end;

    if FileExistsUTF8(fSettings.PathAppRoot + FILECOMPLETIONPROPOSAL) = true then
    begin
      RenameFile(fSettings.PathAppRoot + FILECOMPLETIONPROPOSAL, fSettings.PathConfig + FILECOMPLETIONPROPOSAL);
    end;
  end;

  FSynCmp:= TSynLCCompletion.Create(Self);
  fSynAuC:= TSynLCAutoComplete.Create(Self);

  FSynLsp := TSynLCHighlighter.Create(Self);
  FSynLsp.LoadSettingsFromFile(fSettings.PathConfig + 'SynLCLsp.json');

  InicializarListaModulos;

  CarregarConfiguracoes;

  SetDefaultLang(fSettings.getIdiomaI18n);

  Caption := rsEditorName + ' ' + VersionOfApplication;

  if FileExistsUTF8(fSettings.PathConfig + FILEAUTOCOMPLETELIST) then
  begin
    fSynAuC.AutoCompleteList.LoadFromFile(fSettings.PathConfig + FILEAUTOCOMPLETELIST);
  end;

  CarregarCompletionProposal;
  CarregarListaComandos;

  CarregarPalavras;

  ShowMsgAviso(false);
  ShowListOfCommands(fSettings.ShowListOfCommands);
  ShowExplorer(fSettings.ShowExplorer);

  if not (csDesigning in ComponentState) then
  begin
    Application.AddOnIdleHandler(@DoIdle, true);
  end;

  if fSettings.SessionFiles.Count > 0 then
  begin
    ECTabCtrl1.BeginUpdate;
    try
      bAchouAtivo := false;
      For i:= 0 to fSettings.SessionFiles.Count - 1 do
      begin
        oSessionFile:= TSessionFile(fSettings.SessionFiles.Items[i]);
        if (FileExistsUTF8(oSessionFile.FileName) = true) then
        begin
          if oSessionFile.Active = true then
          begin
            bAchouAtivo := true;
          end;
          NovoDocumento(oSessionFile.FileName, oSessionFile.Active);
          oMruFile := fSettings.getMruFile(oSessionFile.FileName);
          if (oMruFile <> nil) then
          begin
            oMruFile.SetToSynEdit(ECTabCtrl1.Tabs[ECTabCtrl1.Tabs.Count -1 ].Control as TLCSynEdit);
          end;
        end;
      end;
      if bAchouAtivo = false then
      begin
        ECTabCtrl1.MakeTabAvailable(0, true);
      end;
    finally
      ECTabCtrl1.EndUpdate;
    end;

    if fSettings.ShowExplorer = true then
    begin
      MostrarArquivosAbertos;
    end;
  end;

  if ECTabCtrl1.Tabs.Count = 0 then
  begin
    if fSettings.CreateEmptyFile = true then
    begin
      NovoDocumento('');
    end;
  end;
end;

procedure TFrmMain.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  editor: TLCSynEdit;
  i: Integer;
begin
  ECTabCtrl1.BeginUpdate;
  try
    For i:= 0 to High(FileNames) do
    begin
      NovoDocumento(FileNames[i]);
    end;

    editor := GetEditorAtivo;
    if (editor <> nil) then
    begin
      fSettings.AddMruFiles(FileNames[0], editor);
      CriarMenuMRU;
    end;
  finally
    ECTabCtrl1.EndUpdate;
  end;
end;

procedure TFrmMain.IdleTimer1Timer(Sender : TObject);
begin
  DoAutoSave;
end;

procedure TFrmMain.lbxFuncoesDblClick(Sender : TObject);
var
  dc : TDefComando;
  editor: TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if  editor <> nil then
  begin
    if ((Sender as TListBox).ItemIndex >= 0) then
    begin
      dc := (Sender as TListBox).Items.Objects[(Sender as TListBox).ItemIndex] as TDefComando;
      if dc <> nil then
      begin
        if dc.Sintaxe = '' then
        begin
          editor.SelText := dc.Nome
        end
        else
        begin
          editor.SelText := dc.Sintaxe;
        end;
        editor.SetFocus;
      end;
    end;
  end;
end;

procedure TFrmMain.lbxFuncoesSelectionChange(Sender : TObject; User : boolean);
var
  dc : TDefComando;
begin
  if ((Sender as TListBox).ItemIndex >= 0) then
  begin
    dc := (Sender as TListBox).Items.Objects[(Sender as TListBox).ItemIndex] as TDefComando;
    if dc <> nil then
    begin
      lbSintaxe.Caption := '';
      lbDescricao.Caption := '';
      if dc.Sintaxe <> '' then
      begin
        lbSintaxe.Caption := dc.Sintaxe;
      end;
      if dc.Descricao <> '' then
      begin
        lbDescricao.Caption := dc.Descricao;
      end;
      lbSintaxe.Hint := lbSintaxe.Caption;
      lbDescricao.Hint := lbDescricao.Caption;
    end;
  end;
end;

procedure TFrmMain.MenuItem29Click(Sender: TObject);
begin
//  raise Exception.Create('Test error');
  FSobre := TFSobre.Create(Application);
  Try
    FSobre.ShowModal;
  Finally
    FSobre.Release;
  End;
end;

procedure TFrmMain.MenuItemMRUClick(Sender: TObject);
var
  i : Integer;
begin
  if (Sender is TMenuItem) then
  begin
    i := (Sender as TMenuItem).Tag;
    if fSettings.MruFiles.Count > (i - 1) then
    begin
      NovoDocumento(fSettings.getMruFile(i-1).GetPathName);

      fSettings.getMruFile(i-1).SetToSynEdit(GetEditorAtivo);
    end;
  end;
end;

procedure TFrmMain.ReplaceDialog1Find(Sender: TObject);
begin
  DoFindText(Sender);
end;

procedure TFrmMain.ReplaceDialog1Replace(Sender: TObject);
begin
  DoReplaceText(Sender);
end;

procedure TFrmMain.StatusBar1DrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  i:Integer;
   X, Y,
  ImageWidth: Integer;
begin
  i := -1;
  if (Panel.Index =  iPnlModoSel) then
  begin
    i := 4; // Normal
    if (Panel.Text = 'C') then
    begin
      i := 2; // Coluna
    end
    else
    if (Panel.Text = 'L') then
    begin
      i := 3; // Linha
    end;
  end
  else
  if (Panel.Index =  iPnlReadOnly) then
  begin
    i := 0; // Normal
    if (Panel.Text = 'R') then
    begin
      i := 1; // Somente Leitura
    end;
  end;

  if (i >= 0) then
  begin
    ImageWidth := ImageListStatusBar.Width;
   // Draws image
    if ImageWidth > 0 then
    begin
      X := Rect.Left + ((Rect.Right - Rect.Left) - ImageWidth) div 2;
      Y := Rect.Top + ((Rect.Bottom - Rect.Top) - ImageListStatusBar.Height) div 2;
      ImageListStatusBar.Draw(StatusBar.Canvas, X, Y, i);
    end;
  end;
end;

procedure TFrmMain.StatusBar1Hint(Sender: TObject);
begin
  StatusBar1.Panels[iPnlMessage].Text := Application.Hint;
end;

procedure TFrmMain.MostrarArquivosAbertos;
var
  XNode: PVirtualNode;
  Data: PFileNode;
begin
  XNode := nil;
   repeat
     if XNode = nil then
     begin
       XNode := vstExplorer.GetFirst
     end
     else
     begin
       XNode := vstExplorer.GetNextVisible(XNode, true);
     end;

     Data := vstExplorer.GetNodeData(XNode);
     if Data^.isFolder = True then
     begin
       if  (vstExplorer.Expanded[XNode] = false)
       and (DirectoryIsOpen(Data^.FullPath) = true)
       and (vstExplorer.ChildCount[XNode] = 0) then
       begin
         vstExplorer.Expanded[XNode] := true;
       end;
     end;
   until XNode = vstExplorer.GetLast();
end;

procedure TFrmMain.vstExplorerCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
// The node comparison routine is the heart of the tree sort. Here we have to tell the caller which node we consider
// being "larger" or "smaller".
var
  Data1,
  Data2: PFileNode;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  // Folder are always before files. Check if *both* are folders or *both* are non-folders, but not different.
  if Data1^.IsFolder <> Data2^.IsFolder then
  begin
    // One of both is a folder the other is a file.
    if (Data1^.IsFolder = true) then
    begin
      Result := -1
    end
    else
    begin
      Result := 1;
    end;
  end
  else
  begin
    // Both are of same type (folder or file). Just compare captions.
    // Note that we use ANSI comparison, while the strings are Unicode. Since this will implicitely convert the captions
    // to ANSI for comparation it might happen that the sort order is wrong for names which contain text in a language
    // other than the current OS language. A full blown Unicode comparison is beyond the scope of this demo.
    Result := CompareText(Data1^.FullPath + Data1^.Name , Data2^.FullPath + Data2^.Name);
  end;
end;

procedure TFrmMain.vstExplorerFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PFileNode;
begin
  Data := Sender.GetNodeData(Node);
  // Explicitely free the string, the VCL cannot know that there is one but needs to free
  // it nonetheless. For more fields in such a record which must be freed use Finalize(Data^) instead touching
  // every member individually.
  Finalize(Data^);
end;

procedure TFrmMain.vstExplorerGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
var
  Data: PFileNode;
begin
  Data := Sender.GetNodeData(Node);
  HintText := Data^.Name;
end;

procedure TFrmMain.vstExplorerGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PFileNode;
begin
  Data := Sender.GetNodeData(Node);
  if Sender.Expanded[Node] then
  begin
    ImageIndex := Data^.OpenIndex
  end
  else
  begin
    ImageIndex := Data^.CloseIndex;
  end;
end;

procedure TFrmMain.vstExplorerGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PFileNode;
begin
  Data := Sender.GetNodeData(Node);
  CellText := Data^.Name;
end;

procedure TFrmMain.vstExplorerInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  Data : PFileNode;
begin
  Data := Sender.GetNodeData(Node);
  LerDiretorio(Sender, Node, fSettings.Filters, Data^.FullPath);
  ChildCount := Sender.ChildCount[Node];
end;

procedure TFrmMain.vstExplorerInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data : PFileNode;
begin
  Data := Sender.GetNodeData(Node);
  if Data^.isFolder = true then
  begin
    Include(InitialStates, ivsHasChildren);
  end;
end;

procedure TFrmMain.vstExplorerNodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  Data : PFileNode;
  i : Integer;
begin
  Data := Sender.GetNodeData(Sender.FocusedNode);
  if Data = nil then
  begin
    exit;
  end;

  if Data^.isFolder = False then
  begin
    NovoDocumento(IncludeTrailingPathDelimiter(Data^.FullPath) + Data^.Name ,true);

    For i:=0 to fSettings.MruFiles.Count - 1 do
    begin
      if AnsiUpperCase(fSettings.getMruFile(i).GetPathName) = AnsiUpperCase(IncludeTrailingPathDelimiter(Data^.FullPath) + Data^.Name) then
      begin
        fSettings.getMruFile(i).SetToSynEdit(GetEditorAtivo);
      end;
    end;

    fSettings.AddMruFiles(IncludeTrailingPathDelimiter(Data^.FullPath) + Data^.Name, GetEditorAtivo);
    CriarMenuMRU;
  end;
end;

procedure TFrmMain.vstExplorerPaintText(Sender : TBaseVirtualTree; const TargetCanvas : TCanvas; Node : PVirtualNode;
  Column : TColumnIndex; TextType : TVSTTextType);
var
  Data : PFileNode;
begin
  Data := Sender.GetNodeData(Node);
  if Data = nil then
  begin
    exit;
  end;

  TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsBold];
  TargetCanvas.Font.Color := clDefault;
  if  ((DirectoryIsOpen(Data^.FullPath) = true) and (Data^.isFolder = True))
  or  (FileIsOpen(IncludeTrailingPathDelimiter(Data^.FullPath) + Data^.Name, false)) = true then
  begin
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
    TargetCanvas.Font.Color := clBlue;
  end;
end;

procedure TFrmMain.vstMessagesFreeNode(Sender : TBaseVirtualTree; Node : PVirtualNode);
var
  Data: PLCMessageNode;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TFrmMain.vstMessagesGetImageIndex(Sender : TBaseVirtualTree; Node : PVirtualNode; Kind : TVTImageKind;
  Column : TColumnIndex; var Ghosted : Boolean; var ImageIndex : Integer);
var
  oMsgNode: PLCMessageNode;
begin
  oMsgNode := Sender.GetNodeData(Node);
  ImageIndex := oMsgNode^.ImageIndex;
end;

procedure TFrmMain.vstMessagesGetText(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex;
  TextType : TVSTTextType; var CellText : String);
var
  oMsgNode: PLCMessageNode;
begin
  oMsgNode := Sender.GetNodeData(Node);
  CellText := '';
  if oMsgNode^.Tipo <> lcTMUnknown then
  begin
    if TextType = ttNormal then
    begin
      CellText := Format('%s (lin %s col %s) - %s',[UTF8UpperCase(LCDescriptionTypeOfMessages[oMsgNode^.Tipo]),
                                                    FormatFloat('#,#00', oMsgNode^.Linha),
                                                    FormatFloat('#,#00', oMsgNode^.Coluna),
                                                    oMsgNode^.Texto]);
    end;
  end
  else
  begin
    Case TextType of
      ttNormal: CellText := oMsgNode^.Texto;
      ttStatic: CellText := oMsgNode^.TextoEstatico;
    end;
  end;
end;

procedure TFrmMain.vstMessagesNodeClick(Sender : TBaseVirtualTree; const HitInfo : THitInfo);
var
  oMsgNode: PLCMessageNode;
  editor: TLCSynEdit;
begin
  oMsgNode := Sender.GetNodeData(HitInfo.HitNode);

  if  (oMsgNode <> nil)
  and (oMsgNode^.Linha <> -1) then
  begin
    ECTabCtrl1.TabIndex := Sender.Tag;
    editor := GetEditorAtivo;
    if editor <> nil then
    begin
      editor.LineError := oMsgNode^.Linha;
      editor.CaretY := oMsgNode^.Linha;
      editor.LogicalCaretXY := TPoint.Create(oMsgNode^.Coluna, oMsgNode^.Linha);
      editor.SetFocus;
      editor.Invalidate;
    end;
  end;
end;

procedure TFrmMain.vstMessagesPaintText(Sender : TBaseVirtualTree; const TargetCanvas : TCanvas; Node : PVirtualNode;
  Column : TColumnIndex; TextType : TVSTTextType);
//var
//  oMsgNode: PLCMessageNode;
begin
  //oMsgNode := Sender.GetNodeData(Node);
  case TextType of
    //ttNormal:
      //if oMsgNode^.Tipo <> lcTMUnknown then
      //begin
      //  TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
      //end
      //else
      //begin
        //TargetCanvas.Font.Style := TargetCanvas.Font.Style - [fsBold];
      //end;
    ttStatic:
      TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
  end;
end;

procedure TFrmMain.InicializarListaModulos;
var
  sigla: TLCSiglaModuloVetorh;
  modulo: TLCModuloVetorh;
begin
  for sigla in TLCSiglaModuloVetorh do
  begin
    modulo.Id := Sigla;
    modulo.Carregado := false;
    modulo.ArquivosExcluidos := TStringList.Create;

    modulo.PastaBase := '';
    SetLength(modulo.regras, 0);

    Modulos[sigla] := modulo;
  end;
end;

function TFrmMain.AdicionarRegraAoModulo(var aModulo: TLCModuloVetorh;
  const sFile: String): integer;
var
  Regra: TLCArquivoRegra;
begin
  Regra.NomeArquivo := sFile;
  Regra.Funcoes := TLCListOfFunction.Create;

  if FileExistsUTF8(sFile) = true then
  begin
    FCompilador.Lines.LoadFromFile(sFile);
  end;
  FCompilador.CompilarListaFuncoes(Regra.Funcoes);

  Result := High(aModulo.Regras) + 1;
  SetLength(aModulo.Regras, Result + 1);
  aModulo.Regras[Result] := Regra;
end;

procedure TFrmMain.LimparModulo(var aModulo : TLCModuloVetorh);
var
  i:Integer;
begin
  FreeAndNil(aModulo.ArquivosExcluidos);

  For i:=0 to High(aModulo.Regras) do
  begin
    FreeAndNil(aModulo.Regras[i].Funcoes);
  end;
  aModulo.Carregado := false;
end;

procedure TFrmMain.CarregarModulo(var aModulo : TLCModuloVetorh);
var
  sFile,
  sPath:String;

  bFileReadable:Boolean;
  I: Integer;
  SearchRec: TSearchRec;
begin
  sPath := AppendPathDelim(aModulo.PastaBase);
  aModulo.Carregado := true;

  I := FindFirst(sPath + '*.txt', 0, SearchRec);
  while I = 0 do
  begin
    sFile := sPath + SearchRec.Name;

    if aModulo.ArquivosExcluidos.IndexOf(LowerCase(sFile)) < 0 then
    begin
      if  (FileIsText(sFile, bFileReadable) = true)
      and (bFileReadable = true) then
      begin
        AdicionarRegraAoModulo(aModulo, sFile);
      end;
    end;
    I := FindNext(SearchRec);
  end;
end;

procedure TFrmMain.LerDiretorio(Sender: TBaseVirtualTree; Node: PVirtualNode;
  filtro: TStrings; diretorioBase: string);

  Function Coincide(txt: string): boolean;
  var
    filt_n   : string;
  begin
    for filt_n in filtro do
    begin
      if MatchesMask(txt,filt_n) then
      begin
        Exit(true);
      end;
    end;
    Exit(false);
  end;

  procedure GetOpenAndClosedIcons(aFileName: string; var Open, Closed: Integer);
  var
    sExt:string;
  begin
    sExt := AnsiLowerCase(ExtractFileExt(aFileName));

    if sExt = '.txt' then
    begin
      Open := 1;
      Closed := 1;
    end
    else
    if sExt = '.sql' then
    begin
      Open := 5;
      Closed := 5;
    end
    else
    if (sExt = '.exe')
    or (sExt = '.bat') then
    begin
      Open := 7;
      Closed := 7;
    end
    else
    if (sExt = '.xls')
    or (sExt = '.xlsx') then
    begin
      Open := 9;
      Closed := 9;
    end
    else
    if (sExt = '.doc')
    or (sExt = '.docx') then
    begin
      Open := 10;
      Closed := 10;
    end
    else
    if (sExt = '.bmp')
    or (sExt = '.jpg')
    or (sExt = '.gif')
    or (sExt = '.png') then
    begin
      Open := 11;
      Closed := 11;
    end
    else
    if sExt = '.pas' then
    begin
      Open := 12;
      Closed := 12;
    end
    else
    begin
      Open := 6;
      Closed := 6;
    end;
  end;

var
  SearchRec: TSearchRec;
  ChildData: PFileNode;
  nomArc   : string;
  ChildNode: PVirtualNode;
  directorio: String;
begin
  directorio := UTF8ToSys(IncludeTrailingPathDelimiter(diretorioBase));

  Sender.BeginUpdate;
  Screen.Cursor := crHourGlass;
  try
    if FindFirst(directorio + '*.*', faDirectory, SearchRec) = 0 then
    begin
      repeat
        nomArc := SysToUTF8(directorio + SearchRec.Name);
        if  (SearchRec.Name <> '.')
        and (SearchRec.Name <> '..') then
        begin
          if (SearchRec.Attr and faDirectory = faDirectory)
          or (coincide(SearchRec.Name) = true) then
          begin
            ChildNode := Sender.AddChild(Node);
            ChildData := Sender.GetNodeData(ChildNode);

            ChildData^.Name := ExtractFileName(nomArc);
            ChildData^.IsFolder := SearchRec.Attr and faDirectory = faDirectory;
            if ChildData^.IsFolder = true then
            begin
              ChildData^.FullPath := nomArc;
              ChildData^.OpenIndex  := 13;
              ChildData^.CloseIndex := 0;
            end
            else
            begin
              ChildData^.FullPath := ExtractFileDir(nomArc);
              GetOpenAndClosedIcons(ChildData^.Name, ChildData^.OpenIndex, ChildData^.CloseIndex);
            end;
            Sender.ValidateNode(Node, False);
          end;
        end;
      until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    end;
  finally
    Finalize(SearchRec);

    Screen.Cursor := crDefault;
    Sender.EndUpdate;
  end;
end;

procedure TFrmMain.DoIdle(Sender: TObject; var Done: Boolean);
begin
  if GetKeyState(VK_CAPITAL) = 1 then
  begin
    StatusBar1.Panels[iPnlCapital].Text := 'CAPS'
  end
  else
  begin
    StatusBar1.Panels[iPnlCapital].Text := '';
  end;

  if GetKeyState(VK_NUMLOCK) = 1 then
  begin
    StatusBar1.Panels[iPnlNumLock].Text := 'NUM'
  end
  else
  begin
    StatusBar1.Panels[iPnlNumLock].Text := '';
  end;

  if GetKeyState(VK_SCROLL) = 1 then
  begin
    StatusBar1.Panels[iPnlScroll].Text := 'SCRL'
  end
  else
  begin
    StatusBar1.Panels[iPnlScroll].Text := '';
  end;

  DoAutoSave;

  Done := true;
end;

procedure TFrmMain.DoReplaceText(Sender: TObject);
var
  nRet: integer;
  editor: TLCSynEdit;
  rOptions: TSynSearchOptions;
  sSearch: string;
begin
  editor := GetEditorAtivo;
  if (editor = nil) then
  begin
    exit;
  end;

  sSearch := ReplaceDialog1.FindText;
  if Length(sSearch) = 0 then
  begin
    Beep;
    ShowMessage('Nenhum Texto para localizar!');
  end
  else
  begin
    rOptions := [ssoReplace];
    if frMatchCase in ReplaceDialog1.Options then
    begin
      Include(rOptions, ssoMatchCase);
    end;

    if frWholeWord in ReplaceDialog1.Options then
    begin
      Include(rOptions, ssoWholeWord);
    end;

    if frReplaceAll in ReplaceDialog1.Options then
    begin
      Include(rOptions, ssoReplaceAll);
    end;

    if frEntireScope in ReplaceDialog1.Options then
    begin
      Include(rOptions, ssoEntireScope);
    end;

    if editor.SearchReplace(sSearch, ReplaceDialog1.ReplaceText, rOptions) = 0 then
    begin
      if (frEntireScope in ReplaceDialog1.Options) then
      begin
        Beep;
        ShowMessage(Format(rsWarningTextNotFound,[sSearch]));
      end
      else
      begin
        nRet := Application.MessageBox(PChar(rsQuestionSearchBeginFile),
                                   PChar(Application.Title),
                                   MB_YESNO or MB_DEFBUTTON2 or MB_ICONQUESTION);
        if (nRet = IDYES) then
        begin
          ReplaceDialog1.Options :=  ReplaceDialog1.Options + [frEntireScope];
          DoReplaceText(Sender);
        end;

        //ReplaceDialog1.CloseDialog;
        ReplaceDialog1.Options := ReplaceDialog1.Options - [frEntireScope];

        //editor.SetFocus();
      end;
    end
    else
    begin
      //ReplaceDialog1.CloseDialog;
      ReplaceDialog1.Options := ReplaceDialog1.Options - [frEntireScope];

      //editor.SetFocus();
    end;
  end;
end;

procedure TFrmMain.DoFindText(Sender: TObject);
var
  nRet:integer;
  editor: TLCSynEdit;
  rOptions: TSynSearchOptions;
  dlg: TFindDialog;
  sSearch: string;
begin
  editor := GetEditorAtivo;
  if (editor = nil) then
  begin
    exit;
  end;

  if Sender = ReplaceDialog1 then
  begin
    dlg := ReplaceDialog1
  end
  else
  begin
    dlg := FindDialog1;
  end;

  sSearch := dlg.FindText;

  if Length(sSearch) = 0 then
  begin
    dlg.Execute;
  end
  else
  begin
    rOptions := [];
    if not (frDown in dlg.Options) then
    begin
      Include(rOptions, ssoBackwards);
    end;

    if frMatchCase in dlg.Options then
    begin
      Include(rOptions, ssoMatchCase);
    end;

    if frWholeWord in dlg.Options then
    begin
      Include(rOptions, ssoWholeWord);
    end;

    if frEntireScope in dlg.Options then
    begin
      Include(rOptions, ssoEntireScope);
    end;

    if editor.SearchReplace(sSearch, '', rOptions) = 0 then
    begin
      if (frEntireScope in dlg.Options) then
      begin
        Beep;
        ShowMessage(Format(rsWarningTextNotFound,[sSearch]));
      end
      else
      begin
        nRet := Application.MessageBox(PChar(rsQuestionSearchBeginFile),
                                   PChar(Application.Title),
                                   MB_YESNO or MB_DEFBUTTON2 or MB_ICONQUESTION);
        if (nRet = IDYES) then
        begin
          dlg.Options :=  dlg.Options + [frEntireScope];
          DoFindText(Sender);
        end;

        dlg.CloseDialog;
        dlg.Options := dlg.Options - [frEntireScope];

        editor.SetFocus();
      end;
    end
    else
    begin
      dlg.CloseDialog;
      dlg.Options := dlg.Options - [frEntireScope];
      editor.SetFocus();
    end;

  end;
end;

procedure TFrmMain.DoAutoSave;
var
  i : Integer;
begin
  if fSettings.EnabledAutoSaveFile = true then
  begin
    For i:=0 to ECTabCtrl1.Tabs.Count - 1 do
    begin
      With (ECTabCtrl1.Tabs[i].Control as TLCSynEdit) do
      begin
        if  ((IsNew = false)
        and  (Modified = true)) then
        begin
          if (MinutesBetween(Now, LastTimeSaved) >= fSettings.IntervalAutoSaveFile) then
          begin
            Save;
          end;
        end;
      end;
    end;
  end;
end;

procedure TFrmMain.DoExceptionHandler(Sender : TObject; E : Exception);
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  Report := 'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
  begin
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  end;
  ShowMessage(Report);
end;

procedure TFrmMain.DoSpecialLineMarkup(Sender : TObject; Line : integer; var Special : boolean;
  Markup : TSynSelectedColor);
begin
  if Sender is TLCSynEdit then
  begin
    if (Sender as TLCSynEdit).LineError = Line then
    begin
      Special := True ;
      Markup.Assign(FrmMain.fSettings.Editor.LineErrorColor);
    end;
  end;
end;

procedure TFrmMain.DoExecuteCompletion(Sender: TObject);
begin
  DoAddCompletion;
end;

procedure TFrmMain.DoSearchPosition(var APosition: integer);
begin
  DoAddCompletion;

  //if SynCompletion1.ItemListCount > 0 then
  if fSynCmp.ItemList.Count > 0 then
  begin
    APosition := 0;
  end
  else
  begin
    APosition := -1;
  end
end;

procedure TFrmMain.DoAddCompletion;
  function GetPreviousToken(FEditor: TCustomSynEdit): string;
  var
    s: Ansistring;
    i: integer;
  begin
    if FEditor <> nil then
    begin
      s := FEditor.LineText;
      i := FEditor.LogicalCaretXY.X - 1;
      if i > length(s) then
      begin
        result := '';
      end
      else
      begin
        // Clear the spaces after the token
        while (i > 0)
        and   ((s[i] = ' ') or (pos(s[i], fSynCmp.EndOfTokenChr) <> 0)) do
        begin
          dec(i);
        end;

        while (i > 0) and (s[i] > ' ')
        and   (pos(s[i], fSynCmp.EndOfTokenChr) = 0) do
        Begin
          dec(i);
        end;
        result := copy(s, i + 1, FEditor.LogicalCaretXY.X - i - 1);
      end;
    end
    else
    begin
      result := '';
    end;
  end;

var
  nPosIni,
  i:Integer;
  bAchou:Boolean;
  aLastToken:String;
  Palavra: TLCPalavra;
begin
  fSynCmp.Clear;
  aLastToken := lowerCase(Trim(fSynCmp.CurrentString));
  if aLastToken.IsEmpty then
  begin
    aLastToken := lowerCase(Trim(GetPreviousToken(GetEditorAtivo)));
  end;

  if aLastToken.IsEmpty then
  begin
    exit;
  end;

  if (aLastToken = 'definir') then
  begin
    fSynCmp.tag := 1;
    for i := 0 to Pred(FSynLsp.Settings.SettingsToDataType.Words.Count) do
    begin
      fSynCmp.AddItem(FSynLsp.Settings.SettingsToDataType.Words[i],'' , clBlack);
    end;
  end
  else
  if (Copy(aLastToken, 1, 4) = 'cur_') then
  begin
    fSynCmp.tag := 2;
    fSynCmp.AddItem('AbrirCursor', '' , clBlack, 'AbrirCursor();');
    fSynCmp.AddItem('Achou', '' , clBlack, 'Achou');
    fSynCmp.AddItem('FecharCursor', '' , clBlack, 'FecharCursor();');
    fSynCmp.AddItem('NaoAchou', '' , clBlack, 'NaoAchou');
    fSynCmp.AddItem('Proximo', '' , clBlack, 'Proximo();');
    fSynCmp.AddItem('SQL', '' , clBlack, 'SQL "";');
    fSynCmp.AddItem('UsaAbrangencia', '' , clBlack, 'UsaAbrangencia(1);');
  end
  else
  if (Copy(aLastToken, 1, 4) = 'lst_') then
  begin
    fSynCmp.tag := 3;
    fSynCmp.AddItem('Adicionar', '' , clBlack, 'Adicionar();');
    fSynCmp.AddItem('AdicionarCampo', '' , clBlack, 'AdicionarCampo(');
    fSynCmp.AddItem('Anterior', '' , clBlack, 'Anterior();');
    fSynCmp.AddItem('Cancelar', '' , clBlack, 'Cancelar();');
    fSynCmp.AddItem('Chave', '' , clBlack, 'Chave("");');
    fSynCmp.AddItem('DefinirCampos', '' , clBlack, 'DefinirCampos();');
    fSynCmp.AddItem('Editar', '' , clBlack, 'Editar();');
    fSynCmp.AddItem('EditarChave', '' , clBlack, 'EditarChave();');
    fSynCmp.AddItem('EfetivarCampos', '' , clBlack, 'EfetivarCampos();');
    fSynCmp.AddItem('Excluir', '' , clBlack, 'Excluir();');
    fSynCmp.AddItem('FDA', '' , clBlack, 'FDA');
    fSynCmp.AddItem('Gravar', '' , clBlack, 'Gravar();');
    fSynCmp.AddItem('IDA', '' , clBlack, 'IDA');
    fSynCmp.AddItem('Inserir', '' , clBlack, 'Inserir();');
    fSynCmp.AddItem('Limpar', '' , clBlack, 'Limpar();');
    fSynCmp.AddItem('NumReg', '' , clBlack, 'NumReg();');
    fSynCmp.AddItem('Primeiro', '' , clBlack, 'Primeiro();');
    fSynCmp.AddItem('Proximo', '' , clBlack, 'Proximo();');
    fSynCmp.AddItem('QtdRegistros', '' , clBlack, 'QtdRegistros;');
    fSynCmp.AddItem('SetaNumReg', '' , clBlack, 'SetaNumReg();');
    fSynCmp.AddItem('SetarChave', '' , clBlack, 'SetarChave();');
    fSynCmp.AddItem('Ultimo', '' , clBlack,  'Ultimo();');
    fSynCmp.AddItem('VaiParaChave', '' , clBlack, 'VaiParaChave()');
  end
  else
  begin
    fSynCmp.tag := 0;
    bAchou := false;

    nPosIni := fPalavras.GetStartIndex(aLastToken);
    for i:= nPosIni to fPalavras.Count - 1 do
    begin
      palavra := fPalavras.Items[i];
      if palavra.Texto.StartsWith(aLastToken, true) = true then
      begin
        bAchou := true;
        fSynCmp.AddItem(palavra.Texto,
                        DescricaoTiposToken[palavra.Tipo],
                        FSynLsp.Settings.GetSettingsByTokenKind(palavra.Tipo).Foreground,
                        '');//palavra.Sintaxe);
      end
      else
      begin
        // Depois que encontrou o primeiro, no momento que não encontrou mais pode parar de procurar
        if bAchou = true then
        begin
          break;
        end;
      end;
    end;
  end;

  fSynCmp.Width := fSynCmp.TheForm.Canvas.TextWidth(fSynCmp.Items.BiggestDescKind + fSynCmp.Items.BiggestCaption) + 60;
  fSynCmp.LinesInWindow := Min(CompletionLinesInWindow, fSynCmp.ItemList.Count);
end;

procedure TFrmMain.GetTip(Sender : TObject; aWord : string; const aRow : Integer; var aTip : string);
var
  Funcao: TLCDefinitionOfFunction;
  Regra:TLCArquivoRegra;
  i, j,
  nPos:integer;
begin
  aTip := '';
  nPos := LookupList.IndexOf(aWord);
  if (nPos >= 0) then
  begin
    aTip := TCompletionProposal(LookupList.Items[nPos]).Parametros;
  end
  else
  begin
    if TSynLCHighlighter((Sender as TLCSynEdit).Highlighter).IdentKind(False, aWord) in [tLCCustomFunction, tLCIdentifier] then
    begin
      if (Sender as TLCSynEdit).PosicaoRegra <> -1 then
      begin
        Regra := Modulos[(Sender as TLCSynEdit).ModuloVetorh].Regras[(Sender as TLCSynEdit).PosicaoRegra];
        Funcao := FCompilador.SearchFunction(Regra.Funcoes, aWord);
        if Funcao = nil then
        begin
          FCompilador.Lines := (Sender as TLCSynEdit).Lines;
          FCompilador.CompilarListaFuncoes(Regra.Funcoes);
          Funcao := FCompilador.SearchFunction(Regra.Funcoes, aWord);
          if Funcao <> nil then
          begin
            CarregarCustomFunction(Modulos[(Sender as TLCSynEdit).ModuloVetorh], TSynLCHighlighter((Sender as TLCSynEdit).Highlighter));
            (Sender as TLCSynEdit).Invalidate;
          end;
        end;

        if  (Funcao <> nil)
        and (Funcao.RowOfDefinition <> aRow) then
        begin
          aTip := Funcao.GetTipText;
        end
      end;

      if aTip.isEmpty = true then
      begin
        For j:=0 to High(Modulos[(Sender as TLCSynEdit).ModuloVetorh].Regras) do
        begin
          if (Sender as TLCSynEdit).PosicaoRegra <> j then
          begin
            Regra := Modulos[(Sender as TLCSynEdit).ModuloVetorh].Regras[j];
            For i:=0 to Regra.Funcoes.Count - 1 do
            begin
              Funcao := FCompilador.SearchFunction(Regra.Funcoes, aWord);
              if  (Funcao <> nil)
              and (Funcao.RowOfDefinition <> aRow) then
              begin
                aTip := Funcao.GetTipText;
                break;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TFrmMain.AddMsg(sMsg : String; sMsgEstatica : String);
var
  oMsg: TLCMessages;
begin
  oMsg := TLCMessages.Create;
  try
    oMsg.Tipo := lcTMUnknown;
    oMsg.Texto := sMsg;
    oMsg.TextoEstatico := sMsgEstatica;
    oMsg.Linha := -1;
    oMsg.Coluna := -1;

    AddMsg(oMsg);
  finally
    FreeAndNil(oMsg);
  end;
end;

procedure TFrmMain.AddMsg(pMsg : TLCMessages);
var
  oMsgNode: PLCMessageNode;
  oChildNode: PVirtualNode;
begin
  oChildNode := vstMessages.AddChild(nil);
  oMsgNode := vstMessages.GetNodeData(oChildNode);

  oMsgNode^.Texto := pMsg.Texto;
  oMsgNode^.TextoEstatico := pMsg.TextoEstatico;
  oMsgNode^.Linha := pMsg.Linha;
  oMsgNode^.Coluna := pMsg.Coluna;
  oMsgNode^.Tipo := pMsg.Tipo;
  oMsgNode^.ImageIndex := Ord(pMsg.Tipo);

  vstMessages.ValidateNode(nil, False);
end;

function TFrmMain.GetModuloByFileName(const aFileName : String) : TLCSiglaModuloVetorh;
var
  sPath:String;
  Sigla: TLCSiglaModuloVetorh;
begin
  Result := smvNone;
  sPath := LowerCase(ExtractFilePath(aFileName));

  for Sigla in TLCSiglaModuloVetorh do
  begin
    if LowerCase(Modulos[sigla].PastaBase) = sPath then
    begin
      Result := Sigla;
      break;
    end;
  end;
end;

function TFrmMain.GetPosicaoRegraByFileName(const Sigla : TLCSiglaModuloVetorh; const aFileName : String) : Integer;
var
  sFileName:String;
  i:integer;
begin
  result := -1;
  sFileName := LowerCase(aFileName);
  For i:=0 to High(Modulos[Sigla].Regras) do
  begin
    if LowerCase(Modulos[Sigla].Regras[i].NomeArquivo) = sFileName then
    begin
      result := i;
      break;
    end;
  end;
end;

procedure TFrmMain.GetCustomFunction(Sender : TObject; const pToken : String; var pIdentKind : TLCTokenKind);
var
  Funcao: TLCDefinitionOfFunction;
  Regra:TLCArquivoRegra;
  //i, j:integer;
  editor: TLCSynEdit;
begin
  editor := GetEditorAtivo;
  if (editor = nil) then
  begin
    exit;
  end;

  if editor.PosicaoRegra <> -1 then
  begin
    Regra := Modulos[editor.ModuloVetorh].Regras[editor.PosicaoRegra];
    Funcao := FCompilador.SearchFunction(Regra.Funcoes, pToken);
    if Funcao <> nil then
    begin
      pIdentKind := tLCCustomFunction;
      Exit;
    end
  end;

  //For j:=0 to High(Modulos[editor.ModuloVetorh].Regras) do
  //begin
  //  if editor.PosicaoRegra <> j then
  //  begin
  //    Regra := Modulos[editor.ModuloVetorh].Regras[j];
  //    For i:=0 to Regra.Funcoes.Count - 1 do
  //    begin
  //      Funcao := FCompilador.SearchFunction(Regra.Funcoes, pToken);
  //      if Funcao <> nil then
  //      begin
  //        pIdentKind := tLCCustomFunction;
  //        break;
  //      end;
  //    end;
  //  end;
  //end;
end;

procedure TFrmMain.CarregarCustomFunction(var pModulo : TLCModuloVetorh; pHL : TSynLCHighlighter);
Var
  Regra : TLCArquivoRegra;
  Funcao: TLCDefinitionOfFunction;
  aModulo : TLCModuloVetorh;
begin
  pHL.Settings.SettingsToCustomFunction.Words.Clear;

  for aModulo in Modulos do
  begin
    for Regra in aModulo.Regras do
    begin
      For Funcao in Regra.Funcoes do
      begin
        pHL.Settings.SettingsToCustomFunction.Words.Add(LowerCase(Funcao.Name));
      end;
    end;
  end;
  pHL.PopularArrayOfTokenDef;
end;

function TFrmMain.GetEditorAtivo: TLCSynEdit;
begin
  result := nil;
  if  (ECTabCtrl1.Tabs.Count > 0) then
  begin
    if Assigned(ECTabCtrl1.Tabs[ECTabCtrl1.TabIndex].Control) then
    begin
      result := (ECTabCtrl1.Tabs[ECTabCtrl1.TabIndex].Control as TLCSynEdit);
    end;
  end;
end;

procedure TFrmMain.CriarMenuMRU();
var
  oMnu:TMenuItem;
  i:Integer;
begin
  for i:=0 to MenuItem7.Count - 4 do
  begin
    MenuItem7.Delete(0);
  end;

  PopupMRU.Items.Clear;

  For i:=1 to fSettings.MruFiles.Count do
  begin
    oMnu:=TMenuItem.Create(MenuItem7);
    oMnu.Caption := Format('%3.2d : %s',[i, fSettings.getMruFile(i-1).GetPathName]); ;
    oMnu.Tag := i;
    oMnu.OnClick := @MenuItemMRUClick;
    MenuItem7.Insert(i-1, oMnu);

    oMnu:=TMenuItem.Create(PopupMRU.Items);
    oMnu.Caption := Format('%3.2d : %s',[i, fSettings.getMruFile(i-1).GetPathName]); ;
    oMnu.Tag := i;
    oMnu.OnClick := @MenuItemMRUClick;
    PopupMRU.Items.Add(oMnu);
  end;
end;

procedure TFrmMain.CriarBackupArquivo(aFileName : String);
var
  aExt,
  aTmpName,
  aBkpFileName:String;
  i,
  iNumFile,
  iLastFile:Integer;
  lFiles:TStringList;
begin
  if fSettings.MakeBackupFiles = false then
  begin
    exit;
  end;

  if FileExistsUTF8(aFileName) then
  begin
    if (fSettings.PathToBackupFiles = '') then
    begin
      fSettings.PathToBackupFiles := IncludeTrailingPathDelimiter(fSettings.PathAppRoot + 'backup');
    end;

    if DirectoryExistsUTF8(fSettings.PathToBackupFiles) = false then
    begin
      CreateDirUTF8(fSettings.PathToBackupFiles);
    end;

    lFiles := FindAllFiles(fSettings.PathToBackupFiles,ExtractFileName(ChangeFileExt(aFileName, '.*')), false);
    aExt := '';
    iLastFile := 0;
    try
      For i:=0 to lFiles.Count -1 do
      begin
        aExt := ExtractFileExt(lFiles.Strings[i]);
        Delete(aExt, 1, 1);
        iNumFile := StrToIntDef(PChar(aExt) ,0);
        iLastFile := Max(iNumFile, iLastFile);
      end;

      if ((iLastFile >= fSettings.NumberOfBackupFilesToPreserve)
      or  (lFiles.Count >= fSettings.NumberOfBackupFilesToPreserve)) then
      begin
        lFiles.Sorted := false;

        // Trocar o nome para permitir reordenar
        For i:=0 to lFiles.Count -1 do
        begin
          aTmpName := ExtractFilePath(lFiles.Strings[i]) + '_' + ExtractFileName(lFiles.Strings[i]);
          if (FileExistsUTF8(aTmpName) = true) then
          begin
            DeleteFileUTF8(aTmpName);
          end;
          RenameFileUTF8(lFiles.Strings[i], aTmpName);
          lFiles.Strings[i] := aTmpName;
        end;

        // Apagar os arquivos que excedam ao Limite determinado
        while (lFiles.Count >= fSettings.NumberOfBackupFilesToPreserve) do
        begin
          DeleteFileUTF8(lFiles.Strings[0]);
          lFiles.Delete(0);
        end;

        // Renomear para o nome reordenado
        For i:=0 to lFiles.Count -1 do
        begin
          aTmpName := ExtractFileNameOnly(lFiles.Strings[i]);
          Delete(aTmpName, 1, 1);
          aExt := '.' + Format('%.3d', [i + 1]);
          aTmpName := ExtractFilePath(lFiles.Strings[i]) + aTmpName + aExt;
          if (FileExistsUTF8(aTmpName) = true) then
          begin
            DeleteFileUTF8(aTmpName);
          end;
          RenameFileUTF8(lFiles.Strings[i], aTmpName);
        end;

        iLastFile := lFiles.Count;
      end;
    finally
      FreeAndNil(lFiles);
    end;
    aExt := '.' + Format('%.3d', [iLastFile + 1]);
    aBkpFileName := IncludeTrailingPathDelimiter(fSettings.PathToBackupFiles) +
                    ExtractFileName(ChangeFileExt(aFileName, aExt));
    CopyFile(aFileName, aBkpFileName, true, false);
  end;
end;

procedure TFrmMain.ShowMsgAviso(aShow : Boolean);
begin
  Panel4.Visible := aShow;
  Splitter4.Visible := aShow;
  actShowMsg.Checked := aShow;
end;

procedure TFrmMain.ShowExplorer(aShow: Boolean);
begin
  vstExplorer.RootNodeCount := 0;

  DirectoryEdit1.Directory := fSettings.PathBase;

  if (aShow = true) then
  begin
    LerDiretorio(vstExplorer, nil, fSettings.Filters, fSettings.PathBase);
  end;

  actMostrarExplorer.Checked := aShow;
  pnlExplorer.Visible := aShow;
  Splitter3.Visible := aShow;
end;

procedure TFrmMain.ShowListOfCommands(aShow : Boolean);
begin
  actMostrarComandos.Checked := aShow;
  pnlComandos.Visible := aShow;
  Splitter2.Visible := aShow;
end;

procedure TFrmMain.CarregarConfiguracoes(EhChamadaInterna : Boolean);
var
  i: integer;
  Modulo : TModuloVetorh;
begin
  if (EhChamadaInterna = false) then
  begin
    fSettings.LoadFromFile(fSettings.PathConfig + configFileName);

    SetBounds(fSettings.Left, fSettings.Top, fSettings.Width, fSettings.Height);

    if fSettings.Maximized = true then
    begin
      WindowState:= wsMaximized;
    end;

    pnlExplorer.Width := fSettings.ExplorerWidth;
    pnlComandos.Width := fSettings.ListOfCommandsWidth;
    ecaLista.ItemIndex := fSettings.ListOfCommandsIndex;
    IdleTimer1.Interval := Round(fSettings.IntervalAutoSaveFile * SecsPerMin * MSecsPerSec);
    IdleTimer1.AutoEnabled := fSettings.EnabledAutoSaveFile;

    For i:= 0 to fSettings.ModulosVetorh.Count - 1 do
    begin
      Modulo := TModuloVetorh(fSettings.ModulosVetorh.Items[i]);
      Modulos[Modulo.Sigla].PastaBase := Modulo.PastaBase;
      Modulos[Modulo.Sigla].ArquivosExcluidos.Assign(Modulo.ArquivosExcluidos);
    end;
  end
  else
  begin
    ShowExplorer(fSettings.ShowExplorer);
    ShowListOfCommands(fSettings.ShowListOfCommands);
  end;
  CriarMenuMRU;
end;

procedure TFrmMain.SalvarConfiguracoes;
begin
  fSettings.Maximized := (WindowState = wsMaximized);
  if fSettings.Maximized = false then
  begin
    fSettings.Left := Left;
    fSettings.Top := Top;
    fSettings.Width := Width;
    fSettings.Height := Height;
  end;

  fSettings.ExplorerWidth := pnlExplorer.Width;
  fSettings.ListOfCommandsWidth := pnlComandos.Width;
  fSettings.ListOfCommandsIndex := ecaLista.ItemIndex;

  fSettings.SaveToFile(fSettings.PathConfig + configFileName);
end;

function TFrmMain.FecharTodasAbas(ExcetoAtiva : Boolean; SaveSession : Boolean) : Boolean;
var
  start,
  iActive,
  i:integer;
  aFileName: String;
  oSessionFile: TSessionFile;
begin
  result := true;
  try
    ECTabCtrl1.BeginUpdate;
    try
      start := 0;
      if (ExcetoAtiva = true) then
      begin
        ECTabCtrl1.MoveTab(ECTabCtrl1.TabIndex, 0);
        start := 1;
      end;

      iActive := ECTabCtrl1.TabIndex;
      for i:= ECTabCtrl1.Tabs.Count - 1 downto start do
      begin
        aFileName := (ECTabCtrl1.Tabs[i].Control as TLCSynEdit).FileName;

        if FecharDocumento(ECTabCtrl1.Tabs[i].Control as TLCSynEdit) = false then
        begin
          result := false;
          exit;
        end;

        // Gravar informacoes da Sessao (arquivos abertos)
        if  (SaveSession = true)
        and (FileExistsUTF8(aFileName) = true) then
        begin
          oSessionFile := TSessionFile(fSettings.SessionFiles.Insert(0));
          oSessionFile.FileName := aFileName;
          oSessionFile.Active := (iActive = i);
        end;

        ECTabCtrl1.DeleteTab(i);
      end;
    finally
      ECTabCtrl1.EndUpdate;
    end;
  except
    result := false;
    // TODO: Gerar log
  end;
end;

procedure TFrmMain.CarregarCompletionProposal;
var
  CP: TCompletionProposal;
  Lst: TStringList;
  aFileName,
  Linha: String;
  i,
  Lin:Integer;
begin
  Lst := TStringList.Create;
  aFileName := fSettings.PathConfig + FILECOMPLETIONPROPOSAL;
  if FileExistsUTF8(aFileName) = True then
  begin
    Lst.LoadFromFile(aFileName);
  end;

  if (LookupList = nil) then
  begin
    LookupList := TCompletionProposalList.Create(TCompletionProposal);
  end;
  LookupList.Clear;

  LookupList.BeginUpdate;
  try
    CP := nil;
    For Lin:= 0 to Lst.Count - 1 do
    Begin
      Linha := Trim(Lst.Strings[Lin]);
      if Linha <> '' then
      Begin
        i := StrToIntDef(Copy(Linha,1,5),0);
        Delete(Linha,1,5);
        Linha := Trim(Linha);
        Case i of
          1:
            begin
              cp := TCompletionProposal(LookupList.Add);
              cp.Nome := Linha;
            end;
          2:
            begin
              CP.Parametros := Linha
            end;
          3:
            begin
              CP.Descricao := Linha
            end;
        end; // Case
      end;
    end; // For
  finally
    LookupList.EndUpdate;
  end;
  FreeAndNil(Lst);
end;

procedure TFrmMain.CarregarListaComandos;
var
  p,
  i : integer;
  dc : TDefComando;
begin
  lbSintaxe.Caption := '';
  lbDescricao.Caption := '';

  // liberar a memória
  For i:= 0 to lbxComandos.Count - 1 do
  begin
    TDefComando(lbxComandos.Items.Objects[i]).Free;
    lbxComandos.Items.Objects[i] := nil;
  end;

  lbxComandos.Items.Clear;
  lbxComandos.Items.BeginUpdate;
  Try
    for i:= 0 to FSynLsp.Settings.SettingsToKey.Words.Count - 1 do
    begin
      dc := TDefComando.Create;
      dc.Nome := Trim(FSynLsp.Settings.SettingsToKey.Words[i]);
      dc.Descricao := '';
      dc.Sintaxe := '';
      p := LookupList.IndexOf(dc.Nome);
      if P >= 0 then
      begin
        dc.Descricao := TCompletionProposal(LookupList.Items[p]).Descricao;
        dc.Sintaxe := TCompletionProposal(LookupList.Items[p]).Sintaxe;
      end;

      lbxComandos.AddItem(dc.Nome, dc);
    end;
  Finally
    lbxComandos.Items.EndUpdate;
  End;

  // Liberar a memória
  For i:= 0 to lbxFuncoes.Count - 1 do
  begin
    TDefComando(lbxFuncoes.Items.Objects[i]).Free;
    lbxFuncoes.Items.Objects[i] := nil;
  end;
  lbxFuncoes.Items.Clear;
  lbxFuncoes.Items.BeginUpdate;
  Try
    for i:= 0 to FSynLsp.Settings.SettingsToReservedWord.Words.Count - 1 do
    begin
      dc := TDefComando.Create;
      dc.Nome := Trim(FSynLsp.Settings.SettingsToReservedWord.Words[i]);
      dc.Descricao := '';
      dc.Sintaxe := '';

      p := LookupList.IndexOf(dc.Nome);
      if P >= 0 then
      begin
        dc.Descricao := TCompletionProposal(LookupList.Items[p]).Descricao;
        dc.Sintaxe := TCompletionProposal(LookupList.Items[p]).Sintaxe;
      end;

      lbxFuncoes.AddItem(dc.Nome, dc);
    end;
  Finally
    lbxFuncoes.Items.EndUpdate;
  End;

  // Liberar a memória
  For i:= 0 to lbxVariaveis.Count - 1 do
  begin
    TDefComando(lbxVariaveis.Items.Objects[i]).Free;
    lbxVariaveis.Items.Objects[i] := nil;
  end;
  lbxVariaveis.Items.Clear;
  lbxVariaveis.Items.BeginUpdate;
  Try
    for i:= 0 to FSynLsp.Settings.SettingsToVariable.Words.Count - 1 do
    begin
      dc := TDefComando.Create;
      dc.Nome := Trim(FSynLsp.Settings.SettingsToVariable.Words[i]);
      dc.Descricao := '';
      dc.Sintaxe := '';

      //p := LookupList.IndexOf(dc.Nome);
      //if P >= 0 then
      //begin
      //  dc.Descricao := TCompletionProposal(LookupList.Items[p]).Descricao;
      //  dc.Sintaxe := TCompletionProposal(LookupList.Items[p]).Sintaxe;
      //end;

      lbxVariaveis.AddItem(dc.Nome, dc);
    end;
  Finally
    lbxVariaveis.Items.EndUpdate;
  End;
end;

procedure TFrmMain.CarregarPalavras;
var
  p,
  i: integer;
  Palavra: TLCPalavra;
begin
  fPalavras.Clear;

  for i:= 0 to FSynLsp.Settings.SettingsToKey.Words.Count - 1 do
  begin
    Palavra:= TLCPalavra.Create;
    Palavra.Texto := Trim(FSynLsp.Settings.SettingsToKey.Words[i]);
    Palavra.Tipo := tLCKey;
    p := LookupList.IndexOf(Palavra.Texto);
    if P >= 0 then
    begin
      Palavra.Descricao := TCompletionProposal(LookupList.Items[p]).Descricao;
      Palavra.Parametros := TCompletionProposal(LookupList.Items[p]).Parametros;
    end;

    fPalavras.Add(Palavra);
  end;

  for i:= 0 to FSynLsp.Settings.SettingsToReservedWord.Words.Count - 1 do
  begin
    Palavra:= TLCPalavra.Create;
    Palavra.Texto := Trim(FSynLsp.Settings.SettingsToReservedWord.Words[i]);
    Palavra.Tipo := tLCReservedWord;
    p := LookupList.IndexOf(Palavra.Texto);
    if P >= 0 then
    begin
      Palavra.Descricao := TCompletionProposal(LookupList.Items[p]).Descricao;
      Palavra.Parametros := TCompletionProposal(LookupList.Items[p]).Parametros;
    end;

    fPalavras.Add(Palavra);
  end;

  for i:= 0 to FSynLsp.Settings.SettingsToAttributeName.Words.Count - 1 do
  begin
    Palavra:= TLCPalavra.Create;
    Palavra.Texto := Trim(FSynLsp.Settings.SettingsToAttributeName.Words[i]);
    Palavra.Tipo := tLCAttributeName;
    fPalavras.Add(Palavra);
  end;

  for i:= 0 to FSynLsp.Settings.SettingsToDataType.Words.Count - 1 do
  begin
    Palavra:= TLCPalavra.Create;
    Palavra.Texto := Trim(FSynLsp.Settings.SettingsToDataType.Words[i]);
    Palavra.Tipo := tLCDataType;
    fPalavras.Add(Palavra);
  end;

  for i:= 0 to FSynLsp.Settings.SettingsToVariable.Words.Count - 1 do
  begin
    Palavra:= TLCPalavra.Create;
    Palavra.Texto := Trim(FSynLsp.Settings.SettingsToVariable.Words[i]);
    Palavra.Tipo := tLCVariable;
    fPalavras.Add(Palavra);
  end;

  fPalavras.Sort(@OrdenarPalavras);
  fPalavras.AtualizarInidices;
end;

function TFrmMain.DirectoryIsOpen(ADirectory : String) : Boolean;
var
  i: Integer;
  editor:TLCSynEdit;
begin
  Result := False;
  if (ADirectory = '') then
  begin
    exit;
  end;
  for i := 0 to ECTabCtrl1.Tabs.Count - 1 do
  begin
    editor := (ECTabCtrl1.Tabs[i].Control as TLCSynEdit);
    if AnsiUpperCase(ExtractFileDir(editor.FileName)).IndexOf(AnsiUpperCase(ADirectory)) >= 0 then
    begin
      Result := True;
      Exit;
    end; // if
  end;// for
end;

function TFrmMain.FileIsOpen(pFileName : String; pAtivar : Boolean) : Boolean;
var
  i: Integer;
  editor:TLCSynEdit;
begin
  Result := False;
  if (pFileName = '') then
  begin
    exit;
  end;
  for i := 0 to ECTabCtrl1.Tabs.Count - 1 do
  begin
    editor := (ECTabCtrl1.Tabs[i].Control as TLCSynEdit);
    if AnsiUpperCase(editor.FileName) = AnsiUpperCase(pFileName) then
    begin
      if pAtivar = true then
      begin
        ECTabCtrl1.ActivateTab(i);
      end;
      Result := True;
      Exit;
    end; // if
  end;// for
end;

function TFrmMain.FecharDocumento(editor: TLCSynEdit): Boolean;
var
  nRet:integer;
begin
  result := false;
  try
    if (editor.Modified = true) then
    begin
      nRet := Application.MessageBox(PChar(Format(rsQuestionSaveFile,[editor.FileName])),
                                 PChar(Application.Title),
                                 MB_YESNOCANCEL or MB_DEFBUTTON2 or MB_ICONQUESTION);
      if (nRet = IDCANCEL) then
      begin
        exit;
      end;

      if (nRet = IDYES) then
      begin
        if editor.Save = false then
        begin
          exit;
        end;
      end;
    end;

    if editor.Saved = true then
    begin
      fSettings.AddMruFiles(editor.FileName, editor);
      CriarMenuMRU;
    end;

    fSettings.UpdateSettingsByEditor(editor);

    fSynAuC.RemoveEditor(editor);
    fSynCmp.RemoveEditor(editor);

    FreeAndNil(editor);

    if ECTabCtrl1.Tabs.Count = 1 then
    begin
      ECTabCtrl1.Visible := false;
    end;

    result := true;
    vstExplorer.Invalidate;
  except
    // TODO: Gerar Log
  end;
end;

procedure TFrmMain.EditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
var
  ptCaret: TPoint;
  Size : Double;
  Aux : String;
  i : Integer;
  editor: TLCSynEdit;
begin
  if (Sender is TLCSynEdit) = false then
  begin
    exit;
  end;

  editor := (Sender as TLCSynEdit);
  StatusBar1.Panels[iPnlMessage].Text := '';

  if Changes * [scFocus, scCaretX, scCaretY] <> [] then
  begin
    ptCaret.X := editor.CaretXY.x;
    ptCaret.Y := editor.CaretXY.y;

    if  (ptCaret.X > 0)
    and (ptCaret.Y > 0) then
    begin
      StatusBar1.Panels[iPnlCaretXY].Text := 'Lin ' + trim(FormatFloat('#,#00', ptCaret.Y)) +
                                             ' Col ' + trim(FormatFloat('#,#00', ptCaret.X));
    end
    else
      StatusBar1.Panels[iPnlCaretXY].Text := '';
  end;

  if Changes * [scFocus, scInsertMode] <> [] then
  begin
    Statusbar1.Panels[iPnlMode].Text := InsertModeStrs[editor.InsertMode];
  end;

  if Changes * [scFocus, scModified, scReadOnly] <> [] then
  begin
    StatusBar1.Panels[iPnlModified].Text := ModifiedStrs[editor.Modified];
    if editor.ReadOnly = false then
    begin
      if (editor.Modified = true) then
      begin
        ECTabCtrl1.Tabs[ECTabCtrl1.Tabs.IDToIndex(editor.UniqueIndex-1)].ImageIndex := 1;
      end
      else
      begin
        ECTabCtrl1.Tabs[ECTabCtrl1.Tabs.IDToIndex(editor.UniqueIndex-1)].ImageIndex := 0;
      end;
    end;
  end;

  if Changes * [scFocus, scReadOnly] <> [] then
  begin
    if editor.ReadOnly then
    begin
      ECTabCtrl1.Tabs[ECTabCtrl1.Tabs.IDToIndex(editor.UniqueIndex-1)].ImageIndex := 2;
      StatusBar1.Panels[iPnlReadOnly].Text := 'R';
    end
    else
    begin
      StatusBar1.Panels[iPnlReadOnly].Text := 'N';
    end;
  end;

  if editor.SelectionMode = smNormal then
  begin
    StatusBar1.Panels[iPnlModoSel].Text := 'N'
  end
  else
  begin
    if editor.SelectionMode = smColumn then
    begin
      StatusBar1.Panels[iPnlModoSel].Text := 'C'
    end
    else
    begin
      if editor.SelectionMode = smLine then
      begin
        StatusBar1.Panels[iPnlModoSel].Text := 'L';
      end;
    end;
  end;

  if scSelection in Changes then
  begin
    if editor.SelAvail = True then
    begin
      Size := length(editor.SelText);
      if Size = 1 then
      begin
        StatusBar1.Panels[iPnlMessage].Text := ' 1 Caracter Selecionado'
      end
      else
      begin
        Aux := editor.SelText;
        Size := 0;
        for i := 1 to length(Aux) do
        begin
          if  (Aux[i] <> #10)
          and (Aux[i] <> #13) then
          begin
            Size := Size + 1;
          end;
        end;
        StatusBar1.Panels[iPnlMessage].Text := FormatFloat(' #,##0',Size) + ' Caracteres Selecionados';
      end;
    end;
  end;

  Size := Length(editor.Text)/1024;
  if Size > 0 then
  begin
    Aux := 'Kb';
    if Size > 999 then
    begin
      Aux := 'Mb';
      Size := Size / 1024;
    end;
    StatusBar1.Panels[iPnlSize].Text := FormatFloat('0.00',Size) + ' ' + Aux;
  end
  else
  begin
    StatusBar1.Panels[iPnlSize].Text := '';
  end;
  StatusBar1.Panels[iPnlFileCode].Text := AnsiUpperCase(editor.FileCode);

end;

function TFrmMain.VersionOfApplication: String;
var
  FileVerInfo: TFileVersionInfo;
begin
  result := '';
  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.FileName:= Application.ExeName;
    FileVerInfo.ReadFileInfo;
    result := FileVerInfo.VersionStrings.Values['FileVersion'];
    //writeln('Company: ',FileVerInfo.VersionStrings.Values['CompanyName']);
    //writeln('File description: ',FileVerInfo.VersionStrings.Values['FileDescription']);
    //writeln('File version: ',FileVerInfo.VersionStrings.Values['FileVersion']);
    //writeln('Internal name: ',FileVerInfo.VersionStrings.Values['InternalName']);
    //writeln('Legal copyright: ',FileVerInfo.VersionStrings.Values['LegalCopyright']);
    //writeln('Original filename: ',FileVerInfo.VersionStrings.Values['OriginalFilename']);
    //writeln('Product name: ',FileVerInfo.VersionStrings.Values['ProductName']);
    //writeln('Product version: ',FileVerInfo.VersionStrings.Values['ProductVersion']);
  finally
    FileVerInfo.Free;
  end;
end;

function TFrmMain.GetAttributeSettingsDefault(pPadrao:TSynLCHighlighterSettings; const pValue : TLCTokenKind) : TSynLCAttributeSettings;
begin
  Result := nil;
  case pValue of
    tLCAttributeName: Result := TSynLCAttributeSettings(pPadrao.SettingsToAttributeName);
    tLCComment: Result := TSynLCAttributeSettings(pPadrao.SettingsToComment);
    tLCCustomFunction: Result := TSynLCAttributeSettings(pPadrao.SettingsToCustomFunction);
    tLCDataType: Result := TSynLCAttributeSettings(pPadrao.SettingsToDataType);
    tLCIdentifier: Result := TSynLCAttributeSettings(pPadrao.SettingsToIdentifier);
    tLCKey: Result := TSynLCAttributeSettings(pPadrao.SettingsToKey);
    tLCNumber: Result := TSynLCAttributeSettings(pPadrao.SettingsToNumber);
    tLCReservedWord: Result := TSynLCAttributeSettings(pPadrao.SettingsToReservedWord);
    tLCSpace: Result := TSynLCAttributeSettings(pPadrao.SettingsToSpace);
    tLCString: Result := TSynLCAttributeSettings(pPadrao.SettingsToString);
    tLCSymbol: Result := TSynLCAttributeSettings(pPadrao.SettingsToSymbol);
    tLCVariable: Result := TSynLCAttributeSettings(pPadrao.SettingsToVariable);
  end;
end;

procedure TFrmMain.NovoDocumento(NomeArquivo: String; Ativar:Boolean);
var
  tab : TECTab;
  editor : TLCSynEdit;
  bCriar : Boolean;
  M : TSynEditMarkupFoldColors;

begin
  if (FileIsOpen(NomeArquivo) = true) then
  begin
    exit;
  end;

  ECTabCtrl1.BeginUpdate;
  try
    bCriar := true;
    tab := nil;
    editor := nil;
    if  (ECTabCtrl1.Tabs.Count = 1)
    and (NomeArquivo <> '') then
    begin
      tab := ECTabCtrl1.Tabs[0];
      editor := (tab.Control as TLCSynEdit);
      if  (editor.Modified = false)
      and (Trim(editor.Text) = '') then
      begin
        bCriar := false;
        editor.LoadFromFile(NomeArquivo);
      end;
    end;

    if (bCriar = true) then
    begin
      tab := ECTabCtrl1.AddTab(etaLast, Ativar);

      tab.Options := [etoCloseable,etoCloseBtn,etoVisible];
      tab.ImageIndex := 0;

      editor := TLCSynEdit.Create(self, tab.ID+1, NomeArquivo);
      tab.Control := editor;
      tab.PopupMenu := PopupTabs;

      editor.Width := pnlEditor.Width;
      editor.Height:= pnlEditor.Height;
      editor.Align := alClient;
      editor.BorderStyle := bsNone;
      editor.Highlighter := FSynLsp;
      editor.OnStatusChange := @EditorStatusChange;
      editor.OnGetTipOf := @GetTip;
      //TSynLCHighlighter(editor.Highlighter).OnGetCustomIdentKind := @GetCustomFunction;
      editor.OnSpecialLineMarkup := @DoSpecialLineMarkup;
      editor.PopupMenu := PopupEditor;
      editor.SaveDialog := SaveDialog1;
      editor.BookMarkOptions.BookmarkImages := ImageListBookmark;

      M := TSynEditMarkupFoldColors.Create(editor);
      editor.MarkupManager.AddMarkUp(M);

      fSynAuC.AddEditor(editor);
      FSynCmp.AddEditor(editor);
      FSynCmp.OnExecute := @DoExecuteCompletion;
      FSynCmp.OnSearchPosition:= @DoSearchPosition;

      editor.Parent := pnlEditor;
    end;

    if (editor <> nil) then
    begin
      if ECTabCtrl1.Visible = false then
      begin
        ECTabCtrl1.Visible := true;
      end;
      tab.Text := ExtractFileName(editor.FileName);
      tab.Hint := editor.FileName;
      AtualizarPreferencias(editor);
      EditorStatusChange(editor, [scFocus]); // Atualizar o status bar

      Editor.ModuloVetorh := GetModuloByFileName(Editor.FileName);
      Editor.PosicaoRegra := GetPosicaoRegraByFileName(Editor.ModuloVetorh, Editor.FileName);

      if  (Editor.PosicaoRegra = -1)
      and (Editor.ModuloVetorh = smvNone) then
      begin
        Editor.PosicaoRegra := AdicionarRegraAoModulo(Modulos[Editor.ModuloVetorh], Editor.FileName);
      end;

      if Modulos[Editor.ModuloVetorh].Carregado = false then
      begin
        CarregarModulo(Modulos[Editor.ModuloVetorh]);
      end;
      CarregarCustomFunction(Modulos[Editor.ModuloVetorh], TSynLCHighlighter(editor.Highlighter));
    end;
  finally
    ECTabCtrl1.EndUpdate;
  end;

  vstExplorer.Invalidate;

  if (Ativar = true) then
  begin
    ECTabCtrl1.MakeTabAvailable(tab.Index, Ativar);
  end;
end;

procedure TFrmMain.AtualizarPreferencias(editor: TSynEdit);
var
  oMarkupFoldColors: TSynEditMarkupFoldColors;
  oSynMarkHAllCaret: TSynEditMarkupHighlightAllCaret;
  i:integer;
  Elemento: TLCElementoSintaxe;
  oPadrao: TSynLCHighlighterSettings;
begin
  editor.BeginUpdate(false);
  try
    // Editor
    editor.WantTabs := fSettings.Editor.WantTabs;
    editor.MaxUndo := fSettings.Editor.MaxUndo;
    editor.RightEdge := FrmMain.fSettings.Editor.RightEdge;
    editor.RightEdgeColor := FrmMain.fSettings.Editor.RightEdgeColor;
    editor.Options := FrmMain.fSettings.Editor.Options;
    editor.Options2 := FrmMain.fSettings.Editor.Options2;
    editor.TabWidth := FrmMain.fSettings.Editor.TabWidth;

    // Ajustar Teclas de atalho para bookmarks
    editor.Keystrokes[64].Command := ecToggleMarker0;
    editor.Keystrokes[65].Command := ecToggleMarker1;
    editor.Keystrokes[66].Command := ecToggleMarker2;
    editor.Keystrokes[67].Command := ecToggleMarker3;
    editor.Keystrokes[68].Command := ecToggleMarker4;
    editor.Keystrokes[69].Command := ecToggleMarker5;
    editor.Keystrokes[70].Command := ecToggleMarker6;
    editor.Keystrokes[71].Command := ecToggleMarker7;
    editor.Keystrokes[72].Command := ecToggleMarker8;
    editor.Keystrokes[73].Command := ecToggleMarker9;

    // Desativar teclas de atalho para mudar Modo de Seleção
    editor.Keystrokes[87].ShortCut := scNone;
    editor.Keystrokes[88].ShortCut := scNone;
    editor.Keystrokes[89].ShortCut := scNone;

    //
    editor.BracketHighlightStyle := FrmMain.fSettings.Editor.BracketHighlightStyle;
    editor.BracketMatchColor.Assign(FrmMain.fSettings.Editor.BracketMatchColor);

    // Linha ativa
    editor.LineHighlightColor.Assign(FrmMain.fSettings.Editor.ActiveLine);

    // Font
    editor.Font.Name := FrmMain.fSettings.Editor.FontName;
    editor.Font.Size := FrmMain.fSettings.Editor.FontSize;
    editor.Font.Quality := FrmMain.fSettings.Editor.FontQuality;

    // Gutter
    editor.Gutter.Visible := FrmMain.fSettings.Editor.Gutter.Visible;
    editor.Gutter.AutoSize := FrmMain.fSettings.Editor.Gutter.AutoSize;
    editor.Gutter.Width := FrmMain.fSettings.Editor.Gutter.Width;
    editor.Gutter.Color := FrmMain.fSettings.Editor.Gutter.Color;

    // Número das linhas do Gutter
    editor.Gutter.LineNumberPart(0).Visible:=FrmMain.fSettings.Editor.Gutter.LineNumberPart.Visible;
    editor.Gutter.LineNumberPart(0).ShowOnlyLineNumbersMultiplesOf:=FrmMain.fSettings.Editor.Gutter.LineNumberPart.ShowOnlyLineNumbersMultiplesOf;
    editor.Gutter.LineNumberPart(0).AutoSize:=FrmMain.fSettings.Editor.Gutter.LineNumberPart.AutoSize;
    editor.Gutter.LineNumberPart(0).DigitCount:=FrmMain.fSettings.Editor.Gutter.LineNumberPart.DigitCount;
    editor.Gutter.LineNumberPart(0).MarkupInfo.Background:=FrmMain.fSettings.Editor.Gutter.LineNumberPart.MarkupInfo.Background;
    editor.Gutter.LineNumberPart(0).MarkupInfo.Foreground:=FrmMain.fSettings.Editor.Gutter.LineNumberPart.MarkupInfo.Foreground;  // Cor dos número das linhas
    editor.Gutter.LineNumberPart(0).MarkupInfo.Style:=FrmMain.fSettings.Editor.Gutter.LineNumberPart.MarkupInfo.Style;
    editor.Gutter.LineNumberPart(0).LeadingZeros:=FrmMain.fSettings.Editor.Gutter.LineNumberPart.LeadingZeros;
    editor.Gutter.LineNumberPart(0).ZeroStart:=FrmMain.fSettings.Editor.Gutter.LineNumberPart.ZeroStart;

    editor.Gutter.ChangesPart(0).AutoSize := FrmMain.fSettings.Editor.Gutter.ChangesPart.AutoSize;
    editor.Gutter.ChangesPart(0).Width := FrmMain.fSettings.Editor.Gutter.ChangesPart.Width;
    editor.Gutter.ChangesPart(0).ModifiedColor := FrmMain.fSettings.Editor.Gutter.ChangesPart.ModifiedColor;
    editor.Gutter.ChangesPart(0).SavedColor := FrmMain.fSettings.Editor.Gutter.ChangesPart.SavedColor;

    editor.Gutter.SeparatorPart(0).AutoSize:=FrmMain.fSettings.Editor.Gutter.SeparatorPart.AutoSize;
    editor.Gutter.SeparatorPart(0).Width:=FrmMain.fSettings.Editor.Gutter.SeparatorPart.Width;
    editor.Gutter.SeparatorPart(0).MarkupInfo.Background:=FrmMain.fSettings.Editor.Gutter.SeparatorPart.MarkupInfo.Background;

    editor.Gutter.CodeFoldPart(0).MarkupInfo.Background:=FrmMain.fSettings.Editor.Gutter.CodeFoldPart.MarkupInfo.Background;

    oSynMarkHAllCaret := TSynEditMarkupHighlightAllCaret(editor.MarkupByClass[TSynEditMarkupHighlightAllCaret]);
    if assigned(oSynMarkHAllCaret) then
    begin
      oSynMarkHAllCaret.MarkupInfo.Assign(fSettings.Editor.MarkupHighlightAllCaret.MarkupInfo);
      oSynMarkHAllCaret.Trim := fSettings.Editor.MarkupHighlightAllCaret.Trim;
      oSynMarkHAllCaret.FullWord := fSettings.Editor.MarkupHighlightAllCaret.FullWord;
      oSynMarkHAllCaret.WaitTime := fSettings.Editor.MarkupHighlightAllCaret.WaitTime;
      oSynMarkHAllCaret.FullWordMaxLen := fSettings.Editor.MarkupHighlightAllCaret.FullWordMaxLen;
    end;

    oMarkupFoldColors := TSynEditMarkupFoldColors(editor.MarkupByClass[TSynEditMarkupFoldColors]);
    if assigned(oMarkupFoldColors) then
    begin
      for i:= 0 to MAX_NIVEL_IDENTACAO-1 do
      begin
        oMarkupFoldColors.Color[i].Foreground := TLCNiveisIdentacaoConfig(fSettings.Editor.NiveisIdentacao.Items[i]).Atributos.Foreground;
      end;
    end;

    oPadrao:= TSynLCHighlighterSettings.Create;
    try
      For i:= 0 to FrmMain.fSettings.Editor.ElementosSintaxe.count - 1 do
      begin
        Elemento := TLCElementoSintaxe(FrmMain.fSettings.Editor.ElementosSintaxe.Items[i]);
        if Elemento.DefaultValues = true then
        begin
          Elemento.Atributos.Assign(GetAttributeSettingsDefault(oPadrao, Elemento.Kind));
        end;

        case Elemento.Kind of
          tLCAttributeName: FSynLsp.Settings.SettingsToAttributeName.Assign(Elemento.Atributos);
          tLCComment: FSynLsp.Settings.SettingsToComment.Assign(Elemento.Atributos);
          tLCCustomFunction: FSynLsp.Settings.SettingsToCustomFunction.Assign(Elemento.Atributos);
          tLCDataType: FSynLsp.Settings.SettingsToDataType.Assign(Elemento.Atributos);
          tLCIdentifier: FSynLsp.Settings.SettingsToIdentifier.Assign(Elemento.Atributos);
          tLCKey: FSynLsp.Settings.SettingsToKey.Assign(Elemento.Atributos);
          tLCNumber: FSynLsp.Settings.SettingsToNumber.Assign(Elemento.Atributos);
          tLCReservedWord: FSynLsp.Settings.SettingsToReservedWord.Assign(Elemento.Atributos);
          tLCSpace: FSynLsp.Settings.SettingsToSpace.Assign(Elemento.Atributos);
          tLCString: FSynLsp.Settings.SettingsToString.Assign(Elemento.Atributos);
          tLCSymbol: FSynLsp.Settings.SettingsToSymbol.Assign(Elemento.Atributos);
          tLCVariable: FSynLsp.Settings.SettingsToVariable.Assign(Elemento.Atributos);
        end;
      end;
    finally
      FreeAndNil(oPadrao);
    end;
    FSynLsp.UpdateAttributesBySettings;
    editor.Invalidate;
  finally
    editor.EndUpdate;
  end;
end;

end.

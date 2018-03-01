unit uEditorLspSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, SysUtils, Forms,
  SynEdit, SynEditMiscClasses, SynEditMarkupBracket, SynEditMarks,
  LCSynEdit, SynLCHighlighter;

const
  MAX_NIVEL_IDENTACAO = 10;

resourcestring
  rsUnknown = 'Desconhecido';
  rsDescNivelIdentacao = 'Nivel %s';

type
  TLCIdiomaEditor = (ieLCPtBr, ieLCEs);

Const
  LCIdiomasEditor = [Low(TLCIdiomaEditor)..High(TLCIdiomaEditor)];
  LCI18nIdiomasEditor: array[TLCIdiomaEditor] of string = ('pt_br', 'es');

type

  { TLCNiveisIdentacaoConfig }

  TLCNiveisIdentacaoConfig = class(TCollectionItem)
  private
    fAtributos : TSynSelectedColor;
    fDefaultValues : Boolean;
    function GetDescription: String;
    function GetNivel : Integer;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property Description:String read GetDescription;
  published
    property Atributos:TSynSelectedColor read fAtributos write fAtributos;
    property DefaultValues:Boolean read fDefaultValues write fDefaultValues;
    property Nivel:Integer read GetNivel;
  end;
  { TLCElementoSintaxe }

  TLCElementoSintaxe = class(TCollectionItem)
  private
    fAtributos : TSynLCAttributeSettings;
    fDescription : String;
    fKind : TLCTokenKind;
    fDefaultValues : Boolean;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

  published
    property DefaultValues:Boolean read fDefaultValues write fDefaultValues;
    property Description:String read fDescription write fDescription;
    property Kind:TLCTokenKind read fKind write fKind;
    property Atributos:TSynLCAttributeSettings read fAtributos write fAtributos;
  end;

  { TModuloVetorh }

  TModuloVetorh = class(TCollectionItem)
  private
    FArqExc : TStrings;
    fPastaBase : String;
    FSigla : TLCSiglaModuloVetorh;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property PastaBase:String read fPastaBase write fPastaBase;
    property Sigla:TLCSiglaModuloVetorh read FSigla write FSigla;
    property ArquivosExcluidos:TStrings read FArqExc;
  end;

  { TMRUBookmark }

  TMRUBookmark  = class(TCollectionItem)
  private
    FBookmarkNum : integer;
    FColumn : integer;
    FImageIndex : integer;
    FLine : integer;
    FPriority : integer;
    FVisible : boolean;
  published
    property BookmarkNumber: integer read FBookmarkNum write fBookmarkNum;
    property Column: integer read FColumn write FColumn;
    property ImageIndex: integer read FImageIndex write FImageIndex;
    property Line: integer read FLine write FLine;
    property Priority: integer read FPriority write FPriority;
    property Visible: boolean read FVisible write FVisible;
  end;

  { TMRUFile }
  TMRUFile = class(TCollectionItem)
  private
    FBookmarks : TCollection;
    FFixed : Boolean;
    fLastTimeOpened: TDateTime;
    FLeft: integer;
    FTop: Integer;
    FPath: String;
    FName: String;
    FCaret: TPoint;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure GetFromSynEdit(aSynEd:TSynEdit; aSaveBookmarks:Boolean);
    procedure SetToSynEdit(aSynEd:TSynEdit);
    procedure SetPathName(value:String);
    function GetPathName:String;
  published
    property Path:String read FPath write FPath;
    property Name:String read FName write FName;
    property Top:Integer read FTop write FTop;
    property Left:integer read FLeft write FLeft;
    property CaretX:integer read FCaret.x write FCaret.x;
    property CaretY:integer read FCaret.y write FCaret.y;
    property LastTimeOpened:TDateTime read fLastTimeOpened write fLastTimeOpened;
    property Bookmarks:TCollection read FBookmarks;
    property Fixed:Boolean read FFixed write FFixed;
  end;

  { TSessionFile }

  TSessionFile = class(TCollectionItem)
  private
    FActive : Boolean;
    FFileName : String;
  protected
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property FileName:String read FFileName write FFileName;
    property Active:Boolean read FActive write FActive default false;
  end;

  { TGutterPartSettings }

  TGutterPartSettings = class(TPersistent)
  private
    fAutoSize: Boolean;
    fMarkupInfo: TSynSelectedColor;
    fVisible: Boolean;
    fWidth: Integer;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property Visible:Boolean read fVisible write fVisible;
    property AutoSize:Boolean read fAutoSize write fAutoSize;
    property Width:Integer read fWidth write fWidth;
    property MarkupInfo:TSynSelectedColor read fMarkupInfo write fMarkupInfo;
  end;

  { TLineNumberGutterSettings }

  TLineNumberGutterSettings = class(TGutterPartSettings)
  private
    fDigitCount: integer;
    fLeadingZeros: Boolean;
    fShowOnlyLineNumbersMultiplesOf: Integer;
    fZeroStart: Boolean;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property LeadingZeros:Boolean read fLeadingZeros write fLeadingZeros;
    property DigitCount:integer read fDigitCount write fDigitCount;
    property ShowOnlyLineNumbersMultiplesOf:Integer read fShowOnlyLineNumbersMultiplesOf write fShowOnlyLineNumbersMultiplesOf;
    property ZeroStart:Boolean read fZeroStart write fZeroStart;
  end;

  { TChangesPartGutterSettings }

  TChangesPartGutterSettings = class(TGutterPartSettings)
  private
    fModifiedColor: TColor;
    fSavedColor: TColor;
  protected
  public
    constructor  Create; override;
    destructor Destroy; override;
  published
    property ModifiedColor:TColor read fModifiedColor write fModifiedColor;
    property SavedColor:TColor read fSavedColor write fSavedColor;
  end;

  { TGutterSettings }

  TGutterSettings = class(TPersistent)
    private
      fAutoSize: Boolean;
      fChangesPart: TChangesPartGutterSettings;
      fCodeFoldPart: TGutterPartSettings;
      fColor: TColor;
      fLineNumberPart: TLineNumberGutterSettings;
      fSeparatorPart: TGutterPartSettings;
      fVisible: Boolean;
      fWidth: Integer;
    protected
    public
      constructor Create;
      destructor Destroy; override;
    published
      property Visible:Boolean read fVisible write fVisible;
      property AutoSize:Boolean read fAutoSize write fAutoSize;
      property Width:Integer read fWidth write fWidth;
      property Color:TColor read fColor write fColor;

      property ChangesPart:TChangesPartGutterSettings read fChangesPart write fChangesPart;
      property LineNumberPart:TLineNumberGutterSettings read fLineNumberPart write fLineNumberPart;
      property SeparatorPart:TGutterPartSettings read fSeparatorPart write fSeparatorPart;
      property CodeFoldPart:TGutterPartSettings read fCodeFoldPart write fCodeFoldPart;
  end;

  { TMarkupHighlightAllCaretSettings }

  TMarkupHighlightAllCaretSettings = class(TPersistent)
  private
    FFullWord : Boolean;
    FFullWordMaxLen : Integer;
    fMarkupInfo : TSynSelectedColor;
    FTrim : Boolean;
    FWaitTime : Integer;
  protected
  public
    constructor Create;
    destructor Destroy; override;
  published
    property MarkupInfo: TSynSelectedColor read fMarkupInfo write fMarkupInfo;
    property WaitTime: Integer read FWaitTime write FWaitTime;
    property Trim: Boolean read FTrim write FTrim;
    property FullWord: Boolean read FFullWord write FFullWord;
    property FullWordMaxLen: Integer read FFullWordMaxLen write FFullWordMaxLen;
  end;

  { TEditorSettings }

  TEditorSettings = class(TPersistent)
    private
      fCoresPadraoNivelIdentacao: array of TColor;
      fActiveLine: TSynSelectedColor;
      fBracketHighlightStyle: TSynEditBracketHighlightStyle;
      fBracketMatchColor: TSynSelectedColor;
      fElementosSintaxe : TCollection;
      fFontName: String;
      fFontQuality: TFontQuality;
      fFontSize: Integer;
      fGutter: TGutterSettings;
      fLineErrorColor : TSynSelectedColor;
      fMarkupHighlightAllCaret : TMarkupHighlightAllCaretSettings;
      fMaxUndo: Integer;
      fNiveisIdentacao: TCollection;
      FOptions: TSynEditorOptions;
      FOptions2: TSynEditorOptions2;
      fRightEdge: Integer;
      fRightEdgeColor: TColor;
      fTabWidth : Integer;
      fWantTabs: Boolean;
    protected
    public
      constructor Create;
      destructor Destroy; override;
      function GetCorPadraoNivelIdentacao(pNivel:Integer):TColor;
    published
      property ActiveLine: TSynSelectedColor read fActiveLine write fActiveLine;
      property BracketHighlightStyle: TSynEditBracketHighlightStyle read fBracketHighlightStyle write fBracketHighlightStyle;
      property BracketMatchColor: TSynSelectedColor read fBracketMatchColor write fBracketMatchColor;
      property FontName:String read fFontName write fFontName;
      property FontSize:Integer read fFontSize write fFontSize;
      property FontQuality: TFontQuality read fFontQuality write fFontQuality;
      property Gutter:TGutterSettings read fGutter write fGutter;
      property LineErrorColor:TSynSelectedColor read fLineErrorColor write fLineErrorColor;
      property MaxUndo:Integer read fMaxUndo write fMaxUndo;
      property WantTabs:Boolean read fWantTabs write fWantTabs;
      property RightEdge:Integer read fRightEdge write fRightEdge;
      property RightEdgeColor:TColor read fRightEdgeColor write fRightEdgeColor;
      property TabWidth:Integer read fTabWidth write fTabWidth;

      property Options: TSynEditorOptions read FOptions write FOptions;
      property Options2: TSynEditorOptions2 read FOptions2 write FOptions2;

      property MarkupHighlightAllCaret:TMarkupHighlightAllCaretSettings read fMarkupHighlightAllCaret write fMarkupHighlightAllCaret;

      property ElementosSintaxe:TCollection read fElementosSintaxe;
      property NiveisIdentacao:TCollection read fNiveisIdentacao;
  end;

  { TEditorLspSettings }

  TEditorLspSettings = class(TPersistent)
    private
      fCreateEmptyFile : boolean;
      fEditor: TEditorSettings;
      FEnabledAutoSaveFile : Boolean;
      fHeight: Integer;
      fIdioma: TLCIdiomaEditor;
      FIntervalAutoSaveFile : Integer;
      fLeft: Integer;
      fListOfCommandsIndex : Integer;
      fListOfCommandsWidth : Integer;
      FMakeBackupFiles : boolean;
      fMaximized: boolean;
      fMaxMruFilesStored: integer;
      fModulosVetorh : TCollection;
      fMruFiles:TCollection;
      fAskBeforeExit: boolean;
      FNumberOfBackupFilesToPreserve : Integer;
      fPathAppRoot : String;
      fPathConfig : String;
      FPathToBackupFiles : String;
      fSaveBookmarksWhenCloseFile : boolean;
      FSaveSession : Boolean;
      fSessionFiles : TCollection;
      fShowListOfCommands : Boolean;
      fTop: Integer;
      fWidth: Integer;
      fFiltros:TStrings;
      fPathBase:String;
      fShowExplorer:boolean;
      fExplorerWidth:Integer;

      procedure SetIntervalAutoSaveFile(AValue : Integer);
      procedure SetMaxMruFilesStored(AValue: integer);
      procedure SetNumberOfBackupFilesToPreserve(AValue : Integer);
    public
      constructor Create;
      destructor Destroy; override;

      procedure SaveToFile(fileName:String);
      procedure LoadFromFile(fileName:String);

      procedure UpdateSettingsByEditor(SynEd: TSynEdit);

      procedure AddMruFiles(fileName:String; SynEd: TSynEdit);
      function getMruFile(index:Integer):TMRUFile;
      function getMruFile(aFileName:String):TMRUFile;

      function getIdiomaI18n:String;

      // Internal Configurations
      property PathAppRoot:String read fPathAppRoot write fPathAppRoot;
      property PathConfig:String read fPathConfig write fPathConfig;
    published
      // App propertys
      property Idioma:TLCIdiomaEditor read fIdioma write fIdioma;
      property AskBeforeExit:boolean read fAskBeforeExit write fAskBeforeExit;
      property CreateEmptyFile: boolean read fCreateEmptyFile write fCreateEmptyFile;

      // Explorer options
      property Filters:TStrings read fFiltros;
      property PathBase:String read fPathBase write fPathBase;
      property ShowExplorer: Boolean read fShowExplorer write fShowExplorer;
      property ExplorerWidth: Integer read fExplorerWidth write fExplorerWidth;

      // List of Commands Properties
      property ShowListOfCommands: Boolean read fShowListOfCommands write fShowListOfCommands;
      property ListOfCommandsWidth: Integer read fListOfCommandsWidth write fListOfCommandsWidth;
      property ListOfCommandsIndex: Integer read fListOfCommandsIndex write fListOfCommandsIndex;

      // Windows propertys
      property Top:Integer read fTop write fTop;
      property Left:Integer read fLeft write fLeft;
      property Width:Integer read fWidth write fWidth;
      property Height:Integer read fHeight write fHeight;
      property Maximized:boolean read fMaximized write fMaximized;

      // Last open files
      property MaxMruFilesStored:integer read fMaxMruFilesStored write SetMaxMruFilesStored;
      property MruFiles:TCollection read fMruFiles;
      property SaveBookmarksWhenCloseFile:boolean read fSaveBookmarksWhenCloseFile write fSaveBookmarksWhenCloseFile default true;

      // Backup files
      property MakeBackupFiles:boolean read FMakeBackupFiles write FMakeBackupFiles default true;
      property PathToBackupFiles:String read FPathToBackupFiles write FPathToBackupFiles;
      property NumberOfBackupFilesToPreserve:Integer read FNumberOfBackupFilesToPreserve write SetNumberOfBackupFilesToPreserve;

      // session files
      property SaveSession:Boolean read FSaveSession write FSaveSession default false;
      property SessionFiles:TCollection read fSessionFiles;

      // Auto Save
      property EnabledAutoSaveFile:Boolean read FEnabledAutoSaveFile write FEnabledAutoSaveFile default true;
      property IntervalAutoSaveFile:Integer read FIntervalAutoSaveFile write SetIntervalAutoSaveFile default 5; // in minutes

      // Editor
      property Editor:TEditorSettings read fEditor write fEditor;

      // Diversas
      property ModulosVetorh:TCollection read fModulosVetorh;
  end;


implementation

uses
  fpjsonrtti, fpjson, jsonparser,
  SynEditTypes, SynEditMarkupHighAll, LazFileUtils;

{ TLCNiveisIdentacaoConfig }

function TLCNiveisIdentacaoConfig.GetDescription: String;
begin
  Result := Format(rsDescNivelIdentacao, [FormatFloat('#,#00', Nivel+1)]);
end;

function TLCNiveisIdentacaoConfig.GetNivel : Integer;
begin
  Result := Index;
end;

function TLCNiveisIdentacaoConfig.GetDisplayName: string;
begin
  Result:= Description;
end;

constructor TLCNiveisIdentacaoConfig.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fAtributos := TSynSelectedColor.Create;
end;

destructor TLCNiveisIdentacaoConfig.Destroy;
begin
  FreeAndNil(fAtributos);

  inherited Destroy;
end;

{ TLCElementoSintaxe }

function TLCElementoSintaxe.GetDisplayName : string;
begin
  Result := fDescription;
end;

constructor TLCElementoSintaxe.Create(ACollection : TCollection);
begin
  inherited Create(ACollection);
  fDefaultValues:= True;
  fKind := tLCUnknown;
  fDescription := rsUnknown;
  fAtributos := TSynLCAttributeSettings.Create;
end;

destructor TLCElementoSintaxe.Destroy;
begin
  FreeAndNil(fAtributos);

  inherited Destroy;
end;

{ TModuloVetorh }

function TModuloVetorh.GetDisplayName : string;
begin
  Result := AbreviaturaModuloVetorh[FSigla];
end;

constructor TModuloVetorh.Create(ACollection : TCollection);
begin
  inherited Create(ACollection);

  FSigla := smvNone;
  FArqExc := TStringList.Create;
  fPastaBase := '';
end;

destructor TModuloVetorh.Destroy;
begin
  FreeAndNil(FArqExc);
  inherited Destroy;
end;

{ TMarkupHighlightAllCaretSettings }

constructor TMarkupHighlightAllCaretSettings.Create;
begin
  inherited Create;

  fMarkupInfo := TSynSelectedColor.Create;
  fMarkupInfo.Background := $00EEEEEE;
  fMarkupInfo.FrameColor:= clNone;
  fMarkupInfo.Foreground := clNone;

  FFullWord := true;
  FWaitTime := 2000;
  FFullWordMaxLen := 0;
  FTrim := True;
end;

destructor TMarkupHighlightAllCaretSettings.Destroy;
begin
  FreeAndNil(fMarkupInfo);
  inherited Destroy;
end;

{ TSessionFile }

constructor TSessionFile.Create(ACollection : TCollection);
begin
  inherited Create(ACollection);

  FActive := False;
  FFileName := '';
end;

destructor TSessionFile.Destroy;
begin
  inherited Destroy;
end;

{ TChangesPartGutterSettings }

constructor TChangesPartGutterSettings.Create;
begin
  inherited Create;

  fModifiedColor:= $0000E9FC;
  fSavedColor:= clLime;
end;

destructor TChangesPartGutterSettings.Destroy;
begin
  inherited Destroy;
end;

{ TLineNumberGutterSettings }

constructor TLineNumberGutterSettings.Create;
begin
  inherited Create;

  fDigitCount:= 3;
  fLeadingZeros:=true;
  fShowOnlyLineNumbersMultiplesOf:=5;
  fZeroStart:= false;
  MarkupInfo.Background:=12615680;
  MarkupInfo.Foreground:=clLtGray;
end;

destructor TLineNumberGutterSettings.Destroy;
begin
  inherited Destroy;
end;

{ TGutterPartSettings }

constructor TGutterPartSettings.Create;
begin
  inherited Create;
  fMarkupInfo:=TSynSelectedColor.Create;
  fAutoSize:= true;
  fVisible:= true;
  fWidth:= 30;
end;

destructor TGutterPartSettings.Destroy;
begin
  FreeAndNil(fMarkupInfo);

  inherited Destroy;
end;

{ TGutterSettings }

constructor TGutterSettings.Create;
begin
  inherited Create;

  fLineNumberPart:= TLineNumberGutterSettings.Create;
  fSeparatorPart:= TGutterPartSettings.Create;
  fCodeFoldPart:= TGutterPartSettings.Create;
  fChangesPart:= TChangesPartGutterSettings.Create;

  fAutoSize:= true;
  fColor:= 12615680;
  fVisible:= true;
  fWidth:= 30;

  fLineNumberPart.Visible:=true;
  fLineNumberPart.ShowOnlyLineNumbersMultiplesOf:=1;
  fLineNumberPart.AutoSize:=true;
  fLineNumberPart.DigitCount:=3;
  fLineNumberPart.MarkupInfo.Background:=12615680;
  fLineNumberPart.MarkupInfo.Foreground:=clLtGray;
  fLineNumberPart.MarkupInfo.Style:=[];
  fLineNumberPart.LeadingZeros:=true;
  fLineNumberPart.ZeroStart:=false;

  fChangesPart.AutoSize:=false;
  fChangesPart.Width:=2;
  fChangesPart.ModifiedColor:=$0000E9FC;
  fChangesPart.SavedColor:=clLime;

  fSeparatorPart.AutoSize:=false;
  fSeparatorPart.Width:=2;
  fSeparatorPart.MarkupInfo.Background:=12615680;

  fCodeFoldPart.MarkupInfo.Background:=clBtnFace;
end;

destructor TGutterSettings.Destroy;
begin
  FreeAndNil(fLineNumberPart);
  FreeAndNil(fSeparatorPart);
  FreeAndNil(fCodeFoldPart);
  FreeAndNil(fChangesPart);

  inherited Destroy;
end;

{ TEditorSettings }

constructor TEditorSettings.Create;
begin
  SetLength(fCoresPadraoNivelIdentacao, MAX_NIVEL_IDENTACAO);
  fCoresPadraoNivelIdentacao[0] := clRed;
  fCoresPadraoNivelIdentacao[1] := $0098F7; //orange
  fCoresPadraoNivelIdentacao[2] := $22CC40; //green
  fCoresPadraoNivelIdentacao[3] := $CCCC00; //cyan
  fCoresPadraoNivelIdentacao[4] := $FF682A; //blue
  fCoresPadraoNivelIdentacao[5] := $CF00C4; //purple
  fCoresPadraoNivelIdentacao[6] := $C08000;
  fCoresPadraoNivelIdentacao[7] := $408080;
  fCoresPadraoNivelIdentacao[8] := $400080;
  fCoresPadraoNivelIdentacao[9] := $808040;

  inherited Create;

  fGutter:= TGutterSettings.create;
  fActiveLine := TSynSelectedColor.Create;
  fMarkupHighlightAllCaret := TMarkupHighlightAllCaretSettings.Create;
  fElementosSintaxe := TCollection.Create(TLCElementoSintaxe);
  fLineErrorColor := TSynSelectedColor.Create;
  fNiveisIdentacao := TCollection.Create(TLCNiveisIdentacaoConfig);

  fOptions := [eoAltSetsColumnMode
              ,eoAutoIndent
              ,eoBracketHighlight
              ,eoGroupUndo
              ,eoScrollPastEol
              ,eoSmartTabs
              ,eoTabIndent
              ,eoTabsToSpaces
              ,eoTrimTrailingSpaces
              ,eoDragDropEditing
              ,eoEnhanceHomeKey
              ];

  fOptions2 := [eoFoldedCopyPaste,
                eoOverwriteBlock
               ];

  fBracketHighlightStyle:= sbhsBoth;

  fBracketMatchColor:= TSynSelectedColor.Create;
  fBracketMatchColor.Background := clRed;
  fBracketMatchColor.Foreground := clWhite;

  // Linha Ativa
  fActiveLine.Background := $00FFF7E6;
  fActiveLine.Foreground := clNone;
  fActiveLine.FrameColor := $00FFC753;
  fActiveLine.FrameStyle := slsDashed;
  fActiveLine.FrameEdges := sfeAround;

  // Linha com Erro
  fLineErrorColor.Background := $00E5E5E5;
  fLineErrorColor.Foreground := clNone;
  fLineErrorColor.FrameColor := clRed;
  fLineErrorColor.FrameStyle := slsDashed;
  fLineErrorColor.FrameEdges := sfeAround;

  fFontName := 'Courier New';
  fFontSize := 10;
  fFontQuality := fqCleartype;

  fMaxUndo := 1024;
  fWantTabs := true;
  fTabWidth := 2;

  fRightEdge := 80;
  fRightEdgeColor := clLtGray;
end;

destructor TEditorSettings.Destroy;
begin
  FreeAndNil(fActiveLine);
  FreeAndNil(fBracketMatchColor);
  FreeAndNil(fGutter);
  FreeAndNil(fMarkupHighlightAllCaret);
  FreeAndNil(fElementosSintaxe);
  FreeAndNil(fLineErrorColor);
  FreeAndNil(fNiveisIdentacao);

  inherited Destroy;
end;

function TEditorSettings.GetCorPadraoNivelIdentacao(pNivel : Integer) : TColor;
begin
  Result := clNone;
  if  (pNivel >= 0)
  and (pNivel <= High(fCoresPadraoNivelIdentacao)) then
  begin
    Result := fCoresPadraoNivelIdentacao[pNivel];
  end;
end;

{ TEditorLspSettings }

procedure TEditorLspSettings.SetMaxMruFilesStored(AValue: integer);
begin
  if fMaxMruFilesStored = AValue then
  begin
    Exit;
  end;
  // O Limite permitido é 16, pois foi o máximo previsto no editor
  if AValue > 16 then
  begin
    AValue := 16;
  end;

  fMaxMruFilesStored := AValue;

  // Se a quantidade armazenada de arquivo for superior ao novo limite, eliminar os mais antigos
  While (MruFiles.Count > fMaxMruFilesStored) do
  begin
    MruFiles.Delete(MruFiles.Count - 1);
  end;
end;

procedure TEditorLspSettings.SetIntervalAutoSaveFile(AValue : Integer);
begin
  if FIntervalAutoSaveFile = AValue then
  begin
    Exit;
  end;

  if AValue < 1 then
  begin
    AValue := 1;
  end;

  FIntervalAutoSaveFile := AValue;
end;

procedure TEditorLspSettings.SetNumberOfBackupFilesToPreserve(AValue : Integer);
begin
  if FNumberOfBackupFilesToPreserve = AValue then
  begin
    Exit;
  end;

  if AValue < 1 then
  begin
    AValue := 1;
  end
  else
  if AValue > 999 then
  begin
    AValue := 999;
  end;

  FNumberOfBackupFilesToPreserve := AValue;
end;

constructor TEditorLspSettings.Create;
begin
  fMruFiles := TCollection.Create(TMRUFile);
  fSessionFiles := TCollection.Create(TSessionFile);
  fModulosVetorh := TCollection.Create(TModuloVetorh);
  fEditor := TEditorSettings.Create;

  fIdioma := ieLCPtBr;
  fMaxMruFilesStored := 16;
  fHeight := 400;
  fLeft := 0;
  fMaximized:= true;
  fAskBeforeExit:= false;
  fTop:= 0;
  fWidth:= 400;

  fPathAppRoot := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  fPathConfig := IncludeTrailingPathDelimiter(fPathAppRoot + 'config');

  fFiltros := TStringList.Create;
  fPathBase := 'c:\';
  fShowExplorer := true;
  fExplorerWidth := 170;

  fCreateEmptyFile := true;
  fSaveBookmarksWhenCloseFile := true;
  fSaveSession:= false;
  fShowListOfCommands:= false;
  fListOfCommandsWidth := 250;
  fListOfCommandsIndex := 0;

  FPathToBackupFiles := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName) + 'bkp');
  FMakeBackupFiles := true;
  FNumberOfBackupFilesToPreserve := 999;

  FEnabledAutoSaveFile := true;
  FIntervalAutoSaveFile := 5;
end;

destructor TEditorLspSettings.Destroy;
begin
  FreeAndNil(fMruFiles);
  FreeAndNil(fSessionFiles);
  FreeAndNil(fModulosVetorh);
  FreeAndNil(fEditor);
  FreeAndNil(fFiltros);

  inherited Destroy;
end;

procedure TEditorLspSettings.SaveToFile(fileName: String);
var
  Streamer:TJSONStreamer;
  JData : TJSONData;
  Arq:TStrings;
  i:integer;
  elemento: TLCElementoSintaxe;
begin
  i := fEditor.ElementosSintaxe.Count-1;
  while i >= 0 do
  begin
    elemento := TLCElementoSintaxe(fEditor.ElementosSintaxe.Items[i]);
    if elemento.kind in [tLCUnknown, tLCEol] then
    begin
      fEditor.ElementosSintaxe.Delete(i);
    end;
    Dec(i);
  end;

  Arq := TStringList.Create;
  Streamer :=  TJSONStreamer.Create(nil);

  Streamer.Options := Streamer.Options + [jsoTStringsAsArray];
  JData := GetJSON(Streamer.ObjectToJSONString(Self));
  Arq.Text := Jdata.FormatJSON(DefaultFormat,2);
  Arq.SaveToFile(fileName);

  arq.Destroy;
  JData.Destroy;
  Streamer.Destroy;
end;

procedure TEditorLspSettings.LoadFromFile(fileName: String);
var
  DeStreamer:TJSONDeStreamer;
  arq:TStrings;
  i:integer;
  oMru: TMRUFile;
  bAdicionar:Boolean;
  Modulo:TModuloVetorh;
  Sigla: TLCSiglaModuloVetorh;
  kind: TLCTokenKind;
  elemento: TLCElementoSintaxe;
  nivel: TLCNiveisIdentacaoConfig;
begin
  if FileExistsUTF8(fileName) then
  begin
    arq := TStringList.Create;
    DeStreamer := TJSONDeStreamer.Create(nil);

    try
      arq.LoadFromFile(fileName);

      DeStreamer.JSONToObject(arq.Text, self);
    finally
      arq.Destroy;
      DeStreamer.Destroy;
    end;
  end;

  if fFiltros.Count = 0 then
  begin
    fFiltros.Add('*.txt');
    fFiltros.Add('*.sql');
    fFiltros.Add('*.log');
    fFiltros.Add('*.bat');
  end;

  // Verificar se os arquivos da lista MRU existem, caso não existam o mesmo deverá ser eliminado da lista
  i := MruFiles.Count-1;
  while i >= 0 do
  begin
    oMru := TMRUFile(MruFiles.Items[i]);
    if (FileExistsUTF8(oMru.GetPathName) = false) then
    begin
      MruFiles.Delete(i);
    end;
    Dec(i);
  end;

  // Verificar se o limite Arquivos da lista foi atingido, se sim, apagar o mais antigo
  while MruFiles.Count > MaxMruFilesStored do
  begin
    MruFiles.Delete(MruFiles.Count -1);
  end;

  // Criar as configurações para os modulos que não estão gravados ainda
  for sigla in TLCSiglaModuloVetorh do
  begin
    bAdicionar := true;
    for i := 0 to Pred(fModulosVetorh.Count) do
    begin
      if TModuloVetorh(fModulosVetorh.Items[i]).Sigla = sigla then
      begin
        bAdicionar := false;
        break;
      end;
    end;

    if  not (sigla in [smvNone])
    and (bAdicionar = true) then
    begin
      Modulo := TModuloVetorh.Create(fModulosVetorh);
      Modulo.Sigla := sigla;
    end;
  end;

  // Criar os elementos ainda não gravados
  for kind in TLCTokenKind do
  begin
    //TLCTokenKind = (tLCAttributeName, tLCComment, tLCDataType, tLCIdentifier, tLCKey, tLCNull, tLCNumber,
    //              tLCReservedWord, tLCSpace, tLCString, tLCSymbol, tLCUnknown, tLCVariable, tLCEol);
    if not (kind in [tLCNull, tLCUnknown, tLCEol]) then
    begin
      bAdicionar := true;
      for i := 0 to Pred(fEditor.ElementosSintaxe.Count) do
      begin
        if TLCElementoSintaxe(fEditor.ElementosSintaxe.Items[i]).Kind = kind then
        begin
          if TLCElementoSintaxe(fEditor.ElementosSintaxe.Items[i]).Description <> DescricaoTiposToken[Kind] then
          begin
            TLCElementoSintaxe(fEditor.ElementosSintaxe.Items[i]).Description := DescricaoTiposToken[Kind];
          end;

          bAdicionar := false;
          break;
        end;
      end;

      if bAdicionar = true then
      begin
        elemento := TLCElementoSintaxe.Create(fEditor.ElementosSintaxe);
        elemento.Kind := kind;
        elemento.Description := DescricaoTiposToken[Kind];
      end;
    end;
  end;

  if MAX_NIVEL_IDENTACAO > Editor.NiveisIdentacao.Count then
  begin
    for i:= 0 to MAX_NIVEL_IDENTACAO-1 do
    begin
      if i >= Editor.NiveisIdentacao.Count then
      begin
        Nivel := TLCNiveisIdentacaoConfig.Create(Editor.NiveisIdentacao);
        Nivel.DefaultValues := true;
        Nivel.Atributos.Foreground := Editor.GetCorPadraoNivelIdentacao(i);
      end;
    end;
  end;

  if not (fIdioma in LCIdiomasEditor) then
  begin
    fIdioma := ieLCPtBr;
  end;
end;

procedure TEditorLspSettings.UpdateSettingsByEditor(SynEd: TSynEdit);
var
  fSynMarkHAllCaret:TSynEditMarkupHighlightAllCaret;
  Elemento: TLCElementoSintaxe;
  i: integer;
begin
  // Editor
  Editor.WantTabs := SynEd.WantTabs;
  Editor.MaxUndo := SynEd.MaxUndo;

  Editor.RightEdge := SynEd.RightEdge;
  Editor.RightEdgeColor := SynEd.RightEdgeColor;

  Editor.BracketHighlightStyle := SynEd.BracketHighlightStyle;
  Editor.BracketMatchColor.Assign(SynEd.BracketMatchColor);

  Editor.Options := SynEd.Options;
  Editor.Options2 := SynEd.Options2;

  // Linha ativa
  Editor.ActiveLine.Assign(SynEd.LineHighlightColor);

  // Font
  Editor.FontName := SynEd.Font.Name;
  Editor.FontSize := SynEd.Font.Size;
  Editor.FontQuality := SynEd.Font.Quality;

  // Gutter
  Editor.Gutter.Visible := SynEd.Gutter.Visible;
  Editor.Gutter.AutoSize := SynEd.Gutter.AutoSize;
  Editor.Gutter.Width := SynEd.Gutter.Width;
  Editor.Gutter.Color := SynEd.Gutter.Color;

  // Número das linhas do Gutter
  Editor.Gutter.LineNumberPart.Visible := SynEd.Gutter.LineNumberPart(0).Visible;
  Editor.Gutter.LineNumberPart.ShowOnlyLineNumbersMultiplesOf:= SynEd.Gutter.LineNumberPart(0).ShowOnlyLineNumbersMultiplesOf;
  Editor.Gutter.LineNumberPart.AutoSize := SynEd.Gutter.LineNumberPart(0).AutoSize;
  Editor.Gutter.LineNumberPart.DigitCount := SynEd.Gutter.LineNumberPart(0).DigitCount;
  Editor.Gutter.LineNumberPart.MarkupInfo.Assign(SynEd.Gutter.LineNumberPart(0).MarkupInfo);
  Editor.Gutter.LineNumberPart.LeadingZeros := SynEd.Gutter.LineNumberPart(0).LeadingZeros;
  Editor.Gutter.LineNumberPart.ZeroStart := SynEd.Gutter.LineNumberPart(0).ZeroStart;

  Editor.Gutter.ChangesPart.AutoSize := SynEd.Gutter.ChangesPart(0).AutoSize;
  Editor.Gutter.ChangesPart.Width := SynEd.Gutter.ChangesPart(0).Width;
  Editor.Gutter.ChangesPart.ModifiedColor := SynEd.Gutter.ChangesPart(0).ModifiedColor;
  Editor.Gutter.ChangesPart.SavedColor := SynEd.Gutter.ChangesPart(0).SavedColor;

  Editor.Gutter.SeparatorPart.AutoSize := SynEd.Gutter.SeparatorPart(0).AutoSize;
  Editor.Gutter.SeparatorPart.Width := SynEd.Gutter.SeparatorPart(0).Width;
  Editor.Gutter.SeparatorPart.MarkupInfo.Assign(SynEd.Gutter.SeparatorPart(0).MarkupInfo);

  Editor.Gutter.CodeFoldPart.MarkupInfo.Assign(SynEd.Gutter.CodeFoldPart(0).MarkupInfo);

  fSynMarkHAllCaret := TSynEditMarkupHighlightAllCaret(SynEd.MarkupByClass[TSynEditMarkupHighlightAllCaret]);
  if assigned(fSynMarkHAllCaret) then
  begin
    Editor.MarkupHighlightAllCaret.MarkupInfo.Assign(fSynMarkHAllCaret.MarkupInfo);
    Editor.MarkupHighlightAllCaret.Trim := fSynMarkHAllCaret.Trim;
    Editor.MarkupHighlightAllCaret.FullWord := fSynMarkHAllCaret.FullWord;
    Editor.MarkupHighlightAllCaret.WaitTime := fSynMarkHAllCaret.WaitTime;
    Editor.MarkupHighlightAllCaret.FullWordMaxLen := fSynMarkHAllCaret.FullWordMaxLen;
  end;

  For i:= 0 to Editor.ElementosSintaxe.count - 1 do
  begin
    Elemento := TLCElementoSintaxe(Editor.ElementosSintaxe.Items[i]);
    case Elemento.Kind of
      tLCAttributeName: Elemento.Atributos.Assign(TSynLCHighlighter(SynEd.Highlighter).Settings.SettingsToAttributeName);
      tLCComment: Elemento.Atributos.Assign(TSynLCHighlighter(SynEd.Highlighter).Settings.SettingsToComment);
      tLCDataType: Elemento.Atributos.Assign(TSynLCHighlighter(SynEd.Highlighter).Settings.SettingsToDataType);
      tLCIdentifier: Elemento.Atributos.Assign(TSynLCHighlighter(SynEd.Highlighter).Settings.SettingsToIdentifier);
      tLCKey: Elemento.Atributos.Assign(TSynLCHighlighter(SynEd.Highlighter).Settings.SettingsToKey);
      tLCNumber: Elemento.Atributos.Assign(TSynLCHighlighter(SynEd.Highlighter).Settings.SettingsToNumber);
      tLCReservedWord: Elemento.Atributos.Assign(TSynLCHighlighter(SynEd.Highlighter).Settings.SettingsToReservedWord);
      tLCSpace: Elemento.Atributos.Assign(TSynLCHighlighter(SynEd.Highlighter).Settings.SettingsToSpace);
      tLCString: Elemento.Atributos.Assign(TSynLCHighlighter(SynEd.Highlighter).Settings.SettingsToString);
      tLCSymbol: Elemento.Atributos.Assign(TSynLCHighlighter(SynEd.Highlighter).Settings.SettingsToSymbol);
      tLCVariable: Elemento.Atributos.Assign(TSynLCHighlighter(SynEd.Highlighter).Settings.SettingsToVariable);
    end;
  end;
end;

procedure TEditorLspSettings.AddMruFiles(fileName: String; SynEd: TSynEdit);
var
  i : Integer;
  Mru: TMRUFile;
begin
  if FileExistsUTF8(fileName) = false then
  begin
    exit;
  end;

  // Verificar se o arquivo já faz parte da lista
  for i := 0 to Pred(MruFiles.Count) do
  begin
    if AnsiUpperCase(TMRUFile(MruFiles.Items[i]).GetPathName) = AnsiUpperCase(FileName) then
    begin
      getMruFile(i).GetFromSynEdit(SynEd, SaveBookmarksWhenCloseFile);
      getMruFile(i).LastTimeOpened := now;
      MruFiles.Items[i].Index := 0;

      exit;
    end;
  end;

  // Adicionar o arquivo na lista
  mru := TMRUFile(MruFiles.Insert(0));
  Mru.GetFromSynEdit(SynEd, SaveBookmarksWhenCloseFile);
  Mru.SetPathName(FileName);
  Mru.LastTimeOpened := now;

  // Verificar se o limite Arquivos da lista foi atingido, se sim, apagar o mais antigo
  while MruFiles.Count > MaxMruFilesStored do
  begin
    MruFiles.Delete(MruFiles.Count -1);
  end;
end;

function TEditorLspSettings.getMruFile(index: Integer): TMRUFile;
begin
  Result := TMRUFile(Self.MruFiles.Items[index]);
end;

function TEditorLspSettings.getMruFile(aFileName : String) : TMRUFile;
var
  i : integer;
begin
  result := nil;
  For i:=0 to Self.MruFiles.Count - 1 do
  begin
    if (aFileName = TMRUFile(Self.MruFiles.Items[i]).GetPathName) then
    begin
      result := TMRUFile(Self.MruFiles.Items[i]);
      exit;
    end;
  end;
end;

function TEditorLspSettings.getIdiomaI18n : String;
begin
  Result := LCI18nIdiomasEditor[ieLCPtBr];
  if  (fIdioma in (LCIdiomasEditor - [ieLCPtBr])) then
  begin
    Result := LCI18nIdiomasEditor[fIdioma];
  end;
end;

{ TMRUFile }

function TMRUFile.GetDisplayName : string;
begin
  Result := GetPathName;
end;

constructor TMRUFile.Create(ACollection : TCollection);
begin
  inherited Create(ACollection);
  fBookmarks := TCollection.Create(TMRUBookmark);
  FFixed := false;
  FTop := 0;
  FLeft := 0;
  FCaret.X := 0;
  FCaret.Y := 0;
  FPath := '';
  FName := '';
end;

destructor TMRUFile.Destroy;
begin
  FreeAndNil(fBookmarks);
  inherited Destroy;
end;

procedure TMRUFile.GetFromSynEdit(aSynEd : TSynEdit; aSaveBookmarks : Boolean);
var
  i : integer;
  oMRUBookmark : TMRUBookmark;
begin
  FTop := aSynEd.TopLine;
  FLeft := aSynEd.LeftChar;
  FCaret.X := aSynEd.CaretX;
  FCaret.Y := aSynEd.CaretY;
  fBookmarks.Clear;

  if aSaveBookmarks = true then
  begin
    For i:= 0 to aSynEd.Marks.Count - 1 do
    begin
      oMRUBookmark := TMRUBookmark(fBookmarks.Add);
      oMRUBookmark.BookmarkNumber := aSynEd.Marks[i].BookmarkNumber;
      oMRUBookmark.Column := aSynEd.Marks[i].Column;
      oMRUBookmark.ImageIndex := aSynEd.Marks[i].ImageIndex;
      oMRUBookmark.Line := aSynEd.Marks[i].Line;
      oMRUBookmark.Priority := aSynEd.Marks[i].Priority;
      oMRUBookmark.Visible := aSynEd.Marks[i].Visible;
    end;
  end;
end;

procedure TMRUFile.SetToSynEdit(aSynEd: TSynEdit);
var
  i : integer;
  oMRUBookmark : TMRUBookmark;
  oMark : TSynEditMark;
begin
  aSynEd.TopLine := FTop;
  aSynEd.LeftChar := FLeft;
  aSynEd.CaretX := FCaret.X;
  aSynEd.CaretY := FCaret.Y;

  for i:= 0 to fBookmarks.Count - 1 do
  begin
    oMRUBookmark := TMRUBookmark(fBookmarks.Items[i]);

    if aSynEd.IsBookmark(oMRUBookmark.BookmarkNumber) = false then
    begin
      oMark := TSynEditMark.Create(aSynEd);
      oMark.BookmarkNumber := oMRUBookmark.BookmarkNumber;
      oMark.ImageIndex := oMRUBookmark.ImageIndex;
      oMark.Column := oMRUBookmark.Column;
      oMark.Line := oMRUBookmark.Line;
      oMark.Priority := oMRUBookmark.Priority;
      oMark.Visible := oMRUBookmark.Visible;

      aSynEd.Marks.Add(oMark);
    end;
  end;
end;

procedure TMRUFile.SetPathName(value: String);
begin
  FName := ExtractFileName(value);
  FPath := ExtractFilePath(value);
end;

function TMRUFile.GetPathName: String;
begin
  Result := FPath + FName;
end;

end.


unit SynLCHighlighter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LCLProc,
  SynEditHighlighter, SynEditHighlighterFoldBase, SynEditTypes;

type

  TLCTokenKind = (tLCAttributeName, tLCComment, tLCDataType, tLCIdentifier, tLCKey, tLCNull, tLCNumber,
                  tLCReservedWord, tLCSpace, tLCString, tLCSymbol, tLCUnknown, tLCVariable, tLCEol,
                  tLCCustomFunction);

Const
  DescricaoTiposToken: Array[TLCTokenKind] of string = ('Atributos', 'Comentários', 'Tipo Dados',
                                                        'Identificador', 'Comandos', 'Nulo', 'Números',
                                                        'Funções Programador', 'Espaço', 'Texto', 'Símbolos',
                                                        'Desconhecidos', 'Variáveis', 'Final da Linha',
                                                        'Funções Customizadas');

type

  TLCRangeState = (rsLCAttrName, rsLCComment, rsLCString, rsLCUnknown);
  TLCRangeStates = set of TLCRangeState;

  TLCCodeFoldBlockType = (
    cfbtLCInicioFim,
    cfbtLCCommentMultiLine,
    // internal type / no config
    cfbtLCUnknown
    );
  TLCCodeFoldBlockTypes = set of TLCCodeFoldBlockType;

  TLCTokenDef = record
    Texto : string;
    kind : TLCTokenKind;
  end;

  TLCArrayTokenDef = array of TLCTokenDef;

  TPtrLCArrayTokenDef = ^TLCArrayTokenDef;
  TPtrLCTokenDef = ^TLCTokenDef;


const
  CountLCCodeFoldBlockOffset: Pointer = Pointer(PtrInt(Integer(high(TLCCodeFoldBlockType))+1));

Type

  { TSynLCAttributeSettings }

  TSynLCAttributeSettings = class(TPersistent)
  private
    FBackground: TColor;
    FForeground: TColor;
    FFrameColor: TColor;
    FFrameEdges: TSynFrameEdges;
    FFrameStyle: TSynLineStyle;
    fStyle: TFontStyles;
    fStyleMask: TFontStyles;
  protected

  public
    constructor Create; virtual;

    procedure Assign(aSource: TPersistent); override;
  published
    property Background: TColor read FBackground write FBackground;
    property Foreground: TColor read FForeground write FForeground;
    property FrameColor: TColor read FFrameColor write FFrameColor;
    property FrameStyle: TSynLineStyle  read FFrameStyle write FFrameStyle;
    property FrameEdges: TSynFrameEdges read FFrameEdges write FFrameEdges;
    property Style: TFontStyles     read fStyle write fStyle;
    property StyleMask: TFontStyles read fStyleMask write fStyleMask;
  end;

  { TSynLCAttributeWithWordsSettings }

  TSynLCAttributeWithWordsSettings = class(TSynLCAttributeSettings)
  private
    fWords: TStringList;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Words:TStringList read fWords write fWords;
  end;

  { TSynLCHighlighterSettings }

  TSynLCHighlighterSettings = class(TPersistent)
  private
    fCaseSensitive: Boolean;
    fSettingsToAttributeName: TSynLCAttributeWithWordsSettings;
    fSettingsToComment: TSynLCAttributeSettings;
    fSettingsToCustomFunction: TSynLCAttributeWithWordsSettings;
    fSettingsToDataType: TSynLCAttributeWithWordsSettings;
    fSettingsToIdentifier: TSynLCAttributeSettings;
    fSettingsToKey: TSynLCAttributeWithWordsSettings;
    fSettingsToNumber: TSynLCAttributeSettings;
    fSettingsToReservedWord: TSynLCAttributeWithWordsSettings;
    fSettingsToSpace: TSynLCAttributeSettings;
    fSettingsToString: TSynLCAttributeSettings;
    fSettingsToSymbol: TSynLCAttributeSettings;
    fSettingsToVariable: TSynLCAttributeWithWordsSettings;
    procedure SetCaseSensitive(AValue: Boolean);

  protected
    procedure CreateDefaultSettings;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveToFile(fileName:String);
    procedure LoadFromFile(fileName:String);
  published
    property CaseSensitive:Boolean read fCaseSensitive write SetCaseSensitive;
    Property SettingsToComment: TSynLCAttributeSettings read fSettingsToComment write fSettingsToComment;
    Property SettingsToIdentifier: TSynLCAttributeSettings read fSettingsToIdentifier write fSettingsToIdentifier;
    Property SettingsToCustomFunction: TSynLCAttributeWithWordsSettings read fSettingsToCustomFunction write fSettingsToCustomFunction;
    Property SettingsToDataType: TSynLCAttributeWithWordsSettings read fSettingsToDataType write fSettingsToDataType;
    Property SettingsToKey: TSynLCAttributeWithWordsSettings read fSettingsToKey write fSettingsToKey;
    Property SettingsToReservedWord: TSynLCAttributeWithWordsSettings read fSettingsToReservedWord write fSettingsToReservedWord;
    Property SettingsToNumber: TSynLCAttributeSettings read fSettingsToNumber write fSettingsToNumber;
    Property SettingsToSpace: TSynLCAttributeSettings read fSettingsToSpace write fSettingsToSpace;
    Property SettingsToString: TSynLCAttributeSettings read fSettingsToString write fSettingsToString;
    Property SettingsToSymbol: TSynLCAttributeSettings read fSettingsToSymbol write fSettingsToSymbol;
    Property SettingsToVariable: TSynLCAttributeWithWordsSettings read fSettingsToVariable write fSettingsToVariable;
    Property SettingsToAttributeName: TSynLCAttributeWithWordsSettings read fSettingsToAttributeName write fSettingsToAttributeName;
  end;

  TLCOnGetCustomIdentKind = procedure(Sender: TObject; Const pToken:String; var pIdentKind:TLCTokenKind) of object;


  { TSynLCHighlighter }

  TSynLCHighlighter = class(TSynCustomFoldHighlighter)
  private
    fOnGetCustomIdentKind : TLCOnGetCustomIdentKind;
    fSettings:TSynLCHighlighterSettings;

    fIdentifiers: array[#0..#255] of ByteBool;
    fNumberChar: array[char] of Boolean;
    fSpaceChar: array[char] of Boolean;
    fPCharCurrentLine : PChar;
    fNumberCurrentLine : integer;
    fSizeCurrentLine : integer;
    fRanges: TLCRangeStates;

    fPositionInLine: LongInt;// current parser postion in fTextCurrentLine
    fTokenStart: integer;// start of current token in fTextCurrentLine
    fTokenKind: TLCTokenKind;
    FAtLineStart: Boolean;

    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fDataTypeAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fReservedWordAttri: TSynHighlighterAttributes;
    fCustomFunctionWordAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    fAttributeNameAttri: TSynHighlighterAttributes;

    atdA, atdB, atdC, atdD, atdE, atdF, atdG, atdH, atdI, atdJ,
    atdK, atdL, atdM, atdN, atdO, atdP, atdQ, atdR, atdS, atdT,
    atdU, atdV, atdW, atdX, atdY, atdZ:  TLCArrayTokenDef;

    procedure GetArrayToToken(aToken:String; out aArray:TPtrLCArrayTokenDef);
    procedure ResetArrayOfTokenDef;
    function GetKindOfToken(aToken:String; const aDefaultKind: TLCTokenKind;
                                           const aKindExpected: TLCTokenKind = tLCUnknown): TLCTokenKind;

    procedure CreateDefaultAttributes;
    procedure DefineDefaultValidCaracters;

    function IsLineEnd(pPosition: Integer): Boolean;

    procedure CommentMultiLineProc;
    procedure CommentInLineProc;
    procedure CRProc;
    procedure IdentProc(isAttr:Boolean = false);
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PointProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure StringMutlipeLinesProc;
    procedure SymbolProc;
    procedure UnknownProc;
  protected

    // folding
    function StartLCCodeFoldBlock(ABlockType: TLCCodeFoldBlockType): TSynCustomCodeFoldBlock;
    procedure EndLCCodeFoldBlock;
    function CurrentLCCodeFoldBlockType: TLCCodeFoldBlockType;

    procedure DoInitNode(var Node: TSynFoldNodeInfo;
                             FinishingABlock: Boolean;
                             ABlockType: Pointer; aActions: TSynFoldActions;
                             AIsFold: Boolean); override;

    // Fold Config
    function GetFoldConfigInstance(Index: Integer): TSynCustomFoldConfig; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function IdentKind(isAttr:Boolean = false; pToken:String = ''): TLCTokenKind;

    procedure SaveSettingsToFile(fileName:String);
    procedure LoadSettingsFromFile(fileName:String);

    procedure PopularArrayOfTokenDef;
    procedure UpdateAttributesBySettings;

    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;

    function GetEol: Boolean; override;
    function GetToken: String; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenPos: Integer; override;
    function GetTokenKind: integer; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;

    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function GetRange: Pointer; override;

    property Settings:TSynLCHighlighterSettings read fSettings;
    property onGetCustomIdentKind:TLCOnGetCustomIdentKind read fOnGetCustomIdentKind write fOnGetCustomIdentKind;
  published
  end;

  //procedure Registre;

implementation

uses
  fpjsonrtti, fpjson,
  SynEditStrConst;

{ TSynLCHighlighterSettings }

procedure TSynLCHighlighterSettings.SetCaseSensitive(AValue: Boolean);
begin
  if fCaseSensitive = AValue then
  begin
    Exit;
  end;
  fCaseSensitive := AValue;

  // Set the lists of work the casesensitive property
  fSettingsToAttributeName.Words.CaseSensitive := fCaseSensitive;
  fSettingsToDataType.Words.CaseSensitive := fCaseSensitive;
  fSettingsToKey.Words.CaseSensitive := fCaseSensitive;
  fSettingsToReservedWord.Words.CaseSensitive := fCaseSensitive;
  fSettingsToCustomFunction.Words.CaseSensitive := fCaseSensitive;
  fSettingsToVariable.Words.CaseSensitive := fCaseSensitive;

end;

procedure TSynLCHighlighterSettings.CreateDefaultSettings;
begin
  fSettingsToAttributeName:= TSynLCAttributeWithWordsSettings.Create;
  fSettingsToAttributeName.Style:= [fsBold];
  fSettingsToAttributeName.Foreground:=clOlive;

  fSettingsToComment:= TSynLCAttributeSettings.Create;
  fSettingsToComment.Style:= [fsBold];
  fSettingsToComment.Foreground:=clTeal;

  fSettingsToCustomFunction:= TSynLCAttributeWithWordsSettings.Create;
  fSettingsToCustomFunction.Foreground:= $FF8000;
  fSettingsToCustomFunction.Style:= [fsBold];

  fSettingsToDataType:= TSynLCAttributeWithWordsSettings.Create;
  fSettingsToDataType.Style:= [fsBold];
  fSettingsToDataType.Foreground:=clPurple;

  fSettingsToIdentifier:= TSynLCAttributeSettings.Create;
  fSettingsToIdentifier.Foreground:=clNavy;

  fSettingsToKey:= TSynLCAttributeWithWordsSettings.Create;
  fSettingsToKey.Style:= [fsBold];
  fSettingsToKey.Foreground:=clBlack;

  fSettingsToNumber:= TSynLCAttributeSettings.Create;
  fSettingsToNumber.Foreground:=clGreen;

  fSettingsToReservedWord:= TSynLCAttributeWithWordsSettings.Create;
  fSettingsToReservedWord.Foreground:=clNavy;
  fSettingsToReservedWord.Style:= [fsBold];

  fSettingsToSpace:= TSynLCAttributeSettings.Create;

  fSettingsToString:= TSynLCAttributeSettings.Create;
  fSettingsToString.Foreground:=clBlue;

  fSettingsToSymbol:= TSynLCAttributeSettings.Create;
  fSettingsToSymbol.Foreground:=clMaroon;

  fSettingsToVariable:= TSynLCAttributeWithWordsSettings.Create;
  fSettingsToVariable.Style:= [];
  fSettingsToVariable.Foreground:=clRed;
end;

constructor TSynLCHighlighterSettings.Create;
begin
  CreateDefaultSettings;
  CaseSensitive := false;
end;

destructor TSynLCHighlighterSettings.Destroy;
begin
  fSettingsToAttributeName.Destroy;
  fSettingsToComment.Destroy;
  fSettingsToDataType.Destroy;
  fSettingsToIdentifier.Destroy;
  fSettingsToKey.Destroy;
  fSettingsToNumber.Destroy;
  fSettingsToReservedWord.Destroy;
  fSettingsToSpace.Destroy;
  fSettingsToString.Destroy;
  fSettingsToSymbol.Destroy;
  fSettingsToVariable.Destroy;
  fSettingsToCustomFunction.Destroy;

  inherited Destroy;
end;

procedure TSynLCHighlighterSettings.SaveToFile(fileName: String);
var
  Streamer:TJSONStreamer;
  JData : TJSONData;
  Arq:TStrings;
begin
   Arq := TStringList.Create;
   try
     Streamer :=  TJSONStreamer.Create(nil);
     try
       Streamer.Options := Streamer.Options + [jsoTStringsAsArray];
       JData := GetJSON(Streamer.ObjectToJSONString(Self));
       try
         Arq.Text := Jdata.FormatJSON(DefaultFormat,2);
         Arq.SaveToFile(fileName);
       finally
         JData.Destroy;
       end;
     finally
       Streamer.Destroy;
     end;
   finally
     arq.Destroy;
   end;
end;

procedure TSynLCHighlighterSettings.LoadFromFile(fileName: String);
var
  DeStreamer:TJSONDeStreamer;
  arq:TStrings;
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

{ TSynLCAttributeWithWordsSettings }

constructor TSynLCAttributeWithWordsSettings.Create;
begin
  inherited Create;

  fWords := TStringList.Create;
  fWords.CaseSensitive := false;
  fWords.Sorted := true;
end;

destructor TSynLCAttributeWithWordsSettings.Destroy;
begin
  FreeAndNil(fWords);

  inherited Destroy;
end;

{ TSynLCAttributeSettings }

constructor TSynLCAttributeSettings.Create;
begin
  FBackground:= clNone;
  FForeground:= clNone;
  FFrameColor:= clNone;
  FFrameEdges:= sfeNone;
  FFrameStyle:= slsSolid;
  fStyle:= [];
  fStyleMask:= [];
end;

procedure TSynLCAttributeSettings.Assign(aSource : TPersistent);
var
  Source: TSynLCAttributeSettings;
begin
  if  Assigned(aSource)
  and (aSource is TSynLCAttributeSettings) then
  begin
    Source := TSynLCAttributeSettings(aSource);
    Background := Source.Background;
    Foreground := Source.Foreground;
    FrameColor := Source.FrameColor;
    FrameStyle := Source.FrameStyle;
    FrameEdges := Source.FrameEdges;
    Style      := Source.FStyle;
    StyleMask  := Source.FStyleMask;
  end
  else
  begin
    inherited;
  end;
end;

{ TSynLCHighlighter }

procedure TSynLCHighlighter.NullProc;
begin
  fTokenKind := tLCNull;
  if fPositionInLine < fSizeCurrentLine  then
  begin
    inc(fPositionInLine);
  end;
end;

procedure TSynLCHighlighter.NumberProc;
begin
  inc(fPositionInLine);
  fTokenKind := tLCNumber;
  if fPositionInLine < fSizeCurrentLine then
  begin
    while (fNumberChar[fPCharCurrentLine[fPositionInLine]]) do
    begin
      inc(fPositionInLine);
    end;
    if  (fPCharCurrentLine[fPositionInLine] = '.')
    and (fPCharCurrentLine[fPositionInLine + 1] <> '.')  then
    begin
      inc(fPositionInLine);
      while (fNumberChar[fPCharCurrentLine[fPositionInLine]]) do
      begin
        inc(fPositionInLine);
      end;
    end;
  end;
end;

procedure TSynLCHighlighter.PointProc;
begin
  fTokenKind := tLCSymbol;
  inc(fPositionInLine);
  if (fIdentifiers[fPCharCurrentLine[fPositionInLine]] = true) then
  begin
    fRanges := fRanges + [rsLCAttrName];
  end;
end;

procedure TSynLCHighlighter.SpaceProc;
begin
  inc(fPositionInLine);
  fTokenKind := tLCSpace;
  while fSpaceChar[fPCharCurrentLine[fPositionInLine]] do
  begin
    inc(fPositionInLine);
  end;
end;

procedure TSynLCHighlighter.StringProc;
begin
  fTokenKind := tLCString;
  fRanges := fRanges - [rsLCString];

  Inc(fPositionInLine);
  While (IsLineEnd(fPositionInLine) = false) do
  begin
    case (fPCharCurrentLine[fPositionInLine]) of
      #34:
        begin
          Inc(fPositionInLine);
          if (fPCharCurrentLine[fPositionInLine] = #34) then
          begin
            Inc(fPositionInLine);
          end
          else
          begin
            break;
          end;
        end;
      #92:
      begin
        Inc(fPositionInLine);
        if not (fPCharCurrentLine[fPositionInLine] in [#34, #92]) then   // \" \\
        begin
          fRanges := fRanges + [rsLCString];
          break;
        end;
        Inc(fPositionInLine);
      end;
      else
      begin
        Inc(fPositionInLine);
      end;
    end; // case
  end; // while
end;

procedure TSynLCHighlighter.StringMutlipeLinesProc;
begin
  fTokenKind := tLCString;

  while IsLineEnd(fPositionInLine) = false do
  begin
    case fPCharCurrentLine[fPositionInLine] of
      #34:  // #34 = "
      begin
        if (fPCharCurrentLine[fPositionInLine + 1] = #34) then
        begin
          Inc(fPositionInLine, 2);
        end
        else
        begin
          inc(fPositionInLine);
          fRanges := fRanges - [rsLCString];
          break;
        end;
      end;
      #92: // #92 = \
      begin
        Inc(fPositionInLine);
        if (fPCharCurrentLine[fPositionInLine] in [#34, #92]) then   // \" \\
        begin
          Inc(fPositionInLine);
        end;
      end;
      else
      begin
        inc(fPositionInLine);
      end;
    end; // case
  end; // while
end;

procedure TSynLCHighlighter.SymbolProc;
begin
  inc(fPositionInLine);
  fTokenKind := tLCSymbol;
end;

procedure TSynLCHighlighter.UnknownProc;
begin
  inc(fPositionInLine);
  while (fPCharCurrentLine[fPositionInLine] in [#128..#191]) // continued utf8 subcode
  or    (fPCharCurrentLine[fPositionInLine] <> #0) do
  begin
    inc(fPositionInLine);
  end;
  fTokenKind := tLCUnknown;
end;

procedure TSynLCHighlighter.UpdateAttributesBySettings;
begin
  fCommentAttri.Background:=fSettings.SettingsToComment.Background;
  fCommentAttri.Foreground:=fSettings.SettingsToComment.Foreground;
  fCommentAttri.FrameColor:=fSettings.SettingsToComment.FrameColor;
  fCommentAttri.FrameStyle:=fSettings.SettingsToComment.FrameStyle;
  fCommentAttri.FrameEdges:=fSettings.SettingsToComment.FrameEdges;
  fCommentAttri.Style:=fSettings.SettingsToComment.Style;
  fCommentAttri.StyleMask:=fSettings.SettingsToComment.StyleMask;

  fIdentifierAttri.Background:=fSettings.SettingsToIdentifier.Background;
  fIdentifierAttri.Foreground:=fSettings.SettingsToIdentifier.Foreground;
  fIdentifierAttri.FrameColor:=fSettings.SettingsToIdentifier.FrameColor;
  fIdentifierAttri.FrameStyle:=fSettings.SettingsToIdentifier.FrameStyle;
  fIdentifierAttri.FrameEdges:=fSettings.SettingsToIdentifier.FrameEdges;
  fIdentifierAttri.Style:=fSettings.SettingsToIdentifier.Style;
  fIdentifierAttri.StyleMask:=fSettings.SettingsToIdentifier.StyleMask;

  fReservedWordAttri.Background:=fSettings.SettingsToReservedWord.Background;
  fReservedWordAttri.Foreground:=fSettings.SettingsToReservedWord.Foreground;
  fReservedWordAttri.FrameColor:=fSettings.SettingsToReservedWord.FrameColor;
  fReservedWordAttri.FrameStyle:=fSettings.SettingsToReservedWord.FrameStyle;
  fReservedWordAttri.FrameEdges:=fSettings.SettingsToReservedWord.FrameEdges;
  fReservedWordAttri.Style:=fSettings.SettingsToReservedWord.Style;
  fReservedWordAttri.StyleMask:=fSettings.SettingsToReservedWord.StyleMask;
  fSettings.SettingsToReservedWord.Words.Sort;
  fSettings.SettingsToReservedWord.Words.Sorted := true;

  fCustomFunctionWordAttri.Background:=fSettings.SettingsToCustomFunction.Background;
  fCustomFunctionWordAttri.Foreground:=fSettings.SettingsToCustomFunction.Foreground;
  fCustomFunctionWordAttri.FrameColor:=fSettings.SettingsToCustomFunction.FrameColor;
  fCustomFunctionWordAttri.FrameStyle:=fSettings.SettingsToCustomFunction.FrameStyle;
  fCustomFunctionWordAttri.FrameEdges:=fSettings.SettingsToCustomFunction.FrameEdges;
  fCustomFunctionWordAttri.Style:=fSettings.SettingsToCustomFunction.Style;
  fCustomFunctionWordAttri.StyleMask:=fSettings.SettingsToCustomFunction.StyleMask;
  fSettings.SettingsToCustomFunction.Words.Sort;
  fSettings.SettingsToCustomFunction.Words.Sorted := true;

  fDataTypeAttri.Background:=fSettings.SettingsToDataType.Background;
  fDataTypeAttri.Foreground:=fSettings.SettingsToDataType.Foreground;
  fDataTypeAttri.FrameColor:=fSettings.SettingsToDataType.FrameColor;
  fDataTypeAttri.FrameStyle:=fSettings.SettingsToDataType.FrameStyle;
  fDataTypeAttri.FrameEdges:=fSettings.SettingsToDataType.FrameEdges;
  fDataTypeAttri.Style:=fSettings.SettingsToDataType.Style;
  fDataTypeAttri.StyleMask:=fSettings.SettingsToDataType.StyleMask;
  fSettings.SettingsToDataType.Words.Sort;
  fSettings.SettingsToDataType.Words.Sorted := true;

  fKeyAttri.Background:=fSettings.SettingsToKey.Background;
  fKeyAttri.Foreground:=fSettings.SettingsToKey.Foreground;
  fKeyAttri.FrameColor:=fSettings.SettingsToKey.FrameColor;
  fKeyAttri.FrameStyle:=fSettings.SettingsToKey.FrameStyle;
  fKeyAttri.FrameEdges:=fSettings.SettingsToKey.FrameEdges;
  fKeyAttri.Style:=fSettings.SettingsToKey.Style;
  fKeyAttri.StyleMask:=fSettings.SettingsToKey.StyleMask;
  fSettings.SettingsToKey.Words.Sort;
  fSettings.SettingsToKey.Words.Sorted := true;

  fVariableAttri.Background:=fSettings.SettingsToVariable.Background;
  fVariableAttri.Foreground:=fSettings.SettingsToVariable.Foreground;
  fVariableAttri.FrameColor:=fSettings.SettingsToVariable.FrameColor;
  fVariableAttri.FrameStyle:=fSettings.SettingsToVariable.FrameStyle;
  fVariableAttri.FrameEdges:=fSettings.SettingsToVariable.FrameEdges;
  fVariableAttri.Style:=fSettings.SettingsToVariable.Style;
  fVariableAttri.StyleMask:=fSettings.SettingsToVariable.StyleMask;
  fSettings.SettingsToVariable.Words.Sort;
  fSettings.SettingsToVariable.Words.Sorted := true;

  fAttributeNameAttri.Background:=fSettings.SettingsToAttributeName.Background;
  fAttributeNameAttri.Foreground:=fSettings.SettingsToAttributeName.Foreground;
  fAttributeNameAttri.FrameColor:=fSettings.SettingsToAttributeName.FrameColor;
  fAttributeNameAttri.FrameStyle:=fSettings.SettingsToAttributeName.FrameStyle;
  fAttributeNameAttri.FrameEdges:=fSettings.SettingsToAttributeName.FrameEdges;
  fAttributeNameAttri.Style:=fSettings.SettingsToAttributeName.Style;
  fAttributeNameAttri.StyleMask:=fSettings.SettingsToAttributeName.StyleMask;
  fSettings.SettingsToAttributeName.Words.Sort;
  fSettings.SettingsToAttributeName.Words.Sorted := true;

  fNumberAttri.Background:=fSettings.SettingsToNumber.Background;
  fNumberAttri.Foreground:=fSettings.SettingsToNumber.Foreground;
  fNumberAttri.FrameColor:=fSettings.SettingsToNumber.FrameColor;
  fNumberAttri.FrameStyle:=fSettings.SettingsToNumber.FrameStyle;
  fNumberAttri.FrameEdges:=fSettings.SettingsToNumber.FrameEdges;
  fNumberAttri.Style:=fSettings.SettingsToNumber.Style;
  fNumberAttri.StyleMask:=fSettings.SettingsToNumber.StyleMask;

  fSpaceAttri.Background:=fSettings.SettingsToSpace.Background;
  fSpaceAttri.Foreground:=fSettings.SettingsToSpace.Foreground;
  fSpaceAttri.FrameColor:=fSettings.SettingsToSpace.FrameColor;
  fSpaceAttri.FrameStyle:=fSettings.SettingsToSpace.FrameStyle;
  fSpaceAttri.FrameEdges:=fSettings.SettingsToSpace.FrameEdges;
  fSpaceAttri.Style:=fSettings.SettingsToSpace.Style;
  fSpaceAttri.StyleMask:=fSettings.SettingsToSpace.StyleMask;

  fStringAttri.Background:=fSettings.SettingsToString.Background;
  fStringAttri.Foreground:=fSettings.SettingsToString.Foreground;
  fStringAttri.FrameColor:=fSettings.SettingsToString.FrameColor;
  fStringAttri.FrameStyle:=fSettings.SettingsToString.FrameStyle;
  fStringAttri.FrameEdges:=fSettings.SettingsToString.FrameEdges;
  fStringAttri.Style:=fSettings.SettingsToString.Style;
  fStringAttri.StyleMask:=fSettings.SettingsToString.StyleMask;

  fSymbolAttri.Background:=fSettings.SettingsToSymbol.Background;
  fSymbolAttri.Foreground:=fSettings.SettingsToSymbol.Foreground;
  fSymbolAttri.FrameColor:=fSettings.SettingsToSymbol.FrameColor;
  fSymbolAttri.FrameStyle:=fSettings.SettingsToSymbol.FrameStyle;
  fSymbolAttri.FrameEdges:=fSettings.SettingsToSymbol.FrameEdges;
  fSymbolAttri.Style:=fSettings.SettingsToSymbol.Style;
  fSymbolAttri.StyleMask:=fSettings.SettingsToSymbol.StyleMask;
end;

function TSynLCHighlighter.StartLCCodeFoldBlock(ABlockType: TLCCodeFoldBlockType): TSynCustomCodeFoldBlock;
begin
  {$PUSH} {$Warnings OFF} {$Hints OFF}
  Result:=StartCodeFoldBlock(Pointer(PtrInt(ABlockType)) );
  {$POP}
end;

procedure TSynLCHighlighter.EndLCCodeFoldBlock;
begin
  EndCodeFoldBlock(True);
end;

function TSynLCHighlighter.CurrentLCCodeFoldBlockType : TLCCodeFoldBlockType;
var p : pointer;
begin
  result := cfbtLCUnknown;
  p := TopCodeFoldBlockType(0);
  if p <> nil then
  begin
    {$PUSH} {$Warnings OFF} {$Hints OFF}
      result := TLCCodeFoldBlockType(PtrUInt(p));
    {$POP}
  end;
end;

procedure TSynLCHighlighter.DoInitNode(var Node: TSynFoldNodeInfo;
  FinishingABlock: Boolean; ABlockType: Pointer; aActions: TSynFoldActions;
  AIsFold: Boolean);
begin
  inherited DoInitNode(Node, FinishingABlock, ABlockType, aActions, AIsFold);
  
  if not FAtLineStart then
  begin
    Node.FoldAction := Node.FoldAction - [sfaFoldHide];
  end;

  {$PUSH} {$Warnings OFF} {$Hints OFF}
    if  (ABlockType <> nil)
    and (TLCCodeFoldBlockType(PtrUInt(ABlockType)) = cfbtLCInicioFim) then
    begin
      Include( Node.FoldAction, sfaOutlineKeepLevel);
    end;
  {$POP}
end;

procedure TSynLCHighlighter.GetArrayToToken(aToken : String; out aArray : TPtrLCArrayTokenDef);
var
  sValue : string;
begin
  sValue := aToken[1];
  aArray := nil;

  case UpperCase(sValue) of
    'A': begin aArray := @atdA; end;
    'B': begin aArray := @atdB; end;
    'C': begin aArray := @atdC; end;
    'D': begin aArray := @atdD; end;
    'E': begin aArray := @atdE; end;
    'F': begin aArray := @atdF; end;
    'G': begin aArray := @atdG; end;
    'H': begin aArray := @atdH; end;
    'I': begin aArray := @atdI; end;
    'J': begin aArray := @atdJ; end;
    'K': begin aArray := @atdK; end;
    'L': begin aArray := @atdL; end;
    'M': begin aArray := @atdM; end;
    'N': begin aArray := @atdN; end;
    'O': begin aArray := @atdO; end;
    'P': begin aArray := @atdP; end;
    'Q': begin aArray := @atdQ; end;
    'R': begin aArray := @atdR; end;
    'S': begin aArray := @atdS; end;
    'T': begin aArray := @atdT; end;
    'U': begin aArray := @atdU; end;
    'V': begin aArray := @atdV; end;
    'W': begin aArray := @atdW; end;
    'X': begin aArray := @atdX; end;
    'Y': begin aArray := @atdY; end;
    'Z': begin aArray := @atdZ; end;
  end;
end;

procedure TSynLCHighlighter.ResetArrayOfTokenDef;
begin
  SetLength(atdA,0);
  SetLength(atdB,0);
  SetLength(atdC,0);
  SetLength(atdD,0);
  SetLength(atdE,0);
  SetLength(atdF,0);
  SetLength(atdG,0);
  SetLength(atdH,0);
  SetLength(atdI,0);
  SetLength(atdJ,0);
  SetLength(atdK,0);
  SetLength(atdL,0);
  SetLength(atdM,0);
  SetLength(atdN,0);
  SetLength(atdO,0);
  SetLength(atdP,0);
  SetLength(atdQ,0);
  SetLength(atdR,0);
  SetLength(atdS,0);
  SetLength(atdT,0);
  SetLength(atdU,0);
  SetLength(atdV,0);
  SetLength(atdW,0);
  SetLength(atdX,0);
  SetLength(atdY,0);
  SetLength(atdZ,0);
end;

procedure TSynLCHighlighter.PopularArrayOfTokenDef;
  procedure AddListOfToken(aList:TStrings; aKindToken:TLCTokenKind);
  var
    iPos   : Integer;
    rToken : TLCTokenDef;
    sToken : String;
    aArray : TPtrLCArrayTokenDef;
  begin
    for sToken in aList do
    begin
      GetArrayToToken(sToken, aArray);
      if aArray <> nil then
      begin
        rToken.Texto := LowerCase(sToken);
        rToken.Kind := aKindToken;

        iPos := High(aArray^) + 1;
        SetLength(aArray^, iPos + 1);
        aArray^[iPos] := rToken;
      end;
    end;
  end;
begin
  ResetArrayOfTokenDef;

  AddListOfToken(fSettings.SettingsToAttributeName.Words, tLCAttributeName);
  AddListOfToken(fSettings.SettingsToDataType.Words, tLCDataType);
  AddListOfToken(fSettings.SettingsToKey.Words, tLCKey);
  AddListOfToken(fSettings.SettingsToReservedWord.Words, tLCReservedWord);
  AddListOfToken(fSettings.SettingsToVariable.Words, tLCVariable);
  AddListOfToken(fSettings.SettingsToCustomFunction.Words, tLCCustomFunction);
end;

function TSynLCHighlighter.GetKindOfToken(aToken : String; const aDefaultKind : TLCTokenKind;
  const aKindExpected : TLCTokenKind) : TLCTokenKind;
var
  iPos   : Integer;
  aArray : TPtrLCArrayTokenDef;
begin
  Result := aDefaultKind;
  GetArrayToToken(aToken, aArray);
  if aArray <> nil then
  begin
    for iPos := 0 to High(aArray^) do
    begin
      if aArray^[iPos].Texto = aToken then
      begin
        if (aKindExpected = tLCUnknown)
        or (aKindExpected = aArray^[iPos].Kind) then
        begin
          Result := aArray^[iPos].Kind;
          exit;
        end;
      end;
    end;
  end;

  if Assigned(fOnGetCustomIdentKind) then
  begin
    fOnGetCustomIdentKind(self, aToken, Result);
  end;
end;

procedure TSynLCHighlighter.CreateDefaultAttributes;
begin
  fCommentAttri := TSynHighlighterAttributes.Create(@SYNS_AttrComment, SYNS_XML_AttrComment);
  fCommentAttri.Style:= [fsBold];
  fCommentAttri.Foreground:=clTeal;
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighlighterAttributes.Create(@SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier);
  fIdentifierAttri.Foreground:=clNavy;
  AddAttribute(fIdentifierAttri);

  fCustomFunctionWordAttri := TSynHighlighterAttributes.Create(@SYNS_AttrFunction, SYNS_XML_AttrFunction);
  fCustomFunctionWordAttri.Foreground:= $FF8000;
  fCustomFunctionWordAttri.Style:= [fsBold];
  AddAttribute(fCustomFunctionWordAttri);

  fReservedWordAttri := TSynHighlighterAttributes.Create(@SYNS_AttrReservedWord, SYNS_XML_AttrReservedWord);
  fReservedWordAttri.Foreground:=clNavy;
  fReservedWordAttri.Style:= [fsBold];
  AddAttribute(fReservedWordAttri);

  fDataTypeAttri := TSynHighlighterAttributes.Create(@SYNS_AttrDataType, SYNS_XML_AttrDataType);
  AddAttribute(fDataTypeAttri);

  fKeyAttri := TSynHighlighterAttributes.Create(@SYNS_AttrKey, SYNS_XML_AttrKey);
  fKeyAttri.Style:= [fsBold];
  fKeyAttri.Foreground:=clBlack;
  AddAttribute(fKeyAttri);

  fVariableAttri := TSynHighlighterAttributes.Create(@SYNS_AttrVariable, SYNS_XML_AttrVariable);
  AddAttribute(fVariableAttri);

  fAttributeNameAttri := TSynHighlighterAttributes.Create(@SYNS_AttrAttributeName, SYNS_XML_AttrAttributeName);
  fAttributeNameAttri.Style:= [fsBold];
  fAttributeNameAttri.Foreground:=clOlive;
  AddAttribute(fAttributeNameAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(@SYNS_AttrNumber, SYNS_XML_AttrNumber);
  fNumberAttri.Foreground:=clGreen;
  AddAttribute(fNumberAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSpace, SYNS_XML_AttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighlighterAttributes.Create(@SYNS_AttrString, SYNS_XML_AttrString);
  fStringAttri.Foreground:=clBlue;
  AddAttribute(fStringAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(@SYNS_AttrSymbol, SYNS_XML_AttrSymbol);
  fSymbolAttri.Foreground:=clMaroon;
  AddAttribute(fSymbolAttri);
end;

procedure TSynLCHighlighter.DefineDefaultValidCaracters;
var
  I: Char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': fIdentifiers[I] := True;
    else
      fIdentifiers[I] := False;
    end;
    fNumberChar[I]:=(I in ['0'..'9']);
    fSpaceChar[I]:=(I in [#1..#9, #11, #12, #14..#32]);
  end;
end;

function TSynLCHighlighter.IdentKind(isAttr : Boolean; pToken : String) : TLCTokenKind;
var
  aToken: String;
begin
  aToken := pToken;
  if aToken.isEmpty = true then
  begin
    aToken := getToken;
  end;

  if (fSettings.CaseSensitive = false) then
  begin
    aToken := LowerCase(aToken);
  end;

  Result := tLCIdentifier; // default value
  if (isAttr = true) then
  begin
    Result := GetKindOfToken(aToken, Result, tLCAttributeName);
  end
  else
  begin
    Result := GetKindOfToken(aToken, Result);
  end;
end;

function TSynLCHighlighter.IsLineEnd(pPosition: Integer): Boolean;
begin
  Result := (pPosition >= fSizeCurrentLine)
         or (fPCharCurrentLine[pPosition] = #10)
         or (fPCharCurrentLine[pPosition] = #13);
end;

procedure TSynLCHighlighter.CommentMultiLineProc;
begin
  if (rsLCComment in fRanges) = false then
  begin
    fTokenKind := tLCSymbol;
    inc(fPositionInLine);
    if (fPCharCurrentLine[fPositionInLine] = '*') then
    begin
      inc(fPositionInLine);
      fRanges := fRanges + [rsLCComment];
      fTokenKind := tLCComment;

      StartLCCodeFoldBlock(cfbtLCCommentMultiLine);

      CommentMultiLineProc;
    end;
  end
  else
  begin
    fTokenKind := tLCComment;
    while not IsLineEnd(fPositionInLine) do
    begin
      if  (fPCharCurrentLine[fPositionInLine] = '*')
      and (fPCharCurrentLine[fPositionInLine + 1] = '/') then
      begin
        Inc(fPositionInLine, 2);
        fRanges := fRanges - [rsLCComment];
        EndLCCodeFoldBlock;
        break;
      end;
      inc(fPositionInLine);
    end; // while
  end; // else-if
end;

procedure TSynLCHighlighter.CommentInLineProc;
begin
  fTokenKind := tLCComment;
  Inc(fPositionInLine);
  while (IsLineEnd(fPositionInLine) = false)
  and   (fPCharCurrentLine[fPositionInLine] <> '@') do
  begin
    Inc(fPositionInLine);
  end;
  if  (fPCharCurrentLine[fPositionInLine] in ['@']) then
  begin
    Inc(fPositionInLine);
  end;
end;

procedure TSynLCHighlighter.CRProc;
begin
  fTokenKind := tLCSpace;
  inc(fPositionInLine);
  if fPCharCurrentLine[fPositionInLine] = #10 then
  begin
    inc(fPositionInLine);
  end;
end;

procedure TSynLCHighlighter.IdentProc(isAttr:Boolean);
begin
  inc(fPositionInLine);
  while fIdentifiers[fPCharCurrentLine[fPositionInLine]] do
  begin
    inc(fPositionInLine);
  end;
  fTokenKind := IdentKind(isAttr);
end;

procedure TSynLCHighlighter.LFProc;
begin
  fTokenKind := tLCSpace;
  inc(fPositionInLine);
end;

constructor TSynLCHighlighter.Create(AOwner: TComponent);
begin
  fSettings:= TSynLCHighlighterSettings.Create;
  fRanges := [];

  inherited Create(AOwner);

  ResetArrayOfTokenDef;

  CreateDefaultAttributes;
  DefineDefaultValidCaracters;
end;

destructor TSynLCHighlighter.Destroy;
begin
  FreeAndNil(fSettings);

  inherited Destroy;
end;

procedure TSynLCHighlighter.SaveSettingsToFile(fileName: String);
begin
  fSettings.SaveToFile(fileName);
end;

procedure TSynLCHighlighter.LoadSettingsFromFile(fileName: String);
begin
  fSettings.LoadFromFile(fileName);
  PopularArrayOfTokenDef;
  UpdateAttributesBySettings;
end;

procedure TSynLCHighlighter.SetLine(const NewValue: String; LineNumber: Integer);
begin
  Inherited;

  fPositionInLine := 0;
  fPCharCurrentLine:=PChar(NewValue);
  fSizeCurrentLine :=length(NewValue);
  FAtLineStart := True;
  fNumberCurrentLine := LineNumber;

  Next;
end;

procedure TSynLCHighlighter.Next;
var
  aToken: String;
begin
  fTokenStart:= fPositionInLine;

  if fPositionInLine >= fSizeCurrentLine  then
  begin
    NullProc;
    exit;
  end;

  if (rsLCString in fRanges) then
  begin
    StringMutlipeLinesProc;
    exit;
  end;

  if (rsLCComment in fRanges) then
  begin
    CommentMultiLineProc;
    exit;
  end;

  if (rsLCAttrName  in fRanges) then
  begin
    IdentProc(true);
    fRanges := fRanges - [rsLCAttrName];
    exit;
  end;

  case fPCharCurrentLine[fPositionInLine] of
    '@': CommentInLineProc;
    '/': CommentMultiLineProc;
    '}': SymbolProc;
    '{': SymbolProc;
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    '0'..'9': NumberProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    '>': SymbolProc;
    '<': SymbolProc;
    '(': SymbolProc;
    ')': SymbolProc;
    ';': SymbolProc;
    '.': PointProc;
    #92: StringProc; // \
    ']': SymbolProc;
    '[': SymbolProc;
    ',','+','-','*','|','=': SymbolProc;
    #34: StringProc; // "
    else
    begin
      UnknownProc;
    end;
  end;

  if fTokenKind = tLCKey then
  begin
    aToken := LowerCase(GetToken);
    if (aToken = 'inicio') then
    begin
      StartLCCodeFoldBlock(cfbtLCInicioFim);
    end;
    if (aToken = 'fim') then
    begin
      EndLCCodeFoldBlock;
    end;
  end;

  if  FAtLineStart
  and not(fTokenKind in [tLCSpace, tLCComment]) then
  begin
    FAtLineStart := False;
  end;
end;

function TSynLCHighlighter.GetEol: Boolean;
begin
  Result := (fTokenKind = tLCNull) and (fPositionInLine >= fSizeCurrentLine);
end;

function TSynLCHighlighter.GetToken: String;
var
  Len: LongInt;
begin
  Len := fPositionInLine - fTokenStart;
  SetString(Result, (fPCharCurrentLine + fTokenStart), Len);
end;

procedure TSynLCHighlighter.GetTokenEx(out TokenStart: PChar; out
  TokenLength: integer);
begin
  TokenLength := fPositionInLine - fTokenStart;
  TokenStart := nil;
  if TokenLength > 0 then
  begin
    TokenStart := @fPCharCurrentLine[fTokenStart];
  end;
end;

function TSynLCHighlighter.GetTokenPos: Integer;
begin
  Result := fTokenStart;
end;

function TSynLCHighlighter.GetTokenKind: integer;
begin
  Result := Ord(fTokenKind);
end;

function TSynLCHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenKind of
    tLCAttributeName: Result := fAttributeNameAttri;
    tLCComment: Result := fCommentAttri;
    tLCCustomFunction: Result := fCustomFunctionWordAttri;
    tLCIdentifier: Result := fIdentifierAttri;
    tLCDataType: Result := fDataTypeAttri;
    tLCKey: Result := fKeyAttri;
    tLCNumber: Result := fNumberAttri;
    tLCReservedWord: Result := fReservedWordAttri;
    tLCSpace: Result := fSpaceAttri;
    tLCString: Result := fStringAttri;
    tLCSymbol: Result := fSymbolAttri;
    tLCUnknown: Result := fSymbolAttri;
    tLCVariable: Result := fVariableAttri;
  else
    Result := nil;
  end;
end;

function TSynLCHighlighter.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
      SYN_ATTR_COMMENT: Result := fCommentAttri;
      SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
      SYN_ATTR_KEYWORD: Result := fKeyAttri;
      SYN_ATTR_STRING: Result := fStringAttri;
      SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
      SYN_ATTR_NUMBER: Result := fNumberAttri;
    else
      Result := nil;
    end;
end;

procedure TSynLCHighlighter.SetRange(Value: Pointer);
begin
  inherited SetRange(Value);
  {$PUSH} {$Warnings OFF} {$Hints OFF}
    fRanges := TLCRangeStates(Integer(PtrUInt(CodeFoldRange.RangeType)));
  {$POP}
end;

procedure TSynLCHighlighter.ResetRange;
begin
  Inherited;
  fRanges := [];
end;

function TSynLCHighlighter.GetRange: Pointer;
begin
  {$PUSH} {$Warnings OFF} {$Hints OFF}
    CodeFoldRange.RangeType:=Pointer(PtrUInt(Integer(fRanges)));
  {$POP}
  Result := inherited GetRange;
end;

function TSynLCHighlighter.GetFoldConfigInstance(Index: Integer): TSynCustomFoldConfig;
begin
  Result:=inherited GetFoldConfigInstance(Index);
  Result.Enabled := True;
  if TLCCodeFoldBlockType(Index) in [cfbtLCInicioFim, cfbtLCCommentMultiLine] then
  begin
    Result.Modes := Result.Modes + [fmMarkup];
  end;
end;

end.


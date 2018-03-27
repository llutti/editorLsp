unit uCompilador;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Fgl
  ,SynLCHighlighter
  ,SynEditHighlighter;

type
  TLCTypeOfProcess = (tpLCAttrName, tpLCComment, tpLCString, tpLCUnknown);
  TLCTypesProcessing = set of TLCTypeOfProcess;

  TLCDefParametro = record
    Name:String;
    IsEnd:Boolean;
  end;

  TLCListOfParametros = Array of TLCDefParametro;

  TLCTokenDef = record
    Texto : string;
    kind : TLCTokenKind;
  end;

  TLCArrayTokenDef = array of TLCTokenDef;

  TPtrLCArrayTokenDef = ^TLCArrayTokenDef;
  TPtrLCTokenDef = ^TLCTokenDef;

  TLCTipoBloco = (lcTBChave, lcTBSe, lcTBInicio, lcTBPara, lcTBEnquanto, lcTBParenteses, lcTBUnknown);
  TLCBloco = record
    Tipo: TLCTipoBloco;
    Ativo:Boolean;
    LinhaOrigem:Integer;
    ColunaOrigem:Integer;
  end;

  TLCTypeOfMessages = (lcTMUnknown, lcTMHint, lcTMWarning, lcTMError);

Const
  LCDescriptionTypeOfMessages: Array[TLCTypeOfMessages] of string = ('Desconhecido','Sugestão', 'Atenção', 'Erro');
  LCDescriptionBlocos: Array[TLCTipoBloco] of string = ('Chave', 'Se', 'Inicio', 'Para','Enquanto','Parenteses','Desconhecido');

Type
  { TLCMessages }

  TLCMessages = class
  private
    fColuna : Integer;
    fLinha : Integer;
    fTexto : String;
    fTextoEstatico : String;
    fTipo : TLCTypeOfMessages;
  protected
  public
    constructor Create;
  published
    property Texto:String read fTexto write fTexto;
    property TextoEstatico:String read fTextoEstatico write fTextoEstatico;
    property Linha:Integer read fLinha write fLinha;
    property Coluna:Integer read fColuna write fColuna;
    property Tipo:TLCTypeOfMessages read fTipo write fTipo;
end;

  TLCListOfMessages = specialize TFPGObjectList<TLCMessages>;

  { TLCDefinitionOfFunction }

  TLCDefinitionOfFunction = Class
    private
      FDescription : String;
      FName : String;
      FParameters : TLCListOfParametros;
      FRowOfDefinition : Integer;
      FRowOfImplementation: Integer;
    protected
    public
      constructor Create;
      //destructor Destroy; virtual;
      procedure AddParam(aName:String; aIsEnd:Boolean);

      function GetTipText:String;
    published
      property Name:String read FName write FName;
      property Description:String read FDescription write FDescription;
      property Parameters:TLCListOfParametros read FParameters write FParameters;
      property RowOfDefinition:Integer read FRowOfDefinition write FRowOfDefinition;
      property RowOfImplementation:Integer read FRowOfImplementation write FRowOfImplementation;
  end;

  TLCListOfFunction = specialize TFPGObjectList<TLCDefinitionOfFunction>;   //lista de bloques

  TLCProgressoCompilacao = procedure(Const pLinhaAtual:Integer; Const pTotalLinhas:Integer) of object;

  { TLCCompilador }

  TLCCompilador = class(TPersistent)
    private
      fIdentifiers: array[#0..#255] of ByteBool;
      FListOfFunctions: TLCListOfFunction;
      FListOfMessages: TLCListOfMessages;
      fNumberChar: array[char] of Boolean;
      fOnProgressoCompilacao : TLCProgressoCompilacao;
      fSpaceChar: array[char] of Boolean;

      fLines: TStrings; // conjunto de linhas a serem processadas

      fTypeOfToken: TLCTokenKind;
      fProcessing: TLCTypesProcessing;

      fTotalLinhasProcessar:Integer;
      fLine: PChar; // Linha Sendo processada
      fSizeLine: Integer; // Tamanho da linha
      fIndexCurLine:Integer; // Número da linha sendo processada
      fPosIniTok: Integer; // posição inicial do token na linha
      fCurPosInLine: Integer; // posição corrente do ponteiro na linha

      fHL: TSynLCHighlighter;

      fPermiteSenao:Boolean;
      fBlocos: array of TLCBloco;

      fListTokens: TLCArrayTokenDef;
      procedure DefineDefaultValidCaracters;

      function IdentKind(const pIsAttr:Boolean = false): TLCTokenKind;
      function IsLineEnd(pPosition: Integer): Boolean;

      procedure CommentMultiLineProc;
      procedure CommentInLineProc;
      procedure CRProc;
      procedure IdentProc(isAttr:Boolean = false);

      procedure ApostrofoProc;
      procedure LFProc;
      procedure NullProc;
      procedure MinusProc;
      procedure NumberProc;
      procedure PointProc;
      procedure SpaceProc;
      procedure StringProc;
      procedure StringMutlipeLinesProc;
      procedure SymbolProc;
      procedure UnknownProc;

    protected
      function GetLines: TStrings; virtual;
      procedure SetLines(Value: TStrings); virtual;

    public
      constructor Create;
      destructor Destroy; override;

      procedure Initialize;

      procedure SetLine(const NewValue: String; LineNumber: Integer);

      function IsEndOfRows: Boolean;
      function IsEndOfLine: Boolean;

      function NextRow:Boolean;

      function GetToken:String;
      procedure NextToken;

      procedure SkipSpaces(const ExtendToNextLine:Boolean; const SkipStrings:Boolean = true);

      procedure CompilarListaFuncoes(aList: TLCListOfFunction);
      procedure CompilarListaFuncoes;

      procedure AdicionarMensagem(pTipo:TLCTypeOfMessages; pTexto:String; pColuna, pLinha:Integer);

      procedure AdicionarBloco(const pTipoBloco: TLCTipoBloco; const pLinha, pColuna:Integer);
      function RemoverBloco(const pTipoBloco: TLCTipoBloco):Boolean;
      function FinalizarBloco(const pTipoBloco: TLCTipoBloco):Boolean;

      function SintaxeDefinir:Boolean;
      function SintaxeDefinirFuncao:Boolean;
      function SintaxeDefinirNumero:Boolean;
      function SintaxeSe:Boolean;
      function SintaxeEnquanto:Boolean;
      function SintaxeParenteses:Boolean;
      function SintaxePara:Boolean;
      function SintaxeFuncao(pTypeOfToken: TLCTokenKind):Boolean;

      function ChecarSintaxe:Boolean;

      Function SearchFunction(aList: TLCListOfFunction; aName:String):TLCDefinitionOfFunction;
      Function SearchFunction(aName:String):TLCDefinitionOfFunction;

    published
      property onProgressoCompilacao: TLCProgressoCompilacao read fOnProgressoCompilacao write fOnProgressoCompilacao;
      property Lines: TStrings read GetLines write SetLines;
      property ListOfFunctions:TLCListOfFunction read FListOfFunctions write FListOfFunctions;
      property ListOfMessages:TLCListOfMessages read FListOfMessages write FListOfMessages;
      property HL:TSynLCHighlighter read fHL write fHL;
  end;

implementation

{ TLCMessages }

constructor TLCMessages.Create;
begin
  fColuna := -1;
  fLinha := -1;
  fTexto := '';
  fTextoEstatico := '';
  fTipo := lcTMUnknown;
end;

{ TLCDefinitionOfFunction }

constructor TLCDefinitionOfFunction.Create;
begin
  FDescription := '';
  FName := '';
  FRowOfDefinition := -1;
  FRowOfImplementation := -1;
  SetLength(FParameters, 0);
end;

procedure TLCDefinitionOfFunction.AddParam(aName : String; aIsEnd : Boolean);
var
  Param: TLCDefParametro;
  Index:Integer;
begin
  Param.Name := aName;
  Param.IsEnd := aIsEnd;

  Index := Length(FParameters);

  setlength(FParameters, Index + 1);

  FParameters[Index] := Param;
end;

function TLCDefinitionOfFunction.GetTipText : String;
var
  Param : TLCDefParametro;
begin
  Result := '';
  for Param in FParameters do
  begin
    if Result <> '' then
    begin
      Result += ',';
    end;
    Result += '"Numero ';
    if Param.IsEnd = True then
    begin
      Result += 'End ';
    end;
    Result += Param.Name + '"';
  end;
end;

{ TLCCompilador }

function TLCCompilador.GetLines : TStrings;
begin
  Result := FLines;
end;

procedure TLCCompilador.SetLines(Value : TStrings);
begin
  FLines.Assign(Value);
  fTotalLinhasProcessar := FLines.Count;
end;

constructor TLCCompilador.Create;
begin
  FLines := TStringList.Create();
  FListOfFunctions := TLCListOfFunction.Create(true);
  FListOfMessages:= TLCListOfMessages.Create(true);
  fHL := nil;

  SetLength(fListTokens,0);
  SetLength(fBlocos,0);

  DefineDefaultValidCaracters;
end;

destructor TLCCompilador.Destroy;
begin
  FreeAndNil(FListOfMessages);
  FreeAndNil(FListOfFunctions);
  FreeAndNil(FLines);

  inherited Destroy;
end;

procedure TLCCompilador.Initialize;
begin
  FListOfMessages.Clear;
  SetLength(fBlocos,0);
  fPermiteSenao := False;
  fIndexCurLine := -1;
  fTypeOfToken := tLCUnknown;
  fProcessing := [];
end;

procedure TLCCompilador.SetLine(const NewValue : String; LineNumber : Integer);
begin
  fLine := PChar(NewValue);
  fSizeLine := Length(NewValue);
  fIndexCurLine := LineNumber;
  fPosIniTok := 0;
  fCurPosInLine := 0;

  NextToken;
end;

function TLCCompilador.IsEndOfRows : Boolean;
begin
  Result := fIndexCurLine >= fLines.Count;
end;

function TLCCompilador.IsEndOfLine : Boolean;
begin
  Result := (fTypeOfToken = tLCEol) or IsLineEnd(fCurPosInLine);
end;

function TLCCompilador.NextRow : Boolean;
begin
  inc(fIndexCurLine);

  if Assigned(fOnProgressoCompilacao) then
  begin
    fOnProgressoCompilacao(fIndexCurLine, fTotalLinhasProcessar);
  end;

  if IsEndOfRows = True then
  begin
    Result := False;
    Exit;
  end;

  SetLine(fLines[fIndexCurLine], fIndexCurLine);

  Result := True;
end;

function TLCCompilador.GetToken : String;
var
  Len: LongInt;
begin
  Len := fCurPosInLine - fPosIniTok;
  SetString(Result, (fLine + fPosIniTok), Len);
end;

procedure TLCCompilador.NextToken;
begin
  fPosIniTok := fCurPosInLine;

  if fCurPosInLine = fSizeLine then
  begin
    fTypeOfToken := tLCEol;
    Exit;
  end;

  if (tpLCString in fProcessing) then
  begin
    StringMutlipeLinesProc;
    exit;
  end;

  if (tpLCComment in fProcessing) then
  begin
    CommentMultiLineProc;
    exit;
  end;

  if (tpLCAttrName  in fProcessing) then
  begin
    IdentProc(true);
    fProcessing := fProcessing - [tpLCAttrName];
    exit;
  end;

  case fLine[fCurPosInLine] of
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
    '-': MinusProc;
    '>': SymbolProc;
    '<': SymbolProc;
    '(': SymbolProc;
    ')': SymbolProc;
    ';': SymbolProc;
    ':': SymbolProc;
    '.': PointProc;
    #92: StringProc; // \
    ']': SymbolProc;
    '[': SymbolProc;
    ',','+','*','|','=': SymbolProc;
    #34: StringProc; // "
    #39: ApostrofoProc; // '
    else
    begin
      UnknownProc;
    end;
  end;
end;

procedure TLCCompilador.SkipSpaces(const ExtendToNextLine : Boolean; const SkipStrings : Boolean);
var
  ListaProc: array[TLCTokenKind] of boolean = (false, true, false, false, false, true, false,
                                               false, true, false, false, true, false, true,
                                               false);
begin
  //ListaProc[tLCSpace] := true;
  //ListaProc[tLCComment] := true;
  ListaProc[tLCString] := SkipStrings;

  NextToken;
  while (ExtendToNextLine = true)
  and   (IsEndOfRows = false)
  and   (IsEndOfLine = true) do
  begin
    NextRow;
  end;

  while (IsEndOfLine = false)
  and   (ListaProc[fTypeOfToken]) do
  begin
    NextToken;
    while (ExtendToNextLine = true)
    and   (IsEndOfRows = false)
    and   (IsEndOfLine = true) do
    begin
      NextRow;
    end;
  end;
end;

procedure TLCCompilador.CompilarListaFuncoes;
begin
  CompilarListaFuncoes(FListOfFunctions);
end;

procedure TLCCompilador.AdicionarMensagem(pTipo : TLCTypeOfMessages; pTexto : String; pColuna, pLinha : Integer);
var
  oMsg: TLCMessages;
begin
  oMsg := TLCMessages.Create;

  oMsg.Tipo := pTipo;
  oMsg.Texto := pTexto;
  oMsg.Linha := pLinha;
  oMsg.Coluna := pColuna;

  FListOfMessages.Add(oMsg);
end;

procedure TLCCompilador.AdicionarBloco(const pTipoBloco : TLCTipoBloco; const pLinha, pColuna : Integer);
var
  iIndex:Integer;
  oBloco: TLCBloco;
begin
  iIndex := Length(fBlocos);
  SetLength(fBlocos,iIndex + 1);

  oBloco.Tipo := pTipoBloco;
  oBloco.Ativo := True;
  oBloco.LinhaOrigem := pLinha;
  oBloco.ColunaOrigem := pColuna;
  fBlocos[iIndex] := oBloco;
end;

function TLCCompilador.RemoverBloco(const pTipoBloco : TLCTipoBloco) : Boolean;
var
  iIndex:Integer;
begin
  Result := true;
  iIndex := High(fBlocos);
  if (iIndex = -1)
  or (fBlocos[iIndex].Tipo <> pTipoBloco) then
  begin
    Result := false;
    Exit;
  end;
  SetLength(fBlocos,iIndex);
end;

function TLCCompilador.FinalizarBloco(const pTipoBloco : TLCTipoBloco) : Boolean;
var
  iIndex:Integer;
begin
  Result := true;
  iIndex := High(fBlocos);
  if (iIndex = -1)
  or (fBlocos[iIndex].Tipo <> pTipoBloco)
  or (fBlocos[iIndex].Ativo = False) then
  begin
    Result := false;
    Exit;
  end;
  fBlocos[iIndex].Ativo := False;
end;

function TLCCompilador.ChecarSintaxe : Boolean;
var
  oTypeOfToken: TLCTokenKind;
  Token: String;
  iIndex,
  iPosIniToken,
  iLinhaErro: Integer;
begin
  Result := true;

  FListOfMessages.Clear;
  Initialize;

  While (NextRow = true) do
  begin
    While (IsEndOfLine = False) do
    begin
      Token := AnsiUpperCase(GetToken);
      iLinhaErro := fIndexCurLine + 1;
      iPosIniToken := fPosIniTok + 1;

      Case Token of
        'DEFINIR':
          begin
            SkipSpaces(True, False);

            if SintaxeDefinir = false then
            begin
              Result := false;
              exit;
            end;
          end;
        'FUNCAO':
          begin
            SkipSpaces(True, False);

            if SintaxeDefinirFuncao = false then
            begin
              Result := false;
              exit;
            end;
          end;
        'SE':
          begin
            AdicionarBloco(lcTBSe, fIndexCurLine + 1, fPosIniTok + 1);

            SkipSpaces(True, False);

            if SintaxeSe = false then
            begin
              Result := false;
              exit;
            end;

            SkipSpaces(True, False);
            if AnsiUpperCase(GetToken) <> 'INICIO' then
            begin
              RemoverBloco(lcTBSe);
              AdicionarMensagem(lcTMHint, 'É recomendado incluir um Bloco INICIO/FIM para o "SE".', iPosIniToken, iLinhaErro);
            end;
            Continue;
          end;
        'SENAO':
          begin
            if fPermiteSenao = false then
            begin
              AdicionarMensagem(lcTMWarning, 'Encontrado um "SENAO" sem um "SE" correspondente.', iPosIniToken, iLinhaErro);
            end;

            SkipSpaces(True, False);
            if  (AnsiUpperCase(GetToken) <> 'INICIO')
            and (AnsiUpperCase(GetToken) <> 'SE') then
            begin
              AdicionarMensagem(lcTMHint, 'É recomendado incluir um Bloco INICIO/FIM para o "SENAO".', iPosIniToken, iLinhaErro);
            end;
            Continue;
          end;
        'ENQUANTO':
          begin
            SkipSpaces(True, False);

            if SintaxeEnquanto = false then
            begin
              Result := false;
              exit;
            end;

            SkipSpaces(True, False);
            if AnsiUpperCase(GetToken) <> 'INICIO' then
            begin
              RemoverBloco(lcTBEnquanto);
              AdicionarMensagem(lcTMHint, 'É recomendado incluir um Bloco INICIO/FIM para o "ENQUANTO".', iPosIniToken, iLinhaErro);
            end;
            Continue;
          end;
        'PARA':
          begin
            SkipSpaces(True, False);

            if SintaxePara = false then
            begin
              Result := false;
              exit;
            end;

            SkipSpaces(True, False);
            if AnsiUpperCase(GetToken) <> 'INICIO' then
            begin
              RemoverBloco(lcTBPara);
              AdicionarMensagem(lcTMHint, 'É recomendado incluir um Bloco INICIO/FIM para o "PARA".', iPosIniToken, iLinhaErro);
            end;
            Continue;
          end;
        'INICIO':
          begin
            AdicionarBloco(lcTBInicio, fIndexCurLine + 1, fPosIniTok + 1);
          end;
        'FIM':
          begin
            SkipSpaces(True, False);
            Token := GetToken;
            if Token <> ';' then
            begin
              AdicionarMensagem(lcTMHint, 'Depois de um "FIM" é recomendado um ";".', iPosIniToken, iLinhaErro);
            end;

            if RemoverBloco(lcTBInicio) = false then
            begin
              if RemoverBloco(lcTBSe) = false then
              begin
                Result := False;
                AdicionarMensagem(lcTMError, 'Encontrado um "FIM" sem um "INICIO" correspondente.', iPosIniToken, iLinhaErro);
                exit;
              end;
              RemoverBloco(lcTBInicio);
            end;

            if RemoverBloco(lcTBSe) = true then
            begin
              SkipSpaces(True, False);
              Token := UpperCase(GetToken);
              fPermiteSenao := (Token = 'SENAO');
              Continue;
            end;
          end;
        '{':
          begin
            AdicionarBloco(lcTBChave, fIndexCurLine + 1, fPosIniTok + 1);
          end;
        '}':
          begin
            if RemoverBloco(lcTBChave) = false then
            begin
              if RemoverBloco(lcTBSe) = false then
              begin
                Result := False;
                AdicionarMensagem(lcTMError, 'Encontrado um "}" sem um "{" correspondente.', iPosIniToken, iLinhaErro);
                exit;
              end;
              RemoverBloco(lcTBInicio);
            end;

            if RemoverBloco(lcTBSe) = true then
            begin
              SkipSpaces(True, False);
              Token := UpperCase(GetToken);
              fPermiteSenao := (Token = 'SENAO');
              Continue;
            end;
          end;
        '(':
          begin
            AdicionarBloco(lcTBParenteses, fIndexCurLine + 1, fPosIniTok + 1);

            SkipSpaces(True, False);

            if SintaxeParenteses = false then
            begin
              Result := false;
              exit;
            end;
          end;
        ')':
          begin
            if RemoverBloco(lcTBParenteses) = false then
            begin
              Result := False;
              iLinhaErro := fIndexCurLine + 1;
              iPosIniToken := fPosIniTok + 1;
              AdicionarMensagem(lcTMError, 'Encontrado um ")" sem um "(" correspondente.', iPosIniToken, iLinhaErro);
              exit;
            end;
          end;
        'VAPARA':
          begin
            SkipSpaces(True, False);

            if fTypeOfToken <> tLCIdentifier then
            begin
              Result := False;
              iLinhaErro := fIndexCurLine + 1;
              iPosIniToken := fPosIniTok + 1;
              AdicionarMensagem(lcTMError, Format('Nome "%s" é inválido para um "label".',[GetToken]), iPosIniToken, iLinhaErro);
              exit;
            end;
            SkipSpaces(True, False);

            Token := GetToken;
            if Token <> ';' then
            begin
              Result := False;
              iLinhaErro := fIndexCurLine + 1;
              iPosIniToken := fPosIniTok + 1;
              AdicionarMensagem(lcTMError, Format('Símbolo "%s" é Inválido neste local. Era esperado um ";".', [GetToken]), iPosIniToken, iLinhaErro);

              exit;
            end;
          end;
        else
        begin
          Token := AnsiUpperCase(GetToken);
          if fTypeOfToken in [tLCReservedWord, tLCCustomFunction] then
          begin
            if Token = 'EXECSQL' then
            begin
              SkipSpaces(True, False);
              if not (fTypeOfToken in [tLCIdentifier, tLCString]) then
              begin
                Result := False;
                iLinhaErro := fIndexCurLine + 1;
                iPosIniToken := fPosIniTok + 1;
                AdicionarMensagem(lcTMError, 'Parâmetro inválido.', iPosIniToken, iLinhaErro);

                exit;
              end;

              SkipSpaces(True, fTypeOfToken = tLCString);
            end
            else
            begin
              oTypeOfToken := fTypeOfToken;
              SkipSpaces(True, False);

              if SintaxeFuncao(oTypeOfToken) = false then
              begin
                Result := false;
                exit;
              end;

              SkipSpaces(True, False);
            end;

            Token := GetToken;
            if Token <> ';' then
            begin
              Result := False;
              iLinhaErro := fIndexCurLine + 1;
              iPosIniToken := fPosIniTok + 1;
              AdicionarMensagem(lcTMError, Format('Símbolo "%s" é Inválido neste local. Era esperado um ";".', [GetToken]), iPosIniToken, iLinhaErro);

              exit;
            end;
          end
          else
          if fTypeOfToken in [tLCIdentifier, tLCVariable] then
          begin
            SkipSpaces(True, False);
            Token := GetToken;

            if (Token = '+')
            or (Token = '-') then
            begin
              NextToken;

              if Token <> GetToken then
              begin
                Result := False;
                iLinhaErro := fIndexCurLine + 1;
                iPosIniToken := fPosIniTok + 1;
                AdicionarMensagem(lcTMError, Format('Era esperado um símbolo "%s" mas foi identificado "%s".',[Token, GetToken]), iPosIniToken, iLinhaErro);

                exit;
              end;

              SkipSpaces(True, False);
              Token := GetToken;
              if Token <> ';' then
              begin
                Result := False;
                iLinhaErro := fIndexCurLine + 1;
                iPosIniToken := fPosIniTok + 1;
                AdicionarMensagem(lcTMError, Format('Símbolo "%s" é Inválido neste local. Era esperado um ";".', [GetToken]), iPosIniToken, iLinhaErro);

                exit;
              end;

              SkipSpaces(True, False);
              Continue;
            end;

            if Token = '[' then
            begin
              SkipSpaces(True, False);
              if not (fTypeOfToken in [tLCNumber, tLCIdentifier]) then
              begin
                Result := False;
                iLinhaErro := fIndexCurLine + 1;
                iPosIniToken := fPosIniTok + 1;
                AdicionarMensagem(lcTMError, Format('Era aguardado um "Número" ou "Variável" mas foi informado "%s".',[GetToken]), iPosIniToken, iLinhaErro);
                exit;
              end;
              SkipSpaces(True, False);
              Token := GetToken;

              if Token = ']' then
              begin
                SkipSpaces(True, False);
                Token := GetToken;
              end;
            end;

            if Token = '=' then
            begin
              SkipSpaces(True, False);

              if  (fTypeOfToken = tLCSymbol)
              and (GetToken <> '(')
              and (GetToken <> '-') then
              begin
                Result := False;
                iLinhaErro := fIndexCurLine + 1;
                iPosIniToken := fPosIniTok + 1;
                AdicionarMensagem(lcTMError, Format('Símbolo "%s" é inválido.',[GetToken]), iPosIniToken, iLinhaErro);
                exit;
              end;

              if fTypeOfToken = tLCReservedWord then
              begin
                oTypeOfToken := fTypeOfToken;
                SkipSpaces(True, False);

                if SintaxeFuncao(oTypeOfToken) = false then
                begin
                  Result := false;
                  exit;
                end;

                SkipSpaces(True, False);
                if GetToken <> ';' then
                begin
                  Result := False;
                  iLinhaErro := fIndexCurLine + 1;
                  iPosIniToken := fPosIniTok + 1;
                  AdicionarMensagem(lcTMError, Format('Símbolo "%s" é Inválido neste local. Era esperado um ";".', [GetToken]), iPosIniToken, iLinhaErro);

                  exit;
                end;
              end
              else
              begin
                while (IsEndOfLine = false) do
                begin
                  Token := UpperCase(GetToken);

                  if fTypeOfToken = tLCSymbol then
                  begin
                    if Token = ';' then
                    begin
                      break;
                    end;

                    if Token = '(' then
                    begin
                      if SintaxeParenteses = false then
                      begin
                        Result := false;
                        exit;
                      end;
                    end
                    else
                    if Token = '[' then
                    begin
                      SkipSpaces(True, False);
                      if not (fTypeOfToken in [tLCNumber, tLCIdentifier]) then
                      begin
                        Result := False;
                        iLinhaErro := fIndexCurLine + 1;
                        iPosIniToken := fPosIniTok + 1;
                        AdicionarMensagem(lcTMError, Format('Era aguardado um "Número" ou "Variável" mas foi informado "%s".',[GetToken]), iPosIniToken, iLinhaErro);
                        exit;
                      end;
                      SkipSpaces(True, False);
                      Token := GetToken;

                      if Token = ']' then
                      begin
                        SkipSpaces(True, False);
                        Continue;
                      end;
                    end
                    else
                    if Token = '+' then
                    begin
                      SkipSpaces(True, False);
                      Token := UpperCase(GetToken);

                      if Token = '(' then
                      begin
                        Continue;
                      end;

                      if not (fTypeOfToken in [tLCNumber, tLCIdentifier, tLCString, tLCVariable]) then
                      begin
                        Result := False;
                        iLinhaErro := fIndexCurLine + 1;
                        iPosIniToken := fPosIniTok + 1;
                        AdicionarMensagem(lcTMError, Format('Era aguardado um "Número" ou um "Texto" ou "Variável de Ambiente" para foi informado "%s".',[GetToken]), iPosIniToken, iLinhaErro);
                        exit;
                      end;
                    end
                    else
                    if Token = '.' then
                    begin
                      SkipSpaces(True, False);

                      if not (fTypeOfToken in [tLCNumber, tLCIdentifier, tLCAttributeName]) then
                      begin
                        Result := False;
                        iLinhaErro := fIndexCurLine + 1;
                        iPosIniToken := fPosIniTok + 1;
                        AdicionarMensagem(lcTMError, 'Sintaxe Inválida.', iPosIniToken, iLinhaErro);
                        exit;
                      end;
                    end
                    else
                    begin
                      SkipSpaces(True, False);
                      Token := UpperCase(GetToken);

                      if Token = '(' then
                      begin
                        Continue;
                      end;

                      if not (fTypeOfToken in [tLCNumber, tLCIdentifier, tLCVariable]) then
                      begin
                        Result := False;
                        iLinhaErro := fIndexCurLine + 1;
                        iPosIniToken := fPosIniTok + 1;
                        AdicionarMensagem(lcTMError, Format('Era aguardado um número para foi informado "%s".',[GetToken]), iPosIniToken, iLinhaErro);
                        exit;
                      end;

                      SkipSpaces(True, False);
                      if fTypeOfToken <> tLCSymbol then
                      begin
                        Result := False;
                        iLinhaErro := fIndexCurLine + 1;
                        iPosIniToken := fPosIniTok + 1;
                        AdicionarMensagem(lcTMError, Format('Era aguardado "Operador", mas foi informado "%s".',[GetToken]), iPosIniToken, iLinhaErro);
                        exit;
                      end;
                      Continue;
                    end;
                  end
                  else
                  if fTypeOfToken in [tLCNumber, tLCString, tLCIdentifier, tLCVariable] then
                  begin
                    SkipSpaces(True, fTypeOfToken = tLCString);

                    if fTypeOfToken <> tLCSymbol then
                    begin
                      Result := False;
                      iLinhaErro := fIndexCurLine + 1;
                      iPosIniToken := fPosIniTok + 1;
                      AdicionarMensagem(lcTMError, Format('Era aguardado "Operador", mas foi informado "%s".',[GetToken]), iPosIniToken, iLinhaErro);
                      exit;
                    end;
                    Continue;
                  end
                  else
                  if fTypeOfToken in [tLCKey] then
                  begin
                    SkipSpaces(True, False);

                    if SintaxeFuncao(tLCKey) = false then
                    begin
                      Result := false;
                      exit;
                    end;

                    SkipSpaces(True, False);
                    if GetToken <> ';' then
                    begin
                      Result := False;
                      iLinhaErro := fIndexCurLine + 1;
                      iPosIniToken := fPosIniTok + 1;
                      AdicionarMensagem(lcTMError, Format('Símbolo "%s" é Inválido neste local. Era esperado um ";".', [GetToken]), iPosIniToken, iLinhaErro);

                      exit;
                    end;

                    Continue;
                  end
                  else
                  begin
                    Result := False;
                    iLinhaErro := fIndexCurLine + 1;
                    iPosIniToken := fPosIniTok + 1;
                    AdicionarMensagem(lcTMError, 'Sintaxe inválida.', iPosIniToken, iLinhaErro);
                    exit;
                  end;

                  SkipSpaces(True, fTypeOfToken = tLCString);
                end;
              end;
            end
            else
            if Token = '.' then
            begin
              NextToken;

              if fTypeOfToken = tLCAttributeName then
              begin
                Token := UpperCase(GetToken);

                if Token = 'SQL' then
                begin
                  SkipSpaces(True, False);
                  if not (fTypeOfToken in [tLCIdentifier, tLCString]) then
                  begin
                    Result := False;
                    iLinhaErro := fIndexCurLine + 1;
                    iPosIniToken := fPosIniTok + 1;
                    AdicionarMensagem(lcTMError, 'Parâmetro inválido.', iPosIniToken, iLinhaErro);

                    exit;
                  end;
                end
                else
                begin
                  SkipSpaces(True, False);

                  if SintaxeFuncao(tLCAttributeName) = false then
                  begin
                    Result := false;
                    exit;
                  end;
                end;

                SkipSpaces(True, (fTypeOfToken = tLCString));
                if GetToken <> ';' then
                begin
                  Result := False;
                  iLinhaErro := fIndexCurLine + 1;
                  iPosIniToken := fPosIniTok + 1;
                  AdicionarMensagem(lcTMError, Format('Símbolo "%s" é Inválido neste local. Era esperado um ";".', [GetToken]), iPosIniToken, iLinhaErro);

                  exit;
                end;

                SkipSpaces(True, False);
                Continue;
              end;

              while (IsEndOfLine = false) do
              begin
                Token := UpperCase(GetToken);

                if Token = '(' then
                begin
                  if SintaxeParenteses = false then
                  begin
                    Result := false;
                    exit;
                  end;
                  SkipSpaces(True, False);
                end;

                if Token = ';' then
                begin
                  break;
                end;
                SkipSpaces(True, False);
              end;
            end
            else
            if Token = '(' then
            begin
              if SintaxeFuncao(tLCIdentifier) = false then
              begin
                Result := false;
                exit;
              end;

              SkipSpaces(True, False);
              if GetToken <> ';' then
              begin
                Result := False;
                iLinhaErro := fIndexCurLine + 1;
                iPosIniToken := fPosIniTok + 1;
                AdicionarMensagem(lcTMError, Format('Símbolo "%s" é Inválido neste local. Era esperado um ";".', [GetToken]), iPosIniToken, iLinhaErro);

                exit;
              end;
            end
            else
            if Token = ':' then
            begin
              SkipSpaces(True, False);
              Token := GetToken;
              if Token <> ';' then
              begin
                Result := False;
                iLinhaErro := fIndexCurLine + 1;
                iPosIniToken := fPosIniTok + 1;
                AdicionarMensagem(lcTMError, Format('Símbolo "%s" é Inválido neste local. Era esperado um ";".', [GetToken]), iPosIniToken, iLinhaErro);

                exit;
              end;
            end
            else
            if Token <> ';' then
            begin
              Result := False;
              iLinhaErro := fIndexCurLine + 1;
              iPosIniToken := fPosIniTok + 1;
              AdicionarMensagem(lcTMError, Format('Comando Desconhecido "%s".', [GetToken]), iPosIniToken, iLinhaErro);

              exit;
            end;
          end;
        end;
      end;
      SkipSpaces(True, False);
    end;
  end;

  for iIndex:= 0 to High(fBlocos) do
  begin
    Result := False;
    iLinhaErro := fBlocos[iIndex].LinhaOrigem;
    iPosIniToken := fBlocos[iIndex].ColunaOrigem;
    AdicionarMensagem(lcTMError, Format('Faltou Finalizar o Bloco "%s".',[LCDescriptionBlocos[fBlocos[iIndex].Tipo]]), iPosIniToken, iLinhaErro);
  end;
end;

procedure TLCCompilador.CompilarListaFuncoes(aList : TLCListOfFunction);
var
  iRowDef:Integer;
  NomeFuncao,
  Token: String;
  EhEnd: Boolean;
  Funcao: TLCDefinitionOfFunction;
begin
  aList.Clear;
  Initialize;
  While (NextRow = true) do
  begin
    While (IsEndOfLine = False) do
    begin
      Token := GetToken;
      if AnsiUpperCase(Token) = 'DEFINIR' then
      begin
        SkipSpaces(True, True);
        Token := GetToken;
        if AnsiUpperCase(Token) = 'FUNCAO' then
        begin
          SkipSpaces(True, True);
          NomeFuncao := GetToken;
          iRowDef := fIndexCurLine + 1; // fIndexCurLine é base ZERO

          // Buscar os parametros
          SkipSpaces(True, True);
          Token := GetToken;
          if Token = '(' then
          begin
            SkipSpaces(True, True);
            Token := GetToken;
            if (AnsiUpperCase(Token) = 'NUMERO') then
            begin
              Funcao := TLCDefinitionOfFunction.Create;
              Funcao.Name := NomeFuncao;
              Funcao.RowOfDefinition := iRowDef;

              while (IsEndOfRows = false) do
              begin
                if (AnsiUpperCase(Token) = 'NUMERO') then
                begin
                  EhEnd := False;

                  SkipSpaces(True, True);
                  Token := GetToken;
                  if (AnsiUpperCase(Token) = 'END') then
                  begin
                    EhEnd := true;

                    SkipSpaces(True, True);
                    Token := GetToken;
                  end;
                  Funcao.AddParam(Token, EhEnd);
                end;

                SkipSpaces(True, True);
                Token := GetToken;

                if Token = ')' then
                begin
                  aList.Add(Funcao);
                  Break;
                end;
              end;
            end
            else
            begin
              if Token = ')' then
              begin
                Funcao := TLCDefinitionOfFunction.Create;
                Funcao.Name := NomeFuncao;
                Funcao.RowOfDefinition := iRowDef;
                aList.Add(Funcao);
                Break;
              end;
            end;
          end;
        end;
      end
      else
      if AnsiUpperCase(Token) = 'FUNCAO' then
      begin
        SkipSpaces(True, True);
        NomeFuncao := GetToken;
        Funcao := SearchFunction(aList, NomeFuncao);
        if Funcao <> nil then
        begin
          Funcao.RowOfImplementation := fIndexCurLine + 1; // fIndexCurLine é base ZERO
        end;
      end;
      SkipSpaces(True, True);
    end;
  end;
end;

function TLCCompilador.SearchFunction(aList : TLCListOfFunction; aName : String) : TLCDefinitionOfFunction;
var
  i: integer;
  sName:String;
begin
  Result := nil;
  if aName = '' then
  begin
    exit;
  end;
  for i := 0 to aList.Count-1 do
  begin
    sName := UpCase(aList[i].Name);
    if sName = UpCase(aName) then
    begin
       Result := aList[i];
       exit;
    end;
  end;
end;

function TLCCompilador.SearchFunction(aName : String) : TLCDefinitionOfFunction;
begin
  Result := SearchFunction(FListOfFunctions, aName);
end;

procedure TLCCompilador.DefineDefaultValidCaracters;
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

function TLCCompilador.IdentKind(const pIsAttr : Boolean) : TLCTokenKind;
begin
  if fHL <> nil then
  begin
    Result := fHL.IdentKind(pIsAttr, GetToken);
    Exit;
  end;

  Result := tLCIdentifier; // default value
  if pIsAttr = true then
  begin
    Result := tLCAttributeName;
  end;
end;

function TLCCompilador.IsLineEnd(pPosition : Integer) : Boolean;
begin
  Result := (pPosition > fSizeLine)
         or (fLine[pPosition] = #10)
         or (fLine[pPosition] = #13);
end;

procedure TLCCompilador.CommentMultiLineProc;
begin
  if (tpLCComment in fProcessing) = false then
  begin
    fTypeOfToken := tLCSymbol;
    inc(fCurPosInLine);
    if (fLine[fCurPosInLine] = '*') then
    begin
      inc(fCurPosInLine);
      fProcessing := fProcessing + [tpLCComment];
      fTypeOfToken := tLCComment;

      CommentMultiLineProc;
    end;
  end
  else
  begin
    fTypeOfToken := tLCComment;
    while not IsLineEnd(fCurPosInLine) do
    begin
      if  (fLine[fCurPosInLine] = '*')
      and (fLine[fCurPosInLine + 1] = '/') then
      begin
        Inc(fCurPosInLine, 2);
        fProcessing := fProcessing - [tpLCComment];
        fTypeOfToken := tLCUnknown;
        break;
      end;
      inc(fCurPosInLine);
    end; // while Lines
  end; // else-if
end;

procedure TLCCompilador.CommentInLineProc;
begin
  fTypeOfToken := tLCComment;
  Inc(fCurPosInLine);
  while (IsLineEnd(fCurPosInLine) = false)
  and   (fLine[fCurPosInLine] <> '@') do
  begin
    Inc(fCurPosInLine);
  end;
  if  (fLine[fCurPosInLine] in ['@']) then
  begin
    Inc(fCurPosInLine);
  end;
end;

procedure TLCCompilador.CRProc;
begin
  fTypeOfToken := tLCSpace;
  inc(fCurPosInLine);
  if fLine[fCurPosInLine] = #10 then
  begin
    inc(fCurPosInLine);
  end;
end;

procedure TLCCompilador.IdentProc(isAttr : Boolean);
begin
  inc(fCurPosInLine);
  while fIdentifiers[fLine[fCurPosInLine]] do
  begin
    inc(fCurPosInLine);
  end;
  fTypeOfToken := IdentKind(isAttr);
end;

procedure TLCCompilador.ApostrofoProc;
begin
  inc(fCurPosInLine,3);
  fTypeOfToken := tLCString;
end;

procedure TLCCompilador.LFProc;
begin
  fTypeOfToken := tLCSpace;
  inc(fCurPosInLine);
end;

procedure TLCCompilador.NullProc;
begin
  fTypeOfToken := tLCNull;
  if fCurPosInLine < fSizeLine  then
  begin
    inc(fCurPosInLine);
  end;
end;

procedure TLCCompilador.MinusProc;
begin
  fTypeOfToken := tLCSymbol;

  inc(fCurPosInLine);
  if fNumberChar[fLine[fCurPosInLine]] = true then
  begin
    dec(fCurPosInLine);
    NumberProc;
  end;
end;

procedure TLCCompilador.NumberProc;
begin
  inc(fCurPosInLine);
  fTypeOfToken := tLCNumber;
  if fCurPosInLine < fSizeLine then
  begin
    while (fNumberChar[fLine[fCurPosInLine]]) do
    begin
      inc(fCurPosInLine);
    end;
    if  (fLine[fCurPosInLine] = '.')
    and (fLine[fCurPosInLine + 1] <> '.')  then
    begin
      inc(fCurPosInLine);
      while (fNumberChar[fLine[fCurPosInLine]]) do
      begin
        inc(fCurPosInLine);
      end;
    end;
    if (fLine[fCurPosInLine] = 'e')
    or (fLine[fCurPosInLine] = 'E')  then
    begin
      inc(fCurPosInLine);
      if (fLine[fCurPosInLine] = '+')
      or (fLine[fCurPosInLine] = '-')  then
      begin
        inc(fCurPosInLine);
      end;
      while (fNumberChar[fLine[fCurPosInLine]]) do
      begin
        inc(fCurPosInLine);
      end;
    end;
  end;
end;

procedure TLCCompilador.PointProc;
begin
  fTypeOfToken := tLCSymbol;
  inc(fCurPosInLine);
  if (fIdentifiers[fLine[fCurPosInLine]] = true) then
  begin
    fProcessing := fProcessing + [tpLCAttrName];
  end;

end;

function TLCCompilador.SintaxeDefinir : Boolean;
Var
  sOldToken,
  Token:String;
  iPosIniToken,
  iLinhaErro:Integer;
begin
  Result := true;

  if fTypeOfToken <> tLCDataType then
  begin
    iLinhaErro := fIndexCurLine + 1;
    iPosIniToken := fPosIniTok + 1;
    sOldToken := GetToken;

    NextToken;
    Token := GetToken;
    if Token = '.' then
    begin
      NextToken;

      while (IsEndOfLine = false)
      and   (fTypeOfToken <> tLCSpace) do
      begin
        if fTypeOfToken = tLCSymbol then
        begin
          Token := GetToken;
          if Token <> '.' then
          begin
            Result := False;
            iLinhaErro := fIndexCurLine + 1;
            iPosIniToken := fPosIniTok + 1;
            AdicionarMensagem(lcTMError, Format('Símbolo "%s" é Inválido neste local.', [GetToken]), iPosIniToken, iLinhaErro);

            exit;
          end;
        end;

        NextToken;
        if  IsEndOfLine = true then
        begin
          NextRow;
        end;
      end;

      SkipSpaces(True, False);

      if fTypeOfToken <> tLCIdentifier then
      begin
        Result := False;
        iLinhaErro := fIndexCurLine + 1;
        iPosIniToken := fPosIniTok + 1;
        AdicionarMensagem(lcTMError, Format('Identificador "%s" Inválido.', [GetToken]), iPosIniToken, iLinhaErro);

        exit;
      end;

      SkipSpaces(True, False);

      if fTypeOfToken = tLCSymbol then
      begin
        Token := GetToken;
        if Token <> ';' then
        begin
          Result := False;
          iLinhaErro := fIndexCurLine + 1;
          iPosIniToken := fPosIniTok + 1;
          AdicionarMensagem(lcTMError, Format('Símbolo "%s" é Inválido neste local. Era esperado um ";".', [GetToken]), iPosIniToken, iLinhaErro);

          exit;
        end;
      end;
    end
    else
    begin
      Result := False;
      AdicionarMensagem(lcTMError, Format('Tipo de Variável "%s" é Inválido.', [sOldToken]), iPosIniToken, iLinhaErro);

      exit;
    end;
  end
  else
  begin
    Token := AnsiUpperCase(GetToken);
    if SameText('FUNCAO', Token) then
    begin
      SkipSpaces(True, False);

      if SintaxeDefinirFuncao = false then
      begin
        Result := false;
        exit;
      end;
    end
    else
    if SameText('NUMERO', Token) then
    begin
      SkipSpaces(True, False);

      if SintaxeDefinirNumero = false then
      begin
        Result := false;
        exit;
      end;
    end
    else
    begin
      SkipSpaces(True, False);

      if fTypeOfToken <> tLCIdentifier then
      begin
        Result := False;
        iLinhaErro := fIndexCurLine + 1;
        iPosIniToken := fPosIniTok + 1;
        AdicionarMensagem(lcTMError, Format('Identificador "%s" Inválido.', [GetToken]), iPosIniToken, iLinhaErro);

        exit;
      end;

      SkipSpaces(True, False);
    end;

    Token := GetToken;
    if Token <> ';' then
    begin
      Result := False;
      iLinhaErro := fIndexCurLine + 1;
      iPosIniToken := fPosIniTok + 1;
      AdicionarMensagem(lcTMError, Format('Símbolo "%s" é Inválido neste local. Era esperado um ";".', [GetToken]), iPosIniToken, iLinhaErro);

      exit;
    end;
  end;
end;

function TLCCompilador.SintaxeDefinirFuncao : Boolean;
Var
  Token:String;
  bPrimeiro:Boolean;
  iPosIniToken,
  iLinhaErro:Integer;
begin
  Result := True;
  bPrimeiro := true;

  if not (fTypeOfToken in [tLCIdentifier, tLCCustomFunction]) then
  begin
    Result := False;
    iLinhaErro := fIndexCurLine + 1;
    iPosIniToken := fPosIniTok + 1;
    AdicionarMensagem(lcTMError, Format('Nome da Função "%s" Inválido.', [GetToken]), iPosIniToken, iLinhaErro);

    exit;
  end;

  SkipSpaces(True, False);

  if fTypeOfToken = tLCSymbol then
  begin
    Token := GetToken;
    if Token <> '(' then
    begin
      Result := False;
      iLinhaErro := fIndexCurLine + 1;
      iPosIniToken := fPosIniTok + 1;
      AdicionarMensagem(lcTMError, Format('Símbolo "%s" é Inválido neste local. Era esperado um "(".', [GetToken]), iPosIniToken, iLinhaErro);

      exit;
    end;

    SkipSpaces(True, False);

    while (IsEndOfLine = false) do
    begin
      Token := AnsiUpperCase(GetToken);
      if Token <> 'NUMERO' then
      begin
        if  (Token = ')')
        And (bPrimeiro = true) then
        begin
          SkipSpaces(True, False);
          break;
        end;

        Result := False;
        iLinhaErro := fIndexCurLine + 1;
        iPosIniToken := fPosIniTok + 1;
        AdicionarMensagem(lcTMError, Format('Tipo do Parâmetro esperado é "Número" mas foi encontrado "%s".', [GetToken]), iPosIniToken, iLinhaErro);

        exit;
      end;
      bPrimeiro := false;

      SkipSpaces(True, False);
      Token := AnsiUpperCase(GetToken);
      if Token = 'END' then
      begin
        SkipSpaces(True, False);
      end;

      if fTypeOfToken <> tLCIdentifier then
      begin
        Result := False;
        iLinhaErro := fIndexCurLine + 1;
        iPosIniToken := fPosIniTok + 1;
        AdicionarMensagem(lcTMError, Format('Nome do Parâmetro "%s" Inválido.', [GetToken]), iPosIniToken, iLinhaErro);

        exit;
      end;

      SkipSpaces(True, False);

      if fTypeOfToken = tLCSymbol then
      begin
        Token := GetToken;
        if Token = ')' then
        begin
          SkipSpaces(True, False);
          break;
        end
        else
        if Token <> ',' then
        begin
          Result := False;
          iLinhaErro := fIndexCurLine + 1;
          iPosIniToken := fPosIniTok + 1;
          AdicionarMensagem(lcTMError, Format('Símbolo "%s" é Inválido neste local. Era aguardado uma ",".', [GetToken]), iPosIniToken, iLinhaErro);

          exit;
        end;
      end
      else
      begin
        Result := False;
        iLinhaErro := fIndexCurLine + 1;
        iPosIniToken := fPosIniTok + 1;
        AdicionarMensagem(lcTMError, Format('Identificador "%s" Inválido.', [GetToken]), iPosIniToken, iLinhaErro);

        exit;
      end;

      SkipSpaces(True, False);
    end;
  end;
end;

function TLCCompilador.SintaxeDefinirNumero : Boolean;
Var
  Token:String;
  iPosIniToken,
  iLinhaErro:Integer;
begin
  Result := True;

  if fTypeOfToken <> tLCIdentifier then
  begin
    Result := False;
    iLinhaErro := fIndexCurLine + 1;
    iPosIniToken := fPosIniTok + 1;
    AdicionarMensagem(lcTMError, Format('Nome "%s" para Variável é Inválido.', [GetToken]), iPosIniToken, iLinhaErro);

    exit;
  end;

  SkipSpaces(True, False);

  if fTypeOfToken = tLCSymbol then
  begin
    Token := GetToken;
    if Token = ';' then
    begin
      exit;
    end;

    if Token = '[' then
    begin
      SkipSpaces(True, False);

      if fTypeOfToken <> tLCNumber then
      begin
        Result := False;
        iLinhaErro := fIndexCurLine + 1;
        iPosIniToken := fPosIniTok + 1;
        AdicionarMensagem(lcTMError, Format('Número "%s" Inválido.', [GetToken]), iPosIniToken, iLinhaErro);

        exit;
      end;

      SkipSpaces(True, False);
      Token := GetToken;
      if Token <> ']' then
      begin
        Result := False;
        iLinhaErro := fIndexCurLine + 1;
        iPosIniToken := fPosIniTok + 1;
        AdicionarMensagem(lcTMError, Format('Símbolo "%s" Inválido. Era esperado "]".', [GetToken]), iPosIniToken, iLinhaErro);

        exit;
      end;

      SkipSpaces(True, False);
    end;
  end;
end;

function TLCCompilador.SintaxeSe : Boolean;
Var
  Token:String;
  iIndex,
  iLinhaErro,
  iPosIniToken:integer;
begin
  Result := True;
  if fTypeOfToken = tLCSymbol then
  begin
    Token := GetToken;
    if Token <> '(' then
    begin
      Result := False;
      iLinhaErro := fIndexCurLine + 1;
      iPosIniToken := fPosIniTok + 1;
      AdicionarMensagem(lcTMError, Format('Esperava-se "(" para foi encontrado "%s".', [GetToken]), iPosIniToken, iLinhaErro);

      exit;
    end;
  end;

  AdicionarBloco(lcTBParenteses, fIndexCurLine + 1, fPosIniTok + 1);
  SkipSpaces(True, False);

  while (IsEndOfLine = false) do
  begin
    Token := UpperCase(GetToken);
    Case Token of
      '(':
      begin
        AdicionarBloco(lcTBParenteses, fIndexCurLine + 1, fPosIniTok + 1);
      end;
      ')':
      begin
        if RemoverBloco(lcTBParenteses) = false then
        begin
          Result := False;
          iLinhaErro := fIndexCurLine + 1;
          iPosIniToken := fPosIniTok + 1;
          AdicionarMensagem(lcTMError, 'Tentativa de Fechar Parenteses antes de Abrir', iPosIniToken, iLinhaErro);

          exit;
        end;

        iIndex := High(fBlocos);
        if (iIndex = -1)
        or (fBlocos[iIndex].Tipo <> lcTBParenteses) then
        begin
          break;
        end;

        SkipSpaces(True, False);
        Token := UpperCase(GetToken);
        if Token = ')' then
        begin
          Continue;
        end;

        if  (Token <> 'E')
        and (Token <> 'OU')
        and (fTypeOfToken <> tLCSymbol) then
        begin
          Result := False;
          iLinhaErro := fIndexCurLine + 1;
          iPosIniToken := fPosIniTok + 1;
          AdicionarMensagem(lcTMError, Format('Esperava-se "E" ou "OU" para foi encontrado "%s".', [GetToken]), iPosIniToken, iLinhaErro);

          exit;
        end;
      end;
      ';',
      'INICIO':
      begin
        Result := False;
        iLinhaErro := fIndexCurLine + 1;
        iPosIniToken := fPosIniTok + 1;
        AdicionarMensagem(lcTMError, Format('Esperava-se ")" para foi encontrado "%s".', [GetToken]), iPosIniToken, iLinhaErro);

        exit;
      end;
    end;

    SkipSpaces(True, False);
  end;
end;

function TLCCompilador.SintaxeEnquanto : Boolean;
Var
  Token:String;
  iIndex,
  iPosIniToken,
  iLinhaErro:Integer;
begin
  Result := True;
  if fTypeOfToken = tLCSymbol then
  begin
    Token := GetToken;
    if Token <> '(' then
    begin
      Result := False;
      iLinhaErro := fIndexCurLine + 1;
      iPosIniToken := fPosIniTok + 1;
      AdicionarMensagem(lcTMError, Format('Esperava-se "(" para foi encontrado "%s".', [GetToken]), iPosIniToken, iLinhaErro);

      exit;
    end;
  end;

  AdicionarBloco(lcTBParenteses, fIndexCurLine + 1, fPosIniTok + 1);
  SkipSpaces(True, False);

  while (IsEndOfLine = false) do
  begin
    Token := UpperCase(GetToken);
    Case Token of
      '(':
      begin
        AdicionarBloco(lcTBParenteses, fIndexCurLine + 1, fPosIniTok + 1);
      end;
      ')':
      begin
        if RemoverBloco(lcTBParenteses) = false then
        begin
          Result := False;
          iLinhaErro := fIndexCurLine + 1;
          iPosIniToken := fPosIniTok + 1;
          AdicionarMensagem(lcTMError, 'Tentativa de Fechar Parenteses antes de Abrir', iPosIniToken, iLinhaErro);

          exit;
        end;

        iIndex := High(fBlocos);
        if (iIndex = -1)
        or (fBlocos[iIndex].Tipo <> lcTBParenteses) then
        begin
          break;
        end;

        SkipSpaces(True, False);
        Token := UpperCase(GetToken);
        if Token = ')' then
        begin
          Continue;
        end;

        if  (Token <> 'E')
        and (Token <> 'OU')
        and (fTypeOfToken <> tLCSymbol) then
        begin
          Result := False;
          iLinhaErro := fIndexCurLine + 1;
          iPosIniToken := fPosIniTok + 1;
          AdicionarMensagem(lcTMError, Format('Esperava-se "E" ou "OU" para foi encontrado "%s".', [GetToken]), iPosIniToken, iLinhaErro);

          exit;
        end;
      end;
      ';',
      'INICIO':
      begin
        Result := False;
        iLinhaErro := fIndexCurLine + 1;
        iPosIniToken := fPosIniTok + 1;
        AdicionarMensagem(lcTMError, Format('Esperava-se ")" para foi encontrado "%s".', [GetToken]), iPosIniToken, iLinhaErro);

        exit;
      end;
    end;

    SkipSpaces(True, False);
  end;
end;

function TLCCompilador.SintaxeParenteses : Boolean;
Var
  Token:String;
  iIndex,
  iPosIniToken,
  iLinhaErro:Integer;
begin
  Result := True;
  while (IsEndOfLine = false) do
  begin
    Token := UpperCase(GetToken);
    Case Token of
      '(':
      begin
        AdicionarBloco(lcTBParenteses, fIndexCurLine + 1, fPosIniTok + 1);
      end;
      ')':
      begin
        if RemoverBloco(lcTBParenteses) = false then
        begin
          Result := False;
          iLinhaErro := fIndexCurLine + 1;
          iPosIniToken := fPosIniTok + 1;
          AdicionarMensagem(lcTMError, 'Tentativa de Fechar Parenteses antes de Abrir', iPosIniToken, iLinhaErro);

          exit;
        end;

        iIndex := High(fBlocos);
        if (iIndex = -1)
        or (fBlocos[iIndex].Tipo <> lcTBParenteses) then
        begin
          break;
        end;
      end;
      ';',
      '{',
      '}',
      'SE',
      'ENQUANTO',
      'INICIO',
      'FIM':
      begin
        Result := False;
        iLinhaErro := fIndexCurLine + 1;
        iPosIniToken := fPosIniTok + 1;
        AdicionarMensagem(lcTMError, Format('Esperava-se ")" para foi encontrado "%s".', [GetToken]), iPosIniToken, iLinhaErro);

        exit;
      end;
    end;

    SkipSpaces(True, False);
  end;
end;

function TLCCompilador.SintaxePara : Boolean;
Var
  Token:String;
  iIndex,
  iPosIniToken,
  iLinhaErro:Integer;
begin
  Result := True;
  if fTypeOfToken = tLCSymbol then
  begin
    Token := GetToken;
    if Token <> '(' then
    begin
      Result := False;
      iLinhaErro := fIndexCurLine + 1;
      iPosIniToken := fPosIniTok + 1;
      AdicionarMensagem(lcTMError, Format('Esperava-se "(" para foi encontrado "%s".', [GetToken]), iPosIniToken, iLinhaErro);

      exit;
    end;
  end;

  AdicionarBloco(lcTBParenteses, fIndexCurLine + 1, fPosIniTok + 1);
  SkipSpaces(True, False);

  while (IsEndOfLine = false) do
  begin
    Token := UpperCase(GetToken);
    Case Token of
      '(':
      begin
        AdicionarBloco(lcTBParenteses, fIndexCurLine + 1, fPosIniTok + 1);
      end;
      ')':
      begin
        if RemoverBloco(lcTBParenteses) = false then
        begin
          Result := False;
          iLinhaErro := fIndexCurLine + 1;
          iPosIniToken := fPosIniTok + 1;
          AdicionarMensagem(lcTMError, 'Tentativa de Fechar Parenteses antes de Abrir', iPosIniToken, iLinhaErro);

          exit;
        end;

        iIndex := High(fBlocos);
        if (iIndex = -1)
        or (fBlocos[iIndex].Tipo <> lcTBParenteses) then
        begin
          break;
        end;
      end;
      '{',
      '}',
      'SE',
      'ENQUANTO',
      'INICIO',
      'FIM':
      begin
        Result := False;
        iLinhaErro := fIndexCurLine + 1;
        iPosIniToken := fPosIniTok + 1;
        AdicionarMensagem(lcTMError, Format('Esperava-se ")" para foi encontrado "%s".', [GetToken]), iPosIniToken, iLinhaErro);

        exit;
      end;
    end;

    SkipSpaces(True, False);
  end;
end;

function TLCCompilador.SintaxeFuncao(pTypeOfToken : TLCTokenKind) : Boolean;
Var
  Token:String;
  iExpressao,
  iIndex,
  iPosIniToken,
  iLinhaErro:Integer;
  bFoiVirgula:Boolean;
begin
  Result := True;
  bFoiVirgula := False;
  iExpressao := 0;

  if fTypeOfToken = tLCSymbol then
  begin
    Token := GetToken;
    if Token <> '(' then
    begin
      Result := False;
      iLinhaErro := fIndexCurLine + 1;
      iPosIniToken := fPosIniTok + 1;
      AdicionarMensagem(lcTMError, Format('Esperava-se "(" para foi encontrado "%s".', [Token]), iPosIniToken, iLinhaErro);

      exit;
    end;
  end;

  AdicionarBloco(lcTBParenteses, fIndexCurLine + 1, fPosIniTok + 1);
  SkipSpaces(True, False);

  while (IsEndOfLine = false) do
  begin
    Token := UpperCase(GetToken);
    Case Token of
      ',':
      begin
        bFoiVirgula := true;
      end;
      '(':
      begin
        AdicionarBloco(lcTBParenteses, fIndexCurLine + 1, fPosIniTok + 1);
        inc(iExpressao);
      end;
      ')':
      begin
        if bFoiVirgula = true then
        begin
          Result := False;
          iLinhaErro := fIndexCurLine + 1;
          iPosIniToken := fPosIniTok + 1;
          AdicionarMensagem(lcTMError, 'Faltou informar parâmetro.', iPosIniToken, iLinhaErro);

          exit;
        end;

        if RemoverBloco(lcTBParenteses) = false then
        begin
          Result := False;
          iLinhaErro := fIndexCurLine + 1;
          iPosIniToken := fPosIniTok + 1;
          AdicionarMensagem(lcTMError, 'Tentativa de Fechar Parenteses antes de Abrir', iPosIniToken, iLinhaErro);

          exit;
        end;

        iIndex := High(fBlocos);
        if (iIndex = -1)
        or (fBlocos[iIndex].Tipo <> lcTBParenteses) then
        begin
          break;
        end;
        dec(iExpressao);
      end;
      else
      begin
        bFoiVirgula := False;
        if ((pTypeOfToken in [tLCKey,tLCReservedWord]) and not (fTypeOfToken in [tLCKey, tLCString, tLCNumber, tLCIdentifier, tLCVariable]))
        or ((pTypeOfToken = tLCAttributeName) and not (fTypeOfToken in [tLCDataType, tLCString, tLCNumber, tLCIdentifier, tLCVariable]))
        or ((pTypeOfToken in [tLCCustomFunction, tLCIdentifier]) and not (fTypeOfToken in [tLCKey, tLCNumber, tLCIdentifier, tLCVariable])) then
        begin
          if ((pTypeOfToken = tLCKey) and (AnsiUpperCase(GetToken) <> 'GRAVAR')) then
          begin
            Result := False;
            iLinhaErro := fIndexCurLine + 1;
            iPosIniToken := fPosIniTok + 1;
            AdicionarMensagem(lcTMError, 'Parâmetro inválido.', iPosIniToken, iLinhaErro);

            exit;
          end;
        end;

        SkipSpaces(True, (fTypeOfToken = tLCString));
        Token := GetToken;
        if fTypeOfToken <> tLCSymbol then
        begin
          Result := False;
          iLinhaErro := fIndexCurLine + 1;
          iPosIniToken := fPosIniTok + 1;
          AdicionarMensagem(lcTMError, Format('Símbolo "%s" inválido.',[Token]), iPosIniToken, iLinhaErro);

          exit;
        end;

        if Token = '.' then
        begin
          SkipSpaces(True, False);

          if fTypeOfToken <> tLCIdentifier then
          begin
            Result := False;
            iLinhaErro := fIndexCurLine + 1;
            iPosIniToken := fPosIniTok + 1;
            AdicionarMensagem(lcTMError, Format('Identificador "%s" inválido.',[GetToken]), iPosIniToken, iLinhaErro);

            exit;
          end;
        end;

        if iExpressao = 0 then
        begin
          if (Token = '+')
          or (Token = '-')
          or (Token = '/')
          or (Token = '*') then
          begin
            SkipSpaces(True, False);

            if not (fTypeOfToken in [tLCIdentifier,tLCNumber]) then
            begin
              Result := False;
              iLinhaErro := fIndexCurLine + 1;
              iPosIniToken := fPosIniTok + 1;
              AdicionarMensagem(lcTMError, Format('Identificador ou número "%s" inválido.',[GetToken]), iPosIniToken, iLinhaErro);

              exit;
            end;

            continue;
          end;

          if Token <> ',' then
          begin
            continue;
          end;

          bFoiVirgula := true;
        end
        else
        begin
          if  Token = ')' then
          begin
            continue;
          end;
        end;
      end;
    end;

    SkipSpaces(True, False);
  end;
end;

procedure TLCCompilador.SpaceProc;
begin
  inc(fCurPosInLine);
  fTypeOfToken := tLCSpace;
  while fSpaceChar[fLine[fCurPosInLine]] do
  begin
    inc(fCurPosInLine);
  end;
end;

procedure TLCCompilador.StringProc;
begin
  fTypeOfToken := tLCString;
  fProcessing := fProcessing - [tpLCString];

  Inc(fCurPosInLine);
  While (IsLineEnd(fCurPosInLine) = false) do
  begin
    case (fLine[fCurPosInLine]) of
      #34:
        begin
          Inc(fCurPosInLine);
          if (fLine[fCurPosInLine] = #34) then
          begin
            Inc(fCurPosInLine);
          end
          else
          begin
            break;
          end;
        end;
      #92:
      begin
        Inc(fCurPosInLine);
        if not (fLine[fCurPosInLine] in [#34, #92]) then   // \" \\
        begin
          fProcessing := fProcessing + [tpLCString];
          break;
        end;
        Inc(fCurPosInLine);
      end;
      else
      begin
        Inc(fCurPosInLine);
      end;
    end; // case
  end; // while
end;

procedure TLCCompilador.StringMutlipeLinesProc;
begin
  fTypeOfToken := tLCString;
  while not IsLineEnd(fCurPosInLine) do
  begin
    case fLine[fCurPosInLine] of
      #34:  // #34 = "
      begin
        if (fLine[fCurPosInLine + 1] = #34) then
        begin
          Inc(fCurPosInLine, 2);
        end
        else
        begin
          inc(fCurPosInLine);
          fProcessing := fProcessing - [tpLCString];
          break;
        end;
      end;
      #92: // #92 = \
      begin
        Inc(fCurPosInLine);
        if (fLine[fCurPosInLine] in [#34, #92]) then   // \" \\
        begin
          Inc(fCurPosInLine);
        end;
      end;
      else
      begin
        inc(fCurPosInLine);
      end;
    end; // case
  end; // while
end;

procedure TLCCompilador.SymbolProc;
begin
  inc(fCurPosInLine);
  fTypeOfToken := tLCSymbol;
end;

procedure TLCCompilador.UnknownProc;
begin
  inc(fCurPosInLine);
  While (fLine[fCurPosInLine] in [#128..#191]) // continued utf8 subcode
  or    (fLine[fCurPosInLine] <> #0) do
  begin
    inc(fCurPosInLine);
  end;
  fTypeOfToken := tLCUnknown;
end;

end.

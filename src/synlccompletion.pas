unit SynLCCompletion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LCLProc,
  Fgl, LCLType, LazUTF8, Menus,
  SynEdit, SynEditPlugins,
  SynEditKeyCmds, SynCompletion;

type

  { TLCCompletionItem }

  TLCCompletionItem = class
  private
    FCaption : string;
    fColorKind : TColor;
    FDescKind : string;
    FDescription : string;
    FTextToReplace : string;
  public
    property Caption: string read FCaption write FCaption;
    property TextToReplace: string read FTextToReplace write FTextToReplace;
    property Description: string read FDescription write FDescription;
    property DescKind: string read FDescKind write FDescKind;
    property ColorKind: TColor read fColorKind write fColorKind;
    function StartWith(const c: char): boolean; inline;
    function StartWithU(const c: char): boolean; inline;
  end;

  TLCCompletionItems = specialize TFPGObjectList<TLCCompletionItem>;

  { TLCCompletionList }

  TLCCompletionList = class
  private
    fBiggerCaption : String;
    fBiggestDescKind : String;
    fItems: TLCCompletionItems;
    procedure AddItem(pCaption:String; pDescKind:String; pColorKind: TColor; pTextToReplace:String = ''; pDescription:String = '');
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Items: TLCCompletionItems read fItems;
    property BiggestCaption:String read fBiggerCaption;
    property BiggestDescKind:String read fBiggestDescKind;
  end;


  { TSynLCCompletion }

  TSynLCCompletion = class(TSynCompletion)
    private
      fItems: TLCCompletionList;
      function GetItemListCount: integer;

      function OnLCSynCompletionPaintItem(const {%H-}AKey: string; ACanvas: TCanvas; X,
        Y: integer; {%H-}IsSelected: boolean; Index: integer): boolean;

      procedure OnLCSynCompletionCodeCompletion(var Value: string; {%H-}SourceValue: string;
        var {%H-}SourceStart, {%H-}SourceEnd: TPoint; {%H-}KeyChar: TUTF8Char; {%H-}Shift: TShiftState);

    protected
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      procedure AddItem(pCaption : String; pDescKind : String; pColorKind : TColor; pTextToReplace : String = '';
        pDescription : String = '');
      procedure Clear;

      property Items: TLCCompletionList read fItems;
    published
  end;

  { TSynLCAutoComplete }

  TSynLCAutoComplete = class(TLazSynMultiEditPlugin)
   private
     FExecCommandID: TSynEditorCommand;
     FShortCut: TShortCut;
     fAutoCompleteList: TStrings;
     FEndOfTokenChr: string;
     procedure SetAutoCompleteList(List: TStrings);
   protected
     procedure DoEditorAdded(AValue: TCustomSynEdit); override;
     procedure DoEditorRemoving(AValue: TCustomSynEdit); override;
     procedure SetShortCut(Value: TShortCut);
     function GetPreviousToken(aEditor: TCustomSynEdit): string;
     procedure TranslateKey(Sender: TObject; Code: word; SState: TShiftState;
       var {%H-}Data: pointer; var {%H-}IsStartOfCombo: boolean; var Handled: boolean;
       var Command: TSynEditorCommand; FinishComboOnly: Boolean;
       var {%H-}ComboKeyStrokes: TSynEditKeyStrokes);
     procedure ProcessSynCommand(Sender: TObject; {%H-}AfterProcessing: boolean;
               var Handled: boolean; var Command: TSynEditorCommand;
               var {%H-}AChar: TUTF8Char; {%H-}Data: pointer; {%H-}HandlerData: pointer);
   public
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     procedure Execute(token: string; aEditor: TCustomSynEdit); virtual;
     function EditorsCount: integer;
     function GetTokenList: string;
     function GetTokenValue(Token: string): string;
   published
     property AutoCompleteList: TStrings read fAutoCompleteList
       write SetAutoCompleteList;
     property EndOfTokenChr: string read FEndOfTokenChr write FEndOfTokenChr;
     property ShortCut: TShortCut read FShortCut write SetShortCut;
     property ExecCommandID: TSynEditorCommand read FExecCommandID write FExecCommandID;
     property Editor;
  end;

implementation

Uses
  SynLCHighlighter;

{ TSynLCAutoComplete }

procedure TSynLCAutoComplete.SetAutoCompleteList(List: TStrings);
begin
  fAutoCompleteList.Assign(List);
end;

procedure TSynLCAutoComplete.DoEditorAdded(AValue: TCustomSynEdit);
begin
  inherited DoEditorAdded(AValue);
  AValue.RegisterCommandHandler(@ProcessSynCommand, nil);
  AValue.RegisterKeyTranslationHandler(@TranslateKey);
end;

procedure TSynLCAutoComplete.DoEditorRemoving(AValue: TCustomSynEdit);
begin
  inherited DoEditorRemoving(AValue);
  AValue.UnregisterCommandHandler(@ProcessSynCommand);
  AValue.UnRegisterKeyTranslationHandler(@TranslateKey);
end;

procedure TSynLCAutoComplete.SetShortCut(Value: TShortCut);
begin
  FShortCut := Value;
end;

function TSynLCAutoComplete.GetPreviousToken(aEditor: TCustomSynEdit): string;
var
  s: string;
  i: integer;
begin
  if aEditor <> nil then begin
    s := aEditor.LineText;
    i := aEditor.LogicalCaretXY.X - 1;
    if i > length(s) then
      result := ''
    else begin
      while (i > 0) and (s[i] > ' ') and (pos(s[i], FEndOfTokenChr) = 0) do
        dec(i);
      result := copy(s, i + 1, aEditor.LogicalCaretXY.X - i - 1);
    end;
  end
  else
    result := '';
end;

procedure TSynLCAutoComplete.TranslateKey(Sender: TObject; Code: word;
  SState: TShiftState; var Data: pointer; var IsStartOfCombo: boolean;
  var Handled: boolean; var Command: TSynEditorCommand;
  FinishComboOnly: Boolean; var ComboKeyStrokes: TSynEditKeyStrokes);
var
  i: integer;
  ShortCutKey: Word;
  ShortCutShift: TShiftState;
begin
  if (Code = VK_UNKNOWN) or Handled or FinishComboOnly or (FExecCommandID = ecNone) then exit;

  i := IndexOfEditor(Sender as TCustomSynEdit);
  if i >= 0 then begin
    ShortCutToKey(FShortCut, ShortCutKey, ShortCutShift);
    if (SState = ShortCutShift) and (Code = ShortCutKey) then begin
      Command := FExecCommandID;
      Handled := True;
    end;
  end;
end;

procedure TSynLCAutoComplete.ProcessSynCommand(Sender: TObject;
  AfterProcessing: boolean; var Handled: boolean;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
  HandlerData: pointer);
var
  i: integer;
begin
  if Handled or (Command <> FExecCommandID) then
    exit;

  i := IndexOfEditor(Sender as TCustomSynEdit);
  if i >= 0 then begin
    with sender as TCustomSynEdit do begin
      if not ReadOnly then begin
        Editor := Sender as TCustomSynEdit; // Will set Form.SetCurrentEditor
        Execute(GetPreviousToken(Sender as TCustomSynEdit), Sender as TCustomSynEdit);
        Handled := True;
      end;
    end;
  end;
end;

constructor TSynLCAutoComplete.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEndOfTokenChr := '()[].';
  fAutoCompleteList := TStringList.Create;
  fShortCut := Menus.ShortCut(Ord(' '), [ssShift]);
  FExecCommandID := ecSynAutoCompletionExecute;
end;

destructor TSynLCAutoComplete.Destroy;
begin
  FreeAndNil(fAutoCompleteList);
  inherited Destroy;
end;

procedure TSynLCAutoComplete.Execute(token: string; aEditor: TCustomSynEdit);
var
  Temp: string;
  PosX, // llutti
  i, j, prevspace: integer;
  StartOfBlock: tpoint;
begin
//Writeln('[TSynAutoComplete.Execute] Token is "',Token,'"');
  i := AutoCompleteList.IndexOf(token);
  if i <> -1 then begin
    for j := 1 to length(token) do
      aEditor.CommandProcessor(ecDeleteLastChar, ' ', nil);
    inc(i);
    PosX := aEditor.CaretX; // llutti
    StartOfBlock := Point(-1, -1);
    PrevSpace := 0;
    while (i < AutoCompleteList.Count) and
      (length(AutoCompleteList[i]) > 0) and
      (AutoCompleteList[i][1] = '=') do
    begin
      aEditor.CaretX := PosX; // llutti
      for j := 0 to PrevSpace - 1 do
        aEditor.CommandProcessor(ecDeleteLastChar, ' ', nil);
      Temp := AutoCompleteList[i];
      PrevSpace := 0;
      //while (length(temp) >= PrevSpace + 2) and (temp[PrevSpace + 2] <= ' ') do   // llutti
      //  inc(PrevSpace);         // llutti
      for j := 2 to length(Temp) do begin
        aEditor.CommandProcessor(ecChar, Temp[j], nil);
        if Temp[j] = '|' then
          StartOfBlock := aEditor.CaretXY
      end;
      inc(i);
      if (i < AutoCompleteList.Count) and
        (length(AutoCompleteList[i]) > 0) and
        (AutoCompleteList[i][1] = '=') then
        aEditor.CommandProcessor(ecLineBreak, ' ', nil);
    end;
    if (StartOfBlock.x <> -1) and (StartOfBlock.y <> -1) then begin
      aEditor.CaretXY := StartOfBlock;
      aEditor.CommandProcessor(ecDeleteLastChar, ' ', nil);
    end;
  end;
end;

function TSynLCAutoComplete.EditorsCount: integer;
begin
  Result := EditorCount;
end;

function TSynLCAutoComplete.GetTokenList: string;
var
  List: TStringList;
  i: integer;
begin
  Result := '';
  if AutoCompleteList.Count < 1 then Exit;
  List := TStringList.Create;
  i := 0;
  while (i < AutoCompleteList.Count) do begin
    if (length(AutoCompleteList[i]) > 0) and (AutoCompleteList[i][1] <> '=') then
      List.Add(Trim(AutoCompleteList[i]));
    inc(i);
  end;
  Result := List.Text;
  List.Free;
end;

function TSynLCAutoComplete.GetTokenValue(Token: string): string;
var
  i: integer;
  List: TStringList;
begin
  Result := '';
  i := AutoCompleteList.IndexOf(Token);
  if i <> -1 then begin
    List := TStringList.Create;
    Inc(i);
    while (i < AutoCompleteList.Count) and
      (length(AutoCompleteList[i]) > 0) and
      (AutoCompleteList[i][1] = '=') do begin
      if Length(AutoCompleteList[i]) = 1 then
        List.Add('')
      else
        List.Add(Copy(AutoCompleteList[i], 2, Length(AutoCompleteList[i])));
      inc(i);
    end;
    Result := List.Text;
    List.Free;
  end;
end;

{ TLCCompletionList }

procedure TLCCompletionList.AddItem(pCaption : String; pDescKind : String; pColorKind : TColor;
  pTextToReplace : String; pDescription : String);
var
  item:TLCCompletionItem;
begin
  if pCaption.IsEmpty then
  begin
    exit;
  end;

  item := TLCCompletionItem.Create;
  item.Caption := pCaption;
  item.Description := pDescription;
  item.TextToReplace := pTextToReplace;
  item.DescKind := pDescKind;
  item.ColorKind := pColorKind;

  if length(fBiggestDescKind) < length(pDescKind) then
  begin
    fBiggestDescKind := pDescKind;
  end;
  if length(fBiggerCaption) < length(pCaption) then
  begin
    fBiggerCaption := pCaption;
  end;

  fItems.Add(item);
end;

constructor TLCCompletionList.Create;
begin
  fBiggerCaption := '';
  fBiggestDescKind := '';

  fItems:= TLCCompletionItems.Create(true);
end;

destructor TLCCompletionList.Destroy;
begin
  fItems.Destroy;

  inherited Destroy;
end;

procedure TLCCompletionList.Clear;
begin
  fBiggerCaption := '';
  fBiggestDescKind := '';
  fItems.Clear;
end;

{ TLCCompletionItem }

function TLCCompletionItem.StartWith(const c : char) : boolean;
begin
  Result := (fCaption<>'') and (fCaption[1] = c);
end;

function TLCCompletionItem.StartWithU(const c : char) : boolean;
begin
  Result := (fCaption<>'') and (UpCase(fCaption[1]) = c);
end;

{ TSynLCCompletion }

function TSynLCCompletion.GetItemListCount: integer;
begin
  result := ItemList.Count;
end;

function TSynLCCompletion.OnLCSynCompletionPaintItem(const AKey : string; ACanvas : TCanvas; X, Y : integer;
  IsSelected : boolean; Index : integer) : boolean;
var
  item: TLCCompletionItem;
begin
  Result := true;
  if Index > fItems.Items.Count then
  begin
    ACanvas.Font.Color :=  clBlack;
    ACanvas.TextOut(X+10, Y, ItemList[Index]);

    exit;
  end;

  item := fItems.Items[Index];

  // Listar o tipo
  ACanvas.Font.Style := [];
  ACanvas.Font.Color := item.ColorKind;
  ACanvas.TextOut(X+2, Y, item.DescKind);

  // Listar o conteudo
  x += ACanvas.TextWidth(fItems.BiggestDescKind);
  ACanvas.Font.Style := [];
  ACanvas.Font.Color :=  clBlack;
  ACanvas.TextOut(X+5, Y, item.Caption);
end;

procedure TSynLCCompletion.OnLCSynCompletionCodeCompletion(var Value : string; SourceValue : string; var SourceStart,
  SourceEnd : TPoint; KeyChar : TUTF8Char; Shift : TShiftState);
var
  item: TLCCompletionItem;
begin
  item := fItems.Items[Self.Position];
  Value := item.TextToReplace;
  if Value.IsEmpty then
  begin
    Value := item.Caption;
  end;
end;

constructor TSynLCCompletion.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  fItems:= TLCCompletionList.Create;

  TheForm.width := 200;
  TheForm.Font.Quality := fqCleartype;
  TheForm.ShowSizeDrag := True;
  TheForm.ClSelect := $00FFF7E6;

  OnPaintItem := @OnLCSynCompletionPaintItem;
  OnCodeCompletion := @OnLCSynCompletionCodeCompletion;
end;

destructor TSynLCCompletion.Destroy;
begin
  fItems.Destroy;

  inherited Destroy;
end;

procedure TSynLCCompletion.AddItem(pCaption: String; pDescKind: String; pColorKind:TColor; pTextToReplace: String; pDescription: String);
begin
  ItemList.Add(pCaption + '  ' + pDescKind);
  fItems.AddItem(pCaption, pDescKind, pColorKind, pTextToReplace, pDescription);
  //TheForm.width := TheForm.Canvas.TextWidth(pCaption + '  ' + pDescKind) + 40;
end;

procedure TSynLCCompletion.Clear;
begin
  ItemList.Clear;
  fItems.Clear;
end;

end.


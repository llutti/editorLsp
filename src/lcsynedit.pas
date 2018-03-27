unit LCSynEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, controls, graphics
  , Forms // Unit necessária devido ao MessageBox
  , LCLType, SynEdit
  , SynPluginMultiCaret
  , SynEditKeyCmds, SynEditMouseCmds;

resourcestring
  rsUntitled = 'Sem Nome %d';
  rsFileOverride = 'O Arquivo "%s" ja existe. Deseja Sobrescrever?';

type
  TLCSiglaModuloVetorh = (smvNone, smvBS, smvCS, smvFP, smvHR, smvRS, smvSM, smvSP, smvTR);
  TLCSiglaModulosVetorh = set of TLCSiglaModuloVetorh;

Const
  AbreviaturaModuloVetorh : Array[TLCSiglaModuloVetorh] of string = ('', 'BS', 'CS', 'Rubi', 'Ronda', 'RS', 'SM', 'Acesso', 'TR');

type

  { TLCEditorHintWindow }
  TLCEditorHintWindow = class(THintWindow)
  private
    fCurrentIndex:integer;

    procedure SetCurrentIndex(AValue : integer);
  public
    function CalcHintRect(MaxWidth: Integer; const AHint: string;
      AData: Pointer): TRect; override;
    procedure Paint; override;
    property CurrentIndex:integer read fCurrentIndex write SetCurrentIndex;
  end;

  TLCGetTipOfEvent = procedure(Sender: TObject; aWord: string; const aRow:Integer; var aTip:string) of object;

  { TLCSynEdit }

  TLCSynEdit = class(TSynEdit)
    private
      FLastTimeSaved : TDateTime;
      fLineError : Integer;
      FMultiCaret : TSynPluginMultiCaret;
      fUniqueIndex:Integer;
      fIsNew: Boolean;
      fSaved: Boolean;
      fFileName: String;
      FSaveDialog: TSaveDialog;
      FFileCode: String;

      fOnGetTipOfEvent: TLCGetTipOfEvent;

      fCurrentTipTopRow,
      fCurrentTipBottomRow:Integer;
      fLastTipText:string;
      fCallTipWin: TLCEditorHintWindow;

      fSiglaModulo: TLCSiglaModuloVetorh;
      fPosicaoRegra: Integer;

      procedure InitHintWins;
      procedure showCallTips(const tips: string; const CurrentIndex:Integer = 0);
      function GetTipToWord(aValue:string; const aRow:Integer):string;

    protected
      procedure DoExit; override;
      procedure DoOnProcessCommand(var Command: TSynEditorCommand; var AChar: TUTF8Char;
        Data: pointer); override;
      procedure UTF8KeyPress(var Key: TUTF8Char); override;
      procedure KeyDown(var Key: Word; Shift: TShiftState); override;
      procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
      procedure MouseLeave; override;
      procedure SetVisible(Value: Boolean); override;

      procedure DoMoveRowsSelected(aMoveDown:Boolean);

    public
      constructor Create(aOwner:TComponent); override;
      constructor Create(aOwner: TComponent; aUniqueIndex:Integer; aFileName:String = '');

      destructor destroy; override;

      procedure showCallTips(aInternal:Boolean = false);
      procedure hideCallTips;

      function Save:Boolean;
      function SaveAs(NmArq:String; askConfirmation:boolean = true):Boolean;
      procedure LoadFromFile(NomeArq:String);

      property LineError:Integer read fLineError write fLineError;

    published
      property SaveDialog:TSaveDialog read FSaveDialog write FSaveDialog;
      property IsNew:Boolean read fIsNew;
      property Saved:Boolean read fSaved;
      property FileName:String read fFileName;
      property UniqueIndex:Integer read fUniqueIndex;
      property LastTimeSaved:TDateTime read FLastTimeSaved;
      property FileCode:String read fFileCode;

      property ModuloVetorh:TLCSiglaModuloVetorh read fSiglaModulo write fSiglaModulo;
      property PosicaoRegra:Integer read fPosicaoRegra write fPosicaoRegra;

      property OnGetTipOf: TLCGetTipOfEvent read fOnGetTipOfEvent write fOnGetTipOfEvent;

      property MultiCaret: TSynPluginMultiCaret read FMultiCaret;
  end;

Const
  ecShowCallTips           = ecUserFirst + 1;
  ecMoveLinesSelectionUp   = ecUserFirst + 2;
  ecMoveLinesSelectionDown = ecUserFirst + 3;

implementation

Uses
  LConvEncoding, LazFileUtils, SynEditMarkupHighAll,
  Themes, LCLIntf, LazUTF8;

{ TLCEditorHintWindow }

procedure TLCEditorHintWindow.SetCurrentIndex(AValue : integer);
begin
  if fCurrentIndex = AValue then
  begin
    Exit;
  end;

  fCurrentIndex := AValue;
  Invalidate;
end;

function TLCEditorHintWindow.CalcHintRect(MaxWidth: Integer;
  const AHint: string; AData: Pointer): TRect;
const
  HintBorderWidth = 2;
var
  Flags: Cardinal;
  uh: HDC;
  Mon: TMonitor;
begin
  Result := inherited;

  Mon := Screen.MonitorFromPoint(Point(Left, Top)); // don't use Monitor property - it returns wrong monitor for invisible windows.

  if Mon=nil then
  begin
    Mon := Screen.Monitors[0];
  end;

  if Application.Scaled and Scaled and (Mon<>nil) and (PixelsPerInch<>Mon.PixelsPerInch) then
  begin
    AutoAdjustLayout(lapAutoAdjustForDPI, PixelsPerInch, Mon.PixelsPerInch, 0, 0);
  end;

  if AHint = '' then
  begin
    Exit(Rect(0, 0, 0, 0));
  end;

  if MaxWidth <= 0 then
  begin
    MaxWidth := Mon.Width - 4 * HintBorderWidth;
  end;

  Result := Rect(0, 0, MaxWidth, Mon.Height - 4 * HintBorderWidth);
  Flags := DT_CALCRECT or DT_NOPREFIX or DT_WORDBREAK;

  if UseRightToLeftAlignment then
  begin
    Flags := Flags or DT_RTLREADING;
  end;

  uh := Canvas.GetUpdatedHandle([csFontValid]);
  if UseFGThemes then
  begin
    Result := ThemeServices.GetTextExtent(uh, ThemeServices.GetElementDetails(tttToolTipDontCare), AHint, Flags, @Result)
  end
  else
  begin
    DrawText(uh, PChar(AHint), Length(AHint), Result, Flags);
  end;

  // compensate for InflateRect in Paint method
  Inc(Result.Right, 4 * HintBorderWidth);
  Inc(Result.Bottom, 4 * HintBorderWidth);
  //debugln('THintWindow.CalcHintRect Result=',dbgs(Result));
end;

procedure TLCEditorHintWindow.Paint;
const
  HintBorderWidth = 2;
var
  iLeft,
  i: Integer;
  List: TStrings;
  tmpRect,
  ARect: TRect;
  texto:string;

  ThemeBG, ThemeFG: Boolean;
  Details: TThemedElementDetails;
begin
  if ControlCount > 0 then
    inherited Paint         // The window has a custom control.
  else
  begin
    ARect := ClientRect;

    ThemeBG := UseBGThemes;
    ThemeFG := UseFGThemes;

    if ThemeBG or ThemeFG then
    begin
      Details := ThemeServices.GetElementDetails(tttToolTipDontCare);
    end;

    if ThemeBG then
    begin
      ThemeServices.DrawElement(Canvas.Handle, Details, ARect)
    end
    else
    begin
      Canvas.Brush.Color := Color;
      Canvas.Pen.Width := 1;
      Canvas.FillRect(ARect);
      DrawEdge(Canvas.Handle, ARect, BDR_RAISEDOUTER, BF_RECT);
    end;

    InflateRect(ARect, -2 * HintBorderWidth, -2 * HintBorderWidth);

    texto := '';
    List := TStringList.Create;
    try
      List.CommaText := Caption;
      iLeft := aRect.Left;
      for i := 0 to List.Count - 1 do
      begin
        texto := List[i];

        if i < List.Count - 1 then
        begin
          texto := texto + ',';
        end;

        Canvas.Font.Style := [];
        if i = CurrentIndex then
        begin
          Canvas.Font.Style := [fsBold];
        end;

        tmpRect := CalcHintRect(0, texto, nil);

        if i = 0 then
        begin
          aRect.right := aRect.left + tmpRect.right;
        end
        else
        begin
          aRect.left := aRect.right;
          aRect.right += tmpRect.right;
        end;

        if aRect.right > width then
        begin
          aRect.Top += Canvas.GetTextHeight('W');
          aRect.left := iLeft;
          aRect.right := iLeft + tmpRect.right;
        end;

        if ThemeFG then
        begin
          ThemeServices.DrawText(Canvas, Details, texto, ARect, DT_NOPREFIX or DT_VCENTER or DT_CENTER, 0)
        end
        else
        begin
          DrawText(Canvas.GetUpdatedHandle([csFontValid]), PChar(texto),
            Length(texto), ARect, DT_NOPREFIX or DT_VCENTER or DT_LEFT);
        end;
      end;
    finally
      FreeAndNil(List);
    end;
  end;
end;

{ TLCSynEdit }

procedure TLCSynEdit.InitHintWins;
begin
  fCallTipWin := TLCEditorHintWindow.Create(self);
  fCallTipWin.Color := clInfoBk + $01010100;
  fLastTipText := '';
end;

procedure TLCSynEdit.showCallTips(const tips: string; const CurrentIndex:Integer = 0);
var
  pnt: TPoint;
begin
  if tips.isEmpty = true then
  begin
    exit;
  end;
  pnt := ClientToScreen(point(CaretXPix, CaretYPix));

  fCallTipWin.HintRect := fCallTipWin.CalcHintRect(Round(width * 0.5), tips, nil);
  fCallTipWin.OffsetHintRect(pnt, fCallTipWin.Font.Size * 2);
  fCallTipWin.Hint := tips;

  if (fCallTipWin.Hint <> tips)
  or (fCallTipWin.CurrentIndex <> CurrentIndex)
  or (fCallTipWin.Showing = false) then
  begin
    fCallTipWin.CurrentIndex := CurrentIndex;
    fCallTipWin.ActivateHint(tips);
  end;
end;

procedure TLCSynEdit.DoExit;
begin
  hideCallTips;
  inherited DoExit;
end;

procedure TLCSynEdit.DoOnProcessCommand(var Command: TSynEditorCommand;
  var AChar: TUTF8Char; Data: pointer);
begin
  inherited;
  case Command of
    ecShowCallTips:
    begin
      hideCallTips;
      showCallTips(false);
    end;
    ecMoveLinesSelectionUp,
    ecMoveLinesSelectionDown:
    begin
      if not ReadOnly then
      begin
        DoMoveRowsSelected(Command = ecMoveLinesSelectionDown);
      end;
    end;
  end;
end;

procedure TLCSynEdit.UTF8KeyPress(var Key: TUTF8Char);
var
  c: TUTF8Char;
begin
  c := Key;

  inherited;

  case c of
    '(': showCallTips(false);
    ')': if fCallTipWin.Visible then hideCallTips;
  end;
end;

procedure TLCSynEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_BACK:
    begin
      if  (fCallTipWin.Visible = true)
      and (CaretX > 1)
      and (LineText[LogicalCaretXY.X-1] = '(') then
      begin
        hideCallTips;
      end;
    end;
  end;

  fLineError := 0;

  inherited;

  case Key of
    VK_ESCAPE:
      begin
        hideCallTips;
      end;
    else
      begin
         if fCallTipWin.Visible then
         begin
           if  (fCurrentTipTopRow <= CaretY)
           and (fCurrentTipBottomRow >= CaretY) then
           begin
             ShowCallTips(true);
           end
           else
           begin
             hideCallTips;
           end;
         end;
      end;
  end;
end;

procedure TLCSynEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  hideCallTips;

  inherited;
end;

procedure TLCSynEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  fLineError := 0;

  hideCallTips;
end;

procedure TLCSynEdit.MouseLeave;
begin
  inherited MouseLeave;
  hideCallTips;
end;

procedure TLCSynEdit.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);
  if Value = false then
  begin
    hideCallTips;
  end;
end;

function TLCSynEdit.GetTipToWord(aValue : string; const aRow : Integer) : string;
begin
  Result := '';
  if assigned(fOnGetTipOfEvent) then
  begin
    fOnGetTipOfEvent(self, aValue, aRow, Result);
  end;
end;

procedure TLCSynEdit.DoMoveRowsSelected(aMoveDown: Boolean);
var
  i,
  iLineBegin,
  iLineEnd:integer;
  blockBeginSaved,
  blockEndSaved,
  NewBegin,
  NewEnd,
  caretSaved:TPoint;
  Str: TStringList;

  //linha:string;
begin
  if ReadOnly = true then
  begin
    exit;
  end;

  caretSaved := CaretXY;
  blockBeginSaved := BlockBegin;
  blockEndSaved := BlockEnd;
  iLineBegin := BlockBegin.y;
  iLineEnd := BlockEnd.y;

  try
    str := TStringList.Create;
    for i := iLineBegin-1 to iLineEnd-1 do
    begin
      //linha := Lines.Strings[i];
      str.add(Lines.Strings[i]);
    end;

    NewBegin.x := 0;
    NewBegin.y := iLineBegin;

    NewEnd.x := 0;
    NewEnd.y := iLineEnd;

    if (aMoveDown = True) then
    begin
      caretSaved.y := caretSaved.y + 1;

      blockBeginSaved.y := blockBeginSaved.y + 1;
      blockEndSaved.y := blockEndSaved.y + 1;

      NewEnd.y := iLineEnd + 2;
      str.Insert(0,Lines.Strings[iLineEnd]);
    end
    else
    begin
      caretSaved.y := caretSaved.y - 1;
      blockBeginSaved.y := blockBeginSaved.y - 1;
      blockEndSaved.y := blockEndSaved.y - 1;

      NewBegin.y := iLineBegin - 1;
      NewEnd.y := iLineEnd + 1;

      str.add(Lines.Strings[iLineBegin - 2]);
    end;

    // Replace the text
    TextBetweenPoints[NewBegin, NewEnd] := str.Text;

    // Caret position update
    CaretXY := caretSaved;

    // Set the selection text
    BlockBegin := blockBeginSaved;
    BlockEnd := blockEndSaved;

  finally
    FreeAndNil(str);
  end;
end;

constructor TLCSynEdit.Create(aOwner: TComponent);
begin
  if (fUniqueIndex <= 0) then
  begin
    // TODO: Gerar log
    raise Exception.Create('Índice Único não informado ou Menor do que Um.');
  end;

  fFileCode := 'utf8';
  fSiglaModulo := smvNone;
  fPosicaoRegra := -1;

  inherited Create(aOwner);
end;

constructor TLCSynEdit.Create(aOwner: TComponent; aUniqueIndex:Integer; aFileName: String);
var
  fSynMarkHAllCaret:TSynEditMarkupHighlightAllCaret;
begin
  fUniqueIndex := aUniqueIndex;
  fCurrentTipTopRow := -1;
  fCurrentTipBottomRow := -1;
  fLineError := 0;

  Create(aOwner);

  FMultiCaret := TSynPluginMultiCaret.Create(Self);
  with fMultiCaret do
  begin
    Editor := self;
    KeyStrokes.Clear;
  end;

  // Definir algumas teclas de atalho
  AddKey(ecShowCallTips, VK_SPACE, [ssCTRL, ssSHIFT], 0, []);
  AddKey(ecMoveLinesSelectionUp, VK_UP, [ssALT], 0, []);
  AddKey(ecMoveLinesSelectionDown, VK_DOWN, [ssALT], 0, []);

  fSynMarkHAllCaret := TSynEditMarkupHighlightAllCaret(self.MarkupByClass[TSynEditMarkupHighlightAllCaret]);
  if assigned(fSynMarkHAllCaret) then
  begin
    fSynMarkHAllCaret.MarkupInfo.Background := $00FFFF;
    fSynMarkHAllCaret.MarkupInfo.FrameColor:= $C0C0C0;
    fSynMarkHAllCaret.Trim := true;
    fSynMarkHAllCaret.FullWord := true;
    fSynMarkHAllCaret.IgnoreKeywords := false;
  end;

  ShowHint := false;
  InitHintWins;

  FLastTimeSaved := Now;
  fIsNew := true;
  fSaved := false;
  Text := ' ';
  fFileName := Format(rsUntitled, [fUniqueIndex]);
  if (aFileName <> '') then
  begin
    LoadFromFile(aFileName);
  end;
end;

destructor TLCSynEdit.destroy;
begin
  FreeAndNil(fCallTipWin);

  inherited destroy;
end;

procedure TLCSynEdit.showCallTips(aInternal: Boolean);
  procedure ProximaLetra(var pCurP:PChar; var pLen:Integer; var pLetra:String);
  begin
    //pLen := UTF8CharacterLength(pCurP); // Funciona antes da versão 1.9 do lazarus
    pLen := UTF8CodepointSize(pCurP); // Funciona a partir da versão 1.9 do lazarus

    SetLength(pLetra, pLen);
    Move(pCurP^, pLetra[1], pLen);
    inc(pCurP, pLen);
  end;

  function InverterTexto(pTexto: String; pPosicao:Integer):String;
  var
    CurP, EndP: PChar;
    iPosicao,
    iLetras,
    iLen: Integer;
    sLetra: String;
  begin
    Result := '';
    iLetras := 0;
    CurP := PChar(pTexto);
    EndP := CurP + Length(pTexto);

    iPosicao := pPosicao;
    if iPosicao = -1 then
    begin
      iPosicao := UTF8Length(CurP) + 1;
    end;

    while (CurP < EndP)
    and   (iLetras < (iPosicao - 1)) do
    begin
      //iLen := UTF8CharacterLength(CurP);  // Funciona antes da versão 1.9 do lazarus
      iLen := UTF8CodepointSize(CurP);  // Funciona a partir da versão 1.9 do lazarus

      SetLength(sLetra, iLen);
      Move(CurP^, sLetra[1], iLen);

      Result := sLetra + Result;

      inc(iLetras);
      inc(CurP, iLen);
    end;
  end;
var
  CurP,
  EndP: PChar;

  iVirgulas,
  Len: Integer;

  bAchou: Boolean;

  sLetra,
  sTexto,
  sFuncao:String;
begin
  IncPaintLock;
  try
    bAchou := False;
    fCurrentTipTopRow := CaretXY.Y;
    fCurrentTipBottomRow := CaretXY.Y;

    sFuncao := '';
    sTexto := InverterTexto(LineText, CaretXY.X);
    CurP := PChar(sTexto);
    EndP := CurP + Length(sTexto);

    iVirgulas:=0;
    Len := 0;
    while CurP < EndP do
    begin
      ProximaLetra(CurP, Len, sLetra);

      if (sLetra = ';')
      or (sLetra = ')') then
      begin
        break;
      end;

      if sLetra = ',' then
      begin
        Inc(iVirgulas);
      end
      else
      if sLetra = '(' then
      begin
        ProximaLetra(CurP, Len, sLetra);
        while (CurP < EndP) and ((sLetra = #9) or (sLetra = ' ') or (sLetra = ''))  do
        begin
          ProximaLetra(CurP, Len, sLetra);
        end;

        sFuncao := '';
        while (CurP <= EndP) and (sLetra <> #9) and (sLetra <> ' ') and (sLetra <> '')  do
        begin
          sFuncao := sLetra + sFuncao;
          ProximaLetra(CurP, Len, sLetra);
        end;
        bAchou := true;
        Break;
      end;

      if CurP = EndP then
      begin
        if CaretY = 0 then
        begin
          break;
        end;

        fCurrentTipTopRow := fCurrentTipTopRow - 1;

        sTexto := InverterTexto(Self.Lines.Strings[fCurrentTipTopRow-1], -1);
        CurP := PChar(sTexto);
        EndP := CurP + Length(sTexto);
      end;
    end;

    if bAchou = false then
    begin
      hideCallTips;
      exit;
    end;

    if aInternal = false then
    begin
      fLastTipText := GetTipToWord(sFuncao, fCurrentTipTopRow);
    end;
    sTexto := fLastTipText;

    if sTexto.isEmpty then
    begin
      exit;
    end;

    showCallTips(sTexto, iVirgulas);
  finally
    DecPaintLock;
  end;
end;

procedure TLCSynEdit.hideCallTips;
begin
  fCurrentTipTopRow := -1;
  fCurrentTipBottomRow := -1;
  fLastTipText := '';

  if fCallTipWin <> nil then
  begin
    fCallTipWin.Hide;
  end;
end;

function TLCSynEdit.Save: Boolean;
begin
  if  (fIsNew = false) then
  begin
    Lines.SaveToFile(fFileName);
    Modified := False;
    fIsNew := False;
    fSaved := true;
    FLastTimeSaved := now;
    result := true;
    exit;
  end;

  result := SaveAs('');
end;

function TLCSynEdit.SaveAs(NmArq: String; askConfirmation:boolean): Boolean;
begin
  result := false;

  if (NmArq = '') then
  begin
    SaveDialog.FileName := fFileName;
    if SaveDialog.Execute = false then
    begin
      exit;
    end; // if
    NmArq := SaveDialog.FileName;
  end;

  if (NmArq <> '') then
  begin
    fFileName := NmArq;
  end;

  if (askConfirmation = true) then
  begin
    if FileExistsUTF8(fFileName) = True then
    begin
      if Application.MessageBox(PChar(Format(rsFileOverride,[fFileName])),
                                PChar(Application.Title),MB_YESNO + MB_ICONQUESTION + MB_DEFBUTTON2) = ID_NO then
      begin
        Exit;
      end;
    end;
  end;

  fIsNew := false;
  result := Save;
end;

procedure TLCSynEdit.LoadFromFile(NomeArq: String);
var
  stream:TMemoryStream;
  sTxt: String;
begin
  if (fFileName = '') then
  begin
    // TODO: Gerar log
    raise Exception.Create('Nome do Arquivo não informado.');
  end;
  try
    sTxt := '';
    stream := TMemoryStream.Create;
    try
      stream.LoadFromFile(NomeArq);
      SetLength(sTxt, stream.Size);
      stream.ReadBuffer(sTxt[1], stream.Size);
    finally
      stream.Free;
    end;
    Text:= ConvertEncoding(sTxt, GuessEncoding(sTxt), EncodingUTF8, true);

    fFileCode := GuessEncoding(Text);

    fIsNew := false;
    fSaved := true;
    fFileName:= NomeArq;
    ReadOnly := FileIsReadOnly(NomeArq);
  except
    // TODO: Gerar log
  end;
end;

end.

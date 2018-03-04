unit ufcaracteresespeciais;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ExtCtrls, StdCtrls, Buttons, LCLIntf, LCLType, LConvEncoding;

Const
  TitleForm = 'Caracteres Especiais :: ';

  TabASCIIToHTML: array[0..208,0..2] of string = (('32',' ','&nbsp;'),
                                                  ('33','!','!'),
                                                  ('34','"','&quot;'),
                                                  ('35','#','#'),
                                                  ('36','$','$'),
                                                  ('37','%','%'),
                                                  ('38','&','&amp;'),
                                                  ('39',#39,#39), // Aspas Simples
                                                  ('40','(','('),
                                                  ('41',')',')'),
                                                  ('42','*','*'),
                                                  ('43','+','+'),
                                                  ('44',',',','),
                                                  ('45','-','-'),
                                                  ('46','.','.'),
                                                  ('47','/','/'),
                                                  ('48','0','0'),
                                                  ('49','1','1'),
                                                  ('50','2','2'),
                                                  ('51','3','3'),
                                                  ('52','4','4'),
                                                  ('53','5','5'),
                                                  ('54','6','6'),
                                                  ('55','7','7'),
                                                  ('56','8','8'),
                                                  ('57','9','9'),
                                                  ('58',':',':'),
                                                  ('59',';',';'),
                                                  ('60','<','&lt;'),
                                                  ('61','=','='),
                                                  ('62','>','&gt;'),
                                                  ('63','?','?'),
                                                  ('64','@','@'),
                                                  ('65','A','A'),
                                                  ('66','B','B'),
                                                  ('67','C','C'),
                                                  ('68','D','D'),
                                                  ('69','E','E'),
                                                  ('70','F','F'),
                                                  ('71','G','G'),
                                                  ('72','H','H'),
                                                  ('73','I','I'),
                                                  ('74','J','J'),
                                                  ('75','K','K'),
                                                  ('76','L','L'),
                                                  ('77','M','M'),
                                                  ('78','N','N'),
                                                  ('79','O','O'),
                                                  ('80','P','P'),
                                                  ('81','Q','Q'),
                                                  ('82','R','R'),
                                                  ('83','S','S'),
                                                  ('84','T','T'),
                                                  ('85','U','U'),
                                                  ('86','V','V'),
                                                  ('87','W','W'),
                                                  ('88','X','X'),
                                                  ('89','Y','Y'),
                                                  ('90','Z','Z'),
                                                  ('91','[','['),
                                                  ('92','\','\'),
                                                  ('93',']',']'),
                                                  ('94','^','&circ;'),
                                                  ('95','_','_'),
                                                  ('96','`','`'),
                                                  ('97','a','a'),
                                                  ('98','b','b'),
                                                  ('99','c','c'),
                                                  ('100','d','d'),
                                                  ('101','e','e'),
                                                  ('102','f','f'),
                                                  ('103','g','g'),
                                                  ('104','h','h'),
                                                  ('105','i','i'),
                                                  ('106','j','j'),
                                                  ('107','k','k'),
                                                  ('108','l','l'),
                                                  ('109','m','m'),
                                                  ('110','n','n'),
                                                  ('111','o','o'),
                                                  ('112','p','p'),
                                                  ('113','q','q'),
                                                  ('114','r','r'),
                                                  ('115','s','s'),
                                                  ('116','t','t'),
                                                  ('117','u','u'),
                                                  ('118','v','v'),
                                                  ('119','w','w'),
                                                  ('120','x','x'),
                                                  ('121','y','y'),
                                                  ('122','z','z'),
                                                  ('123','{','{'),
                                                  ('124','|','|'),
                                                  ('125','}','}'),
                                                  ('126','~','&tilde;'),
                                                  ('128','€','€'),
                                                  ('129','‚','‚'),
                                                  ('130','ƒ','ƒ'),
                                                  ('131','„','„'),
                                                  ('132','…','…'),
                                                  ('133','†','†'),
                                                  ('134','‡','‡'),
                                                  ('135','ˆ','ˆ'),
                                                  ('136','‰','‰'),
                                                  ('137','Š','Š'),
                                                  ('138','‹','‹'),
                                                  ('139','Œ','Œ'),
                                                  ('140','Ž','Ž'),
                                                  ('141','‘','‘'),
                                                  ('142','’','’'),
                                                  ('143','“','“'),
                                                  ('144','”','”'),
                                                  ('145','•','•'),
                                                  ('146','–','–'),
                                                  ('147','—','—'),
                                                  //('152','˜'',' '), // estudar melhor como mostrar este caracter especial
                                                  ('153','™','&trade;'),
                                                  ('161','¡','&iexcl;'),
                                                  ('162','¢','&cent;'),
                                                  ('163','£','&pound;'),
                                                  ('164','¤','&curren;'),
                                                  ('165','¥','&yen;'),
                                                  ('166','¦','&brvbar;'),
                                                  ('167','§','&sect;'),
                                                  ('168','¨','&uml;'),
                                                  ('169','©','&copy;'),
                                                  ('170','ª','&ordf;'),
                                                  ('171','«','&laquo;'),
                                                  ('172','¬','&not;'),
                                                  ('173','­','&shy;'),
                                                  ('174','®','&reg;'),
                                                  ('175','¯','&macr;'),
                                                  ('176','°','&deg;'),
                                                  ('177','±','&plusmn;'),
                                                  ('178','²','&sup2;'),
                                                  ('179','³','&sup3;'),
                                                  ('180','´','&acute;'),
                                                  ('181','µ','&micro;'),
                                                  ('182','¶','&para;'),
                                                  ('183','·','&middot;'),
                                                  ('184','¸','&cedil;'),
                                                  ('185','¹','&sup1;'),
                                                  ('186','º','&ordm;'),
                                                  ('187','»','&raquo;'),
                                                  ('188','¼','&frac14;'),
                                                  ('189','½','&frac12;'),
                                                  ('190','¾','&frac34;'),
                                                  ('191','¿','&iquest;'),
                                                  ('192','À','&Agrave;'),
                                                  ('193','Á','&Aacute;'),
                                                  ('194','Â','&Acirc;'),
                                                  ('195','Ã','&Atilde;'),
                                                  ('196','Ä','&Auml;'),
                                                  ('197','Å','&Aring;'),
                                                  ('198','Æ','&AElig;'),
                                                  ('199','Ç','&Ccedil;'),
                                                  ('200','È','&Egrave;'),
                                                  ('201','É','&Eacute;'),
                                                  ('202','Ê','&Ecirc;'),
                                                  ('203','Ë','&Euml;'),
                                                  ('204','Ì','&Igrave;'),
                                                  ('205','Í','&Iacute;'),
                                                  ('206','Î','&Icirc;'),
                                                  ('207','Ï','&Iuml;'),
                                                  ('208','Ð','&ETH;'),
                                                  ('209','Ñ','&Ntilde;'),
                                                  ('210','Ò','&Ograve;'),
                                                  ('211','Ó','&Oacute;'),
                                                  ('212','Ô','&Ocirc;'),
                                                  ('213','Õ','&Otilde;'),
                                                  ('214','Ö','&Ouml;'),
                                                  ('216','Ø','&Oslash;'),
                                                  ('217','Ù','&Ugrave;'),
                                                  ('218','Ú','&Uacute;'),
                                                  ('219','Û','&Ucirc;'),
                                                  ('220','Ü','&Uuml;'),
                                                  ('221','Ý','&Yacute;'),
                                                  ('222','Þ','&THORN;'),
                                                  ('223','ß','&szlig;'),
                                                  ('224','à','&agrave;'),
                                                  ('225','á','&aacute;'),
                                                  ('226','â','&acirc;'),
                                                  ('227','ã','&atilde;'),
                                                  ('228','ä','&auml;'),
                                                  ('229','å','&aring;'),
                                                  ('230','æ','&aelig;'),
                                                  ('231','ç','&ccedil;'),
                                                  ('232','è','&egrave;'),
                                                  ('233','é','&eacute;'),
                                                  ('234','ê','&ecirc;'),
                                                  ('235','ë','&euml;'),
                                                  ('236','ì','&igrave;'),
                                                  ('237','í','&iacute;'),
                                                  ('238','î','&icirc;'),
                                                  ('239','ï','&iuml;'),
                                                  ('240','ð','&eth;'),
                                                  ('241','ñ','&ntilde;'),
                                                  ('242','ò','&ograve;'),
                                                  ('243','ó','&oacute;'),
                                                  ('244','ô','&ocirc;'),
                                                  ('245','õ','&otilde;'),
                                                  ('246','ö','&ouml;'),
                                                  ('248','ø','&oslash;'),
                                                  ('249','ù','&ugrave;'),
                                                  ('250','ú','&uacute;'),
                                                  ('251','û','&ucirc;'),
                                                  ('252','ü','&uuml;'),
                                                  ('253','ý','&yacute;'),
                                                  ('254','þ','&thorn;'),
                                                  ('255','ÿ','&yuml;'));

type
  TTipoRetorno = (trASCII, trChar, trHTML);

  { TInfoChar }

  TInfoChar = class(TPersistent)
  private
    fASCIIValue : string;
    fHTMLValue : string;
    fId : Integer;
  public
    property ASCIIValue:Integer read fId write fId;
    property CharValue: string  read fASCIIValue write fASCIIValue;
    property HTMLValue: string  read fHTMLValue write fHTMLValue;
  end;

  { TFCaracteresEspeciais }

  TFCaracteresEspeciais = class(TForm)
    bbInsert: TBitBtn;
    bbClose: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    ValASCII_lb: TLabel;
    Panel1: TPanel;
    StringGrid1: TStringGrid;
    ValHTML_lb: TLabel;
    procedure bbInsertClick(Sender: TObject);
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1KeyDown(Sender: TObject; var {%H-}Key: Word;
      Shift: TShiftState);
    procedure StringGrid1KeyUp(Sender: TObject; var {%H-}{%H-}Key: Word;
      {%H-}{%H-}Shift: TShiftState);
    procedure StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
  private
    FTipoRetorno: TTipoRetorno;
    Function IntToChar(aValue: integer; ReturnHTML: Boolean): String;
    procedure SetTipoRetorno(AValue : TTipoRetorno);

    { private declarations }
  public

    property TipoRetorno:TTipoRetorno read FTipoRetorno write SetTipoRetorno;
    { public declarations }
  end;

var
  FCaracteresEspeciais: TFCaracteresEspeciais;

implementation

uses ufmain, LCSynEdit;

{$R *.lfm}

{ TFCaracteresEspeciais }

procedure TFCaracteresEspeciais.FormCreate(Sender: TObject);
Var
  i, Lin, Col:Integer;
  aChar:String;
  oInfoChar:TInfoChar;
begin
  TipoRetorno := trChar;
  i := 32;
  For Lin:= 0 To StringGrid1.RowCount - 1 do
  begin
    For Col := 0 to StringGrid1.ColCount - 1 do
    begin
      While (i in [127, 129, 141, 143, 144, 157]) do
      begin
        inc(i);
      end;

      aChar := IntToChar(i, false);
      if aChar = '' then
      begin
//        aChar := Chr(i);
        aChar :=CP850ToUTF8(Chr(i));
      end;

      oInfoChar := TInfoChar.Create;
      oInfoChar.ASCIIValue := i;
      oInfoChar.CharValue := aChar;
      oInfoChar.HTMLValue := IntToChar(i, true); ;

      StringGrid1.Cells[Col, Lin] := aChar;
      StringGrid1.Objects[Col, Lin] := oInfoChar;
      inc(i);

      if i > 255 then
      begin
        break;
      end;
    end;

    if i > 255 then
    begin
      break;
    end;
  end;
end;

procedure TFCaracteresEspeciais.StringGrid1KeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  FCaracteresEspeciais.tag := 0;
  if ssCTRL in Shift then
  begin
    FCaracteresEspeciais.tag := 1;
  end;
end;

procedure TFCaracteresEspeciais.StringGrid1KeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  FCaracteresEspeciais.tag := 0;
end;

procedure TFCaracteresEspeciais.bbInsertClick(Sender: TObject);
var
  editor : TLCSynEdit;
begin
  editor := FrmMain.GetEditorAtivo;
  if editor <> nil then
  begin
    case TipoRetorno of
      trASCII: editor.SelText := ValASCII_lb.Caption;
      trHTML : editor.SelText := ValHTML_lb.Caption;
    else
      editor.SelText := StringGrid1.Cells[StringGrid1.Col, StringGrid1.Row];
    end;
  end;

  if FCaracteresEspeciais.tag = 1 then
  begin
    Close;
  end;
end;

procedure TFCaracteresEspeciais.FormCloseQuery(Sender : TObject; var CanClose : boolean);
var
  Lin, Col : Integer;
begin
  try
    For Lin:= 0 To StringGrid1.RowCount - 1 do
    begin
      For Col := 0 to StringGrid1.ColCount - 1 do
      begin
        if (StringGrid1.Objects[Col, Lin] <> nil) then
        begin
          StringGrid1.Objects[Col, Lin].Free;
        end;
      end;
    end;
  except
    // TODO: Gerar Log
  end;
  canClose := true;
end;


procedure TFCaracteresEspeciais.StringGrid1SelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
Var
  oInfoChar:TInfoChar;
begin
  CanSelect := true;
  oInfoChar := TInfoChar(StringGrid1.Objects[aCol, aRow]);

  If  (oInfoChar <> nil)
  and (oInfoChar.ASCIIValue > 0) then
  begin
    ValASCII_lb.Caption := IntToStr(oInfoChar.ASCIIValue);
    ValHTML_lb.Caption := oInfoChar.HTMLValue;
  end
  else
  begin
    ValASCII_lb.Caption := '';
    ValHTML_lb.Caption := '';
  end;
end;

function TFCaracteresEspeciais.IntToChar(aValue: integer; ReturnHTML: Boolean): String;
var
  i: integer;
  aFind:String;
begin
  aFind := IntToStr(aValue);
  Result := '';
  for i:=0 to length(TabASCIIToHTML) - 1 do
  begin
    if TabASCIIToHTML[i,0] = aFind then
    begin
      if ReturnHTML = true then
      begin
        Result := TabASCIIToHTML[i,2];
      end
      else
      begin
        Result := TabASCIIToHTML[i,1];
      end;
      break;
    end;
  end;
end;

procedure TFCaracteresEspeciais.SetTipoRetorno(AValue : TTipoRetorno);
begin
  if FTipoRetorno = AValue then
  begin
    Exit;
  end;

  FTipoRetorno := AValue;
  if (FTipoRetorno in [trChar, trASCII]) then
  begin
    Self.Caption := TitleForm + ' ASCII';
  end
  else
  if (FTipoRetorno = trHTML) then
  begin
    Self.Caption := TitleForm + ' HTML';
  end;
end;

end.


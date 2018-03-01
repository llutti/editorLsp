unit uFSobre;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, ButtonPanel, ECLink;

type

  { TFSobre }

  TFSobre = class(TForm)
    ButtonPanel1 : TButtonPanel;
    ECLink1 : TECLink;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Image1: TImage;
    Versao_lb: TLabel;
    Data_lb : TLabel;
    plataformalb : TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FSobre: TFSobre;

implementation

uses
  ufmain, SysUtils;

{$R *.lfm}

procedure TFSobre.FormCreate(Sender: TObject);
begin
  Versao_lb.Caption := Versao_lb.Caption + FrmMain.VersionOfApplication;
  Data_lb.Caption := Trim(Data_lb.Caption) + ' ' + {$I %date%};
  plataformalb.Caption := Trim(plataformalb.Caption) + ' ' + lowerCase({$I %FPCTARGETCPU%})
                                                     + '-' + lowerCase({$I %FPCTARGETOS%});
end;

end.

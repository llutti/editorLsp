program bccsLsp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, ufmain, LCSynEdit, uEditorLspSettings,
  uFSobre, ufirparalinha, ufcaracteresespeciais, ufParametrosFuncoes,
  uFAutoCompletar, UFSettingsEditor;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Title:='BCCS :: Editor LSP';
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.


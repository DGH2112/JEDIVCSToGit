program JEDIVCSToGit;

uses
  Vcl.Forms,
  JVTGMainForm in 'Source\JVTGMainForm.pas' {frmJEDIVCSToGit},
  JVTGRelativePathForm in 'Source\JVTGRelativePathForm.pas' {frmExtractRelPath},
  JVTGTypes in 'Source\JVTGTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmJEDIVCSToGit, frmJEDIVCSToGit);
  Application.Run;
end.

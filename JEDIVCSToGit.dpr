program JEDIVCSToGit;

{$R 'JVTGITHVerInfo.res' 'JVTGITHVerInfo.RC'}

uses
  Vcl.Forms,
  JVTGMainForm in 'Source\JVTGMainForm.pas' {frmJEDIVCSToGit},
  JVTGRelativePathForm in 'Source\JVTGRelativePathForm.pas' {frmExtractRelPath},
  JVTGTypes in 'Source\JVTGTypes.pas',
  JVTGGitErrorForm in 'Source\JVTGGitErrorForm.pas' {frmGITError},
  JVTGFunctions in 'Source\JVTGFunctions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'JEDI VCS to GIT Migration Tool';
  Application.CreateForm(TfrmJEDIVCSToGit, frmJEDIVCSToGit);
  Application.Run;
end.

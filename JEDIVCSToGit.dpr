program JEDIVCSToGit;

uses
  Vcl.Forms,
  JEDIVCSToGitMainForm in 'JEDIVCSToGitMainForm.pas' {frmJEDIVCSToGit};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmJEDIVCSToGit, frmJEDIVCSToGit);
  Application.Run;
end.

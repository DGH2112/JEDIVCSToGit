program JEDIVCSToGit;

uses
  Vcl.Forms,
  JVTGMainForm in 'Source\JVTGMainForm.pas' {frmJEDIVCSToGit};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmJEDIVCSToGit, frmJEDIVCSToGit);
  Application.Run;
end.

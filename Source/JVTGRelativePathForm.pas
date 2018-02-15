Unit JVTGRelativePathForm;

Interface

Uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Buttons,
  JVTGTypes;

Type
  TfrmExtractRelPath = Class(TForm)
    lblExistingGitRepoPath: TLabel;
    edtExistingGitRepoPath: TEdit;
    edtModulePath: TEdit;
    lblModulePath: TLabel;
    edtModuleName: TEdit;
    lblModuleName: TLabel;
    edtRelPath: TEdit;
    lblRelPath: TLabel;
    btnAbort: TBitBtn;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
  Strict Private
  Strict Protected
  Public
    Class Function Execute(Const slPaths : TStringList; Const RepoData : TJVTGRepoData;
      Var strRelPath : String) : Boolean;
  End;

Var
  frmExtractRelPath: TfrmExtractRelPath;

Implementation

{$R *.dfm}

Class Function TfrmExtractRelPath.Execute(Const slPaths : TStringList; Const RepoData : TJVTGRepoData;
  Var strRelPath : String) : Boolean;

Var
  F : TfrmExtractRelPath;
  iResult : TModalResult;
  iIndex: Integer;
  iLen: Integer;
  
Begin
  Result := False;
  iIndex := slPaths.IndexOfName(RepoData.FModuleName);
  If iIndex > -1 Then
    Begin
      iLen := Length(RepoData.FOLDGitRepoPath);
      If CompareText(Copy(RepoData.FModulePath, 1, iLen), RepoData.FOLDGitRepoPath) = 0 Then
        Begin
          strRelPath := RepoData.FModulePath;
          Delete(strRelPath, 1, iLen);
          slPaths.Values[RepoData.FModuleName] := strRelPath;
        End Else
          Begin
            F := TfrmExtractRelPath.Create(Application.MainForm);
            Try
              F.edtExistingGitRepoPath.Text := RepoData.FOLDGitRepoPath;
              F.edtModulePath.Text := RepoData.FModulePath;
              F.edtModuleName.Text := RepoData.FModuleName;
              F.edtRelPath.Text := strRelPath;
              iResult := F.ShowModal;
              Case iResult Of
                mrOk:
                  Begin
                    strRelPath := F.edtRelPath.Text;
                    slPaths.Values[RepoData.FModuleName] := strRelPath;
                    Result := True;
                  End;
                mrCancel: ;
                mrAbort: Abort;
              End;  
            Finally
              F.Free;
            End;
          End;
    End Else
    Begin
      strRelPath := slPaths.ValueFromIndex[iIndex];
      Result := True;
    End;
End;

End.

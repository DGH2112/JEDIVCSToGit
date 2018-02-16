(**
  
  This module contains a form for requesting the subdirectory for files.

  @Author  David Hoyle
  @Version 1.0
  @Date    16 Feb 2018
  
**)
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
    lblRelPath: TLabel;
    btnAbort: TBitBtn;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    edtRelPath: TComboBox;
  Strict Private
  Strict Protected
    Class Procedure AddTrailingSlash(Var strPath : String);
    Class Function  CheckPathExists(Const boolResult : Boolean; Const strNewGitRepoPath : String;
      Var strRelPath : String) : Boolean;
  Public
    Class Function Execute(Const slPaths : TStringList; Const RepoData : TJVTGRepoData;
      Var strRelPath : String) : Boolean;
  End;

Implementation

{$R *.dfm}

Class Procedure TfrmExtractRelPath.AddTrailingSlash(Var strPath: String);

Begin
  If (Length(strPath) > 0) And (strPath[Length(strPath)] <> '\') Then
    strPath := strPath + '\';
End;

Class Function TfrmExtractRelPath.CheckPathExists(Const boolResult : Boolean;
  Const strNewGitRepoPath : String; Var strRelPath : String) : Boolean;

Begin
  Result := boolResult;
  If Not DirectoryExists(strNewGitRepoPath + strRelPath) Then
    If InputQuery(Application.Title, 'Create Directory', strRelPath) Then
      Begin
        AddTrailingSlash(strRelPath);
        If Not ForceDirectories(strNewGitRepoPath + strRelPath) Then
          Raise Exception.Create(Format('Could not create the folder "%s"!',
            [strNewGitRepoPath + strRelPath]));
      End Else
        Result := False;
End;

Class Function TfrmExtractRelPath.Execute(Const slPaths : TStringList; Const RepoData : TJVTGRepoData;
  Var strRelPath : String) : Boolean;

Var
  F : TfrmExtractRelPath;
  iResult : TModalResult;
  iIndex: Integer;
  iLen: Integer;
  i: Integer;
  
Begin
  Result := False;
  iIndex := slPaths.IndexOfName(RepoData.FModuleName);
  If iIndex = -1 Then
    Begin
      iLen := Length(RepoData.FOLDGitRepoPath);
      If CompareText(Copy(RepoData.FModulePath, 1, iLen), RepoData.FOLDGitRepoPath) = 0 Then
        Begin
          strRelPath := RepoData.FModulePath;
          Delete(strRelPath, 1, iLen);
          slPaths.Values[RepoData.FModuleName] := strRelPath;
          Result := True;
        End Else
          Begin
            F := TfrmExtractRelPath.Create(Application.MainForm);
            Try
              F.edtExistingGitRepoPath.Text := RepoData.FOLDGitRepoPath;
              F.edtModulePath.Text := RepoData.FModulePath;
              F.edtModuleName.Text := RepoData.FModuleName;
              F.edtRelPath.Text := strRelPath;
              For i := 0 To slPaths.Count -  1 Do
                If F.edtRelPath.Items.IndexOf(slPaths.ValueFromIndex[i]) = -1 Then
                  F.edtRelPath.Items.Add(slPaths.ValueFromIndex[i]);
              iResult := F.ShowModal;
              Case iResult Of
                mrOk:
                  Begin
                    strRelPath := F.edtRelPath.Text;
                    F.AddTrailingSlash(strRelPath);
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
  Result := CheckPathExists(Result, RepoData.FNEWGitRepoPath, strRelPath);
End;

End.

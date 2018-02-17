(**
  
  This module contains simple types for use in the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    17 Feb 2018
  
**)
Unit JVTGTypes;

Interface

Type
  (** A record to describe repository and module information. **)
  TJVTGRepoData = Record
    FOLDGitRepoPath : String;
    FNEWGitRepoPath : String;
    FModulePath     : String;
    FModuleName     : String;
    Constructor Create(Const strOldGitRepoPath, strNewGitRepoPath, strModulePath, strModuleName: String);
  End;

Implementation

{ TJVTGRepoData }

(**

  A constructor for the TJVTGRepoData record.

  @precon  None.
  @postcon Initialises the the record.

  @param   strOldGitRepoPath as a String as a constant
  @param   strNewGitRepoPath as a String as a constant
  @param   strModulePath     as a String as a constant
  @param   strModuleName     as a String as a constant

**)
Constructor TJVTGRepoData.Create(Const strOldGitRepoPath, strNewGitRepoPath, strModulePath,
  strModuleName: String);

Begin
  FOLDGitRepoPath := strOldGitRepoPath;
  FNEWGitRepoPath := strNewGitRepoPath;
  FModulePath := strModulePath;
  FModuleName := strModuleName;
End;

End.

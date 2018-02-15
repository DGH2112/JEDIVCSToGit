(**
  
  This module contains simple types for use in the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    15 Feb 2018
  
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
  End;

Implementation

End.

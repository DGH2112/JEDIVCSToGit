(**
  
  This module contains common function for use throughout the application.

  @Author  David Hoyle
  @Version 1.0
  @Date    17 Feb 2018
  
**)
Unit JVTGFunctions;

Interface

Type
  (** A record to describe the EXE version information. **)
  TJVTGBuildInfo = Record
    FMajor   : Integer;
    FMinor   : Integer;
    FRelease : Integer;
    FBuild   : Integer;
  End;

  Procedure GetBuildInfo(Var BuildInfo : TJVTGBuildInfo);

Implementation

Uses
  WinAPI.Windows;

(**

  This method updates the application caption to include the applications version number and build 
  information.

  @precon  None.
  @postcon The applications caption is updated with version and build information.

  @param   BuildInfo as a TJVTGBuildInfo as a reference

**)
Procedure GetBuildInfo(Var BuildInfo : TJVTGBuildInfo);

Const
  iWordMask = $FFFF;
  iShift16 = 16;

Var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;

Begin
  BuildInfo.FMajor := 0;
  BuildInfo.FMinor := 0;
  BuildInfo.FRelease := 0;
  BuildInfo.FBuild := 0;   
  VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
  If VerInfoSize <> 0 Then
    Begin
      GetMem(VerInfo, VerInfoSize);
      GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo);
      VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
      BuildInfo.FMajor := VerValue^.dwFileVersionMS Shr iShift16;
      BuildInfo.FMinor := VerValue^.dwFileVersionMS And iWordMask;
      BuildInfo.FRelease := VerValue^.dwFileVersionLS Shr iShift16;
      BuildInfo.FBuild := VerValue^.dwFileVersionLS And iWordMask;
      FreeMem(VerInfo, VerInfoSize);
    End;
End;

End.

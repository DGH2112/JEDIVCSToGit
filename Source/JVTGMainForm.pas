(**
  
  This module contains a class / form to extract all the JEDI VCS commits for a series of projects that
  match a pattern and output them chronologically and commit these to a new Git Repositiory.

  @Author  David Hoyle
  @Version 1.0
  @Date    10 Feb 2018
  
**)
Unit JVTGMainForm;

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
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Phys.MSSQL,
  FireDAC.Phys.MSSQLDef,
  FireDAC.VCLUI.Wait,
  Data.DB,
  FireDAC.Comp.Client,
  Datasnap.Provider,
  Vcl.Grids,
  Vcl.DBGrids,
  FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.DApt.Intf,
  FireDAC.DApt,
  FireDAC.Comp.DataSet,
  Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls;

Type
  (** A record to describe the information required by DGHCreateProcess. **)
  TProcessInfo = Record
    boolEnabled : Boolean;
    strEXE    : String;
    strParams : String;
    strDir    : String;
    strTitle  : String;
  End;
  (** A form to hold data sets from the JED VCS database containing the revisions and blobs. **)
  TfrmJEDIVCSToGit = Class(TForm)
    FDConnection: TFDConnection;
    DBGrid: TDBGrid;
    RevisionsDataSource: TDataSource;
    RevisionsFDQuery: TFDQuery;
    btnGetRevisions: TButton;
    mmoGitOutput: TMemo;
    Splitter: TSplitter;
    pnlTop: TPanel;
    lblGitRepoPath: TLabel;
    edtGitRepoPath: TEdit;
    StatusBar: TStatusBar;
    pnlMain: TPanel;
    BlobsGrid: TDBGrid;
    BlobsFDQuery: TFDQuery;
    BlobsDataSource: TDataSource;
    lblProjectNamePattern: TLabel;
    edtProjectNamePattern: TEdit;
    pnlMainqq: TPanel;
    DBGridSplitter: TSplitter;
    Procedure btnGetRevisionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edtProjectNamePatternExit(Sender: TObject);
  Strict Private
    FFileNames : TstringList;
    FLastMessage: String;
    FItemCount: Integer;
    FItem: Integer;
    FGitPI : TProcessInfo;
    FGitRepoPath: String;
  Strict Protected
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure CommitToGit(Const strComment: String; Const dtCommitDateTime: TDateTime);
    Procedure ProcessMsgevent(Const strMsg : String; Var boolAbort : Boolean);
    Procedure IdleEvent;
    Procedure ExecuteGit(Const strCmdParams : String);
    Procedure CheckGitRepoPath;
    Procedure CheckThereIsNoExistingGitRepo;
  Public
  End;

Var
  (** A form variable managed by Delphi for the application main form. **)
  frmJEDIVCSToGit: TfrmJEDIVCSToGit;

Implementation

{$R *.dfm}

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  System.IniFiles,
  System.Zip,
  System.UITypes;

Type
  (** A method signature for the DGHCreateProcess message event handler. **)
  TProcessMsgHandler = Procedure(Const strMsg : String; var boolAbort : Boolean) Of Object;
  (** A method signature for the DGHCreateProcess idle event handler. **)
  TIdleHandler = Procedure Of Object;

ResourceString
  (** A resource string to say that the directory was not found. **)
  strDirectoryNotFound = 'The directory "%s" does not exist.';
  (** A resource string to say the user aborted the process. **)
  strUserAbort = 'User Abort!';
  (** A resource string to say that the EXE file was not found. **)
  strEXENotFound = 'The executable file "%s" does not exist.';

Const
  (** An ini section name for the majority of the application settings. **)
  strSetupINISection = 'Setup';
  (** Ini file extension. **)
  strIniExt = '.ini';
  (** An ini key for the top of the main window. **)
  strTopKey = 'Top';
  (** An ini key for the left of the main window. **)
  strLeftKey = 'Left';
  (** An ini key for the Height of the main window. **)
  strHeightKey = 'Height';
  (** An ini key for the Width of the main window. **)
  strWidthKey = 'Width';
  (** An ini section name for the revision column widths. **)
  strRevsColWidthsIniSection = 'RevisionsColumnWidths';
  (** An ini section name for the blob column widths. **)
  strBlobColumnWidthsIniSection = 'BlobColumnWidths';
  (** An ini key for the ProjectNamePattern for matching revisions. **)
  strProjectNamePatternKey = 'ProjectNamePattern';
  (** An ini key for the new repository path. **)
  strRepoPathKey = 'RepoPath';
  (** An ini key for the output height **)
  strOutputHeightKey = 'OutputHeight';

Function DGHFindOnPath(var strEXEName : String; Const strDirs : String) : Boolean; Forward;

(**

  This function creates a process with message handlers which must be implemented by the passed interface
  in order for the calling process to get messages from the process console and handle idle and abort.

  @precon  ProcMsgHndr must be a valid class implementing TDGHCreateProcessEvent.
  @postcon Creates a process with message handlers which must be implemented by the passed interface in 
           order for the calling process to get messages from the process console and handle idle and 
           abort.

  @param   Process           as a TProcessInfo as a reference
  @param   ProcessMsgHandler as a TProcessMsgHandler as a constant
  @param   IdleHandler       as a TIdleHandler as a constant
  @return  an Integer

**)
Function  DGHCreateProcess(Var Process : TProcessInfo; Const ProcessMsgHandler : TProcessMsgHandler;
  Const IdleHandler : TIdleHandler) : Integer;

Type
  EDGHCreateProcessException = Exception;

Const
  iPipeSize = 4096;

Var
  boolAbort: Boolean;

  (**

    This method checks that the process directory and executable exists.

    @precon  None.
    @postcon Raises exceptions if either the process directory or EXE are not valid.

  **)
  Procedure CheckProcess;

  Begin
    If Not DirectoryExists(Process.strDir) Then
      Raise EDGHCreateProcessException.CreateFmt(strDirectoryNotFound, [Process.strDir]);
    If Not FileExists(Process.strEXE) Then
      Begin
        If Not DGHFindOnPath(Process.strEXE, '') Then
          Raise EDGHCreateProcessException.CreateFmt(strEXENotFound, [Process.strEXE]);
      End;
  End;

  (**

    This procedure configures the security attributes for the progress to be created.

    @precon  None.
    @postcon The passed security attributes are configured.

    @param   SecurityAttrib as a TSecurityAttributes as a reference

  **)
  Procedure ConfigSecurityAttrib(Var SecurityAttrib : TSecurityAttributes);

  Begin
    FillChar(SecurityAttrib, SizeOf(SecurityAttrib), 0);
    SecurityAttrib.nLength := SizeOf(SecurityAttrib);
    SecurityAttrib.bInheritHandle := True;
    SecurityAttrib.lpSecurityDescriptor := Nil;
  End;

  (**

    This procedure configures the startup information for the new process to be created.

    @precon  None.
    @postcon The startup information is configured.

    @param   StartupInfo as a TStartupInfo as a reference
    @param   hWrite      as a THandle as a constant

  **)
  Procedure ConfigStartUp(Var StartupInfo : TStartupInfo; Const hWrite : THandle);

  Begin
    FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
    StartupInfo.cb := SizeOf(TStartupInfo);
    StartupInfo.cb := SizeOf(StartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    StartupInfo.wShowWindow := SW_HIDE;
    StartupInfo.hStdOutput := hWrite;
    StartupInfo.hStdError := hWrite;
  End;

  (**

    This prcoedure is called periodically by the process handler in order to retreive console output from
    the running process. Output everything from the console (pipe the anonymous pipe) but the last line 
    as this may not be a complete line of information from the console (except if boolPurge is true).

    @precon  slLines must be a valid instance of a TStringList class to accumulate the console output.
    @postcon Outputs to the IDGHCreareProcessEvent interface output information from the console.

    @param   slLines as a TStringList as a constant
    @param   hRead   as a THandle as a constant
    @param   Purge   as a Boolean as a constant

  **)
  Procedure ProcessOutput(Const slLines : TStringList; Const hRead : THandle;
    Const Purge : Boolean = False);

  Var
    iTotalBytesInPipe : Cardinal;
    iBytesRead : Cardinal;
    strOutput : AnsiString;

  Begin
    If Assigned(Idlehandler) Then
      IdleHandler;
    If boolAbort Then
      Begin
        If Assigned(ProcessMsgHandler) Then
          ProcessMsgHandler(strUserAbort, boolAbort);
        Exit;
      End;
    Win32Check(PeekNamedPipe(hRead, Nil, 0, Nil, @iTotalBytesInPipe, Nil));
    If iTotalBytesInPipe > 0 Then
      Begin
        SetLength(strOutput, iTotalBytesInPipe);
        ReadFile(hRead, strOutput[1], iTotalBytesInPipe, iBytesRead, Nil);
        SetLength(strOutput, iBytesRead);
        slLines.Append(StringReplace(UTF8ToString(strOutput), #10, #13#10, [rfReplaceAll]));
      End;
    // Use a string list to output each line except the last as it may not
    // be complete yet.
    If Assigned(ProcessMsgHandler) Then
      While slLines.Count > 1 - Integer(Purge) Do
        Begin
          ProcessMsgHandler(slLines[0], boolAbort);
          slLines.Delete(0);
        End;
  End;

  (**

    This procedure runs the process collecting information from the console output and feeding it back 
    into the output memo.

    @precon  None.
    @postcon The process is run and the output captured.

    @param   SecurityAttrib as a TSecurityAttributes as a constant
    @param   StartupInfo    as a TStartupInfo as a constant
    @param   hRead          as a THandle as a constant

  **)
  Procedure RunProcess(Const SecurityAttrib : TSecurityAttributes; Const StartupInfo : TStartupInfo;
    Const hRead : THandle);

  Const
    iWaitIntervalInMS = 50;

  Var
    ProcessInfo : TProcessInformation;
    slLines : TStringList;
    iExitCode : Cardinal;
  
  Begin
    Win32Check(CreateProcess(PChar(Process.strEXE),
      PChar('"' + Process.strEXE + '" ' + Process.strParams), @SecurityAttrib,
      Nil, True, CREATE_NEW_CONSOLE, Nil, PChar(Process.strDir), StartupInfo, ProcessInfo));
    Try
      slLines := TStringList.Create;
      Try
        While WaitforSingleObject(ProcessInfo.hProcess, iWaitIntervalInMS) = WAIT_TIMEOUT Do
          Begin
            ProcessOutput(slLines, hRead);
            If boolAbort Then
              Begin
                TerminateProcess(ProcessInfo.hProcess, 0);
                Break;
              End;
          End;
        ProcessOutput(slLines, hRead, True);
      Finally
        slLines.Free;
      End;
      If GetExitCodeProcess(ProcessInfo.hProcess, iExitCode) Then
        Inc(Result, iExitCode)
    Finally
      Win32Check(CloseHandle(ProcessInfo.hThread));
      Win32Check(CloseHandle(ProcessInfo.hProcess));
    End;
  End;

Var
  hRead, hWrite : THandle;
  SecurityAttrib : TSecurityAttributes;
  StartupInfo : TStartupInfo;

Begin
  Result := 0;
  boolAbort := False;
  ConfigSecurityAttrib(SecurityAttrib);
  Win32Check(CreatePipe(hRead, hWrite, @SecurityAttrib, iPipeSize));
  Try
    If Process.boolEnabled Then
      Try
        CheckProcess;
        ConfigStartUp(StartupInfo, hWrite);
        RunProcess(SecurityAttrib, StartupInfo, hRead);
      Except
        On E : EDGHCreateProcessException Do
          If Assigned(ProcessMsgHandler) Then
            Begin
              ProcessMsgHandler(E.Message, boolAbort);
              Inc(Result);
            End;
      End;
  Finally
    Win32Check(CloseHandle(hWrite));
    Win32Check(CloseHandle(hRead));
  End;
End;

(**

  This method searches the given paths (semi-colon delimited) and the enironment path for the exe file 
  name. If found the result is true and the full path to the file is returned in strEXEName.

  @precon  None.
  @postcon Searches the given paths (semi-colon delimited) and the enironment path for the exe file name
           . If found the result is true and the full path to the file is returned in strEXEName.

  @param   strEXEName as a String as a reference
  @param   strDirs    as a String as a constant
  @return  a Boolean

**)
Function DGHFindOnPath(var strEXEName : String; Const strDirs : String) : Boolean;

  (**

    This procedure checks the paths and deletes empty paths and ensures the rest have a trailing
    backslash.

    @precon  slPaths must be a valid instance.
    @postcon Empty paths are deleted and all paths are terminated with a backslash.

    @param   slPaths as a TStringList as a constant

  **)
  Procedure CheckPaths(Const slPaths : TStringList);

  Var
    iPath: Integer;
    iLength: Integer;

  Begin
    For iPath := slPaths.Count - 1 DownTo 0 Do
      Begin
        iLength := Length(slPaths[iPath]);
        If iLength = 0 Then
          slPaths.Delete(iPath)
        Else
          If slPaths[iPath][iLength] <> '\' Then
            slPaths[iPath] := slPaths[iPath] + '\';
      End;
  End;

  (**

    This function searches the paths for the executeable and returns true if found and strEXEName is
    updated.

    @precon  slPaths must be a valid instance.
    @postcon If found, returns true and strEXEName is updated.

    @param   slPaths as a TStringList as a constant
    @return  a Boolean

  **)
  Function SearchPaths(Const slPaths : TStringList) : Boolean;

  Var
    iPath: Integer;
    i : Integer;
    recSearch: TSearchRec;
    strPath, strExPath : String;
    iSize: Integer;
    
  Begin
    Result := False;
    strEXEName := ExtractFileName(strEXEName);
    For iPath := 0 To slPaths.Count - 1 Do
      Begin
        strPath := slPaths[iPath];
        SetLength(strExPath, MAX_PATH);
        iSize := ExpandEnvironmentStrings(PChar(strPath), PChar(strExPath), MAX_PATH);
        SetLength(strExPath, Pred(iSize));
        i := FindFirst(strExPath + strEXEName, faAnyFile, recSearch);
        Try
          If i = 0 Then
            Begin
              strEXEName := strExPath + strEXEName;
              Result := True;
              Break;
            End;
        Finally
          FindClose(recSearch);
        End;
      End;
  End;

Const
  strPathEnVar = 'path';

Var
  slPaths : TStringList;

Begin
  slPaths := TStringList.Create;
  Try
    slPaths.Text := GetEnvironmentVariable(strPathEnVar);
    If strDirs <> '' Then
      slPaths.Text := strDirs + ';' + slPaths.Text;
    slPaths.Text := StringReplace(slPaths.Text, ';', #13#10, [rfReplaceAll]);
    CheckPaths(slPaths);
    Result := SearchPaths(slPaths);
  Finally
    slPaths.Free;
  End;
End;

(**

  This is an on click event handler for the Get Revisions button.

  @precon  None.
  @postcon This method starts the process of extracting files from JEDI VCS to put into GIT.

  @param   Sender as a TObject

**)
Procedure TfrmJEDIVCSToGit.btnGetRevisionsClick(Sender: TObject);

ResourceString
  strFileNeedsRenaming = 'The file "%s" needs renaming to "%s"!';
  strExtracting = 'Extracting: %s';

Const
  strBlogZip = 'Blog.zip';
  strFileData = 'FileData';
  strModuleName = 'Module Name';
  strExtension = 'Extension';
  strComment_i = 'comment_i';
  strTSTAMP = 'TSTAMP';
  strRecOfRecs = '%d of %d, Elapsed time: %1.1n seconds...';
  strMoveParams = 'mv -v %s%s %s%s';
  strAddParams = 'add -v %s%s';
  strGitInit = 'init';
  strTmpSource = 'Source\';
  dblMSInSec = 1000.0;

Var
  strZipFileName: String;
  Z: TZipFile;
  iFile: Integer;
  strOldFileName: String;
  strRepoFileName: String;
  strSubDir: String;
  iStartTime: UInt64;
  boolAbort: Boolean;

Begin
  CheckGitRepoPath;
  CheckThereIsNoExistingGitRepo;
  ExecuteGit(strGitInit);
  DBGrid.ReadOnly := True;
  BlobsGrid.ReadOnly := True;
  Try
    iStartTime := GetTickCount64;
    RevisionsDataSource.DataSet.Last;
    FItemCount := RevisionsDataSource.DataSet.RecordCount;
    RevisionsDataSource.DataSet.First;
    strZipFileName := FGitRepoPath + strBlogZip;
    While Not RevisionsDataSource.DataSet.Eof Do
      Begin
        BlobsDataSource.DataSet.First;
        While Not BlobsDataSource.DataSet.Eof Do
          Begin
           (BlobsDataSource.DataSet.FieldByName(strFileData) As TBlobField).SaveToFile(strZipFileName);
            Z := TZipFile.Create;
            Try
              Z.Open(strZipFileName, zmRead);
              For iFile := 0 To Z.FileCount - 1 Do
                Begin
                  strSubDir := strTmpSource;
                  strRepoFileName :=
                    RevisionsDataSource.DataSet.FieldByName(strModuleName).AsString + '.' +
                    BlobsDataSource.DataSet.FieldByName(strExtension).AsString;
                  strOldFileName := FFileNames.Values[strRepoFileName];
                  If strOldFileName <> '' Then
                    If CompareText(strOldFileName, Z.FileName[iFile]) <> 0 Then
                      Begin
                        CodeSite.Send(Format(strFileNeedsRenaming, [strOldFileName, Z.FileName[iFile]]));
                        ExecuteGit(Format(strMoveParams, [strSubDir, strOldFileName, strSubDir, Z.FileName[iFile]]));
                      End;
                  FFileNames.Values[strRepoFilename] := Z.FileName[iFile];
                  Z.Extract(Z.FileName[iFile], FGitRepoPath + strSubDir);
                  ProcessMsgevent(Format(strExtracting, [FGitRepoPath + strSubDir + Z.FileName[iFile]]), boolAbort);
                  ExecuteGit(Format(strAddParams, [strSubDir, Z.FileName[iFile]]));
                End;
              Z.Close;
            Finally
              Z.Free;
            End;
            BlobsDataSource.DataSet.Next;
          End;
        CommitToGit(RevisionsDataSource.DataSet.FieldByName(strComment_i).AsString,
          RevisionsDataSource.DataSet.FieldByName(strTSTAMP).AsDateTime);
        Inc(FItem);
        StatusBar.Panels[0].Text := Format(strRecOfRecs, [FItem, FItemCount,
          Int(GetTickCount64 - iStartTime) / dblMSInSec]);
        RevisionsDataSource.DataSet.Next;
      End;
  Finally
    DBGrid.ReadOnly := False;
    BlobsGrid.ReadOnly := False;;
  End;
End;

(**

  This method checks the Git Repository Path to ensure its a valid directory.

  @precon  None.
  @postcon Raises an exception if the path does not exist or is empty.

**)
Procedure TfrmJEDIVCSToGit.CheckGitRepoPath;

ResourceString
  strGitRepositoryPathDoesNotExist = 'The Git Repository path "%s" does not exist!';

Begin
  If (Length(edtGitRepoPath.Text) = 0) Or (Not DirectoryExists(edtGitRepoPath.Text)) Then
    Raise Exception.CreateFmt(strGitRepositoryPathDoesNotExist, [edtGitRepoPath.Text]);
  FGitRepoPath := edtGitRepoPath.Text;
  If FGitRepoPath[Length(FGitRepoPath)] <> '\' Then
    FGitRepoPath := FGitRepoPath + '\';
End;

(**

  This method checks that there is no existing GIT repository in the repository path.

  @precon  None.
  @postcon Raises an exception if there is already a repository.

**)
Procedure TfrmJEDIVCSToGit.CheckThereIsNoExistingGitRepo;

ResourceString
  strGitRepositoryAlreadyExists = 'A git repository already exists in "%s"!';

Const
  strGitDir = '.git';

Begin
  If DirectoryExists(FGitRepoPath + strGitDir) Then
    Raise Exception.CreateFmt(strGitRepositoryAlreadyExists, [FGitRepoPath]);
End;

(**

  This method commits the current staged files to git using the given comment and time and date stamp.

  @precon  None.
  @postcon The currently staged files are committed.

  @param   strComment       as a String as a constant
  @param   dtCommitDateTime as a TDateTime as a constant

**)
Procedure TfrmJEDIVCSToGit.CommitToGit(Const strComment: String; Const dtCommitDateTime: TDateTime);

Const
  strCommitDate = 'commit -v --date "%s" -m "%s"';
  strDateFmt = 'dd/mmm/yyyy HH:nn:ss';

Var
  strCleanComment : String;
  
Begin
  strCleanComment := StringReplace(strComment, '"', '''', [rfReplaceAll]);
  strCleanComment := StringReplace(strCleanComment, #13, '', [rfReplaceAll]);
  strCleanComment := StringReplace(strCleanComment, #10, '\n', [rfReplaceAll]);
  ExecuteGit(Format(strCommitDate, [FormatDateTime(strDateFmt, dtCommitDateTime), strCleanComment]));
End;

(**

  This method updates the ProjectNamePattern macro in the revisiob query while maintaining the DBGrids
  column widths.

  @precon  None.
  @postcon Updates the ProjectNamePattern macro in the revisiob query while maintaining the DBGrids
           column widths.

  @param   Sender as a TObject

**)
Procedure TfrmJEDIVCSToGit.edtProjectNamePatternExit(Sender: TObject);

Const
  strProjectNamePatternMacro = 'ProjectNamePattern';

Var
  M: TFDMacro;
  iColumn: Integer;
  aiColumnWidths : TArray<Integer>;

Begin
  SetLength(aiColumnWidths, DBGrid.Columns.Count);
  For iColumn := 0 To DBGrid.Columns.Count - 1 Do
    aiColumnWidths[iColumn] := DBGrid.Columns[iColumn].Width;
  M := RevisionsFDQuery.MacroByName(strProjectNamePatternMacro);
  M.Value := edtProjectNamePattern.Text;
  RevisionsFDQuery.Active := True;
  For iColumn := 0 To DBGrid.Columns.Count - 1 Do
    DBGrid.Columns[iColumn].Width := aiColumnWidths[iColumn];
End;

(**

  This method executes GIT and captures any errors and prompts for an action.

  @precon  None.
  @postcon Executes GIT and captures any errors and prompts for an action.

  @param   strCmdParams as a String as a constant

**)
Procedure TfrmJEDIVCSToGit.ExecuteGit(Const strCmdParams: String);

ResourceString
  strMsg = 'The last GIT command (%s) failed:'#13#10'%s';

Var
  boolAbort : Boolean;
  iResult: Integer;
  
Begin
  FGitPI.strDir := FGitRepoPath;
  FGitPI.strParams := strCmdParams;
  ProcessMsgevent(Format('%s%s %s', [FGitPI.strDir, ExtractFileName(FGitPI.strEXE), FGitPI.strParams]), boolAbort);
  FLastMessage := '';
  iResult := DGHCreateProcess(FGitPI, ProcessMsgEvent, IdleEvent);
  If iResult <> 0 Then
    Case MessageDlg(Format(strMsg, [strCmdParams, FLastMessage]), mtError, [mbIgnore, mbAbort], 0) Of
      mrAbort: Abort;
    End;
  ProcessMsgevent(#13#10, boolAbort);
End;

(**

  This is an OnFormCreate Event Handler for the TfrmJEDVICSToGit class.

  @precon  None.
  @postcon Loads the applications settings and cretes a string list for filenames.

  @param   Sender as a TObject

**)
Procedure TfrmJEDIVCSToGit.FormCreate(Sender: TObject);

Const
  strGITExe = 'GIT.exe';
  strConnection =
    'SERVER=SEASONSFALL0001\SQLEXPRESS2008'#13#10 +
    'OSAuthent=Yes'#13#10 +
    'ApplicationName=JEDIVCSToGit'#13#10 +
    'Workstation=SEASONSFALL0001'#13#10 +
    'Database=JEDIVCS24'#13#10 +
    'DriverID=MSSQL'#13#10 +
    'User_Name=sysdba';

Begin
  FFilenames := TStringList.Create;
  FFileNames.Duplicates := dupIgnore;
  FItemCount := 0;
  FItem := 0;
  FGitPI.boolEnabled := True;
  FGitPI.strEXE := strGITExe;
  FDConnection.Params.Text := strConnection;
  FDConnection.Connected := True;
  RevisionsFDQuery.Active := True;
  BlobsFDQuery.Active := True;
  LoadSettings;
End;

(**

  This is an OnFormDestroy Event Handler for the TfrmJEDVCSToGit class.

  @precon  None.
  @postcon Saves the applications settings and the log file and free memory used by the filename string
           list.

  @param   Sender as a TObject

**)
Procedure TfrmJEDIVCSToGit.FormDestroy(Sender: TObject);

Const
  strLog = 'Git.log';

Begin
  SaveSettings;
  mmoGitOutput.Lines.SaveToFile(FGitRepoPath + strLog);
  FFileNames.Free;
End;

(**

  This is an on idle event handler for the command line processes to ensure the application updates
  its interface.

  @precon  None.
  @postcon The application messge query is updated.

**)
Procedure TfrmJEDIVCSToGit.IdleEvent;

Begin
  Application.ProcessMessages;
End;

(**

  This method loads the applications settings from an INI file.

  @precon  None.
  @postcon The applications settings are loaded from an INI file.

**)
Procedure TfrmJEDIVCSToGit.LoadSettings;

Const
  iDefaultWidth = 100;

Var
  iniFile: TMemIniFile;
  iColumn : Integer;

Begin
  iniFile := TMemIniFile.Create(ChangeFileExt(ParamStr(0), strIniExt));
  Try
    Top := iniFile.ReadInteger(strSetupINISection, strTopKey, Top);
    Left := iniFile.ReadInteger(strSetupIniSection, strLeftKey, Left);
    Height := iniFile.ReadInteger(strSetupIniSection, strHeightKey, Height);
    Width := iniFile.ReadInteger(strSetupIniSection, strWidthKey, Width);
    For iColumn := 0 To DBGrid.Columns.Count - 1 Do
      DBGrid.Columns[iColumn].Width := iniFile.ReadInteger(strRevsColWidthsIniSection,
        DBGrid.Columns[iColumn].FieldName, iDefaultWidth);
    For iColumn := 0 To BlobsGrid.Columns.Count - 1 Do
      BlobsGrid.Columns[iColumn].Width := iniFile.ReadInteger(strBlobColumnWidthsIniSection,
        BlobsGrid.Columns[iColumn].FieldName, iDefaultWidth);
    edtProjectNamePattern.Text := iniFile.ReadString(strSetupIniSection, strProjectNamePatternKey, '');
    edtGitRepoPath.Text := iniFile.ReadString(strSetupIniSection, strRepoPathKey, '');
    mmoGitOutput.Height := iniFile.ReadInteger(strSetupIniSection, strOutputHeightKey,
      mmoGitOutput.Height);
  Finally
    iniFile.Free;
  End;
  edtProjectNamePatternExit(Nil);
End;

(**

  This method processses a message from a command line and outputs the information to the output
  log.

  @precon  None.
  @postcon Command line process information is output to the output log.

  @nohint boolAbort

  @param   strMsg    as a String as a constant
  @param   boolAbort as a Boolean as a reference

**)
Procedure TfrmJEDIVCSToGit.ProcessMsgevent(Const strMsg: String; Var boolAbort: Boolean);

Var
  sl : TStringList;
  iLine: Integer;
  
Begin
  sl := TStringList.Create;
  Try
    sl.Text := strMsg;
    For iLine := 0 To sl.Count - 1 Do
      Begin
        mmoGitOutput.Lines.Append(sl[iLine]);
        If sl[iLine] <> '' Then
          Begin
            If FLastMessage <> '' Then
              FLastMessage := FLastMessage + #13#10;
            FLastMessage := FLastMessage + sl[iLine];
          End;
      End;
  Finally
    sl.Free;
  End;
End;

(**

  This method saves the applications settings to an ini file.

  @precon  None.
  @postcon The applications settings are saved.

**)
Procedure TfrmJEDIVCSToGit.SaveSettings;

Var
  iniFile: TMemIniFile;
  iColumn : Integer;

Begin
  iniFile := TMemIniFile.Create(ChangeFileExt(ParamStr(0), strIniExt));
  Try
    iniFile.WriteInteger(strSetupIniSection, strTopKey, Top);
    iniFile.WriteInteger(strSetupIniSection, strLeftKey, Left);
    iniFile.WriteInteger(strSetupIniSection, strHeightKey, Height);
    iniFile.WriteInteger(strSetupIniSection, strWidthKey, Width);
    For iColumn := 0 To DBGrid.Columns.Count - 1 Do
      iniFile.WriteInteger(strRevsColWidthsIniSection, DBGrid.Columns[iColumn].FieldName,
        DBGrid.Columns[iColumn].Width);
    For iColumn := 0 To BlobsGrid.Columns.Count - 1 Do
      iniFile.WriteInteger(strBlobColumnWidthsIniSection, BlobsGrid.Columns[iColumn].FieldName,
        BlobsGrid.Columns[iColumn].Width);
    iniFile.WriteString(strSetupIniSection, strProjectNamePatternKey, edtProjectNamePattern.Text);
    iniFile.WriteString(strSetupIniSection, strRepoPathKey, edtGitRepoPath.Text);
    iniFile.WriteInteger(strSetupIniSection, strOutputHeightKey, mmoGitOutput.Height);
    iniFile.UpdateFile;
  Finally
    iniFile.Free;
  End;
End;

End.

(**
  
  This module contains a class / form to extract all the JEDI VCS commits for a series of projects that
  match a pattern and output them chronologically and commit these to a new Git Repositiory.

  @Author  David Hoyle
  @Version 1.0
  @Date    17 Feb 2018
  
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
  (** A form to hold data sets from the JED VCS database containing the revisions and blobs. **)
  TfrmJEDIVCSToGit = Class(TForm)
    FDConnection: TFDConnection;
    DBGrid: TDBGrid;
    RevisionsDataSource: TDataSource;
    RevisionsFDQuery: TFDQuery;
    btnGetRevisions: TButton;
    Splitter: TSplitter;
    pnlTop: TPanel;
    lblNewGitRepoPath: TLabel;
    edtNewGitRepoPath: TEdit;
    StatusBar: TStatusBar;
    pnlMain: TPanel;
    BlobsGrid: TDBGrid;
    BlobsFDQuery: TFDQuery;
    BlobsDataSource: TDataSource;
    lblProjectNamePattern: TLabel;
    edtProjectNamePattern: TEdit;
    pnlMainqq: TPanel;
    DBGridSplitter: TSplitter;
    pnlGitRepos: TGridPanel;
    lblOldGitRepoPath: TLabel;
    edtOldGitRepoPath: TEdit;
    lbxGitOutput: TListBox;
    chkStatus: TCheckBox;
    Procedure btnGetRevisionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edtProjectNamePatternExit(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure lbxGitOutputDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure DBGridDrawDataCell(Sender: TObject; const Rect: TRect; Field: TField;
      State: TGridDrawState);
  Strict Private
    Type
      (** A record to describe the information required by DGHCreateProcess. @nohints **)
      TProcessInfo = Record
        boolEnabled : Boolean;
        strEXE    : String;
        strParams : String;
        strDir    : String;
        strTitle  : String;
      End;
    (** An enumerate to define the types of the messages. @nohints **)
    TMsgType = (mtInformation, mtTitle);
    (** A method signature for the DGHCreateProcess message event handler. @nohints **)
    TProcessMsgHandler = Procedure(Const strMsg : String; var boolAbort : Boolean;
      Const MsgType : TMsgType = mtInformation) Of Object;
    (** A method signature for the DGHCreateProcess idle event handler. @nohints **)
    TIdleHandler = Procedure Of Object;
  Strict Private
    FFileNames           : TStringList;
    FLastMessage         : String;
    FItemCount           : Integer;
    FItem                : Integer;
    FGitPI               : TProcessInfo;
    FNewGitRepoPath      : String;
    FStartTime           : UInt64;
    FPercentage          : Double;
    FRelativePaths       : TStringList;
    FOldGitRepoPath      : String;
  Strict Protected
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure CommitToGit(Const strComment: String; Const dtCommitDateTime: TDateTime);
    Procedure ProcessMsgevent(Const strMsg: String; Var boolAbort: Boolean;
      Const MsgType : TMsgType = mtInformation);
    Procedure IdleEvent;
    Procedure ExecuteGit(Const strCmdParams : String);
    Procedure CheckGitRepoPath;
    Procedure CheckThereIsAnExistingGitRepo;
    Procedure UpdateStatus;
    Function  CalcTime(Const iTime : UInt64): String;
    Function  DGHCreateProcess(Var Process : TProcessInfo; Const ProcessMsgHandler : TProcessMsgHandler;
      Const IdleHandler : TIdleHandler) : Integer;
    Function GetActualPathAndFileCase(Const strRelPathFile: String) : String;
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
  System.UITypes,
  JVTGRelativePathForm,
  JVTGTypes, 
  JVTGGitErrorForm, 
  JVTGFunctions;

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
  strNewRepoPathKey = 'NewRepoPath';
  (** An ini key for the old repository path. **)
  strOldRepoPathKey = 'OldRepoPath';
  (** An ini key for the output height **)
  strOutputHeightKey = 'OutputHeight';
  (** An ini key for the height of the Blob Grid. **)
  strBlobGridHeightKey = 'BlobGridHeight';
  (** A constant double for the percentable multiplier. **)
  dblPercentageMultiplier = 100.0;
  (** An ini key for whether the GIT STATUS should be shown. **)
  strShowStatusKey = 'ShowStatus';

Function DGHFindOnPath(var strEXEName : String; Const strDirs : String) : Boolean; Forward;

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

Const
  strGitStatus = 'status';

  (**

    This method processes the blobs associated with a revision and adds them to the git repo.

    @precon  strZipFileName must be a valid ZIP file.
    @postcon The revisiob blobs are added to the Git Repo.

    @param   strZipFileName as a String as a constant
    @return  an Integer

  **)
  Function ProcessBlobs(Const strZipFileName : String) : Integer;

    (**

      This method checks the file to be extracted and whether it needs to rename an existing file.

      @precon  None.
      @postcon Renames the file if its name has changed and updates the output filename.

      @param   strSubDir        as a String as a constant
      @param   strFileToExtract as a String as a constant

    **)
    Procedure CheckFileNamesForRename(Const strSubDir, strFileToExtract : String);

    ResourceString
      strFileNeedsRenaming = 'The file "%s" needs renaming to "%s"!';
    
    Const
      strModuleName = 'Module Name';
      strExtension = 'Extension';
      strMoveParams = 'mv -v "%s" "%s%s"';
    
    Var
      strOldFileName: String;
      strRepoFileName: String;
      strActualPathAndFile: String;
    
    Begin
      strRepoFileName :=
        RevisionsDataSource.DataSet.FieldByName(strModuleName).AsString + '.' +
        BlobsDataSource.DataSet.FieldByName(strExtension).AsString;
      strOldFileName := FFileNames.Values[strRepoFileName];
      If strOldFileName <> '' Then
        If CompareText(strOldFileName, strFileToExtract) <> 0 Then
          Begin
            CodeSite.Send(Format(strFileNeedsRenaming, [strOldFileName, strFileToExtract]));
            strActualPathAndFile := strSubDir + strOldFileName;
            strActualPathAndFile := GetActualPathAndFileCase(strActualPathAndFile);
            ExecuteGit(Format(strMoveParams, [strActualPathAndFile,
              ExtractFilePath(strActualPathAndFile), strFileToExtract]));
            If chkStatus.Checked Then
              ExecuteGit(strGitStatus);
          End;
      FFileNames.Values[strRepoFilename] := strFileToExtract;
    End;
  
  ResourceString
    strExtracting = 'Extracting: %s';
  
  Const
    strFileData = 'FileData';
    strAddParams = 'add -v "%s"';
    strPath = 'path';
    strModuleName = 'Module Name';

  Var
    Z: TZipFile;
    iFile: Integer;
    strSubDir: String;
    boolAbort: Boolean;
    RepoData : TJVTGRepoData;
    strActualFileCase: String;
    
  Begin
    Result := 0;
    BlobsDataSource.DataSet.First;
    While Not BlobsDataSource.DataSet.Eof Do
      Begin
       (BlobsDataSource.DataSet.FieldByName(strFileData) As TBlobField).SaveToFile(strZipFileName);
        Z := TZipFile.Create;
        Try
          Z.Open(strZipFileName, zmRead);
          For iFile := 0 To Z.FileCount - 1 Do
            Begin
              RepoData.Create(FOldGitRepoPath, FNewGitRepoPath,
                RevisionsDataSource.Dataset.FieldByName(strPath).AsString,
                RevisionsDataSource.Dataset.FieldByName(strModuleName).AsString);
              If TfrmExtractRelPath.Execute(FRelativePaths, RepoData, strSubDir) Then
                Begin
                  CheckFileNamesForRename(strSubDir, Z.FileName[iFile]);
                  Z.Extract(Z.FileName[iFile], FNewGitRepoPath + strSubDir);
                  ProcessMsgevent(Format(strExtracting, [FNewGitRepoPath + strSubDir + Z.FileName[iFile]]),
                    boolAbort);
                  strActualFileCase := strSubDir + Z.FileName[iFile];
                  strActualFileCase := GetActualPathAndFileCase(strActualFileCase);
                  ExecuteGit(Format(strAddParams, [strActualFileCase]));
                  Inc(Result);
                  If chkStatus.Checked Then
                    ExecuteGit(strGitStatus);
                End;
            End;
          Z.Close;
        Finally
          Z.Free;
        End;
        BlobsDataSource.DataSet.Next;
      End;
  End;
  
  (**

    This method iterates through the revision records processing the blobs assoviated with each revision
    extracting the files, adding them and committing them.

    @precon  None.
    @postcon The revision records are processed.

  **)
  Procedure ProcessRevisions;

  Const
    strBlobZip = 'Blob.zip';
    strComment_i = 'comment_i';
    strTSTAMP = 'TSTAMP';
   
  Var
    strZipFileName: String;
    
  Begin
    strZipFileName := FNewGitRepoPath + strBlobZip;
    While Not RevisionsDataSource.DataSet.Eof Do
      Begin
        ProcessBlobs(strZipFileName);
        CommitToGit(RevisionsDataSource.DataSet.FieldByName(strComment_i).AsString,
          RevisionsDataSource.DataSet.FieldByName(strTSTAMP).AsDateTime);
        If chkStatus.Checked Then
          ExecuteGit(strGitStatus);
        Inc(FItem);
        UpdateStatus;
        RevisionsDataSource.DataSet.Next;
      End;
  End;

Begin
  CheckGitRepoPath;
  CheckThereIsAnExistingGitRepo;
  DBGrid.ReadOnly := True;
  BlobsGrid.ReadOnly := True;
  Try
    FStartTime := GetTickCount64;
    RevisionsDataSource.DataSet.Last;
    FItemCount := RevisionsDataSource.DataSet.RecordCount;
    RevisionsDataSource.DataSet.First;
    ProcessRevisions;
  Finally
    DBGrid.ReadOnly := False;
    BlobsGrid.ReadOnly := False;;
  End;
End;

(**

  This method calculates the time (in milliseconds) as minutes and seconds.

  @precon  None.
  @postcon Returns the number of milliseconds as minutes and seconds as a string.

  @param   iTime as an UInt64 as a constant
  @return  a String

**)
Function TfrmJEDIVCSToGit.CalcTime(Const iTime : UInt64): String;

ResourceString
  strMinsSecs = '%d mins %d secs';

Const
  dblMSInSec = 1000.0;
  iSecsInMin = 60;

Var
  iSeconds: Int64;
  iMinutes: Int64;

Begin
  iSeconds := Trunc(Int(iTime) / dblMSInSec);
  iMinutes := iSeconds Div iSecsInMin;
  iSeconds := iSeconds Mod iSecsInMin;
  Result := Format(strMinsSecs, [iMinutes, iSeconds]);
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
  If (Length(edtNewGitRepoPath.Text) = 0) Or (Not DirectoryExists(edtNewGitRepoPath.Text)) Then
    Raise Exception.CreateFmt(strGitRepositoryPathDoesNotExist, [edtNewGitRepoPath.Text]);
  FNewGitRepoPath := edtNewGitRepoPath.Text;
  If FNewGitRepoPath[Length(FNewGitRepoPath)] <> '\' Then
    FNewGitRepoPath := FNewGitRepoPath + '\';
  FOldGitRepoPath := edtOldGitRepoPath.Text;
  If FOldGitRepoPath[Length(FOldGitRepoPath)] <> '\' Then
    FOldGitRepoPath := FOldGitRepoPath + '\';
End;

(**

  This method checks that there is no existing GIT repository in the repository path.

  @precon  None.
  @postcon Raises an exception if there is already a repository.

**)
Procedure TfrmJEDIVCSToGit.CheckThereIsAnExistingGitRepo;

ResourceString
  strGitRepositoryDoesNotExists = 'A GIT repository does NOT exists in "%s"!';

Const
  strGitDir = '.git';

Begin
  If Not DirectoryExists(FNewGitRepoPath + strGitDir) Then
    Raise Exception.CreateFmt(strGitRepositoryDoesNotExists, [FNewGitRepoPath]);
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

  This is an on Draw Data Cell event handler for the DBGrids.

  @precon  None.
  @postcon Draws the grid data with clWindow background.

  @param   Sender as a TObject
  @param   Rect   as a TRect as a constant
  @param   Field  as a TField
  @param   State  as a TGridDrawState

**)
Procedure TfrmJEDIVCSToGit.DBGridDrawDataCell(Sender: TObject; Const Rect: TRect; Field: TField;
  State: TGridDrawState);

ResourceString
  strBlob = '(Blob)';

Const
  iPaddding = 5;
  
Var
  C: TCanvas;
  strText: String;
  R: TRect;

Begin
  C := (Sender As TDBGrid).Canvas;
  If gdSelected In State Then
    Begin
      C.Brush.Color := clHighlight;
      C.Font.Color := clHighlightText;
    End Else
    Begin
      C.Brush.Color := clWindow;
      C.Font.Color := clWindowText;
    End;
  C.FillRect(Rect);
  Case Field.DataType Of
    ftBlob: strText := strBlob;
  Else
    strText := Field.AsString;
  End;
  R := Rect;
  InflateRect(R, -iPaddding, 0);
  Case Field.Alignment Of
    taLeftJustify:  C.TextRect(R, strText, [tfLeft, tfVerticalCenter]);
    taRightJustify: C.TextRect(R, strText, [tfRight, tfVerticalCenter]);
    taCenter:       C.TextRect(R, strText, [tfCenter, tfVerticalCenter]);
  End;
End;

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
Function TfrmJEDIVCSToGit.DGHCreateProcess(Var Process : TProcessInfo;
  Const ProcessMsgHandler : TProcessMsgHandler; Const IdleHandler : TIdleHandler) : Integer;

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
  If FDConnection.Connected Then
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
  FGitPI.strDir := FNewGitRepoPath;
  FGitPI.strParams := strCmdParams;
  ProcessMsgevent(Format('%s%s %s', [FGitPI.strDir, ExtractFileName(FGitPI.strEXE), FGitPI.strParams]),
    boolAbort, mtTitle);
  FLastMessage := '';
  iResult := DGHCreateProcess(FGitPI, ProcessMsgEvent, IdleEvent);
  If iResult <> 0 Then
    Case TfrmGITError.Execute(Format(strMsg, [strCmdParams, FLastMessage])) Of
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

ResourceString
  strPleaseSpecifyFireDACINIFileAsFirstParameter = 'Please specify a FireDAC INI file as the first ' + 
    'parameter!';
  strCouldNotLoadINIFile = 'Could not load the INI file "%s"';
  strJEDIVCSToGitBuild = 'JEDI VCS to Git %d.%d%s (Build %d.%d.%d.%d): ';

Const
  strBugFix = ' abcedfghijklmnopqrstuvwxyz';
  strGITExe = 'GIT.exe';
  
Var
  BuildInfo: TJVTGBuildInfo;

Begin
  GetBuildInfo(BuildInfo);
  Caption := Format(strJEDIVCSToGitBuild, [
      BuildInfo.FMajor,
      BuildInfo.FMinor,
      strBugFix[BuildInfo.FRelease + 1],
      BuildInfo.FMajor,
      BuildInfo.FMinor,
      BuildInfo.FRelease,
      BuildInfo.FBuild
    ]);
  FFilenames := TStringList.Create;
  FFileNames.Duplicates := dupIgnore;
  FItemCount := 0;
  FItem := 0;
  FGitPI.boolEnabled := True;
  FGitPI.strEXE := strGITExe;
  If (ParamCount > 0) And FileExists(ParamStr(1)) Then
    Begin
      FDConnection.Params.LoadFromFile(ParamStr(1));
      FDConnection.Connected := True;
      RevisionsFDQuery.Active := True;
      BlobsFDQuery.Active := True;
    End Else
      If ParamCount = 0 Then
        ShowMessage(strPleaseSpecifyFireDACINIFileAsFirstParameter)
      Else
        ShowMessage(Format(strCouldNotLoadINIFile, [ParamStr(1)]));
  FRelativePaths := TStringList.Create;
  FRelativePaths.Duplicates := dupIgnore;
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
  lbxGitOutput.Items.SaveToFile(FNewGitRepoPath + strLog);
  FRelativePaths.Free;
  FFileNames.Free;
End;

(**

  This method searches for the actual case for the file path and filename so that GIT ADD does not fail
  to add the file.

  @precon  None.
  @postcon Returns the correct cased file path and name.

  @param   strRelPathFile as a String as a constant
  @return  a String

**)
Function TfrmJEDIVCSToGit.GetActualPathAndFileCase(Const strRelPathFile: String) : String;

Var
  sl : TStringList;
  i: Integer;
  strCurrentPath : String;
  recSearch: TSearchRec;
  iResult: Integer;
  
Begin
  Result := '';
  sl := TStringList.Create;
  Try
    sl.Text := StringReplace(strRelPathFile, '\', #13#10, [rfReplaceAll]);
    strCurrentPath := FNewGitRepoPath;
    For i := 0 To sl.Count - 1 Do
      Begin
        If (Result <> '') And (Result[Length(Result)] <> '\') Then
          Result := Result + '\';
        If (strCurrentPath <> '') And (strCurrentPath[Length(strCurrentPath)] <> '\') Then
          strCurrentPath := strCurrentPath + '\';
        iResult := FindFirst(strCurrentPath + sl[i], faAnyFile, recSearch);
        Try
          If iResult = 0 Then
            Begin
              Result := Result + recSearch.Name;
              strCurrentPath := strCurrentPath + recSearch.Name;
            End;
        Finally
          FindClose(recSearch);
        End;
      End;
  Finally
    sl.Free;
  End;
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

  This is an on draw item event handler for the Git Output listbox.

  @precon  None.
  @postcon Custom Draws the list box items to show GIT commands differently from the rest of the
           information.

  @nocheck MissingConstInParam 
  @nohint  Control
  
  @param   Control as a TWinControl
  @param   Index   as an Integer
  @param   Rect    as a TRect
  @param   State   as a TOwnerDrawState

**)
Procedure TfrmJEDIVCSToGit.lbxGitOutputDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);

Var
  strText: String;
  
Begin
  If odSelected In State Then
    Begin
      lbxGitOutput.Canvas.Brush.Color := clHighlight;
      lbxGitOutput.Canvas.FillRect(Rect);
      lbxGitOutput.Canvas.FrameRect(Rect);
    End Else
    Begin
      lbxGitOutput.Canvas.Brush.Color := clWindow;
      lbxGitOutput.Canvas.FillRect(Rect);
    End;
  Case NativeUInt(lbxGitOutput.Items.Objects[Index]) Of
    NativeUInt(mtTitle):
      Begin
        lbxGitOutput.Canvas.Font.Style := [fsBold];
        lbxGitOutput.Canvas.Font.Color := clMaroon;
      End;
  Else
    lbxGitOutput.Canvas.Font.Style := [];
    lbxGitOutput.Canvas.Font.Color := clNavy;
  End;
  If odSelected In State Then
    lbxGitOutput.Canvas.Font.Color := clHighlightText;
  strText := lbxGitOutput.Items[Index];
  lbxGitOutput.Canvas.TextRect(Rect, strText, [tfLeft, tfVerticalCenter]);
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
    edtNewGitRepoPath.Text := iniFile.ReadString(strSetupIniSection, strNewRepoPathKey, '');
    edtOldGitRepoPath.Text := iniFile.ReadString(strSetupIniSection, strOldRepoPathKey, '');
    lbxGitOutput.Height := iniFile.ReadInteger(strSetupIniSection, strOutputHeightKey,
      lbxGitOutput.Height);
    BlobsGrid.Height := iniFile.ReadInteger(strSetupIniSection, strBlobGridHeightKey, BlobsGrid.Height);
    chkStatus.Checked := iniFile.ReadBool(strSetupINISection, strShowStatusKey, True);
  Finally
    iniFile.Free;
  End;
  edtProjectNamePatternExit(Nil);
End;

(**

  This method processses a message from a command line and outputs the information to the output log.

  @precon  None.
  @postcon Command line process information is output to the output log.

  @nohint  boolAbort

  @param   strMsg    as a String as a constant
  @param   boolAbort as a Boolean as a reference
  @param   MsgType   as a TMsgType as a constant

**)
Procedure TfrmJEDIVCSToGit.ProcessMsgevent(Const strMsg: String; Var boolAbort: Boolean;
  Const MsgType : TMsgType = mtInformation);

Var
  sl : TStringList;
  iLine: Integer;
  
Begin
  sl := TStringList.Create;
  Try
    sl.Text := strMsg;
    For iLine := 0 To sl.Count - 1 Do
      Begin
        lbxGitOutput.Items.AddObject(sl[iLine], TObject(MsgType));
        lbxGitOutput.ItemIndex := Pred(lbxGitOutput.Items.Count);
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
    iniFile.WriteString(strSetupIniSection, strNewRepoPathKey, edtNewGitRepoPath.Text);
    iniFile.WriteString(strSetupIniSection, strOldRepoPathKey, edtOldGitRepoPath.Text);
    iniFile.WriteInteger(strSetupIniSection, strOutputHeightKey, lbxGitOutput.Height);
    iniFile.WriteInteger(strSetupIniSection, strBlobGridHeightKey, BlobsGrid.Height);
    iniFile.WriteBool(strSetupINISection, strShowStatusKey, chkStatus.Checked);
    iniFile.UpdateFile;
  Finally
    iniFile.Free;
  End;
End;

(**

  This is an on draw panel event handler for the statusbar.

  @precon  None.
  @postcon Draws the main panel (0) with a progress bar.

  @nocheck MissingConstInParam
  
  @param   StatusBar as a TStatusBar
  @param   Panel     as a TStatusPanel
  @param   Rect      as a TRect as a constant

**)
Procedure TfrmJEDIVCSToGit.StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
  Const Rect: TRect);

Const
  iPadding = 0;
  iLightRed = $8080FF;
  iLightGreen = $80FF80;

Var
  R : TRect;
  strText: String;
  
Begin
  R := Rect;
  strText := Panel.Text;
  StatusBar.Canvas.Brush.Color := iLightRed;
  InflateRect(R, -iPadding, 0);
  StatusBar.Canvas.FillRect(R);
  StatusBar.Canvas.TextRect(R, strText, [tfLeft]);
  StatusBar.Canvas.Brush.Color := iLightGreen;
  R.Right := R.Left + Trunc(Int(R.Right - R.Left) * (FPercentage / dblPercentageMultiplier));
  StatusBar.Canvas.FillRect(R);
  StatusBar.Canvas.TextRect(R, strText, [tfLeft]);
End;

(**

  This method updates the statusbar status and pregress position.

  @precon  None.
  @postcon The statusbar status and position is updated.

**)
Procedure TfrmJEDIVCSToGit.UpdateStatus;

ResourceString
  strRecOfRecs = ' %d of %d (%1.1n%%), Elapsed: %s, Remaining: %s, Total: %s...';

Var
  iTotal: UINt64;

Begin
  FPercentage := Int(FItem) / Int(FItemCount) * dblPercentageMultiplier;
  If FPercentage > 0 Then
    iTotal := Trunc(Int(GetTickCount64 - FStartTime) * dblPercentageMultiplier / FPercentage)
  Else
    iTotal := 0;
  StatusBar.Panels[0].Text := Format(strRecOfRecs, [
    FItem,
    FItemCount,
    FPercentage,
    CalcTime(GetTickCount64 - FStartTime),
    CalcTime(iTotal - (GetTickCount64 - FStartTime)),
    CalcTime(iTotal)
  ]);
End;

End.

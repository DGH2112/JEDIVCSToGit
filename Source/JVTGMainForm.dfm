object frmJEDIVCSToGit: TfrmJEDIVCSToGit
  Left = 0
  Top = 0
  Caption = 'JEDIVCS To Git'
  ClientHeight = 539
  ClientWidth = 840
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 840
    Height = 33
    Align = alTop
    TabOrder = 0
    object pnlGitRepos: TGridPanel
      Left = 1
      Top = 1
      Width = 838
      Height = 31
      Align = alClient
      ColumnCollection = <
        item
          SizeStyle = ssAbsolute
          Value = 135.000000000000000000
        end
        item
          Value = 30.000000013333320000
        end
        item
          SizeStyle = ssAbsolute
          Value = 115.000000000000000000
        end
        item
          Value = 34.999999993333340000
        end
        item
          SizeStyle = ssAbsolute
          Value = 120.000000000000000000
        end
        item
          Value = 34.999999993333340000
        end
        item
          SizeStyle = ssAbsolute
          Value = 110.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 5
          Control = edtNewGitRepoPath
          Row = 0
        end
        item
          Column = 4
          Control = lblNewGitRepoPath
          Row = 0
        end
        item
          Column = 2
          Control = lblOldGitRepoPath
          Row = 0
        end
        item
          Column = 3
          Control = edtOldGitRepoPath
          Row = 0
        end
        item
          Column = 6
          Control = btnGetRevisions
          Row = 0
        end
        item
          Column = 0
          Control = lblProjectNamePattern
          Row = 0
        end
        item
          Column = 1
          Control = edtProjectNamePattern
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 0
      ExplicitHeight = 28
      DesignSize = (
        838
        31)
      object edtNewGitRepoPath: TEdit
        AlignWithMargins = True
        Left = 604
        Top = 4
        Width = 118
        Height = 23
        Align = alClient
        TabOrder = 0
        ExplicitLeft = 126
        ExplicitTop = 1
        ExplicitWidth = 83
        ExplicitHeight = 24
      end
      object lblNewGitRepoPath: TLabel
        AlignWithMargins = True
        Left = 484
        Top = 4
        Width = 114
        Height = 23
        Align = alClient
        Caption = 'New Git Repo Path'
        Layout = tlCenter
        ExplicitLeft = 1
        ExplicitTop = 1
        ExplicitWidth = 106
        ExplicitHeight = 16
      end
      object lblOldGitRepoPath: TLabel
        AlignWithMargins = True
        Left = 245
        Top = 4
        Width = 109
        Height = 23
        Align = alClient
        Caption = 'Old Git Repo Path'
        Layout = tlCenter
        ExplicitLeft = 209
        ExplicitTop = 1
        ExplicitWidth = 100
        ExplicitHeight = 16
      end
      object edtOldGitRepoPath: TEdit
        AlignWithMargins = True
        Left = 360
        Top = 4
        Width = 118
        Height = 23
        Align = alClient
        TabOrder = 1
        ExplicitLeft = 334
        ExplicitTop = 1
        ExplicitWidth = 84
        ExplicitHeight = 24
      end
      object btnGetRevisions: TButton
        Left = 731
        Top = 3
        Width = 97
        Height = 25
        Anchors = []
        Caption = 'Get Revisions'
        TabOrder = 2
        OnClick = btnGetRevisionsClick
        ExplicitLeft = 736
        ExplicitTop = 6
      end
      object lblProjectNamePattern: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 129
        Height = 23
        Align = alClient
        Caption = 'Project Name Pattern'
        Layout = tlCenter
        ExplicitLeft = 8
        ExplicitTop = 9
        ExplicitWidth = 122
        ExplicitHeight = 16
      end
      object edtProjectNamePattern: TEdit
        AlignWithMargins = True
        Left = 139
        Top = 4
        Width = 100
        Height = 23
        Align = alClient
        TabOrder = 3
        Text = 'edtProjectNamePattern'
        OnExit = edtProjectNamePatternExit
        ExplicitLeft = 136
        ExplicitTop = 6
        ExplicitWidth = 169
        ExplicitHeight = 24
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 520
    Width = 840
    Height = 19
    Panels = <
      item
        Style = psOwnerDraw
        Width = 50
      end>
    OnDrawPanel = StatusBarDrawPanel
  end
  object pnlMainqq: TPanel
    Left = 0
    Top = 33
    Width = 840
    Height = 487
    Align = alClient
    Caption = 'pnlMain'
    TabOrder = 2
    object Splitter: TSplitter
      Left = 1
      Top = 375
      Width = 838
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ResizeStyle = rsUpdate
      ExplicitLeft = -6
      ExplicitTop = 358
    end
    object pnlMain: TPanel
      Left = 1
      Top = 1
      Width = 838
      Height = 374
      Align = alClient
      BevelOuter = bvNone
      Caption = 'pnlMasterDetail'
      TabOrder = 0
      object DBGridSplitter: TSplitter
        Left = 0
        Top = 257
        Width = 838
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        ResizeStyle = rsUpdate
        ExplicitTop = 0
        ExplicitWidth = 252
      end
      object DBGrid: TDBGrid
        Left = 0
        Top = 0
        Width = 838
        Height = 257
        Align = alClient
        DataSource = RevisionsDataSource
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -13
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
      object BlobsGrid: TDBGrid
        Left = 0
        Top = 260
        Width = 838
        Height = 114
        Align = alBottom
        DataSource = BlobsDataSource
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -13
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
    end
    object mmoGitOutput: TMemo
      Left = 1
      Top = 378
      Width = 838
      Height = 108
      Align = alBottom
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 1
    end
  end
  object FDConnection: TFDConnection
    Params.Strings = (
      'SERVER=SEASONSFALL0001\SQLEXPRESS2008'
      'OSAuthent=Yes'
      'ApplicationName=Enterprise/Architect/Ultimate'
      'Workstation=SEASONSFALL0001'
      'MARS=yes'
      'ODBCAdvanced=ServerSPN=JEDIVCS24'
      'Database=JEDIVCS24'
      'DriverID=MSSQL'
      'User_Name=sysdba')
    Connected = True
    LoginPrompt = False
    Left = 80
    Top = 104
  end
  object RevisionsDataSource: TDataSource
    DataSet = RevisionsFDQuery
    Left = 744
    Top = 96
  end
  object RevisionsFDQuery: TFDQuery
    Connection = FDConnection
    SQL.Strings = (
      'SELECT DISTINCT'
      '  M.MODULEID, M.NAME AS [Module Name], M.PATH,'
      '  R.REVISIONID, R.VERSION, R.REVISION, R.COMMENT_I,'
      '  VL.TSTAMP, VL.DESCRIPTION'
      'FROM projects P'
      '  INNER JOIN pjmodule PM ON P.PROJECTID = PM.PROJECTID'
      '  INNER JOIN modules M ON PM.MODULEID = M.MODULEID'
      '  INNER JOIN revision R ON M.MODULEID = R.MODULEID'
      '  INNER JOIN logcomm L ON L.REVISIONID = R.REVISIONID'
      '  INNER JOIN vcslog VL ON L.LOGID = VL.LOGID'
      'WHERE'
      '  P.NAME LIKE !ProjectNamePattern'
      'ORDER BY'
      '  TSTAMP, MODULEID, VERSION, REVISION')
    Left = 632
    Top = 96
    MacroData = <
      item
        Value = 'BrowseAndDocIt%'
        Name = 'PROJECTNAMEPATTERN'
        DataType = mdString
      end>
  end
  object BlobsFDQuery: TFDQuery
    IndexFieldNames = 'revisionid'
    MasterSource = RevisionsDataSource
    MasterFields = 'revisionid'
    Connection = FDConnection
    SQL.Strings = (
      'SELECT *'
      'FROM Blobs')
    Left = 640
    Top = 344
  end
  object BlobsDataSource: TDataSource
    DataSet = BlobsFDQuery
    Left = 744
    Top = 352
  end
end

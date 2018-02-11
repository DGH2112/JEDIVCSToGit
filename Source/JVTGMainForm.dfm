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
    DesignSize = (
      840
      33)
    object lblProjectNamePattern: TLabel
      Left = 8
      Top = 9
      Width = 122
      Height = 16
      Caption = 'Project Name Pattern'
    end
    object btnGetRevisions: TButton
      Left = 736
      Top = 6
      Width = 97
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Get Revisions'
      TabOrder = 0
      OnClick = btnGetRevisionsClick
    end
    object edtProjectNamePattern: TEdit
      Left = 136
      Top = 6
      Width = 169
      Height = 24
      TabOrder = 1
      Text = 'edtProjectNamePattern'
      OnExit = edtProjectNamePatternExit
    end
    object pnlGitRepos: TGridPanel
      Left = 311
      Top = 6
      Width = 419
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      ColumnCollection = <
        item
          SizeStyle = ssAbsolute
          Value = 125.000000000000000000
        end
        item
          Value = 50.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 125.000000000000000000
        end
        item
          Value = 50.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 1
          Control = edtNewGitRepoPath
          Row = 0
        end
        item
          Column = 0
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
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 2
      object edtNewGitRepoPath: TEdit
        Left = 126
        Top = 1
        Width = 83
        Height = 23
        Align = alClient
        TabOrder = 0
        ExplicitLeft = 425
        ExplicitTop = 6
        ExplicitWidth = 305
        ExplicitHeight = 24
      end
      object lblNewGitRepoPath: TLabel
        Left = 1
        Top = 1
        Width = 125
        Height = 23
        Align = alClient
        Caption = 'New Git Repo Path'
        Layout = tlCenter
        ExplicitWidth = 106
        ExplicitHeight = 16
      end
      object lblOldGitRepoPath: TLabel
        Left = 209
        Top = 1
        Width = 125
        Height = 23
        Align = alClient
        Caption = 'Old Git Repo Path'
        Layout = tlCenter
        ExplicitWidth = 100
        ExplicitHeight = 16
      end
      object edtOldGitRepoPath: TEdit
        Left = 334
        Top = 1
        Width = 84
        Height = 23
        Align = alClient
        TabOrder = 1
        ExplicitLeft = 298
        ExplicitWidth = 121
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
        Width = 50
      end>
  end
  object pnlMainqq: TPanel
    Left = 0
    Top = 33
    Width = 840
    Height = 487
    Align = alClient
    Caption = 'pnlMain'
    TabOrder = 2
    ExplicitTop = 41
    ExplicitHeight = 479
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
      ExplicitHeight = 366
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
      ExplicitTop = 370
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

object frmExtractRelPath: TfrmExtractRelPath
  Left = 0
  Top = 0
  Caption = 'frmExtractRelPath'
  ClientHeight = 164
  ClientWidth = 608
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    608
    164)
  PixelsPerInch = 96
  TextHeight = 16
  object lblExistingGitRepoPath: TLabel
    Left = 8
    Top = 11
    Width = 124
    Height = 16
    Caption = '&Existing Git Repo Path'
  end
  object lblModulePath: TLabel
    Left = 8
    Top = 41
    Width = 70
    Height = 16
    Caption = '&Module Path'
  end
  object lblModuleName: TLabel
    Left = 8
    Top = 71
    Width = 70
    Height = 16
    Caption = 'Module &Path'
  end
  object lblRelPath: TLabel
    Left = 8
    Top = 101
    Width = 74
    Height = 16
    Caption = '&Relative Path'
  end
  object edtExistingGitRepoPath: TEdit
    Left = 160
    Top = 8
    Width = 440
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    ReadOnly = True
    TabOrder = 0
    Text = 'edtExistingGitRepoPath'
    ExplicitWidth = 557
  end
  object edtModulePath: TEdit
    Left = 160
    Top = 38
    Width = 440
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    ReadOnly = True
    TabOrder = 1
    Text = 'edtExistingGitRepoPath'
    ExplicitWidth = 557
  end
  object edtModuleName: TEdit
    Left = 160
    Top = 68
    Width = 440
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    ReadOnly = True
    TabOrder = 2
    Text = 'edtExistingGitRepoPath'
    ExplicitWidth = 557
  end
  object edtRelPath: TEdit
    Left = 160
    Top = 98
    Width = 440
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = 'edtExistingGitRepoPath'
    ExplicitWidth = 557
  end
  object btnAbort: TBitBtn
    Left = 525
    Top = 131
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkAbort
    NumGlyphs = 2
    TabOrder = 4
    ExplicitLeft = 529
    ExplicitTop = 148
  end
  object btnCancel: TBitBtn
    Left = 444
    Top = 131
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 5
    ExplicitLeft = 448
    ExplicitTop = 148
  end
  object btnOK: TBitBtn
    Left = 363
    Top = 131
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 6
    ExplicitLeft = 367
    ExplicitTop = 148
  end
end

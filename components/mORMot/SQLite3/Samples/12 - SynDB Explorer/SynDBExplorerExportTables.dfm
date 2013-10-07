object DBExportTablesForm: TDBExportTablesForm
  Left = 387
  Top = 248
  BorderStyle = bsDialog
  Caption = ' SynDB Explorer - Export Tables'
  ClientHeight = 471
  ClientWidth = 393
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    393
    471)
  PixelsPerInch = 96
  TextHeight = 13
  object BtnExport: TButton
    Left = 24
    Top = 430
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Export'
    ModalResult = 1
    TabOrder = 0
  end
  object BtnCancel: TButton
    Left = 128
    Top = 430
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GroupWhere: TGroupBox
    Left = 16
    Top = 8
    Width = 360
    Height = 345
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Per-table additional WHERE clause for export '
    TabOrder = 2
  end
  object ChkUseStandardCollations: TCheckBox
    Left = 24
    Top = 379
    Width = 361
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 
      'Use standard SQLite3 format (collations not optimized for mORMot' +
      ')'
    TabOrder = 3
  end
  object ChkNoBlobExport: TCheckBox
    Left = 24
    Top = 362
    Width = 361
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Do not export BLOB content (save space and time)'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object ChkZipDBFile: TCheckBox
    Left = 24
    Top = 397
    Width = 361
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Compress resulting SQLite3 DB file into .zip archive'
    TabOrder = 5
  end
end

object DBQueryBuilderForm: TDBQueryBuilderForm
  Left = 1423
  Top = 215
  BorderStyle = bsSingle
  Caption = ' SynDB Explorer - Query Builder'
  ClientHeight = 374
  ClientWidth = 799
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    799
    374)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupJoin: TGroupBox
    Left = 344
    Top = 8
    Width = 441
    Height = 201
    Caption = ' Select how Tables are JOINed '
    TabOrder = 0
  end
  object GroupFields: TGroupBox
    Left = 8
    Top = 8
    Width = 329
    Height = 201
    Caption = ' Select Columns to be retrieved for each table '
    TabOrder = 1
    DesignSize = (
      329
      201)
    object FieldsTable: TListBox
      Left = 8
      Top = 16
      Width = 129
      Height = 177
      Anchors = [akLeft, akTop, akBottom]
      ItemHeight = 13
      TabOrder = 0
      OnClick = FieldsTableClick
    end
    object FieldsColumn: TCheckListBox
      Left = 144
      Top = 33
      Width = 177
      Height = 160
      Anchors = [akLeft, akTop, akBottom]
      ItemHeight = 13
      TabOrder = 1
      OnClick = FieldsColumnClick
    end
    object FieldsAll: TCheckBox
      Left = 144
      Top = 16
      Width = 97
      Height = 17
      Caption = 'all columns (*)'
      TabOrder = 2
      OnClick = FieldsAllClick
    end
  end
  object MemoSQL: TMemo
    Left = 8
    Top = 215
    Width = 777
    Height = 113
    Anchors = [akLeft, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssHorizontal
    TabOrder = 2
  end
  object BtnUseSQL: TButton
    Left = 24
    Top = 335
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Use SQL'
    ModalResult = 1
    TabOrder = 3
  end
  object BtnCancel: TButton
    Left = 336
    Top = 335
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object BtnExecSQL: TButton
    Left = 112
    Top = 335
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Exec SQL'
    ModalResult = 6
    TabOrder = 5
  end
  object BtnToObject: TButton
    Left = 232
    Top = 335
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'To Object'
    ModalResult = 4
    TabOrder = 6
  end
end

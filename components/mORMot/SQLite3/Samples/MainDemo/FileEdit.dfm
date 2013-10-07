object EditForm: TEditForm
  Left = 356
  Top = 250
  Width = 630
  Height = 535
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    614
    497)
  PixelsPerInch = 96
  TextHeight = 13
  object Name: TLabeledEdit
    Left = 96
    Top = 24
    Width = 257
    Height = 21
    EditLabel.Width = 27
    EditLabel.Height = 13
    EditLabel.Caption = 'Name'
    LabelPosition = lpLeft
    TabOrder = 0
  end
  object KeyWords: TLabeledEdit
    Left = 96
    Top = 56
    Width = 456
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 49
    EditLabel.Height = 13
    EditLabel.Caption = 'KeyWords'
    LabelPosition = lpLeft
    TabOrder = 1
  end
  object Memo: TMemo
    Left = 16
    Top = 88
    Width = 576
    Height = 332
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 2
  end
end

object RecordEditForm: TRecordEditForm
  Left = 314
  Top = 204
  BorderStyle = bsDialog
  ClientHeight = 510
  ClientWidth = 529
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BottomPanel: TPanel
    Left = 0
    Top = 453
    Width = 529
    Height = 57
    Align = alBottom
    TabOrder = 0
  end
  object Scroll: TScrollBox
    Left = 0
    Top = 0
    Width = 529
    Height = 453
    Align = alClient
    TabOrder = 1
  end
end

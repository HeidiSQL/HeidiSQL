object frmQueryProgress: TfrmQueryProgress
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Status'
  ClientHeight = 78
  ClientWidth = 222
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 16
    Width = 158
    Height = 13
    Caption = 'Waiting for query to complete ...'
  end
  object Button1: TButton
    Left = 72
    Top = 44
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Abort'
    Default = True
    TabOrder = 0
  end
end

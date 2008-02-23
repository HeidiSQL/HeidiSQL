object frmUpdateDownload: TfrmUpdateDownload
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Downloading update file ...'
  ClientHeight = 95
  ClientWidth = 286
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    286
    95)
  PixelsPerInch = 96
  TextHeight = 13
  object lblStatus: TLabel
    Left = 8
    Top = 31
    Width = 270
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'lblStatus'
    WordWrap = True
  end
  object progressDownload: TProgressBar
    Left = 8
    Top = 8
    Width = 270
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 104
    Top = 62
    Width = 68
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
end

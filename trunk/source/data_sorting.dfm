object DataSortingForm: TDataSortingForm
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'DataSortingForm'
  ClientHeight = 63
  ClientWidth = 204
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBevel: TPanel
    Left = 0
    Top = 0
    Width = 204
    Height = 63
    Align = alClient
    BorderWidth = 3
    TabOrder = 0
    DesignSize = (
      204
      63)
    object btnOK: TButton
      Left = 3
      Top = 34
      Width = 60
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'OK'
      Default = True
      Enabled = False
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Left = 68
      Top = 34
      Width = 60
      Height = 25
      Anchors = [akLeft, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      OnClick = btnCancelClick
    end
    object btnAddCol: TButton
      Left = 134
      Top = 34
      Width = 60
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Add Col'
      TabOrder = 2
      OnClick = btnAddColClick
    end
  end
end

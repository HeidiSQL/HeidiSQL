object DataSortingForm: TDataSortingForm
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'DataSortingForm'
  ClientHeight = 97
  ClientWidth = 204
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBevel: TPanel
    Left = 0
    Top = 0
    Width = 204
    Height = 97
    Align = alClient
    BorderWidth = 3
    TabOrder = 0
    DesignSize = (
      204
      97)
    object btnOK: TButton
      Left = 3
      Top = 68
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
      Top = 68
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
      Top = 68
      Width = 60
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Add Col'
      TabOrder = 2
      OnClick = btnAddColClick
    end
    object btnReset: TButton
      Left = 69
      Top = 39
      Width = 125
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Reset sorting'
      ModalResult = 2
      TabOrder = 3
      OnClick = btnResetClick
    end
  end
  object timerRefresh: TTimer
    Enabled = False
    Interval = 100
    OnTimer = DisplaySortingControls
    Left = 8
    Top = 8
  end
end

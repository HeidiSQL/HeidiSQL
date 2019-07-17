object frmPasswordChange: TfrmPasswordChange
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Change expired password'
  ClientHeight = 187
  ClientWidth = 456
  Color = clBtnFace
  Constraints.MaxHeight = 300
  Constraints.MaxWidth = 600
  Constraints.MinHeight = 185
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    456
    187)
  PixelsPerInch = 96
  TextHeight = 13
  object lblHeading: TLabel
    Left = 8
    Top = 16
    Width = 440
    Height = 51
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    Caption = 'lblHeading'
    WordWrap = True
  end
  object lblPassword: TLabel
    Left = 8
    Top = 76
    Width = 74
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'New password:'
  end
  object lblRepeatPassword: TLabel
    Left = 8
    Top = 103
    Width = 111
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Repeat new password:'
  end
  object lblStatus: TLabel
    Left = 8
    Top = 132
    Width = 41
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'lblStatus'
  end
  object editPassword: TButtonedEdit
    Left = 146
    Top = 73
    Width = 302
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    Images = MainForm.VirtualImageListMain
    RightButton.DropDownMenu = popupPassword
    RightButton.ImageIndex = 75
    RightButton.Visible = True
    TabOrder = 0
    TextHint = 'Type new password or select a suggestion'
    OnChange = editPasswordChange
    OnClick = editPasswordChange
    OnKeyDown = editPasswordKeyDown
  end
  object editRepeatPassword: TButtonedEdit
    Left = 146
    Top = 100
    Width = 302
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    Images = MainForm.VirtualImageListMain
    PasswordChar = '*'
    TabOrder = 1
    TextHint = 'Retype password'
    OnChange = editPasswordChange
    OnClick = editPasswordChange
    OnKeyDown = editPasswordKeyDown
  end
  object btnCancel: TButton
    Left = 292
    Top = 154
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 213
    Top = 154
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCopyToClipboard: TButton
    Left = 373
    Top = 154
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Copy'
    ImageIndex = 3
    Images = MainForm.VirtualImageListMain
    TabOrder = 4
    OnClick = btnCopyToClipboardClick
  end
  object progressbarPasswordStrength: TProgressBar
    Left = 146
    Top = 131
    Width = 302
    Height = 17
    TabOrder = 5
  end
  object popupPassword: TPopupMenu
    Left = 344
    Top = 8
    object N6characters1: TMenuItem
      Caption = '6 characters'
      OnClick = menuPasswordClick
      object menuDummy1: TMenuItem
        Caption = 'dummy'
        OnClick = menuPasswordInsert
      end
    end
    object N8characters1: TMenuItem
      Caption = '8 characters'
      OnClick = menuPasswordClick
      object menuDummy2: TMenuItem
        Caption = 'dummy'
      end
    end
    object N10characters1: TMenuItem
      Caption = '10 characters'
      OnClick = menuPasswordClick
      object menuDummy3: TMenuItem
        Caption = 'dummy'
      end
    end
    object N12characters1: TMenuItem
      Caption = '12 characters'
      OnClick = menuPasswordClick
      object menuDummy4: TMenuItem
        Caption = 'dummy'
      end
    end
    object N30characters1: TMenuItem
      Caption = '30 characters'
      OnClick = menuPasswordClick
      object menuDummy5: TMenuItem
        Caption = 'dummy'
      end
    end
  end
end

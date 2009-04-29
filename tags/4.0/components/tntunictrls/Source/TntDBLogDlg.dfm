object TntLoginDialog: TTntLoginDialog
  Left = 307
  Top = 131
  ActiveControl = Password
  BorderStyle = bsDialog
  Caption = 'Database Login'
  ClientHeight = 147
  ClientWidth = 273
  Color = clBtnFace
  ParentFont = True
  
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object OKButton: TTntButton
    Left = 109
    Top = 114
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelButton: TTntButton
    Left = 190
    Top = 114
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Panel: TTntPanel
    Left = 8
    Top = 7
    Width = 257
    Height = 98
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    object Label3: TTntLabel
      Left = 10
      Top = 6
      Width = 50
      Height = 13
      Caption = 'Database:'
    end
    object DatabaseName: TTntLabel
      Left = 91
      Top = 6
      Width = 3
      Height = 13
    end
    object Bevel: TTntBevel
      Left = 1
      Top = 24
      Width = 254
      Height = 9
      Shape = bsTopLine
    end
    object Panel1: TTntPanel
      Left = 2
      Top = 31
      Width = 253
      Height = 65
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TTntLabel
        Left = 8
        Top = 8
        Width = 56
        Height = 13
        Caption = '&User Name:'
        FocusControl = UserName
      end
      object Label2: TTntLabel
        Left = 8
        Top = 36
        Width = 50
        Height = 13
        Caption = '&Password:'
        FocusControl = Password
      end
      object UserName: TTntEdit
        Left = 86
        Top = 5
        Width = 153
        Height = 21
        MaxLength = 31
        TabOrder = 0
      end
      object Password: TTntEdit
        Left = 86
        Top = 33
        Width = 153
        Height = 21
        MaxLength = 31
        PasswordCharW = #9679
        TabOrder = 1
        PasswordChar_UTF7 = '+Jc8'
      end
    end
  end
end

object FieldEditForm: TFieldEditForm
  Left = 572
  Top = 111
  BorderStyle = bsDialog
  BorderWidth = 5
  Caption = 'Field-Editor'
  ClientHeight = 354
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pc: TPageControl
    Left = 0
    Top = 0
    Width = 297
    Height = 321
    ActivePage = tabField
    Align = alTop
    Images = MainForm.PngImageListMain
    TabOrder = 0
    OnChange = pcChange
    object tabField: TTabSheet
      Caption = 'Field'
      ImageIndex = 42
      object lblName: TLabel
        Left = 8
        Top = 40
        Width = 31
        Height = 13
        Caption = '&Name:'
        FocusControl = EditFieldname
      end
      object lblType: TLabel
        Left = 8
        Top = 64
        Width = 28
        Height = 13
        Caption = '&Type:'
        FocusControl = ComboBoxType
      end
      object lblLengthSet: TLabel
        Left = 8
        Top = 88
        Width = 63
        Height = 13
        Caption = '&Length / Set:'
        FocusControl = EditLength
      end
      object lblDefault: TLabel
        Left = 8
        Top = 112
        Width = 39
        Height = 13
        Caption = '&Default:'
        FocusControl = EditDefault
      end
      object lblPosition: TLabel
        Left = 8
        Top = 17
        Width = 41
        Height = 13
        Caption = '&Position:'
        FocusControl = ComboBoxPosition
      end
      object lblComment: TLabel
        Left = 8
        Top = 136
        Width = 49
        Height = 13
        Caption = '&Comment:'
        FocusControl = EditComment
      end
      object btnDatatypeHelp: TButton
        Left = 259
        Top = 61
        Width = 21
        Height = 21
        Hint = 'Help on selected datatype'
        Caption = '?'
        TabOrder = 3
        OnClick = btnDatatypeHelpClick
      end
      object EditDefault: TEdit
        Left = 88
        Top = 109
        Width = 192
        Height = 21
        TabOrder = 5
      end
      object EditLength: TEdit
        Left = 88
        Top = 85
        Width = 192
        Height = 21
        TabOrder = 4
      end
      object ComboBoxType: TComboBox
        Left = 88
        Top = 61
        Width = 165
        Height = 19
        Style = csOwnerDrawFixed
        ItemHeight = 13
        TabOrder = 2
        OnChange = ComboBoxTypeChange
        OnDrawItem = ComboBoxTypeDrawItem
        OnKeyDown = ComboBoxTypeKeyDown
        OnKeyUp = ComboBoxTypeKeyUp
      end
      object EditFieldname: TEdit
        Left = 88
        Top = 37
        Width = 192
        Height = 21
        TabOrder = 1
      end
      object GroupBoxAttributes: TGroupBox
        Left = 8
        Top = 168
        Width = 272
        Height = 113
        Caption = 'Attributes'
        TabOrder = 7
        object CheckBoxBinary: TCheckBox
          Left = 16
          Top = 24
          Width = 65
          Height = 17
          Caption = 'Binary'
          TabOrder = 0
        end
        object CheckBoxUnsigned: TCheckBox
          Left = 16
          Top = 48
          Width = 65
          Height = 17
          Caption = 'Unsigned'
          TabOrder = 1
          OnClick = CheckBoxUnsignedClick
        end
        object CheckBoxZerofill: TCheckBox
          Left = 16
          Top = 72
          Width = 57
          Height = 17
          Caption = 'Zerofill'
          TabOrder = 2
          OnClick = CheckBoxZerofillClick
        end
        object CheckBoxNotNull: TCheckBox
          Left = 128
          Top = 24
          Width = 69
          Height = 17
          Caption = 'Not Null'
          TabOrder = 3
        end
        object CheckBoxAutoIncrement: TCheckBox
          Left = 128
          Top = 48
          Width = 97
          Height = 17
          Caption = 'AutoIncrement'
          TabOrder = 4
        end
      end
      object ComboBoxPosition: TComboBox
        Left = 88
        Top = 14
        Width = 192
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
      end
      object EditComment: TEdit
        Left = 88
        Top = 133
        Width = 192
        Height = 21
        TabOrder = 6
      end
    end
    object tabIndexes: TTabSheet
      Caption = 'Indexes'
      ImageIndex = 13
      ParentShowHint = False
      ShowHint = True
      object lblIndexName: TLabel
        Left = 8
        Top = 11
        Width = 63
        Height = 13
        Caption = '&Index-Name:'
        FocusControl = ComboBoxKeys
      end
      object lblColumnsUsed: TLabel
        Left = 8
        Top = 104
        Width = 70
        Height = 13
        Caption = 'Columns used:'
      end
      object lblColumnsAvailable: TLabel
        Left = 168
        Top = 104
        Width = 90
        Height = 13
        Caption = 'Available Columns:'
      end
      object btnAddColumnToIndex: TPngSpeedButton
        Tag = 1
        Left = 133
        Top = 152
        Width = 25
        Height = 25
        Hint = 'Add field to index'
        Flat = True
        OnClick = AddField
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000000F54944415478DA63FCFFFF3F032580719819507222830D
          486DEDB198E14AAE01B7FFFEFE2BD36F3B9B936403809A2F8AB349E8FDF9F387
          E1E1BB870CBF7EFE66F8F503847F31FC046230FD1DC106C95D9C708D116C0050
          F34931567133451E65863FFFFEC0F1EFBF7F81F837C3EF7F40FC1728F6F70F84
          06F257AF5CCB707DFA6D88018587538F88B28959CBF32A42348314820DF80DA1
          FF400D80CAFDF9FF8761CDB2F50C77E6DE67847B216347EC79613611030E664E
          86872F1F32BC7EFF06ABB3415EFBFD0B68C8EF3F0C0F163D664409C49895C137
          810AE5801A1C37A6EF3C41722C842DF465036ADE04D4EC31300969681A00002A
          EDDDE1EE0C96E70000000049454E44AE426082}
      end
      object btnDeleteColumnFromIndex: TPngSpeedButton
        Tag = 2
        Left = 133
        Top = 184
        Width = 25
        Height = 25
        Hint = 'Remove field from index'
        Flat = True
        OnClick = RemoveField
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000000FD4944415478DA63FCFFFF3F032580718419E03FD37DF7
          AF1FBFBCB7E7EFFF4596019E131DBFFFFCFEEBC9BE8AA3AA2806949CC8009BF2
          F7F75F863FBFFF30FCFEF587E1D7CFDF0CBF7E80F02F869F400CA2258524197E
          FDFACD70F3F6AD4BA73B2EEAA31860256CCBF0F71FD0807F7FE0F8F7DFBF40FC
          9BE1F73F20FE0B14FBFB074C9F3B7F96E1E68DDBA7AE4FBF6D0E36A0F070EA7F
          0B516B84669042B001BF21F41FA80150B973E7CE33DCBA7EE7E89DB9F76DC006
          E4EE4BFC8FCBD9403F83D95C1C5C0C9292520CAF5FBD66B87DE3EE05A0664392
          0251BF40CB0268C17EA0458F809AD5C98A059564C51DC040F67BB0E83179D148
          71421A9C060000DB7FEAE1DBB736010000000049454E44AE426082}
      end
      object btnAddAllColumnsToIndex: TPngSpeedButton
        Left = 133
        Top = 120
        Width = 25
        Height = 25
        Hint = 'Add all fields to index'
        Flat = True
        OnClick = btnAddAllColumnsToIndexClick
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B22592000002274944415478DAA5D35F4853511800F06F2CEFD06D64732B
          EFFC43C2DA4B26D1A288961122155162294952EE2956BE4A20850F3E06F51AA3
          D062617FE8A13604B7118E25B93D883899FF46F75E560F091384E6C3EE39E7DE
          BE7BD362246DD007E7E13B707E7CDFF9CE31A8AA0AFF13864A81C3836E23A5AC
          8151068C29203ECFE52A068E0D1DE9A18406AA38CEC6080288147E6C81F82267
          280B9C1AF1B8A8CC32C73D1EAED6560B5461A0E07AFF260C5FC6A452E0DAD825
          8ECA34F4C11FB9B0B37766F4E4B8ABC5E56B6A6A06A650602AD391D0DB30AC3D
          154A81EBC12BAB08341342CF85FCD164E7A3762FE6F10E6F8791300299A52510
          0411E422D1DB587E92FD030CBCEB9DE7ADCEA366CE0CE27709F29BF9A2C56435
          F1761E1C75FB21BD9C0641141E233E3CF7302D974CE1F664FF4CBDD979DAED70
          6379542F51663296CCF47C3DBF0EC9F954347E7FF6FCAE63BC13B995AAAFE14F
          1CAA4340458069000145557E011B082C24A31FEFCDEC0E68E19FBAB9C0D7F06D
          386B90F21250C28A16CE623AB09707FB3E3B2C661741FA2AE92D241E24E5BF80
          6D248B071B9F5D9EA8D6F2BE975D5E9C7FBCBDF5AC7E892BC20A88DF24BC4419
          1082D9D1B9D2290CC67C1C563019B818ECDCD9EB0B768D1FB4B7F87887135BA3
          BFD7D474043E8DA4CA3FA4FED7575D5845A6B5B18DB35AAC40B681682202D3C3
          9FCB033AF2AABB074B0EEC812A1B6240640A85AD02C4861295015ADC98E836E2
          E106AD777C5C10BE1BABFC33FD2B7E02A38E58F0529AE0B50000000049454E44
          AE426082}
      end
      object btnDeleteAllColumnsFromIndex: TPngSpeedButton
        Left = 133
        Top = 216
        Width = 25
        Height = 25
        Hint = 'Remove all fields from index'
        Flat = True
        OnClick = btnDeleteAllColumnsFromIndexClick
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001874455874536F667477617265005061696E742E4E45542076332E31
          3072B22592000002284944415478DA63FCFFFF3F032580116680EF3457391636
          160656561606165696A7CBA2D6FF25DA00D71EBBFF3CDC3C0CAC6C60CD0C7F18
          7EBF031A94BE3472FD1AA20C706CB7FAEF66E7CEC0C20C7401107FFEF299E1CA
          934BBF8086692F8D587787A001B64DE6FF3D1C2106C0F0F3D7CF181EBCB9BF60
          45ECC64498E2F4EDB1BB595898BDA7BA2EF8856280659DF17F90DFD9D8D91814
          651418349434802E61653874E5E05FA02B1C56C46C3C02529CB239EA3B0B2BF3
          93191E8B5531021104EC5A2CD88006B52BC82A14E9AAEA32BC79FF86E1E5C7E7
          0C5F7E7DF909D4C8AE20A2C0007401C3F36FCF2F010DD1C73000069CBB6C765A
          E85BB8890B8B33B030B13030313231B0015DC4C2CCCCC0C2C8C270FBED2D8617
          DF9E9F9AEEBEC81CAB010EAD963B2D0CCDDDC4452006303331030D6003B299C1
          FC5BAF81067C7D767496F7521B14038CCBF4C05E5052542AD2D3D46378FDF615
          C3F337402FFCFCFC534440845D514281E1EBAFAF0CCF3F3FBBB03064B521DC0B
          9A99AAFF99817E6363676550525264D0D6D20207E2DE237BFF021397C3EEE243
          47FC66B859000DDF0FE43F5A19BB491D2510D55295FEFB85F9829DC8CC08C440
          673E7EFC88E1CEFD3B0B0ED79D8447A3FF0CF71D4003FCD6266D458D46E52485
          FF01E1BE0C4C603F32337C78F781E1CCD9B3BF58D898B58F379D259C9014E3E5
          FEF3F0723380BCC1CCCACCF0FBD7AF77C0F84F3FD77399B8A40C028A097272CC
          CC4C60438071FDF4EAD45BC467264A00000AF7DBE129230F2A0000000049454E
          44AE426082}
      end
      object ComboBoxKeys: TComboBoxEx
        Left = 72
        Top = 8
        Width = 209
        Height = 22
        ItemsEx = <>
        Style = csExDropDownList
        ItemHeight = 16
        TabOrder = 0
        OnChange = ComboBoxKeysChange
        Images = MainForm.PngImageListMain
      end
      object CheckBoxUnique: TCheckBox
        Left = 8
        Top = 72
        Width = 57
        Height = 17
        Caption = 'Unique'
        TabOrder = 1
        OnClick = CheckBoxUniqueClick
      end
      object ButtonAdd: TButton
        Left = 144
        Top = 32
        Width = 65
        Height = 25
        Caption = 'Add'
        TabOrder = 2
        OnClick = ButtonAddClick
      end
      object ButtonDelete: TButton
        Left = 216
        Top = 32
        Width = 65
        Height = 25
        Caption = 'Delete'
        Enabled = False
        TabOrder = 3
        OnClick = ButtonDeleteClick
      end
      object listColumnsUsed: TListBox
        Left = 8
        Top = 120
        Width = 113
        Height = 161
        Enabled = False
        ItemHeight = 13
        TabOrder = 4
        OnClick = listClick
        OnDblClick = RemoveField
      end
      object listColumnsAvailable: TListBox
        Left = 168
        Top = 120
        Width = 113
        Height = 161
        Enabled = False
        ItemHeight = 13
        TabOrder = 5
        OnClick = listClick
        OnDblClick = AddField
      end
      object ButtonAddPrimary: TButton
        Left = 72
        Top = 32
        Width = 67
        Height = 25
        Caption = 'Add Primary'
        Enabled = False
        TabOrder = 6
        OnClick = ButtonAddPrimaryClick
      end
      object CheckBoxFulltext: TCheckBox
        Left = 88
        Top = 72
        Width = 97
        Height = 17
        Caption = 'Fulltext'
        TabOrder = 7
        OnClick = CheckBoxFulltextClick
      end
    end
  end
  object ButtonCancel: TButton
    Left = 207
    Top = 327
    Width = 90
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ButtonOK: TButton
    Left = 111
    Top = 327
    Width = 90
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = OKClick
  end
end

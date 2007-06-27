object frmMain: TfrmMain
  Left = 264
  Top = 174
  Width = 454
  Height = 401
  Caption = 'Design Time Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object navMain: TDBNavigator
    Left = 0
    Top = 0
    Width = 446
    Height = 25
    DataSource = dsMain
    Align = alTop
    TabOrder = 0
  end
  object gdMain: TDBGrid
    Left = 0
    Top = 25
    Width = 446
    Height = 349
    Align = alClient
    DataSource = dsMain
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object conMain: TZConnection
    Protocol = 'mysql'
    HostName = 'localhost'
    Port = 0
    Database = 'zeoslib'
    User = 'root'
    AutoCommit = True
    ReadOnly = True
    TransactIsolationLevel = tiNone
    Connected = True
    Left = 56
    Top = 72
  end
  object qrMain: TZQuery
    Connection = conMain
    OnCalcFields = qrMainCalcFields
    RequestLive = True
    CachedUpdates = False
    SQL.Strings = (
      'select * from department')
    ParamCheck = True
    Params = <>
    ShowRecordTypes = [utModified, utInserted, utUnmodified]
    Left = 96
    Top = 72
  end
  object dsMain: TDataSource
    DataSet = qrMain
    Left = 176
    Top = 72
  end
  object upMain: TZUpdateSQL
    DeleteSQL.Strings = (
      'DELETE FROM :Id')
    InsertSQL.Strings = (
      'INSERT INTO :Name')
    ModifySQL.Strings = (
      'UPDATE :Address, :Id')
    Left = 136
    Top = 72
    ParamData = <
      item
        DataType = ftUnknown
        Name = 'Address'
        ParamType = ptUnknown
      end
      item
        DataType = ftString
        Name = 'Id'
        ParamType = ptUnknown
        Value = 'xxx'
      end
      item
        DataType = ftString
        Name = 'Name'
        ParamType = ptUnknown
        Value = 'qwe'
      end>
  end
end

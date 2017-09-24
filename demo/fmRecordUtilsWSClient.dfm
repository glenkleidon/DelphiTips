object PlayerForm: TPlayerForm
  Left = 0
  Top = 0
  Caption = 'Player:'
  ClientHeight = 162
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    418
    162)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 265
    Top = 74
    Width = 34
    Height = 13
    Caption = 'Format'
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 418
    Height = 73
    Align = alTop
    TabOrder = 1
    object CardHolderPanel: TPanel
      Left = 0
      Top = 0
      Width = 414
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object ErrorMsg: TLabel
        Left = 0
        Top = 0
        Width = 414
        Height = 41
        Align = alClient
        Alignment = taCenter
        AutoSize = False
        Caption = 'Error Msg'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        Layout = tlCenter
        Visible = False
        ExplicitLeft = 160
        ExplicitTop = 24
        ExplicitWidth = 46
        ExplicitHeight = 13
      end
    end
  end
  object DealButton: TButton
    Left = 265
    Top = 129
    Width = 72
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Hit Me'
    TabOrder = 2
    OnClick = DealButtonClick
  end
  object SpinEdit1: TSpinEdit
    Left = 370
    Top = 128
    Width = 40
    Height = 26
    Anchors = [akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    MaxValue = 52
    MinValue = 1
    ParentFont = False
    TabOrder = 3
    Value = 1
  end
  object PlayersNameEdit: TLabeledEdit
    Left = 8
    Top = 133
    Width = 251
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 65
    EditLabel.Height = 13
    EditLabel.Caption = 'Players Name'
    TabOrder = 4
    OnChange = PlayersNameEditChange
  end
  object ResponseFormat: TComboBox
    Left = 265
    Top = 90
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 5
    Text = 'application/json'
    Items.Strings = (
      'application/json'
      'application/x-www-form-urlencoded'
      'application/octet-stream')
  end
  object DefaultCardPanel: TPanel
    Left = 16
    Top = 8
    Width = 66
    Height = 41
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clCream
    Font.Charset = OEM_CHARSET
    Font.Color = clWindowText
    Font.Height = -29
    Font.Name = 'Terminal'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 0
    Visible = False
  end
  object URLEdit: TLabeledEdit
    Left = 8
    Top = 90
    Width = 251
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 19
    EditLabel.Height = 13
    EditLabel.Caption = 'URL'
    TabOrder = 6
    OnChange = PlayersNameEditChange
  end
end

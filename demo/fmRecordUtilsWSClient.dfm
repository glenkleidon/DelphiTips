object PlayerForm: TPlayerForm
  Left = 0
  Top = 0
  Caption = 'Player:'
  ClientHeight = 182
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
    182)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 265
    Top = 82
    Width = 34
    Height = 13
    Caption = 'Format'
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 418
    Height = 81
    Align = alTop
    TabOrder = 1
    object ErrorMsg: TLabel
      Left = 0
      Top = 53
      Width = 414
      Height = 24
      Align = alBottom
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
    end
    object CardHolderPanel: TPanel
      Left = 0
      Top = 0
      Width = 414
      Height = 41
      BevelOuter = bvNone
      TabOrder = 0
    end
  end
  object SpinEdit1: TSpinEdit
    Left = 370
    Top = 136
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
    TabOrder = 2
    Value = 1
  end
  object PlayersNameEdit: TLabeledEdit
    Left = 8
    Top = 141
    Width = 251
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 65
    EditLabel.Height = 13
    EditLabel.Caption = 'Players Name'
    TabOrder = 3
    OnChange = PlayersNameEditChange
  end
  object ResponseFormat: TComboBox
    Left = 265
    Top = 98
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 4
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
    Top = 98
    Width = 251
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 19
    EditLabel.Height = 13
    EditLabel.Caption = 'URL'
    TabOrder = 5
    OnChange = PlayersNameEditChange
  end
  object DealButton: TButton
    Left = 265
    Top = 136
    Width = 72
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Hit Me'
    TabOrder = 6
    OnClick = DealButtonClick
  end
end

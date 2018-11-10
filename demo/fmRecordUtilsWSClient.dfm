object PlayerForm: TPlayerForm
  Left = 0
  Top = 0
  Caption = 'Player:'
  ClientHeight = 201
  ClientWidth = 423
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ErrorMsg: TLabel
    Left = 0
    Top = 0
    Width = 423
    Height = 24
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
    ExplicitLeft = 8
    ExplicitTop = 18
    ExplicitWidth = 418
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 138
    Width = 423
    Height = 63
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    ExplicitLeft = 8
    ExplicitTop = 159
    ExplicitWidth = 418
    ExplicitHeight = 57
    object CardHolderPanel: TPanel
      Left = 0
      Top = 0
      Width = 414
      Height = 41
      BevelOuter = bvNone
      TabOrder = 0
    end
  end
  object UserPanel: TPanel
    Left = 0
    Top = 24
    Width = 423
    Height = 114
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 57
    ExplicitWidth = 418
    ExplicitHeight = 120
    DesignSize = (
      423
      114)
    object Label1: TLabel
      Left = 265
      Top = 6
      Width = 34
      Height = 13
      Caption = 'Format'
    end
    object URLEdit: TLabeledEdit
      Left = 8
      Top = 22
      Width = 256
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 19
      EditLabel.Height = 13
      EditLabel.Caption = 'URL'
      TabOrder = 0
      OnChange = PlayersNameEditChange
      ExplicitWidth = 251
    end
    object SpinEdit1: TSpinEdit
      Left = 375
      Top = 72
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
      TabOrder = 1
      Value = 1
      ExplicitLeft = 370
    end
    object ResponseFormat: TComboBox
      Left = 265
      Top = 22
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 2
      Text = 'application/json'
      Items.Strings = (
        'application/json'
        'application/x-www-form-urlencoded'
        'application/octet-stream')
    end
    object PlayersNameEdit: TLabeledEdit
      Left = 8
      Top = 75
      Width = 256
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 65
      EditLabel.Height = 13
      EditLabel.Caption = 'Players Name'
      TabOrder = 3
      OnChange = PlayersNameEditChange
      ExplicitWidth = 251
    end
    object DealButton: TButton
      Left = 270
      Top = 71
      Width = 72
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Hit Me'
      TabOrder = 4
      OnClick = DealButtonClick
      ExplicitLeft = 265
    end
  end
  object DefaultCardPanel: TPanel
    Left = 8
    Top = 152
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
    TabOrder = 2
    Visible = False
  end
end

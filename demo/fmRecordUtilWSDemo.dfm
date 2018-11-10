object ServerForm: TServerForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Cards'
  ClientHeight = 368
  ClientWidth = 186
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ServerGroupBox: TGroupBox
    Left = 0
    Top = 0
    Width = 186
    Height = 368
    Align = alClient
    Caption = 'Server'#39's Deck'
    TabOrder = 0
    DesignSize = (
      186
      368)
    object CardsLeftLabel: TLabel
      Left = 8
      Top = 329
      Width = 65
      Height = 24
      Alignment = taCenter
      Anchors = [akLeft, akBottom]
      AutoSize = False
      Caption = '0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object CardPanel: TPanel
      Left = 79
      Top = 19
      Width = 101
      Height = 41
      BevelOuter = bvNone
      Color = clCream
      Font.Charset = OEM_CHARSET
      Font.Color = clWindowText
      Font.Height = -29
      Font.Name = 'Terminal'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      OnClick = CardPanelClick
    end
    object ListBox1: TListBox
      Left = 8
      Top = 50
      Width = 65
      Height = 273
      Anchors = [akLeft, akTop, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -27
      Font.Name = 'Terminal'
      Font.Style = []
      ItemHeight = 24
      ParentFont = False
      TabOrder = 1
      OnClick = ListBox1Click
    end
    object Button1: TButton
      Left = 6
      Top = 19
      Width = 67
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Shuffle'
      TabOrder = 2
      OnClick = Button1Click
    end
    object GroupBox1: TGroupBox
      Left = 79
      Top = 279
      Width = 101
      Height = 74
      Caption = 'Players'
      TabOrder = 3
      object ClientType: TComboBox
        Left = 8
        Top = 14
        Width = 82
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 0
        Text = 'Windows'
        Items.Strings = (
          'Windows'
          'Web')
      end
      object AddClientButton: TButton
        Left = 16
        Top = 41
        Width = 59
        Height = 22
        Caption = 'Add'
        TabOrder = 1
        OnClick = AddClientButtonClick
      end
    end
  end
  object DealerGroupBox: TGroupBox
    Left = 79
    Top = 66
    Width = 101
    Height = 207
    Caption = 'Dealer Cards'
    TabOrder = 1
    DesignSize = (
      101
      207)
    object DealButton: TButton
      Left = 4
      Top = 20
      Width = 41
      Height = 25
      Caption = 'Deal'
      Enabled = False
      TabOrder = 0
      OnClick = DealButtonClick
    end
    object SpinEdit1: TSpinEdit
      Left = 53
      Top = 20
      Width = 40
      Height = 26
      Enabled = False
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
    end
    object ListBox2: TListBox
      Left = 23
      Top = 51
      Width = 49
      Height = 142
      Anchors = [akLeft, akTop, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -27
      Font.Name = 'Terminal'
      Font.Style = []
      ItemHeight = 24
      ParentFont = False
      TabOrder = 2
      OnClick = ListBox1Click
    end
  end
  object DeckWebServer: TIdHTTPServer
    Active = True
    Bindings = <>
    DefaultPort = 8181
    ParseParams = False
    OnCommandOther = DeckWebServerCommandOther
    OnCommandGet = DeckWebServerCommandGet
    Left = 24
    Top = 240
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 160
    Top = 240
  end
end

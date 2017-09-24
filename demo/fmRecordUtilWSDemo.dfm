object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 358
  ClientWidth = 635
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
    Width = 176
    Height = 358
    Align = alLeft
    Caption = 'Server'#39's Deck'
    TabOrder = 0
    ExplicitLeft = 177
    object CardsLeftLabel: TLabel
      Left = 77
      Top = 319
      Width = 49
      Height = 24
      Alignment = taCenter
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
    object Button1: TButton
      Left = 6
      Top = 319
      Width = 49
      Height = 25
      Caption = 'Shuffle'
      TabOrder = 0
      OnClick = Button1Click
    end
    object CardPanel: TPanel
      Left = 79
      Top = 29
      Width = 82
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
      TabOrder = 1
    end
    object DealButton: TButton
      Left = 79
      Top = 76
      Width = 41
      Height = 25
      Caption = 'Deal'
      Enabled = False
      TabOrder = 2
      OnClick = DealButtonClick
    end
    object ListBox1: TListBox
      Left = 8
      Top = 29
      Width = 65
      Height = 284
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -27
      Font.Name = 'Terminal'
      Font.Style = []
      ItemHeight = 24
      ParentFont = False
      TabOrder = 3
      OnClick = ListBox1Click
    end
    object ListBox2: TListBox
      Left = 77
      Top = 108
      Width = 49
      Height = 205
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -27
      Font.Name = 'Terminal'
      Font.Style = []
      ItemHeight = 24
      ParentFont = False
      TabOrder = 4
      OnClick = ListBox1Click
    end
    object SpinEdit1: TSpinEdit
      Left = 120
      Top = 76
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
      TabOrder = 5
      Value = 0
    end
  end
  object Memo1: TMemo
    Left = 192
    Top = 13
    Width = 217
    Height = 89
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object IdHTTP1: TIdHTTP
    AllowCookies = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = -1
    Request.ContentRangeStart = -1
    Request.ContentRangeInstanceLength = -1
    Request.Accept = 'text/html, */*'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    Request.Ranges.Units = 'bytes'
    Request.Ranges = <>
    HTTPOptions = [hoForceEncodeParams]
    Left = 584
    Top = 56
  end
  object IdHTTPServer1: TIdHTTPServer
    Active = True
    Bindings = <>
    DefaultPort = 8181
    OnCommandGet = IdHTTPServer1CommandGet
    Left = 584
    Top = 8
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 200
    Top = 168
  end
end

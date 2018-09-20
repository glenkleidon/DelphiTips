object Form1: TForm1
  Left = 192
  Top = 124
  Caption = 'Form1'
  ClientHeight = 525
  ClientWidth = 963
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 963
    Height = 488
    Align = alClient
    ExplicitLeft = 24
    ExplicitTop = 16
    ExplicitWidth = 905
    ExplicitHeight = 297
  end
  object Panel1: TPanel
    Left = 0
    Top = 488
    Width = 963
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 150
      Top = 9
      Width = 54
      Height = 13
      Caption = 'page 1 of 1'
    end
    object Button1: TButton
      Left = 16
      Top = 4
      Width = 75
      Height = 25
      Caption = '&Load Image'
      TabOrder = 0
      OnClick = Button1Click
    end
    object SpinEdit1: TSpinEdit
      Left = 97
      Top = 6
      Width = 48
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = SpinEdit1Change
    end
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 600
    Top = 48
  end
end

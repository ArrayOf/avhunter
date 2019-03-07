object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'AVHunter'
  ClientHeight = 411
  ClientWidth = 458
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabeledEditArquivoMAP: TLabeledEdit
    Left = 16
    Top = 32
    Width = 433
    Height = 21
    EditLabel.Width = 69
    EditLabel.Height = 13
    EditLabel.Caption = 'Arquivo MAP:'
    TabOrder = 0
    Text = 'C:\fontes_avhunter\example\Win32\Debug\example.map'
  end
  object Button1: TButton
    Left = 375
    Top = 59
    Width = 75
    Height = 25
    Caption = '&Processar'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 17
    Top = 160
    Width = 433
    Height = 225
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object Edit1: TEdit
    Left = 17
    Top = 61
    Width = 200
    Height = 21
    TabOrder = 3
    Text = '005EB5EF'
  end
end

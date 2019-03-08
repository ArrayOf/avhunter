object fMain: TfMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'AVHunter | Apontador de AccessViolation'
  ClientHeight = 341
  ClientWidth = 562
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 21
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 562
    Height = 306
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel1'
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    ExplicitWidth = 759
    ExplicitHeight = 296
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 562
      Height = 306
      Align = alClient
      BevelOuter = bvNone
      Caption = 'Panel3'
      ShowCaption = False
      TabOrder = 0
      ExplicitLeft = 209
      ExplicitWidth = 550
      ExplicitHeight = 296
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 562
        Height = 153
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Panel4'
        ShowCaption = False
        TabOrder = 0
        ExplicitWidth = 550
        object SpeedButton2: TSpeedButton
          Left = 222
          Top = 104
          Width = 82
          Height = 29
          Action = ActionProcessar
          ParentShowHint = False
          ShowHint = True
        end
        object SpeedButton1: TSpeedButton
          Left = 454
          Top = 32
          Width = 82
          Height = 29
          Action = ActionArquivo
          ParentShowHint = False
          ShowHint = True
        end
        object LabeledEdit1: TLabeledEdit
          Left = 6
          Top = 32
          Width = 442
          Height = 29
          EditLabel.Width = 95
          EditLabel.Height = 21
          EditLabel.Caption = 'Arquivo MAP:'
          TabOrder = 0
        end
        object LabeledEdit2: TLabeledEdit
          Left = 6
          Top = 104
          Width = 210
          Height = 29
          EditLabel.Width = 155
          EditLabel.Height = 21
          EditLabel.Caption = 'Endere'#231'o de mem'#243'ria:'
          TabOrder = 1
        end
      end
      object Panel5: TPanel
        Left = 0
        Top = 153
        Width = 562
        Height = 153
        Align = alClient
        BevelOuter = bvNone
        Caption = 'Panel5'
        Padding.Left = 15
        Padding.Top = 15
        Padding.Right = 15
        Padding.Bottom = 15
        ShowCaption = False
        TabOrder = 1
        ExplicitWidth = 550
        ExplicitHeight = 143
        object Memo2: TMemo
          Left = 15
          Top = 15
          Width = 532
          Height = 123
          Align = alClient
          Color = clBackground
          Font.Charset = ANSI_CHARSET
          Font.Color = clWhite
          Font.Height = -16
          Font.Name = 'Courier New'
          Font.Style = [fsBold]
          Lines.Strings = (
            'Memo2')
          ParentFont = False
          ReadOnly = True
          TabOrder = 0
          StyleElements = []
          ExplicitTop = 6
        end
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 306
    Width = 562
    Height = 35
    Align = alBottom
    BevelInner = bvLowered
    Caption = 'Panel2'
    ShowCaption = False
    TabOrder = 1
    ExplicitTop = 296
    ExplicitWidth = 759
    object LinkLabel1: TLinkLabel
      Left = 2
      Top = 2
      Width = 558
      Height = 31
      Align = alClient
      Caption = '<a href="http://arrayof.com.br">http://arrayof.com.br</a>'
      TabOrder = 0
      OnLinkClick = LinkLabel1LinkClick
      ExplicitWidth = 148
      ExplicitHeight = 25
    end
  end
  object ActionList1: TActionList
    Left = 40
    Top = 112
    object ActionArquivo: TAction
      Caption = '&Arquivo'
      ShortCut = 16463
      OnExecute = ActionArquivoExecute
    end
    object ActionProcessar: TAction
      Caption = '&Processar'
      ShortCut = 16504
      OnExecute = ActionProcessarExecute
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 40
    Top = 176
  end
end

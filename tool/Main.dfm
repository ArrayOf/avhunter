object fMain: TfMain
  Left = 0
  Top = 0
  Caption = 'AVHunter | Apontador de AccessViolation'
  ClientHeight = 326
  ClientWidth = 760
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
  object StatusBar1: TStatusBar
    Left = 0
    Top = 307
    Width = 760
    Height = 19
    Panels = <
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end>
    ExplicitLeft = 296
    ExplicitTop = 200
    ExplicitWidth = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 760
    Height = 307
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel1'
    ParentBackground = False
    ShowCaption = False
    TabOrder = 1
    ExplicitTop = -6
    ExplicitWidth = 785
    object SpeedButton1: TSpeedButton
      Left = 663
      Top = 32
      Width = 82
      Height = 29
      Action = ActionArquivo
    end
    object SpeedButton2: TSpeedButton
      Left = 431
      Top = 88
      Width = 82
      Height = 29
      Action = ActionProcessar
    end
    object Label1: TLabel
      Left = 215
      Top = 149
      Width = 72
      Height = 21
      Caption = 'Resultado:'
    end
    object Memo1: TMemo
      Left = 0
      Top = 0
      Width = 209
      Height = 307
      Align = alLeft
      Lines.Strings = (
        '## Como usar'
        ''
        'No Delphi ative a gera'#231#227'o '
        'do arquivo MAP')
      TabOrder = 0
    end
    object LabeledEdit1: TLabeledEdit
      Left = 215
      Top = 32
      Width = 442
      Height = 29
      EditLabel.Width = 95
      EditLabel.Height = 21
      EditLabel.Caption = 'Arquivo MAP:'
      TabOrder = 1
    end
    object LabeledEdit2: TLabeledEdit
      Left = 215
      Top = 88
      Width = 210
      Height = 29
      EditLabel.Width = 155
      EditLabel.Height = 21
      EditLabel.Caption = 'Endere'#231'o de mem'#243'ria:'
      TabOrder = 2
    end
    object Memo2: TMemo
      Left = 215
      Top = 176
      Width = 530
      Height = 125
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Courier New'
      Font.Style = []
      Lines.Strings = (
        'Memo2')
      ParentFont = False
      TabOrder = 3
    end
  end
  object ActionList1: TActionList
    Left = 72
    Top = 120
    object ActionArquivo: TAction
      Caption = '&Arquivo'
      ShortCut = 16463
      OnExecute = ActionArquivoExecute
    end
    object ActionProcessar: TAction
      Caption = 'Processar'
      OnExecute = ActionProcessarExecute
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 72
    Top = 208
  end
end

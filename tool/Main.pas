unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  avhunter_lib,
  System.Actions,
  Vcl.ActnList,
  Vcl.ComCtrls,
  Vcl.Buttons;

type
  TfMain = class(TForm)
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    Memo1: TMemo;
    ActionList1: TActionList;
    LabeledEdit1: TLabeledEdit;
    SpeedButton1: TSpeedButton;
    LabeledEdit2: TLabeledEdit;
    SpeedButton2: TSpeedButton;
    Memo2: TMemo;
    Label1: TLabel;
    ActionArquivo: TAction;
    OpenDialog1: TOpenDialog;
    ActionProcessar: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ActionArquivoExecute(Sender: TObject);
    procedure ActionProcessarExecute(Sender: TObject);
  private
    { Private declarations }
    FAVHunter: TAVHunter;
    function GetFileName: string;
    function GetAddress: string;
    property FileName: string read GetFileName;
    property Address: string read GetAddress;
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.ActionArquivoExecute(Sender: TObject);
begin
  if Self.OpenDialog1.Execute(Self.Handle) then
  begin
    Self.LabeledEdit1.Text := Self.OpenDialog1.FileName;
  end;
end;

procedure TfMain.ActionProcessarExecute(Sender: TObject);
var
  rLocation: TAVHunterInfo;
begin
  Self.ActionProcessar.Enabled := False;
  Screen.Cursor                := crHourGlass;

  try
    Self.FAVHunter.LoadMAPFile(Self.FileName);
    Self.FAVHunter.GetLocation(Self.Address, rLocation);

    Self.Memo2.Clear;
    with Self.Memo2.Lines do
    begin
      Add(Format('Arquivo...: %s', [rLocation.FileName]));
      Add(Format('Unit......: %s', [rLocation.&Unit]));
      Add(Format('Method....: %s', [rLocation.Method]));
      Add(Format('Line......: %d', [rLocation.Line]));
    end;

  finally
    Self.ActionProcessar.Enabled := True;
    Screen.Cursor                := crDefault;
  end;
end;

procedure TfMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.FAVHunter.Free;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  Self.FAVHunter              := TAVHunter.Create;
end;

function TfMain.GetAddress: string;
begin
  Result := Self.LabeledEdit2.Text;
end;

function TfMain.GetFileName: string;
begin
  Result := Self.LabeledEdit1.Text;
end;

end.

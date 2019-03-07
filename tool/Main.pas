unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  avhunter_lib;

type
  TForm2 = class(TForm)
    LabeledEditArquivoMAP: TLabeledEdit;
    Button1: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FAVHunter: TAVHunter;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
var
  rLocation: TAVHunterInfo;
begin
  Self.Button1.Enabled := False;
  Screen.Cursor        := crHourGlass;

  try
    Self.FAVHunter.LoadMAPFile(Self.LabeledEditArquivoMAP.Text);
    Self.FAVHunter.GetLocation(Self.Edit1.Text, rLocation);

    Self.Memo1.Clear;
    with Self.Memo1.Lines do
    begin
      Add(Format('Arquivo: %s', [rLocation.FileName]));
      Add(Format('Unit: %s', [rLocation.&Unit]));
      Add(Format('Method: %s', [rLocation.Method]));
      Add(Format('Line: %d', [rLocation.Line]));
    end;

  finally
    Self.Button1.Enabled := True;
    Screen.Cursor        := crDefault;
  end;

end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.FAVHunter.Free;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  Self.FAVHunter              := TAVHunter.Create;
end;

end.

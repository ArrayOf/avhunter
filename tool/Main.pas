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
  Self.FAVHunter.LoadMAPFile(Self.LabeledEditArquivoMAP.Text);
  Self.FAVHunter.GetLocation(Self.Edit1.Text, rLocation);

  Self.Memo1.Clear;
  Self.Memo1.Lines.Add(Format('Unit: %s', [rLocation.&Unit]));
  Self.Memo1.Lines.Add(Format('Method: %s', [rLocation.Method]));
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

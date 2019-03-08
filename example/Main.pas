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
  Vcl.StdCtrls;

type
  TForm3 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
var
  slTeste: TStringList;
begin
  slTeste := TStringList.Create;
  slTeste.Add('OLÁ MUNDO!');
  slTeste.Add('OLÁ MUNDO!');
  FreeAndNil(slTeste);

  slTeste.Add('olá mundo!');
  slTeste.Add('olá mundo!');
  slTeste.Add('olá mundo!');
end;

end.

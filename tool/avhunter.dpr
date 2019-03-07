program avhunter;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form2},
  avhunter_lib in '..\lib\avhunter_lib.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

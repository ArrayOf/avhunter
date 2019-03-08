program avhunter;

uses
  Vcl.Forms,
  Main in 'Main.pas' {fMain},
  avhunter_lib in '..\lib\avhunter_lib.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'AVHunter';
  TStyleManager.TrySetStyle('Metropolis UI Dark');
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.

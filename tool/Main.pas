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
  System.Actions,
  Vcl.ActnList,
  Vcl.ComCtrls,
  Vcl.Buttons,

  avhunter_lib;

type
  TfMain = class(TForm)
    Panel1: TPanel;
    ActionList1: TActionList;
    ActionArquivo: TAction;
    OpenDialog1: TOpenDialog;
    ActionProcessar: TAction;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    SpeedButton2: TSpeedButton;
    SpeedButton1: TSpeedButton;
    Panel5: TPanel;
    Memo2: TMemo;
    LinkLabel1: TLinkLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ActionArquivoExecute(Sender: TObject);
    procedure ActionProcessarExecute(Sender: TObject);
    procedure LinkLabel1LinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
  private
    { Private declarations }
    FAVHunter: TAVHunter;
    function GetFileName: string;
    function GetAddress: string;
    procedure LoadConfig;
    procedure SaveConfig;
    procedure SetAddress(const Value: string);
    procedure SetFileName(const Value: string);
    function GetINIFileName: string;
    property FileName: string read GetFileName write SetFileName;
    property Address: string read GetAddress write SetAddress;
    property INIFileName: string read GetINIFileName;
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

uses
  System.IniFiles,
  System.Diagnostics,
  ShellApi;

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
  rCronos  : TStopwatch;
begin
  Self.ActionProcessar.Enabled := False;
  Screen.Cursor                := crHourGlass;
  rCronos                      := TStopwatch.StartNew;

  try
    Self.FAVHunter.LoadMAPFile(Self.FileName);
    Self.FAVHunter.GetLocation(Self.Address, rLocation);
    rCronos.Stop;

    Self.Memo2.Clear;
    with Self.Memo2.Lines do
    begin
      Add(Format('> File.............: %s', [rLocation.FileName]));
      Add(Format('> Unit.............: %s', [rLocation.&Unit]));
      Add(Format('> Method...........: %s', [rLocation.Method]));
      Add(Format('> Line.............: %d', [rLocation.Line]));
      Add(Format('> Processing time..: %d Milliseconds', [rCronos.ElapsedMilliseconds]));
    end;

  finally
    Self.ActionProcessar.Enabled := True;
    Screen.Cursor                := crDefault;
  end;
end;

procedure TfMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.SaveConfig;
  Self.FAVHunter.Free;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  Self.LoadConfig;
  Self.Memo2.Clear;
  Self.FAVHunter := TAVHunter.Create;
end;

function TfMain.GetAddress: string;
begin
  Result := Self.LabeledEdit2.Text;
end;

function TfMain.GetFileName: string;
begin
  Result := Self.LabeledEdit1.Text;
end;

function TfMain.GetINIFileName: string;
begin
  Result := Format('%s\avhunter.ini', [ExtractFilePath(ParamStr(0))]);
end;

procedure TfMain.LinkLabel1LinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
begin
  ShellExecute(0, nil, PChar(Link), nil, nil, 1);
end;

procedure TfMain.LoadConfig;
var
  _ini: TIniFile;
begin
  _ini := TIniFile.Create(Self.INIFileName);
  try
    Self.FileName := _ini.ReadString('CONFIG', 'map_file', EmptyStr);
    Self.Address  := _ini.ReadString('CONFIG', 'address', EmptyStr);
  finally
    _ini.Free;
  end;
end;

procedure TfMain.SaveConfig;
var
  _ini    : TIniFile;
  sFile   : string;
  sAddress: string;
begin
  _ini := TIniFile.Create(Self.INIFileName);
  try
    sFile    := Self.FileName;
    sAddress := Self.Address;

    _ini.WriteString('CONFIG', 'map_file', sFile);
    _ini.WriteString('CONFIG', 'address', sAddress);
  finally
    _ini.Free;
  end;
end;

procedure TfMain.SetAddress(const Value: string);
begin
  Self.LabeledEdit2.Text := Value;
end;

procedure TfMain.SetFileName(const Value: string);
begin
  Self.LabeledEdit1.Text := Value;
end;

end.

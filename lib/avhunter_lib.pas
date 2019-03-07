unit avhunter_lib;

interface

type

  TAVHunterInfo = record
    &Unit: string;
    Line: Integer;
    Method: string;
  end;

  TAVHunter = class(TObject)
  private
    FInternalList: TObject;
  public
    procedure LoadMAPFile(const AFileName: string);
    function GetLocation(const AAddress: string; out ALocation: TAVHunterInfo): Boolean;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils, System.Generics.Collections, System.RegularExpressions,
  Winapi.Windows;

type

  TRow = record
    Endereco: Integer;
    Linha: Integer;
  end;

  TMethod = record
    Address: Integer;
    Name: string;
    Rows: array of TRow;
  end;

  PUnit = ^TUnit;

  TUnit = record
    Start: Integer;
    &End: Integer;
    &Unit: string;
    Path: string;
    Methods: array of TMethod;
  end;

  TListAVHunter = class(TList<PUnit>)
  public
    destructor Destroy; override;
  end;

  { TAVHunter }

destructor TAVHunter.Destroy;
begin
  if Assigned(Self.FInternalList) then
  begin
    FreeAndNil(Self.FInternalList);
  end;
  inherited;
end;

function TAVHunter.GetLocation(const AAddress: string; out ALocation: TAVHunterInfo): Boolean;
var
  iAddress     : Integer;
  oListAVHunter: TListAVHunter;
  pItem        : PUnit;
begin
  iAddress := StrToInt('$' + AAddress) - $401000;
  if (iAddress <= 0) then
  begin
    Exit(False);
  end;

  oListAVHunter := TListAVHunter(Self.FInternalList);

  for pItem in oListAVHunter do
  begin
    if (iAddress >= pItem.Start) and (iAddress <= pItem.&End) then
    begin
      ALocation.&Unit := pItem.&Unit;
      Exit(True);
    end;
  end;

  Result := False;
end;

procedure TAVHunter.LoadMAPFile(const AFileName: string);

const
  REGEX = '^[ ]....:(.{8})[ ](.{8}).*?M=([^ ]+)';
var
  _RegEx       : TRegEx;
  _Matche      : TMatch;
  MAPHandler   : Text;
  pItem        : PUnit;
  oListAVHunter: TListAVHunter;

  procedure _ValidateFile;
  begin
    if not(FileExists(AFileName)) then
    begin
      raise Exception.Create('O arquivo MAP não foi localizado');
    end;
  end;

  procedure _Discard(const AUntil: string);
  var
    sLine: string;
  begin
    repeat
      Readln(MAPHandler, sLine);
    until (sLine = AUntil) or Eof(MAPHandler);
    Readln(MAPHandler);
  end;

  procedure _CreateNewList;
  begin
    if (Assigned(Self.FInternalList)) then
    begin
      Self.FInternalList.Free;
    end;

    oListAVHunter      := TListAVHunter.Create;
    Self.FInternalList := oListAVHunter;
  end;

  procedure _MAPUnits;
  var
    sLine: string;
  begin
    while not(Eof(MAPHandler)) do
    begin
      Readln(MAPHandler, sLine);
      if (sLine = EmptyStr) then
      begin
        Exit;
      end;

      _Matche := _RegEx.Match(sLine);

      New(pItem);
      pItem.Start := StrToInt('$' + _Matche.Groups[1].Value);
      pItem.&End  := pItem.Start + StrToInt('$' + _Matche.Groups[2].Value) - 1;
      pItem.&Unit := _Matche.Groups[3].Value;

      oListAVHunter.Add(pItem);
    end;
  end;

  procedure _MAPMethods;
  var
    sLine: string;
  begin
    while not(Eof(MAPHandler)) do
    begin
      Readln(MAPHandler, sLine);
      if (sLine = EmptyStr) then
      begin
        Exit;
      end;

      OutputDebugString(PChar(sLine));
    end;
  end;

begin
  _CreateNewList;
  _ValidateFile;

  try
    _RegEx := TRegEx.Create(REGEX, [roNotEmpty, roCompiled]);

    Assign(MAPHandler, AFileName);
    Reset(MAPHandler);

    _Discard('Detailed map of segments');
    _MAPUnits;

    _Discard('  Address             Publics by Value');
    _MAPMethods;

  finally
    Close(MAPHandler);
  end;

end;

{ TListAVHunter }

destructor TListAVHunter.Destroy;
var
  pItem: PUnit;
begin
  for pItem in Self do
  begin
    Dispose(pItem);
  end;

  inherited;
end;

end.

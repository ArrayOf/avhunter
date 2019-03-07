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
    FGenesys: Pointer;
  private
    procedure Clear;
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

  PRow = ^TRow;

  TRow = record
    Address: Integer;
    Row: Integer;
  end;

  PMethod = ^TMethod;

  TMethod = record
    Address: Integer;
    Name: string;
    Next: PMethod;
    RowsGenesys: PRow;
  end;

  PUnit = ^TUnit;

  TUnit = record
    Start: Integer;
    &End: Integer;
    &Unit: string;
    Path: string;
    Next: PUnit;
    MethodGenesys: PMethod;
  end;

  { TAVHunter }

procedure TAVHunter.Clear;
var
  pUnitItem      : PUnit;
  pUnitItemNext  : PUnit;
  pMethodItem    : PMethod;
  pMethodItemNext: PMethod;
begin
  pUnitItem       := nil;
  pUnitItemNext   := nil;
  pMethodItem     := nil;
  pMethodItemNext := nil;

  pUnitItem     := Self.FGenesys;
  Self.FGenesys := nil;

  while Assigned(pUnitItem) do
  begin

    // pMethodItem := pUnitItem.MethodGenesys;
    // while Assigned(pMethodItem) do
    // begin
    // pMethodItemNext := pMethodItem.Next;
    // Dispose(pMethodItem);
    // pMethodItem := pMethodItemNext;
    // end;

    pUnitItemNext := pUnitItem.Next;
    Dispose(pUnitItem);
    pUnitItem := pUnitItemNext;
  end;
end;

destructor TAVHunter.Destroy;
begin
  Self.Clear;
  inherited;
end;

function TAVHunter.GetLocation(const AAddress: string; out ALocation: TAVHunterInfo): Boolean;
var
  iAddress   : Integer;
  pUnitItem  : PUnit;
  pMethodItem: PMethod;
begin
  iAddress := StrToInt('$' + AAddress) - $401000;
  if (iAddress <= 0) then
  begin
    Exit(False);
  end;

  pUnitItem := Self.FGenesys;

  repeat
    if (iAddress >= pUnitItem.Start) and (iAddress <= pUnitItem.&End) then
    begin
      ALocation.&Unit := pUnitItem.&Unit;

      pMethodItem := pUnitItem.MethodGenesys;
      repeat
        if (iAddress <= pMethodItem.Address) or not(Assigned(pMethodItem.Next)) then
        begin
          ALocation.Method := pMethodItem.Name;
          Exit(True);
        end;

        pMethodItem := pMethodItem.Next;
      until not(Assigned(pMethodItem));

      Exit(False);
    end;

    pUnitItem := pUnitItem.Next;
  until not(Assigned(pUnitItem));

  Result := False;
end;

procedure TAVHunter.LoadMAPFile(const AFileName: string);
var
  MAPHandler: Text;

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

  procedure _MAPUnits;
  const
    REGEX = '^[ ]....:(.{8})[ ](.{8}).*?M=([^ ]+)';
  var
    _RegEx   : TRegEx;
    _Matche  : TMatch;
    sLine    : string;
    pItem    : PUnit;
    pPrevious: PUnit;
  begin
    _RegEx := TRegEx.Create(REGEX, [roNotEmpty, roCompiled]);

    New(pPrevious);
    pPrevious.Start := -1;
    pPrevious.&End  := -1;
    pPrevious.&Unit := '** PONTEIRO GENESYS **';
    pPrevious.Next  := nil;
    Self.FGenesys   := pPrevious;

    while not(Eof(MAPHandler)) do
    begin
      Readln(MAPHandler, sLine);
      if (sLine = EmptyStr) then
      begin
        Exit;
      end;

      _Matche := _RegEx.Match(sLine);

      New(pItem);
      pItem.Start         := StrToInt('$' + _Matche.Groups[1].Value);
      pItem.&End          := pItem.Start + StrToInt('$' + _Matche.Groups[2].Value) - 1;
      pItem.&Unit         := _Matche.Groups[3].Value;
      pItem.MethodGenesys := nil;
      pItem.Next          := nil;

      pPrevious.Next := pItem;
      pPrevious      := pItem;
    end;
  end;

  procedure _MAPMethods;
  const
    REGEX = '^[ ].{4}:(.{8}).{7}(.*)$';
  var
    _RegEx   : TRegEx;
    _Matche  : TMatch;
    sLine    : string;
    pItem    : PMethod;
    pPrevious: PMethod;
    pUnitItem: PUnit;
  begin
    _RegEx := TRegEx.Create(REGEX, [roNotEmpty, roCompiled]);

    while not(Eof(MAPHandler)) do
    begin
      Readln(MAPHandler, sLine);
      if (sLine = EmptyStr) then
      begin
        Exit;
      end;

      _Matche := _RegEx.Match(sLine);

      New(pItem);
      pItem.Address := StrToInt('$' + _Matche.Groups[1].Value);
      pItem.Name    := _Matche.Groups[2].Value;
      pItem.Next    := nil;

      pUnitItem := Self.FGenesys;
      repeat
        if (pItem.Address >= pUnitItem.Start) and (pItem.Address <= pUnitItem.&End) then
        begin
          if not Assigned(pUnitItem.MethodGenesys) then
          begin
            New(pPrevious);

            pPrevious.Address     := -1;
            pPrevious.Name        := '** PONTEIRO GENESYS **';
            pPrevious.Next        := nil;
            pPrevious.RowsGenesys := nil;

            pUnitItem.MethodGenesys := pPrevious;
          end;

          pPrevious := pUnitItem.MethodGenesys;
          while Assigned(pPrevious.Next) do
          begin
            pPrevious := pPrevious.Next;
          end;
          pPrevious.Next := pItem;

          Break;
        end;

        pUnitItem := pUnitItem.Next;
      until not Assigned(pUnitItem);

      // OutputDebugString(PChar(sLine));
    end;
  end;

begin
  Self.Clear;
  _ValidateFile;

  try

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

end.

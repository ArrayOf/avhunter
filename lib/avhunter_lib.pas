unit avhunter_lib;

interface

uses
  System.Generics.Collections;

type

  TAVHunterInfo = record
    &Unit: string;
    FileName: string;
    Line: Integer;
    Method: string;
  end;

  TAVHunter = class(TObject)
  private
    FGenesys: Pointer;
    FTrash  : TList<Pointer>;
  private
    procedure Clear;
  public
    procedure LoadMAPFile(const AFileName: string);
    function GetLocation(const AAddress: string; out ALocation: TAVHunterInfo): Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  System.RegularExpressions,
  Winapi.Windows;

type

  PRow = ^TRow;

  TRow = record
    Address: Integer;
    Row: Integer;
    Next: PRow;
  end;

  PMethod = ^TMethod;

  TMethod = record
    Address: Integer;
    Name: string[255];
    Next: PMethod;
    RowGenesys: PRow;
  end;

  PUnit = ^TUnit;

  TUnit = record
    Start: Integer;
    &End: Integer;
    &Unit: string[255];
    Path: string[255];
    Next: PUnit;
    MethodGenesys: PMethod;
  end;

  { TAVHunter }

procedure TAVHunter.Clear;
var
  pItem: Pointer;
begin
  for pItem in Self.FTrash do
  begin
    Dispose(pItem);
  end;

  Self.FTrash.Clear;
end;

constructor TAVHunter.Create;
begin
  inherited;
  Self.FTrash := TList<Pointer>.Create;
end;

destructor TAVHunter.Destroy;
begin
  Self.Clear;
  Self.FTrash.Free;
  inherited;
end;

function TAVHunter.GetLocation(const AAddress: string; out ALocation: TAVHunterInfo): Boolean;
var
  iAddress   : Integer;
  pUnitItem  : PUnit;
  pMethodItem: PMethod;
  pRowItem   : PRow;
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
      ALocation.&Unit    := pUnitItem.&Unit;
      ALocation.FileName := pUnitItem.Path;

      pMethodItem := pUnitItem.MethodGenesys;
      repeat
        if (iAddress <= pMethodItem.Address) or not(Assigned(pMethodItem.Next)) then
        begin
          ALocation.Method := pMethodItem.Name;

          pRowItem := pMethodItem.RowGenesys;
          repeat
            if (iAddress <= pRowItem.Address) or not(Assigned(pRowItem.Next)) then
            begin
              ALocation.Line := pRowItem.Row;
              Exit(True);
            end;

            pRowItem := pRowItem.Next;
          until not(Assigned(pRowItem));

          Exit(False);
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
    REGEX = '^[ ].{4}:(.{8})[ ](.{8})[ ].*?M=([^ ]+)';
  var
    _RegEx   : TRegEx;
    _Match   : TMatch;
    sLine    : string;
    pItem    : PUnit;
    pPrevious: PUnit;
  begin
    _RegEx := TRegEx.Create(REGEX, [roNotEmpty, roCompiled]);

    New(pPrevious);
    Self.FTrash.Add(pPrevious);

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

      _Match := _RegEx.Match(sLine);
      if not(_Match.Success) then
      begin
        Continue;
      end;

      New(pItem);
      Self.FTrash.Add(pItem);

      pItem.Start         := StrToInt('$' + _Match.Groups[1].Value);
      pItem.&End          := pItem.Start + StrToInt('$' + _Match.Groups[2].Value) - 1;
      pItem.&Unit         := _Match.Groups[3].Value;
      pItem.MethodGenesys := nil;
      pItem.Next          := nil;

      // OutputDebugString(PChar(_Match.Groups[3].Value));

      pPrevious.Next := pItem;
      pPrevious      := pItem;
    end;
  end;

  procedure _MAPMethods;
  const
    REGEX = '^[ ].{4}:(.{8}).{7}(.*)$';
  var
    _RegEx        : TRegEx;
    _Match        : TMatch;
    sLine         : string;
    pMethodItem   : PMethod;
    pMethodGenesys: PMethod;
    pPrevious     : PMethod;
    pUnitItem     : PUnit;
  begin
    _RegEx    := TRegEx.Create(REGEX, [roNotEmpty, roCompiled]);
    pUnitItem := Self.FGenesys;

    while not(Eof(MAPHandler)) do
    begin
      Readln(MAPHandler, sLine);
      if (sLine = EmptyStr) then
      begin
        Exit;
      end;

      _Match := _RegEx.Match(sLine);

      New(pMethodItem);
      Self.FTrash.Add(pMethodItem);

      pMethodItem.Address    := StrToInt('$' + _Match.Groups[1].Value);
      pMethodItem.Name       := _Match.Groups[2].Value;
      pMethodItem.Next       := nil;
      pMethodItem.RowGenesys := nil;

      while Assigned(pUnitItem) do
      begin
        if (pMethodItem.Address >= pUnitItem.Start) and (pMethodItem.Address <= pUnitItem.&End) then
        begin
          if not Assigned(pUnitItem.MethodGenesys) then
          begin
            New(pMethodGenesys);
            Self.FTrash.Add(pMethodGenesys);

            pMethodGenesys.Address    := -1;
            pMethodGenesys.Name       := '** PONTEIRO GENESYS **';
            pMethodGenesys.Next       := nil;
            pMethodGenesys.RowGenesys := nil;

            pUnitItem.MethodGenesys := pMethodGenesys;
          end;

          pPrevious := pUnitItem.MethodGenesys;
          while Assigned(pPrevious.Next) do
          begin
            pPrevious := pPrevious.Next;
          end;
          pPrevious.Next := pMethodItem;

          Break;
        end;

        pUnitItem := pUnitItem.Next;
      end;
    end;
  end;
  procedure _MAPRows;
  const
    REGEX    = '^Line numbers for ([^(]+)\(([^)]+)\).*$';
    RE_LINES = '(\d{1,})[ ]\d{4}:(.{8})';
  var
    _RegEx       : TRegEx;
    _RegExLines  : TRegEx;
    _Match       : TMatch;
    _MatchesLines: TMatchCollection;
    _MatchLine   : TMatch;
    sLine        : string;
    pUnitItem    : PUnit;
    pMethodItem  : PMethod;
    pRowItem     : PRow;
    pRowGenesys  : PRow;
    pRowPrevious : PRow;
    sUnit        : string;
    sFileName    : string;
  begin
    _RegEx      := TRegEx.Create(REGEX, [roNotEmpty, roCompiled]);
    _RegExLines := TRegEx.Create(RE_LINES, [roNotEmpty, roCompiled]);

    while not(Eof(MAPHandler)) do
    begin
      Readln(MAPHandler, sLine);
      if (sLine = EmptyStr) then
      begin
        Continue;
      end;

      _Match := _RegEx.Match(sLine);
      if not(_Match.Success) then
      begin
        Continue;
      end;

      sUnit     := _Match.Groups[1].Value;
      sFileName := _Match.Groups[2].Value;

      pUnitItem := Self.FGenesys;
      repeat
        if (pUnitItem.&Unit = sUnit) then
        begin
          pUnitItem.Path := sFileName;

          Readln(MAPHandler);

          while not(Eof(MAPHandler)) do
          begin
            Readln(MAPHandler, sLine);
            if (sLine = EmptyStr) then
            begin
              Break;
            end;

            _MatchesLines := _RegExLines.Matches(sLine);
            for _MatchLine in _MatchesLines do
            begin
              New(pRowItem);
              Self.FTrash.Add(pRowItem);

              pRowItem.Address := StrToInt('$' + _MatchLine.Groups[2].Value);
              pRowItem.Row     := StrToInt(_MatchLine.Groups[1].Value);
              pRowItem.Next    := nil;

              pMethodItem := pUnitItem.MethodGenesys;

              if not Assigned(pMethodItem) then
              begin
                Continue;
              end;

              repeat
                if (pRowItem.Address <= pMethodItem.Address) or not(Assigned(pMethodItem.Next)) then
                begin
                  if not(Assigned(pMethodItem.RowGenesys)) then
                  begin
                    New(pRowGenesys);
                    Self.FTrash.Add(pRowGenesys);

                    pRowGenesys.Address := -1;
                    pRowGenesys.Row     := -1;
                    pRowGenesys.Next    := nil;

                    pMethodItem.RowGenesys := pRowGenesys;
                    pRowPrevious           := pRowGenesys;
                  end;

                  pRowPrevious.Next := pRowItem;
                  pRowPrevious      := pRowItem;

                  Break;
                end;

                pMethodItem := pMethodItem.Next;
              until not(Assigned(pMethodItem));
            end;

          end;

          Break;
        end;

        pUnitItem := pUnitItem.Next;
      until not(Assigned(pUnitItem));
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

    _MAPRows;

  finally
    Close(MAPHandler);
  end;

end;

end.

unit BCEditor.Search;

interface

uses
  System.Classes;

type
  TBCEditorSearchCustom = class
  protected
    function GetLength(aIndex: Integer): Integer; virtual; abstract;
    function GetPattern: string; virtual; abstract;
    function GetResult(aIndex: Integer): Integer; virtual; abstract;
    function GetResultCount: Integer; virtual; abstract;
    procedure SetPattern(const Value: string); virtual; abstract;
  public
    function FindAll(const NewText: string): Integer; virtual; abstract;
    function Replace(const aOccurrence, aReplacement: string): string; virtual; abstract;
    property Lengths[aIndex: Integer]: Integer read GetLength;
    property Pattern: string read GetPattern write SetPattern;
    property ResultCount: Integer read GetResultCount;
    property Results[aIndex: Integer]: Integer read GetResult;
  end;

  { TBCEditorNormalSearch }

  TBCEditorNormalSearch = class(TBCEditorSearchCustom)
  strict private
    FCaseSensitive: Boolean;
    FCount: Integer;
    FLookAt: Integer;
    FOrigin: PChar;
    FPattern, FCasedPattern: string;
    FPatternLength, FPatternLengthSuccessor: Integer;
    FResults: TList;
    FRun: PChar;
    FShift: array [Char] of Integer;
    FShiftInitialized: Boolean;
    FTextLength: Integer;
    FTextToSearch: string;
    FTheEnd: PChar;
    FWholeWordsOnly: Boolean;
    function GetFinished: Boolean;
    procedure InitShiftTable;
    procedure SetCaseSensitive(const Value: Boolean);
  protected
    function GetLength(Index: Integer): Integer; override;
    function GetPattern: string; override;
    function GetResult(Index: Integer): Integer; override;
    function GetResultCount: Integer; override;
    function TestWholeWord: Boolean;
    procedure SetPattern(const Value: string); override;
  public
    constructor Create;
    destructor Destroy; override;

    function FindAll(const NewText: string): Integer; override;
    function FindFirst(const NewText: string): Integer;
    function Next: Integer;
    function Replace(const aOccurrence, aReplacement: string): string; override;
    procedure FixResults(First, Delta: Integer);
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property Count: Integer read FCount write FCount;
    property Finished: Boolean read GetFinished;
    property Pattern read FCasedPattern;
    property WholeWordsOnly: Boolean read FWholeWordsOnly write FWholeWordsOnly;
  end;

implementation

uses
  System.SysUtils, System.Character, BCEditor.Consts;

constructor TBCEditorNormalSearch.Create;
begin
  inherited;
  FResults := TList.Create;
end;

function TBCEditorNormalSearch.GetFinished: Boolean;
begin
  Result := (FRun >= FTheEnd) or (FPatternLength >= FTextLength);
end;

function TBCEditorNormalSearch.GetResult(Index: Integer): Integer;
begin
  Result := 0;
  if (Index >= 0) and (Index < FResults.Count) then
    Result := Integer(FResults[Index]);
end;

function TBCEditorNormalSearch.GetResultCount: Integer;
begin
  Result := FResults.Count;
end;

procedure TBCEditorNormalSearch.FixResults(First, Delta: Integer);
var
  i: Integer;
begin
  if (Delta <> 0) and (FResults.Count > 0) then
  begin
    i := Pred(FResults.Count);
    while i >= 0 do
    begin
      if Integer(FResults[i]) <= First then
        Break;
      FResults[i] := pointer(Integer(FResults[i]) - Delta);
      Dec(i);
    end;
  end;
end;

procedure TBCEditorNormalSearch.InitShiftTable;
var
  LChar: Char;
  i: Integer;
begin
  FPatternLength := Length(FPattern);
  if FPatternLength = 0 then
    raise Exception.Create('Pattern is empty');
  FPatternLengthSuccessor := FPatternLength + 1;
  FLookAt := 1;
  for LChar := low(Char) to high(Char) do
    FShift[LChar] := FPatternLengthSuccessor;
  for i := 1 to FPatternLength do
    FShift[FPattern[i]] := FPatternLengthSuccessor - i;
  while FLookAt < FPatternLength do
  begin
    if FPattern[FPatternLength] = FPattern[FPatternLength - FLookAt] then
      break;
    Inc(FLookAt);
  end;
  FShiftInitialized := True;
end;

function TBCEditorNormalSearch.TestWholeWord: Boolean;
var
  Test: PChar;

  function IsWordBreakChar(AChar: Char): Boolean;
  begin
    if (AChar < BCEDITOR_EXCLAMATION_MARK) or AChar.IsWhiteSpace then
      Result := True
    else
    if AChar = BCEDITOR_LOW_LINE then
      Result := False
    else
      Result := not AChar.IsLetterOrDigit;
  end;

begin
  Test := FRun - FPatternLength;

  Result := ((Test < FOrigin) or IsWordBreakChar(Test[0])) and ((FRun >= FTheEnd) or IsWordBreakChar(FRun[1]));
end;

function TBCEditorNormalSearch.Next: Integer;
var
  i: Integer;
  J: PChar;
begin
  Result := 0;
  Inc(FRun, FPatternLength);
  while FRun < FTheEnd do
  begin
    if FPattern[FPatternLength] <> FRun^ then
      Inc(FRun, FShift[(FRun + 1)^])
    else
    begin
      J := FRun - FPatternLength + 1;
      i := 1;
      while FPattern[i] = J^ do
      begin
        if i = FPatternLength then
        begin
          if FWholeWordsOnly then
            if not TestWholeWord then
              break;
          Inc(FCount);
          Result := FRun - FOrigin - FPatternLength + 2;
          Exit;
        end;
        Inc(i);
        Inc(J);
      end;
      Inc(FRun, FLookAt);
      if FRun >= FTheEnd then
        break;
      Inc(FRun, FShift[FRun^] - 1);
    end;
  end;
end;

destructor TBCEditorNormalSearch.Destroy;
begin
  FResults.Free;
  inherited Destroy;
end;

procedure TBCEditorNormalSearch.SetPattern(const Value: string);
begin
  if FPattern <> Value then
  begin
    FCasedPattern := Value;
    if CaseSensitive then
      FPattern := FCasedPattern
    else
      FPattern := LowerCase(FCasedPattern);
    FShiftInitialized := False;
  end;
  FCount := 0;
end;

procedure TBCEditorNormalSearch.SetCaseSensitive(const Value: Boolean);
begin
  if FCaseSensitive <> Value then
  begin
    FCaseSensitive := Value;
    if FCaseSensitive then
      FPattern := FCasedPattern
    else
      FPattern := LowerCase(FCasedPattern);
    FShiftInitialized := False;
  end;
end;

function TBCEditorNormalSearch.FindAll(const NewText: string): Integer;
var
  Found: Integer;
begin
  FResults.Count := 0;
  Found := FindFirst(NewText);
  while Found > 0 do
  begin
    FResults.Add(pointer(Found));
    Found := Next;
  end;
  Result := FResults.Count;
end;

function TBCEditorNormalSearch.Replace(const aOccurrence, aReplacement: string): string;
begin
  Result := aReplacement;
end;

function TBCEditorNormalSearch.FindFirst(const NewText: string): Integer;
begin
  if not FShiftInitialized then
    InitShiftTable;
  Result := 0;
  FTextLength := Length(NewText);
  if FTextLength >= FPatternLength then
  begin
    if CaseSensitive then
      FTextToSearch := NewText
    else
      FTextToSearch := LowerCase(NewText);
    FOrigin := PChar(FTextToSearch);
    FTheEnd := FOrigin + FTextLength;
    FRun := FOrigin - 1;
    Result := Next;
  end;
end;

function TBCEditorNormalSearch.GetLength(Index: Integer): Integer;
begin
  Result := FPatternLength;
end;

function TBCEditorNormalSearch.GetPattern: string;
begin
  Result := FCasedPattern;
end;

end.


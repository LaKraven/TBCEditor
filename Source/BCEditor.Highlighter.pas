unit BCEditor.Highlighter;

interface

uses
  System.Classes, System.SysUtils, Vcl.Controls, Vcl.Graphics, BCEditor.Highlighter.Rules, BCEditor.Highlighter.Token,
  BCEditor.Types, BCEditor.Highlighter.Attributes, BCEditor.Highlighter.Info, BCEditor.Editor.SkipRegions,
  BCEditor.Highlighter.Colors, BCEditor.Editor.CodeFolding.FoldRegions;

type
  TBCEditorHighlighter = class(TObject)
  strict private
    FAttributes: TStringList;
    FBeginningOfLine: Boolean;
    FCodeFoldingRegions: TBCEditorCodeFoldingRegions;
    FColors: TBCEditorHighlighterColors;
    FCompletionProposalSkipRegions: TBCEditorSkipRegions;
    FCurrentLine: PChar;
    FCurrentRange: TBCEditorRange;
    FCurrentToken: TBCEditorToken;
    FEditor: TWinControl;
    FEndOfLine: Boolean;
    FFileName: string;
    FInfo: TBCEditorHighlighterInfo;
    FLoading: Boolean;
    FMatchingPairs: TList;
    FMainRules: TBCEditorRange;
    FName: string;
    FPrepared: Boolean;
    FPreviousEol: Boolean;
    FRunPosition: LongInt;
    FTemporaryCurrentTokens: TList;
    FTokenPosition: Integer;
    FWordBreakChars: TBCEditorCharSet;
    procedure AddAllAttributes(ARange: TBCEditorRange);
    procedure UpdateAttributes(ARange: TBCEditorRange; AParentRange: TBCEditorRange);
  protected
    function GetAttribute(Index: Integer): TBCEditorHighlighterAttribute;
    procedure AddAttribute(AHighlighterAttribute: TBCEditorHighlighterAttribute);
    procedure Prepare;
    procedure Reset;
    procedure SetAttributesOnChange(AEvent: TNotifyEvent);
    procedure SetWordBreakChars(AChars: TBCEditorCharSet);
  public
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;

    function GetCurrentRange: Pointer;
    function GetCurrentRangeAttribute: TBCEditorHighlighterAttribute;
    function GetEol: Boolean;
    function GetToken: string;
    function GetTokenAttribute: TBCEditorHighlighterAttribute;
    function GetTokenKind: Integer;
    function GetTokenLength: Integer;
    function GetTokenPosition: Integer;
    procedure AddKeywords(var StringList: TStrings);
    procedure Clear;
    procedure LoadFromFile(AFileName: string);
    procedure Next;
    procedure NextToEol;
    procedure ResetCurrentRange;
    procedure SetCurrentLine(NewValue: string; LineNumber: Integer);
    procedure SetCurrentRange(Value: Pointer);
    procedure UpdateColors;

    property Attribute[Index: Integer]: TBCEditorHighlighterAttribute read GetAttribute;
    property Attributes: TStringList read FAttributes;
    property CodeFoldingRegions: TBCEditorCodeFoldingRegions read FCodeFoldingRegions;
    property CompletionProposalSkipRegions: TBCEditorSkipRegions read FCompletionProposalSkipRegions write FCompletionProposalSkipRegions;
    property Editor: TWinControl read FEditor;
    property FileName: string read FFileName write FFileName;
    property Info: TBCEditorHighlighterInfo read FInfo write FInfo;
    property Loading: Boolean read FLoading write FLoading;
    property MainRules: TBCEditorRange read FMainRules;
    property MatchingPairs: TList read FMatchingPairs write FMatchingPairs;
    property Name: string read FName write FName;
    property Colors: TBCEditorHighlighterColors read FColors write FColors;
    property WordBreakChars: TBCEditorCharSet read FWordBreakChars write SetWordBreakChars;
  end;

implementation

uses
  BCEditor.Highlighter.JSONImporter, System.Types, BCEditor.Utils, BCEditor.Consts, BCEditor.Editor.Base;

{ TBCEditorHighlighter }

constructor TBCEditorHighlighter.Create(AOwner: TWinControl);
begin
  inherited Create;

  FEditor := AOwner;
  FWordBreakChars := BCEDITOR_WORD_BREAK_CHARACTERS;

  FAttributes := TStringList.Create;
  FAttributes.Duplicates := dupIgnore;
  FAttributes.Sorted := False;

  FCodeFoldingRegions := TBCEditorCodeFoldingRegions.Create(TBCEditorFoldRegionItem);
  FCompletionProposalSkipRegions := TBCEditorSkipRegions.Create(TBCEditorSkipRegionItem);

  FPrepared := False;

  Info := TBCEditorHighlighterInfo.Create;
  FMainRules := TBCEditorRange.Create;
  FMainRules.Parent := FMainRules;

  FEndOfLine := False;
  FBeginningOfLine := True;
  FPreviousEol := False;
  FCurrentRange := MainRules;

  FColors := TBCEditorHighlighterColors.Create(Self);
  FMatchingPairs := TList.Create;

  FTemporaryCurrentTokens := TList.Create;

  FLoading := False;
end;

destructor TBCEditorHighlighter.Destroy;
begin
  Clear;

  FMainRules.Free;
  FMainRules := nil;
  FInfo.Free;
  FInfo := nil;
  FAttributes.Free;
  FAttributes := nil;
  FCodeFoldingRegions.Free;
  FCodeFoldingRegions := nil;
  FCompletionProposalSkipRegions.Free;
  FCompletionProposalSkipRegions := nil;
  FMatchingPairs.Free;
  FMatchingPairs := nil;
  FColors.Free;
  FColors := nil;
  FTemporaryCurrentTokens.Free;

  inherited;
end;

procedure TBCEditorHighlighter.AddAllAttributes(ARange: TBCEditorRange);
var
  i: Integer;
begin
  AddAttribute(ARange.Attribute);
  for i := 0 to ARange.KeyListCount - 1 do
    AddAttribute(ARange.KeyList[i].Attribute);
  for i := 0 to ARange.SetCount - 1 do
    AddAttribute(ARange.Sets[i].Attribute);
  for i := 0 to ARange.RangeCount - 1 do
    AddAllAttributes(ARange.Ranges[i]);
end;

procedure TBCEditorHighlighter.SetCurrentLine(NewValue: string; LineNumber: Integer);
begin
  if Assigned(FCurrentRange) then
    if not FCurrentRange.Prepared then
      Prepare;

  FCurrentLine := PChar(NewValue);
  FRunPosition := 0;
  FTokenPosition := 0;
  FEndOfLine := False;
  FBeginningOfLine := True;
  FPreviousEol := False;
  FCurrentToken := nil;
  Next;
end;

procedure TBCEditorHighlighter.Next;
var
  i: Integer;
  LParser: TBCEditorAbstractParser;
  LKeyword: PChar;
  LCloseParent: Boolean;
begin
  while FTemporaryCurrentTokens.Count > 0 do
  begin
    FCurrentToken := TBCEditorToken(FTemporaryCurrentTokens[0]);
    FCurrentToken.Free;
    FCurrentToken := nil;
    FTemporaryCurrentTokens.Delete(0);
  end;

  if FPreviousEol then
  begin
    if Assigned(FCurrentRange) then
      if FCurrentRange.CloseOnEol or FCurrentRange.CloseOnTerm then
        FCurrentRange := FCurrentRange.Parent;
    FEndOfLine := True;
    Exit;
  end;

  if Assigned(FCurrentRange) then
  begin
    if FCurrentRange.AlternativeClose <> '' then
    begin
      LKeyword := PChar(FCurrentRange.AlternativeClose);
      i := FRunPosition;
      while (FCurrentLine[i] <> BCEDITOR_NONE_CHAR) and (FCurrentLine[i] = LKeyword^) do
      begin
        Inc(LKeyword);
        Inc(i);
      end;
      if LKeyword^ = BCEDITOR_NONE_CHAR then
        FCurrentRange := FCurrentRange.Parent;
    end;
  end;

  FTokenPosition := FRunPosition;
  if Assigned(FCurrentRange) then
  begin
    LCloseParent := FCurrentRange.CloseParent;
    if FCurrentRange.CloseOnTerm and CharInSet(FCurrentLine[FRunPosition], FCurrentRange.Delimiters) and
      not (FCurrentRange.SkipWhitespace and CharInSet(FCurrentLine[FRunPosition], BCEDITOR_ABSOLUTE_DELIMITERS)) then
    begin
      FCurrentRange := FCurrentRange.Parent;
      if Assigned(FCurrentRange) then
        if LCloseParent then
          FCurrentRange := FCurrentRange.Parent;
    end;
  end;

  if Assigned(FCurrentRange) then
  begin
    LParser := FCurrentRange.SymbolList[FCurrentRange.CaseFunct(FCurrentLine[FRunPosition])];
    if not Assigned(LParser) then
      Inc(FRunPosition)
    else
    if not LParser.GetToken(FCurrentRange, FCurrentLine, FRunPosition, FCurrentToken) then
    begin
      FCurrentToken := FCurrentRange.DefaultToken;

      while not CharInSet(FCurrentLine[FRunPosition], FCurrentRange.Delimiters) do
        Inc(FRunPosition);
    end
    else
    if FCurrentRange.ClosingToken = FCurrentToken then
      FCurrentRange := FCurrentRange.Parent
    else
    if Assigned(FCurrentToken) and Assigned(FCurrentToken.OpenRule) then
      if FCurrentToken.OpenRule is TBCEditorRange then
      begin
        FCurrentRange := TBCEditorRange(FCurrentToken.OpenRule);
        FCurrentRange.ClosingToken := FCurrentToken.ClosingToken;
        if FCurrentRange.OpenBeginningOfLine and not FBeginningOfLine then
        begin
          FCurrentRange := FCurrentRange.Parent;
          FCurrentToken := FCurrentRange.DefaultToken;
        end;
      end;
    if Assigned(FCurrentToken) then
      if FCurrentToken.Temporary then
        FTemporaryCurrentTokens.Add(FCurrentToken);
  end;

  if FBeginningOfLine then
    if (FRunPosition - 1 >= 0) then
      if not CharInset(FCurrentLine[FRunPosition - 1], BCEDITOR_ABSOLUTE_DELIMITERS) then
        FBeginningOfLine := False;

  if FCurrentLine[FRunPosition] = BCEDITOR_NONE_CHAR then
    FPreviousEol := True;
end;

function TBCEditorHighlighter.GetCurrentRangeAttribute: TBCEditorHighlighterAttribute;
begin
  Result := nil;
  if Assigned(FCurrentRange) then
    Result := FCurrentRange.Attribute;
end;

function TBCEditorHighlighter.GetEol: Boolean;
begin
  Result := FEndOfLine;
end;

function TBCEditorHighlighter.GetCurrentRange: Pointer;
begin
  Result := FCurrentRange;
end;

function TBCEditorHighlighter.GetToken: string;
var
  LLength: LongInt;
begin
  LLength := FRunPosition - FTokenPosition;
  SetString(Result, FCurrentLine + FTokenPosition, LLength);
end;

function TBCEditorHighlighter.GetTokenAttribute: TBCEditorHighlighterAttribute;
begin
  if Assigned(FCurrentToken) then
    Result := FCurrentToken.Attribute
  else
    Result := nil;
end;

function TBCEditorHighlighter.GetTokenPosition: Integer;
begin
  Result := FTokenPosition;
end;

procedure TBCEditorHighlighter.ResetCurrentRange;
begin
  FCurrentRange := MainRules;
end;

procedure TBCEditorHighlighter.SetCurrentRange(Value: Pointer);
begin
  FCurrentRange := TBCEditorRange(Value);
end;

procedure TBCEditorHighlighter.AddKeywords(var StringList: TStrings);
var
  i, j: Integer;
begin
  if not Assigned(StringList) then
    Exit;
  for i := 0 to FMainRules.KeyListCount - 1 do
    for j := 0 to FMainRules.KeyList[i].KeyList.Count - 1 do
      StringList.Add(FMainRules.KeyList[i].KeyList[j]);
end;

procedure TBCEditorHighlighter.Reset;
begin
  MainRules.Reset;
end;

function TBCEditorHighlighter.GetTokenKind: Integer;
var
  i, j: Integer;
  LToken: string;
  LTokenType: TBCEditorRangeType;
begin
  LTokenType := FCurrentRange.TokenType;
  if LTokenType <> ttUnspecified then
    Result := Integer(LTokenType)
  else
  { keyword token type }
  begin
    LToken := GetToken;
    for i := 0 to FCurrentRange.KeyListCount - 1 do
      for j := 0 to FCurrentRange.KeyList[i].KeyList.Count - 1 do
      if FCurrentRange.KeyList[i].KeyList[j].Equals(LToken) then
      begin
        Result := Integer(FCurrentRange.KeyList[i].TokenType);
        Exit;
      end;
    Result := 0; { ttUnspecified }
  end;
end;

procedure TBCEditorHighlighter.Clear;
var
  i: Integer;
begin
  FAttributes.Clear;
  FMainRules.Clear;
  FInfo.Clear;
  FCompletionProposalSkipRegions.Clear;
  for i := FMatchingPairs.Count - 1 downto 0 do
    Dispose(PBCEditorMatchingPairToken(FMatchingPairs.Items[i]));
  FMatchingPairs.Clear;
  FCodeFoldingRegions.SkipRegions.Clear;
  FCodeFoldingRegions.Clear;
  (Editor as TBCBaseEditor).ClearMatchingPair;
end;

procedure TBCEditorHighlighter.Prepare;
begin
  FAttributes.Clear;
  AddAllAttributes(MainRules);
  FMainRules.Prepare(FMainRules);
end;

procedure TBCEditorHighlighter.UpdateAttributes(ARange: TBCEditorRange; AParentRange: TBCEditorRange);
var
  i: Integer;

  procedure SetAttributes(Attribute: TBCEditorHighlighterAttribute);
  var
    Element: PBCEditorHighlighterElement;
  begin
    Element := FColors.GetElement(Attribute.Element);
    if Assigned(Element) then
    begin
      if Attribute.ParentBackground and Assigned(AParentRange) then
        Attribute.Background := AParentRange.Attribute.Background
      else
        Attribute.Background := Element.Background;
      if Attribute.ParentForeground and Assigned(AParentRange) then
        Attribute.Foreground := AParentRange.Attribute.Foreground
      else
        Attribute.Foreground := Element.Foreground;
      Attribute.Style := Element.Style;
    end;
  end;

begin
  SetAttributes(ARange.Attribute);
  for i := 0 to ARange.KeyListCount - 1 do
    SetAttributes(ARange.KeyList[i].Attribute);
  for i := 0 to ARange.SetCount - 1 do
    SetAttributes(ARange.Sets[i].Attribute);

  if ARange.RangeCount > 0 then
  for i := 0 to ARange.RangeCount - 1 do
    UpdateAttributes(ARange.Ranges[i], ARange);
end;

procedure TBCEditorHighlighter.UpdateColors;
begin
  UpdateAttributes(MainRules, nil);
  MainRules.Prepare(MainRules);
end;

procedure TBCEditorHighlighter.LoadFromFile(AFileName: string);
var
  LStream: TStream;
  LEditor: TBCBaseEditor;
begin
  FLoading := True;
  FFileName := AFileName;
  FName := ExtractFileName(AFileName);
  FName := Copy(FName, 1, Pos('.', FName) - 1);
  LEditor := FEditor as TBCBaseEditor;
  if Assigned(LEditor) then
  begin
    LStream := LEditor.CreateFileStream(LEditor.GetHighlighterFileName(AFileName));
    try
      with TBCEditorHighlighterJSONImporter.Create(Self) do
      try
        ImportFromStream(LStream);
      finally
        Free;
      end;
    finally
      LStream.Free;
    end;
    UpdateColors;
  end;
  FLoading := False;
end;

function TBCEditorHighlighter.GetAttribute(Index: Integer): TBCEditorHighlighterAttribute;
begin
  Result := nil;
  if (Index >= 0) and (Index < FAttributes.Count) then
    Result := TBCEditorHighlighterAttribute(FAttributes.Objects[Index]);
end;

procedure TBCEditorHighlighter.AddAttribute(AHighlighterAttribute: TBCEditorHighlighterAttribute);
begin
  FAttributes.AddObject(AHighlighterAttribute.Name, AHighlighterAttribute);
end;

procedure TBCEditorHighlighter.SetWordBreakChars(AChars: TBCEditorCharSet);
begin
  FWordBreakChars := AChars;
end;

procedure TBCEditorHighlighter.NextToEol;
begin
  while not GetEol do
    Next;
end;

procedure TBCEditorHighlighter.SetAttributesOnChange(AEvent: TNotifyEvent);
var
  i: Integer;
  LHighlighterAttribute: TBCEditorHighlighterAttribute;
begin
  for i := FAttributes.Count - 1 downto 0 do
  begin
    LHighlighterAttribute := TBCEditorHighlighterAttribute(FAttributes.Objects[i]);
    if Assigned(LHighlighterAttribute) then
    begin
      LHighlighterAttribute.OnChange := AEvent;
      LHighlighterAttribute.InternalSaveDefaultValues;
    end;
  end;
end;

function TBCEditorHighlighter.GetTokenLength: Integer;
begin
  Result := FRunPosition - FTokenPosition;
end;

end.

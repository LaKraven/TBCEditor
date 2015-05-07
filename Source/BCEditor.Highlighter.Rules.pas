unit BCEditor.Highlighter.Rules;

interface

uses
  Vcl.Graphics, System.Classes, System.SysUtils, BCEditor.Highlighter.Token, BCEditor.Highlighter.Attributes,
  BCEditor.Types;

type
  TBCEditorRange = class;

  TBCEditorSet = class;

  TBCEditorAbstractParser = class abstract
  public
    function GetToken(ACurrentRule: TBCEditorRange; APLine: PChar; var ARun: Integer; var AToken: TBCEditorToken): Boolean;
      virtual; abstract;
  end;

  TBCEditorParser = class(TBCEditorAbstractParser)
  strict private
    FHeadNode: TBCEditorTokenNode;
    FSets: TList;
  public
    constructor Create(AChar: Char; AToken: TBCEditorToken; ABreakType: TBCEditorBreakType); reintroduce; overload; virtual;
    constructor Create(ASet: TBCEditorSet); reintroduce; overload; virtual;
    destructor Destroy; override;

    function GetToken(ACurrentRange: TBCEditorRange; APLine: PChar; var ARun: Integer; var AToken: TBCEditorToken): Boolean; override;
    procedure AddSet(ASet: TBCEditorSet);
    procedure AddTokenNode(AString: string; AToken: TBCEditorToken; ABreakType: TBCEditorBreakType);
    property HeadNode: TBCEditorTokenNode read FHeadNode;
    property Sets: TList read FSets;
  end;

  TBCEditorDefaultParser = class(TBCEditorAbstractParser)
  strict private
    FToken: TBCEditorToken;
  public
    constructor Create(AToken: TBCEditorToken); reintroduce; virtual;
    destructor Destroy; override;

    function GetToken(ACurrentRange: TBCEditorRange; APLine: PChar; var ARun: Integer; var AToken: TBCEditorToken): Boolean; override;
    property Token: TBCEditorToken read FToken;
  end;

  TDelimitersParser = class(TBCEditorAbstractParser)
  strict private
    FToken: TBCEditorToken;
  public
    constructor Create(AToken: TBCEditorToken); virtual;
    destructor Destroy; override;

    function GetToken(ACurrentRange: TBCEditorRange; APLine: PChar; var ARun: Integer; var AToken: TBCEditorToken): Boolean; override;
    property Token: TBCEditorToken read FToken;
  end;

  TBCEditorRule = class(TBCEditorAbstractRule)
  private
    FStyle: string;
    FAttribute: TBCEditorHighlighterAttribute;
  protected
    FParent: TBCEditorRange;
    function GetAttribute: TBCEditorHighlighterAttribute;
  public
    constructor Create;
    destructor Destroy; override;

    property Attribute: TBCEditorHighlighterAttribute read GetAttribute;
    property Parent: TBCEditorRange read FParent write FParent;
    property Style: string read FStyle;
  end;

  TBCEditorKeyList = class(TBCEditorRule)
  strict private
    FKeyList: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    property KeyList: TStringList read FKeyList write FKeyList;
  end;

  TBCEditorSet = class(TBCEditorRule)
  strict private
    //FBreakType: TBCEditorBreakType;
    FCharSet: TBCEditorCharSet;
    //FStartType: TBCEditorStartType;
  public
    constructor Create(ACharSet: TBCEditorCharSet = []);

    //property BreakType: TBCEditorBreakType read FBreakType;
    property CharSet: TBCEditorCharSet read FCharSet write FCharSet;
    //property StartType: TBCEditorStartType read FStartType;
  end;

  TBCEditorAbstractParserArray = array [Char] of TBCEditorAbstractParser;
  TBCEditorBooleanArray = array [Char] of Boolean;
  TBCEditorCaseFunction = function(AChar: Char): Char;
  TBCEditorStringCaseFunction = function(const AString: string): string;

  TBCEditorRange = class(TBCEditorRule)
  strict private
    FAlternativeClose: string;
    FCaseFunct: TBCEditorCaseFunction;
    FCaseSensitive: Boolean;
    FCloseOnEol: Boolean;
    FCloseOnTerm: Boolean;
    FCloseToken: TBCEditorMultiToken;
    FClosingToken: TBCEditorToken;
    FDefaultSymbols: TBCEditorDefaultParser;
    FDefaultTermSymbol: TDelimitersParser;
    FDefaultToken: TBCEditorToken;
    FDelimiters: TBCEditorCharSet;
    //FHasNodeAnyStart: TBCEditorBooleanArray;
    FKeyList: TList;
    FOpenToken: TBCEditorMultiToken;
    FPrepared: Boolean;
    FRanges: TList;
    FSets: TList;
    FStringCaseFunct: TBCEditorStringCaseFunction;
    FSymbolList: TBCEditorAbstractParserArray;
    FTokens: TList;
    function GetKeyList(Index: Integer): TBCEditorKeyList;
    function GetKeyListCount: Integer;
    function GetRange(Index: Integer): TBCEditorRange;
    function GetRangeCount: Integer;
    function GetSet(Index: Integer): TBCEditorSet;
    function GetSetCount: Integer;
    function GetToken(Index: Integer): TBCEditorToken;
    function GetTokenCount: Integer;
    procedure SetCaseSensitive(const Value: Boolean);
  public
    constructor Create(AOpenToken: string = ''; ACloseToken: string = ''); virtual;
    destructor Destroy; override;

    function AddKeyList(AName: string; AColor: TColor): TBCEditorKeyList; overload;
    function AddRange(AOpen, AClose, AName: string; AColor: TColor): TBCEditorRange; overload;
    function AddSet(AName: string; ACharSet: TBCEditorCharSet; AColor: TColor): TBCEditorSet; overload;
    function FindToken(AString: string): TBCEditorToken;
    procedure AddKeyList(NewKeyList: TBCEditorKeyList); overload;
    procedure AddRange(NewRange: TBCEditorRange); overload;
    procedure AddRule(NewRule: TBCEditorRule);
    procedure AddSet(NewSet: TBCEditorSet); overload;
    procedure AddToken(NewToken: TBCEditorToken);
    procedure AddTokenRange(AOpenToken, ACloseToken: string);
    procedure Clear;
    procedure DeleteCoupleTokens(Index: Integer);
    procedure DeleteKeyList(Index: Integer); overload;
    procedure DeleteKeyList(var AKeyList: TBCEditorKeyList); overload;
    procedure DeleteRange(Index: Integer); overload;
    procedure DeleteRange(var ARange: TBCEditorRange); overload;
    procedure DeleteRule(ARule: TBCEditorRule);
    procedure DeleteSet(var ASet: TBCEditorSet); overload;
    procedure DeleteSet(Index: Integer); overload;
    procedure InsertKeyList(AKeyList: TBCEditorKeyList; Index: Integer);
    procedure InsertRange(ARange: TBCEditorRange; Index: Integer);
    procedure InsertRule(ARule: TBCEditorRule; Index: Integer);
    procedure InsertSet(ASet: TBCEditorSet; Index: Integer);
    procedure MoveKeyList(AKeyList: TBCEditorKeyList; NewIndex: Integer);
    procedure MoveRange(ARange: TBCEditorRange; NewIndex: Integer);
    procedure MoveRule(ARule: TBCEditorRule; NewIndex: Integer);
    procedure MoveSet(ASet: TBCEditorSet; NewIndex: Integer);
    procedure Prepare(AParent: TBCEditorRange);
    procedure RemoveRule(ARule: TBCEditorRule);
    procedure Reset;
    procedure SetDelimiters(ADelimiters: TBCEditorCharSet);
    property AlternativeClose: string read FAlternativeClose write FAlternativeClose;
    property CaseFunct: TBCEditorCaseFunction read FCaseFunct;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property CloseOnEol: Boolean read FCloseOnEol write FCloseOnEol;
    property CloseOnTerm: Boolean read FCloseOnTerm write FCloseOnTerm;
    property CloseToken: TBCEditorMultiToken read FCloseToken write FCloseToken;
    property ClosingToken: TBCEditorToken read FClosingToken write FClosingToken;
    property DefaultToken: TBCEditorToken read FDefaultToken;
    property Delimiters: TBCEditorCharSet read FDelimiters write FDelimiters;
    //property HasNodeAnyStart: TBCEditorBooleanArray read FHasNodeAnyStart;
    property KeyListCount: Integer read GetKeyListCount;
    property KeyList[Index: Integer]: TBCEditorKeyList read GetKeyList;
    property OpenToken: TBCEditorMultiToken read FOpenToken write FOpenToken;
    property Prepared: Boolean read FPrepared;
    property RangeCount: Integer read GetRangeCount;
    property Ranges[Index: Integer]: TBCEditorRange read GetRange;
    property SetCount: Integer read GetSetCount;
    property Sets[Index: Integer]: TBCEditorSet read GetSet;
    property StringCaseFunct: TBCEditorStringCaseFunction read FStringCaseFunct;
    property SymbolList: TBCEditorAbstractParserArray read FSymbolList;
    property TokenCount: Integer read GetTokenCount;
    property Tokens[Index: Integer]: TBCEditorToken read GetToken;
  end;

implementation

uses
  BCEditor.Utils, BCEditor.Consts, System.Types;

{function DoesNodeHaveAnyStart(Node: TBCEditorTokenNode): Boolean;
var
  i: Integer;
begin
  Result := False;

  if Node.StartType = stAny then
  begin
    Result := True;
    Exit;
  end;
  for i := 0 to Node.NextNodes.Count - 1 do
    if (Node.NextNodes.Nodes[i].StartType = stAny) or DoesNodeHaveAnyStart(Node.NextNodes.Nodes[i]) then
    begin
      Result := True;
      Exit;
    end
end;  }

function CaseNone(AChar: Char): Char;
begin
  Result := AChar;
end;

function StringCaseNone(const AString: string): string;
begin
  Result := AString;
end;

{ TBCEditorParser }

constructor TBCEditorParser.Create(AChar: Char; AToken: TBCEditorToken; ABreakType: TBCEditorBreakType);
begin
  inherited Create;

  FHeadNode := TBCEditorTokenNode.Create(AChar, AToken, ABreakType);
  FSets := TList.Create;
end;

constructor TBCEditorParser.Create(ASet: TBCEditorSet);
begin
  inherited Create;

  FSets := TList.Create;
  AddSet(ASet);
end;

destructor TBCEditorParser.Destroy;
begin
  if Assigned(FHeadNode) then
  begin
    FHeadNode.Free;
    FHeadNode := nil;
  end;
  FSets.Clear;
  FSets.Free;
  FSets := nil;
  inherited;
end;

procedure TBCEditorParser.AddTokenNode(AString: string; AToken: TBCEditorToken; ABreakType: TBCEditorBreakType);
var
  i: Integer;
  LLength: Integer;
  TokenNode: TBCEditorTokenNode;
  TokenNodeList: TBCEditorTokenNodeList;
begin
  TokenNodeList := HeadNode.NextNodes;
  TokenNode := nil;
  LLength := Length(AString);
  for i := 1 to LLength do
  begin
    TokenNode := TokenNodeList.FindNode(AString[i]);
    if not Assigned(TokenNode) then
    begin
      TokenNode := TBCEditorTokenNode.Create(AString[i]);
      TokenNodeList.AddNode(TokenNode);
    end;
    TokenNodeList := TokenNode.NextNodes;
  end;
  //TokenNode.StartType := AToken.StartType;
  TokenNode.BreakType := ABreakType;
  TokenNode.Token := AToken;
end;

procedure TBCEditorParser.AddSet(ASet: TBCEditorSet);
begin
  Sets.Add(ASet);
end;

function TBCEditorParser.GetToken(ACurrentRange: TBCEditorRange; APLine: PChar; var ARun: Integer;
  var AToken: TBCEditorToken): Boolean;
var
  CurrentTokenNode, StartTokenNode, FindTokenNode: TBCEditorTokenNode;
  i, StartPosition, NextPosition, PreviousPosition: Integer;
  AllowedDelimiters: TBCEditorCharSet;

 (* function CanBeToken: Boolean;
 // var
 //   i: Integer;
  begin
    CanBeToken := True;
    if not Assigned(CurrentTokenNode.Token) then
      CanBeToken := False
   { else
    if (CurrentTokenNode.BreakType = btTerm) and not CharInSet(APLine[Succ(ARun)], ACurrentRange.FDelimiters) then
      CanBeToken := False  }
    {else
    case CurrentTokenNode.Token.StartLine of
      slFirstNonSpace:
        for i := 0 to StartPosition - 1 do
          if not CharInSet(APLine[i], [BCEDITOR_SPACE_CHAR, BCEDITOR_TAB_CHAR]) then
          begin
            CanBeToken := False;
            Break;
          end;
      slFirst:
        if StartPosition <> 0 then
          CanBeToken := False;
    end;  }
  end;   *)

begin
  Result := False;
  StartPosition := ARun;
  if Assigned(HeadNode) then
  begin
    CurrentTokenNode := HeadNode;
    NextPosition := StartPosition;
    StartTokenNode := nil;
    repeat
      if Assigned(StartTokenNode) then
      begin
        CurrentTokenNode := StartTokenNode;
        ARun := NextPosition;
        StartTokenNode := nil;
      end;
      if Assigned(CurrentTokenNode.Token) then // if CanBeToken then
        FindTokenNode := CurrentTokenNode
      else
        FindTokenNode := nil;
      PreviousPosition := ARun;
      while (CurrentTokenNode.NextNodes.Count > 0) and (APLine[ARun] <> BCEDITOR_NONE_CHAR) do
      begin
        Inc(ARun);
        CurrentTokenNode := CurrentTokenNode.NextNodes.FindNode(ACurrentRange.CaseFunct(APLine[ARun]));
        if not Assigned(CurrentTokenNode) then
        begin
          Dec(ARun);
          Break;
        end;

        if Assigned(CurrentTokenNode.Token) then //if CanBeToken then
        begin
          FindTokenNode := CurrentTokenNode;
          PreviousPosition := ARun;
        end;

        if not Assigned(StartTokenNode) then
          if //ACurrentRange.HasNodeAnyStart[ACurrentRange.CaseFunct(CurrentTokenNode.Char)] or
            CharInSet(CurrentTokenNode.Char, ACurrentRange.Delimiters){ or CharInSet(ACurrentRange.CaseFunct(APLine[ARun]),
            ACurrentRange.FDelimiters) }then
          begin
            StartTokenNode := CurrentTokenNode;
            NextPosition := ARun;
          end;
      end;

      ARun := PreviousPosition;

      if not Assigned(FindTokenNode) then
        Continue;
      if not Assigned(FindTokenNode.Token) then
        Continue;

      if APLine[ARun] <> BCEDITOR_NONE_CHAR then
        Inc(ARun);

      if FindTokenNode.BreakType = btAny then
      begin
        Result := True;
        AToken := FindTokenNode.Token;
        Exit;
      end;

      if CharInSet(APLine[ARun], ACurrentRange.Delimiters) then
      begin
        Result := True;
        AToken := FindTokenNode.Token;
        Exit;
      end;
    until not Assigned(StartTokenNode);
  end;
  ARun := StartPosition;
  AllowedDelimiters := ACurrentRange.Delimiters;
  for i := 0 to Sets.Count - 1 do
    AllowedDelimiters := AllowedDelimiters - TBCEditorSet(Sets[i]).CharSet;

  for i := 0 to Sets.Count - 1 do
  begin
    ARun := StartPosition;
    repeat
      Inc(ARun);
    until not CharInSet(APLine[ARun], TBCEditorSet(Sets[i]).CharSet) or (APLine[ARun] = BCEDITOR_NONE_CHAR);

   { if TBCEditorSet(Sets[i]).BreakType = btAny then
    begin
      Result := True;
      AToken := TBCEditorToken.Create(TBCEditorSet(Sets[i]).Attribute);
      AToken.Temporary := True;
      Exit;
    end;  }
    if CharInSet(APLine[ARun], AllowedDelimiters) then
    begin
      Result := True;
      AToken := TBCEditorToken.Create(TBCEditorSet(Sets[i]).Attribute);
      AToken.Temporary := True;
      Exit;
    end;
  end;
  ARun := Succ(StartPosition);
end;

constructor TBCEditorDefaultParser.Create(AToken: TBCEditorToken);
begin
  FToken := AToken;
end;

destructor TBCEditorDefaultParser.Destroy;
begin
  FToken.Free;
  FToken := nil;
  inherited;
end;

function TBCEditorDefaultParser.GetToken(ACurrentRange: TBCEditorRange; APLine: PChar; var ARun: Integer;
  var AToken: TBCEditorToken): Boolean;
begin
  Inc(ARun);
  Result := False;
end;

constructor TDelimitersParser.Create(AToken: TBCEditorToken);
begin
  inherited Create;
  FToken := AToken;
end;

destructor TDelimitersParser.Destroy;
begin
  FToken.Free;
  FToken := nil;
  inherited;
end;

function TDelimitersParser.GetToken(ACurrentRange: TBCEditorRange; APLine: PChar; var ARun: Integer; var AToken: TBCEditorToken): Boolean;
begin
  if APLine[ARun] <> BCEDITOR_NONE_CHAR then
    Inc(ARun);
  AToken := Self.Token;
  Result := True;
end;

constructor TBCEditorRule.Create;
begin
  inherited;

  FAttribute := TBCEditorHighlighterAttribute.Create('');
end;

destructor TBCEditorRule.Destroy;
begin
  FAttribute.Free;
  FAttribute := nil;

  inherited;
end;

function TBCEditorRule.GetAttribute: TBCEditorHighlighterAttribute;
begin
  Result := nil;

  if Assigned(FAttribute) then
  begin
    if FAttribute.ParentForeground then
      FAttribute.Foreground := Parent.Attribute.Foreground;
    if FAttribute.ParentBackground then
      FAttribute.Background := Parent.Attribute.Background;
    Result := FAttribute;
  end;
end;

{ TBCEditorRange }

constructor TBCEditorRange.Create(AOpenToken: string; ACloseToken: string);
begin
  inherited Create;

  FOpenToken := TBCEditorMultiToken.Create;
  FCloseToken := TBCEditorMultiToken.Create;
  AddTokenRange(AOpenToken, ACloseToken);

  FillChar(FSymbolList, SizeOf(SymbolList), 0);

  SetCaseSensitive(False);

  FPrepared := False;

  FRanges := TList.Create;
  FKeyList := TList.Create;
  FSets := TList.Create;
  FTokens := TList.Create;

  FDelimiters := BCEDITOR_DEFAULT_DELIMITERS;

  FAttribute.Foreground := clWindowText;
  FAttribute.Background := clWindow;
end;

destructor TBCEditorRange.Destroy;
begin
  Clear;
  Reset;

  FOpenToken.Free;
  FOpenToken := nil;
  FCloseToken.Free;
  FCloseToken := nil;
  FAttribute.Free;
  FAttribute := nil;
  FKeyList.Free;
  FKeyList := nil;
  FSets.Free;
  FSets := nil;
  FTokens.Free;
  FTokens := nil;
  FRanges.Free;
  FRanges := nil;

  inherited;
end;

procedure TBCEditorRange.AddToken(NewToken: TBCEditorToken);
var
  Token: TBCEditorToken;
begin
  Token := FindToken(NewToken.Symbol);
  if not Assigned(Token) then
    FTokens.Add(NewToken);
end;

function TBCEditorRange.FindToken(AString: string): TBCEditorToken;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to FTokens.Count - 1 do
    if TBCEditorToken(FTokens.Items[i]).Symbol = AString then
    begin
      Result := TBCEditorToken(FTokens.Items[i]);
      Break;
    end;
end;

procedure TBCEditorRange.AddRule(NewRule: TBCEditorRule);
begin
  if NewRule is TBCEditorRange then
    AddRange(NewRule as TBCEditorRange)
  else
  if NewRule is TBCEditorKeyList then
    AddKeyList(NewRule as TBCEditorKeyList)
  else
  if NewRule is TBCEditorSet then
    AddSet(NewRule as TBCEditorSet);
end;

procedure TBCEditorRange.AddRange(NewRange: TBCEditorRange);
begin
  FRanges.Add(NewRange);
  NewRange.Parent := Self;
end;

function TBCEditorRange.AddRange(AOpen, AClose, AName: string; AColor: TColor): TBCEditorRange;
begin
  Result := TBCEditorRange.Create(AOpen, AClose);
  with Result do
  begin
    FAttribute.Foreground := AColor;
    FAttribute.ParentForeground := False;
  end;
  AddRange(Result);
end;

procedure TBCEditorRange.AddKeyList(NewKeyList: TBCEditorKeyList);
begin
  FKeyList.Add(NewKeyList);
  NewKeyList.Parent := Self;
end;

function TBCEditorRange.AddKeyList(AName: string; AColor: TColor): TBCEditorKeyList;
begin
  Result := TBCEditorKeyList.Create;
  with Result do
  begin
    FAttribute.Foreground := AColor;
    FAttribute.ParentForeground := False;
  end;
  AddKeyList(Result);
end;

procedure TBCEditorRange.AddSet(NewSet: TBCEditorSet);
begin
  FSets.Add(NewSet);
  NewSet.Parent := Self;
end;

function TBCEditorRange.AddSet(AName: string; ACharSet: TBCEditorCharSet; AColor: TColor): TBCEditorSet;
begin
  Result := TBCEditorSet.Create(ACharSet);
  with Result do
  begin
    FAttribute.Foreground := AColor;
    FAttribute.ParentForeground := False;
  end;
  AddSet(Result);
end;

procedure TBCEditorRange.InsertRule(ARule: TBCEditorRule; Index: Integer);
begin
  if ARule is TBCEditorRange then
    InsertRange(TBCEditorRange(ARule), Index)
  else
  if ARule is TBCEditorKeyList then
    InsertKeyList(TBCEditorKeyList(ARule), Index)
  else
  if ARule is TBCEditorSet then
    InsertSet(TBCEditorSet(ARule), Index);
end;

procedure TBCEditorRange.InsertRange(ARange: TBCEditorRange; Index: Integer);
begin
  FRanges.Insert(Index, ARange);
end;

procedure TBCEditorRange.InsertKeyList(AKeyList: TBCEditorKeyList; Index: Integer);
begin
  FKeyList.Insert(Index, AKeyList);
end;

procedure TBCEditorRange.InsertSet(ASet: TBCEditorSet; Index: Integer);
begin
  FSets.Insert(Index, ASet);
end;

procedure TBCEditorRange.MoveRule(ARule: TBCEditorRule; NewIndex: Integer);
begin
  if ARule is TBCEditorRange then
    MoveRange(TBCEditorRange(ARule), NewIndex);
  if ARule is TBCEditorKeyList then
    MoveKeyList(TBCEditorKeyList(ARule), NewIndex);
  if ARule is TBCEditorSet then
    MoveSet(TBCEditorSet(ARule), NewIndex);
end;

procedure TBCEditorRange.MoveRange(ARange: TBCEditorRange; NewIndex: Integer);
begin
  FRanges.Exchange(FRanges.IndexOf(ARange), NewIndex);
end;

procedure TBCEditorRange.MoveKeyList(AKeyList: TBCEditorKeyList; NewIndex: Integer);
begin
  FKeyList.Exchange(FRanges.IndexOf(AKeyList), NewIndex);
end;

procedure TBCEditorRange.MoveSet(ASet: TBCEditorSet; NewIndex: Integer);
begin
  FSets.Exchange(FRanges.IndexOf(ASet), NewIndex);
end;

procedure TBCEditorRange.RemoveRule(ARule: TBCEditorRule);
begin
  if ARule is TBCEditorRange then
    FRanges.Remove(ARule)
  else
  if ARule is TBCEditorKeyList then
    FKeyList.Remove(ARule)
  else
  if ARule is TBCEditorSet then
    FSets.Remove(ARule);
end;

procedure TBCEditorRange.DeleteRule(ARule: TBCEditorRule);
begin
  if ARule is TBCEditorRange then
    DeleteRange(TBCEditorRange(ARule))
  else
  if ARule is TBCEditorKeyList then
    DeleteKeyList(TBCEditorKeyList(ARule))
  else
  if ARule is TBCEditorSet then
    DeleteSet(TBCEditorSet(ARule));
end;

procedure TBCEditorRange.DeleteRange(var ARange: TBCEditorRange);
begin
  if Assigned(ARange) then
  begin
    FRanges.Remove(ARange);
    ARange.Free;
    ARange := nil;
  end;
end;

procedure TBCEditorRange.DeleteRange(Index: Integer);
begin
  TBCEditorRange(FRanges[Index]).Free;
  FRanges.Delete(Index);
end;

procedure TBCEditorRange.DeleteKeyList(var AKeyList: TBCEditorKeyList);
begin
  if Assigned(AKeyList) then
  begin
    FKeyList.Remove(AKeyList);
    AKeyList.Free;
    AKeyList := nil;
  end;
end;

procedure TBCEditorRange.DeleteKeyList(Index: Integer);
begin
  TBCEditorKeyList(FKeyList[Index]).Free;
  FKeyList.Delete(Index);
end;

procedure TBCEditorRange.DeleteSet(var ASet: TBCEditorSet);
begin
  if Assigned(ASet) then
  begin
    FSets.Remove(ASet);
    ASet.Free;
    ASet := nil;
  end;
end;

procedure TBCEditorRange.DeleteSet(Index: Integer);
begin
  TBCEditorSet(FSets[Index]).Free;
  FSets.Delete(Index);
end;

function TBCEditorRange.GetTokenCount: Integer;
begin
  Result := FTokens.Count;
end;

function TBCEditorRange.GetRangeCount: Integer;
begin
  Result := FRanges.Count;
end;

function TBCEditorRange.GetKeyListCount: Integer;
begin
  Result := FKeyList.Count;
end;

function TBCEditorRange.GetSetCount: Integer;
begin
  Result := FSets.Count;
end;

function TBCEditorRange.GetToken(Index: Integer): TBCEditorToken;
begin
  Result := TBCEditorToken(FTokens[Index]);
end;

function TBCEditorRange.GetRange(Index: Integer): TBCEditorRange;
begin
  Result := TBCEditorRange(FRanges[Index]);
end;

function TBCEditorRange.GetKeyList(Index: Integer): TBCEditorKeyList;
begin
  Result := TBCEditorKeyList(FKeyList[Index]);
end;

function TBCEditorRange.GetSet(Index: Integer): TBCEditorSet;
begin
  Result := TBCEditorSet(FSets[Index]);
end;

procedure TBCEditorRange.AddTokenRange(AOpenToken, ACloseToken: string);//: Integer;
begin
  FOpenToken.AddSymbol(AOpenToken);
  FCloseToken.AddSymbol(ACloseToken);
end;

procedure TBCEditorRange.DeleteCoupleTokens(Index: Integer);
begin
  FOpenToken.DeleteSymbol(Index);
  FCloseToken.DeleteSymbol(Index);
end;

procedure TBCEditorRange.SetDelimiters(ADelimiters: TBCEditorCharSet);
var
  i: Integer;
begin
  Delimiters := ADelimiters;
  for i := 0 to RangeCount - 1 do
    Ranges[i].SetDelimiters(ADelimiters);
end;

procedure TBCEditorRange.SetCaseSensitive(const Value: Boolean);
begin
  FCaseSensitive := Value;
  if not Value then
  begin
    FCaseFunct := UpCase;
    FStringCaseFunct := UpperCase;
  end
  else
  begin
    FCaseFunct := CaseNone;
    FStringCaseFunct := StringCaseNone;
  end;
end;

procedure QuickSortTokenList(const List: TList; const LowerPosition, UpperPosition: Integer);
var
  i, MiddlePosition: Integer;
  PivotValue: string;
begin
  if LowerPosition < UpperPosition then
  begin
    PivotValue := TBCEditorToken(List[LowerPosition]).Symbol;
    MiddlePosition := LowerPosition;

    for i := LowerPosition + 1 to UpperPosition do
    begin
      if TBCEditorToken(List[i]).Symbol < PivotValue then
      begin
        Inc(MiddlePosition);
        List.Exchange(i, MiddlePosition);
      end;
    end;
    List.Exchange(LowerPosition, MiddlePosition);

    QuickSortTokenList(List, LowerPosition, MiddlePosition - 1);
    QuickSortTokenList(List, MiddlePosition + 1, UpperPosition);
  end;
end;

procedure TBCEditorRange.Prepare(AParent: TBCEditorRange);
var
  i, j, LLength: Integer;
  Token: TBCEditorToken;
  Symbol: string;
  FirstChar: Char;
  BreakType: TBCEditorBreakType;

  function InsertTokenDefault(Token: TBCEditorToken; Rules: TBCEditorRange; Attribute: TBCEditorHighlighterAttribute): TBCEditorToken;
  begin
    Result := Rules.FindToken(Token.Symbol);
    if not Assigned(Result) then
    begin
      Result := TBCEditorToken.Create(Token);
      Rules.AddToken(Result);
    end;
    if not Assigned(Result.Attribute) then
      Result.Attribute := Attribute;
  end;

  function InsertToken(Token: TBCEditorToken; Rules: TBCEditorRange): TBCEditorToken;
  begin
    Result := Rules.FindToken(Token.Symbol);
    if not Assigned(Result) then
    begin
      Result := TBCEditorToken.Create(Token);
      Rules.AddToken(Result);
    end
    else
      Result.Attribute := Token.Attribute;
  end;

var
  Range: TBCEditorRange;
  KeyList: TBCEditorKeyList;
  LToken: TBCEditorToken;
  c: Char;
begin
  Reset;
  FDefaultToken := TBCEditorToken.Create(Attribute);
  if Assigned(FDefaultTermSymbol) then
  begin
    FDefaultTermSymbol.Free;
    FDefaultTermSymbol := nil;
  end;
  FDefaultTermSymbol := TDelimitersParser.Create(TBCEditorToken.Create(Attribute));
  FDefaultSymbols := TBCEditorDefaultParser.Create(TBCEditorToken.Create(Attribute));

  FDelimiters := FDelimiters + BCEDITOR_ABSOLUTE_DELIMITERS;

  if not Assigned(AParent) then
    if Assigned(Parent) then
    for j := 0 to OpenToken.SymbolCount - 1 do
    begin
      if OpenToken.Symbols[j] = '' then
        Continue;
      LToken := Parent.FindToken(OpenToken.Symbols[j]);
      Token := TBCEditorToken.Create(CloseToken, j);
      LToken.ClosingToken := InsertTokenDefault(Token, Self, Attribute);
      Token.Free;
    end
    else
      FParent := AParent;

  for i := 0 to FRanges.Count - 1 do
  begin
    Range := TBCEditorRange(FRanges[i]);

    for j := 0 to Range.FOpenToken.SymbolCount - 1 do
    begin
      Token := TBCEditorToken.Create(Range.OpenToken, j);
      LToken := InsertTokenDefault(Token, Self, Range.Attribute);
      LToken.OpenRule := Range;
      Token.Free;

      Token := TBCEditorToken.Create(Range.CloseToken, j);
      LToken.ClosingToken := InsertTokenDefault(Token, Range, Range.Attribute);
      Token.Free;
    end;
    Range.Prepare(Self);
  end;

  for i := 0 to FKeyList.Count - 1 do
  begin
    KeyList := TBCEditorKeyList(FKeyList[i]);

    for j := 0 to KeyList.KeyList.Count - 1 do
    begin
      Token := TBCEditorToken.Create(KeyList.Attribute);
      Token.Symbol := KeyList.KeyList[j];
      InsertToken(Token, Self);
      Token.Free;
    end;
  end;

  QuickSortTokenList(FTokens, 0, FTokens.Count - 1);
  for i := 0 to FTokens.Count - 1 do
  begin
    Token := TBCEditorToken(FTokens[i]);
    LLength := Length(Token.Symbol);
    if LLength < 1 then
      Continue;
    Symbol := Token.Symbol;
    FirstChar := Symbol[1];

    if Token.BreakType <> btUnspecified then
      BreakType := Token.BreakType
    else
    if CharInSet(Symbol[LLength], FDelimiters) then
      BreakType := btAny
    else
      BreakType := btTerm;

    c := CaseFunct(FirstChar);

    if not Assigned(SymbolList[c]) then
    begin
      if LLength = 1 then
        FSymbolList[c] := TBCEditorParser.Create(FirstChar, Token, BreakType)
      else
        FSymbolList[c] := TBCEditorParser.Create(FirstChar, FDefaultToken, BreakType);
    end;
    if LLength <> 1 then
      TBCEditorParser(SymbolList[c]).AddTokenNode(StringCaseFunct(Copy(Symbol, 2, LLength - 1)), Token, BreakType);
  end;

  if FSets.Count > 0 then
    for i := 0 to 255 do
    begin
      c := Char(i);
      for j := 0 to FSets.Count - 1 do
      begin
        if CharInSet(c, TBCEditorSet(FSets[j]).CharSet) then
          if not Assigned(SymbolList[CaseFunct(c)]) then
            FSymbolList[CaseFunct(c)] := TBCEditorParser.Create(TBCEditorSet(FSets[j]))
          else
            TBCEditorParser(SymbolList[CaseFunct(c)]).AddSet(TBCEditorSet(FSets[j]));
      end;
    end;

 { for i := 0 to 255 do
  begin
    c := Char(i);
    if Assigned(SymbolList[Char(i)]) then
      if SymbolList[c] <> FDefaultTermSymbol then
        if SymbolList[c] <> FDefaultSymbols then
          if Assigned(TBCEditorParser(SymbolList[c]).HeadNode) then
            FHasNodeAnyStart[c] := DoesNodeHaveAnyStart(TBCEditorParser(SymbolList[CaseFunct(c)]).HeadNode);
  end;  }

  for i := 0 to 255 do
  begin
    c := Char(i);
    if not Assigned(SymbolList[c]) then
    begin
      if CharInSet(c, FDelimiters) then
        FSymbolList[c] := FDefaultTermSymbol
      else
        FSymbolList[c] := FDefaultSymbols;
    end;
  end;

  FPrepared := True;
end;

procedure TBCEditorRange.Reset;
var
  i: Integer;
  c: Char;
begin
  if not FPrepared then
    Exit;
  for i := 0 to 255 do
  begin
    c := Char(i);
    if Assigned(SymbolList[c]) and (SymbolList[c] <> FDefaultTermSymbol) and (SymbolList[c] <> FDefaultSymbols) then
    begin
      FSymbolList[c].Free;
      FSymbolList[c] := nil;
    end
    else
      FSymbolList[c] := nil;
  end;
  FDefaultToken.Free;
  FDefaultToken := nil;
  FDefaultTermSymbol.Free;
  FDefaultTermSymbol := nil;
  FDefaultSymbols.Free;
  FDefaultSymbols := nil;
  for i := 0 to FRanges.Count - 1 do
    TBCEditorRange(FRanges[i]).Reset;
  ClearList(FTokens);
  FPrepared := False;
end;

procedure TBCEditorRange.Clear;
var
  i: Integer;
begin
  OpenToken.Clear;
  CloseToken.Clear;
  AddTokenRange('', '');
  CloseOnTerm := False;
  CloseOnEol := False;
  Reset;
  for i := 0 to FRanges.Count - 1 do
    TBCEditorRange(FRanges[i]).Clear;
  ClearList(FRanges);
  ClearList(FTokens);
  ClearList(FKeyList);
  ClearList(FSets);
end;

constructor TBCEditorKeyList.Create;
begin
  inherited;

  FKeyList := TStringList.Create;
  FAttribute.Foreground := clWindowText;
  FAttribute.Background := clWindow;
end;

destructor TBCEditorKeyList.Destroy;
begin
  FKeyList.Free;
  FKeyList := nil;

  inherited;
end;

constructor TBCEditorSet.Create(ACharSet: TBCEditorCharSet = []);
begin
  inherited Create;

  FCharSet := ACharSet;
  FAttribute.Foreground := clWindowText;
  FAttribute.Background := clWindow;
end;

end.

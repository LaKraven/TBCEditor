unit BCEditor.Highlighter.Token;

interface

uses
  System.Classes, BCEditor.Types, BCEditor.Highlighter.Attributes;

type
  TBCEditorAbstractRule = class(TObject)
  strict private
    FTokenType: TBCEditorRangeType;
  public
    property TokenType: TBCEditorRangeType read FTokenType write FTokenType;
  end;

  TBCEditorAbstractToken = class(TObject)
  strict private
    FAttribute: TBCEditorHighlighterAttribute;
    FBreakType: TBCEditorBreakType;
   // FFinishOnEol: Boolean;
    FOpenRule: TBCEditorAbstractRule;
   // FStartLine: TBCEditorStartLine;
    //FStartType: TBCEditorStartType;
  public
    constructor Create; reintroduce; overload;
    constructor Create(AHighlighterAttribute: TBCEditorHighlighterAttribute); reintroduce; overload;
    constructor Create(AToken: TBCEditorAbstractToken); reintroduce; overload;

    procedure Clear;
    property Attribute: TBCEditorHighlighterAttribute read FAttribute write FAttribute;
    property BreakType: TBCEditorBreakType read FBreakType write FBreakType;
  //  property FinishOnEol: Boolean read FFinishOnEol write FFinishOnEol;
    property OpenRule: TBCEditorAbstractRule read FOpenRule write FOpenRule;
 //   property StartLine: TBCEditorStartLine read FStartLine write FStartLine;
  //  property StartType: TBCEditorStartType read FStartType write FStartType;
  end;

  TBCEditorMultiToken = class(TBCEditorAbstractToken)
  strict private
    FSymbols: TStringList;
    function GetSymbol(Index: Integer): string;
    procedure SetSymbol(Index: Integer; ASymbol: string);
  public
    constructor Create; reintroduce; overload;
    constructor Create(AHighlighterAttribute: TBCEditorHighlighterAttribute); reintroduce; overload;
    constructor Create(AMultiToken: TBCEditorMultiToken); reintroduce; overload;
    destructor Destroy; override;

    function AddSymbol(ASymbol: string): Integer;
    function SymbolCount: Integer;
    procedure Clear;
    procedure DeleteSymbol(Index: Integer);
    property Symbols[index: Integer]: string read GetSymbol write SetSymbol;
  end;

  TBCEditorToken = class(TBCEditorAbstractToken)
  strict private
    FClosingToken: TBCEditorToken;
    FSymbol: string;
    FTemporary: Boolean;
    function GetSymbol: string;
  public
    constructor Create; overload;
    constructor Create(AHighlighterAttribute: TBCEditorHighlighterAttribute); overload;
    constructor Create(AToken: TBCEditorToken); overload;
    constructor Create(AMultiToken: TBCEditorMultiToken; Index: Integer); overload;

    procedure Clear;
    property Symbol: string read GetSymbol write FSymbol;
    property ClosingToken: TBCEditorToken read FClosingToken write FClosingToken;
    property Temporary: Boolean read FTemporary write FTemporary;
  end;

  TBCEditorTokenNodeList = class;

  TBCEditorTokenNode = class(TObject)
  strict private
    FChar: Char;
    FBreakType: TBCEditorBreakType;
    //FStartType: TBCEditorStartType;
    FNextNodes: TBCEditorTokenNodeList;
    FToken: TBCEditorToken;
  public
    constructor Create(AChar: Char; AToken: TBCEditorToken; ABreakType: TBCEditorBreakType); overload; // virtual;
    constructor Create(AChar: Char); overload;
    destructor Destroy; override;

    property Char: Char read FChar write FChar;
    property BreakType: TBCEditorBreakType read FBreakType write FBreakType;
   // property StartType: TBCEditorStartType read FStartType write FStartType;
    property NextNodes: TBCEditorTokenNodeList read FNextNodes write FNextNodes;
    property Token: TBCEditorToken read FToken write FToken;
  end;

  TBCEditorTokenNodeList = class(TObject)
  strict private
    FNodeList: TList;
  public
    constructor Create;
    destructor Destroy; override;

    function FindNode(AChar: Char): TBCEditorTokenNode;
    function GetCount: Integer;
    function GetNode(Index: Integer): TBCEditorTokenNode;
    procedure AddNode(Node: TBCEditorTokenNode);
    procedure SetNode(Index: Integer; Value: TBCEditorTokenNode);
    property Count: Integer read GetCount;
    property Nodes[index: Integer]: TBCEditorTokenNode read GetNode write SetNode;
  end;

implementation

uses
  System.SysUtils, BCEditor.Utils;

{ TBCEditorAbstractToken }

constructor TBCEditorAbstractToken.Create;
begin
  inherited;

  FAttribute := nil;
  FOpenRule := nil;
 // FStartLine := slNotFirst;
  //FStartType := stUnspecified;
  FBreakType := btUnspecified;
end;

constructor TBCEditorAbstractToken.Create(AHighlighterAttribute: TBCEditorHighlighterAttribute);
begin
  Create;
  FAttribute := AHighlighterAttribute;
end;

constructor TBCEditorAbstractToken.Create(AToken: TBCEditorAbstractToken);
begin
  inherited Create;
  FAttribute := AToken.Attribute;
 // FStartLine := AToken.StartLine;
 // FStartType := AToken.StartType;
  FBreakType := AToken.BreakType;
end;

procedure TBCEditorAbstractToken.Clear;
begin
 // FStartType := stAny;
  FBreakType := btAny;
end;

{ TBCEditorMultiToken }

constructor TBCEditorMultiToken.Create;
begin
  inherited;

  FSymbols := TStringList.Create;
 // FStartType := stAny;
  BreakType := btAny;
end;

constructor TBCEditorMultiToken.Create(AHighlighterAttribute: TBCEditorHighlighterAttribute);
begin
  inherited;

  Create;
end;

constructor TBCEditorMultiToken.Create(AMultiToken: TBCEditorMultiToken);
begin
  inherited Create(AMultiToken as TBCEditorAbstractToken);

  Create;
end;

destructor TBCEditorMultiToken.Destroy;
begin
  FSymbols.Free;
  FSymbols := nil;
  inherited;
end;

function TBCEditorMultiToken.AddSymbol(ASymbol: string): Integer;
begin
  Result := FSymbols.Add(ASymbol);
end;

procedure TBCEditorMultiToken.Clear;
begin
  FSymbols.Clear;
end;

procedure TBCEditorMultiToken.DeleteSymbol(Index: Integer);
begin
  if (Index > -1) and (Index < FSymbols.Count) then
    FSymbols.Delete(Index)
  //else
  //  raise Exception.Create(Format('%s.DeleteSymbol: Index out of bounds', [ClassName]));
end;

function TBCEditorMultiToken.GetSymbol(Index: Integer): string;
begin
  if (Index > -1) and (Index < FSymbols.Count) then
    Result := FSymbols[Index]
  //else
  //  raise Exception.Create(Format('%s.GetSymbol: Index out of bounds', [ClassName]));
end;

procedure TBCEditorMultiToken.SetSymbol(Index: Integer; ASymbol: string);
begin
  if (Index > -1) and (Index < FSymbols.Count) then
    FSymbols[Index] := ASymbol
  //else
  //  raise Exception.Create(Format('%s.SetSymbol: Index out of bounds', [ClassName]));
end;

function TBCEditorMultiToken.SymbolCount: Integer;
begin
  Result := FSymbols.Count;
end;

constructor TBCEditorToken.Create;
begin
  inherited;

  Symbol := '';
  FTemporary := False;
end;

constructor TBCEditorToken.Create(AHighlighterAttribute: TBCEditorHighlighterAttribute);
begin
  inherited;
  Symbol := '';
end;

constructor TBCEditorToken.Create(AToken: TBCEditorToken);
begin
  inherited Create(AToken as TBCEditorAbstractToken);
  Symbol := AToken.Symbol;
end;

constructor TBCEditorToken.Create(AMultiToken: TBCEditorMultiToken; Index: Integer);
begin
  inherited Create(AMultiToken as TBCEditorAbstractToken);
  Symbol := AMultiToken.Symbols[Index];
end;

function TBCEditorToken.GetSymbol: string;
begin
  {if FinishOnEol then
    Result := FSymbol + BCEDITOR_NONE_CHAR
  else   }
  Result := FSymbol;
end;

procedure TBCEditorToken.Clear;
begin
  Symbol := '';
end;

{ TBCEditorTokenNode }

constructor TBCEditorTokenNode.Create(AChar: Char);
begin
  inherited Create;
  FChar := AChar;
  FNextNodes := TBCEditorTokenNodeList.Create;
  FToken := nil;
end;

constructor TBCEditorTokenNode.Create(AChar: Char; AToken: TBCEditorToken; ABreakType: TBCEditorBreakType);
begin
  Create(AChar);
  FBreakType := ABreakType;
 // FStartType := AToken.StartType;
  FToken := AToken;
end;

destructor TBCEditorTokenNode.Destroy;
begin
  FNextNodes.Free;
  FNextNodes := nil;
  inherited;
end;

{ TBCEditorTokenNodeList }

constructor TBCEditorTokenNodeList.Create;
begin
  inherited;

  FNodeList := TList.Create;
end;

destructor TBCEditorTokenNodeList.Destroy;
begin
  FreeList(FNodeList);
  inherited;
end;

procedure TBCEditorTokenNodeList.AddNode(Node: TBCEditorTokenNode);
begin
  FNodeList.Add(Node);
end;

function TBCEditorTokenNodeList.FindNode(AChar: Char): TBCEditorTokenNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FNodeList.Count - 1 do
    if TBCEditorTokenNode(FNodeList[i]).Char = AChar then
    begin
      Result := TBCEditorTokenNode(FNodeList[i]);
      Break;
    end;
end;

function TBCEditorTokenNodeList.GetCount: Integer;
begin
  Result := FNodeList.Count;
end;

function TBCEditorTokenNodeList.GetNode(Index: Integer): TBCEditorTokenNode;
begin
  Result := TBCEditorTokenNode(FNodeList[index]);
end;

procedure TBCEditorTokenNodeList.SetNode(Index: Integer; Value: TBCEditorTokenNode);
begin
  if Index < FNodeList.Count then
    TBCEditorTokenNode(FNodeList[index]).Free;
  FNodeList[index] := Value;
end;

end.

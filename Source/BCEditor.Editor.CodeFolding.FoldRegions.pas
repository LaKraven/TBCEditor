unit BCEditor.Editor.CodeFolding.FoldRegions;

interface

uses
  System.Classes, System.SysUtils, BCEditor.Editor.SkipRegions;

type
  TBCEditorCodeFoldingRegions = class;

  TBCEditorFoldRegionItem = class(TCollectionItem)
  strict private
    FOpenTokenBeginningOfLine: Boolean;
    FCloseTokenBeginningOfLine: Boolean;
    FBreakIfNotFoundBeforeNextRegion: string;
    FCloseAtNextToken: Boolean;
    FCloseToken: string;
    FCloseTokenLength: Integer;
    FNoSubs: Boolean;
    FOpenIsClose: Boolean;
    FOpenToken: string;
    FOpenTokenEnd: string;
    FOpenTokenLength: Integer;
    FParentRegion: TBCEditorFoldRegionItem;
    FSharedClose: Boolean;
    FSkipIfFoundAfterOpenToken: string;
    FTokenEndIsPreviousLine: Boolean;
  public
    property OpenTokenBeginningOfLine: Boolean read FOpenTokenBeginningOfLine write FOpenTokenBeginningOfLine default False;
    property CloseTokenBeginningOfLine: Boolean read FCloseTokenBeginningOfLine write FCloseTokenBeginningOfLine default False;
    property BreakIfNotFoundBeforeNextRegion: string read FBreakIfNotFoundBeforeNextRegion write FBreakIfNotFoundBeforeNextRegion;
    property CloseAtNextToken: Boolean read FCloseAtNextToken write FCloseAtNextToken;
    property CloseToken: string read FCloseToken write FCloseToken;
    property CloseTokenLength: Integer read FCloseTokenLength write FCloseTokenLength;
    property NoSubs: Boolean read FNoSubs write FNoSubs default False;
    property OpenIsClose: Boolean read FOpenIsClose write FOpenIsClose default False;
    property OpenToken: string read FOpenToken write FOpenToken;
    property OpenTokenEnd: string read FOpenTokenEnd write FOpenTokenEnd;
    property OpenTokenLength: Integer read FOpenTokenLength write FOpenTokenLength;
    property ParentRegion: TBCEditorFoldRegionItem read FParentRegion write FParentRegion;
    property SharedClose: Boolean read FSharedClose write FSharedClose default False;
    property SkipIfFoundAfterOpenToken: string read FSkipIfFoundAfterOpenToken write FSkipIfFoundAfterOpenToken;
    property TokenEndIsPreviousLine: Boolean read FTokenEndIsPreviousLine write FTokenEndIsPreviousLine;
  end;

  TBCEditorCodeFoldingRegions = class(TCollection)
  strict private
    FReverseRegions: TBCEditorCodeFoldingRegions;
    FSkipRegions: TBCEditorSkipRegions;
    FStringEscapeChar: Char;
    function GetItem(Index: Integer): TBCEditorFoldRegionItem;
  public
    constructor Create(ItemClass: TCollectionItemClass);
    destructor Destroy; override;
    function Add(AOpenToken: string; ACloseToken: string): TBCEditorFoldRegionItem;
    function Contains(const AOpenToken, ACloseToken: string): Boolean;
    property Items[index: Integer]: TBCEditorFoldRegionItem read GetItem; default;
    property ReverseRegions: TBCEditorCodeFoldingRegions read FReverseRegions write FReverseRegions;
    property SkipRegions: TBCEditorSkipRegions read FSkipRegions;
    property StringEscapeChar: Char read FStringEscapeChar write FStringEscapeChar default #0;
  end;

implementation

{ TBCEditorCodeFoldingRegions }

function TBCEditorCodeFoldingRegions.Add(AOpenToken: string; ACloseToken: string): TBCEditorFoldRegionItem;
begin
  Result := TBCEditorFoldRegionItem(inherited Add);
  with Result do
  begin
    OpenToken := AOpenToken;
    OpenTokenLength := Length(AOpenToken);
    CloseToken := ACloseToken;
    CloseTokenLength := Length(ACloseToken);
    OpenTokenBeginningOfLine := False;
    CloseTokenBeginningOfLine := False;
    SharedClose := False;
    OpenIsClose := False;
    NoSubs := False;
    SkipIfFoundAfterOpenToken := '';
    BreakIfNotFoundBeforeNextRegion := '';
  end;
end;

constructor TBCEditorCodeFoldingRegions.Create(ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  FSkipRegions := TBCEditorSkipRegions.Create(TBCEditorSkipRegionItem);
  FStringEscapeChar := #0;
end;

destructor TBCEditorCodeFoldingRegions.Destroy;
begin
  FSkipRegions.Free;
  if Assigned(FReverseRegions) then
    FReverseRegions.Free;
  inherited;
end;

function TBCEditorCodeFoldingRegions.Contains(const AOpenToken, ACloseToken: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    if (Items[i].OpenToken = AOpenToken) and (Items[i].CloseToken = ACloseToken) then
      Exit(True);
end;

function TBCEditorCodeFoldingRegions.GetItem(Index: Integer): TBCEditorFoldRegionItem;
begin
  Result := TBCEditorFoldRegionItem(inherited Items[index]);
end;

end.

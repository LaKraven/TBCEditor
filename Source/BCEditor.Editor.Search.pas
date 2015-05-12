unit BCEditor.Editor.Search;

interface

uses
  System.Classes, Vcl.Controls, BCEditor.Editor.Search.Map, BCEditor.Types,
  BCEditor.Editor.Search.Highlighter;

const
  BCEDITOR_SEARCH_OPTIONS = [soHighlightResults, soSearchOnTyping, soBeepIfStringNotFound];

type
  TBCEditorSearch = class(TPersistent)
  strict private
    FEnabled: Boolean;
    FEngine: TBCEditorSearchEngine;
    FHighlighter: TBCEditorSearchHighlighter;
    FMap: TBCEditorSearchMap;
    FOnChange: TBCEditorSearchChangeEvent;
    FOptions: TBCEditorSearchOptions;
    FSearchText: String;
    procedure DoChange;
    procedure SetEnabled(const Value: Boolean);
    procedure SetEngine(const Value: TBCEditorSearchEngine);
    procedure SetHighlighter(const Value: TBCEditorSearchHighlighter);
    procedure SetMap(const Value: TBCEditorSearchMap);
    procedure SetOnChange(const Value: TBCEditorSearchChangeEvent);
    procedure SetSearchText(const Value: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Engine: TBCEditorSearchEngine read FEngine write SetEngine default seNormal;
    property Highlighter: TBCEditorSearchHighlighter read FHighlighter write SetHighlighter;
    property Map: TBCEditorSearchMap read FMap write SetMap;
    property OnChange: TBCEditorSearchChangeEvent read FOnChange write SetOnChange;
    property Options: TBCEditorSearchOptions read FOptions write FOptions default BCEDITOR_SEARCH_OPTIONS;
    property SearchText: string read FSearchText write SetSearchText;
  end;

implementation

{ TBCEditorSearchPanel }

constructor TBCEditorSearch.Create;
begin
  inherited;

  FSearchText := '';
  FEngine := seNormal;
  FMap := TBCEditorSearchMap.Create;
  FHighlighter := TBCEditorSearchHighlighter.Create;
  FOptions := BCEDITOR_SEARCH_OPTIONS;
  FEnabled := True;
end;

destructor TBCEditorSearch.Destroy;
begin
  FMap.Free;
  FHighlighter.Free;
  inherited;
end;

procedure TBCEditorSearch.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorSearch) then
  with Source as TBCEditorSearch do
  begin
    Self.FEnabled := FEnabled;
    Self.FSearchText := FSearchText;
    Self.FEngine := FEngine;
    Self.FOptions := FOptions;
    Self.FMap.Assign(FMap);
    Self.FHighlighter.Assign(FHighlighter);
    Self.DoChange;
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorSearch.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(scRefresh);
end;

procedure TBCEditorSearch.SetOnChange(const Value: TBCEditorSearchChangeEvent);
begin
  FOnChange := Value;
  FMap.OnChange := FOnChange;
  FHighlighter.OnChange := FOnChange;
end;

procedure TBCEditorSearch.SetEngine(const Value: TBCEditorSearchEngine);
begin
  FEngine := Value;
  if Assigned(FOnChange) then
    FOnChange(scEngineUpdate);
end;

procedure TBCEditorSearch.SetSearchText(const Value: String);
begin
  FSearchText := Value;
  if Assigned(FOnChange) then
    FOnChange(scSearch);
end;

procedure TBCEditorSearch.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if Assigned(FOnChange) then
      FOnChange(scSearch);
  end;
end;

procedure TBCEditorSearch.SetHighlighter(const Value: TBCEditorSearchHighlighter);
begin
  FHighlighter.Assign(Value);
end;

procedure TBCEditorSearch.SetMap(const Value: TBCEditorSearchMap);
begin
  FMap.Assign(Value);
end;

end.

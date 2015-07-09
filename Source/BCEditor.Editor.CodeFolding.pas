unit BCEditor.Editor.CodeFolding;

interface

uses
  System.Classes, System.SysUtils, Vcl.Graphics,
  BCEditor.Editor.CodeFolding.Types, BCEditor.Editor.CodeFolding.Colors, BCEditor.Editor.CodeFolding.Hint;

type
  TBCEditorCodeFolding = class(TPersistent)
  strict private
    FColors: TBCEditorCodeFoldingColors;
    FHint: TBCEditorCodeFoldingHint;
    FMarkStyle: TBCEditorCodeFoldingMarkStyle;
    FMouseOverHint: Boolean;
    FOnChange: TBCEditorCodeFoldingChangeEvent;
    FOptions: TBCEditorCodeFoldingOptions;
    FPadding: Integer;
    FWidth: Integer;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetColors(const Value: TBCEditorCodeFoldingColors);
    procedure SetHint(Value: TBCEditorCodeFoldingHint);
    procedure SetMarkStyle(const Value: TBCEditorCodeFoldingMarkStyle);
    procedure SetOnChange(Value: TBCEditorCodeFoldingChangeEvent);
    procedure SetOptions(Value: TBCEditorCodeFoldingOptions);
    procedure SetPadding(const Value: Integer);
    procedure SetWidth(Value: Integer);
    procedure SetVisible(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    function GetWidth: Integer;
    procedure Assign(Source: TPersistent); override;
    property MouseOverHint: Boolean read FMouseOverHint write FMouseOverHint;
  published
    property Colors: TBCEditorCodeFoldingColors read FColors write SetColors;
    property Hint: TBCEditorCodeFoldingHint read FHint write SetHint;
    property MarkStyle: TBCEditorCodeFoldingMarkStyle read FMarkStyle write SetMarkStyle default msSquare;
    property OnChange: TBCEditorCodeFoldingChangeEvent read FOnChange write SetOnChange;
    property Options: TBCEditorCodeFoldingOptions read FOptions write SetOptions default [cfoShowCollapsedCodeHint, cfoHighlightIndentGuides, cfoHighlightMatchingPair, cfoShowIndentGuides, cfoUncollapseByHintClick];
    property Padding: Integer read FPadding write SetPadding default 2;
    property Width: Integer read FWidth write SetWidth default 14;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

implementation

uses
  System.Math;

{ TBCEditorCodeFolding }

constructor TBCEditorCodeFolding.Create;
begin
  inherited;

  FVisible := False;
  FOptions := [cfoShowCollapsedCodeHint, cfoHighlightIndentGuides, cfoHighlightMatchingPair, cfoShowIndentGuides, cfoUncollapseByHintClick];
  FMarkStyle := msSquare;
  FColors := TBCEditorCodeFoldingColors.Create;
  FHint := TBCEditorCodeFoldingHint.Create;
  FPadding := 2;
  FWidth := 14;

  FMouseOverHint := False;
end;

destructor TBCEditorCodeFolding.Destroy;
begin
  FColors.Free;
  FHint.Free;

  inherited;
end;

procedure TBCEditorCodeFolding.SetOnChange(Value: TBCEditorCodeFoldingChangeEvent);
begin
  FOnChange := Value;
  FColors.OnChange := Value;
end;

procedure TBCEditorCodeFolding.Assign(Source: TPersistent);
begin
  if Source is TBCEditorCodeFolding then
  with Source as TBCEditorCodeFolding do
  begin
    Self.FVisible := FVisible;
    Self.FOptions := FOptions;
    Self.FColors.Assign(FColors);
    Self.FHint.Assign(FHint);
    Self.FWidth := FWidth;
    if Assigned(Self.OnChange) then
      Self.OnChange(fcRescan);
  end
  else
    inherited;
end;

procedure TBCEditorCodeFolding.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(fcRefresh);
end;

procedure TBCEditorCodeFolding.SetMarkStyle(const Value: TBCEditorCodeFoldingMarkStyle);
begin
  if Value <> FMarkStyle then
  begin
    FMarkStyle := Value;
    DoChange;
  end;
end;

procedure TBCEditorCodeFolding.SetVisible(const Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    if Assigned(FOnChange) then
      FOnChange(fcEnabled);
  end;
end;

procedure TBCEditorCodeFolding.SetOptions(Value: TBCEditorCodeFoldingOptions);
begin
  FOptions := Value;
  DoChange;
end;

procedure TBCEditorCodeFolding.SetColors(const Value: TBCEditorCodeFoldingColors);
begin
  FColors.Assign(Value);
end;

procedure TBCEditorCodeFolding.SetHint(Value: TBCEditorCodeFoldingHint);
begin
  FHint.Assign(Value);
end;

procedure TBCEditorCodeFolding.SetPadding(const Value: Integer);
begin
  if Value <> FPadding then
  begin
    FPadding := Value;
    DoChange;
  end;
end;

function TBCEditorCodeFolding.GetWidth: Integer;
begin
  if FVisible then
    Result := FWidth
  else
    Result := 0;
end;

procedure TBCEditorCodeFolding.SetWidth(Value: Integer);
begin
  Value := Max(0, Value);
  if FWidth <> Value then
  begin
    FWidth := Value;
    DoChange;
  end;
end;

end.

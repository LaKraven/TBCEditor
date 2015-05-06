unit BCEditor.Editor.Search.Highlighter;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Editor.Search.Highlighter.Colors, BCEditor.Types;

type
  TBCEditorSearchHighlighter = class(TPersistent)
  strict private
    FAlphaBlending: Byte;
    FColors: TBCEditorSearchColors;
    FOnChange: TBCEditorSearchChangeEvent;
    procedure SetAlphaBlending(const Value: Byte);
    procedure SetColors(const Value: TBCEditorSearchColors);
    procedure DoChange;
    procedure SetOnChange(Value: TBCEditorSearchChangeEvent);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property AlphaBlending: Byte read FAlphaBlending write SetAlphaBlending default 255;
    property Colors: TBCEditorSearchColors read FColors write SetColors;
    property OnChange: TBCEditorSearchChangeEvent read FOnChange write SetOnChange;
  end;

implementation

{ TBCEditorSearchHighlighter }

constructor TBCEditorSearchHighlighter.Create;
begin
  inherited;

  FAlphaBlending := 255;
  FColors := TBCEditorSearchColors.Create;
end;

destructor TBCEditorSearchHighlighter.Destroy;
begin
  FColors.Free;
  inherited;
end;

procedure TBCEditorSearchHighlighter.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorSearchHighlighter) then
  with Source as TBCEditorSearchHighlighter do
  begin
    Self.FAlphaBlending := FAlphaBlending;
    Self.FColors.Assign(Colors);
    Self.DoChange;
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorSearchHighlighter.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(scRefresh);
end;

procedure TBCEditorSearchHighlighter.SetOnChange(Value: TBCEditorSearchChangeEvent);
begin
  FOnChange := Value;
  FColors.OnChange := FOnChange;
end;

procedure TBCEditorSearchHighlighter.SetColors(const Value: TBCEditorSearchColors);
begin
  FColors.Assign(Value);
end;

procedure TBCEditorSearchHighlighter.SetAlphaBlending(const Value: Byte);
begin
  if Value <> FAlphaBlending then
  begin
    FAlphaBlending := Value;
    DoChange;
  end;
end;

end.

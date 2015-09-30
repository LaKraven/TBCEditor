unit BCEditor.Editor.Minimap.Colors;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Consts;

type
  TBCEditorMinimapColors = class(TPersistent)
  strict private
    FBookmark: TColor;
    FVisibleLines: TColor;
    FOnChange: TNotifyEvent;
    procedure SetBookmark(const Value: TColor);
    procedure SetVisibleLines(const Value: TColor);
    procedure DoChange;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property VisibleLines: TColor read FVisibleLines write SetVisibleLines default clMinimapVisibleLines;
    property Bookmark: TColor read FBookmark write SetBookmark default clMinimapBookmark;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TBCEditorMinimapColors }

constructor TBCEditorMinimapColors.Create;
begin
  inherited;

  FBookmark := clMinimapBookmark;
  FVisibleLines := clMinimapVisibleLines;
end;

procedure TBCEditorMinimapColors.Assign(Source: TPersistent);
begin
  if Source is TBCEditorMinimapColors then
  with Source as TBCEditorMinimapColors do
  begin
    Self.FBookmark := FBookmark;
    Self.FVisibleLines := FVisibleLines;
    Self.DoChange;
  end
  else
    inherited;
end;

procedure TBCEditorMinimapColors.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorMinimapColors.SetBookmark(const Value: TColor);
begin
  if Value <> FBookmark then
  begin
    FBookmark := Value;
    DoChange;
  end;
end;

procedure TBCEditorMinimapColors.SetVisibleLines(const Value: TColor);
begin
  if Value <> FVisibleLines then
  begin
    FVisibleLines := Value;
    DoChange;
  end;
end;

end.

unit BCEditor.Editor.LeftMargin.Colors;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Consts;

type
  TBCEditorLeftMarginColors = class(TPersistent)
  strict private
    FActiveLineBackground: TColor;
    FBackground: TColor;
    FBookmarkPanelBackground: TColor;
    FBorder: TColor;
    FLineNumberLine: TColor;
    FLineStateModified: TColor;
    FLineStateNormal: TColor;
    FOnChange: TNotifyEvent;
    procedure SetActiveLineBackground(const Value: TColor);
    procedure SetBackground(const Value: TColor);
    procedure SetBookmarkPanelBackground(const Value: TColor);
    procedure SetBorder(const Value: TColor);
    procedure SetLineNumberLine(const Value: TColor);
    procedure SetLineStateModified(const Value: TColor);
    procedure SetLineStateNormal(const Value: TColor);
    procedure DoChange;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property ActiveLineBackground: TColor read FActiveLineBackground write SetActiveLineBackground default clActiveLineBackground;
    property Background: TColor read FBackground write SetBackground default clLeftMarginBackground;
    property BookmarkPanelBackground: TColor read FBookmarkPanelBackground write SetBookmarkPanelBackground default clLeftMarginBackground;
    property Border: TColor read FBorder write SetBorder default clLeftMarginBackground;
    property LineNumberLine: TColor read FLineNumberLine write SetLineNumberLine default clLeftMarginFontForeground;
    property LineStateModified: TColor read FLineStateModified write SetLineStateModified default clYellow;
    property LineStateNormal: TColor read FLineStateNormal write SetLineStateNormal default clLime;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TBCEditorLeftMarginColors }

constructor TBCEditorLeftMarginColors.Create;
begin
  inherited;

  FActiveLineBackground := clActiveLineBackground;
  FBackground := clLeftMarginBackground;
  FBookmarkPanelBackground := clLeftMarginBackground;
  FBorder := clLeftMarginBackground;
  FLineNumberLine := clLeftMarginFontForeground;
  FLineStateModified := clYellow;
  FLineStateNormal := clLime;
end;

procedure TBCEditorLeftMarginColors.Assign(Source: TPersistent);
begin
  if Source is TBCEditorLeftMarginColors then
  with Source as TBCEditorLeftMarginColors do
  begin
    Self.FActiveLineBackground := FActiveLineBackground;
    Self.FBackground := FBackground;
    Self.FBookmarkPanelBackground := FBookmarkPanelBackground;
    Self.FBorder := FBorder;
    Self.FLineNumberLine := FLineNumberLine;
    Self.FLineStateModified := FLineStateModified;
    Self.FLineStateNormal := FLineStateNormal;
    Self.DoChange;
  end
  else
    inherited;
end;

procedure TBCEditorLeftMarginColors.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMarginColors.SetActiveLineBackground(const Value: TColor);
begin
  if Value <> FActiveLineBackground then
  begin
    FActiveLineBackground := Value;
    DoChange;
  end;
end;

procedure TBCEditorLeftMarginColors.SetBackground(const Value: TColor);
begin
  if Value <> FBackground then
  begin
    FBackground := Value;
    DoChange;
  end;
end;

procedure TBCEditorLeftMarginColors.SetBookmarkPanelBackground(const Value: TColor);
begin
  if Value <> FBookmarkPanelBackground then
  begin
    FBookmarkPanelBackground := Value;
    DoChange;
  end;
end;

procedure TBCEditorLeftMarginColors.SetBorder(const Value: TColor);
begin
  if Value <> FBorder then
  begin
    FBorder := Value;
    DoChange;
  end;
end;

procedure TBCEditorLeftMarginColors.SetLineNumberLine(const Value: TColor);
begin
  if Value <> FLineNumberLine then
  begin
    FLineNumberLine := Value;
    DoChange;
  end;
end;

procedure TBCEditorLeftMarginColors.SetLineStateModified(const Value: TColor);
begin
  if Value <> FLineStateModified then
  begin
    FLineStateModified := Value;
    DoChange;
  end;
end;

procedure TBCEditorLeftMarginColors.SetLineStateNormal(const Value: TColor);
begin
  if Value <> FLineStateNormal then
  begin
    FLineStateNormal := Value;
    DoChange;
  end;
end;

end.

unit BCEditor.Editor.CodeFolding.Colors;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Consts, BCEditor.Editor.CodeFolding.Types;

type
  TBCEditorCodeFoldingColors = class(TPersistent)
  strict private
    FBackground: TColor;
    FCollapsedLine: TColor;
    FFoldingLine: TColor;
    FFoldingLineHighlight: TColor;
    FIndent: TColor;
    FIndentHighlight: TColor;
    FOnChange: TBCEditorCodeFoldingChangeEvent;
    procedure SetBackground(const Value: TColor);
    procedure SetCollapsedLine(const Value: TColor);
    procedure SetFoldingLine(const Value: TColor);
    procedure SetFoldingLineHighlight(const Value: TColor);
    procedure SetIndent(const Value: TColor);
    procedure SetIndentHighlight(const Value: TColor);
    procedure DoChange;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property CollapsedLine: TColor read FCollapsedLine write SetCollapsedLine default clLeftMarginFontForeground;
    property Background: TColor read FBackground write SetBackground default clLeftMarginBackground;
    property FoldingLine: TColor read FFoldingLine write SetFoldingLine default clLeftMarginFontForeground;
    property FoldingLineHighlight: TColor read FFoldingLineHighlight write SetFoldingLineHighlight default clLeftMarginFontForeground;
    property Indent: TColor read FIndent write SetIndent default clIndent;
    property IndentHighlight: TColor read FIndentHighlight write SetIndentHighlight default clIndentHighlight;
    property OnChange: TBCEditorCodeFoldingChangeEvent read FOnChange write FOnChange;
  end;

implementation

{ TBCEditorCodeFoldingColors }

constructor TBCEditorCodeFoldingColors.Create;
begin
  inherited;

  FCollapsedLine := clLeftMarginFontForeground;
  FBackground := clLeftMarginBackground;
  FFoldingLine := clLeftMarginFontForeground;
  FFoldingLineHighlight := clLeftMarginFontForeground;
  FIndentHighlight := clIndentHighlight;
end;

procedure TBCEditorCodeFoldingColors.Assign(Source: TPersistent);
begin
  if Source is TBCEditorCodeFoldingColors then
  with Source as TBCEditorCodeFoldingColors do
  begin
    Self.FCollapsedLine := FCollapsedLine;
    Self.FBackground := FBackground;
    Self.FFoldingLine := FFoldingLine;
    Self.FFoldingLineHighlight := FFoldingLineHighlight;
    Self.FIndentHighlight := FIndentHighlight;
    Self.DoChange;
  end
  else
    inherited;
end;

procedure TBCEditorCodeFoldingColors.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(fcRefresh);
end;

procedure TBCEditorCodeFoldingColors.SetBackground(const Value: TColor);
begin
  if Value <> FBackground then
  begin
    FBackground := Value;
    DoChange;
  end;
end;

procedure TBCEditorCodeFoldingColors.SetFoldingLine(const Value: TColor);
begin
  if Value <> FFoldingLine then
  begin
    FFoldingLine := Value;
    DoChange;
  end;
end;

procedure TBCEditorCodeFoldingColors.SetFoldingLineHighlight(const Value: TColor);
begin
  if Value <> FFoldingLineHighlight then
  begin
    FFoldingLineHighlight := Value;
    DoChange;
  end;
end;

procedure TBCEditorCodeFoldingColors.SetCollapsedLine(const Value: TColor);
begin
  if Value <> FCollapsedLine then
  begin
    FCollapsedLine := Value;
    DoChange;
  end;
end;

procedure TBCEditorCodeFoldingColors.SetIndent(const Value: TColor);
begin
  if Value <> FIndent then
  begin
    FIndent := Value;
    DoChange;
  end;
end;

procedure TBCEditorCodeFoldingColors.SetIndentHighlight(const Value: TColor);
begin
  if Value <> FIndentHighlight then
  begin
    FIndentHighlight := Value;
    DoChange;
  end;
end;

end.

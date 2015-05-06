unit BCEditor.Editor.Search.Map.Colors;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Consts, BCEditor.Types;

type
  TBCEditorSearchMapColors = class(TPersistent)
  strict private
    FActiveLine: TColor;
    FBackground: TColor;
    FForeground: TColor;
    FOnChange: TBCEditorSearchChangeEvent;
    procedure SetActiveLine(Value: TColor);
    procedure SetBackground(Value: TColor);
    procedure SetForeground(Value: TColor);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property ActiveLine: TColor read FActiveLine write SetActiveLine default clLeftMarginBookmarkBackground;
    property Background: TColor read FBackground write SetBackground default clLeftMarginBackground;
    property Foreground: TColor read FForeground write SetForeground default clSearchHighlighter;
    property OnChange: TBCEditorSearchChangeEvent read FOnChange write FOnChange;
  end;

implementation

{ TBCEditorSelectedColor }

constructor TBCEditorSearchMapColors.Create;
begin
  inherited;

  FActiveLine := clLeftMarginBookmarkBackground;
  FBackground := clLeftMarginBackground;
  FForeground := clSearchHighlighter;
end;

procedure TBCEditorSearchMapColors.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorSearchMapColors) then
  with Source as TBCEditorSearchMapColors do
  begin
    Self.FBackground := FBackground;
    Self.FForeground := FForeground;
    Self.FActiveLine := FActiveLine;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(scRefresh);
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorSearchMapColors.SetActiveLine(Value: TColor);
begin
  if FActiveLine <> Value then
  begin
    FActiveLine := Value;
    if Assigned(FOnChange) then
      FOnChange(scRefresh);
  end;
end;

procedure TBCEditorSearchMapColors.SetBackground(Value: TColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    if Assigned(FOnChange) then
      FOnChange(scRefresh);
  end;
end;

procedure TBCEditorSearchMapColors.SetForeground(Value: TColor);
begin
  if FForeground <> Value then
  begin
    FForeground := Value;
    if Assigned(FOnChange) then
      FOnChange(scRefresh);
  end;
end;

end.

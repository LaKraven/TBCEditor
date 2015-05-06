unit BCEditor.Editor.Search.Highlighter.Colors;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Consts, BCEditor.Types;

type
  TBCEditorSearchColors = class(TPersistent)
  strict private
    FBackground: TColor;
    FForeground: TColor;
    FOnChange: TBCEditorSearchChangeEvent;
    procedure SetBackground(const Value: TColor);
    procedure SetForeground(const Value: TColor);
    procedure DoChange;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Background: TColor read FBackground write SetBackground default clSearchHighlighter;
    property Foreground: TColor read FForeground write SetForeground default clWindowText;
    property OnChange: TBCEditorSearchChangeEvent read FOnChange write FOnChange;
  end;

implementation

{ TBCEditorSearchColors }

constructor TBCEditorSearchColors.Create;
begin
  inherited;

  FBackground := clSearchHighlighter;
  FForeground := clWindowText;
end;

procedure TBCEditorSearchColors.Assign(Source: TPersistent);
begin
  if Source is TBCEditorSearchColors then
  with Source as TBCEditorSearchColors do
  begin
    Self.FBackground := FBackground;
    Self.FForeground := FForeground;
    Self.DoChange;
  end
  else
    inherited;
end;

procedure TBCEditorSearchColors.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(scRefresh);
end;

procedure TBCEditorSearchColors.SetBackground(const Value: TColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    DoChange;
  end;
end;

procedure TBCEditorSearchColors.SetForeground(const Value: TColor);
begin
  if FForeground <> Value then
  begin
    FForeground := Value;
    DoChange;
  end;
end;

end.

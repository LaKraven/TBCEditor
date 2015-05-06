unit BCEditor.Editor.Selection.Colors;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Consts;

type
  TBCEditorSelectedColor = class(TPersistent)
  strict private
    FBackground: TColor;
    FForeground: TColor;
    FOnChange: TNotifyEvent;
    procedure SetBackground(Value: TColor);
    procedure SetForeground(Value: TColor);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Background: TColor read FBackground write SetBackground default clSelectionColor;
    property Foreground: TColor read FForeground write SetForeground default clHighLightText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TBCEditorSelectedColor }

constructor TBCEditorSelectedColor.Create;
begin
  inherited;

  FBackground := clSelectionColor;
  FForeground := clHighLightText;
end;

procedure TBCEditorSelectedColor.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorSelectedColor) then
  with Source as TBCEditorSelectedColor do
  begin
    Self.FBackground := FBackground;
    Self.FForeground := FForeground;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorSelectedColor.SetBackground(Value: TColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TBCEditorSelectedColor.SetForeground(Value: TColor);
begin
  if FForeground <> Value then
  begin
    FForeground := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

end.

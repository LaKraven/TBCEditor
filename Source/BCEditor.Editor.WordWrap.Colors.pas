unit BCEditor.Editor.WordWrap.Colors;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Consts;

type
  TBCEditorWordWrapColors = class(TPersistent)
  strict private
    FBackground: TColor;
    FArrow: TColor;
    FLines: TColor;
    FOnChange: TNotifyEvent;
    procedure SetBackground(const Value: TColor);
    procedure SetArrow(const Value: TColor);
    procedure SetLines(const Value: TColor);
    procedure DoChange;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Background: TColor read FBackground write SetBackground default clWordWrapIndicatorBackground;
    property Arrow: TColor read FArrow write SetArrow default clWordWrapIndicatorArrow;
    property Lines: TColor read FLines write SetLines default clWordWrapIndicatorLines;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TBCEditorCodeFoldingColors }

constructor TBCEditorWordWrapColors.Create;
begin
  inherited;

  FBackground := clWordWrapIndicatorBackground;
  FArrow := clWordWrapIndicatorArrow;
  FLines := clWordWrapIndicatorLines;
end;

procedure TBCEditorWordWrapColors.Assign(Source: TPersistent);
begin
  if Source is TBCEditorWordWrapColors then
  with Source as TBCEditorWordWrapColors do
  begin
    Self.FBackground := FBackground;
    Self.FArrow := FArrow;
    Self.FLines := FLines;
    Self.DoChange;
  end
  else
    inherited;
end;

procedure TBCEditorWordWrapColors.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorWordWrapColors.SetBackground(const Value: TColor);
begin
  if Value <> FBackground then
  begin
    FBackground := Value;
    DoChange;
  end;
end;

procedure TBCEditorWordWrapColors.SetArrow(const Value: TColor);
begin
  if Value <> FArrow then
  begin
    FArrow := Value;
    DoChange;
  end;
end;

procedure TBCEditorWordWrapColors.SetLines(const Value: TColor);
begin
  if Value <> FLines then
  begin
    FLines := Value;
    DoChange;
  end;
end;

end.

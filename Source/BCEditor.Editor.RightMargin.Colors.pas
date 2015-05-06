unit BCEditor.Editor.RightMargin.Colors;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorRightMarginColors = class(TPersistent)
  strict private
    FEdge: TColor;
    FMovingEdge: TColor;
    FOnChange: TNotifyEvent;
    procedure SetEdge(Value: TColor);
    procedure SetMovingEdge(Value: TColor);
    procedure DoChange;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Edge: TColor read FEdge write SetEdge default clSilver;
    property MovingEdge: TColor read FMovingEdge write SetMovingEdge default clSilver;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TBCEditorSelectedColor }

constructor TBCEditorRightMarginColors.Create;
begin
  inherited;

  FEdge := clSilver;
  FMovingEdge := clSilver;
end;

procedure TBCEditorRightMarginColors.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorRightMarginColors) then
  with Source as TBCEditorRightMarginColors do
  begin
    Self.FEdge := FEdge;
    Self.FMovingEdge := FMovingEdge;
    Self.DoChange;
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorRightMarginColors.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorRightMarginColors.SetEdge(Value: TColor);
begin
  if FEdge <> Value then
  begin
    FEdge := Value;
    DoChange;
  end;
end;

procedure TBCEditorRightMarginColors.SetMovingEdge(Value: TColor);
begin
  if FMovingEdge <> Value then
  begin
    FMovingEdge := Value;
    DoChange;
  end;
end;

end.

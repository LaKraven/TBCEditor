unit BCEditor.Editor.LeftMargin.Border;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorLeftMarginBorderStyle = (mbsNone, mbsMiddle, mbsRight);

  TBCEditorLeftMarginBorder = class(TPersistent)
  strict private
    FColor: TColor;
    FStyle: TBCEditorLeftMarginBorderStyle;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetStyle(const Value: TBCEditorLeftMarginBorderStyle);
    procedure DoChange;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clWindow;
    property Style: TBCEditorLeftMarginBorderStyle read FStyle write SetStyle default mbsNone;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

constructor TBCEditorLeftMarginBorder.Create;
begin
  inherited;

  FColor := clWindow;
  FStyle := mbsNone;
end;

procedure TBCEditorLeftMarginBorder.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMarginBorder.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorLeftMarginBorder) then
  with Source as TBCEditorLeftMarginBorder do
  begin
    Self.FColor := FColor;
    Self.FStyle := FStyle;
    Self.DoChange;
  end
  else
    inherited;
end;

procedure TBCEditorLeftMarginBorder.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    DoChange
  end;
end;

procedure TBCEditorLeftMarginBorder.SetStyle(const Value: TBCEditorLeftMarginBorderStyle);
begin
  FStyle := Value;
  DoChange
end;

end.

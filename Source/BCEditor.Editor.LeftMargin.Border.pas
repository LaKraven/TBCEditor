unit BCEditor.Editor.LeftMargin.Border;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorLeftMarginBorderStyle = (mbsNone, mbsMiddle, mbsRight);

  TBCEditorLeftMarginBorder = class(TPersistent)
  strict private
    FStyle: TBCEditorLeftMarginBorderStyle;
    FOnChange: TNotifyEvent;
    procedure SetStyle(const Value: TBCEditorLeftMarginBorderStyle);
    procedure DoChange;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Style: TBCEditorLeftMarginBorderStyle read FStyle write SetStyle default mbsNone;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

constructor TBCEditorLeftMarginBorder.Create;
begin
  inherited;

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
    Self.FStyle := FStyle;
    Self.DoChange;
  end
  else
    inherited;
end;

procedure TBCEditorLeftMarginBorder.SetStyle(const Value: TBCEditorLeftMarginBorderStyle);
begin
  FStyle := Value;
  DoChange
end;

end.

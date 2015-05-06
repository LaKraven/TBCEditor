unit BCEditor.Editor.LeftMargin.LineState.Colors;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorLeftMarginLineStateColors = class(TPersistent)
  strict private
    FModified: TColor;
    FNormal: TColor;
    FOnChange: TNotifyEvent;
    procedure DoChange;
    procedure SetModified(const Value: TColor);
    procedure SetNormal(const Value: TColor);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Modified: TColor read FModified write SetModified default clYellow;
    property Normal: TColor read FNormal write SetNormal default clLime;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TBCEditorLeftMarginLineStateColors }

constructor TBCEditorLeftMarginLineStateColors.Create;
begin
  inherited;

  FModified := clYellow;
  FNormal := clLime;
end;

procedure TBCEditorLeftMarginLineStateColors.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorLeftMarginLineStateColors) then
  with Source as TBCEditorLeftMarginLineStateColors do
  begin
    Self.FModified := FModified;
    Self.FNormal := FNormal;
    Self.DoChange;
  end
  else
    inherited;
end;

procedure TBCEditorLeftMarginLineStateColors.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMarginLineStateColors.SetModified(const Value: TColor);
begin
  if FModified <> Value then
  begin
    FModified := Value;
    DoChange
  end;
end;

procedure TBCEditorLeftMarginLineStateColors.SetNormal(const Value: TColor);
begin
  if FNormal <> Value then
  begin
    FNormal := Value;
    DoChange
  end;
end;

end.

unit BCEditor.Editor.SpecialChars.EndOfLine;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Types;

type
  TBCEditorSpecialCharsEndOfLine = class(TPersistent)
  strict private
    FColor: TColor;
    FOnChange: TNotifyEvent;
    FStyle: TBCEditorSpecialCharsEndOfLineStyle;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetColor(const Value: TColor);
    procedure SetStyle(const Value: TBCEditorSpecialCharsEndOfLineStyle);
    procedure SetVisible(const Value: Boolean);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Style: TBCEditorSpecialCharsEndOfLineStyle read FStyle write SetStyle default eolArrow;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

implementation

{ TBCEditorSpecialCharsEndOfLine }

constructor TBCEditorSpecialCharsEndOfLine.Create;
begin
  inherited;

  FColor := clBlack;
  FStyle := eolArrow;
  FVisible := False;
end;

procedure TBCEditorSpecialCharsEndOfLine.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorSpecialCharsEndOfLine) then
  with Source as TBCEditorSpecialCharsEndOfLine do
  begin
    Self.FColor := FColor;
    Self.FStyle := FStyle;
    Self.FVisible := FVisible;
    Self.DoChange;
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorSpecialCharsEndOfLine.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorSpecialCharsEndOfLine.SetColor(const Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    DoChange;
  end;
end;

procedure TBCEditorSpecialCharsEndOfLine.SetStyle(const Value: TBCEditorSpecialCharsEndOfLineStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    DoChange;
  end;
end;

procedure TBCEditorSpecialCharsEndOfLine.SetVisible(const Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    DoChange;
  end;
end;

end.

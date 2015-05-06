unit BCEditor.Editor.SpecialChars.Selection;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorSpecialCharsSelection = class(TPersistent)
  strict private
    FColor: TColor;
    FOnChange: TNotifyEvent;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetColor(const Value: TColor);
    procedure SetVisible(const Value: Boolean);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

implementation

{ TBCEditorSpecialCharsSelection }

constructor TBCEditorSpecialCharsSelection.Create;
begin
  inherited;

  FColor := clBlack;
  FVisible := False;
end;

procedure TBCEditorSpecialCharsSelection.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorSpecialCharsSelection) then
  with Source as TBCEditorSpecialCharsSelection do
  begin
    Self.FColor := FColor;
    Self.FVisible := FVisible;
    Self.DoChange;
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorSpecialCharsSelection.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorSpecialCharsSelection.SetColor(const Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    DoChange;
  end;
end;

procedure TBCEditorSpecialCharsSelection.SetVisible(const Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    DoChange;
  end;
end;

end.

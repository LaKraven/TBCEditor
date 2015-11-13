unit BCEditor.Editor.Caret.NonBlinking;

interface

uses
  System.Classes, BCEditor.Editor.Caret.NonBlinking.Colors;

type
  TBCEditorCaretNonBlinking = class(TPersistent)
  strict private
    FColors: TBCEditorCaretNonBlinkingColors;
    FEnabled: Boolean;
    FOnChange: TNotifyEvent;
    procedure DoChange;
    procedure SetColors(Value: TBCEditorCaretNonBlinkingColors);
    procedure SetEnabled(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Colors: TBCEditorCaretNonBlinkingColors read FColors write SetColors;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

constructor TBCEditorCaretNonBlinking.Create;
begin
  inherited;

  FColors := TBCEditorCaretNonBlinkingColors.Create;
  FEnabled := False;
end;

destructor TBCEditorCaretNonBlinking.Destroy;
begin
  FColors.Free;

  inherited;
end;

procedure TBCEditorCaretNonBlinking.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorCaretNonBlinking) then
  with Source as TBCEditorCaretNonBlinking do
  begin
    Self.FColors.Assign(FColors);
    Self.FEnabled := FEnabled;
    Self.DoChange;
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorCaretNonBlinking.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorCaretNonBlinking.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    DoChange;
  end;
end;

procedure TBCEditorCaretNonBlinking.SetColors(Value: TBCEditorCaretNonBlinkingColors);
begin
  FColors.Assign(Value);
end;

end.

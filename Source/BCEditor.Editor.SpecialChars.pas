unit BCEditor.Editor.SpecialChars;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Editor.SpecialChars.Selection, BCEditor.Editor.SpecialChars.EndOfLine,
  BCEditor.Types;

type
  TBCEditorSpecialChars = class(TPersistent)
  strict private
    FColor: TColor;
    FEndOfLine: TBCEditorSpecialCharsEndOfLine;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorSpecialCharsOptions;
    FSelection: TBCEditorSpecialCharsSelection;
    FStyle: TBCEditorSpecialCharsStyle;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetColor(const Value: TColor);
    procedure SetEndOfLine(const Value: TBCEditorSpecialCharsEndOfLine);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetOptions(const Value: TBCEditorSpecialCharsOptions);
    procedure SetSelection(const Value: TBCEditorSpecialCharsSelection);
    procedure SetStyle(const Value: TBCEditorSpecialCharsStyle);
    procedure SetVisible(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property EndOfLine: TBCEditorSpecialCharsEndOfLine read FEndOfLine write SetEndOfLine;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Options: TBCEditorSpecialCharsOptions read FOptions write SetOptions default [scoUseTextColor];
    property Selection: TBCEditorSpecialCharsSelection read FSelection write SetSelection;
    property Style: TBCEditorSpecialCharsStyle read FStyle write SetStyle;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;

implementation

{ TBCEditorSpecialChars }

constructor TBCEditorSpecialChars.Create;
begin
  inherited;

  FColor := clBlack;
  FEndOfLine := TBCEditorSpecialCharsEndOfLine.Create;
  FSelection := TBCEditorSpecialCharsSelection.Create;
  FVisible := False;
  FOptions := [scoUseTextColor];
end;

destructor TBCEditorSpecialChars.Destroy;
begin
  FEndOfLine.Free;
  FSelection.Free;
  inherited Destroy;
end;

procedure TBCEditorSpecialChars.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
  FEndOfLine.OnChange := FOnChange;
  FSelection.OnChange := FOnChange;
end;

procedure TBCEditorSpecialChars.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorSpecialChars) then
  with Source as TBCEditorSpecialChars do
  begin
    Self.FColor := FColor;
    Self.FEndOfLine.Assign(FEndOfLine);
    Self.FOptions := FOptions;
    Self.FSelection.Assign(FSelection);
    Self.FVisible := FVisible;
    Self.DoChange;
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorSpecialChars.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorSpecialChars.SetColor(const Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    DoChange;
  end;
end;

procedure TBCEditorSpecialChars.SetEndOfLine(const Value: TBCEditorSpecialCharsEndOfLine);
begin
  FEndOfLine.Assign(Value);
end;

procedure TBCEditorSpecialChars.SetSelection(const Value: TBCEditorSpecialCharsSelection);
begin
  FSelection.Assign(Value);
end;

procedure TBCEditorSpecialChars.SetVisible(const Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    DoChange;
  end;
end;

procedure TBCEditorSpecialChars.SetStyle(const Value: TBCEditorSpecialCharsStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    DoChange;
  end;
end;

procedure TBCEditorSpecialChars.SetOptions(const Value: TBCEditorSpecialCharsOptions);
begin
  if Value <> FOptions then
  begin
    FOptions := Value;
    DoChange;
  end;
end;

end.

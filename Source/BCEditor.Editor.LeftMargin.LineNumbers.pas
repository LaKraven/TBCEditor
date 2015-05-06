unit BCEditor.Editor.LeftMargin.LineNumbers;

interface

uses
  System.Classes, BCEditor.Types;

type
  TBCEditorLeftMarginLineNumbers = class(TPersistent)
  strict private
    FAutosizeDigitCount: Integer;
    FDigitCount: Integer;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorLeftMarginLineNumberOptions;
    FStartFrom: Integer;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetDigitCount(Value: Integer);
    procedure SetOptions(const Value: TBCEditorLeftMarginLineNumberOptions);
    procedure SetStartFrom(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property AutosizeDigitCount: Integer read FAutosizeDigitCount write FAutosizeDigitCount;
  published
    property DigitCount: Integer read FDigitCount write SetDigitCount default 4;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Options: TBCEditorLeftMarginLineNumberOptions read FOptions write SetOptions default [lnoIntens];
    property StartFrom: Integer read FStartFrom write SetStartFrom default 1;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

implementation

uses
  BCEditor.Utils, System.Math;

const
  MIN_DIGIT_COUNT = 2;
  MAX_DIGIT_COUNT = 12;

{ TBCEditorLeftMarginLineNumbers }

constructor TBCEditorLeftMarginLineNumbers.Create;
begin
  inherited;

  FAutosizeDigitCount := 4;
  FDigitCount := 4;
  FOptions := [lnoIntens];
  FStartFrom := 1;
  FVisible := True;
end;

procedure TBCEditorLeftMarginLineNumbers.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorLeftMarginLineNumbers) then
  with Source as TBCEditorLeftMarginLineNumbers do
  begin
    Self.FAutosizeDigitCount := FAutosizeDigitCount;
    Self.FDigitCount := FDigitCount;
    Self.FOptions := FOptions;
    Self.FStartFrom := FStartFrom;
    Self.FVisible := FVisible;

    Self.DoChange;
  end
  else
    inherited;
end;

procedure TBCEditorLeftMarginLineNumbers.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMarginLineNumbers.SetDigitCount(Value: Integer);
begin
  Value := MinMax(Value, MIN_DIGIT_COUNT, MAX_DIGIT_COUNT);
  if FDigitCount <> Value then
  begin
    FDigitCount := Value;
    FAutosizeDigitCount := FDigitCount;
    DoChange
  end;
end;

procedure TBCEditorLeftMarginLineNumbers.SetOptions(const Value: TBCEditorLeftMarginLineNumberOptions);
begin
  if Value <> FOptions then
  begin
    FOptions := Value;
    DoChange
  end;
end;

procedure TBCEditorLeftMarginLineNumbers.SetStartFrom(const Value: Integer);
begin
  if Value <> FStartFrom then
  begin
    FStartFrom := Value;
    if FStartFrom < 0 then
      FStartFrom := 0;
    DoChange
  end;
end;

procedure TBCEditorLeftMarginLineNumbers.SetVisible(const Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    DoChange
  end;
end;

end.

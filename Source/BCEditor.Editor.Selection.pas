unit BCEditor.Editor.Selection;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Editor.Selection.Colors, BCEditor.Types;

type
  TBCEditorSelection = class(TPersistent)
  strict private
    FActiveMode: TBCEditorSelectionMode;
    FColors: TBCEditorSelectedColor;
    FMode: TBCEditorSelectionMode;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorSelectionOptions;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetActiveMode(const Value: TBCEditorSelectionMode);
    procedure SetColors(const Value: TBCEditorSelectedColor);
    procedure SetMode(const Value: TBCEditorSelectionMode);
    procedure SetOnChange(Value: TNotifyEvent);
    procedure SetOptions(Value: TBCEditorSelectionOptions);
    procedure SetVisible(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property ActiveMode: TBCEditorSelectionMode read FActiveMode write SetActiveMode stored False;
  published
    property Colors: TBCEditorSelectedColor read FColors write SetColors;
    property Mode: TBCEditorSelectionMode read FMode write SetMode default smNormal;
    property Options: TBCEditorSelectionOptions read FOptions write SetOptions default [];
    property Visible: Boolean read FVisible write SetVisible default True;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

implementation

{ TBCEditorSelection }

constructor TBCEditorSelection.Create;
begin
  inherited;

  FColors := TBCEditorSelectedColor.Create;
  FActiveMode := smNormal;
  FMode := smNormal;
  FOptions := [];
  FVisible := True;
end;

destructor TBCEditorSelection.Destroy;
begin
  FColors.Free;
  inherited Destroy;
end;

procedure TBCEditorSelection.SetOnChange(Value: TNotifyEvent);
begin
  FOnChange := Value;
  FColors.OnChange := FOnChange;
end;

procedure TBCEditorSelection.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorSelection) then
  with Source as TBCEditorSelection do
  begin
    Self.FColors.Assign(FColors);
    Self.FActiveMode := FActiveMode;
    Self.FMode := FMode;
    Self.FOptions := FOptions;
    Self.FVisible := FVisible;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorSelection.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorSelection.SetColors(const Value: TBCEditorSelectedColor);
begin
  FColors.Assign(Value);
end;

procedure TBCEditorSelection.SetMode(const Value: TBCEditorSelectionMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    ActiveMode := Value;
    DoChange;
  end;
end;

procedure TBCEditorSelection.SetActiveMode(const Value: TBCEditorSelectionMode);
begin
  if FActiveMode <> Value then
  begin
    FActiveMode := Value;
    DoChange;
  end;
end;

procedure TBCEditorSelection.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    DoChange;
  end;
end;

procedure TBCEditorSelection.SetOptions(Value: TBCEditorSelectionOptions);
begin
  if (Value <> FOptions) then
  begin
    FOptions := Value;
    DoChange;
  end;
end;

end.

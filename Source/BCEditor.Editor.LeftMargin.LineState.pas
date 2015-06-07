unit BCEditor.Editor.LeftMargin.LineState;

interface

uses
  System.Classes;

type
  TBCEditorLeftMarginLineState = class(TPersistent)
  strict private
    FEnabled: Boolean;
    FOnChange: TNotifyEvent;
    FWidth: Integer;
    procedure DoChange;
    procedure SetEnabled(const Value: Boolean);
    procedure SetOnChange(Value: TNotifyEvent);
    procedure SetWidth(const Value: Integer);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Width: Integer read FWidth write SetWidth default 2;
  end;

implementation

{ TBCEditorLeftMarginLineState }

constructor TBCEditorLeftMarginLineState.Create;
begin
  inherited;

  FEnabled := True;
  FWidth := 2;
end;

procedure TBCEditorLeftMarginLineState.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorLeftMarginLineState) then
  with Source as TBCEditorLeftMarginLineState do
  begin
    Self.FEnabled := FEnabled;
    Self.FWidth := FWidth;
    Self.DoChange;
  end
  else
    inherited;
end;

procedure TBCEditorLeftMarginLineState.SetOnChange(Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TBCEditorLeftMarginLineState.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMarginLineState.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    DoChange
  end;
end;

procedure TBCEditorLeftMarginLineState.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    DoChange
  end;
end;

end.

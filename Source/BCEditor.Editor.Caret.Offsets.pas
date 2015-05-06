unit BCEditor.Editor.Caret.Offsets;

interface

uses
  System.Classes;

type
  TBCEditorCaretOffsets = class(TPersistent)
  strict private
    FX: Integer;
    FY: Integer;
    FOnChange: TNotifyEvent;
    procedure DoChange(Sender: TObject);
    procedure SetX(Value: Integer);
    procedure SetY(Value: Integer);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property X: Integer read FX write SetX default 0;
    property Y: Integer read FY write SetY default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TBCEditorCaretOffsets }

constructor TBCEditorCaretOffsets.Create;
begin
  inherited;

  FX := 0;
  FY := 0;
end;

procedure TBCEditorCaretOffsets.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorCaretOffsets) then
  with Source as TBCEditorCaretOffsets do
  begin
    Self.FX := FX;
    Self.FY := FY;
    Self.DoChange(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorCaretOffsets.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Sender);
end;

procedure TBCEditorCaretOffsets.SetX(Value: Integer);
begin
  if Value <> FX then
  begin
    FX := Value;
    DoChange(Self);
  end;
end;

procedure TBCEditorCaretOffsets.SetY(Value: Integer);
begin
  if Value <> FY then
  begin
    FY := Value;
    DoChange(Self);
  end;
end;

end.

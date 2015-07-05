unit BCEditor.Editor.Undo;

interface

uses
  System.Classes, BCEditor.Consts, BCEditor.Types;

type
  TBCEditorUndo = class(TPersistent)
  strict private
    FMaxActions: Integer;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorUndoOptions;
    procedure DoChange;
    procedure SetMaxActions(Value: Integer);
    procedure SetOptions(const Value: TBCEditorUndoOptions);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property MaxActions: Integer read FMaxActions write SetMaxActions default BCEDITOR_MAX_UNDO_ACTIONS;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Options: TBCEditorUndoOptions read FOptions write SetOptions default [uoGroupUndo];
  end;

implementation

constructor TBCEditorUndo.Create;
begin
  inherited;

  FMaxActions := BCEDITOR_MAX_UNDO_ACTIONS;
  FOptions := [uoGroupUndo];
end;

procedure TBCEditorUndo.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorUndo.Assign(Source: TPersistent);
begin
  if Source is TBCEditorUndo then
  with Source as TBCEditorUndo do
  begin
    Self.FMaxActions := FMaxActions;
    Self.FOptions := FOptions;
    Self.DoChange;
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorUndo.SetMaxActions(Value: Integer);
begin
  if Value <> FMaxActions then
  begin
    FMaxActions := Value;
    DoChange;
  end;
end;

procedure TBCEditorUndo.SetOptions(const Value: TBCEditorUndoOptions);
begin
  if Value <> FOptions then
  begin
    FOptions := Value;
    DoChange;
  end;
end;

end.

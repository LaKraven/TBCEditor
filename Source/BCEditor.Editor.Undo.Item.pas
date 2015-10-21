unit BCEditor.Editor.Undo.Item;

interface

uses
  System.Classes, BCEditor.Types;

type
  TBCEditorUndoItem = class(TPersistent)
  protected
    FChangeCaretPosition: TBCEditorTextPosition;
    FChangeData: Pointer;
    FChangeEndPosition: TBCEditorTextPosition;
    FChangeNumber: Integer;
    FChangeReason: TBCEditorChangeReason;
    FChangeSelectionMode: TBCEditorSelectionMode;
    FChangeBeginPosition: TBCEditorTextPosition;
    FChangeString: string;
  public
    procedure Assign(Source: TPersistent); override;

    property ChangeCaretPosition: TBCEditorTextPosition read FChangeCaretPosition write FChangeCaretPosition;
    property ChangeData: Pointer read FChangeData write FChangeData;
    property ChangeEndPosition: TBCEditorTextPosition read FChangeEndPosition write FChangeEndPosition;
    property ChangeNumber: Integer read FChangeNumber write FChangeNumber;
    property ChangeReason: TBCEditorChangeReason read FChangeReason write FChangeReason;
    property ChangeSelectionMode: TBCEditorSelectionMode read FChangeSelectionMode write FChangeSelectionMode;
    property ChangeBeginPosition: TBCEditorTextPosition read FChangeBeginPosition write FChangeBeginPosition;
    property ChangeString: string read FChangeString write FChangeString;
  end;

implementation

{ TBCEditorUndoItem }

procedure TBCEditorUndoItem.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorUndoItem) then
  with Source as TBCEditorUndoItem do
  begin
    Self.FChangeReason := FChangeReason;
    Self.FChangeSelectionMode := FChangeSelectionMode;
    Self.FChangeBeginPosition := FChangeBeginPosition;
    Self.FChangeEndPosition := FChangeEndPosition;
    Self.FChangeString := FChangeString;
    Self.FChangeNumber := FChangeNumber;
  end
  else
    inherited Assign(Source);
end;

end.

unit BCEditor.Editor.Undo.Item;

interface

uses
  System.Classes, BCEditor.Types;

type
  TBCEditorChangeReason = (crInsert, crPaste, crDragDropInsert, crDeleteAfterCursor, crDelete,
    crLineBreak, crLineInsert, crIndent, crUnindent, crSilentDelete, crSilentDeleteAfterCursor, crAutoCompleteBegin,
    crAutoCompleteEnd, crCaret, crSelection, crNothing, crGroupBreak, crDeleteAll, crWhiteSpaceAdd,
    crDeleteCollapsedFold, crCollapseFold, crUncollapseFold);

  TBCEditorUndoItem = class(TPersistent)
  protected
    FChangeCaret: Integer;
    FChangeData: Pointer;
    FChangeEndPosition: TBCEditorTextPosition;
    FChangeIndex: Integer;
    FChangeNumber: Integer;
    FChangeReason: TBCEditorChangeReason;
    FChangeSelectionMode: TBCEditorSelectionMode;
    FChangeStartPosition: TBCEditorTextPosition;
    FChangeString: string;
  public
    procedure Assign(Source: TPersistent); override;

    property ChangeCaret: Integer read FChangeCaret write FChangeCaret;
    property ChangeData: Pointer read FChangeData write FChangeData;
    property ChangeEndPosition: TBCEditorTextPosition read FChangeEndPosition write FChangeEndPosition;
    property ChangeIndex: Integer read FChangeIndex write FChangeIndex;
    property ChangeNumber: Integer read FChangeNumber write FChangeNumber;
    property ChangeReason: TBCEditorChangeReason read FChangeReason write FChangeReason;
    property ChangeSelectionMode: TBCEditorSelectionMode read FChangeSelectionMode write FChangeSelectionMode;
    property ChangeStartPosition: TBCEditorTextPosition read FChangeStartPosition write FChangeStartPosition;
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
    Self.FChangeStartPosition := FChangeStartPosition;
    Self.FChangeEndPosition := FChangeEndPosition;
    Self.FChangeString := FChangeString;
    Self.FChangeNumber := FChangeNumber;
  end
  else
    inherited Assign(Source);
end;

end.

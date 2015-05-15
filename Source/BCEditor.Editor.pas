unit BCEditor.Editor;

interface

uses
  BCEditor.Editor.Base, BCEditor.MacroRecorder, BCEditor.Editor.KeyCommands;

type
  TBCCustomEditor = class(TBCBaseEditor)
  strict private
    FDocumentName: string;
    FFileDateTime: TDateTime;
    FSearchString: string;
    FMacroRecorder: TBCEditorMacroRecorder;
  protected
    procedure DoOnProcessCommand(var Command: TBCEditorCommand; var AChar: Char; Data: Pointer); override;
  public
    property DocumentName: string read FDocumentName write FDocumentName;
    property FileDateTime: TDateTime read FFileDateTime write FFileDateTime;
    property MacroRecorder: TBCEditorMacroRecorder read FMacroRecorder write FMacroRecorder;
    property SearchString: string read FSearchString write FSearchString;
  end;

  TBCEditor = class(TBCCustomEditor)
  published
    property ActiveLine;
    property Align;
    property Anchors;
    property BackgroundColor;
    property BorderStyle;
    property Caret;
    property CodeFolding;
    property CompletionProposal;
    property Constraints;
    property Ctl3D;
    property Directories;
    property Enabled;
    property Font;
    property Height;
    property ImeMode;
    property ImeName;
    property InsertMode;
    property KeyCommands;
    property LeftMargin;
    property Lines;
    property LineSpacing;
    property MatchingPair;
    property Minimap;
    property Name;
    property OnBookmarkPanelAfterPaint;
    property OnBookmarkPanelBeforePaint;
    property OnBookmarkPanelLinePaint;
    property OnCaretChanged;
    property OnChange;
    property OnClearBookmark;
    property OnClick;
    property OnCommandProcessed;
    property OnContextHelp;
    property OnCustomLineColors;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropFiles;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLeftMarginClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnPlaceBookmark;
    property OnProcessCommand;
    property OnProcessUserCommand;
    property OnReplaceText;
    property OnRightMarginMouseUp;
    property OnScroll;
    property OnSelectionChanged;
    property OnStartDock;
    property OnStartDrag;
    property Options;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property Replace;
    property RightMargin;
    property Scroll;
    property Search;
    property Selection;
    property ShowHint;
    property SpecialChars;
    property TabOrder;
    property Tabs;
    property TabStop;
    property Tag;
    property Undo;
    property WantReturns;
    property Width;
    property Visible;
    property WordWrap;
  end;

implementation

uses
  Winapi.Windows, System.Classes;

procedure TBCCustomEditor.DoOnProcessCommand(var Command: TBCEditorCommand; var AChar: Char; Data: Pointer);
begin
  inherited;
  if Assigned(FMacroRecorder) then
    if FMacroRecorder.State = msRecording then
      FMacroRecorder.AddEvent(Command, AChar, Data);
end;

end.

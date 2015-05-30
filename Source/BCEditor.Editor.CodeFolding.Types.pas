unit BCEditor.Editor.CodeFolding.Types;

interface

type
  TBCEditorCodeFoldingMarkStyle = (msSquare, msCircle);
  TBCEditorCodeFoldingChanges = (fcEnabled, fcRefresh, fcRescan);

  TBCEditorCodeFoldingChangeEvent = procedure(Event: TBCEditorCodeFoldingChanges) of object;

  TBCEditorCodeFoldingOption = (
    cfoFoldMultilineComments,
    cfoHighlightFoldingLine,
    cfoHighlightIndentGuides,
    cfoHighlightMatchingPair,
    cfoShowCollapsedCodeHint,
    cfoShowCollapsedLine,
    cfoShowIndentGuides,
    cfoUncollapseByHintClick
  );
  TBCEditorCodeFoldingOptions = set of TBCEditorCodeFoldingOption;

implementation

end.

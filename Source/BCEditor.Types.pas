unit BCEditor.Types;

interface

uses
  Winapi.Windows, System.Classes, Vcl.Forms, Vcl.Graphics, Vcl.Controls, BCEditor.Highlighter.Attributes, System.SysUtils;

type
  TBCEditorCharMethod = function(AChar: Char): Boolean of object;

  TBCEditorCaretStyle = (csVerticalLine, csThinVerticalLine, csHorizontalLine, csThinHorizontalLine, csHalfBlock, csBlock);

  TBCEditorDropFilesEvent = procedure(Sender: TObject; X, Y: Integer; AFiles: TStrings) of object;

  TBCEditorPaintEvent = procedure(Sender: TObject; ACanvas: TCanvas) of object;

  TBCEditorReplaceAction = (raCancel, raSkip, raReplace, raReplaceAll);

  TBCEditorReplaceTextEvent = procedure(Sender: TObject; const ASearch, AReplace: string; ALine, AColumn: Integer;
    ADeleteLine: Boolean; var AAction: TBCEditorReplaceAction) of object;

  TBCEditorScrollEvent = procedure(Sender: TObject; ScrollBar: TScrollBarKind) of object;

  TBCEditorCaretChangedEvent = procedure(Sender: TObject; X, Y: Integer) of object;

  TBCEditorBookmarkPanelPaintEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect; AFirstLine: Integer; ALastLine: Integer) of object;
  TBCEditorBookmarkPanelLinePaintEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect; ALineNumber: Integer) of object;

  TBCEditorCustomLineColorsEvent = procedure(Sender: TObject; ALine: Integer; var AUseColors: Boolean;
    var AForeground: TColor; var ABackground: TColor) of object;

  TBCEditorStateFlag = (sfCaretChanged, sfScrollBarChanged, sfLinesChanging, sfIgnoreNextChar, sfCaretVisible, sfDblClicked,
    sfWaitForDragging, sfCodeFoldingInfoClicked, sfInSelection);
  TBCEditorStateFlags = set of TBCEditorStateFlag;

  TBCEditorOption = (
    eoAutoIndent, { Will indent the caret on new lines with the same amount of leading white space as the preceding line }
    eoDragDropEditing, { Allows you to select a block of text and drag it within the document to another location }
    eoDropFiles, { Allows the editor accept OLE file drops }
    eoTrimTrailingSpaces { Spaces at the end of lines will be trimmed and not saved }
  );
  TBCEditorOptions = set of TBCEditorOption;

  TBCEditorCaretOption = (
    coRightMouseClickMovesCaret { When clicking with the right mouse for a popup menu, move the cursor to that location }
  );
  TBCEditorCaretOptions = set of TBCEditorCaretOption;

  TBCEditorScrollOption = (
    soAutosizeMaxWidth, { Automatically resizes the MaxScrollWidth property when inserting text }
    soHalfPage, { When scrolling with page-up and page-down commands, only scroll a half page at a time }
    soHintFollows, { The scroll hint follows the mouse when scrolling vertically }
    soPastEndOfFileMarker, { Allows the cursor to go past the end of file marker }
    soPastEndOfLine, { Allows the cursor to go past the last character into the white space at the end of a line }
    soShowHint { Shows a hint of the visible line numbers when scrolling vertically }
    );
  TBCEditorScrollOptions = set of TBCEditorScrollOption;

  TBCEditorTabOption = (
    toSelectedBlockIndent,
    toTabsToSpaces
    );
  TBCEditorTabOptions = set of TBCEditorTabOption;

  PBCEditorSelectionMode = ^TBCEditorSelectionMode;
  TBCEditorSelectionMode = (smNormal, smLine, smColumn);

  TBCEditorSelectionOption = (
    soALTSetsColumnMode,
    soHighlightSimilarTerms,
    soToEndOfLine,
    soTripleClickRowSelect
  );
  TBCEditorSelectionOptions = set of TBCEditorSelectionOption;

    TBCEditorSearchChanges = (scRefresh, scSearch, scEngineUpdate);
  TBCEditorSearchChangeEvent = procedure(Event: TBCEditorSearchChanges) of object;

  TBCEditorSearchOption = (
    soBackwards,
    soBeepIfStringNotFound,
    soCaseSensitive,
    soEntireScope,
    soHighlightResults,
    soSearchOnTyping,
    soSelectedOnly,
    soShowStringNotFound,
    soWholeWordsOnly
  );
  TBCEditorSearchOptions = set of TBCEditorSearchOption;

  TBCEditorReplaceOption = (
    roBackwards,
    roCaseSensitive,
    roEntireScope,
    roPrompt,
    roReplaceAll,
    roSelectedOnly,
    roWholeWordsOnly
  );
  TBCEditorReplaceOptions = set of TBCEditorReplaceOption;

  TBCEditorReplaceActionOption = (eraReplace, eraDeleteLine);

  TBCEditorSearchEngine = (
    seNormal,
    seRegularExpression,
    seWildCard
  );

  TBCEditorSearchMapOption = (
    moShowActiveLine
  );
  TBCEditorSearchMapOptions = set of TBCEditorSearchMapOption;

  TBCEditorCompletionProposalOption = (
    cpoCaseSensitive,
    cpoFiltered,
    cpoParseItemsFromText,
    cpoResizeable
  );
  TBCEditorCompletionProposalOptions = set of TBCEditorCompletionProposalOption;

  TBCEditorLeftMarginBookMarkPanelOption = (
    bpoToggleBookmarkByClick
  );
  TBCEditorLeftMarginBookMarkPanelOptions = set of TBCEditorLeftMarginBookMarkPanelOption;

  TBCEditorRightMarginOption = (
    rmoMouseMove,
    rmoShowMovingHint
  );
  TBCEditorRightMarginOptions = set of TBCEditorRightMarginOption;

  TBCEditorTextPosition = record
    Char: Integer;
    Line: Integer;
  end;
  PBCEditorTextPosition = ^TBCEditorTextPosition;

  TBCEditorDisplayPosition = record
    Column: Integer;
    Row: Integer;
  end;

  TBCEditorMatchingPairTokenMatch = record
    Position: TBCEditorTextPosition;
    Token: string;
  end;

  //TBCEditorStartType = (stUnspecified, stAny, stTerm);
  TBCEditorBreakType = (btUnspecified, btAny, btTerm);
  //TBCEditorStartLine = (slNotFirst, slFirst, slFirstNonSpace);
  TBCEditorRangeType = (ttUnspecified, ttAddress, ttChar, ttComment, ttDirective, ttFloat, ttHex, ttMailtoLink,
    ttNumber, ttOctal, ttReservedWord, ttString, ttSymbol, ttWebLink);

  TBCEditorMatchingPairToken = record
    OpenToken: string;
    CloseToken: string;
  end;
  PBCEditorMatchingPairToken = ^TBCEditorMatchingPairToken;

  TBCEditorMatchingTokenResult = (trCloseAndOpenTokenFound, trCloseTokenFound, trNotFound, trOpenTokenFound,
    trOpenAndCloseTokenFound);

  TBCEditorMatchingPairMatch = record
    OpenToken: string;
    CloseToken: string;
    OpenTokenPos: TBCEditorTextPosition;
    CloseTokenPos: TBCEditorTextPosition;
    TokenAttribute: TBCEditorHighlighterAttribute;
  end;

  TBCEditorKeyPressWEvent = procedure(Sender: TObject; var Key: Char) of object;

  TBCEditorContextHelpEvent = procedure(Sender: TObject; Word: string) of object;

  TBCEditorMouseCursorEvent = procedure(Sender: TObject; const aLineCharPos: TBCEditorTextPosition; var aCursor: TCursor) of object;

  TBCEditorCharSet = set of AnsiChar;

  TBCEditorTokenHelper = record
    Position: Integer;
    Length: Integer;
    VisualLength: Integer;
    MaxLength: Integer;
    CharsBefore: Integer;
    Text: string;
    TabString: string;
    Foreground, Background: TColor;
    FontStyle: TFontStyles;
  end;

  TBCEditorCompletionType = (ctNone, ctCode, ctHint);
  TBCEditorSpecialCharsEndOfLineStyle = (eolArrow, eolPilcrow);

  TBCEditorSpecialCharsOption = (scoUseTextColor);
  TBCEditorSpecialCharsOptions = set of TBCEditorSpecialCharsOption;
  TBCEditorSpecialCharsStyle = (scsDot, scsSolid);

  TBCEditorByteArray = array of Byte;
  PBCEditorByteArray = ^TBCEditorByteArray; { Can't use System.SysUtils PByteArray because it isn't dynamic }

  TBCEditorTabConvertProc = function(const Line: string; TabWidth: Integer; var HasTabs: Boolean): string;

  TBCEditorLeftMarginLineNumberOption = (
    lnoIntens,
    lnoLeadingZeros,
    lnoAfterLastLine
  );
  TBCEditorLeftMarginLineNumberOptions = set of TBCEditorLeftMarginLineNumberOption;

  TBCEditorMatchingPairOption = (mpoHighlightAfterToken, mpoHighlightUnmatched);
  TBCEditorMatchingPairOptions = set of TBCEditorMatchingPairOption;

  TBCEditorMinimapOption = (moShowIndentGuides);
  TBCEditorMinimapOptions = set of TBCEditorMinimapOption;

  TBCEditorUndoOption = (uoGroupUndo);
  TBCEditorUndoOptions = set of TBCEditorUndoOption;

  TBCEditorCase = (cNone=-1, cUpper=0, cLower=1, cAlternating=2, cSentence=3, cTitle=4, cOriginal=5);

implementation

end.

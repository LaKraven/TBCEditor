unit BCEditor.Consts;

interface

uses
  BCEditor.Types;

const
  BCEDITOR_WHEEL_DIVISOR = 120;
  BCEDITOR_DEFAULT_OPTIONS = [eoAutoIndent, eoDragDropEditing];
  { Clipboard formats }
  BCEDITOR_CLIPBOARD_FORMAT_BCEDITOR = 'BCEditor Control Block Type';
  BCEDITOR_CLIPBOARD_FORMAT_BORLAND = 'Borland IDE Block Type';
  BCEDITOR_CLIPBOARD_FORMAT_MSDEV = 'MSDEVColumnSelect';
  { Max values }
  BCEDITOR_MAX_BOOKMARKS = 9;
  BCEDITOR_MAX_SCROLL_RANGE = 32767;
  BCEDITOR_MAX_UNDO_ACTIONS = 1024;
  { Characters }
  BCEDITOR_SPECIAL_CHARACTERS = ['À' .. 'Ö', 'Ø' .. 'ö', 'ø' .. 'ÿ'];
  BCEDITOR_NUMBERS = ['0' .. '9'];
  BCEDITOR_UNDERSCORE = ['_'];
  BCEDITOR_STRING_UPPER_CHARACTERS = ['A' .. 'Z'];
  BCEDITOR_STRING_LOWER_CHARACTERS = ['a' .. 'z'];
  BCEDITOR_VALID_STRING_CHARACTERS = BCEDITOR_UNDERSCORE + BCEDITOR_STRING_UPPER_CHARACTERS + BCEDITOR_STRING_LOWER_CHARACTERS;
  BCEDITOR_WORD_BREAK_CHARACTERS = ['.', ',', ';', ':', '"', '''', '!', '?', '[', ']', '(', ')', '{', '}', '^', '-',
    '=', '+', '-', '*', '/', '\', '|', ' '];
  BCEDITOR_DEFAULT_DELIMITERS: TBCEditorCharSet = ['*', '/', '+', '-', '=', '\', '|', '&', '(', ')', '[', ']', '{', '}',
    '`', '~', '!', '@', ',', '$', '%', '^', '?', ':', ';', '''', '"', '.', '>', '<', '#'];
  BCEDITOR_NONE_CHAR = #0;
  BCEDITOR_F11_CHAR = #8;
  BCEDITOR_TAB_CHAR = #9;
  BCEDITOR_LINEFEED = #10;
  BCEDITOR_CARRIAGE_RETURN = #13;
  BCEDITOR_ESCAPE = #27;
  BCEDITOR_SPACE_CHAR = #32;
  BCEDITOR_EXCLAMATION_MARK = #33;
  BCEDITOR_LOW_LINE = #95;
  BCEDITOR_CTRL_BACKSPACE = #127;
  BCEDITOR_NON_BREAKING_SPACE = #255;
  BCEDITOR_LINE_SEPARATOR = Char($2028);
  BCEDITOR_FILLER_CHAR = Char($E000);
  BCEDITOR_ABSOLUTE_DELIMITERS: TBCEditorCharSet = [BCEDITOR_NONE_CHAR, BCEDITOR_TAB_CHAR, BCEDITOR_LINEFEED,
    BCEDITOR_CARRIAGE_RETURN, BCEDITOR_SPACE_CHAR];
  { Encoding }
  UTF8BOM: array [0 .. 2] of Byte = ($EF, $BB, $BF);
  { Highlighter attribute elements }
  BCEDITOR_ATTRIBUTE_ELEMENT_ADDRESS = 'Address';
  BCEDITOR_ATTRIBUTE_ELEMENT_CHARACTER = 'Character';
  BCEDITOR_ATTRIBUTE_ELEMENT_COMMENT = 'Comment';
  BCEDITOR_ATTRIBUTE_ELEMENT_DIRECTIVE = 'Directive';
  BCEDITOR_ATTRIBUTE_ELEMENT_FLOAT = 'Float';
  BCEDITOR_ATTRIBUTE_ELEMENT_HEX = 'Hex';
  BCEDITOR_ATTRIBUTE_ELEMENT_MAIL_TO_LINK = 'MailtoLink';
  BCEDITOR_ATTRIBUTE_ELEMENT_NUMBER = 'Number';
  BCEDITOR_ATTRIBUTE_ELEMENT_OCTAL = 'Octal';
  BCEDITOR_ATTRIBUTE_ELEMENT_RESERVED_WORD = 'ReservedWord';
  BCEDITOR_ATTRIBUTE_ELEMENT_STRING = 'String';
  BCEDITOR_ATTRIBUTE_ELEMENT_SYMBOL = 'Symbol';
  BCEDITOR_ATTRIBUTE_ELEMENT_WEB_LINK = 'WebLink';
  { Colors }
  clSelectionColor = $00A56D53;
  clSearchHighlighter = $0078AAFF;
  clActiveLineBackground = $00E6FAFF;
  clLeftMarginBackground = $00FFFFFF;
  clLeftMarginFontForeground = $00CC9999;
  clLeftMarginBookmarkBackground = $00F4F4F4;
  clIndentHighlight = $00CC9999;
  clIndent = $00CC9999;
  clMinimapVisibleLines = $00E6FAFF;

implementation

end.

unit BCEditor.Editor.KeyCommands;

interface

uses
  System.Classes, System.SysUtils, Vcl.Menus;

const
  ecNone = 0;
  ecEditCommandFirst = 501;
  ecEditCommandLast = 1000;
  { Caret moving }
  ecLeft = 1;
  ecRight = 2;
  ecUp = 3;
  ecDown = 4;
  ecWordLeft = 5;
  ecWordRight = 6;
  ecLineStart = 7;
  ecLineEnd = 8;
  ecPageUp = 9;
  ecPageDown = 10;
  ecPageLeft = 11;
  ecPageRight = 12;
  ecPageTop = 13;
  ecPageBottom = 14;
  ecEditorTop = 15;
  ecEditorBottom = 16;
  ecGotoXY = 17;
  ecOffsetCaret = 18;
  { Selection }
  ecSelection = 100;
  ecSelectionLeft = ecLeft + ecSelection;
  ecSelectionRight = ecRight + ecSelection;
  ecSelectionUp = ecUp + ecSelection;
  ecSelectionDown = ecDown + ecSelection;
  ecSelectionWordLeft = ecWordLeft + ecSelection;
  ecSelectionWordRight = ecWordRight + ecSelection;
  ecSelectionLineStart = ecLineStart + ecSelection;
  ecSelectionLineEnd = ecLineEnd + ecSelection;
  ecSelectionPageUp = ecPageUp + ecSelection;
  ecSelectionPageDown = ecPageDown + ecSelection;
  ecSelectionPageLeft = ecPageLeft + ecSelection;
  ecSelectionPageRight = ecPageRight + ecSelection;
  ecSelectionPageTop = ecPageTop + ecSelection;
  ecSelectionPageBottom = ecPageBottom + ecSelection;
  ecSelectionEditorTop = ecEditorTop + ecSelection;
  ecSelectionEditorBottom = ecEditorBottom + ecSelection;
  ecSelectionGotoXY = ecGotoXY + ecSelection;
  ecSelOffsetCaret = ecOffsetCaret + ecSelection;
  ecSelectionScope = ecSelection + 21;
  ecSelectionWord = ecSelection + 22;
  ecSelectAll = ecSelection + 23;
  { Scrolling }
  ecScrollUp = 211;
  ecScrollDown = 212;
  ecScrollLeft = 213;
  ecScrollRight = 214;
  { Mode }
  ecInsertMode = 221;
  ecOverwriteMode = 222;
  ecToggleMode = 223;
  { Selection modes }
  ecNormalSelect = 231;
  ecColumnSelect = 232;
  ecLineSelect = 233;
  { Bookmark }
  ecGotoBookmark1 = 302;
  ecGotoBookmark2 = 303;
  ecGotoBookmark3 = 304;
  ecGotoBookmark4 = 305;
  ecGotoBookmark5 = 306;
  ecGotoBookmark6 = 307;
  ecGotoBookmark7 = 308;
  ecGotoBookmark8 = 309;
  ecGotoBookmark9 = 310;
  ecSetBookmark1 = 352;
  ecSetBookmark2 = 353;
  ecSetBookmark3 = 354;
  ecSetBookmark4 = 355;
  ecSetBookmark5 = 356;
  ecSetBookmark6 = 357;
  ecSetBookmark7 = 358;
  ecSetBookmark8 = 359;
  ecSetBookmark9 = 360;
  { Focus }
  ecGotFocus = 480;
  ecLostFocus = 481;
  { Help }
  ecContextHelp = 490;
  { Deletion }
  ecDeleteLastChar = 501;
  ecDeleteChar = 502;
  ecDeleteWord = 503;
  ecDeleteLastWord = 504;
  ecDeleteBeginningOfLine = 505;
  ecDeleteEndOfLine = 506;
  ecDeleteLine = 507;
  ecClear = 508;
  { Insert }
  ecLineBreak = 509;
  ecInsertLine = 510;
  ecChar = 511;
  ecString = 512;
  ecImeStr = 550;
  { Clipboard }
  ecUndo = 601;
  ecRedo = 602;
  ecCopy = 603;
  ecCut = 604;
  ecPaste = 605;
  { Indent }
  ecBlockIndent = 610;
  ecBlockUnindent = 611;
  ecTab = 612;
  ecShiftTab = 613;
  { Case }
  ecUpperCase = 620;
  ecLowerCase = 621;
  ecAlternatingCase = 622;
  ecSentenceCase = 623;
  ecTitleCase = 624;
  ecUpperCaseBlock = 625;
  ecLowerCaseBlock = 626;
  ecAlternatingCaseBlock = 627;
  { Move }
  ecMoveLineUp      = 701;
  ecMoveLineDown    = 702;
  ecMoveCharLeft    = 703;
  ecMoveCharRight   = 704;
  { Search }
  ecSearchNext = 800;
  ecSearchPrevious = 801;

  ecUserFirst = 1001;

  ecCollapse = ecUserFirst + 100;
  ecUncollapse = ecUserFirst + 101;
  ecCollapseLevel = ecUserFirst + 102;
  ecUncollapseLevel = ecUserFirst + 103;
  ecCollapseAll = ecUserFirst + 104;
  ecUncollapseAll = ecUserFirst + 105;
  ecCollapseCurrent = ecUserFirst + 109;

type
  TBCEditorCommand = type Word;

  TBCEditorHookedCommandEvent = procedure(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean;
    var Command: TBCEditorCommand; var AChar: Char; Data: Pointer; HandlerData: Pointer) of object;
  TBCEditorProcessCommandEvent = procedure(Sender: TObject; var Command: TBCEditorCommand; var AChar: Char;
    Data: Pointer) of object;

  TBCEditorHookedCommandHandler = class(TObject)
  strict private
    FEvent: TBCEditorHookedCommandEvent;
    FData: Pointer;
  public
    constructor Create(AEvent: TBCEditorHookedCommandEvent; AData: pointer);
    function Equals(AEvent: TBCEditorHookedCommandEvent): Boolean; reintroduce;
    property Data: Pointer read FData write FData;
    property Event: TBCEditorHookedCommandEvent read FEvent write FEvent;
  end;

  TBCEditorKeyCommand = class(TCollectionItem)
  strict private
    FKey: Word;
    FKey2: Word;
    FShiftState: TShiftState;
    FShiftState2: TShiftState;
    FCommand: TBCEditorCommand;
    function GetShortCut: TShortCut;
    function GetShortCut2: TShortCut;
    procedure SetCommand(const Value: TBCEditorCommand);
    procedure SetKey(const Value: Word);
    procedure SetKey2(const Value: Word);
    procedure SetShiftState(const Value: TShiftState);
    procedure SetShiftState2(const Value: TShiftState);
    procedure SetShortCut(const Value: TShortCut);
    procedure SetShortCut2(const Value: TShortCut);
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
    property Key: word read FKey write SetKey;
    property Key2: word read FKey2 write SetKey2;
    property ShiftState: TShiftState read FShiftState write SetShiftState;
    property ShiftState2: TShiftState read FShiftState2 write SetShiftState2;
  published
    property Command: TBCEditorCommand read FCommand write SetCommand;
    property ShortCut: TShortCut read GetShortCut write SetShortCut default 0;
    property ShortCut2: TShortCut read GetShortCut2 write SetShortCut2 default 0;
  end;

  TBCEditorKeyCommands = class(TCollection)
  strict private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TBCEditorKeyCommand;
    procedure SetItem(Index: Integer; Value: TBCEditorKeyCommand);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);

    function Add: TBCEditorKeyCommand;
    function FindCommand(Command: TBCEditorCommand): Integer;
    function FindKeyCode(KeyCode: Word; Shift: TShiftState): Integer;
    function FindKeyCode2(KeyCode1: Word; Shift1: TShiftState; Keycode2: Word; Shift2: TShiftState): Integer;
    function FindShortcut(ShortCut: TShortCut): Integer;
    function FindShortcut2(ShortCut1, ShortCut2: TShortCut): Integer;
    procedure AddKey(const Command: TBCEditorCommand; const Key: Word; const Shift: TShiftState);
    procedure Assign(Source: TPersistent); override;
    procedure ResetDefaults;
  public
    property Items[index: Integer]: TBCEditorKeyCommand read GetItem write SetItem; default;
  end;

function IdentToEditorCommand(const Ident: string; var Command: LongInt): Boolean;
function EditorCommandToIdent(Command: LongInt; var Ident: string): Boolean;

implementation

uses
  Winapi.Windows, BCEditor.Language;

type
  TBCEditorCommandString = record
    Value: TBCEditorCommand;
    Name: string;
  end;

const
  EditorCommandStrings: array [0 .. 109] of TBCEditorCommandString = (
    (Value: ecNone; Name: 'ecNone'),
    (Value: ecLeft; Name: 'ecLeft'),
    (Value: ecRight; Name: 'ecRight'),
    (Value: ecUp; Name: 'ecUp'),
    (Value: ecDown; Name: 'ecDown'),
    (Value: ecWordLeft; Name: 'ecWordLeft'),
    (Value: ecWordRight; Name: 'ecWordRight'),
    (Value: ecLineStart; Name: 'ecLineStart'),
    (Value: ecLineEnd; Name: 'ecLineEnd'),
    (Value: ecPageUp; Name: 'ecPageUp'),
    (Value: ecPageDown; Name: 'ecPageDown'),
    (Value: ecPageLeft; Name: 'ecPageLeft'),
    (Value: ecPageRight; Name: 'ecPageRight'),
    (Value: ecPageTop; Name: 'ecPageTop'),
    (Value: ecPageBottom; Name: 'ecPageBottom'),
    (Value: ecEditorTop; Name: 'ecEditorTop'),
    (Value: ecEditorBottom; Name: 'ecEditorBottom'),
    (Value: ecGotoXY; Name: 'ecGotoXY'),
    (Value: ecSelectionLeft; Name: 'ecSelectionLeft'),
    (Value: ecSelectionRight; Name: 'ecSelectionRight'),
    (Value: ecSelectionUp; Name: 'ecSelectionUp'),
    (Value: ecSelectionDown; Name: 'ecSelectionDown'),
    (Value: ecSelectionWordLeft; Name: 'ecSelectionWordLeft'),
    (Value: ecSelectionWordRight; Name: 'ecSelectionWordRight'),
    (Value: ecSelectionLineStart; Name: 'ecSelectionLineStart'),
    (Value: ecSelectionLineEnd; Name: 'ecSelectionLineEnd'),
    (Value: ecSelectionPageUp; Name: 'ecSelectionPageUp'),
    (Value: ecSelectionPageDown; Name: 'ecSelectionPageDown'),
    (Value: ecSelectionPageLeft; Name: 'ecSelectionPageLeft'),
    (Value: ecSelectionPageRight; Name: 'ecSelectionPageRight'),
    (Value: ecSelectionPageTop; Name: 'ecSelectionPageTop'),
    (Value: ecSelectionPageBottom; Name: 'ecSelectionPageBottom'),
    (Value: ecSelectionEditorTop; Name: 'ecSelectionEditorTop'),
    (Value: ecSelectionEditorBottom; Name: 'ecSelectionEditorBottom'),
    (Value: ecSelectionGotoXY; Name: 'ecSelectionGotoXY'),
    (Value: ecSelectionWord; Name: 'ecSelectionWord'),
    (Value: ecSelectAll; Name: 'ecSelectAll'),
    (Value: ecScrollUp; Name: 'ecScrollUp'),
    (Value: ecScrollDown; Name: 'ecScrollDown'),
    (Value: ecScrollLeft; Name: 'ecScrollLeft'),
    (Value: ecScrollRight; Name: 'ecScrollRight'),
    (Value: ecDeleteLastChar; Name: 'ecDeleteLastChar'),
    (Value: ecDeleteChar; Name: 'ecDeleteChar'),
    (Value: ecDeleteWord; Name: 'ecDeleteWord'),
    (Value: ecDeleteLastWord; Name: 'ecDeleteLastWord'),
    (Value: ecDeleteBeginningOfLine; Name: 'ecDeleteBeginningOfLine'),
    (Value: ecDeleteEndOfLine; Name: 'ecDeleteEndOfLine'),
    (Value: ecDeleteLine; Name: 'ecDeleteLine'),
    (Value: ecClear; Name: 'ecClear'),
    (Value: ecLineBreak; Name: 'ecLineBreak'),
    (Value: ecInsertLine; Name: 'ecInsertLine'),
    (Value: ecChar; Name: 'ecChar'),
    (Value: ecImeStr; Name: 'ecImeStr'),
    (Value: ecUndo; Name: 'ecUndo'),
    (Value: ecRedo; Name: 'ecRedo'),
    (Value: ecCut; Name: 'ecCut'),
    (Value: ecCopy; Name: 'ecCopy'),
    (Value: ecPaste; Name: 'ecPaste'),
    (Value: ecInsertMode; Name: 'ecInsertMode'),
    (Value: ecOverwriteMode; Name: 'ecOverwriteMode'),
    (Value: ecToggleMode; Name: 'ecToggleMode'),
    (Value: ecBlockIndent; Name: 'ecBlockIndent'),
    (Value: ecBlockUnindent; Name: 'ecBlockUnindent'),
    (Value: ecTab; Name: 'ecTab'),
    (Value: ecShiftTab; Name: 'ecShiftTab'),
    (Value: ecNormalSelect; Name: 'ecNormalSelect'),
    (Value: ecColumnSelect; Name: 'ecColumnSelect'),
    (Value: ecLineSelect; Name: 'ecLineSelect'),
    (Value: ecUserFirst; Name: 'ecUserFirst'),
    (Value: ecContextHelp; Name: 'ecContextHelp'),
    (Value: ecGotoBookmark1; Name: 'ecGotoBookmark1'),
    (Value: ecGotoBookmark2; Name: 'ecGotoBookmark2'),
    (Value: ecGotoBookmark3; Name: 'ecGotoBookmark3'),
    (Value: ecGotoBookmark4; Name: 'ecGotoBookmark4'),
    (Value: ecGotoBookmark5; Name: 'ecGotoBookmark5'),
    (Value: ecGotoBookmark6; Name: 'ecGotoBookmark6'),
    (Value: ecGotoBookmark7; Name: 'ecGotoBookmark7'),
    (Value: ecGotoBookmark8; Name: 'ecGotoBookmark8'),
    (Value: ecGotoBookmark9; Name: 'ecGotoBookmark9'),
    (Value: ecSetBookmark1; Name: 'ecSetBookmark1'),
    (Value: ecSetBookmark2; Name: 'ecSetBookmark2'),
    (Value: ecSetBookmark3; Name: 'ecSetBookmark3'),
    (Value: ecSetBookmark4; Name: 'ecSetBookmark4'),
    (Value: ecSetBookmark5; Name: 'ecSetBookmark5'),
    (Value: ecSetBookmark6; Name: 'ecSetBookmark6'),
    (Value: ecSetBookmark7; Name: 'ecSetBookmark7'),
    (Value: ecSetBookmark8; Name: 'ecSetBookmark8'),
    (Value: ecSetBookmark9; Name: 'ecSetBookmark9'),
    (Value: ecString; Name: 'ecString'),
    (Value: ecMoveLineUp; Name: 'ecMoveLineUp'),
    (Value: ecMoveLineDown; Name: 'ecMoveLineDown'),
    (Value: ecMoveCharLeft; Name: 'ecMoveCharLeft'),
    (Value: ecMoveCharRight; Name: 'ecMoveCharRight'),
    (Value: ecUpperCase; Name: 'ecUpperCase'),
    (Value: ecLowerCase; Name: 'ecLowerCase'),
    (Value: ecAlternatingCase; Name: 'ecAlternatingCase'),
    (Value: ecSentenceCase; Name: 'ecSentenceCase'),
    (Value: ecTitleCase; Name: 'ecTitleCase'),
    (Value: ecUpperCaseBlock; Name: 'ecUpperCaseBlock'),
    (Value: ecLowerCaseBlock; Name: 'ecLowerCaseBlock'),
    (Value: ecAlternatingCaseBlock; Name: 'ecAlternatingCaseBlock'),
    (Value: ecCollapse; Name: 'ecCollapse'),
    (Value: ecUncollapse; Name: 'ecUncollapse'),
    (Value: ecCollapseLevel; Name: 'ecCollapseLevel'),
    (Value: ecUncollapseLevel; Name: 'ecUncollapseLevel'),
    (Value: ecCollapseAll; Name: 'ecCollapseAll'),
    (Value: ecUncollapseAll; Name: 'ecUncollapseAll'),
    (Value: ecCollapseCurrent; Name: 'ecCollapseCurrent'),
    (Value: ecSearchNext; Name: 'ecSearchNext'),
    (Value: ecSearchPrevious; Name: 'ecSearchPrevious')//,
  );

function IdentToEditorCommand(const Ident: string; var Command: LongInt): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := Low(EditorCommandStrings) to High(EditorCommandStrings) do
    if CompareText(EditorCommandStrings[i].Name, Ident) = 0 then
    begin
      Command := EditorCommandStrings[i].Value;
      Exit;
    end;
  Result := False;
end;

function EditorCommandToIdent(Command: LongInt; var Ident: string): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := Low(EditorCommandStrings) to High(EditorCommandStrings) do
    if EditorCommandStrings[i].Value = Command then
    begin
      Ident := EditorCommandStrings[i].Name;
      Exit;
    end;
  Result := False;
end;

function EditorCommandToCodeString(Command: TBCEditorCommand): string;
begin
  if not EditorCommandToIdent(Command, Result) then
    Result := IntToStr(Command);
end;

{ TBCEditorHookedCommandHandler }

constructor TBCEditorHookedCommandHandler.Create(AEvent: TBCEditorHookedCommandEvent; AData: pointer);
begin
  inherited Create;
  FEvent := AEvent;
  FData := AData;
end;

function TBCEditorHookedCommandHandler.Equals(AEvent: TBCEditorHookedCommandEvent): Boolean;
begin
  Result := (TMethod(FEvent).Code = TMethod(AEvent).Code) and (TMethod(FEvent).Data = TMethod(AEvent).Data);
end;

{ TBCEditorKeyCommand }

procedure TBCEditorKeyCommand.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorKeyCommand) then
  with Source as TBCEditorKeyCommand do
  begin
    Self.FCommand := FCommand;
    Self.FKey := FKey;
    Self.FKey2 := FKey2;
    Self.FShiftState := FShiftState;
    Self.FShiftState2 := FShiftState2;
  end
  else
    inherited Assign(Source);
end;

function TBCEditorKeyCommand.GetDisplayName: string;
begin
  Result := Format('%s - %s', [EditorCommandToCodeString(Command), ShortCutToText(ShortCut)]);
  if ShortCut <> 0 then
    Result := Result + ' ' + ShortCutToText(ShortCut2);
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TBCEditorKeyCommand.GetShortCut: TShortCut;
begin
  Result := Vcl.Menus.ShortCut(Key, ShiftState);
end;

procedure TBCEditorKeyCommand.SetCommand(const Value: TBCEditorCommand);
begin
  if Value <> FCommand then
    FCommand := Value;
end;

procedure TBCEditorKeyCommand.SetKey(const Value: Word);
begin
  if Value <> FKey then
    FKey := Value;
end;

procedure TBCEditorKeyCommand.SetShiftState(const Value: TShiftState);
begin
  if Value <> FShiftState then
    FShiftState := Value;
end;

procedure TBCEditorKeyCommand.SetShortCut(const Value: TShortCut);
var
  NewKey: Word;
  NewShiftState: TShiftState;
  Duplicate: Integer;
begin
  if Value <> 0 then
  begin
    Duplicate := TBCEditorKeyCommands(Collection).FindShortcut2(Value, ShortCut2);
    if (Duplicate <> -1) and (Duplicate <> Self.Index) then
      raise Exception.Create(SBCEditorDuplicateShortcut);
  end;

  Vcl.Menus.ShortCutToKey(Value, NewKey, NewShiftState);
  if (NewKey <> Key) or (NewShiftState <> ShiftState) then
  begin
    Key := NewKey;
    ShiftState := NewShiftState;
  end;
end;

procedure TBCEditorKeyCommand.SetKey2(const Value: Word);
begin
  if Value <> FKey2 then
    FKey2 := Value;
end;

procedure TBCEditorKeyCommand.SetShiftState2(const Value: TShiftState);
begin
  if Value <> FShiftState2 then
    FShiftState2 := Value;
end;

procedure TBCEditorKeyCommand.SetShortCut2(const Value: TShortCut);
var
  NewKey: Word;
  NewShiftState: TShiftState;
  Dup: Integer;
begin
  if Value <> 0 then
  begin
    Dup := TBCEditorKeyCommands(Collection).FindShortcut2(ShortCut, Value);
    if (Dup <> -1) and (Dup <> Self.Index) then
      raise Exception.Create(SBCEditorDuplicateShortcut);
  end;

  Vcl.Menus.ShortCutToKey(Value, NewKey, NewShiftState);
  if (NewKey <> Key2) or (NewShiftState <> ShiftState2) then
  begin
    Key2 := NewKey;
    ShiftState2 := NewShiftState;
  end;
end;

function TBCEditorKeyCommand.GetShortCut2: TShortCut;
begin
  Result := Vcl.Menus.ShortCut(Key2, ShiftState2);
end;

{ TBCEditorKeyCommands }

function TBCEditorKeyCommands.Add: TBCEditorKeyCommand;
begin
  Result := TBCEditorKeyCommand(inherited Add);
end;

procedure TBCEditorKeyCommands.AddKey(const Command: TBCEditorCommand; const Key: Word; const Shift: TShiftState);
var
  NewKeystroke: TBCEditorKeyCommand;
begin
  NewKeystroke := Add;
  NewKeystroke.Key := Key;
  NewKeystroke.ShiftState := Shift;
  NewKeystroke.Command := Command;
end;

procedure TBCEditorKeyCommands.Assign(Source: TPersistent);
var
  i: Integer;
begin
  if Assigned(Source) and (Source is TBCEditorKeyCommands) then
  with Source as TBCEditorKeyCommands do
  begin
    Self.Clear;
    for i := 0 to Count - 1 do
      with Self.Add do
        Assign((Source as TBCEditorKeyCommands)[i]);
  end
  else
    inherited Assign(Source);
end;

constructor TBCEditorKeyCommands.Create(AOwner: TPersistent);
begin
  inherited Create(TBCEditorKeyCommand);
  FOwner := AOwner;
end;

function TBCEditorKeyCommands.FindCommand(Command: TBCEditorCommand): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Items[i].Command = Command then
    begin
      Result := i;
      Break;
    end;
end;

function TBCEditorKeyCommands.FindKeyCode(Keycode: Word; Shift: TShiftState): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if (Items[i].Key = Keycode) and (Items[i].ShiftState = Shift) and (Items[i].Key2 = 0) then
    begin
      Result := i;
      Break;
    end;
end;

function TBCEditorKeyCommands.FindKeyCode2(KeyCode1: Word; Shift1: TShiftState; KeyCode2: Word; Shift2: TShiftState): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if (Items[i].Key = KeyCode1) and (Items[i].ShiftState = Shift1) and (Items[i].Key2 = KeyCode2) and
      (Items[i].ShiftState2 = Shift2) then
    begin
      Result := i;
      Break;
    end;
end;

function TBCEditorKeyCommands.FindShortcut(ShortCut: TShortCut): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Items[i].ShortCut = ShortCut then
    begin
      Result := i;
      Break;
    end;
end;

function TBCEditorKeyCommands.FindShortcut2(ShortCut1, ShortCut2: TShortCut): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if (Items[i].ShortCut = ShortCut1) and (Items[i].ShortCut2 = ShortCut2) then
    begin
      Result := i;
      break;
    end;
end;

function TBCEditorKeyCommands.GetItem(Index: Integer): TBCEditorKeyCommand;
begin
  Result := TBCEditorKeyCommand(inherited GetItem(index));
end;

function TBCEditorKeyCommands.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TBCEditorKeyCommands.ResetDefaults;
begin
  Clear;

  { Scrolling, caret moving and selection }
  AddKey(ecUp, VK_UP, []);
  AddKey(ecSelectionUp, VK_UP, [ssShift]);
  AddKey(ecScrollUp, VK_UP, [ssCtrl]);
  AddKey(ecDown, VK_DOWN, []);
  AddKey(ecSelectionDown, VK_DOWN, [ssShift]);
  AddKey(ecScrollDown, VK_DOWN, [ssCtrl]);
  AddKey(ecLeft, VK_LEFT, []);
  AddKey(ecSelectionLeft, VK_LEFT, [ssShift]);
  AddKey(ecWordLeft, VK_LEFT, [ssCtrl]);
  AddKey(ecSelectionWordLeft, VK_LEFT, [ssShift, ssCtrl]);
  AddKey(ecRight, VK_RIGHT, []);
  AddKey(ecSelectionRight, VK_RIGHT, [ssShift]);
  AddKey(ecWordRight, VK_RIGHT, [ssCtrl]);
  AddKey(ecSelectionWordRight, VK_RIGHT, [ssShift, ssCtrl]);
  AddKey(ecPageDown, VK_NEXT, []);
  AddKey(ecSelectionPageDown, VK_NEXT, [ssShift]);
  AddKey(ecPageBottom, VK_NEXT, [ssCtrl]);
  AddKey(ecSelectionPageBottom, VK_NEXT, [ssShift, ssCtrl]);
  AddKey(ecPageUp, VK_PRIOR, []);
  AddKey(ecSelectionPageUp, VK_PRIOR, [ssShift]);
  AddKey(ecPageTop, VK_PRIOR, [ssCtrl]);
  AddKey(ecSelectionPageTop, VK_PRIOR, [ssShift, ssCtrl]);
  AddKey(ecLineStart, VK_HOME, []);
  AddKey(ecSelectionLineStart, VK_HOME, [ssShift]);
  AddKey(ecEditorTop, VK_HOME, [ssCtrl]);
  AddKey(ecSelectionEditorTop, VK_HOME, [ssShift, ssCtrl]);
  AddKey(ecLineEnd, VK_END, []);
  AddKey(ecSelectionLineEnd, VK_END, [ssShift]);
  AddKey(ecEditorBottom, VK_END, [ssCtrl]);
  AddKey(ecSelectionEditorBottom, VK_END, [ssShift, ssCtrl]);
  { Insert key alone }
  AddKey(ecToggleMode, VK_INSERT, []);
  { Clipboard }
  AddKey(ecUndo, VK_BACK, [ssAlt]);
  AddKey(ecRedo, VK_BACK, [ssAlt, ssShift]);
  AddKey(ecCopy, VK_INSERT, [ssCtrl]);
  AddKey(ecCut, VK_DELETE, [ssShift]);
  AddKey(ecPaste, VK_INSERT, [ssShift]);
  { Deletion }
  AddKey(ecDeleteChar, VK_DELETE, []);
  AddKey(ecDeleteLastChar, VK_BACK, []);
  AddKey(ecDeleteLastChar, VK_BACK, [ssShift]);
  AddKey(ecDeleteLastWord, VK_BACK, [ssCtrl]);
  { Search }
  AddKey(ecSearchNext, VK_F3, []);
  AddKey(ecSearchPrevious, VK_F3, [ssShift]);
  { Enter (return) & Tab }
  AddKey(ecLineBreak, VK_RETURN, []);
  AddKey(ecLineBreak, VK_RETURN, [ssShift]);
  AddKey(ecTab, VK_TAB, []);
  AddKey(ecShiftTab, VK_TAB, [ssShift]);
  { Help }
  AddKey(ecContextHelp, VK_F1, []);
  { Standard edit commands }
  AddKey(ecUndo, ord('Z'), [ssCtrl]);
  AddKey(ecRedo, ord('Z'), [ssCtrl, ssShift]);
  AddKey(ecCut, ord('X'), [ssCtrl]);
  AddKey(ecCopy, ord('C'), [ssCtrl]);
  AddKey(ecPaste, ord('V'), [ssCtrl]);
  AddKey(ecSelectAll, ord('A'), [ssCtrl]);
  { Block commands }
  AddKey(ecBlockIndent, ord('I'), [ssCtrl, ssShift]);
  AddKey(ecBlockUnindent, ord('U'), [ssCtrl, ssShift]);
  { Fragment deletion }
  AddKey(ecDeleteWord, ord('T'), [ssCtrl]);
  { Line operations }
  AddKey(ecInsertLine, ord('M'), [ssCtrl]);
  AddKey(ecMoveLineUp, VK_UP, [ssCtrl, ssAlt]);
  AddKey(ecMoveLineDown, VK_DOWN, [ssCtrl, ssAlt]);
  AddKey(ecDeleteLine, ord('Y'), [ssCtrl]);
  AddKey(ecDeleteEndOfLine, ord('Y'), [ssCtrl, ssShift]);
  AddKey(ecMoveCharLeft, VK_LEFT, [ssAlt, ssCtrl]);
  AddKey(ecMoveCharRight, VK_RIGHT, [ssAlt, ssCtrl]);
  { Bookmarks }
  AddKey(ecGotoBookmark1, ord('1'), [ssCtrl]);
  AddKey(ecGotoBookmark2, ord('2'), [ssCtrl]);
  AddKey(ecGotoBookmark3, ord('3'), [ssCtrl]);
  AddKey(ecGotoBookmark4, ord('4'), [ssCtrl]);
  AddKey(ecGotoBookmark5, ord('5'), [ssCtrl]);
  AddKey(ecGotoBookmark6, ord('6'), [ssCtrl]);
  AddKey(ecGotoBookmark7, ord('7'), [ssCtrl]);
  AddKey(ecGotoBookmark8, ord('8'), [ssCtrl]);
  AddKey(ecGotoBookmark9, ord('9'), [ssCtrl]);
  AddKey(ecSetBookmark1, ord('1'), [ssCtrl, ssShift]);
  AddKey(ecSetBookmark2, ord('2'), [ssCtrl, ssShift]);
  AddKey(ecSetBookmark3, ord('3'), [ssCtrl, ssShift]);
  AddKey(ecSetBookmark4, ord('4'), [ssCtrl, ssShift]);
  AddKey(ecSetBookmark5, ord('5'), [ssCtrl, ssShift]);
  AddKey(ecSetBookmark6, ord('6'), [ssCtrl, ssShift]);
  AddKey(ecSetBookmark7, ord('7'), [ssCtrl, ssShift]);
  AddKey(ecSetBookmark8, ord('8'), [ssCtrl, ssShift]);
  AddKey(ecSetBookmark9, ord('9'), [ssCtrl, ssShift]);
  { Selection modes }
  AddKey(ecNormalSelect, ord('N'), [ssCtrl,ssAlt]);
  AddKey(ecColumnSelect, ord('C'), [ssCtrl,ssAlt]);
  AddKey(ecLineSelect, ord('L'), [ssCtrl,ssAlt]);
end;

procedure TBCEditorKeyCommands.SetItem(Index: Integer; Value: TBCEditorKeyCommand);
begin
  inherited SetItem(index, Value);
end;

initialization

  RegisterIntegerConsts(TypeInfo(TBCEditorCommand), IdentToEditorCommand, EditorCommandToIdent);

end.

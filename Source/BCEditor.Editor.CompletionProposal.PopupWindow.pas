unit BCEditor.Editor.CompletionProposal.PopupWindow;

interface

uses
  Winapi.Messages, System.Classes, System.Types, Vcl.Forms, Vcl.Controls, Vcl.Graphics, BCEditor.Utils,
  BCEditor.Types, BCEditor.Editor.CompletionProposal.Columns, BCEditor.Editor.PopupWindow
{$IFDEF USE_ALPHASKINS}, sCommonData, acSBUtils{$ENDIF};

type
  TBCEditorValidateEvent = procedure(Sender: TObject; Shift: TShiftState; EndToken: Char) of object;

  TBCEditorCompletionProposalPopupWindow = class(TBCEditorPopupWindow)
  strict private
    FAdjustCompletionStart: Boolean;
    FAssignedList: TStrings;
    FBackgroundColor: TColor;
    FForegroundColor: TColor;
    FBitmapBuffer: TBitmap;
    FCaseSensitive: Boolean;
    FCloseChars: string;
    FColumns: TBCEditorProposalColumns; // TODO
{$IFDEF USE_ALPHASKINS}
    FCommonData: TsScrollWndData;
{$ENDIF}
    FCompletionStart: Integer;
    FSelectedLine: Integer;
    FCurrentString: string;
    FFiltered: Boolean;
    FFormWidth: Integer;
    FItemHeight: Integer;
    FItemList: TStrings;
    FMargin: Integer;
    FMouseWheelAccumulator: Integer;
    FOnValidate: TBCEditorValidateEvent;
{$IFDEF USE_ALPHASKINS}
    FScrollWnd: TacScrollWnd;
{$ENDIF}
    FTopLine: Integer;
    FSelectedBackgroundColor: TColor;
    FSelectedTextColor: TColor;
    FTriggerChars: string;
    FVisibleLines: Integer;
    function IsWordBreakChar(AChar: Char): Boolean;
    function GetItemList: TStrings;
    procedure AddKeyHandlers;
    procedure EditorKeyPress(Sender: TObject; var Key: Char);
    procedure HandleDblClick(Sender: TObject);
    procedure HandleOnValidate(Sender: TObject; Shift: TShiftState; EndToken: Char);
    procedure MoveLine(LineCount: Integer);
    procedure MoveSelectedLine(LineCount: Integer);
    procedure RemoveKeyHandlers;
    procedure SetCurrentString(const Value: string);
    procedure SetTopLine(const Value: Integer);
    procedure UpdateScrollBar;
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
{$IFDEF USE_ALPHASKINS}
    procedure AfterConstruction; override;
{$ENDIF}
    function GetCurrentInput: string;
    procedure Execute(ACurrentString: string; X, Y: Integer);
    procedure Assign(Source: TPersistent); override;
    procedure WndProc(var Message: TMessage); override;
    property CurrentString: string read FCurrentString write SetCurrentString;
    property ItemList: TStrings read GetItemList;
    property TopLine: Integer read FTopLine write SetTopLine;
    property OnValidate: TBCEditorValidateEvent read FOnValidate write FOnValidate;
{$IFDEF USE_ALPHASKINS}
    property SkinData: TsScrollWndData read FCommonData write FCommonData;
{$ENDIF}
  end;

implementation

uses
  Winapi.Windows, System.SysUtils, System.UITypes, BCEditor.Editor.Base, BCEditor.Editor.KeyCommands,
  BCEditor.Editor.Utils, BCEditor.Consts, System.Math, Vcl.Dialogs, BCEditor.Editor.CompletionProposal
{$IFDEF USE_ALPHASKINS}, Winapi.CommCtrl, sVCLUtils, sMessages, sConst, sSkinProps{$ENDIF};

{ TBCEditorCompletionProposalPopupWindow }

constructor TBCEditorCompletionProposalPopupWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

{$IFDEF USE_ALPHASKINS}
  FCommonData := TsScrollWndData.Create(Self, True);
  FCommonData.COC := COC_TsMemo;
  if FCommonData.SkinSection = '' then
    FCommonData.SkinSection := s_Edit;
{$ENDIF}
  AddKeyHandlers;

  Visible := False;

  FBitmapBuffer := Vcl.Graphics.TBitmap.Create;
  FItemList := TStringList.Create;
  FAssignedList := TStringList.Create;
  FFiltered := False;

  FSelectedBackgroundColor := clHighlight;
  FSelectedTextColor := clHighlightText;
  FBackgroundColor := clWindow;
  FForegroundColor := clWindowText;

  FCaseSensitive := False;

  FColumns := TBCEditorProposalColumns.Create(AOwner, TBCEditorProposalColumn);

  FItemHeight := 0;
  FMargin := 2;

  OnValidate := HandleOnValidate;
  OnDblClick := HandleDblClick;
  FTriggerChars := '.';
end;

destructor TBCEditorCompletionProposalPopupWindow.Destroy;
begin
{$IFDEF USE_ALPHASKINS}
  if FScrollWnd <> nil then
    FreeAndNil(FScrollWnd);
  if Assigned(FCommonData) then
    FreeAndNil(FCommonData);
{$ENDIF}
  RemoveKeyHandlers;

  FColumns.Free;
  FBitmapBuffer.Free;
  FItemList.Free;
  FAssignedList.Free;

  inherited Destroy;
end;

{$IFDEF USE_ALPHASKINS}
procedure TBCEditorCompletionProposalPopupWindow.AfterConstruction;
begin
  inherited AfterConstruction;

  UpdateData(FCommonData);
end;
{$ENDIF}

procedure TBCEditorCompletionProposalPopupWindow.Assign(Source: TPersistent);
begin
  if Source is TBCEditorCompletionProposal then
    with Source as TBCEditorCompletionProposal do
    begin
      Self.FBackgroundColor := Colors.Background;
      Self.FCaseSensitive := cpoCaseSensitive in Options;
      Self.FCloseChars := CloseChars;
      Self.FColumns.Assign(Columns);
      Self.FFiltered := cpoFiltered in Options;
      Self.FBitmapBuffer.Canvas.Font.Assign(Font);
      Self.FItemHeight := TextHeight(FBitmapBuffer.Canvas, 'X');
      Self.FForegroundColor := Colors.Foreground;
      Self.FFormWidth := Width;
      Self.FSelectedBackgroundColor := Colors.SelectedBackground;
      Self.FSelectedTextColor := Colors.SelectedText;
      Self.FTriggerChars := Trigger.Chars;
      Self.FVisibleLines := VisibleLines;
    end
  else
    inherited;
end;

procedure TBCEditorCompletionProposalPopupWindow.AddKeyHandlers;
var
  Editor: TBCBaseEditor;
begin
  Editor := Owner as TBCBaseEditor;
  if Assigned(Editor) then
  begin
    Editor.AddKeyPressHandler(EditorKeyPress);
    Editor.AddKeyDownHandler(EditorKeyDown);
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.RemoveKeyHandlers;
var
  Editor: TBCBaseEditor;
begin
  Editor := Owner as TBCBaseEditor;
  if Assigned(Editor) then
  begin
    Editor.RemoveKeyPressHandler(EditorKeyPress);
    Editor.RemoveKeyDownHandler(EditorKeyDown);
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  LChar: Char;
  LEditor: TBCBaseEditor;
  LTextCaretPosition: TBCEditorTextPosition;
begin
  LEditor := nil;
  if Assigned(Owner) then
    LEditor := Owner as TBCBaseEditor;
  case Key of
    VK_RETURN, VK_TAB:
      if Assigned(OnValidate) then
        OnValidate(Self, Shift, BCEDITOR_NONE_CHAR);
    VK_ESCAPE:
      Hide;
    VK_LEFT:
      begin
        if Length(FCurrentString) > 0 then
        begin
          CurrentString := Copy(FCurrentString, 1, Length(FCurrentString) - 1);
          if Assigned(LEditor) then
            LEditor.CommandProcessor(ecLeft, BCEDITOR_NONE_CHAR, nil);
        end
        else
        begin
          // Since we have control, we need to re-send the key to
          // the editor so that the cursor behaves properly
          if Assigned(LEditor) then
            LEditor.CommandProcessor(ecLeft, BCEDITOR_NONE_CHAR, nil);

          Hide;
        end;
      end;
    VK_RIGHT:
      begin
        if Assigned(LEditor) then
          with LEditor do
          begin
            LTextCaretPosition := TextCaretPosition;
            if LTextCaretPosition.Char <= Length(LineText) then
              LChar := LineText[LTextCaretPosition.Char]
            else
              LChar := BCEDITOR_SPACE_CHAR;

            if Self.IsWordBreakChar(LChar) then
              Self.Hide
            else
              CurrentString := FCurrentString + LChar;

            CommandProcessor(ecRight, BCEDITOR_NONE_CHAR, nil);
          end;
      end;
    VK_PRIOR:
      MoveLine(-FVisibleLines);
    VK_NEXT:
      MoveLine(FVisibleLines);
    VK_END:
      TopLine := FAssignedList.Count - 1;
    VK_HOME:
      TopLine := 0;
    VK_UP:
      if ssCtrl in Shift then
        FSelectedLine := 0
      else
        MoveSelectedLine(-1);
    VK_DOWN:
      if ssCtrl in Shift then
        FSelectedLine := FAssignedList.Count - 1
      else
        MoveSelectedLine(1);
    VK_BACK:
      if Shift = [] then
      begin
        if Length(FCurrentString) > 0 then
        begin
          CurrentString := Copy(FCurrentString, 1, Length(FCurrentString) - 1);

          if Assigned(LEditor) then
            LEditor.CommandProcessor(ecBackspace, BCEDITOR_NONE_CHAR, nil);
        end
        else
        begin
          if Assigned(LEditor) then
            LEditor.CommandProcessor(ecBackspace, BCEDITOR_NONE_CHAR, nil);

          Hide;
        end;
      end;
    VK_DELETE:
      if Assigned(LEditor) then
        LEditor.CommandProcessor(ecDeleteChar, BCEDITOR_NONE_CHAR, nil);
  end;
  Key := 0;
  Invalidate;
end;

procedure TBCEditorCompletionProposalPopupWindow.EditorKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    BCEDITOR_CARRIAGE_RETURN, BCEDITOR_ESCAPE:
      Hide;
    BCEDITOR_SPACE_CHAR .. high(Char):
      begin
        if IsWordBreakChar(Key) and Assigned(OnValidate) then
        begin
          if Key = BCEDITOR_SPACE_CHAR then
            OnValidate(Self, [], BCEDITOR_NONE_CHAR)
          else
            OnValidate(Self, [], Key);
        end;

        CurrentString := FCurrentString + Key;

        if Assigned(OnKeyPress) then
          OnKeyPress(Self, Key);
      end;
    BCEDITOR_BACKSPACE_CHAR:
      with Owner as TBCBaseEditor do
        CommandProcessor(ecChar, Key, nil);
  end;
  Invalidate;
end;

function TBCEditorCompletionProposalPopupWindow.CanResize(var NewWidth, NewHeight: Integer): Boolean;
var
  NewVisibleLines: Integer;
begin
  Result := True;

  if FItemHeight <> 0 then
  begin
    NewVisibleLines := NewHeight div FItemHeight;
    if NewVisibleLines < 1 then
      NewVisibleLines := 1;
  end
  else
    NewVisibleLines := 0;

  FVisibleLines := NewVisibleLines;
end;

procedure TBCEditorCompletionProposalPopupWindow.Resize;
begin
  inherited;
  if FItemHeight <> 0 then
    FVisibleLines := ClientHeight div FItemHeight;

  Invalidate;
end;

procedure TBCEditorCompletionProposalPopupWindow.Paint;
var
  i: Integer;
begin
  FBitmapBuffer.Width := ClientWidth;
  FBitmapBuffer.Height := ClientHeight;

  with FBitmapBuffer do
  begin
    Canvas.Brush.Color := FBackgroundColor;
    Canvas.Rectangle(0, 0, ClientWidth, ClientHeight);
    for i := 0 to Min(FVisibleLines, FAssignedList.Count - 1) do
    begin
      if i + TopLine >= FAssignedList.Count then
        Break;

      if i + TopLine = FSelectedLine then
      begin
        Canvas.Brush.Color := FSelectedBackgroundColor;
        Canvas.Pen.Color := FSelectedBackgroundColor;
        Canvas.Rectangle(0, FItemHeight * i, ClientWidth, FItemHeight * (i + 1));
        Canvas.Font.Color := FSelectedTextColor;
      end
      else
      begin
        Canvas.Brush.Color := FBackgroundColor;
        Canvas.Pen.Color := FBackgroundColor;
        Canvas.Font.Color := FForegroundColor;
      end;
      Canvas.TextOut(FMargin, FItemHeight * i, FAssignedList[TopLine + i]);
    end;
  end;
  Canvas.Draw(0, 0, FBitmapBuffer);
end;

procedure TBCEditorCompletionProposalPopupWindow.MoveLine(LineCount: Integer);
begin
  if LineCount > 0 then
  begin
    if (TopLine < (FAssignedList.Count - LineCount)) then
      TopLine := TopLine + LineCount
    else
      TopLine := FAssignedList.Count - 1;
  end
  else
  begin
    if TopLine + LineCount > 0 then
      TopLine := TopLine + LineCount
    else
      TopLine := 0;
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.MoveSelectedLine(LineCount: Integer);
begin
  FSelectedLine := MinMax(FSelectedLine + LineCount, 0, FAssignedList.Count - 1);
  if FSelectedLine >= TopLine + FVisibleLines then
    TopLine := FSelectedLine - FVisibleLines + 1;
  if FSelectedLine < TopLine then
    TopLine := FSelectedLine;
end;

procedure TBCEditorCompletionProposalPopupWindow.SetCurrentString(const Value: string);

  function MatchItem(AIndex: Integer; UseItemList: Boolean): Boolean;
  var
    CompareString: string;
  begin
    if FFiltered and (not UseItemList) then
      CompareString := FAssignedList[AIndex]
    else
      CompareString := FItemList[AIndex];

    CompareString := Copy(CompareString, 1, Length(Value));

    if FCaseSensitive then
      Result := WideCompareStr(CompareString, Value) = 0
    else
      Result := WideCompareText(CompareString, Value) = 0;
  end;

  procedure RecalcList;
  var
    i: Integer;
  begin
    FAssignedList.Clear;
    for i := 0 to FItemList.Count - 1 do
    begin
      if MatchItem(i, True) then
        FAssignedList.AddObject(FItemList[i], TObject(i));
    end;
  end;

var
  i: Integer;
begin
  FCurrentString := Value;

  if FFiltered then
  begin
    RecalcList;
    TopLine := 0;
    Repaint;
  end
  else
  begin
    i := 0;
    while (i < ItemList.Count) and (not MatchItem(i, True)) do
      inc(i);

    if i < ItemList.Count then
      TopLine := i
    else
      TopLine := 0;
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.SetTopLine(const Value: Integer);
var
  LDelta: Integer;
  LClientRect: TRect;
begin
  if Value <> TopLine then
  begin
    LDelta := TopLine - Value;
    FTopLine := Value;
    LClientRect := ClientRect;
    if Abs(LDelta) < FVisibleLines then
      ScrollWindow(Handle, 0, FItemHeight * LDelta, @LClientRect, @LClientRect)
    else
      Invalidate;
    UpdateScrollBar;
  end;
end;

function TBCEditorCompletionProposalPopupWindow.IsWordBreakChar(AChar: Char): Boolean;
begin
  Result := (Owner as TBCBaseEditor).IsWordBreakChar(AChar);
end;

procedure TBCEditorCompletionProposalPopupWindow.WMMouseWheel(var Msg: TMessage);
var
  Delta: Integer;
  WheelClicks: Integer;
begin
  if csDesigning in ComponentState then
    exit;

  if GetKeyState(VK_CONTROL) >= 0 then
    Delta := Mouse.WheelScrollLines
  else
    Delta := FVisibleLines;

  Inc(FMouseWheelAccumulator, Integer(Msg.wParamHi));
  WheelClicks := FMouseWheelAccumulator div WHEEL_DELTA;
  FMouseWheelAccumulator := FMouseWheelAccumulator mod WHEEL_DELTA;
  if (Delta = Integer(WHEEL_PAGESCROLL)) or (Delta > FVisibleLines) then
    Delta := FVisibleLines;

  TopLine := TopLine - (Delta * WheelClicks);
end;

procedure TBCEditorCompletionProposalPopupWindow.Execute(ACurrentString: string; X, Y: Integer);

  procedure RecalcFormPlacement;
  var
    LWidth: Integer;
    LHeight: Integer;
    LX: Integer;
    LY: Integer;
  begin
    LX := X - TextWidth(FBitmapBuffer.Canvas, ACurrentString);
    LY := Y;

    LWidth := FFormWidth;
    LHeight := FItemHeight * FVisibleLines + 2;

    if LX + LWidth > Screen.DesktopWidth then
    begin
      LX := Screen.DesktopWidth - LWidth - 5;
      if LX < 0 then
        LX := 0;
    end;

    if LY + LHeight > Screen.DesktopHeight then
    begin
      LY := LY - LHeight - (Owner as TBCBaseEditor).LineHeight - 2;
      if LY < 0 then
        LY := 0;
    end;

    SetWindowPos(Handle, HWND_TOP, LX, LY, 0, 0, SWP_NOACTIVATE or SWP_SHOWWINDOW);

    Width := LWidth;
    Height := LHeight;
  end;

begin
  FAssignedList.Assign(ItemList);
  if FAssignedList.Count > 0 then
  begin
    RecalcFormPlacement;
    CurrentString := ACurrentString;
    UpdateScrollBar;
    Visible := True;
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.HandleOnValidate(Sender: TObject; Shift: TShiftState; EndToken: Char);
var
  Editor: TBCBaseEditor;
  Value: string;
  LTextPosition: TBCEditorTextPosition;
begin
  if not Assigned(Owner) then
    exit;
  Editor := Owner as TBCBaseEditor;
  with Editor do
  begin
    BeginUpdate;
    BeginUndoBlock;
    try
      LTextPosition := Editor.TextCaretPosition;
      if FAdjustCompletionStart then
        FCompletionStart := GetTextPosition(FCompletionStart, LTextPosition.Line).Char;
      SelectionBeginPosition := GetTextPosition(FCompletionStart, LTextPosition.Line);
      if EndToken = BCEDITOR_NONE_CHAR then
        SelectionEndPosition := GetTextPosition(WordEnd.Char, LTextPosition.Line)
      else
        SelectionEndPosition := LTextPosition;

      if FSelectedLine < FAssignedList.Count then
        Value := FAssignedList[FSelectedLine]
      else
        Value := SelectedText;

      if SelectedText <> Value then
        SelectedText := Value;

      with Editor do
      begin
        if CanFocus then
          SetFocus;
        EnsureCursorPositionVisible;
        TextCaretPosition := SelectionEndPosition;
        SelectionBeginPosition := TextCaretPosition;
      end;
    finally
      EndUndoBlock;
      EndUpdate;
    end;
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.HandleDblClick(Sender: TObject);
begin
  if Assigned(OnValidate) then
    OnValidate(Self, [], BCEDITOR_NONE_CHAR);
  Hide;
end;

function TBCEditorCompletionProposalPopupWindow.GetCurrentInput: string;
var
  S: string;
  i: Integer;
  LEditor: TBCBaseEditor;
  LTextCaretPosition: TBCEditorTextPosition;
begin
  Result := '';
  LEditor := Owner as TBCBaseEditor;
  S := LEditor.LineText;
  LTextCaretPosition := LEditor.TextCaretPosition;
  i := LTextCaretPosition.Char;
  if i <= Length(S) then
  begin
    FAdjustCompletionStart := False;
    while (i > 0) and (S[i] > BCEDITOR_SPACE_CHAR) and not Self.IsWordBreakChar(S[i]) do
      Dec(i);

    FCompletionStart := i + 1;
    Result := Copy(S, i + 1, LTextCaretPosition.Char - i - 1);
  end
  else
    FAdjustCompletionStart := True;

  FCompletionStart := i;
end;

function TBCEditorCompletionProposalPopupWindow.GetItemList: TStrings;
begin
  Result := FItemList;
end;

procedure TBCEditorCompletionProposalPopupWindow.UpdateScrollBar;
var
  LScrollInfo: TScrollInfo;
begin
  LScrollInfo.cbSize := SizeOf(ScrollInfo);
  LScrollInfo.fMask := SIF_ALL;
  LScrollInfo.fMask := LScrollInfo.fMask or SIF_DISABLENOSCROLL;

  if Visible then
    SendMessage(Handle, WM_SETREDRAW, 0, 0);

  LScrollInfo.nMin := 0;
  LScrollInfo.nMax := Max(0, FItemList.Count - 2);
  LScrollInfo.nPage := FVisibleLines;
  LScrollInfo.nPos := TopLine;

  ShowScrollBar(Handle, SB_VERT, (LScrollInfo.nMin = 0) or (LScrollInfo.nMax > FVisibleLines));
  SetScrollInfo(Handle, SB_VERT, LScrollInfo, True);

  if FItemList.Count <= FVisibleLines then
    EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_BOTH)
  else
  begin
    EnableScrollBar(Handle, SB_VERT, ESB_ENABLE_BOTH);
    if TopLine <= 0 then
      EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_UP)
    else
    if TopLine + FVisibleLines >= FItemList.Count then
      EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_DOWN);
  end;

  if Visible then
    SendMessage(Handle, WM_SETREDRAW, -1, 0);

{$IFDEF USE_VCL_STYLES}
  Perform(CM_UPDATE_VCLSTYLE_SCROLLBARS, 0, 0);
{$ENDIF}
end;

procedure TBCEditorCompletionProposalPopupWindow.WMVScroll(var Msg: TWMScroll);
begin
  Invalidate;
  Msg.Result := 0;

  case Msg.ScrollCode of
    { Scrolls to start / end of the text }
    SB_TOP:
      TopLine := 0;
    SB_BOTTOM:
      TopLine := FItemList.Count - 1;
    { Scrolls one line up / down }
    SB_LINEDOWN:
      TopLine := Min(FItemList.Count - FVisibleLines, TopLine + 1);
    SB_LINEUP:
      TopLine := Max(0, TopLine - 1);
    { Scrolls one page of lines up / down }
    SB_PAGEDOWN:
      TopLine := Min(FItemList.Count - FVisibleLines, TopLine + FVisibleLines);
    SB_PAGEUP:
      TopLine := Max(0, TopLine - FVisibleLines);
    { Scrolls to the current scroll bar position }
    SB_THUMBPOSITION, SB_THUMBTRACK:
      begin
        if FItemList.Count > BCEDITOR_MAX_SCROLL_RANGE then
          TopLine := MulDiv(FVisibleLines + FItemList.Count - 1, Msg.Pos, BCEDITOR_MAX_SCROLL_RANGE)
        else
          TopLine := Msg.Pos;
      end;
  end;
  Invalidate;
end;

procedure TBCEditorCompletionProposalPopupWindow.WndProc(var Message: TMessage);
begin
{$IFDEF USE_ALPHASKINS}
  if Message.Msg = SM_ALPHACMD then
    case Message.wParamHi of
      AC_CTRLHANDLED:
        begin
          Message.Result := 1;
          Exit;
        end;

      AC_GETAPPLICATION:
        begin
          Message.Result := LRESULT(Application);
          Exit
        end;

      AC_REMOVESKIN:
        if (ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager)) and not(csDestroying in ComponentState) then
        begin
          if FScrollWnd <> nil then
            FreeAndNil(FScrollWnd);

          CommonWndProc(Message, FCommonData);
          RecreateWnd;
          exit;
        end;

      AC_REFRESH:
        if (ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager)) and Visible then
        begin
          CommonWndProc(Message, FCommonData);
          RefreshEditScrolls(SkinData, FScrollWnd);
          SendMessage(Handle, WM_NCPAINT, 0, 0);
          exit;
        end;

      AC_SETNEWSKIN:
        if (ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager)) then
        begin
          CommonWndProc(Message, FCommonData);
          exit;
        end;
    end;

  if not ControlIsReady(Self) or not Assigned(FCommonData) or not FCommonData.Skinned then
    inherited
  else
  begin
    if Message.Msg = SM_ALPHACMD then
      case Message.wParamHi of
        AC_ENDPARENTUPDATE:
          if FCommonData.Updating then
          begin
            if not InUpdating(FCommonData, True) then
              Perform(WM_NCPAINT, 0, 0);

            exit;
          end;
      end;

    CommonWndProc(Message, FCommonData);

    inherited;

    case Message.Msg of
      TB_SETANCHORHIGHLIGHT, WM_SIZE:
        SendMessage(Handle, WM_NCPAINT, 0, 0);
      CM_SHOWINGCHANGED:
        RefreshEditScrolls(SkinData, FScrollWnd);
    end;
  end;
{$ELSE}
  inherited;
{$ENDIF}
end;

procedure TBCEditorCompletionProposalPopupWindow.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSelectedLine := Max(0, TopLine + (Y div FItemHeight));
  inherited MouseDown(Button, Shift, X, Y);
  Refresh;
end;

end.

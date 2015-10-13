unit BCEditor.Editor.CompletionProposal.PopupWindow;

interface

uses
  Winapi.Messages, System.Classes, System.Types, Vcl.StdCtrls, Vcl.Forms, Vcl.Controls, Vcl.Graphics, BCEditor.Utils,
  BCEditor.Types, BCEditor.Editor.CompletionProposal.Columns, BCEditor.Editor.PopupWindow, Vcl.ExtCtrls
{$IFDEF USE_ALPHASKINS}, sCommonData, acSBUtils{$ENDIF};

type
  TBCEditorValidateEvent = procedure(Sender: TObject; Shift: TShiftState; EndToken: Char) of object;

  TBCEditorCompletionProposalPopupWindow = class(TBCEditorPopupWindow)
  strict private
    FAdjustCompletionStart: Boolean;
    FAssignedList: TStrings;
    FBackgroundColor: TColor;
    FBufferBmp: TBitmap;
    FBorderColor: TColor;
   // FBorderWidth: Integer;
    FCaseSensitive: Boolean;
    FCloseChars: string;
    FColumns: TBCEditorProposalColumns;
{$IFDEF USE_ALPHASKINS}
    FCommonData: TsScrollWndData;
{$ENDIF}
    FCompletionStart: Integer;
    FSelectedLine: Integer;
    FCurrentString: string;
    FEffectiveItemHeight: Integer;
    FFiltered: Boolean;
    FFont: TFont;
    FFontHeight: Integer;
    FFormWidth: Integer;
    FHeightBuffer: Integer;
    FItemHeight: Integer;
    FItemList: TStrings;
    FMargin: Integer;
    FMouseWheelAccumulator: Integer;
    FNoNextKey: Boolean;
    FOnCancel: TNotifyEvent;
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
    procedure AddKeyPressHandler;
    procedure DoDoubleClick(Sender: TObject);
    procedure EditorKeyPress(Sender: TObject; var Key: Char);
    procedure FontChange(Sender: TObject);
    procedure HandleOnCancel(Sender: TObject);
    procedure HandleDblClick(Sender: TObject);
    procedure HandleOnKeyPress(Sender: TObject; var Key: Char);
    procedure HandleOnValidate(Sender: TObject; Shift: TShiftState; EndToken: Char);
    procedure MoveLine(LineCount: Integer);
    procedure RecalcItemHeight;
    procedure RemoveKeyPressHandler;
    // procedure SetColumns(Value: TBCEditorProposalColumns);
    procedure SetCurrentString(const Value: string);
    // procedure SetFont(const Value: TFont);
    // procedure SetItemHeight(const Value: Integer);
    procedure SetTopLine(const Value: Integer);
    procedure UpdateScrollBar;
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
  protected
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure DoKeyPressW(Key: Char);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPressW(var Key: Char); virtual;
    procedure Paint; override;
    procedure Resize; override;
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

    (* property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor default clWindow;
      property BorderWidth: Integer read FBorderWidth write FBorderWidth default 3;
      property BorderColor: TColor read FBorderColor write FBorderColor default clBtnFace;
      property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive default False;
      property CloseChars: string read FCloseChars write FCloseChars;
      property Columns: TBCEditorProposalColumns read FColumns write SetColumns;

      property Filtered: Boolean read FFiltered write FFiltered;
      property Font: TFont read FFont write SetFont;
      property FormWidth: Integer read FFormWidth write FFormWidth; { Don't use the width because it triggers resizing }
      property ItemHeight: Integer read FItemHeight write SetItemHeight default 0;

      property Margin: Integer read FMargin write FMargin default 2;


      property SelectedBackgroundColor: TColor read FSelectedBackgroundColor write FSelectedBackgroundColor
      default clHighlight;
      property SelectedTextColor: TColor read FSelectedTextColor write FSelectedTextColor default clHighlightText;
      property TriggerChars: string read FTriggerChars write FTriggerChars;
      property VisibleLines: Integer read FVisibleLines write FVisibleLines; *)
    property CurrentString: string read FCurrentString write SetCurrentString;
    property ItemList: TStrings read GetItemList;
    property TopLine: Integer read FTopLine write SetTopLine;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
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
  AddKeyPressHandler;

  Visible := False;
  //FBorderWidth := 3;

  FBufferBmp := Vcl.Graphics.TBitmap.Create;
  FItemList := TStringList.Create;
  FAssignedList := TStringList.Create;
  FFiltered := False;

  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;

  FSelectedBackgroundColor := clHighlight;
  FSelectedTextColor := clHighlightText;
  FBackgroundColor := clWindow;
  FBorderColor := clBtnFace;

  FCaseSensitive := False;

  FColumns := TBCEditorProposalColumns.Create(AOwner, TBCEditorProposalColumn);

  FItemHeight := 0;
  FMargin := 2;
  FEffectiveItemHeight := 0;
  RecalcItemHeight;

  FHeightBuffer := 0;
  FFont.OnChange := FontChange;

  OnDblClick := DoDoubleClick;

  OnKeyPress := HandleOnKeyPress;
  OnValidate := HandleOnValidate;
  OnCancel := HandleOnCancel;
  OnDblClick := HandleDblClick;
  FTriggerChars := '.';
  FNoNextKey := False;
end;

destructor TBCEditorCompletionProposalPopupWindow.Destroy;
begin
{$IFDEF USE_ALPHASKINS}
  if FScrollWnd <> nil then
    FreeAndNil(FScrollWnd);
  if Assigned(FCommonData) then
    FreeAndNil(FCommonData);
{$ENDIF}
  RemoveKeyPressHandler;

  FColumns.Free;
  FBufferBmp.Free;
  FItemList.Free;
  FAssignedList.Free;
  FFont.Free;

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
      Self.FBorderColor := Colors.Border;
      Self.FCaseSensitive := cpoCaseSensitive in Options;
      Self.FCloseChars := CloseChars;
      Self.FColumns.Assign(Columns);
      Self.FFiltered := cpoFiltered in Options;
      Self.Font.Assign(Font);
      Self.FFormWidth := Width;
      Self.FSelectedBackgroundColor := Colors.SelectedBackground;
      Self.FSelectedTextColor := Colors.SelectedText;
      Self.FTriggerChars := Trigger.Chars;
      Self.FVisibleLines := VisibleLines;
    end
  else
    inherited;
end;

procedure TBCEditorCompletionProposalPopupWindow.AddKeyPressHandler;
var
  Editor: TBCBaseEditor;
begin
  Editor := Owner as TBCBaseEditor;
  if Assigned(Editor) then
    Editor.AddKeyPressHandler(EditorKeyPress);
end;

procedure TBCEditorCompletionProposalPopupWindow.RemoveKeyPressHandler;
var
  Editor: TBCBaseEditor;
begin
  Editor := Owner as TBCBaseEditor;
  if Assigned(Editor) then
    Editor.RemoveKeyPressHandler(EditorKeyPress);
end;

procedure TBCEditorCompletionProposalPopupWindow.KeyDown(var Key: Word; Shift: TShiftState);
var
  LChar: Char;
begin
  case Key of
    VK_RETURN, VK_TAB:
      if Assigned(OnValidate) then
        OnValidate(Self, Shift, BCEDITOR_NONE_CHAR);
    VK_ESCAPE:
      begin
        if Assigned(OnCancel) then
          OnCancel(Self);
      end;
    VK_LEFT:
      begin
        if Length(FCurrentString) > 0 then
        begin
          CurrentString := Copy(FCurrentString, 1, Length(FCurrentString) - 1);
          if Assigned(Owner) then
            (Owner as TBCBaseEditor).CommandProcessor(ecLeft, BCEDITOR_NONE_CHAR, nil);
        end
        else
        begin
          // Since we have control, we need to re-send the key to
          // the editor so that the cursor behaves properly
          if Assigned(Owner) then
            (Owner as TBCBaseEditor).CommandProcessor(ecLeft, BCEDITOR_NONE_CHAR, nil);

          if Assigned(OnCancel) then
            OnCancel(Self);
        end;
      end;
    VK_RIGHT:
      begin
        if Assigned(Owner) then
          with Owner as TBCBaseEditor do
          begin
            if DisplayCaretX <= Length(LineText) then
              LChar := LineText[DisplayCaretX]
            else
              LChar := BCEDITOR_SPACE_CHAR;

            if Self.IsWordBreakChar(LChar) then
            begin
              if Assigned(OnCancel) then
                OnCancel(Self)
            end
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
        TopLine := 0
      else
        MoveLine(-1);
    VK_DOWN:
      if ssCtrl in Shift then
        TopLine := FAssignedList.Count - 1
      else
        MoveLine(1);
    VK_BACK:
      if Shift = [] then
      begin
        if Length(FCurrentString) > 0 then
        begin
          CurrentString := Copy(FCurrentString, 1, Length(FCurrentString) - 1);

          if Assigned(Owner) then
            (Owner as TBCBaseEditor).CommandProcessor(ecBackspace, BCEDITOR_NONE_CHAR, nil);
        end
        else
        begin
          if Assigned(Owner) then
            (Owner as TBCBaseEditor).CommandProcessor(ecBackspace, BCEDITOR_NONE_CHAR, nil);

          if Assigned(OnCancel) then
            OnCancel(Self);
        end;
      end;
    VK_DELETE:
      if Assigned(Owner) then
        (Owner as TBCBaseEditor).CommandProcessor(ecDeleteChar, BCEDITOR_NONE_CHAR, nil);
  end;
  Invalidate;
end;

procedure TBCEditorCompletionProposalPopupWindow.DoKeyPressW(Key: Char);
begin
  if Key <> BCEDITOR_NONE_CHAR then
    KeyPressW(Key);
end;

procedure TBCEditorCompletionProposalPopupWindow.KeyPressW(var Key: Char);
begin
  case Key of
    BCEDITOR_CARRIAGE_RETURN, BCEDITOR_ESCAPE:
      ;
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
      if Assigned(OnKeyPress) then
        OnKeyPress(Self, Key);
      else
      with Owner as TBCBaseEditor do
        CommandProcessor(ecChar, Key, nil);

    if Assigned(OnCancel) then
      OnCancel(Self);
  end;
  Invalidate;
end;

function TBCEditorCompletionProposalPopupWindow.CanResize(var NewWidth, NewHeight: Integer): Boolean;
var
  NewVisibleLines: Integer;
begin
  Result := True;

  if FEffectiveItemHeight <> 0 then
  begin
    NewVisibleLines := (NewHeight - {FBorderWidth -} FHeightBuffer) div FEffectiveItemHeight;
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
  if FEffectiveItemHeight <> 0 then
    FVisibleLines := (ClientHeight - FHeightBuffer) div FEffectiveItemHeight;

  Invalidate;
end;

procedure TBCEditorCompletionProposalPopupWindow.Paint;

  procedure ResetCanvas;
  begin
    with FBufferBmp.Canvas do
    begin
      Pen.Color := FBackgroundColor;
      Brush.Color := FBackgroundColor;
      Font.Assign(FFont);
    end;
  end;

var
  i: Integer;
begin
  FBufferBmp.Width := ClientWidth;
  FBufferBmp.Height := ClientHeight;

  with FBufferBmp do
  begin
    ResetCanvas;
    Canvas.Rectangle(0, 0, ClientWidth, ClientHeight);
    for i := 0 to Min(FVisibleLines, FAssignedList.Count - 1) do
    begin
      if i + TopLine >= FAssignedList.Count then
        Continue;
      if i + TopLine = FSelectedLine then
        with Canvas do
        begin
          Canvas.Brush.Color := FSelectedBackgroundColor;
          Pen.Color := FSelectedBackgroundColor;
          Rectangle(0, FEffectiveItemHeight * i, ClientWidth, FEffectiveItemHeight * (i + 1));
          Pen.Color := FSelectedTextColor;
          Font.Assign(FFont);
          Font.Color := FSelectedTextColor;
        end;
      BCEditor.Utils.TextOut(Canvas, FMargin, FEffectiveItemHeight * i, FAssignedList[TopLine + i]);

      if i + TopLine = FSelectedLine then
        ResetCanvas;
    end;
  end;
  Canvas.Draw(0, FHeightBuffer, FBufferBmp);
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

procedure TBCEditorCompletionProposalPopupWindow.SetCurrentString(const Value: string);

  function MatchItem(AIndex: Integer; UseItemList: Boolean): Boolean;
  var
    CompareString: string;
  begin
    if (FFiltered) and (not UseItemList) then
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

procedure TBCEditorCompletionProposalPopupWindow.DoDoubleClick(Sender: TObject);
begin
  if Assigned(OnValidate) then
    OnValidate(Self, [], BCEDITOR_NONE_CHAR);
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

procedure TBCEditorCompletionProposalPopupWindow.RecalcItemHeight;
begin
  Canvas.Font.Assign(FFont);
  FFontHeight := TextHeight(Canvas, 'X');
  if FItemHeight > 0 then
    FEffectiveItemHeight := FItemHeight
  else
    FEffectiveItemHeight := FFontHeight;
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

  inc(FMouseWheelAccumulator, Integer(Msg.wParamHi));
  WheelClicks := FMouseWheelAccumulator div WHEEL_DELTA;
  FMouseWheelAccumulator := FMouseWheelAccumulator mod WHEEL_DELTA;
  if (Delta = Integer(WHEEL_PAGESCROLL)) or (Delta > FVisibleLines) then
    Delta := FVisibleLines;

  TopLine := TopLine - (Delta * WheelClicks);
end;

procedure TBCEditorCompletionProposalPopupWindow.WMChar(var Msg: TWMChar);
begin
  DoKeyPressW(Char(Msg.CharCode))
end;

procedure TBCEditorCompletionProposalPopupWindow.FontChange(Sender: TObject);
begin
  RecalcItemHeight;
end;

procedure TBCEditorCompletionProposalPopupWindow.Execute(ACurrentString: string; X, Y: Integer);

  procedure RecalcFormPlacement;
  var
    LWidth: Integer;
    LHeight: Integer;
    LX: Integer;
    LY: Integer;
  begin
    LX := X;
    LY := Y;

    LWidth := FFormWidth;
    LHeight := FHeightBuffer + FEffectiveItemHeight * FVisibleLines; // + FBorderWidth;

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
  FNoNextKey := Visible;
end;

procedure TBCEditorCompletionProposalPopupWindow.HandleOnCancel(Sender: TObject);
var
  Editor: TBCBaseEditor;
begin
  Editor := Owner as TBCBaseEditor;
  FNoNextKey := False;

  Editor.SetFocus;
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

      if FAssignedList.Count > TopLine then
        Value := FAssignedList[TopLine]
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

procedure TBCEditorCompletionProposalPopupWindow.HandleOnKeyPress(Sender: TObject; var Key: Char);
var
  Editor: TBCBaseEditor;
begin
  Editor := Owner as TBCBaseEditor;
  with Editor do
    CommandProcessor(ecChar, Key, nil);
end;

procedure TBCEditorCompletionProposalPopupWindow.EditorKeyPress(Sender: TObject; var Key: Char);
begin
  if FNoNextKey then
  begin
    FNoNextKey := False;
    Key := BCEDITOR_NONE_CHAR;
  end
end;

procedure TBCEditorCompletionProposalPopupWindow.HandleDblClick(Sender: TObject);
begin
  HandleOnValidate(Sender, [], BCEDITOR_NONE_CHAR);
end;

function TBCEditorCompletionProposalPopupWindow.GetCurrentInput: string;
var
  S: string;
  i: Integer;
  Editor: TBCBaseEditor;
begin
  Result := '';
  Editor := Owner as TBCBaseEditor;
  S := Editor.LineText;
  i := Editor.DisplayCaretX - 1;
  if i <= Length(S) then
  begin
    FAdjustCompletionStart := False;
    while (i > 0) and (S[i] > BCEDITOR_SPACE_CHAR) and not Self.IsWordBreakChar(S[i]) do
      Dec(i);

    FCompletionStart := i + 1;
    Result := Copy(S, i + 1, Editor.DisplayCaretX - i - 1);
  end
  else
    FAdjustCompletionStart := True;

  FCompletionStart := i + 1;
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

  LScrollInfo.nMin := 1;
  LScrollInfo.nMax := Max(1, FItemList.Count);
  LScrollInfo.nPage := FVisibleLines;
  LScrollInfo.nPos := TopLine;

  ShowScrollBar(Handle, SB_VERT, (LScrollInfo.nMin = 0) or (LScrollInfo.nMax > FVisibleLines));
  SetScrollInfo(Handle, SB_VERT, LScrollInfo, True);

  if FItemList.Count <= FVisibleLines then
    EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_BOTH)
  else
  begin
    EnableScrollBar(Handle, SB_VERT, ESB_ENABLE_BOTH);
    if FSelectedLine <= 1 then
      EnableScrollBar(Handle, SB_VERT, ESB_DISABLE_UP)
    else if FSelectedLine - FItemList.Count + 1 = 0 then
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
      TopLine := 1;
    SB_BOTTOM:
      TopLine := FItemList.Count;
    { Scrolls one line up / down }
    SB_LINEDOWN:
      TopLine := TopLine + 1;
    SB_LINEUP:
      TopLine := TopLine - 1;
    { Scrolls one page of lines up / down }
    SB_PAGEDOWN:
      TopLine := TopLine + FVisibleLines;
    SB_PAGEUP:
      TopLine := TopLine - FVisibleLines;
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
          exit;
        end;

      AC_GETAPPLICATION:
        begin
          Message.Result := LRESULT(Application);
          exit
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

end.

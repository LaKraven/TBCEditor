unit BCEditor.Editor.CompletionProposal.PopupWindow;

interface

uses
  Winapi.Messages, System.Classes, System.Types, Vcl.StdCtrls, Vcl.Forms, Vcl.Controls, Vcl.Graphics, BCEditor.Utils,
  BCEditor.Types, BCEditor.Editor.CompletionProposal.Columns{$IFDEF USE_ALPHASKINS}, sScrollBar{$ENDIF},
  BCEditor.Editor.PopupWindow;

type
  TBCEditorValidateEvent = procedure(Sender: TObject; Shift: TShiftState; EndToken: Char) of object;

  TBCEditorCompletionProposalPopupWindow = class(TBCEditorPopupWindow)
  strict private
    FAdjustCompletionStart: Boolean;
    FAssignedList: TStrings;
    FBackgroundColor: TColor;
    FBitmap: TBitmap;
    FBorderColor: TColor;
    FBorderWidth: Integer;
    FCaseSensitive: Boolean;
    FCloseChars: string;
    FColumns: TBCEditorProposalColumns;
    FCompletionStart: Integer;
    FCurrentString: string;
    FEffectiveItemHeight: Integer;
    FFiltered: Boolean;
    FFont: TFont;
    FFontHeight: Integer;
    FFormWidth: Integer;
    FHeightBuffer: Integer;
    FImages: TImageList;
    FItemHeight: Integer;
    FItemList: TStrings;
    FMargin: Integer;
    FMouseWheelAccumulator: Integer;
    FNoNextKey: Boolean;
    FOnCancel: TNotifyEvent;
    FOnValidate: TBCEditorValidateEvent;
    FPosition: Integer;
{$IFDEF USE_ALPHASKINS}
    FScrollBar: TsScrollBar;
{$ELSE}
    FScrollBar: TScrollBar;
{$ENDIF}
    FSelectedBackgroundColor: TColor;
    FSelectedTextColor: TColor;
    FTriggerChars: string;
    FVisibleLines: Integer;
    function IsWordBreakChar(AChar: Char): Boolean;
    function GetItemList: TStrings;
    procedure AddKeyPressHandler;
    procedure AdjustScrollBarPosition;
    procedure AdjustMetrics;
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
    procedure SetColumns(Value: TBCEditorProposalColumns);
    procedure SetCurrentString(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetImages(const Value: TImageList);
    procedure SetItemHeight(const Value: Integer);
    procedure SetItemList(const Value: TStrings);
    procedure SetPosition(const Value: Integer);
    procedure ScrollBarOnChange(Sender: TObject);
    //procedure ScrollBarOnEnter(Sender: TObject);
    procedure ScrollBarOnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure StringListChange(Sender: TObject);
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
  protected
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure DoKeyPressW(Key: Char);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPressW(var Key: Char); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetCurrentInput: string;
    procedure Execute(ACurrentString: string; X, Y: Integer);

    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor default clWindow;
    property BorderWidth: Integer read FBorderWidth write FBorderWidth default 3;
    property BorderColor: TColor read FBorderColor write FBorderColor default clBtnFace;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive default False;
    property CloseChars: string read FCloseChars write FCloseChars;
    property Columns: TBCEditorProposalColumns read FColumns write SetColumns;
    property CurrentString: string read FCurrentString write SetCurrentString;
    property Filtered: Boolean read FFiltered write FFiltered;
    property Font: TFont read FFont write SetFont;
    property FormWidth: Integer read FFormWidth write FFormWidth; { Don't use the width because it triggers resizing }
    property Images: TImageList read FImages write SetImages;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 0;
    property ItemList: TStrings read GetItemList write SetItemList;
    property Margin: Integer read FMargin write FMargin default 2;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property OnValidate: TBCEditorValidateEvent read FOnValidate write FOnValidate;
    property Position: Integer read FPosition write SetPosition;
    property SelectedBackgroundColor: TColor read FSelectedBackgroundColor write FSelectedBackgroundColor
      default clHighlight;
    property SelectedTextColor: TColor read FSelectedTextColor write FSelectedTextColor default clHighlightText;
    property TriggerChars: string read FTriggerChars write FTriggerChars;
    property VisibleLines: Integer read FVisibleLines write FVisibleLines;
  end;

implementation

uses
  Winapi.Windows, System.SysUtils, System.UITypes, BCEditor.Editor.Base, BCEditor.Editor.KeyCommands,
  BCEditor.Editor.Utils, BCEditor.Consts, System.Math, Vcl.Dialogs;

{ TBCEditorCompletionProposalPopupWindow }

constructor TBCEditorCompletionProposalPopupWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AddKeyPressHandler;

  Visible := False;
  FBorderWidth := 3;

  FBitmap := Vcl.Graphics.TBitmap.Create;
  FItemList := TStringList.Create;
  FAssignedList := TStringList.Create;
  FFiltered := False;

{$IFDEF USE_ALPHASKINS}
  FScrollBar := TsScrollBar.Create(Self);
{$ELSE}
  FScrollBar := TScrollBar.Create(Self);
{$ENDIF}
  with FScrollBar do
  begin
    Kind := sbVertical;
    ParentCtl3D := False;
    OnChange := ScrollBarOnChange;
    OnScroll := ScrollBarOnScroll;
    //OnEnter := ScrollBarOnEnter;
    Parent := Self;
  end;

  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;

  FSelectedBackgroundColor := clHighlight;
  FSelectedTextColor := clHighlightText;
  FBackgroundColor := clWindow;
  FBorderColor := clBtnFace;

  (FItemList as TStringList).OnChange := StringListChange;
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
  TriggerChars := '.';
  FNoNextKey := False;
end;

destructor TBCEditorCompletionProposalPopupWindow.Destroy;
begin
  RemoveKeyPressHandler;

  FColumns.Free;
  FBitmap.Free;
  FItemList.Free;
  FAssignedList.Free;
  FFont.Free;

  inherited Destroy;
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
          CurrentString := Copy(CurrentString, 1, Length(CurrentString) - 1);
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
              CurrentString := CurrentString + LChar;

            CommandProcessor(ecRight, BCEDITOR_NONE_CHAR, nil);
          end;
      end;
    VK_PRIOR:
      MoveLine(-FVisibleLines);
    VK_NEXT:
      MoveLine(FVisibleLines);
    VK_END:
      Position := FAssignedList.Count - 1;
    VK_HOME:
      Position := 0;
    VK_UP:
      if ssCtrl in Shift then
        Position := 0
      else
        MoveLine(-1);
    VK_DOWN:
      if ssCtrl in Shift then
        Position := FAssignedList.Count - 1
      else
        MoveLine(1);
    VK_BACK:
      if Shift = [] then
      begin
        if Length(FCurrentString) > 0 then
        begin
          CurrentString := Copy(CurrentString, 1, Length(CurrentString) - 1);

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
    begin if Key <> BCEDITOR_NONE_CHAR then KeyPressW(Key);
    end;

    procedure TBCEditorCompletionProposalPopupWindow.KeyPressW(var Key: Char);
    begin case Key of BCEDITOR_CARRIAGE_RETURN, BCEDITOR_ESCAPE:
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

        CurrentString := CurrentString + Key;

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

procedure TBCEditorCompletionProposalPopupWindow.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Y := (Y - FHeightBuffer) div FEffectiveItemHeight;
  Position := FScrollBar.Position + Y;
end;

function TBCEditorCompletionProposalPopupWindow.CanResize(var NewWidth, NewHeight: Integer): Boolean;
var
  NewVisibleLines: Integer;
begin
  Result := True;

  if FEffectiveItemHeight <> 0 then
  begin
    NewVisibleLines := (NewHeight - FBorderWidth - FHeightBuffer) div FEffectiveItemHeight;
    if NewVisibleLines < 1 then
      NewVisibleLines := 1;
  end
  else
    NewVisibleLines := 0;

  FVisibleLines := NewVisibleLines;
  NewHeight := FEffectiveItemHeight * FVisibleLines + FHeightBuffer + FBorderWidth;
  if (NewWidth - FBorderWidth) < FScrollBar.Width then
    NewWidth := FScrollBar.Width + FBorderWidth;
end;

procedure TBCEditorCompletionProposalPopupWindow.Resize;
begin
  inherited;
  if FEffectiveItemHeight <> 0 then
    FVisibleLines := (ClientHeight - FHeightBuffer) div FEffectiveItemHeight;

  if not(csCreating in ControlState) then
    AdjustMetrics;

  AdjustScrollBarPosition;
  Invalidate;
end;

procedure TBCEditorCompletionProposalPopupWindow.Paint;

  procedure ResetCanvas;
  begin
    with FBitmap.Canvas do
    begin
      Pen.Color := FBackgroundColor;
      Brush.Color := FBackgroundColor;
      Font.Assign(FFont);
    end;
  end;

var
  i: Integer;
begin
  with FBitmap do
  begin
    ResetCanvas;
    Canvas.Rectangle(0, 0, ClientWidth, ClientHeight);
    for i := 0 to Min(FVisibleLines, FAssignedList.Count - 1) do
    begin
      if i + FScrollBar.Position >= FAssignedList.Count then
        Continue;
      if i + FScrollBar.Position = Position then
        with Canvas do
        begin
          Canvas.Brush.Color := FSelectedBackgroundColor;
          Pen.Color := FSelectedBackgroundColor;
          Rectangle(0, FEffectiveItemHeight * i, ClientWidth - FScrollBar.Width, FEffectiveItemHeight * (i + 1));
          Pen.Color := FSelectedTextColor;
          Font.Assign(FFont);
          Font.Color := FSelectedTextColor;
        end;
      BCEditor.Utils.TextOut(Canvas, FMargin, FEffectiveItemHeight * i, FAssignedList[FScrollBar.Position + i]);

      if i + FScrollBar.Position = Position then
        ResetCanvas;
    end;
  end;
  Canvas.Draw(0, FHeightBuffer, FBitmap);
end;

procedure TBCEditorCompletionProposalPopupWindow.ScrollBarOnChange(Sender: TObject);
begin
  if Position < FScrollBar.Position then
    Position := FScrollBar.Position
  else if Position > FScrollBar.Position + FVisibleLines - 1 then
    Position := FScrollBar.Position + FVisibleLines - 1
  else
    Repaint;
end;

procedure TBCEditorCompletionProposalPopupWindow.ScrollBarOnScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  with Owner as TBCBaseEditor do
  begin
    SetFocus;
    // This tricks the caret into showing itself again.
    AlwaysShowCaret := False;
    AlwaysShowCaret := True;
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.MoveLine(LineCount: Integer);
begin
  if LineCount > 0 then
  begin
    if (Position < (FAssignedList.Count - LineCount)) then
      Position := Position + LineCount
    else
      Position := FAssignedList.Count - 1;
  end
  else
  begin
    if Position + LineCount > 0 then
      Position := Position + LineCount
    else
      Position := 0;
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
    AdjustScrollBarPosition;
    Position := 0;
    Repaint;
  end
  else
  begin
    i := 0;
    while (i < ItemList.Count) and (not MatchItem(i, True)) do
      inc(i);

    if i < ItemList.Count then
      Position := i
    else
      Position := 0;
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.SetItemList(const Value: TStrings);
begin
  FItemList.Assign(Value);
  FAssignedList.Assign(Value);
  CurrentString := CurrentString;
end;

procedure TBCEditorCompletionProposalPopupWindow.DoDoubleClick(Sender: TObject);
begin
  if Assigned(OnValidate) then
    OnValidate(Self, [], BCEDITOR_NONE_CHAR);
end;

procedure TBCEditorCompletionProposalPopupWindow.SetPosition(const Value: Integer);
begin
  if ((Value <= 0) and (FPosition = 0)) or (FPosition = Value) then
    exit;

  if Value <= FAssignedList.Count - 1 then
  begin
    FPosition := Value;
    if Position < FScrollBar.Position then
      FScrollBar.Position := Position
    else
    if FScrollBar.Position < (Position - FVisibleLines + 1) then
      FScrollBar.Position := Position - FVisibleLines + 1;

    Repaint;
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.SetItemHeight(const Value: Integer);
begin
  if Value <> FItemHeight then
  begin
    FItemHeight := Value;
    RecalcItemHeight;
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.SetImages(const Value: TImageList);
begin
  if FImages <> Value then
  begin
    if Assigned(FImages) then
      FImages.RemoveFreeNotification(Self);
    FImages := Value;
    if Assigned(FImages) then
      FImages.FreeNotification(Self);
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.RecalcItemHeight;
begin
  Canvas.Font.Assign(FFont);
  FFontHeight := TextHeight(Canvas, 'X');
  if FItemHeight > 0 then
    FEffectiveItemHeight := FItemHeight
  else
  begin
    FEffectiveItemHeight := FFontHeight;
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.StringListChange(Sender: TObject);
begin
  FScrollBar.Position := Position;
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

  Position := Position - (Delta * WheelClicks);
end;

procedure TBCEditorCompletionProposalPopupWindow.WMChar(var Msg: TWMChar);
begin
  DoKeyPressW(Char(Msg.CharCode))
end;

procedure TBCEditorCompletionProposalPopupWindow.AdjustMetrics;
begin
  FHeightBuffer := 0;

  if Assigned(FScrollBar) then
    if (ClientWidth >= FScrollBar.Width) and (ClientHeight >= FHeightBuffer) then
    begin
      FBitmap.Width := ClientWidth - FScrollBar.Width;
      FBitmap.Height := ClientHeight - FHeightBuffer;
    end;
end;

procedure TBCEditorCompletionProposalPopupWindow.AdjustScrollBarPosition;
var
  LOffset: Integer;
begin
  if Assigned(FScrollBar) then
  begin
    LOffset := 0;
    FScrollBar.Top := FHeightBuffer + LOffset;
    FScrollBar.Height := ClientHeight - FHeightBuffer - 2 * LOffset;
    FScrollBar.Left := ClientWidth - FScrollBar.Width - LOffset;

    if FAssignedList.Count - FVisibleLines < 0 then
    begin
      FScrollBar.PageSize := 0;
      FScrollBar.Max := 0;
      FScrollBar.Enabled := False;
    end
    else
    begin
      FScrollBar.PageSize := 0;
      FScrollBar.Max := FAssignedList.Count - FVisibleLines;
      if FScrollBar.Max <> 0 then
      begin
        FScrollBar.LargeChange := FVisibleLines;
        FScrollBar.PageSize := 1;
        FScrollBar.Enabled := True;
      end
      else
        FScrollBar.Enabled := False;
    end;
  end;
end;

procedure TBCEditorCompletionProposalPopupWindow.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  RecalcItemHeight;
  AdjustMetrics;
end;

procedure TBCEditorCompletionProposalPopupWindow.SetColumns(Value: TBCEditorProposalColumns);
begin
  FColumns.Assign(Value);
end;

procedure TBCEditorCompletionProposalPopupWindow.FontChange(Sender: TObject);
begin
  RecalcItemHeight;
  AdjustMetrics;
end;

procedure TBCEditorCompletionProposalPopupWindow.Execute(ACurrentString: string; X, Y: Integer);

  function GetWorkAreaWidth: Integer;
  begin
    Result := Screen.DesktopWidth;
  end;

  function GetWorkAreaHeight: Integer;
  begin
    Result := Screen.DesktopHeight;
  end;

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
    LHeight := FHeightBuffer + FEffectiveItemHeight * FVisibleLines + FBorderWidth;

    if LX + LWidth > GetWorkAreaWidth then
    begin
      LX := GetWorkAreaWidth - LWidth - 5;
      if LX < 0 then
        LX := 0;
    end;

    if LY + LHeight > GetWorkAreaHeight then
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
    FScrollBar.Visible := True;
    RecalcFormPlacement;
    CurrentString := ACurrentString;
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

      if FAssignedList.Count > Position then
        Value := FAssignedList[Position]
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

end.

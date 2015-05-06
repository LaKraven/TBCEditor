unit BCEditor.Print;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Printers, BCEditor.Editor.Base, BCEditor.Types,
  BCEditor.Print.Types, BCEditor.Print.HeaderFooter, BCEditor.Print.PrinterInfo, BCEditor.Print.Margins,
  BCEditor.Utils, BCEditor.Highlighter, BCEditor.Editor.Selection, BCEditor.TextDrawer;

type
  TBCEditorPageLine = class
  public
    FirstLine: Integer;
  end;

  TBCEditorPrint = class(TComponent)
  strict private
    FAbort: Boolean;
    FBlockBeginPosition: TBCEditorTextPosition;
    FBlockEndPosition: TBCEditorTextPosition;
    FCanvas: TCanvas;
    FCharWidth: Integer;
    FColors: Boolean;
    FCopies: Integer;
    FDefaultBackground: TColor;
    FDocumentTitle: string;
    FEditor: TBCBaseEditor;
    FFont: TFont;
    FFontDummy: TFont;
    FFontColor: TColor;
    FFooter: TBCEditorPrintFooter;
    FHeader: TBCEditorPrintHeader;
    FHighlight: Boolean;
    FHighlighter: TBCEditorHighlighter;
    FHighlighterRangesSet: Boolean;
    FLineHeight: Integer;
    FLineNumber: Integer;
    FLineNumbers: Boolean;
    FLineNumbersInMargin: Boolean;
    FLineOffset: Integer;
    FLines: TStrings;
    FMargins: TBCEditorPrintMargins;
    FMaxCol: Integer;
    FMaxLeftChar: Integer;
    FMaxWidth: Integer;
    FOldFont: TFont;
    FOnPrintLine: TBCEditorPrintLineEvent;
    FOnPrintStatus: TBCEditorPrintStatusEvent;
    FPageCount: Integer;
    FPageOffset: Integer;
    FPages: TList;
    FPagesCounted: Boolean;
    FPrinterInfo: TBCEditorPrinterInfo;
    FPrinting: Boolean;
    FSelectionAvailable: Boolean;
    FSelectedOnly: Boolean;
    FSelectionMode: TBCEditorSelectionMode;
    FTabWidth: Integer;
    FTextDrawer: TBCEditorTextDrawer;
    FTitle: string;
    FWrap: Boolean;
    FYPos: Integer;
    function ClipLineToRect(ALine: string): string;
    function GetPageCount: Integer;
    procedure CalculatePages;
    procedure HandleWrap(const Text: string; MaxWidth: Integer);
    procedure InitHighlighterRanges;
    procedure InitPrint;
    procedure PrintPage(Num: Integer);
    procedure RestoreCurrentFont;
    procedure SaveCurrentFont;
    procedure SetCharWidth(const Value: Integer);
    procedure SetEditor(const Value: TBCBaseEditor);
    procedure SetFont(const Value: TFont);
    procedure SetFooter(const Value: TBCEditorPrintFooter);
    procedure SetHeader(const Value: TBCEditorPrintHeader);
    procedure SetHighlighter(const Value: TBCEditorHighlighter);
    procedure SetLines(const Value: TStrings);
    procedure SetMargins(const Value: TBCEditorPrintMargins);
    procedure SetMaxLeftChar(const Value: Integer);
    procedure SetPixelsPerInch;
    procedure TextOut(const Text: string; AList: TList);
    procedure WriteLine(const Text: string);
    procedure WriteLineNumber;
  protected
    procedure PrintLine(LineNumber, PageNumber: Integer); virtual;
    procedure PrintStatus(Status: TBCEditorPrintStatus; PageNumber: Integer; var Abort: Boolean); virtual;
    property CharWidth: Integer read FCharWidth write SetCharWidth;
    property MaxLeftChar: Integer read FMaxLeftChar write SetMaxLeftChar;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromStream(AStream: TStream);
    procedure Print;
    procedure PrintRange(StartPage, EndPage: Integer);
    procedure PrintToCanvas(ACanvas: TCanvas; PageNumber: Integer);
    procedure SaveToStream(AStream: TStream);
    procedure UpdatePages(ACanvas: TCanvas);
    property Editor: TBCBaseEditor read FEditor write SetEditor;
    property PageCount: Integer read GetPageCount;
    property PrinterInfo: TBCEditorPrinterInfo read FPrinterInfo;
  published
    property Color: TColor read FDefaultBackground write FDefaultBackground;
    property Colors: Boolean read FColors write FColors default False;
    property Copies: Integer read FCopies write FCopies;
    property DocumentTitle: string read FDocumentTitle write FDocumentTitle;
    property Font: TFont read FFont write SetFont;
    property Footer: TBCEditorPrintFooter read FFooter write SetFooter;
    property Header: TBCEditorPrintHeader read FHeader write SetHeader;
    property Highlight: Boolean read FHighlight write FHighlight default True;
    property Highlighter: TBCEditorHighlighter read FHighlighter write SetHighlighter;
    property LineNumbers: Boolean read FLineNumbers write FLineNumbers default False;
    property LineNumbersInMargin: Boolean read FLineNumbersInMargin write FLineNumbersInMargin default False;
    property LineOffset: Integer read FLineOffset write FLineOffset default 0;
    //property Lines: TStrings read FLines write SetLines;
    property Margins: TBCEditorPrintMargins read FMargins write SetMargins;
    property OnPrintLine: TBCEditorPrintLineEvent read FOnPrintLine write FOnPrintLine;
    property OnPrintStatus: TBCEditorPrintStatusEvent read FOnPrintStatus write FOnPrintStatus;
    property PageOffset: Integer read FPageOffset write FPageOffset default 0;
    property SelectedOnly: Boolean read FSelectedOnly write FSelectedOnly default False;
    property TabWidth: Integer read FTabWidth write FTabWidth;
    property Title: string read FTitle write FTitle;
    property Wrap: Boolean read FWrap write FWrap default True;
  end;

implementation

uses
  System.Math, System.UITypes, BCEditor.Highlighter.Attributes, BCEditor.Consts;

{ TBCEditorPrint }

constructor TBCEditorPrint.Create(AOwner: TComponent);
begin
  inherited;
  FCopies := 1;
  FFooter := TBCEditorPrintFooter.Create;
  FHeader := TBCEditorPrintHeader.Create;
  FLines := TStringList.Create;
  FMargins := TBCEditorPrintMargins.Create;
  FPrinterInfo := TBCEditorPrinterInfo.Create;
  FFont := TFont.Create;
  FOldFont := TFont.Create;
  MaxLeftChar := 1024;
  FWrap := True;
  FHighlight := True;
  FColors := False;
  FLineNumbers := False;
  FLineOffset := 0;
  FPageOffset := 0;
  FLineNumbersInMargin := False;
  FPages := TList.Create;
  FTabWidth := 8;
  FDefaultBackground := clWhite;
  FFontDummy := TFont.Create;
  with FFontDummy do
  begin
    Name := 'Courier New';
    Size := 10;
  end;
  FTextDrawer := TBCEditorTextDrawer.Create([fsBold], FFontDummy);
end;

destructor TBCEditorPrint.Destroy;
var
  i: Integer;
begin
  FFooter.Free;
  FHeader.Free;
  FLines.Free;
  FMargins.Free;
  FPrinterInfo.Free;
  FFont.Free;
  FOldFont.Free;
  for i := 0 to FPages.Count - 1 do
    TBCEditorPageLine(FPages[i]).Free;
  FPages.Free;
  FTextDrawer.Free;
  FFontDummy.Free;
  inherited;
end;

procedure TBCEditorPrint.SetLines(const Value: TStrings);
var
  i, j: Integer;
  TabConvertProc: TBCEditorTabConvertProc;
  S: string;
  HasTabs: Boolean;
begin
  TabConvertProc := GetBestTabConvertProc(FTabWidth);
  with FLines do
  begin
    BeginUpdate;
    try
      Clear;
      for i := 0 to Value.Count - 1 do
      begin
        S := TabConvertProc(Value[i], FTabWidth, HasTabs);
        j := Pos(BCEDITOR_TAB_CHAR, S);
        while j > 0 do
        begin
          S[j] := ' ';
          j := Pos(BCEDITOR_TAB_CHAR, S);
        end;
        Add(S);
      end;
    finally
      EndUpdate;
    end;
  end;
  FHighlighterRangesSet := False;
  FPagesCounted := False;
end;

procedure TBCEditorPrint.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  FPagesCounted := False;
end;

procedure TBCEditorPrint.SetCharWidth(const Value: Integer);
begin
  if FCharWidth <> Value then
    FCharWidth := Value;
end;

procedure TBCEditorPrint.SetMaxLeftChar(const Value: Integer);
begin
  if FMaxLeftChar <> Value then
    FMaxLeftChar := Value;
end;

procedure TBCEditorPrint.SetHighlighter(const Value: TBCEditorHighlighter);
begin
  FHighlighter := Value;
  FHighlighterRangesSet := False;
  FPagesCounted := False;
end;

procedure TBCEditorPrint.InitPrint;
var
  TempSize: Integer;
  TempTextMetrics: TTextMetric;
begin
  FFontColor := FFont.Color;

  FCanvas.Font.Assign(FFont);
  if not FPrinting then
  begin
    SetPixelsPerInch;
    TempSize := FCanvas.Font.Size;
    FCanvas.Font.PixelsPerInch := FFont.PixelsPerInch;
    FCanvas.Font.Size := TempSize;
  end;
  FCanvas.Font.Style := [fsBold, fsItalic, fsUnderline, fsStrikeOut];

  GetTextMetrics(FCanvas.Handle, TempTextMetrics);
  CharWidth := TempTextMetrics.tmAveCharWidth;
  FLineHeight := TempTextMetrics.tmHeight + TempTextMetrics.tmExternalLeading;

  FTextDrawer.SetBaseFont(FFont);
  FTextDrawer.Style := FFont.Style;

  FMargins.InitPage(FCanvas, 1, FPrinterInfo, FLineNumbers, FLineNumbersInMargin, FLines.Count - 1 + FLineOffset);
  CalculatePages;
  FHeader.InitPrint(FCanvas, FPageCount, FTitle, FMargins);
  FFooter.InitPrint(FCanvas, FPageCount, FTitle, FMargins);
end;

procedure TBCEditorPrint.SetPixelsPerInch;
var
  TempSize: Integer;
begin
  FHeader.SetPixelsPerInch(FPrinterInfo.YPixPerInch);
  FFooter.SetPixelsPerInch(FPrinterInfo.YPixPerInch);
  TempSize := FFont.Size;
  FFont.PixelsPerInch := FPrinterInfo.YPixPerInch;
  FFont.Size := TempSize;
end;

procedure TBCEditorPrint.InitHighlighterRanges;
var
  i: Integer;
begin
  if not FHighlighterRangesSet and Assigned(FHighlighter) and (FLines.Count > 0) then
  begin
    FHighlighter.ResetCurrentRange;
    FLines.Objects[0] := FHighlighter.GetCurrentRange;
    i := 1;
    while i < FLines.Count do
    begin
      FHighlighter.SetCurrentLine(FLines[i - 1], i - 1);
      FHighlighter.NextToEol;
      FLines.Objects[i] := FHighlighter.GetCurrentRange;
      Inc(i);
    end;
    FHighlighterRangesSet := True;
  end;
end;

procedure TBCEditorPrint.CalculatePages;
var
  S, Text: string;
  i, j: Integer;
  LList: TList;
  YPos: Integer;
  PageLine: TBCEditorPageLine;
  StartLine, EndLine: Integer;
  SelectionStart, SelectionLength: Integer;

  procedure CountWrapped;
  var
    j: Integer;
  begin
    for j := 0 to LList.Count - 1 do
      YPos := YPos + FLineHeight;
  end;

begin
  InitHighlighterRanges;
  for i := 0 to FPages.Count - 1 do
    TBCEditorPageLine(FPages[i]).Free;
  FPages.Clear;
  FMaxWidth := FMargins.PixelRight - FMargins.PixelLeft;
  S := '';
  FMaxCol := 0;
  while TextWidth(FCanvas, S) < FMaxWidth do
  begin
    S := S + 'W';
    FMaxCol := FMaxCol + 1;
  end;
  FMaxCol := FMaxCol - 1;
  S := StringOfChar('W', FMaxCol);
  FMaxWidth := TextWidth(FCanvas, S);
  FPageCount := 1;
  PageLine := TBCEditorPageLine.Create;
  PageLine.FirstLine := 0;
  FPages.Add(PageLine);
  YPos := FMargins.PixelTop;
  if SelectedOnly then
  begin
    StartLine := FBlockBeginPosition.Line - 1;
    EndLine := FBlockEndPosition.Line - 1;
  end
  else
  begin
    StartLine := 0;
    EndLine := FLines.Count - 1;
  end;
  for i := StartLine to EndLine do
  begin
    if not FSelectedOnly or (FSelectionMode = smLine) then
      Text := FLines[i]
    else
    begin
      if (FSelectionMode = smColumn) or (i = FBlockBeginPosition.Line - 1) then
        SelectionStart := FBlockBeginPosition.Char
      else
        SelectionStart := 1;
      if (FSelectionMode = smColumn) or (i = FBlockEndPosition.Line - 1) then
        SelectionLength := FBlockEndPosition.Char - SelectionStart
      else
        SelectionLength := MaxInt;
      Text := Copy(FLines[i], SelectionStart, SelectionLength);
    end;
    if YPos + FLineHeight > FMargins.PixelBottom then
    begin
      YPos := FMargins.PixelTop;
      FPageCount := FPageCount + 1;
      PageLine := TBCEditorPageLine.Create;
      PageLine.FirstLine := i;
      FPages.Add(PageLine);
    end;
    if Wrap and (TextWidth(FCanvas, Text) > FMaxWidth) then
    begin
      LList := TList.Create;
      try
        if WrapTextEx(Text, [' ', '-', BCEDITOR_TAB_CHAR, ','], FMaxCol, LList) then
          CountWrapped
        else
        begin
          if WrapTextEx(Text, [';', ')', '.'], FMaxCol, LList) then
            CountWrapped
          else
            while Length(Text) > 0 do
            begin
              S := Copy(Text, 1, FMaxCol);
              Delete(Text, 1, FMaxCol);
              if Length(Text) > 0 then
                YPos := YPos + FLineHeight;
            end;
        end;
        for j := 0 to LList.Count - 1 do
          TBCEditorWrapPosition(LList[j]).Free;
      finally
        LList.Free;
      end;
    end;
    YPos := YPos + FLineHeight;
  end;
  FPagesCounted := True;
end;

procedure TBCEditorPrint.WriteLineNumber;
var
  S: string;
begin
  SaveCurrentFont;
  S := (FLineNumber + FLineOffset).ToString + ': ';
  FCanvas.Brush.Color := FDefaultBackground;
  FCanvas.Font.Style := [];
  FCanvas.Font.Color := clBlack;
  FCanvas.TextOut(FMargins.PixelLeft - FCanvas.TextWidth(S), FYPos, S);
  RestoreCurrentFont;
end;

procedure TBCEditorPrint.HandleWrap(const Text: string; MaxWidth: Integer);
var
  S: string;
  LList: TList;
  j: Integer;

  procedure WrapPrimitive;
  var
    i: Integer;
    WrapPos: TBCEditorWrapPosition;
  begin
    i := 1;
    while i <= Length(Text) do
    begin
      S := '';
      while (Length(S) < FMaxCol) and (i <= Length(Text)) do
      begin
        S := S + Text[i];
        i := i + 1;
      end;
      WrapPos := TBCEditorWrapPosition.Create;
      WrapPos.Index := i - 1;
      LList.Add(WrapPos);
      if (Length(S) - i) <= FMaxCol then
        Break;
    end;
  end;

begin
  S := '';
  LList := TList.Create;
  try
    if WrapTextEx(Text, [' ', '-', BCEDITOR_TAB_CHAR, ','], FMaxCol, LList) then
      TextOut(Text, LList)
    else
    begin
      if WrapTextEx(Text, [';', ')', '.'], FMaxCol, LList) then
        TextOut(Text, LList)
      else
      begin
        WrapPrimitive;
        TextOut(Text, LList)
      end;
    end;
    for j := 0 to LList.Count - 1 do
      TBCEditorWrapPosition(LList[j]).Free;
  finally
    LList.Free;
  end;
end;

procedure TBCEditorPrint.SaveCurrentFont;
begin
  FOldFont.Assign(FCanvas.Font);
end;

procedure TBCEditorPrint.RestoreCurrentFont;
begin
  FCanvas.Font.Assign(FOldFont);
end;

function TBCEditorPrint.ClipLineToRect(ALine: string): string;
begin
  while FCanvas.TextWidth(ALine) > FMaxWidth do
    SetLength(ALine, Length(ALine) - 1);

  Result := ALine;
end;

procedure TBCEditorPrint.TextOut(const Text: string; AList: TList);
var
  Token: string;
  TokenPos: Integer;
  Attr: TBCEditorHighlighterAttribute;
  AColor: TColor;
  TokenStart: Integer;
  LCount: Integer;
  Handled: Boolean;
  aStr: string;
  i, WrapPos, OldWrapPos: Integer;
  Lines: TStringList;
  ClipRect: TRect;

  procedure ClippedTextOut(X, Y: Integer; Text: string);
  begin
    Text := ClipLineToRect(Text);
    if Highlight and Assigned(FHighlighter) and (FLines.Count > 0) then
    begin
      SetBkMode(FCanvas.Handle, TRANSPARENT);
      FTextDrawer.ExtTextOut(X, Y, [], ClipRect, PChar(Text), Length(Text)); //ExtTextOutDistance);
      SetBkMode(FCanvas.Handle, OPAQUE);
    end
    else
      ExtTextOut(FCanvas.Handle, X, Y, 0, nil, PChar(Text), Length(Text), nil);
  end;

  procedure SplitToken;
  var
    aStr: string;
    Last: Integer;
    FirstPos: Integer;
    TokenEnd: Integer;
  begin
    Last := TokenPos;
    FirstPos := TokenPos;
    TokenEnd := TokenPos + Length(Token);
    while (LCount < AList.Count) and (TokenEnd > TBCEditorWrapPosition(AList[LCount]).Index) do
    begin
      aStr := Copy(Text, Last + 1, TBCEditorWrapPosition(AList[LCount]).Index - Last);
      Last := TBCEditorWrapPosition(AList[LCount]).Index;
      ClippedTextOut(FMargins.PixelLeft + FirstPos * FTextDrawer.CharWidth, FYPos, aStr);
      FirstPos := 0;
      LCount := LCount + 1;
      FYPos := FYPos + FLineHeight;
    end;
    aStr := Copy(Text, Last + 1, TokenEnd - Last);
    ClippedTextOut(FMargins.PixelLeft + FirstPos * FTextDrawer.CharWidth, FYPos, aStr);
    TokenStart := TokenPos + Length(Token) - Length(aStr);
  end;

begin
  FTextDrawer.BeginDrawing(FCanvas.Handle);
  with FMargins do
    ClipRect := Rect(PixelLeft, PixelTop, PixelRight, PixelBottom);

  if Highlight and Assigned(FHighlighter) and (FLines.Count > 0) then
  begin
    SaveCurrentFont;
    FHighlighter.SetCurrentRange(FLines.Objects[FLineNumber - 1]);
    FHighlighter.SetCurrentLine(Text, FLineNumber);
    Token := '';
    TokenStart := 0;
    LCount := 0;
    while not FHighlighter.GetEol do
    begin
      Token := FHighlighter.GetToken;
      TokenPos := FHighlighter.GetTokenPosition;
      Attr := FHighlighter.GetTokenAttribute;

      if Assigned(Attr) then
      begin
        FCanvas.Font.Style := Attr.Style;
        if FColors then
        begin
          AColor := Attr.Foreground;
          if AColor = clNone then
            AColor := FFont.Color;
          FCanvas.Font.Color := AColor;
          AColor := Attr.Background;
          if AColor = clNone then
            AColor := FDefaultBackground;
          FCanvas.Brush.Color := AColor;
        end
        else
        begin
          FCanvas.Font.Color := FFontColor;
          FCanvas.Brush.Color := FDefaultBackground;
        end;
      end
      else
      begin
        FCanvas.Font.Color := FFontColor;
        FCanvas.Brush.Color := FDefaultBackground;
      end;
      Handled := False;
      if Assigned(AList) then
        if LCount < AList.Count then
        begin
          if TokenPos >= TBCEditorWrapPosition(AList[LCount]).Index then
          begin
            LCount := LCount + 1;
            TokenStart := TokenPos;
            FYPos := FYPos + FLineHeight;
          end
          else
          if TokenPos + Length(Token) > TBCEditorWrapPosition(AList[LCount]).Index then
          begin
            Handled := True;
            SplitToken;
          end;
        end;
      if not Handled then
        ClippedTextOut(FMargins.PixelLeft + (TokenPos - TokenStart) * FTextDrawer.CharWidth, FYPos, Token);
      FHighlighter.Next;
    end;
    RestoreCurrentFont;
  end
  else
  begin
    Lines := TStringList.Create;
    try
      OldWrapPos := 0;
      if Assigned(AList) then
        for i := 0 to AList.Count - 1 do
        begin
          WrapPos := TBCEditorWrapPosition(AList[i]).Index;
          if i = 0 then
            aStr := Copy(Text, 1, WrapPos)
          else
            aStr := Copy(Text, OldWrapPos + 1, WrapPos - OldWrapPos);
          Lines.Add(aStr);
          OldWrapPos := WrapPos;
        end;
      if Length(Text) > 0 then
        Lines.Add(Copy(Text, OldWrapPos + 1, MaxInt));

      for i := 0 to Lines.Count - 1 do
      begin
        ClippedTextOut(FMargins.PixelLeft, FYPos, Lines[i]);
        if i < Lines.Count - 1 then
          FYPos := FYPos + FLineHeight;
      end;
    finally
      Lines.Free;
    end
  end;
  FTextDrawer.EndDrawing;
end;

procedure TBCEditorPrint.WriteLine(const Text: string);
begin
  if FLineNumbers then
    WriteLineNumber;
  if Wrap and (FCanvas.TextWidth(Text) > FMaxWidth) then
    HandleWrap(Text, FMaxWidth)
  else
    TextOut(Text, nil);
  FYPos := FYPos + FLineHeight;
end;

procedure TBCEditorPrint.PrintPage(Num: Integer);
var
  i, EndLine: Integer;
  SelectionStart, SelectionLen: Integer;
begin
  PrintStatus(psNewPage, Num, FAbort);
  if not FAbort then
  begin
    FCanvas.Brush.Color := Color;
    with FMargins do
      FCanvas.FillRect(Rect(PixelLeft, PixelTop, PixelRight, PixelBottom));
    FMargins.InitPage(FCanvas, Num, FPrinterInfo, FLineNumbers, FLineNumbersInMargin, FLines.Count - 1 + FLineOffset);
    FHeader.Print(FCanvas, Num + FPageOffset);
    if FPages.Count > 0 then
    begin
      FYPos := FMargins.PixelTop;
      if Num = FPageCount then
        EndLine := FLines.Count - 1
      else
        EndLine := TBCEditorPageLine(FPages[Num]).FirstLine - 1;
      for i := TBCEditorPageLine(FPages[Num - 1]).FirstLine to EndLine do
      begin
        FLineNumber := i + 1;
        if (not FSelectedOnly or ((i >= FBlockBeginPosition.Line - 1) and (i <= FBlockEndPosition.Line - 1))) then
        begin
          if (not FSelectedOnly or (FSelectionMode = smLine)) then
            WriteLine(FLines[i])
          else
          begin
            if (FSelectionMode = smColumn) or (i = FBlockBeginPosition.Line - 1) then
              SelectionStart := FBlockBeginPosition.Char
            else
              SelectionStart := 1;
            if (FSelectionMode = smColumn) or (i = FBlockEndPosition.Line - 1) then
              SelectionLen := FBlockEndPosition.Char - SelectionStart
            else
              SelectionLen := MaxInt;
            WriteLine(Copy(FLines[i], SelectionStart, SelectionLen));
          end;
          PrintLine(i + 1, Num);
        end;
      end;
    end;
    FFooter.Print(FCanvas, Num + FPageOffset);
  end;
end;

procedure TBCEditorPrint.UpdatePages(ACanvas: TCanvas);
begin
  FCanvas := ACanvas;
  FPrinterInfo.UpdatePrinter;
  InitPrint;
end;

procedure TBCEditorPrint.PrintToCanvas(ACanvas: TCanvas; PageNumber: Integer);
begin
  FAbort := False;
  FPrinting := False;
  FCanvas := ACanvas;
  PrintPage(PageNumber);
end;

procedure TBCEditorPrint.Print;
begin
  PrintRange(1, -1);
end;

procedure TBCEditorPrint.PrintRange(StartPage, EndPage: Integer);
var
  i, j: Integer;
begin
  if FSelectedOnly and not FSelectionAvailable then
    Exit;

  FPrinting := True;
  FAbort := False;
  if FDocumentTitle <> '' then
    Printer.Title := FDocumentTitle
  else
    Printer.Title := FTitle;
  Printer.BeginDoc;
  PrintStatus(psBegin, StartPage, FAbort);
  UpdatePages(Printer.Canvas);

  for j := 1 to Copies do
  begin
    i := StartPage;
    if EndPage < 0 then
      EndPage := FPageCount;
    while (i <= EndPage) and (not FAbort) do
    begin
      PrintPage(i);
      if ((i < EndPage) or (j < Copies)) and not FAbort then
        Printer.NewPage;
      i := i + 1;
    end;
  end;
  if not FAbort then
    PrintStatus(psEnd, EndPage, FAbort);
  Printer.EndDoc;
  FPrinting := False;
end;

procedure TBCEditorPrint.PrintLine(LineNumber, PageNumber: Integer);
begin
  if Assigned(FOnPrintLine) then
    FOnPrintLine(Self, LineNumber, PageNumber);
end;

procedure TBCEditorPrint.PrintStatus(Status: TBCEditorPrintStatus; PageNumber: Integer; var Abort: Boolean);
begin
  Abort := False;
  if Assigned(FOnPrintStatus) then
    FOnPrintStatus(Self, Status, PageNumber, Abort);
  if Abort then
    if FPrinting then
      Printer.Abort;
end;

function TBCEditorPrint.GetPageCount: Integer;
var
  LCanvas: TCanvas;
  LHandle: HDC;
begin
  Result := 0;
  if FPagesCounted then
    Result := FPageCount
  else
  begin
    LCanvas := TCanvas.Create;
    LHandle := GetDC(0);
    try
      if LHandle <> 0 then
      begin
        LCanvas.Handle := LHandle;
        UpdatePages(LCanvas);
        LCanvas.Handle := 0;
        Result := FPageCount;
        FPagesCounted := True;
      end;
    finally
      ReleaseDC(0, LHandle);
      LCanvas.Free;
    end;
  end;
end;

procedure TBCEditorPrint.SetEditor(const Value: TBCBaseEditor);
begin
  FEditor := Value;
  Highlighter := Value.Highlighter;
  Font := Value.Font;
  CharWidth := Value.CharWidth;
  FTabWidth := Value.Tabs.Width;
  SetLines(Value.Lines);
  FSelectionAvailable := Value.SelectionAvailable;
  FBlockBeginPosition := Value.SelectionBeginPosition;
  FBlockEndPosition := Value.SelectionEndPosition;
  FSelectionMode := Value.Selection.Mode;
end;

procedure TBCEditorPrint.LoadFromStream(AStream: TStream);
var
  LLength, BufferSize: Integer;
  Buffer: PChar;
begin
  FHeader.LoadFromStream(AStream);
  FFooter.LoadFromStream(AStream);
  FMargins.LoadFromStream(AStream);
  with AStream do
  begin
    Read(LLength, SizeOf(LLength));
    BufferSize := LLength * SizeOf(Char);
    GetMem(Buffer, BufferSize + SizeOf(Char));
    try
      Read(Buffer^, BufferSize);
      Buffer[BufferSize div SizeOf(Char)] := BCEDITOR_NONE_CHAR;
      FTitle := Buffer;
    finally
      FreeMem(Buffer);
    end;
    Read(LLength, SizeOf(LLength));
    BufferSize := LLength * SizeOf(Char);
    GetMem(Buffer, BufferSize + SizeOf(Char));
    try
      Read(Buffer^, BufferSize);
      Buffer[BufferSize div SizeOf(Char)] := BCEDITOR_NONE_CHAR;
      FDocumentTitle := Buffer;
    finally
      FreeMem(Buffer);
    end;
    Read(FWrap, SizeOf(FWrap));
    Read(FHighlight, SizeOf(FHighlight));
    Read(FColors, SizeOf(FColors));
    Read(FLineNumbers, SizeOf(FLineNumbers));
    Read(FLineOffset, SizeOf(FLineOffset));
    Read(FPageOffset, SizeOf(FPageOffset));
  end;
end;

procedure TBCEditorPrint.SaveToStream(AStream: TStream);
var
  LLength: Integer;
begin
  FHeader.SaveToStream(AStream);
  FFooter.SaveToStream(AStream);
  FMargins.SaveToStream(AStream);
  with AStream do
  begin
    LLength := Length(FTitle);
    Write(LLength, SizeOf(LLength));
    Write(PChar(FTitle)^, LLength * SizeOf(Char));
    LLength := Length(FDocumentTitle);
    Write(LLength, SizeOf(LLength));
    Write(PChar(FDocumentTitle)^, LLength * SizeOf(Char));
    Write(FWrap, SizeOf(FWrap));
    Write(FHighlight, SizeOf(FHighlight));
    Write(FColors, SizeOf(FColors));
    Write(FLineNumbers, SizeOf(FLineNumbers));
    Write(FLineOffset, SizeOf(FLineOffset));
    Write(FPageOffset, SizeOf(FPageOffset));
  end;
end;

procedure TBCEditorPrint.SetFooter(const Value: TBCEditorPrintFooter);
begin
  FFooter.Assign(Value);
end;

procedure TBCEditorPrint.SetHeader(const Value: TBCEditorPrintHeader);
begin
  FHeader.Assign(Value);
end;

procedure TBCEditorPrint.SetMargins(const Value: TBCEditorPrintMargins);
begin
  FMargins.Assign(Value);
end;

end.

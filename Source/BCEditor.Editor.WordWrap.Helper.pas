unit BCEditor.Editor.WordWrap.Helper;

interface

uses
  System.SysUtils, System.Classes, BCEditor.Types, BCEditor.Lines, Vcl.Controls;

const
  MaxIndex = MaxInt div 16;

type
  TBCEditorLineIndex = 0 .. MaxIndex;
  TBCEditorRowIndex = 0 .. MaxIndex;

  TBCEditorRowIndexArray = array [TBCEditorLineIndex] of TBCEditorRowIndex;
  PBCEditorRowIndexArray = ^TBCEditorRowIndexArray;

  TBCEditorRowLengthArray = array [TBCEditorRowIndex] of Word;
  PBCEditorRowLengthArray = ^TBCEditorRowLengthArray;

  TBCEditorWordWrapHelper = class
  strict private
    FEditor: TCustomControl;
    FLineCapacity: Integer;
    FLineCount: Integer;
    FLineOffsets: PBCEditorRowIndexArray;
    FMaxRowLength: Word;
    FMinRowLength: Word;
    FRowCapacity: Integer;
    FRowLengths: PBCEditorRowLengthArray;
    procedure GrowLines(AMinSize: Integer);
    procedure GrowRows(AMinSize: Integer);
    procedure MoveLines(AStart: TBCEditorLineIndex; AMoveBy: Integer);
    procedure MoveRows(AStart: TBCEditorRowIndex; AMoveBy: Integer);
    procedure SetEmpty;
  protected
    function GetRealRowCount: Integer;
    function RewrapLine(AIndex: TBCEditorLineIndex): Integer;
    procedure TrimArrays;
    procedure WrapLines;
    property Editor: TCustomControl read FEditor;
    property LineOffsets: PBCEditorRowIndexArray read FLineOffsets;
    property RowLengths: PBCEditorRowLengthArray read FRowLengths;
  public
    constructor Create(AOwner: TCustomControl);
    destructor Destroy; override;

    function DisplayToTextPosition(const ADisplayPosition: TBCEditorDisplayPosition): TBCEditorTextPosition;
    function GetRowCount: Integer; overload;
    function GetRowCount(ALine: Integer): Integer; overload;
    function GetRowLength(ARow: Integer): Integer;
    function LinesDeleted(AIndex: Integer; ACount: Integer): Integer;
    //function LinesFolded(AFromLine, AToLine: Integer): Integer;
    function LinesInserted(AIndex: Integer; ACount: Integer): Integer;
    function LinesPutted(AIndex: Integer; ACount: Integer): Integer;
    function LinesUnFolded(AFromLine, AToLine: Integer): Integer;
    function LineToRealRow(const ALine: Integer): Integer;
    function LineToRow(const ALine: Integer): Integer;
    function RealRowToLine(const ARow: Integer): Integer;
    function RowToLine(const ARow: Integer): Integer;
    function TextToDisplayPosition(const ATextPosition: TBCEditorTextPosition): TBCEditorDisplayPosition;
    procedure DisplayChanged;
    procedure Reset;
    property LineCount: Integer read FLineCount;
    property RealLineCount: Integer read GetRealRowCount;
  end;

implementation

uses
  System.RTLConsts, System.Math, BCEditor.Editor.Base, BCEditor.Editor.WordWrap, BCEditor.Consts;

{ TBCEditorWordWrapHelper }

function TBCEditorWordWrapHelper.TextToDisplayPosition(const ATextPosition: TBCEditorTextPosition): TBCEditorDisplayPosition;
var
  StartRow: Integer;
  Row: Integer;
  RowLength: Integer;
begin
  Assert(ATextPosition.Char > 0);
  Assert(ATextPosition.Line > 0);

  if FLineCount < ATextPosition.Line then
  begin
    Result.Column := ATextPosition.Char;
    Result.Row := GetRowCount + (ATextPosition.Line - FLineCount);
    Exit;
  end;

  if ATextPosition.Line = 1 then
    StartRow := 0
  else
    StartRow := FLineOffsets[ATextPosition.Line - 2];

  { Search for correct column and row pos }
  RowLength := 0;
  for Row := StartRow to FLineOffsets[ATextPosition.Line - 1] - 1 do
  begin
    Inc(RowLength, FRowLengths[Row]);

    if ATextPosition.Char <= RowLength then
    begin
      Result.Column := ATextPosition.Char - RowLength + FRowLengths[Row]{ + Indent};
      Result.Row := Row + 1;
      Exit;
    end;
  end;

  Result.Column := ATextPosition.Char - RowLength + FRowLengths[FLineOffsets[ATextPosition.Line - 1] - 1] {+ Indent};
  Result.Row := FLineOffsets[ATextPosition.Line - 1];
end;

constructor TBCEditorWordWrapHelper.Create(AOwner: TCustomControl);
begin
  inherited Create;
  FEditor := AOwner as TBCBaseEditor;
  Reset;
end;

destructor TBCEditorWordWrapHelper.Destroy;
begin
  inherited;
  FreeMem(FLineOffsets);
  FreeMem(FRowLengths);
end;

procedure TBCEditorWordWrapHelper.DisplayChanged;
begin
  if (FEditor as TBCBaseEditor).WordWrap.Style = wwsRightMargin then
  begin
    if (FEditor as TBCBaseEditor).RightMargin.Position <> FMaxRowLength then
      Reset
  end
  else
  if (FEditor as TBCBaseEditor).CharsInWindow <> FMaxRowLength then
    Reset;
end;

function TBCEditorWordWrapHelper.GetRowCount(ALine: Integer): Integer;
begin
  if FLineCount > 0 then
    Result := FLineOffsets[ALine - 1] - FLineOffsets[ALine - 2]
  else
    Result := 0;
end;

function TBCEditorWordWrapHelper.GetRealRowCount: Integer;
begin
  if FLineCount > 0 then
    Result := FLineOffsets[FLineCount - 1]
  else
    Result := 0;
end;

function TBCEditorWordWrapHelper.GetRowCount: Integer;
begin
  if FLineCount > 0 then
    Result := FLineOffsets[FLineCount - 1]
  else
    Result := 0;
end;

function TBCEditorWordWrapHelper.GetRowLength(ARow: Integer): Integer;
begin
  if (ARow < 0) or (ARow > GetRowCount) then
    TList.Error(SListIndexError, ARow);

  Result := FRowLengths[ARow - 1];
end;

procedure TBCEditorWordWrapHelper.GrowLines(AMinSize: Integer);
const
  vStepSize = 256;
begin
  Assert(AMinSize > 0);
  if AMinSize > FLineCapacity then
  begin
    AMinSize := AMinSize + vStepSize - (AMinSize mod vStepSize);
    ReallocMem(FLineOffsets, AMinSize * SizeOf(TBCEditorRowIndex));
    FLineCapacity := AMinSize;
  end;
end;

procedure TBCEditorWordWrapHelper.GrowRows(AMinSize: Integer);
const
  StepSize = 512;
begin
  Assert(AMinSize > 0);
  if AMinSize > FRowCapacity then
  begin
    AMinSize := AMinSize + StepSize - (AMinSize mod StepSize);
    ReallocMem(FRowLengths, AMinSize * SizeOf(Word));
    FRowCapacity := AMinSize;
  end;
end;

function TBCEditorWordWrapHelper.LinesDeleted(AIndex: Integer; ACount: Integer): Integer;
var
  StartRow: Integer;
  EndRow: Integer;
  Line: Integer;
begin
  Result := 0;
  if FMaxRowLength = 0 then
    Exit;

  Assert(AIndex >= 0);
  Assert(ACount >= 1);
  Assert(AIndex + ACount <= LineCount);

  if AIndex = 0 then
    StartRow := 0
  else
    StartRow := FLineOffsets[AIndex - 1];
  EndRow := FLineOffsets[AIndex + ACount - 1];
  Result := EndRow - StartRow;
  if EndRow < GetRealRowCount then
    MoveRows(EndRow, -Result);
  MoveLines(AIndex + ACount, -ACount);
  Dec(FLineCount, ACount);
  for Line := AIndex to LineCount - 1 do
    Dec(FLineOffsets[Line], Result);
end;

{function TBCEditorWordWrapHelper.LinesFolded(AFromLine, AToLine: Integer): Integer;
begin
  Result := 0;
end; }

function TBCEditorWordWrapHelper.LinesInserted(AIndex: Integer; ACount: Integer): Integer;
var
  PrevOffset: TBCEditorRowIndex;
  Line: Integer;
begin
  Result := 0;
  if FMaxRowLength = 0 then
    Exit;
  Assert(AIndex >= 0);
  Assert(ACount >= 1);
  Assert(AIndex <= LineCount);
  GrowLines(LineCount + ACount);
  if AIndex < LineCount then
  begin
    Inc(FLineCount, ACount);
    MoveLines(AIndex, ACount);
  end
  else
    Inc(FLineCount, ACount);
  if AIndex = 0 then
    PrevOffset := 0
  else
    PrevOffset := FLineOffsets[AIndex - 1];
  for Line := AIndex to AIndex + ACount - 1 do
    FLineOffsets[Line] := PrevOffset;
  for Line := AIndex to AIndex + ACount - 1 do
    Inc(Result, RewrapLine(Line));
end;

function TBCEditorWordWrapHelper.LinesPutted(AIndex: Integer; ACount: Integer): Integer;
var
  Line: Integer;
begin
  Result := 0;
  if FMaxRowLength = 0 then
    Exit;
  Assert(AIndex >= 0);
  Assert(ACount >= 1);
  Assert(AIndex + ACount <= LineCount);
  for Line := AIndex to AIndex + ACount - 1 do
    Inc(Result, RewrapLine(Line));
end;

function TBCEditorWordWrapHelper.LinesUnfolded(AFromLine, AToLine: Integer): Integer;
begin
  Result := 0;
end;

function TBCEditorWordWrapHelper.LineToRealRow(const ALine: Integer): Integer;
begin
  Assert(ALine > 0);

  if FLineCount < ALine then
  begin
    Result := GetRowCount + ALine - FLineCount;
    Exit;
  end;

  if ALine = 1 then
    Result := 1
  else
    Result := FLineOffsets[ALine - 2] + 1;
end;

function TBCEditorWordWrapHelper.LineToRow(const ALine: Integer): Integer;
begin
  Result := LineToRealRow(ALine);
end;

procedure TBCEditorWordWrapHelper.MoveLines(AStart: TBCEditorLineIndex; AMoveBy: Integer);
var
  LMoveCount: Integer;
begin
  Assert(AMoveBy <> 0);
  Assert(AStart + AMoveBy >= 0);
  Assert(AStart + AMoveBy < LineCount);
  LMoveCount := LineCount - AStart;
  if AMoveBy > 0 then
    Dec(LMoveCount, AMoveBy);
  Move(FLineOffsets[AStart], FLineOffsets[AStart + AMoveBy], LMoveCount * SizeOf(TBCEditorRowIndex));
end;

procedure TBCEditorWordWrapHelper.MoveRows(AStart: TBCEditorRowIndex; AMoveBy: Integer);
var
  LMoveCount: Integer;
begin
  Assert(AMoveBy <> 0);
  Assert(AStart + AMoveBy >= 0);
  Assert(AStart + AMoveBy < GetRealRowCount);
  LMoveCount := GetRealRowCount - AStart;
  if AMoveBy > 0 then
    Dec(LMoveCount, AMoveBy);
  Move(FRowLengths[AStart], FRowLengths[AStart + AMoveBy], LMoveCount * SizeOf(Word));
end;

function TBCEditorWordWrapHelper.DisplayToTextPosition(const ADisplayPosition: TBCEditorDisplayPosition): TBCEditorTextPosition;
var
  Line: Integer;
  Row: Integer;
  Indent: Integer;
begin
  Assert(ADisplayPosition.Column > 0);
  Assert(ADisplayPosition.Row > 0);

  if ADisplayPosition.Row > GetRealRowCount then
  begin
    Result.Char := ADisplayPosition.Column;
    Result.Line := ADisplayPosition.Row - GetRealRowCount + LineCount;
    Exit;
  end;

  Indent := 0;

  for Line := FLineCount - 2 downto 0 do
  if ADisplayPosition.Row > FLineOffsets[Line] then
  begin
    Result.Line := Line + 2;

    if ADisplayPosition.Row = FLineOffsets[Line + 1] then
      Result.Char := Min(ADisplayPosition.Column - Indent, FMaxRowLength + 1)
    else
      Result.Char := Min(ADisplayPosition.Column - Indent, FRowLengths[ADisplayPosition.Row - 1] + 1);

    for Row := FLineOffsets[Line] to ADisplayPosition.Row - 2 do
      Inc(Result.Char, FRowLengths[Row]);

    Exit;
  end;

  Result.Line := 1;

  if ADisplayPosition.Row = FLineOffsets[0] then
    Result.Char := Min(ADisplayPosition.Column - Indent, FMaxRowLength + 1)
  else
    Result.Char := Min(ADisplayPosition.Column - Indent, FRowLengths[ADisplayPosition.Row - 1] + 1);

  for Row := 0 to ADisplayPosition.Row - 2 do
    Inc(Result.Char, FRowLengths[Row]);
end;

function TBCEditorWordWrapHelper.RealRowToLine(const ARow: Integer): Integer;
var
  Line: Integer;
begin
  Assert(ARow > 0);

  if ARow > GetRowCount then
  begin
    Result := ARow - GetRowCount + LineCount;
    Exit;
  end;

  for Line := LineCount - 2 downto 0 do
  if ARow > FLineOffsets[Line] then
  begin
    Result := Line + 2;
    Exit;
  end;

  Result := 1;
end;

procedure TBCEditorWordWrapHelper.Reset;
begin
  Assert((FEditor as TBCBaseEditor).CharsInWindow >= 0);

  if ((FEditor as TBCBaseEditor).WordWrap.Style = wwsRightMargin) and ((FEditor as TBCBaseEditor).RightMargin.Position > 0)
  then
  begin
    FMaxRowLength := (FEditor as TBCBaseEditor).RightMargin.Position;
    FMinRowLength := (FEditor as TBCBaseEditor).RightMargin.Position - ((FEditor as TBCBaseEditor).RightMargin.Position div 3);
  end
  else
  begin
    FMaxRowLength := (FEditor as TBCBaseEditor).CharsInWindow;
    FMinRowLength := (FEditor as TBCBaseEditor).CharsInWindow - ((FEditor as TBCBaseEditor).CharsInWindow div 3);
  end;

  if FMinRowLength <= 0 then
    FMinRowLength := 1;

  WrapLines;
end;

function TBCEditorWordWrapHelper.RewrapLine(AIndex: TBCEditorLineIndex): Integer;
var
  MaxNewRows: Cardinal;
  LineText: string;
  LineRowCount: Integer;
  TempRowLengths: PBCEditorRowLengthArray;
  RowBegin: PChar;
  LineEnd: PChar;
  RowEnd: PChar;
  Runner: PChar;
  RowMinEnd: PChar;
  LastVisibleChar: PChar;

  StartRow: Integer;
  OldNextRow: Integer;
  Line: Integer;

  RowIndexArray: PBCEditorRowIndexArray;
begin
  LineText := (FEditor as TBCBaseEditor).Lines[AIndex];
  MaxNewRows := ((Length(LineText) - 1) div FMinRowLength) + 1;
  TempRowLengths := AllocMem(MaxNewRows * SizeOf(Word));
  try
    LineRowCount := 0;
    RowBegin := PChar(LineText);
    RowEnd := RowBegin + FMaxRowLength;
    LineEnd := RowBegin + Length(LineText);
    while RowEnd < LineEnd do
    begin
      RowMinEnd := RowBegin + FMinRowLength;
      Runner := RowEnd;
      while Runner > RowMinEnd do
      begin
        if (FEditor as TBCBaseEditor).IsWordBreakChar(Runner^) then
        begin
          RowEnd := Runner;
          break;
        end;
        Dec(Runner);
      end;

      if RowEnd > RowBegin then
      begin
        LastVisibleChar := RowEnd - 1;
        while (LastVisibleChar^ = BCEDITOR_FILLER_CHAR) and (LastVisibleChar > RowBegin) do
          Dec(LastVisibleChar);
        RowEnd := LastVisibleChar + 1;
      end;

      TempRowLengths[LineRowCount] := RowEnd - RowBegin;

      Inc(LineRowCount);
      RowBegin := RowEnd;
      Inc(RowEnd, FMaxRowLength);
    end;
    if (LineEnd > RowBegin) or (Length(LineText) = 0) then
    begin
      TempRowLengths[LineRowCount] := LineEnd - RowBegin;
      Inc(LineRowCount);
    end;

    if AIndex = 0 then
      StartRow := 0
    else
      StartRow := FLineOffsets[AIndex - 1];
    OldNextRow := FLineOffsets[AIndex];
    Result := LineRowCount - (OldNextRow - StartRow);
    if Result <> 0 then
    begin
      if Result > 0 then
      begin
        GrowRows(GetRealRowCount + Result);
        if Result = 1 then
        begin
          RowIndexArray := FLineOffsets;
          for Line := AIndex to LineCount - 1 do
            Inc(RowIndexArray[Line])
        end
        else
        begin
          RowIndexArray := FLineOffsets;
          for Line := AIndex to LineCount - 1 do
            Inc(RowIndexArray[Line], Result);
        end;
        if OldNextRow < GetRealRowCount - Result then
          MoveRows(OldNextRow, Result);
      end
      else
      begin
        if OldNextRow < GetRealRowCount then
          MoveRows(OldNextRow, Result);
        for Line := AIndex to LineCount - 1 do
          Inc(FLineOffsets[Line], Result);
      end;
    end;
    Move(TempRowLengths[0], FRowLengths[StartRow], LineRowCount * SizeOf(Word));
  finally
    FreeMem(TempRowLengths);
  end;
end;

function TBCEditorWordWrapHelper.RowToLine(const ARow: Integer): Integer;
begin
  Result := RealRowToLine(ARow);
end;

procedure TBCEditorWordWrapHelper.WrapLines;
var
  Row: Integer;
  Line: Integer;
  LineText: string;
  MaxNewRows: Integer;
  RowBegin: PChar;
  LineEnd: PChar;
  RowEnd: PChar;
  Runner: PChar;
  RowMinEnd: PChar;
  LastVisibleChar: PChar;
begin
  if ((FEditor as TBCBaseEditor).Lines.Count = 0) or (FMaxRowLength <= 0) then
  begin
    SetEmpty;
    Exit;
  end;

  GrowLines((FEditor as TBCBaseEditor).Lines.Count);
  GrowRows((FEditor as TBCBaseEditor).Lines.Count);

  Row := 0;
  for Line := 0 to (FEditor as TBCBaseEditor).Lines.Count - 1 do
  begin
    LineText := (FEditor as TBCBaseEditor).Lines[Line];

    MaxNewRows := ((Length(LineText) - 1) div FMinRowLength) + 1;
    GrowRows(Row + MaxNewRows);

    RowBegin := PChar(LineText);
    RowEnd := RowBegin + FMaxRowLength;
    LineEnd := RowBegin + Length(LineText);
    while RowEnd < LineEnd do
    begin
      RowMinEnd := RowBegin + FMinRowLength;
      Runner := RowEnd;
      while Runner > RowMinEnd do
      begin
        if (FEditor as TBCBaseEditor).IsWordBreakChar(Runner^) then
        begin
          RowEnd := Runner;
          Break;
        end;
        Dec(Runner);
      end;

      if RowEnd > RowBegin then
      begin
        LastVisibleChar := RowEnd - 1;
        while (LastVisibleChar^ = BCEDITOR_FILLER_CHAR) and (LastVisibleChar > RowBegin) do
          Dec(LastVisibleChar);
        RowEnd := LastVisibleChar + 1;
      end;

      FRowLengths[Row] := RowEnd - RowBegin;

      Inc(Row);
      RowBegin := RowEnd;
      Inc(RowEnd, FMaxRowLength);
    end;
    if (LineEnd > RowBegin) or (Length(LineText) = 0) then
    begin
      FRowLengths[Row] := LineEnd - RowBegin;
      Inc(Row);
    end;
    FLineOffsets[Line] := Row;
  end;
  FLineCount := (FEditor as TBCBaseEditor).Lines.Count;
end;

procedure TBCEditorWordWrapHelper.SetEmpty;
begin
  FLineCount := 0;
  TrimArrays;
end;

procedure TBCEditorWordWrapHelper.TrimArrays;
begin
  ReallocMem(FLineOffsets, LineCount * SizeOf(TBCEditorRowIndex));
  FLineCapacity := LineCount;
  ReallocMem(FRowLengths, GetRealRowCount * SizeOf(Word));
  FRowCapacity := GetRealRowCount;
end;

end.

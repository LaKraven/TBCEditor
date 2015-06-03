unit BCEditor.Lines;

interface

uses
  System.SysUtils, Vcl.Graphics, BCEditor.Utils, System.Classes, BCEditor.Types;

type
  TBCEditorLinesRange = Pointer;

  TBCEditorStringFlag = (sfHasTabs, sfHasNoTabs, sfExpandedLengthUnknown);
  TBCEditorStringFlags = set of TBCEditorStringFlag;

  TBCEditorLineAttributeMask = (amBackground, amForeground);
  TBCEditorLineAttributeMasks = set of TBCEditorLineAttributeMask;

  TBCEditorLineState = (lsNone, lsNormal, lsModified);
  PBCEditorLineAttribute = ^TBCEditorLineAttribute;

  TBCEditorLineAttribute = record
    Foreground: TColor;
    Background: TColor;
    Mask: TBCEditorLineAttributeMasks;
    LineState: TBCEditorLineState;
  end;

  TBCEditorStringRec = record
    FString: string;
    FRange: TBCEditorLinesRange;
    FExpandedLength: Integer;
    FFlags: TBCEditorStringFlags;
    FAttribute: TBCEditorLineAttribute;
  end;
  PEditorStringRec = ^TBCEditorStringRec;

const
  EditorStringRecSize = SizeOf(TBCEditorStringRec);
  MaxEditorStrings = MaxInt div EditorStringRecSize;

  NullRange = TBCEditorLinesRange(-1);

type
  PEditorStringRecList = ^TBCEditorStringRecList;
  TBCEditorStringRecList = array [0 .. MaxEditorStrings - 1] of TBCEditorStringRec;

  TStringListChangeEvent = procedure(Sender: TObject; Index: Integer; Count: Integer) of object;

  TBCEditorLines = class(TStrings)
  strict private
    FCapacity: Integer;
    FCount: Integer;
    FIndexOfLongestLine: Integer;
    FLengthOfLongestLine: Integer;
    FList: PEditorStringRecList;
    FLongestLineNeedsUpdate: Boolean;
    FOnBeforePutted: TStringListChangeEvent;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOnCleared: TNotifyEvent;
    FOnDeleted: TStringListChangeEvent;
    FOnInserted: TStringListChangeEvent;
    FOnPutted: TStringListChangeEvent;
    FOwner: TObject;
    FStreaming: Boolean;
    FTabConvertProc: TBCEditorTabConvertProc;
    FTabWidth: Integer;
    FUpdateCount: Integer;
    function ExpandString(Index: integer): string;
    function GetAttributes(Index: Integer): PBCEditorLineAttribute;
    function GetExpandedString(Index: Integer): string;
    function GetExpandedStringLength(Index: Integer): Integer;
    function GetRange(Index: Integer): TBCEditorLinesRange;
    procedure Grow;
    procedure PutAttributes(Index: Integer; const Value: PBCEditorLineAttribute);
    procedure PutRange(Index: Integer; ARange: TBCEditorLinesRange);
  protected
    function Get(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetTextStr: string; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetTabWidth(Value: Integer);
    procedure SetTextStr(const Value: string); override;
    procedure SetUpdateState(Updating: Boolean); override;
    procedure InsertItem(Index: Integer; const S: string);
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;

    function AccessStringLength(Index: Integer): Integer;
    function Add(const S: string): Integer; override;
    function GetLengthOfLongestLine{(AStart, AEnd: Integer)}: Integer; overload;
    function GetIsLineWhitespaceOnly(AIndex: Integer): Boolean;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure DeleteLines(Index, NumLines: Integer);
    procedure Insert(Index: Integer; const S: string); override;
    procedure InsertLines(Index, NumLines: Integer; Strings: TStrings = nil);
    procedure InsertStrings(Index: Integer; NewStrings: TStrings);
    procedure InsertText(Index: Integer; NewText: string);
    procedure LoadFromStream(Stream: TStream; Encoding: TEncoding = nil); override;
    procedure SaveToStream(Stream: TStream; Encoding: TEncoding = nil); override;
    property Attributes[index: Integer]: PBCEditorLineAttribute read GetAttributes write PutAttributes;
    property Count: Integer read FCount;
    property ExpandedStrings[Index: Integer]: string read GetExpandedString;
    property ExpandedStringLengths[index: Integer]: Integer read GetExpandedStringLength;
    property List: PEditorStringRecList read FList;
    property OnBeforePutted: TStringListChangeEvent read FOnBeforePutted write FOnBeforePutted;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnCleared: TNotifyEvent read FOnCleared write FOnCleared;
    property OnDeleted: TStringListChangeEvent read FOnDeleted write FOnDeleted;
    property OnInserted: TStringListChangeEvent read FOnInserted write FOnInserted;
    property OnPutted: TStringListChangeEvent read FOnPutted write FOnPutted;
    property Owner: TObject read FOwner write FOwner;
    property Ranges[index: Integer]: TBCEditorLinesRange read GetRange write PutRange;
    property Strings[Index: Integer]: string read Get write Put; default;
    property TabWidth: Integer read FTabWidth write SetTabWidth;
    property Text: string read GetTextStr write SetTextStr;
  end;

  EEditorStringList = class(Exception);

implementation

uses
  BCEditor.Consts, BCEditor.Language, System.Math;

{ TBCEditorLines }

procedure ListIndexOutOfBounds(Index: Integer);
begin
  raise EEditorStringList.CreateFmt(SBCEditorListIndexOutOfBounds, [index]);
end;

constructor TBCEditorLines.Create;
begin
  inherited Create;

  FCount := 0;
  FOwner := AOwner;
  FUpdateCount := 0;
  FIndexOfLongestLine := -1;
  FLengthOfLongestLine := 0;
  FLongestLineNeedsUpdate := False;
  TabWidth := 4;
  Add(EmptyStr);
end;

destructor TBCEditorLines.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  if FCount > 0 then
  begin
    //FreeCharacterLengths(0, FCount);
    Finalize(FList^[0], FCount);
  end;
  FCount := 0;
  SetCapacity(0);

  inherited;
end;

function TBCEditorLines.Add(const S: string): Integer;
begin
  Result := FCount;
  InsertItem(Result, S);
  if Assigned(OnInserted) and (FUpdateCount = 0) then
    OnInserted(Self, Result, 1);
end;

function TBCEditorLines.GetLengthOfLongestLine: Integer;
var
  i, MaxLen: Integer;
  PRec: PEditorStringRec;
begin
  if FIndexOfLongestLine < 0 then
  begin
    MaxLen := 0;
    if FCount > 0 then
    begin
      PRec := @FList^[0];
      for i := 0 to fCount - 1 do
      begin
        if sfExpandedLengthUnknown in PRec^.fFlags then
          ExpandString(i);
        if PRec^.fExpandedLength > MaxLen then
        begin
          MaxLen := PRec^.FExpandedLength;
          FIndexOfLongestLine := i;
        end;
        Inc(PRec);
      end;
    end;
  end;
  if (fIndexOfLongestLine >= 0) and (FIndexOfLongestLine < FCount) then
    Result := FList^[fIndexOfLongestLine].FExpandedLength
  else
    Result := 0;
end;

function TBCEditorLines.GetIsLineWhitespaceOnly(AIndex: Integer): Boolean;
var
  i, Len: Integer;
begin
  Result := True;
  if (AIndex < 0) or (AIndex > FCount - 1) then
    Exit;
  with FList^[AIndex] do
  begin
    Len := Length(FString);
    if Len = 0 then
      Exit;
    i := 1;
    while i <= Len do
    begin
      if FString[i] >= BCEDITOR_EXCLAMATION_MARK then
      begin
        Result := False;
        Break;
      end;
      Inc(i);
    end;
  end;
end;

function TBCEditorLines.AccessStringLength(Index: Integer): Integer;
begin
  Result := 0;
  if (index < 0) or (index > FCount - 1) then
    Exit;
  Result := Length(FList^[index].FString);
end;

procedure TBCEditorLines.Clear;
begin
  if FCount <> 0 then
  begin
    BeginUpdate;
    try
      Finalize(FList^[0], FCount);
      FCount := 0;
      SetCapacity(0);
      if Assigned(FOnCleared) then
        FOnCleared(Self);
    finally
      EndUpdate;
    end;
  end;
  { Clear information about longest line }
  FIndexOfLongestLine := -1;
  FLengthOfLongestLine := 0;
end;

procedure TBCEditorLines.Delete(Index: Integer);
begin
  if (Index < 0) or (Index > FCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  try
    Finalize(FList^[Index]);
    Dec(FCount);
    if index < FCount then
      System.Move(FList[Index + 1], FList[Index], (FCount - Index) * EditorStringRecSize);
  finally
    EndUpdate;
  end;
  FIndexOfLongestLine := -1;
  if Assigned(FOnDeleted) then
    FOnDeleted(Self, Index, 1);
end;

procedure TBCEditorLines.DeleteLines(Index, NumLines: Integer);
var
  LinesAfter: Integer;
begin
  if NumLines > 0 then
  begin
    if (index < 0) or (Index > FCount) then
      ListIndexOutOfBounds(index);
    LinesAfter := FCount - (Index + NumLines);
    if LinesAfter < 0 then
      NumLines := FCount - Index - 1;
    Finalize(FList^[Index], NumLines);
    if LinesAfter > 0 then
    begin
      BeginUpdate;
      try
        System.Move(FList[Index + NumLines], FList[Index], LinesAfter * EditorStringRecSize);
      finally
        EndUpdate;
      end;
    end;
    Dec(FCount, NumLines);

    FIndexOfLongestLine := -1;
    if Assigned(FOnDeleted) then
      FOnDeleted(Self, index, NumLines);
  end;
end;

function TBCEditorLines.GetAttributes(Index: Integer): PBCEditorLineAttribute;
begin
  if (index >= 0) and (index < FCount) then
    Result := @(FList^[index].FAttribute)
  else
    Result := nil;
end;

procedure TBCEditorLines.PutAttributes(Index: Integer; const Value: PBCEditorLineAttribute);
begin
  if (index < 0) or (index >= FCount) then
    ListIndexOutOfBounds(index);
  BeginUpdate;
  FList^[index].FAttribute.Background := Value.Background;
  FList^[index].FAttribute.Foreground := Value.Foreground;
  FList^[index].FAttribute.Mask := Value.Mask;
  FList^[index].FAttribute.LineState := Value.LineState;
  EndUpdate;
end;

function TBCEditorLines.ExpandString(Index: integer): string;
var
  HasTabs: boolean;
begin
  with FList^[Index] do
  begin
    if FString = '' then
    begin
      Result := '';
      Exclude(FFlags, sfExpandedLengthUnknown);
      Exclude(FFlags, sfHasTabs);
      Include(FFlags, sfHasNoTabs);
      FExpandedLength := 0;
    end
    else
    begin
      Result := FTabConvertProc(FString, FTabWidth, HasTabs);
      FExpandedLength := Length(Result);
      Exclude(FFlags, sfExpandedLengthUnknown);
      Exclude(FFlags, sfHasTabs);
      Exclude(FFlags, sfHasNoTabs);
      if HasTabs then
        Include(FFlags, sfHasTabs)
      else
        Include(FFlags, sfHasNoTabs);
    end;
  end;
end;

function TBCEditorLines.Get(Index: Integer): string;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FList^[index].FString
  else
    Result := '';
end;

function TBCEditorLines.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TBCEditorLines.GetCount: Integer;
begin
  Result := FCount;
end;

function TBCEditorLines.GetExpandedString(Index: integer): string;
begin
  Result := '';
  if (Index >= 0) and (Index < fCount) then
  begin
    if sfHasNoTabs in FList^[Index].FFlags then
      Result := Get(Index)
    else
      Result := ExpandString(Index);
  end
end;

function TBCEditorLines.GetExpandedStringLength(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < FCount) then
  begin
    if sfExpandedLengthUnknown in FList^[index].FFlags then
      Result := Length( ExpandedStrings[index] ) //AnalyzeString(Index);
    else
      Result := FList^[Index].FExpandedLength;
  end
  else
    Result := 0;
end;

function TBCEditorLines.GetRange(Index: Integer): TBCEditorLinesRange;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FList^[Index].FRange
  else
    Result := nil;
end;

function TBCEditorLines.GetTextStr: string;
var
  i, L, Size, LCount: Integer;
  P: PChar;
  S, LLineBreak: string;
begin
  LCount := GetCount;
  Size := 0;
  LLineBreak := SLineBreak;
  for i := 0 to LCount - 1 do
    Inc(Size, Length(Get(i)) + Length(LLineBreak));
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for i := 0 to LCount - 1 do
  begin
    S := Get(i);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L * SizeOf(Char));
      Inc(P, L);
    end;
    L := Length(LLineBreak);
    if L <> 0 then
    begin
      System.Move(Pointer(LLineBreak)^, P^, L * SizeOf(Char));
      Inc(P, L);
    end;
  end;
end;

procedure TBCEditorLines.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    Delta := 16;
  SetCapacity(FCapacity + Delta);
end;

procedure TBCEditorLines.Insert(Index: Integer; const S: string);
begin
  if (index < 0) or (index > FCount) then
    ListIndexOutOfBounds(index);
  BeginUpdate;
  InsertItem(index, S);
  if Assigned(FOnInserted) then
    FOnInserted(Self, index, 1);
  EndUpdate;
end;

procedure TBCEditorLines.InsertItem(Index: Integer; const S: string);
begin
  if FCount = FCapacity then
    Grow;

  if Index < FCount then
    System.Move(FList^[index], FList^[index + 1], (FCount - Index) * EditorStringRecSize);
  FIndexOfLongestLine := -1;
  with FList^[index] do
  begin
    Pointer(FString) := nil;
    FString := S;
    FRange := NullRange;
    FExpandedLength := -1;
    FFlags := [sfExpandedLengthUnknown];
    FAttribute.Foreground := clNone;
    FAttribute.Background := clNone;
    FAttribute.Mask := [];
    FAttribute.LineState := lsNone;
  end;
  Inc(FCount);
end;

procedure TBCEditorLines.InsertLines(Index, NumLines: Integer; Strings: TStrings = nil);
var
  i: Integer;
  LLine: Integer;
begin
  if (index < 0) or (index > FCount) then
    ListIndexOutOfBounds(index);
  if NumLines > 0 then
  begin
    BeginUpdate;
    try
      SetCapacity(FCount + NumLines);
      if index < FCount then
        System.Move(FList^[index], FList^[index + NumLines], (FCount - index) * EditorStringRecSize);
      i := 0;
      for LLine := Index to Index + NumLines - 1 do
        with FList^[LLine] do
        begin
          Pointer(FString) := nil;
          if Assigned(Strings) then
            FString := Strings[i];
          Inc(i);
          FRange := NullRange;
          FExpandedLength := -1;
          FFlags := [sfExpandedLengthUnknown];
          FAttribute.Mask := [];
          FAttribute.Foreground := clNone;
          FAttribute.Background := clNone;
          FAttribute.LineState := lsModified;
        end;
      Inc(FCount, NumLines);
    finally
      EndUpdate;
    end;

    if Assigned(OnInserted) then
      OnInserted(Self, Index, NumLines);
  end;
end;

procedure TBCEditorLines.InsertStrings(Index: Integer; NewStrings: TStrings);
var
  Cnt: Integer;
begin
  Cnt := NewStrings.Count;
  if Cnt = 0 then
    Exit;

  BeginUpdate;
  try
    InsertLines(index, Cnt, NewStrings);
  finally
    EndUpdate;
  end;
end;

procedure TBCEditorLines.InsertText(Index: Integer; NewText: string);
var
  TmpStringList: TStringList;
begin
  if NewText = '' then
    Exit;

  TmpStringList := TStringList.Create;
  try
    TmpStringList.Text := NewText;
    InsertStrings(index, TmpStringList);
  finally
    TmpStringList.Free;
  end;
end;

procedure TBCEditorLines.LoadFromStream(Stream: TStream; Encoding: TEncoding = nil);
var
  Size: Integer;
  Buffer: TBytes;
  StrBuffer: string;
begin
  FStreaming := True;

  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    if Assigned(Encoding) then
    begin
      SetLength(Buffer, Size);
      Stream.Read(Buffer[0], Size);
      Size := TEncoding.GetBufferEncoding(Buffer, Encoding);
      SetTextStr(Encoding.GetString(Buffer, Size, Length(Buffer) - Size));
    end
    else
    begin
      SetLength(StrBuffer, Size shr 1);
      Stream.ReadBuffer(StrBuffer[1], Size);
      SetTextStr(StrBuffer);
    end;
  finally
    EndUpdate;
  end;

  if Assigned(OnInserted) then
    OnInserted(Self, 0, FCount);

  FStreaming := False;
end;

procedure TBCEditorLines.SaveToStream(Stream: TStream; Encoding: TEncoding);
var
  Buffer, Preamble: TBytes;
begin
  FStreaming := True;

  if Encoding = nil then
    Encoding := TEncoding.Default;
  Buffer := Encoding.GetBytes(GetTextStr);
  Preamble := Encoding.GetPreamble;
  if Length(Preamble) > 0 then
    Stream.WriteBuffer(Preamble[0], Length(Preamble));
  Stream.WriteBuffer(Buffer[0], Length(Buffer));

  FStreaming := False;
end;

procedure TBCEditorLines.Put(Index: Integer; const S: string);
begin
  if ((index = 0) and (FCount = 0)) or (FCount = index) then
    Add(S)
  else
  begin
    if (Index < 0) or (Index >= FCount) then
      ListIndexOutOfBounds(index);
    if Assigned(OnBeforePutted) then
      OnBeforePutted(Self, index, 1);
    with FList^[index] do
    begin
      Include(FFlags, sfExpandedLengthUnknown);
      Exclude(FFlags, sfHasTabs);
      Exclude(FFlags, sfHasNoTabs);
      FString := S;
      FAttribute.LineState := lsModified;
    end;

    if Assigned(FOnPutted) then
      FOnPutted(Self, Index, 1);
  end;
end;

procedure TBCEditorLines.PutObject(Index: Integer; AObject: TObject);
begin
  { Do nothing }
end;

procedure TBCEditorLines.PutRange(Index: Integer; ARange: TBCEditorLinesRange);
begin
  if (Index < 0) or (Index >= FCount) then
    ListIndexOutOfBounds(index);
  BeginUpdate;
  FList^[index].FRange := ARange;
  EndUpdate;
end;

procedure TBCEditorLines.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < Count then
    EListError.Create(SBCEditorInvalidCapacity);
  ReallocMem(FList, NewCapacity * EditorStringRecSize);
  FCapacity := NewCapacity;
end;

procedure TBCEditorLines.SetTabWidth(Value: Integer);
var
  i: Integer;
begin
  if Value <> FTabWidth then
  begin
    FTabWidth := Value;
    FTabConvertProc := GetTabConvertProc(FTabWidth);
    FIndexOfLongestLine := -1;
    for i := 0 to FCount - 1 do
      with FList^[i] do
      begin
        FExpandedLength := -1;
        Exclude(FFlags, sfHasNoTabs);
        Include(FFlags, sfExpandedLengthUnknown);
      end;
  end;
end;

procedure TBCEditorLines.SetTextStr(const Value: string);
var
  S: string;
  Size: Integer;
  P, Start, Pmax: PWideChar;
begin
  Clear;
  P := Pointer(Value);
  if Assigned(P) then
  begin
    Size := Length(Value);
    Pmax := @Value[Size];
    while P <= Pmax do
    begin
      Start := P;
      while (P^ <> #13) and (P^ <> #10) and (P^ <> WideChar($2028)) and (P <= Pmax) do
        Inc(P);
      if P <> Start then
      begin
        SetString(S, Start, P - Start);
        InsertItem(fCount, S);
      end
      else
        InsertItem(fCount, '');
      if P^ = #13 then
        Inc(P);
      if P^ = #10 then
        Inc(P);
      if P^ = WideChar($2028) then
        Inc(P);
    end;
  end;

  if (FUpdateCount = 0) and Assigned(FOnInserted) then
    FOnInserted(Self, 0, FCount);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLines.SetUpdateState(Updating: Boolean);
begin
  if Updating then
  begin
    if Assigned(FOnChanging) then
      FOnChanging(Self);
  end
  else
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.

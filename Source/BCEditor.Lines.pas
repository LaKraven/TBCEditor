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

  TBCEditorStringRecord = record
    FString: string;
    FRange: TBCEditorLinesRange;
    FExpandedLength: Integer;
    FFlags: TBCEditorStringFlags;
    FAttribute: TBCEditorLineAttribute;
  end;
  PBCEditorStringRecord = ^TBCEditorStringRecord;

const
  CSTRINGRECORDSIZE = SizeOf(TBCEditorStringRecord);
  CMAXSTRINGS = MaxInt div CSTRINGRECORDSIZE;
  CNULLRANGE = TBCEditorLinesRange(-1);

type
  PEditorStringRecordList = ^TBCEditorStringRecordList;
  TBCEditorStringRecordList = array [0 .. CMAXSTRINGS - 1] of TBCEditorStringRecord;

  TStringListChangeEvent = procedure(Sender: TObject; Index: Integer; Count: Integer) of object;

  TBCEditorLines = class(TStrings)
  strict private
    FCapacity: Integer;
    FCount: Integer;
    FIndexOfLongestLine: Integer;
    FLengthOfLongestLine: Integer;
    FList: PEditorStringRecordList;
    FLongestLineNeedsUpdate: Boolean;
    FOnAfterSetText: TNotifyEvent;
    FOnBeforePutted: TStringListChangeEvent;
    FOnBeforeSetText: TNotifyEvent;
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
    function GetLengthOfLongestLine: Integer; overload;
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
    property Attributes[Index: Integer]: PBCEditorLineAttribute read GetAttributes write PutAttributes;
    property Count: Integer read FCount;
    property ExpandedStrings[Index: Integer]: string read GetExpandedString;
    property ExpandedStringLengths[Index: Integer]: Integer read GetExpandedStringLength;
    property List: PEditorStringRecordList read FList;
    property OnAfterSetText: TNotifyEvent read FOnAfterSetText write FOnAfterSetText;
    property OnBeforePutted: TStringListChangeEvent read FOnBeforePutted write FOnBeforePutted;
    property OnBeforeSetText: TNotifyEvent read FOnBeforeSetText write FOnBeforeSetText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnCleared: TNotifyEvent read FOnCleared write FOnCleared;
    property OnDeleted: TStringListChangeEvent read FOnDeleted write FOnDeleted;
    property OnInserted: TStringListChangeEvent read FOnInserted write FOnInserted;
    property OnPutted: TStringListChangeEvent read FOnPutted write FOnPutted;
    property Owner: TObject read FOwner write FOwner;
    property Ranges[Index: Integer]: TBCEditorLinesRange read GetRange write PutRange;
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
  raise EEditorStringList.CreateFmt(SBCEditorListIndexOutOfBounds, [Index]);
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
    Finalize(FList^[0], FCount);
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
  i, LMaxLength: Integer;
  PStringRecord: PBCEditorStringRecord;
begin
  if FIndexOfLongestLine < 0 then
  begin
    LMaxLength := 0;
    if FCount > 0 then
    begin
      PStringRecord := @FList^[0];
      for i := 0 to FCount - 1 do
      begin
        if sfExpandedLengthUnknown in PStringRecord^.fFlags then
          ExpandString(i);
        if PStringRecord^.fExpandedLength > LMaxLength then
        begin
          LMaxLength := PStringRecord^.FExpandedLength;
          FIndexOfLongestLine := i;
        end;
        Inc(PStringRecord);
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
  i, LLength: Integer;
begin
  Result := True;
  if (AIndex < 0) or (AIndex > FCount - 1) then
    Exit;
  with FList^[AIndex] do
  begin
    LLength := Length(FString);
    if LLength = 0 then
      Exit;
    i := 1;
    while i <= LLength do
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
  if (Index < 0) or (Index > FCount - 1) then
    Exit;
  Result := Length(FList^[Index].FString);
end;

procedure TBCEditorLines.Clear;
begin
  if FCount <> 0 then
  begin
    //BeginUpdate;
    try
      Finalize(FList^[0], FCount);
      FCount := 0;
      SetCapacity(0);
      if Assigned(FOnCleared) then
        FOnCleared(Self);
    finally
      //EndUpdate;
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
    if Index < FCount then
      System.Move(FList[Index + 1], FList[Index], (FCount - Index) * CSTRINGRECORDSIZE);
  finally
    EndUpdate;
  end;
  FIndexOfLongestLine := -1;
  if Assigned(FOnDeleted) then
    FOnDeleted(Self, Index, 1);
end;

procedure TBCEditorLines.DeleteLines(Index, NumLines: Integer);
var
  LLinesAfter: Integer;
begin
  if NumLines > 0 then
  begin
    if (Index < 0) or (Index > FCount) then
      ListIndexOutOfBounds(Index);
    LLinesAfter := FCount - (Index + NumLines);
    if LLinesAfter < 0 then
      NumLines := FCount - Index - 1;
    Finalize(FList^[Index], NumLines);
    if LLinesAfter > 0 then
    begin
      BeginUpdate;
      try
        System.Move(FList[Index + NumLines], FList[Index], LLinesAfter * CSTRINGRECORDSIZE);
      finally
        EndUpdate;
      end;
    end;
    Dec(FCount, NumLines);

    FIndexOfLongestLine := -1;
    if Assigned(FOnDeleted) then
      FOnDeleted(Self, Index, NumLines);
  end;
end;

function TBCEditorLines.GetAttributes(Index: Integer): PBCEditorLineAttribute;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := @(FList^[Index].FAttribute)
  else
    Result := nil;
end;

procedure TBCEditorLines.PutAttributes(Index: Integer; const Value: PBCEditorLineAttribute);
begin
  if (Index < 0) or (Index >= FCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  FList^[Index].FAttribute.Background := Value.Background;
  FList^[Index].FAttribute.Foreground := Value.Foreground;
  FList^[Index].FAttribute.Mask := Value.Mask;
  FList^[Index].FAttribute.LineState := Value.LineState;
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
    Result := FList^[Index].FString
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
  if (Index >= 0) and (Index < FCount) then
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
    if sfExpandedLengthUnknown in FList^[Index].FFlags then
      Result := Length(ExpandedStrings[Index])
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
  LDelta: Integer;
begin
  if FCapacity > 64 then
    LDelta := FCapacity div 4
  else
    LDelta := 16;
  SetCapacity(FCapacity + LDelta);
end;

procedure TBCEditorLines.Insert(Index: Integer; const S: string);
begin
  if (Index < 0) or (Index > FCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  InsertItem(Index, S);
  if Assigned(FOnInserted) then
    FOnInserted(Self, Index, 1);
  EndUpdate;
end;

procedure TBCEditorLines.InsertItem(Index: Integer; const S: string);
begin
  if FCount = FCapacity then
    Grow;

  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1], (FCount - Index) * CSTRINGRECORDSIZE);
  FIndexOfLongestLine := -1;
  with FList^[Index] do
  begin
    Pointer(FString) := nil;
    FString := S;
    FRange := CNULLRANGE;
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
  if (Index < 0) or (Index > FCount) then
    ListIndexOutOfBounds(Index);
  if NumLines > 0 then
  begin
    BeginUpdate;
    try
      SetCapacity(FCount + NumLines);
      if Index < FCount then
        System.Move(FList^[Index], FList^[Index + NumLines], (FCount - Index) * CSTRINGRECORDSIZE);
      i := 0;
      for LLine := Index to Index + NumLines - 1 do
      with FList^[LLine] do
      begin
        Pointer(FString) := nil;
        if Assigned(Strings) then
          FString := Strings[i];
        Inc(i);
        FRange := CNULLRANGE;
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
  LCount: Integer;
begin
  LCount := NewStrings.Count;
  if LCount = 0 then
    Exit;

  BeginUpdate;
  try
    InsertLines(Index, LCount, NewStrings);
  finally
    EndUpdate;
  end;
end;

procedure TBCEditorLines.InsertText(Index: Integer; NewText: string);
var
  LStringList: TStringList;
begin
  if NewText = '' then
    Exit;

  LStringList := TStringList.Create;
  try
    LStringList.Text := NewText;
    InsertStrings(Index, LStringList);
  finally
    LStringList.Free;
  end;
end;

procedure TBCEditorLines.LoadFromStream(Stream: TStream; Encoding: TEncoding = nil);
var
  LSize: Integer;
  LBuffer: TBytes;
  LStrBuffer: string;
begin
  FStreaming := True;

  BeginUpdate;
  try
    LSize := Stream.Size - Stream.Position;
    if Assigned(Encoding) then
    begin
      SetLength(LBuffer, LSize);
      Stream.Read(LBuffer[0], LSize);
      LSize := TEncoding.GetBufferEncoding(LBuffer, Encoding);
      LStrBuffer := Encoding.GetString(LBuffer, LSize, Length(LBuffer) - LSize);
    end
    else
    begin
      SetLength(LStrBuffer, LSize shr 1);
      Stream.ReadBuffer(LStrBuffer[1], LSize);
    end;
    SetTextStr(LStrBuffer);
  finally
    EndUpdate;
  end;

  if Assigned(OnInserted) then
    OnInserted(Self, 0, FCount);

  FStreaming := False;
end;

procedure TBCEditorLines.SaveToStream(Stream: TStream; Encoding: TEncoding);
var
  LBuffer, LPreamble: TBytes;
begin
  FStreaming := True;

  if Encoding = nil then
    Encoding := TEncoding.Default;
  LBuffer := Encoding.GetBytes(GetTextStr);
  LPreamble := Encoding.GetPreamble;
  if Length(LPreamble) > 0 then
    Stream.WriteBuffer(LPreamble[0], Length(LPreamble));
  Stream.WriteBuffer(LBuffer[0], Length(LBuffer));

  FStreaming := False;
end;

procedure TBCEditorLines.Put(Index: Integer; const S: string);
begin
  if ((Index = 0) and (FCount = 0)) or (FCount = Index) then
    Add(S)
  else
  begin
    if (Index < 0) or (Index >= FCount) then
      ListIndexOutOfBounds(Index);
    if Assigned(OnBeforePutted) then
      OnBeforePutted(Self, Index, 1);
    with FList^[Index] do
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
    ListIndexOutOfBounds(Index);
  FList^[Index].FRange := ARange;
end;

procedure TBCEditorLines.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < Count then
    EListError.Create(SBCEditorInvalidCapacity);
  ReallocMem(FList, NewCapacity * CSTRINGRECORDSIZE);
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
  LLength: Integer;
  PValue, PStartValue, PLastChar: PChar;
begin
  if Assigned(FOnBeforeSetText) then
    FOnBeforeSetText(Self);
  Clear;
  PValue := Pointer(Value);
  if Assigned(PValue) then
  begin
    LLength := Length(Value);
    PLastChar := @Value[LLength];
    while PValue <= PLastChar do
    begin
      PStartValue := PValue;
      while (PValue^ <> #13) and (PValue^ <> #10) and (PValue^ <> WideChar($2028)) and (PValue <= PLastChar) do
        Inc(PValue);
      if PValue <> PStartValue then
      begin
        SetString(S, PStartValue, PValue - PStartValue);
        InsertItem(FCount, S);
      end
      else
        InsertItem(FCount, '');
      if PValue^ = #13 then
        Inc(PValue);
      if PValue^ = #10 then
        Inc(PValue);
      if PValue^ = WideChar($2028) then
        Inc(PValue);
    end;
  end;

  if (FUpdateCount = 0) and Assigned(FOnInserted) then
    FOnInserted(Self, 0, FCount);
  if Assigned(FOnChange) then
    FOnChange(Self);
  if Assigned(FOnAfterSetText) then
    FOnAfterSetText(Self);
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

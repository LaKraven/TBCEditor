unit BCEditor.Editor.CodeFolding.Ranges;

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils, BCEditor.Editor.CodeFolding.FoldRegions;

type
  TBCEditorCodeFoldingRange = class;
  TBCEditorAllCodeFoldingRanges = class;

  TBCEditorCodeFoldingRanges = class(TPersistent)
  strict private
    FList: TList;
    function Get(Index: Integer): TBCEditorCodeFoldingRange;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(AAllCodeFoldingRanges: TBCEditorAllCodeFoldingRanges; AFromLine, AIndentLevel, AFoldRangeLevel: Integer;
      AFoldRegion: TBCEditorFoldRegionItem; AToLine: Integer = 0): TBCEditorCodeFoldingRange;
    procedure Clear;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TBCEditorCodeFoldingRange read Get; default;
    //property List: TList read FList;
  end;

  TBCEditorAllCodeFoldingRanges = class(TBCEditorCodeFoldingRanges)
  strict private
    FAllRanges: TList;
    function GetAllCount: Integer;
    function GetAllFoldRange(Index: Integer): TBCEditorCodeFoldingRange;
    procedure SetAllFoldRange(Index: Integer; Value: TBCEditorCodeFoldingRange);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ClearAll;
    procedure Delete(FoldRange: TBCEditorCodeFoldingRange); overload;
    procedure Delete(AIndex: Integer); overload;
    procedure Assign(Source: TPersistent); override;
    procedure UpdateFoldRanges;
    procedure SetParentCollapsedOfSubCodeFoldingRanges(AFoldRange: TBCEditorCodeFoldingRange);

    property AllCount: Integer read GetAllCount;
    property AllCodeFoldingRanges[Index: Integer]: TBCEditorCodeFoldingRange read GetAllFoldRange write SetAllFoldRange; default;
    property AllRanges: TList read FAllRanges;
  end;

  TBCEditorCodeFoldingRange = class
  strict private
    FAllCodeFoldingRanges: TBCEditorAllCodeFoldingRanges;
    FCollapsed: Boolean;
    FCollapsedBy: Integer;
    FCollapsedLines: TStringList;
    FCollapseMarkRect: TRect;
    FFoldRegion: TBCEditorFoldRegionItem;
    FFromLine: Integer;
    FIndentLevel: Integer;
    FParentCollapsed: Boolean;
    FUndoListed: Boolean;
    FFoldRangeLevel: Integer;
    FSubCodeFoldingRanges: TBCEditorCodeFoldingRanges;
    FToLine: Integer;
    FIsExtraTokenFound: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Collapsable: Boolean;
    procedure MoveBy(LineCount: Integer);
    procedure MoveChildren(By: Integer);
    procedure SetParentCollapsedOfSubCodeFoldingRanges(AParentCollapsed: Boolean; ACollapsedBy: Integer);
    procedure Widen(LineCount: Integer);
    property AllCodeFoldingRanges: TBCEditorAllCodeFoldingRanges read FAllCodeFoldingRanges write FAllCodeFoldingRanges;
    property Collapsed: Boolean read FCollapsed write FCollapsed default False;
    property CollapsedBy: Integer read FCollapsedBy write FCollapsedBy;
    property CollapsedLines: TStringList read FCollapsedLines;
    property CollapseMarkRect: TRect read FCollapseMarkRect write FCollapseMarkRect;
    property FoldRegion: TBCEditorFoldRegionItem read FFoldRegion write FFoldRegion;
    property FromLine: Integer read FFromLine write FFromLine;
    property IsExtraTokenFound: Boolean read FIsExtraTokenFound write FIsExtraTokenFound default False;
    property IndentLevel: Integer read FIndentLevel write FIndentLevel;
    property ParentCollapsed: Boolean read FParentCollapsed write FParentCollapsed;
    property UndoListed: Boolean read FUndoListed write FUndoListed default False;
    property FoldRangeLevel: Integer read FFoldRangeLevel write FFoldRangeLevel;
    property SubCodeFoldingRanges: TBCEditorCodeFoldingRanges read FSubCodeFoldingRanges;
    property ToLine: Integer read FToLine write FToLine;
  end;

implementation

uses
  BCEditor.Utils;

{ TBCEditorAllCodeFoldingRanges }

constructor TBCEditorAllCodeFoldingRanges.Create;
begin
  inherited;

  FAllRanges := TList.Create;
end;

destructor TBCEditorAllCodeFoldingRanges.Destroy;
begin
  FreeList(FAllRanges);

  inherited;
end;

procedure TBCEditorAllCodeFoldingRanges.Assign(Source: TPersistent);

  procedure RecursiveAssign(ADestinationCodeFoldingRanges: TBCEditorCodeFoldingRanges; ASourceCodeFoldingRanges: TBCEditorCodeFoldingRanges);
  var
    i: Integer;
    LDestinationCodeFoldingRange: TBCEditorCodeFoldingRange;
    LSourceCodeFoldingRange: TBCEditorCodeFoldingRange;
  begin
    if Assigned(ASourceCodeFoldingRanges) then
    for i := 0 to ASourceCodeFoldingRanges.Count - 1 do
    begin
      LSourceCodeFoldingRange := ASourceCodeFoldingRanges[i];

      LDestinationCodeFoldingRange := ADestinationCodeFoldingRanges.Add(Self, LSourceCodeFoldingRange.FromLine, LSourceCodeFoldingRange.IndentLevel,
        LSourceCodeFoldingRange.FoldRangeLevel, LSourceCodeFoldingRange.FoldRegion, LSourceCodeFoldingRange.ToLine);
      LDestinationCodeFoldingRange.CollapsedBy := LSourceCodeFoldingRange.CollapsedBy;
      LDestinationCodeFoldingRange.Collapsed := LSourceCodeFoldingRange.Collapsed;
      LDestinationCodeFoldingRange.ParentCollapsed := LSourceCodeFoldingRange.ParentCollapsed;
      if Assigned(LSourceCodeFoldingRange.CollapsedLines) then
        LDestinationCodeFoldingRange.CollapsedLines.Assign(LSourceCodeFoldingRange.CollapsedLines);
      LDestinationCodeFoldingRange.FoldRegion := LSourceCodeFoldingRange.FoldRegion;

      RecursiveAssign(LDestinationCodeFoldingRange.SubCodeFoldingRanges, LSourceCodeFoldingRange.SubCodeFoldingRanges);
    end;
  end;

begin
  if Assigned(Source) and (Source is TBCEditorAllCodeFoldingRanges) then
    RecursiveAssign(Self, Source as TBCEditorAllCodeFoldingRanges);
end;

procedure TBCEditorAllCodeFoldingRanges.ClearAll;
begin
  ClearList(FAllRanges);
end;

procedure TBCEditorAllCodeFoldingRanges.Delete(FoldRange: TBCEditorCodeFoldingRange);
var
  i: Integer;
begin
  for i := 0 to FAllRanges.Count - 1 do
  if FAllRanges[i] = FoldRange then
  begin
    TBCEditorCodeFoldingRange(FAllRanges[i]).Free;
    FAllRanges[i] := nil;
    FAllRanges.Delete(i);
    Break;
  end;
end;

procedure TBCEditorAllCodeFoldingRanges.Delete(AIndex: Integer);
begin
  FAllRanges.Delete(AIndex);
end;

function TBCEditorAllCodeFoldingRanges.GetAllCount: Integer;
begin
  Result := FAllRanges.Count;
end;

function TBCEditorAllCodeFoldingRanges.GetAllFoldRange(Index: Integer): TBCEditorCodeFoldingRange;
begin
  if Cardinal(Index) < Cardinal(FAllRanges.Count) then
    Result := FAllRanges.List[index]
  else
    Result := nil;
end;

procedure TBCEditorAllCodeFoldingRanges.SetAllFoldRange(Index: Integer; Value: TBCEditorCodeFoldingRange);
begin
  FAllRanges[index] := Value;
end;

procedure TBCEditorAllCodeFoldingRanges.SetParentCollapsedOfSubCodeFoldingRanges(AFoldRange: TBCEditorCodeFoldingRange);
var
	I: Integer;
  FoldRange: TBCEditorCodeFoldingRange;
begin
  for I := 0 to AllCount - 1 do
  begin
    FoldRange := GetAllFoldRange(I);
    if FoldRange = AFoldRange then
      Continue;
    if FoldRange.FromLine > AFoldRange.ToLine then
      Break;
    if (FoldRange.FromLine > AFoldRange.FromLine) and (FoldRange.FromLine <> AFoldRange.ToLine) then
      FoldRange.ParentCollapsed := True;
  end;
end;

procedure TBCEditorAllCodeFoldingRanges.UpdateFoldRanges;
var
  I: Integer;
  FoldRange: TBCEditorCodeFoldingRange;
begin
  for I := 0 to AllCount - 1 do
  begin
    FoldRange := GetAllFoldRange(I);
    FoldRange.ParentCollapsed := False;
  end;
  for I := 0 to AllCount - 1 do
  begin
    FoldRange := GetAllFoldRange(I);
    if not FoldRange.ParentCollapsed then
      SetParentCollapsedOfSubCodeFoldingRanges(FoldRange);
  end;
end;

{ TBCEditorCodeFoldingRanges }

constructor TBCEditorCodeFoldingRanges.Create;
begin
  inherited;

  FList := TList.Create;
end;

destructor TBCEditorCodeFoldingRanges.Destroy;
begin
  FList.Clear;
  FList.Free;
  FList := nil;

  inherited;
end;

function TBCEditorCodeFoldingRanges.Add(AAllCodeFoldingRanges: TBCEditorAllCodeFoldingRanges; AFromLine, AIndentLevel, AFoldRangeLevel: Integer;
  AFoldRegion: TBCEditorFoldRegionItem; AToLine: Integer): TBCEditorCodeFoldingRange;
begin
  Result := TBCEditorCodeFoldingRange.Create;
  with Result do
  begin
    FromLine := AFromLine;
    ToLine := AToLine;
    IndentLevel := AIndentLevel;
    FoldRangeLevel := AFoldRangeLevel;
    AllCodeFoldingRanges := AAllCodeFoldingRanges;
    FoldRegion := AFoldRegion;
  end;
  FList.Add(Result);
  AAllCodeFoldingRanges.AllRanges.Add(Result);
end;

procedure TBCEditorCodeFoldingRanges.Clear;
begin
  FList.Clear;
end;

function TBCEditorCodeFoldingRanges.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TBCEditorCodeFoldingRanges.Get(Index: Integer): TBCEditorCodeFoldingRange;
begin
  Result := FList[index];
end;

{ TBCEditorCodeFoldingRange }

function TBCEditorCodeFoldingRange.Collapsable: Boolean;
begin
  Result := FFromLine <> FToLine;
end;

constructor TBCEditorCodeFoldingRange.Create;
begin
  inherited;

  FSubCodeFoldingRanges := TBCEditorCodeFoldingRanges.Create;
  FCollapsedLines := TStringList.Create;
  FCollapsed := False;
  FCollapsedBy := -1;
  FIsExtraTokenFound := False;
  FUndoListed := False;
end;

destructor TBCEditorCodeFoldingRange.Destroy;
begin;
  FSubCodeFoldingRanges.Clear;
  FSubCodeFoldingRanges.Free;
  FSubCodeFoldingRanges := nil;
  FCollapsedLines.Free;
  FCollapsedLines := nil;

  inherited;
end;

procedure TBCEditorCodeFoldingRange.MoveBy(LineCount: Integer);
begin
  Inc(FFromLine, LineCount);
  Inc(FToLine, LineCount);
end;

procedure TBCEditorCodeFoldingRange.MoveChildren(By: Integer);
var
  i: Integer;
  LCodeFoldingRange: TBCEditorCodeFoldingRange;
begin
  for i := 0 to FSubCodeFoldingRanges.Count - 1 do
  begin
    LCodeFoldingRange := FSubCodeFoldingRanges[i];
    if Assigned(LCodeFoldingRange) then
    begin
      LCodeFoldingRange.MoveChildren(By);

      with FAllCodeFoldingRanges.AllRanges do
      if LCodeFoldingRange.FParentCollapsed then
        Move(IndexOf(LCodeFoldingRange), IndexOf(LCodeFoldingRange) + By);
    end;
  end;
end;

procedure TBCEditorCodeFoldingRange.SetParentCollapsedOfSubCodeFoldingRanges(AParentCollapsed: Boolean; ACollapsedBy: Integer);
var
  i: Integer;
  LCodeFoldingRange: TBCEditorCodeFoldingRange;
begin
  for i := 0 to FSubCodeFoldingRanges.Count - 1 do
  begin
    LCodeFoldingRange := FSubCodeFoldingRanges[i];
    LCodeFoldingRange.SetParentCollapsedOfSubCodeFoldingRanges(AParentCollapsed, ACollapsedBy);

    if (LCodeFoldingRange.FCollapsedBy = -1) or (LCodeFoldingRange.FCollapsedBy = ACollapsedBy) then
    begin
      LCodeFoldingRange.FParentCollapsed := AParentCollapsed;

      if not AParentCollapsed then
        LCodeFoldingRange.FCollapsedBy := -1
      else
        LCodeFoldingRange.FCollapsedBy := ACollapsedBy;
    end;
  end;
end;

procedure TBCEditorCodeFoldingRange.Widen(LineCount: Integer);
begin
  Inc(FToLine, LineCount);
end;

end.

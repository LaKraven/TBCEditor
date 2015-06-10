unit BCEditor.Editor.CodeFolding.Ranges;

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils, BCEditor.Editor.CodeFolding.FoldRegions;

type
  TBCEditorCodeFoldingRange = class;
  TBCEditorAllCodeFoldingRanges = class;

  TBCEditorCodeFoldingRanges = class(TPersistent)
  strict private
    FRanges: TList;
    function GetRange(Index: Integer): TBCEditorCodeFoldingRange;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(AAllFold: TBCEditorAllCodeFoldingRanges; AFromLine, AIndentLevel, AFoldRangeLevel: Integer;
      AFoldRegion: TBCEditorFoldRegionItem; AToLine: Integer = 0): TBCEditorCodeFoldingRange;
    procedure Clear;

    property Count: Integer read GetCount;
    property FoldRanges[Index: Integer]: TBCEditorCodeFoldingRange read GetRange; default;
    property Ranges: TList read FRanges;
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
    procedure Delete(FoldRange: TBCEditorCodeFoldingRange);
    procedure Assign(Source: TPersistent); override;
    procedure UpdateFoldRanges;
    procedure SetParentCollapsedOfSubFoldRanges(AFoldRange: TBCEditorCodeFoldingRange);

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
    FFoldRangeLevel: Integer;
    FSubFoldRanges: TBCEditorCodeFoldingRanges;
    FToLine: Integer;
    FIsExtraTokenFound: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Collapsable: Boolean;
    procedure MoveBy(LineCount: Integer);
    procedure MoveChildren(By: Integer);
    procedure SetParentCollapsedOfSubFoldRanges(AParentCollapsed: Boolean; ACollapsedBy: Integer);
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
    property FoldRangeLevel: Integer read FFoldRangeLevel write FFoldRangeLevel;
    property SubFoldRanges: TBCEditorCodeFoldingRanges read FSubFoldRanges;
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

  procedure RecursiveAssign(FoldRanges: TBCEditorCodeFoldingRanges; SrcFoldRanges: TBCEditorCodeFoldingRanges);
  var
    i: Integer;
    LCodeFoldingRange: TBCEditorCodeFoldingRange;
    LSourceCodeFoldingRange: TBCEditorCodeFoldingRange;
  begin
    if Assigned(SrcFoldRanges) then
    for i := 0 to SrcFoldRanges.Count - 1 do
    begin
      LSourceCodeFoldingRange := SrcFoldRanges[i];

      LCodeFoldingRange := FoldRanges.Add(Self, LSourceCodeFoldingRange.FromLine, LSourceCodeFoldingRange.IndentLevel,
        LSourceCodeFoldingRange.FoldRangeLevel, LSourceCodeFoldingRange.FoldRegion, LSourceCodeFoldingRange.ToLine);
      LCodeFoldingRange.CollapsedBy := LSourceCodeFoldingRange.CollapsedBy;
      LCodeFoldingRange.Collapsed := LSourceCodeFoldingRange.Collapsed;
      LCodeFoldingRange.ParentCollapsed := LSourceCodeFoldingRange.ParentCollapsed;
      if Assigned(LSourceCodeFoldingRange.CollapsedLines) then
        LCodeFoldingRange.CollapsedLines.Assign(LSourceCodeFoldingRange.CollapsedLines);
      LCodeFoldingRange.FoldRegion := LSourceCodeFoldingRange.FoldRegion;

      RecursiveAssign(LCodeFoldingRange.SubFoldRanges, SrcFoldRanges[i].SubFoldRanges);
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
    TObject(FAllRanges[i]).Free;
    FAllRanges[i] := nil;
    FAllRanges.Delete(i);
    Break;
  end;
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

procedure TBCEditorAllCodeFoldingRanges.SetParentCollapsedOfSubFoldRanges(AFoldRange: TBCEditorCodeFoldingRange);
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
      SetParentCollapsedOfSubFoldRanges(FoldRange);
  end;
end;

{ TBCEditorCodeFoldingRanges }

constructor TBCEditorCodeFoldingRanges.Create;
begin
  inherited;

  FRanges := TList.Create;
end;

destructor TBCEditorCodeFoldingRanges.Destroy;
begin
  FRanges.Clear;
  FRanges.Free;
  FRanges := nil;

  inherited;
end;

function TBCEditorCodeFoldingRanges.Add(AAllFold: TBCEditorAllCodeFoldingRanges; AFromLine, AIndentLevel, AFoldRangeLevel: Integer;
  AFoldRegion: TBCEditorFoldRegionItem; AToLine: Integer): TBCEditorCodeFoldingRange;
begin
  Result := TBCEditorCodeFoldingRange.Create;
  with Result do
  begin
    FromLine := AFromLine;
    ToLine := AToLine;
    IndentLevel := AIndentLevel;
    FoldRangeLevel := AFoldRangeLevel;
    AllCodeFoldingRanges := AAllFold;
    FoldRegion := AFoldRegion;
  end;
  FRanges.Add(Result);
  AAllFold.AllRanges.Add(Result);
end;

procedure TBCEditorCodeFoldingRanges.Clear;
begin
  FRanges.Clear;
end;

function TBCEditorCodeFoldingRanges.GetCount: Integer;
begin
  Result := FRanges.Count;
end;

function TBCEditorCodeFoldingRanges.GetRange(Index: Integer): TBCEditorCodeFoldingRange;
begin
  Result := FRanges[index];
end;

{ TBCEditorCodeFoldingRange }

function TBCEditorCodeFoldingRange.Collapsable: Boolean;
begin
  Result := FFromLine <> FToLine;
end;

constructor TBCEditorCodeFoldingRange.Create;
begin
  inherited;

  FSubFoldRanges := TBCEditorCodeFoldingRanges.Create;
  FCollapsedLines := TStringList.Create;
  FCollapsed := False;
  FCollapsedBy := -1;
  FIsExtraTokenFound := False;
end;

destructor TBCEditorCodeFoldingRange.Destroy;
begin;
  FSubFoldRanges.Clear;
  FSubFoldRanges.Free;
  FSubFoldRanges := nil;
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
begin
  for i := 0 to FSubFoldRanges.Count - 1 do
  begin
    FSubFoldRanges[i].MoveChildren(By);

    with FAllCodeFoldingRanges.AllRanges do
    if FSubFoldRanges[i].FParentCollapsed then
      Move(IndexOf(FSubFoldRanges[i]), IndexOf(FSubFoldRanges[i]) + By);
  end;
end;

procedure TBCEditorCodeFoldingRange.SetParentCollapsedOfSubFoldRanges(AParentCollapsed: Boolean; ACollapsedBy: Integer);
var
  i: Integer;
begin
  for i := 0 to FSubFoldRanges.Count - 1 do
  begin
    FSubFoldRanges[i].SetParentCollapsedOfSubFoldRanges(AParentCollapsed, ACollapsedBy);

    if (FSubFoldRanges[i].FCollapsedBy = -1) or (FSubFoldRanges[i].FCollapsedBy = ACollapsedBy) then
    begin
      FSubFoldRanges[i].FParentCollapsed := AParentCollapsed;

      if not AParentCollapsed then
        FSubFoldRanges[i].FCollapsedBy := -1
      else
        FSubFoldRanges[i].FCollapsedBy := ACollapsedBy;
    end;
  end;
end;

procedure TBCEditorCodeFoldingRange.Widen(LineCount: Integer);
begin
  Inc(FToLine, LineCount);
end;

end.

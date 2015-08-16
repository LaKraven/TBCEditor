unit BCEditor.Editor.Undo.List;

interface

uses
  System.Classes, BCEditor.Editor.Undo.Item, BCEditor.Types, BCEditor.Consts;

type
  TBCEditorUndoList = class(TPersistent)
  protected
    FBlockChangeNumber: Integer;
    FBlockCount: Integer;
    FFullUndoImposible: Boolean;
    FItems: TList;
    FLockCount: Integer;
    FMaxUndoActions: Integer;
    FNextChangeNumber: Integer;
    FInitialChangeNumber: Integer;
    FInsideRedo: Boolean;
    FOnAddedUndo: TNotifyEvent;
    procedure EnsureMaxEntries;
    function GetCanUndo: Boolean;
    function GetItemCount: Integer;
    procedure SetMaxUndoActions(Value: Integer);
    procedure SetInitialState(const Value: Boolean);
    function GetInitialState: Boolean;
    function GetItems(Index: Integer): TBCEditorUndoItem;
    procedure SetItems(Index: Integer; const Value: TBCEditorUndoItem);
  public
    constructor Create;
    destructor Destroy; override;

    function PeekItem: TBCEditorUndoItem;
    function PopItem: TBCEditorUndoItem;
    function LastChangeReason: TBCEditorChangeReason;
    function LastChangeString: string;
    procedure AddChange(AReason: TBCEditorChangeReason; const ACaretPosition, ASelectionStartPosition, ASelectionEndPosition: TBCEditorTextPosition;
      const ChangeText: string; SelectionMode: TBCEditorSelectionMode; Data: Pointer = nil; Index: Integer = 0);
    procedure BeginBlock;
    procedure Clear;
    procedure EndBlock;
    procedure Lock;
    procedure PushItem(Item: TBCEditorUndoItem);
    procedure Unlock;
  public
    procedure AddGroupBreak;
    procedure Assign(Source: TPersistent); override;
    procedure DeleteItem(AIndex: Integer);
    property BlockChangeNumber: Integer read FBlockChangeNumber write FBlockChangeNumber;
    property BlockCount: Integer read FBlockCount;
    property CanUndo: Boolean read GetCanUndo;
    property FullUndoImpossible: Boolean read FFullUndoImposible;
    property InitialState: Boolean read GetInitialState write SetInitialState;
    property InsideRedo: Boolean read FInsideRedo write FInsideRedo default False;
    property ItemCount: Integer read GetItemCount;
    property Items[index: Integer]: TBCEditorUndoItem read GetItems write SetItems;
    property MaxUndoActions: Integer read FMaxUndoActions write SetMaxUndoActions default BCEDITOR_MAX_UNDO_ACTIONS;
    property OnAddedUndo: TNotifyEvent read FOnAddedUndo write FOnAddedUndo;
  end;

implementation

{ TBCEditorUndoList }

constructor TBCEditorUndoList.Create;
begin
  inherited;

  FItems := TList.Create;
  FMaxUndoActions := BCEDITOR_MAX_UNDO_ACTIONS;
  FNextChangeNumber := 1;
  FInsideRedo := False;
end;

destructor TBCEditorUndoList.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TBCEditorUndoList.Assign(Source: TPersistent);
var
  i: Integer;
  UndoItem: TBCEditorUndoItem;
begin
  if Assigned(Source) and (Source is TBCEditorUndoList) then
  with Source as TBCEditorUndoList do
  begin
    Self.Clear;
    for i := 0 to (Source as TBCEditorUndoList).FItems.Count - 1 do
    begin
      UndoItem := TBCEditorUndoItem.Create;
      UndoItem.Assign(FItems[i]);
      Self.FItems.Add(UndoItem);
    end;
    Self.FBlockChangeNumber := FBlockChangeNumber;
    Self.FBlockCount := FBlockCount;
    Self.FFullUndoImposible := FFullUndoImposible;
    Self.FLockCount := FLockCount;
    Self.FMaxUndoActions := FMaxUndoActions;
    Self.FNextChangeNumber := FNextChangeNumber;
    Self.FInsideRedo := FInsideRedo;
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorUndoList.AddChange(AReason: TBCEditorChangeReason;
  const ACaretPosition, ASelectionStartPosition, ASelectionEndPosition: TBCEditorTextPosition;
  const ChangeText: string; SelectionMode: TBCEditorSelectionMode; Data: Pointer = nil; Index: Integer = 0);
var
  NewItem: TBCEditorUndoItem;
begin
  if FLockCount = 0 then
  begin
    NewItem := TBCEditorUndoItem.Create;
    try
      with NewItem do
      begin
        ChangeReason := AReason;
        ChangeSelectionMode := SelectionMode;
        ChangeCaretPosition := ACaretPosition;
        ChangeStartPosition := ASelectionStartPosition;
        ChangeEndPosition := ASelectionEndPosition;
        ChangeString := ChangeText;
        ChangeData := Data;
        //ChangeIndex := Index;

        if FBlockChangeNumber <> 0 then
          ChangeNumber := FBlockChangeNumber
        else
        begin
          ChangeNumber := FNextChangeNumber;
          if FBlockCount = 0 then
          begin
            Inc(FNextChangeNumber);
            if FNextChangeNumber = 0 then
              Inc(FNextChangeNumber);
          end;
        end;
      end;
      PushItem(NewItem);
    except
      NewItem.Free;
      raise;
    end;
  end;
end;

procedure TBCEditorUndoList.BeginBlock;
begin
  Inc(FBlockCount);
  FBlockChangeNumber := FNextChangeNumber;
end;

procedure TBCEditorUndoList.Clear;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    TBCEditorUndoItem(FItems[i]).Free;
  FItems.Clear;
  FFullUndoImposible := False;
end;

procedure TBCEditorUndoList.EndBlock;
var
  iBlockID: Integer;
begin
  if FBlockCount > 0 then
  begin
    Dec(FBlockCount);
    if FBlockCount = 0 then
    begin
      iBlockID := FBlockChangeNumber;
      FBlockChangeNumber := 0;
      Inc(FNextChangeNumber);
      if FNextChangeNumber = 0 then
        Inc(FNextChangeNumber);
      if (FItems.Count > 0) and (PeekItem.ChangeNumber = iBlockID) and Assigned(OnAddedUndo) then
      begin
        OnAddedUndo(Self);
      end;
    end;
  end;
end;

procedure TBCEditorUndoList.EnsureMaxEntries;
var
  Item: TBCEditorUndoItem;
begin
  if FItems.Count > FMaxUndoActions then
  begin
    FFullUndoImposible := True;
    while FItems.Count > FMaxUndoActions do
    begin
      Item := FItems[0];
      Item.Free;
      FItems.Delete(0);
    end;
  end;
end;

function TBCEditorUndoList.GetCanUndo: Boolean;
begin
  Result := FItems.Count > 0;
end;

function TBCEditorUndoList.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TBCEditorUndoList.Lock;
begin
  Inc(FLockCount);
end;

function TBCEditorUndoList.PeekItem: TBCEditorUndoItem;
var
  iLast: Integer;
begin
  Result := nil;
  iLast := FItems.Count - 1;
  if iLast >= 0 then
    Result := FItems[iLast];
end;

function TBCEditorUndoList.PopItem: TBCEditorUndoItem;
var
  iLast: Integer;
begin
  Result := nil;
  iLast := FItems.Count - 1;
  if iLast >= 0 then
  begin
    Result := FItems[iLast];
    FItems.Delete(iLast);
  end;
end;

procedure TBCEditorUndoList.PushItem(Item: TBCEditorUndoItem);
begin
  if Assigned(Item) then
  begin
    FItems.Add(Item);
    EnsureMaxEntries;
    if (Item.ChangeReason <> crGroupBreak) and Assigned(OnAddedUndo) then
      OnAddedUndo(Self);
  end;
end;

procedure TBCEditorUndoList.SetMaxUndoActions(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FMaxUndoActions then
  begin
    FMaxUndoActions := Value;
    EnsureMaxEntries;
  end;
end;

procedure TBCEditorUndoList.Unlock;
begin
  if FLockCount > 0 then
    Dec(FLockCount);
end;

function TBCEditorUndoList.LastChangeReason: TBCEditorChangeReason;
begin
  if FItems.Count = 0 then
    Result := crNothing
  else
    Result := TBCEditorUndoItem(FItems[FItems.Count - 1]).ChangeReason;
end;

function TBCEditorUndoList.LastChangeString: string;
begin
  if FItems.Count = 0 then
    Result := ''
  else
    Result := TBCEditorUndoItem(FItems[FItems.Count - 1]).ChangeString;
end;

procedure TBCEditorUndoList.AddGroupBreak;
var
  vDummy: TBCEditorTextPosition;
begin
  // Add the GroupBreak even if ItemCount = 0. Since items are stored in
  // reverse order in TBCCustomEditor.fRedoList, a GroupBreak could be lost.
  if LastChangeReason <> crGroupBreak then
    AddChange(crGroupBreak, vDummy, vDummy, vDummy, '', smNormal);
end;

procedure TBCEditorUndoList.SetInitialState(const Value: Boolean);
begin
  if Value then
  begin
    if ItemCount = 0 then
      FInitialChangeNumber := 0
    else
      FInitialChangeNumber := PeekItem.ChangeNumber;
  end
  else
  if ItemCount = 0 then
  begin
    if FInitialChangeNumber = 0 then
      FInitialChangeNumber := -1;
  end
  else
  if PeekItem.ChangeNumber = FInitialChangeNumber then
    FInitialChangeNumber := -1;
end;

function TBCEditorUndoList.GetInitialState: Boolean;
begin
  if ItemCount = 0 then
    Result := FInitialChangeNumber = 0
  else
    Result := PeekItem.ChangeNumber = FInitialChangeNumber;
end;

function TBCEditorUndoList.GetItems(Index: Integer): TBCEditorUndoItem;
begin
  Result := TBCEditorUndoItem(FItems[index]);
end;

procedure TBCEditorUndoList.SetItems(Index: Integer; const Value: TBCEditorUndoItem);
begin
  FItems[index] := Value;
end;

procedure TBCEditorUndoList.DeleteItem(AIndex: Integer);
begin
  TBCEditorUndoItem(FItems[AIndex]).Free;
  FItems.Delete(AIndex);
end;

end.

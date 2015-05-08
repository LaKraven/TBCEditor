unit BCEditor.Editor.Bookmarks;

interface

uses
  Vcl.Controls, System.Classes, System.Contnrs, BCEditor.Consts;

type
  TBCEditorBookmark = class
  protected
    FBookmarkNumber: Integer;
    FChar: Integer;
    FEdit: TCustomControl;
    FImage: Integer;
    FInternalImage: Boolean;
    FLine: Integer;
    FVisible: Boolean;
    function GetIsBookmark: Boolean;
    procedure Invalidate;
    procedure SetChar(const Value: Integer); virtual;
    procedure SetImage(const Value: Integer); virtual;
    procedure SetInternalImage(const Value: Boolean);
    procedure SetLine(const Value: Integer); virtual;
    procedure SetVisible(const Value: Boolean);
  public
    constructor Create(AOwner: TCustomControl);

    property BookmarkNumber: Integer read FBookmarkNumber write FBookmarkNumber;
    property Char: Integer read FChar write SetChar;
    property ImageIndex: Integer read FImage write SetImage;
    property InternalImage: Boolean read FInternalImage write SetInternalImage;
    property IsBookmark: Boolean read GetIsBookmark;
    property Line: Integer read FLine write SetLine;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  TBCEditorBookmarkEvent = procedure(Sender: TObject; var Mark: TBCEditorBookmark) of object;
  TBCEditorBookmarks = array [1 .. BCEDITOR_MAX_BOOKMARKS] of TBCEditorBookmark;

  TBCEditorBookmarkList = class(TObjectList)
  protected
    FEdit: TCustomControl;
    FOnChange: TNotifyEvent;
    function GetItem(AIndex: Integer): TBCEditorBookmark;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure SetItem(AIndex: Integer; AItem: TBCEditorBookmark);
    property OwnsObjects;
  public
    constructor Create(AOwner: TCustomControl);

    function Extract(AItem: TBCEditorBookmark): TBCEditorBookmark;
    function First: TBCEditorBookmark;
    function Last: TBCEditorBookmark;
    procedure ClearLine(ALine: Integer);
    procedure GetMarksForLine(ALine: Integer; var AMarks: TBCEditorBookmarks);
    procedure Place(AMark: TBCEditorBookmark);
  public
    property Items[AIndex: Integer]: TBCEditorBookmark read GetItem write SetItem; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  BCEditor.Editor.Base, System.Types;

{ TBCEditorBookmark }

constructor TBCEditorBookmark.Create(AOwner: TCustomControl);
begin
  inherited Create;

  FBookmarkNumber := -1;
  FEdit := AOwner as TBCBaseEditor;
end;

function TBCEditorBookmark.GetIsBookmark: Boolean;
begin
  Result := FBookmarkNumber >= 0;
end;

procedure TBCEditorBookmark.SetChar(const Value: Integer);
begin
  FChar := Value;
end;

procedure TBCEditorBookmark.Invalidate;
begin
  if FVisible and Assigned(FEdit) and (FEdit is TBCBaseEditor) then
    (FEdit as TBCBaseEditor).InvalidateLeftMarginLines(FLine, FLine);
end;

procedure TBCEditorBookmark.SetImage(const Value: Integer);
begin
  FImage := Value;
  Invalidate;
end;

procedure TBCEditorBookmark.SetInternalImage(const Value: Boolean);
begin
  FInternalImage := Value;
  Invalidate;
end;

procedure TBCEditorBookmark.SetLine(const Value: Integer);
begin
  if FVisible and Assigned(FEdit) then
  begin
    if FLine > 0 then
      Invalidate;
    FLine := Value;
    Invalidate;
  end
  else
    FLine := Value;
end;

procedure TBCEditorBookmark.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Invalidate;
  end;
end;

{ TBCEditorBookmarkList }

procedure TBCEditorBookmarkList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TBCEditorBookmarkList.GetItem(AIndex: Integer): TBCEditorBookmark;
begin
  Result := TBCEditorBookmark(inherited GetItem(AIndex));
end;

procedure TBCEditorBookmarkList.SetItem(AIndex: Integer; AItem: TBCEditorBookmark);
begin
  inherited SetItem(AIndex, AItem);
end;

constructor TBCEditorBookmarkList.Create(AOwner: TCustomControl);
begin
  inherited Create;
  FEdit := AOwner as TBCBaseEditor;
end;

function TBCEditorBookmarkList.First: TBCEditorBookmark;
begin
  Result := TBCEditorBookmark(inherited First);
end;

function TBCEditorBookmarkList.Last: TBCEditorBookmark;
begin
  Result := TBCEditorBookmark(inherited Last);
end;

function TBCEditorBookmarkList.Extract(AItem: TBCEditorBookmark): TBCEditorBookmark;
begin
  Result := TBCEditorBookmark(inherited Extract(AItem));
end;

procedure TBCEditorBookmarkList.ClearLine(ALine: Integer);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if not Items[i].IsBookmark and (Items[i].Line = ALine) then
      Delete(i);
end;

procedure TBCEditorBookmarkList.GetMarksForLine(ALine: Integer; var AMarks: TBCEditorBookmarks);
var
  i, j: Integer;
begin
  FillChar(AMarks, SizeOf(AMarks), 0);
  j := 0;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Line = ALine then
    begin
      Inc(j);
      AMarks[j] := Items[i];
      if j = BCEDITOR_MAX_BOOKMARKS then
        Break;
    end;
  end;
end;

procedure TBCEditorBookmarkList.Place(AMark: TBCEditorBookmark);
begin
  if Assigned(FEdit) and (FEdit is TBCBaseEditor) then
    if Assigned((FEdit as TBCBaseEditor).OnPlaceBookmark) then
      (FEdit as TBCBaseEditor).OnPlaceBookmark(FEdit, AMark);
  if Assigned(AMark) then
    Add(AMark);
end;

end.

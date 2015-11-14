unit BCEditor.Editor.CompletionProposal.Columns;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorProposalColumn = class(TCollectionItem)
  strict private
    FItemList: TStrings;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ItemList: TStrings read FItemList write FItemList;
  end;

  TBCEditorProposalColumns = class(TCollection)
  strict private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TBCEditorProposalColumn;
    procedure SetItem(Index: Integer; Value: TBCEditorProposalColumn);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
    function Add: TBCEditorProposalColumn;
    function FindItemID(ID: Integer): TBCEditorProposalColumn;
    function Insert(Index: Integer): TBCEditorProposalColumn;
    property Items[index: Integer]: TBCEditorProposalColumn read GetItem write SetItem; default;
  end;

implementation

{ TBCEditorProposalColumn }

constructor TBCEditorProposalColumn.Create(Collection: TCollection);
begin
  inherited;
  FItemList := TStringList.Create;
end;

destructor TBCEditorProposalColumn.Destroy;
begin
  FItemList.Free;

  inherited;
end;

procedure TBCEditorProposalColumn.Assign(Source: TPersistent);
begin
  if Source is TBCEditorProposalColumn then
  with Source as TBCEditorProposalColumn do
    Self.FItemList.Assign(FItemList)
  else
    inherited Assign(Source);
end;

{ TBCEditorProposalColumns }

constructor TBCEditorProposalColumns.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  FOwner := AOwner;
end;

function TBCEditorProposalColumns.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TBCEditorProposalColumns.GetItem(Index: Integer): TBCEditorProposalColumn;
begin
  Result := inherited GetItem(Index) as TBCEditorProposalColumn;
end;

procedure TBCEditorProposalColumns.SetItem(Index: Integer; Value: TBCEditorProposalColumn);
begin
  inherited SetItem(Index, Value);
end;

function TBCEditorProposalColumns.Add: TBCEditorProposalColumn;
begin
  Result := inherited Add as TBCEditorProposalColumn;
end;

function TBCEditorProposalColumns.FindItemID(ID: Integer): TBCEditorProposalColumn;
begin
  Result := inherited FindItemID(ID) as TBCEditorProposalColumn;
end;

function TBCEditorProposalColumns.Insert(Index: Integer): TBCEditorProposalColumn;
begin
  Result := inherited Insert(Index) as TBCEditorProposalColumn;
end;

end.

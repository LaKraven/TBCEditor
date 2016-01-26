unit BCEditor.Editor.SyncEdit;

interface

uses
  System.Classes, BCEditor.Types;

type
  TBCEditorSyncEdit = class(TPersistent)
  private
    FActive: Boolean;
    FEditBeginPosition: TBCEditorTextPosition;
    FEditEndPosition: TBCEditorTextPosition;
    FEditWidth: Integer;
    FEnabled: Boolean;
    FOnChange: TNotifyEvent;
    FShortCut: TShortCut;
    FSyncItems: TList;
    FOptions: TBCEditorSyncEditOptions;
    procedure DoChange(Sender: TObject);
    procedure SetActive(AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    function IsTextPositionInEdit(ATextPosition: TBCEditorTextPosition): Boolean;
    procedure Abort;
    procedure Assign(ASource: TPersistent); override;
    procedure ClearSyncItems;
    procedure MoveEndPositionChar(ACount: Integer);
    property Active: Boolean read FActive write SetActive default False;
    property EditBeginPosition: TBCEditorTextPosition read FEditBeginPosition write FEditBeginPosition;
    property EditEndPosition: TBCEditorTextPosition read FEditEndPosition write FEditEndPosition;
    property EditWidth: Integer read FEditWidth write FEditWidth;
    property SyncItems: TList read FSyncItems write FSyncItems;
  published
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Options: TBCEditorSyncEditOptions read FOptions write FOptions default [seCaseSensitive];
    property ShortCut: TShortCut read FShortCut write FShortCut;
  end;

implementation

uses
  Vcl.Menus;

{ TBCEditorSyncEdit }

constructor TBCEditorSyncEdit.Create;
begin
  inherited Create;

  FActive := False;
  FEnabled := True;
  FShortCut := Vcl.Menus.ShortCut(Ord('J'), [ssCtrl, ssShift]);
  FOptions := [seCaseSensitive];
  FSyncItems := TList.Create;
end;

destructor TBCEditorSyncEdit.Destroy;
begin
  ClearSyncItems;
  FSyncItems.Free;
  inherited;
end;

procedure TBCEditorSyncEdit.ClearSyncItems;
var
  i: Integer;
begin
  for i := FSyncItems.Count - 1 downto 0 do
    Dispose(PBCEditorTextPosition(FSyncItems.Items[i]));
  FSyncItems.Clear;
end;

procedure TBCEditorSyncEdit.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSyncEdit) then
  with ASource as TBCEditorSyncEdit do
  begin
    Self.Enabled := FEnabled;
    Self.FShortCut := FShortCut;
    Self.DoChange(Self);
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorSyncEdit.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Sender);
end;

procedure TBCEditorSyncEdit.SetActive(AValue: Boolean);
begin
  if FActive <> AValue then
  begin
    FActive := AValue;
    DoChange(Self);
  end;
end;

function TBCEditorSyncEdit.IsTextPositionInEdit(ATextPosition: TBCEditorTextPosition): Boolean;
begin
  Result := ((ATextPosition.Line > FEditBeginPosition.Line) or
    (ATextPosition.Line = FEditBeginPosition.Line) and (ATextPosition.Char >= FEditBeginPosition.Char))
    and
    ((ATextPosition.Line < FEditEndPosition.Line) or
    (ATextPosition.Line = FEditEndPosition.Line) and (ATextPosition.Char < FEditEndPosition.Char));
end;

procedure TBCEditorSyncEdit.MoveEndPositionChar(ACount: Integer);
begin
  Inc(FEditEndPosition.Char, ACount);
end;

procedure TBCEditorSyncEdit.Abort;
begin
  FActive := False;
end;

end.

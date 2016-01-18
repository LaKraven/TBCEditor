unit BCEditor.Editor.SyncEdit;

interface

uses
  System.Classes, BCEditor.Types;

type
  TBCEditorSyncEdit = class(TPersistent)
  private
    FActive: Boolean;
    FOnChange: TNotifyEvent;
    FShortCut: TShortCut;
    FEditBeginPosition: TBCEditorTextPosition;
    FEditEndPosition: TBCEditorTextPosition;
    procedure DoChange(Sender: TObject);
    procedure SetActive(AValue: Boolean);
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
    property Active: Boolean read FActive write SetActive default False;
    property EditBeginPosition: TBCEditorTextPosition read FEditBeginPosition write FEditBeginPosition;
    property EditEndPosition: TBCEditorTextPosition read FEditEndPosition write FEditEndPosition;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
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
  FShortCut := Vcl.Menus.ShortCut(Ord('J'), [ssCtrl, ssShift]);
end;

procedure TBCEditorSyncEdit.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorSyncEdit) then
  with ASource as TBCEditorSyncEdit do
  begin
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

end.

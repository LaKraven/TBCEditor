unit BCEditor.Editor.LeftMargin.Bookmarks;

interface

uses
  Vcl.Controls, System.Classes, Vcl.Graphics, BCEditor.Editor.LeftMargin.Bookmarks.Panel;

type
  TBCEditorLeftMarginBookMarks = class(TPersistent)
  strict private
    FImages: TImageList;
    FOnChange: TNotifyEvent;
    FOwner: TComponent;
    FPanel: TBCEditorLeftMarginBookMarkPanel;
    FShortCuts: Boolean;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetImages(const Value: TImageList);
    procedure SetOnChange(Value: TNotifyEvent);
    procedure SetVisible(Value: Boolean);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Images: TImageList read FImages write SetImages;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Panel: TBCEditorLeftMarginBookMarkPanel read FPanel write FPanel;
    property ShortCuts: Boolean read FShortCuts write FShortCuts default True;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

implementation

{ TBCEditorBookMarkOptions }

constructor TBCEditorLeftMarginBookMarks.Create(AOwner: TComponent);
begin
  inherited Create;

  FOwner := AOwner;
  FPanel := TBCEditorLeftMarginBookMarkPanel.Create;
  FShortCuts := True;
  FVisible := True;
end;

destructor TBCEditorLeftMarginBookMarks.Destroy;
begin
  FPanel.Free;

  inherited;
end;

procedure TBCEditorLeftMarginBookMarks.SetOnChange(Value: TNotifyEvent);
begin
  FOnChange := Value;
  FPanel.OnChange := Value;
end;

procedure TBCEditorLeftMarginBookMarks.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorLeftMarginBookMarks) then
  with Source as TBCEditorLeftMarginBookMarks do
  begin
    Self.FImages := FImages;
    Self.FShortCuts := FShortCuts;
    Self.FVisible := FVisible;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorLeftMarginBookMarks.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMarginBookMarks.SetImages(const Value: TImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    if Assigned(FImages) then
      FImages.FreeNotification(FOwner);
    DoChange;
  end;
end;

procedure TBCEditorLeftMarginBookMarks.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    DoChange;
  end;
end;

end.

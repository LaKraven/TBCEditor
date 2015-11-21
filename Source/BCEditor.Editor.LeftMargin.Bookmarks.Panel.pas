unit BCEditor.Editor.LeftMargin.Bookmarks.Panel;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Types;

type
  TBCEditorLeftMarginBookMarkPanel = class(TPersistent)
  strict private
    FLeftMargin: Integer;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorLeftMarginBookMarkPanelOptions;
    FOtherMarkXOffset: Integer;
    FVisible: Boolean;
    FWidth: Integer;
    procedure DoChange;
    procedure SetLeftMargin(Value: Integer);
    procedure SetOtherMarkXOffset(Value: Integer);
    procedure SetWidth(Value: Integer);
    procedure SetVisible(const Value: Boolean);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property LeftMargin: Integer read FLeftMargin write SetLeftMargin default 2;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Options: TBCEditorLeftMarginBookMarkPanelOptions read FOptions write FOptions default [bpoToggleBookmarkByClick];
    property OtherMarkXOffset: Integer read FOtherMarkXOffset write SetOtherMarkXOffset default 12;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Width: Integer read FWidth write SetWidth default 20;
  end;

implementation

uses
  System.Math;

constructor TBCEditorLeftMarginBookMarkPanel.Create;
begin
  inherited;

  FWidth := 20;
  FLeftMargin := 2;
  FOptions := [bpoToggleBookmarkByClick];
  FVisible := True;
  FOtherMarkXOffset := 12;
end;

procedure TBCEditorLeftMarginBookMarkPanel.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorLeftMarginBookMarkPanel) then
  with Source as TBCEditorLeftMarginBookMarkPanel do
  begin
    Self.FLeftMargin := FLeftMargin;
    Self.FOtherMarkXOffset := FOtherMarkXOffset;
    Self.FVisible := FVisible;
    Self.FWidth := FWidth;

    if Assigned(Self.FOnChange) then
      Self.FOnChange(Self);
  end
  else
    inherited;
end;

procedure TBCEditorLeftMarginBookMarkPanel.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLeftMarginBookMarkPanel.SetWidth(Value: Integer);
begin
  Value := Max(0, Value);
  if FWidth <> Value then
  begin
    FWidth := Value;
    DoChange
  end;
end;

procedure TBCEditorLeftMarginBookMarkPanel.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    DoChange
  end;
end;

procedure TBCEditorLeftMarginBookMarkPanel.SetLeftMargin(Value: Integer);
begin
  if FLeftMargin <> Value then
  begin
    FLeftMargin := Value;
    DoChange;
  end;
end;

procedure TBCEditorLeftMarginBookMarkPanel.SetOtherMarkXOffset(Value: Integer);
begin
  if FOtherMarkXOffset <> Value then
  begin
    FOtherMarkXOffset := Value;
    DoChange;
  end;
end;

end.

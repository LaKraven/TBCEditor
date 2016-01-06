unit BCEditor.Editor.Minimap.Indicator;

interface

uses
  System.Classes, BCEditor.Types;

type
  TBCEditorMinimapIndicator = class(TPersistent)
  strict private
    FAlphaBlending: Byte;
    FOnChange: TNotifyEvent;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetAlphaBlending(const AValue: Byte);
    procedure SetVisible(AValue: Boolean);
  public
    constructor Create;
    procedure Assign(ASource: TPersistent); override;
  published
    property AlphaBlending: Byte read FAlphaBlending write SetAlphaBlending default 96;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

implementation

{ TBCEditorSearchHighlighter }

constructor TBCEditorMinimapIndicator.Create;
begin
  inherited;

  FVisible := True;
  FAlphaBlending := 96;
end;

procedure TBCEditorMinimapIndicator.Assign(ASource: TPersistent);
begin
  if Assigned(ASource) and (ASource is TBCEditorMinimapIndicator) then
  with ASource as TBCEditorMinimapIndicator do
  begin
    Self.FAlphaBlending := FAlphaBlending;
    Self.FVisible := FVisible;
    Self.DoChange;
  end
  else
    inherited Assign(ASource);
end;

procedure TBCEditorMinimapIndicator.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorMinimapIndicator.SetAlphaBlending(const AValue: Byte);
begin
  if FAlphaBlending <> AValue then
  begin
    FAlphaBlending := AValue;
    DoChange;
  end;
end;

procedure TBCEditorMinimapIndicator.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    DoChange;
  end;
end;

end.

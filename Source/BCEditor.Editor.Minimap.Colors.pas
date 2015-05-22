unit BCEditor.Editor.Minimap.Colors;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Consts, BCEditor.Editor.CodeFolding.Types;

type
  TBCEditorMinimapColors = class(TPersistent)
  strict private
    FVisibleLines: TColor;
    FOnChange: TNotifyEvent;
    procedure SetVisibleLines(const Value: TColor);
    procedure DoChange;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property VisibleLines: TColor read FVisibleLines write SetVisibleLines default clMinimapVisibleLines;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TBCEditorMinimapColors }

constructor TBCEditorMinimapColors.Create;
begin
  inherited;

  FVisibleLines := clMinimapVisibleLines;
end;

procedure TBCEditorMinimapColors.Assign(Source: TPersistent);
begin
  if Source is TBCEditorMinimapColors then
  with Source as TBCEditorMinimapColors do
  begin
    Self.FVisibleLines := FVisibleLines;
    Self.DoChange;
  end
  else
    inherited;
end;

procedure TBCEditorMinimapColors.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorMinimapColors.SetVisibleLines(const Value: TColor);
begin
  if Value <> FVisibleLines then
  begin
    FVisibleLines := Value;
    DoChange;
  end;
end;

end.

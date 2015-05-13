unit BCEditor.Editor.Minimap;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Types;

type
  TBCEditorMinimap = class(TPersistent)
  strict private
    FCharHeight: Integer;
    FCharWidth: Integer;
    FClicked: Boolean;
    FDragging: Boolean;
    FFont: TFont;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorMinimapOptions;
    FTopLine: Integer;
    FVisible: Boolean;
    FVisibleLines: Integer;
    FWidth: Integer;
    procedure DoChange;
    procedure SetFont(Value: TFont);
    procedure SetOnChange(Value: TNotifyEvent);
    procedure SetVisible(Value: Boolean);
    procedure SetWidth(Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function GetWidth: Integer;
    procedure Assign(Source: TPersistent); override;
    property CharWidth: Integer read FCharWidth write FCharWidth;
    property CharHeight: Integer read FCharHeight write FCharHeight;
    property Clicked: Boolean read FClicked write FClicked;
    property Dragging: Boolean read FDragging write FDragging;
    property TopLine: Integer read FTopLine write FTopLine default 1;
    property VisibleLines: Integer read FVisibleLines write FVisibleLines;
  published
    property Font: TFont read FFont write SetFont;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Options: TBCEditorMinimapOptions read FOptions write FOptions default [moShowIndentGuides];
    property Visible: Boolean read FVisible write SetVisible default False;
    property Width: Integer read FWidth write SetWidth default 100;
  end;

implementation

uses
  System.Math;

{ TBCEditorMinimap }

constructor TBCEditorMinimap.Create;
begin
  inherited;

  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 3;
  FFont.Style := [];

  FVisible := False;
  FWidth := 140;
  FDragging := False;
  FOptions := [moShowIndentGuides];

  FClicked := False;

  FTopLine := 1;
end;

destructor TBCEditorMinimap.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TBCEditorMinimap.Assign(Source: TPersistent);
begin
  if Source is TBCEditorMinimap then
  with Source as TBCEditorMinimap do
  begin
    Self.FFont.Assign(FFont);
    Self.FOptions := FOptions;
    Self.FVisible := FVisible;
    Self.FWidth := FWidth;
  end
  else
    inherited;
end;

procedure TBCEditorMinimap.SetOnChange(Value: TNotifyEvent);
begin
  FOnChange := Value;
  FFont.OnChange := Value;
end;

procedure TBCEditorMinimap.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorMinimap.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TBCEditorMinimap.SetWidth(Value: Integer);
begin
  Value := Max(0, Value);
  if FWidth <> Value then
  begin
    FWidth := Value;
    DoChange;
  end;
end;

function TBCEditorMinimap.GetWidth: Integer;
begin
  if FVisible then
    Result := FWidth
  else
    Result := 0;
end;

procedure TBCEditorMinimap.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    DoChange;
  end;
end;

end.

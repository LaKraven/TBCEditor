unit BCEditor.Editor.Search.Map;

interface

uses
  System.Classes, BCEditor.Editor.Search.Map.Colors, BCEditor.Types;

type
  TBCEditorSearchMap = class(TPersistent)
  strict private
    FColors: TBCEditorSearchMapColors;
    FOnChange: TBCEditorSearchChangeEvent;
    FOptions: TBCEditorSearchMapOptions;
    FVisible: Boolean;
    FWidth: Integer;
    procedure DoChange;
    procedure SetOnChange(Value: TBCEditorSearchChangeEvent);
    procedure SetColors(const Value: TBCEditorSearchMapColors);
    procedure SetOptions(const Value: TBCEditorSearchMapOptions);
    procedure SetVisible(Value: Boolean);
    procedure SetWidth(Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetWidth: Integer;
  published
    property Colors: TBCEditorSearchMapColors read FColors write SetColors;
    property OnChange: TBCEditorSearchChangeEvent read FOnChange write SetOnChange;
    property Options: TBCEditorSearchMapOptions read FOptions write SetOptions default [moShowActiveLine];
    property Visible: Boolean read FVisible write SetVisible default False;
    property Width: Integer read FWidth write SetWidth default 5;
  end;

implementation

uses
  System.Math;

{ TBCEditorSearchMap }

constructor TBCEditorSearchMap.Create;
begin
  inherited;

  FColors := TBCEditorSearchMapColors.Create;
  FOptions := [moShowActiveLine];
  FVisible := False;
  FWidth := 5;
end;

destructor TBCEditorSearchMap.Destroy;
begin
  FColors.Free;
  inherited;
end;

procedure TBCEditorSearchMap.Assign(Source: TPersistent);
begin
  if Source is TBCEditorSearchMap then
  with Source as TBCEditorSearchMap do
  begin
    Self.FVisible := FVisible;
    Self.FOptions := Options;
    Self.FWidth := FWidth;
    Self.FColors := FColors;
    Self.DoChange;
  end
  else
    inherited;
end;

procedure TBCEditorSearchMap.SetOnChange(Value: TBCEditorSearchChangeEvent);
begin
  FOnChange := Value;
  FColors.OnChange := FOnChange;
end;

procedure TBCEditorSearchMap.SetWidth(Value: Integer);
begin
  Value := Max(0, Value);
  if FWidth <> Value then
    FWidth := Value;
  DoChange;
end;

procedure TBCEditorSearchMap.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(scSearch);
end;

procedure TBCEditorSearchMap.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    DoChange;
  end;
end;

procedure TBCEditorSearchMap.SetOptions(const Value: TBCEditorSearchMapOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    DoChange;
  end;
end;

function TBCEditorSearchMap.GetWidth: Integer;
begin
  if FVisible then
    Result := FWidth
  else
    Result := 0;
end;

procedure TBCEditorSearchMap.SetColors(const Value: TBCEditorSearchMapColors);
begin
  FColors.Assign(Value);
end;

end.

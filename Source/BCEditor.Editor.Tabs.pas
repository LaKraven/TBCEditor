unit BCEditor.Editor.Tabs;

interface

uses
  System.Classes, BCEditor.Types;

const
  BCEDITOR_DEFAULT_TAB_OPTIONS = [toSelectedBlockIndent];

type
  TBCEditorTabs = class(TPersistent)
  strict private
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorTabOptions;
    FWantTabs: Boolean;
    FWidth: Integer;
    procedure SetWidth(Value: Integer);
    procedure SetWantTabs(const Value: Boolean);
    procedure SetOptions(const Value: TBCEditorTabOptions);
    procedure DoChange;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Options: TBCEditorTabOptions read FOptions write SetOptions default BCEDITOR_DEFAULT_TAB_OPTIONS;
    property WantTabs: Boolean read FWantTabs write SetWantTabs default True;
    property Width: Integer read FWidth write SetWidth default 2;
  end;

implementation

uses
  BCEditor.Utils;

{ TBCEditorTabs }

constructor TBCEditorTabs.Create;
begin
  inherited;

  FOptions := BCEDITOR_DEFAULT_TAB_OPTIONS;
  FWantTabs := True;
  FWidth := 2;
end;

procedure TBCEditorTabs.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorTabs.Assign(Source: TPersistent);
begin
  if Source is TBCEditorTabs then
  with Source as TBCEditorTabs do
  begin
    Self.FOptions := FOptions;
    Self.FWantTabs := FWantTabs;
    Self.FWidth := FWidth;
    Self.DoChange;
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorTabs.SetOptions(const Value: TBCEditorTabOptions);
begin
  if Value <> FOptions then
  begin
    FOptions := Value;
    DoChange;
  end;
end;

procedure TBCEditorTabs.SetWidth(Value: Integer);
begin
  Value := MinMax(Value, 1, 256);
  if FWidth <> Value then
  begin
    FWidth := Value;
    DoChange;
  end;
end;

procedure TBCEditorTabs.SetWantTabs(const Value: Boolean);
begin
  if FWantTabs <> Value then
  begin
    FWantTabs := Value;
    DoChange;
  end;
end;

end.

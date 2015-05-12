unit BCEditor.Editor.Scroll;

interface

uses
  System.Classes, System.UITypes, BCEditor.Types, BCEditor.Editor.Scroll.Hint;

const
  BCEDITOR_DEFAULT_SCROLL_OPTIONS = [soAutosizeMaxWidth, soPastEndOfLine, soShowHint];

type
  TBCEditorScroll = class(TPersistent)
  strict private
    FBars: System.UITypes.TScrollStyle;
    FHint: TBCEditorScrollHint;
    FMaxWidth: Integer;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorScrollOptions;
    procedure DoChange;
    procedure SetBars(const Value: System.UITypes.TScrollStyle);
    procedure SetHint(const Value: TBCEditorScrollHint);
    procedure SetOptions(const Value: TBCEditorScrollOptions);
    procedure SetMaxWidth(Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Bars: System.UITypes.TScrollStyle read FBars write SetBars default System.UITypes.TScrollStyle.ssBoth;
    property Hint: TBCEditorScrollHint read FHint write SetHint;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth default 1024;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Options: TBCEditorScrollOptions read FOptions write SetOptions default BCEDITOR_DEFAULT_SCROLL_OPTIONS;
  end;

implementation

uses
  BCEditor.Utils;

{ TBCEditorScroll }

constructor TBCEditorScroll.Create;
begin
  inherited;

  FOptions := BCEDITOR_DEFAULT_SCROLL_OPTIONS;
  FMaxWidth := 1024;
  FBars := System.UITypes.TScrollStyle.ssBoth;
  FHint := TBCEditorScrollHint.Create;
end;

destructor TBCEditorScroll.Destroy;
begin
  FHint.Free;

  inherited;
end;

procedure TBCEditorScroll.SetBars(const Value: System.UITypes.TScrollStyle);
begin
  if FBars <> Value then
  begin
    FBars := Value;
    DoChange;
  end;
end;

procedure TBCEditorScroll.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorScroll.Assign(Source: TPersistent);
begin
  if Source is TBCEditorScroll then
  with Source as TBCEditorScroll do
  begin
    Self.FBars := FBars;
    Self.FHint.Assign(FHint);
    Self.FOptions := FOptions;
    Self.FMaxWidth := FMaxWidth;
    Self.DoChange;
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorScroll.SetOptions(const Value: TBCEditorScrollOptions);
begin
  if Value <> FOptions then
  begin
    FOptions := Value;
    DoChange;
  end;
end;

procedure TBCEditorScroll.SetMaxWidth(Value: Integer);
begin
  Value := MinMax(Value, 1, MaxInt - 1);
  if FMaxWidth <> Value then
  begin
    FMaxWidth := Value;
    DoChange;
  end;
end;

procedure TBCEditorScroll.SetHint(const Value: TBCEditorScrollHint);
begin
  FHint.Assign(Value);
end;

end.

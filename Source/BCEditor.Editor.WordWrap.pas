unit BCEditor.Editor.WordWrap;

interface

uses
  System.Classes, BCEditor.Editor.Glyph;

type
  TBCEditorWordWrapStyle = (wwsClientWidth, wwsRightMargin, wwsSpecified);

  TBCEditorWordWrap = class(TPersistent)
  strict private
    FEnabled: Boolean;
    FPosition: Integer;
    FOnChange: TNotifyEvent;
    FIndicator: TBCEditorGlyph;
    FStyle: TBCEditorWordWrapStyle;
    procedure DoChange;
    procedure SetEnabled(const Value: Boolean);
    procedure SetIndicator(const Value: TBCEditorGlyph);
    procedure SetPosition(const Value: Integer);
    procedure SetStyle(const Value: TBCEditorWordWrapStyle);
    procedure SetOnChange(Value: TNotifyEvent);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Position: Integer read FPosition write SetPosition;
    property Style: TBCEditorWordWrapStyle read FStyle write SetStyle;
    property Indicator: TBCEditorGlyph read FIndicator write SetIndicator;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

implementation

uses
  Vcl.Graphics;

{ TBCEditorWordWrap }

constructor TBCEditorWordWrap.Create;
begin
  inherited;

  FEnabled := False;
  FPosition := 80;
  FIndicator := TBCEditorGlyph.Create(HINSTANCE, 'BCEDITORWRAPPED', clLime);
  FStyle := wwsClientWidth;
end;

procedure TBCEditorWordWrap.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorWordWrap) then
  with Source as TBCEditorWordWrap do
  begin
    Self.FEnabled := FEnabled;
    Self.FPosition := FPosition;
    Self.FStyle := FStyle;
    Self.FIndicator.Assign(FIndicator);
    Self.DoChange;
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorWordWrap.SetOnChange(Value: TNotifyEvent);
begin
  FOnChange := Value;
  FIndicator.OnChange := Value;
end;

destructor TBCEditorWordWrap.Destroy;
begin
  FIndicator.Free;
  inherited;
end;

procedure TBCEditorWordWrap.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorWordWrap.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    DoChange;
  end;
end;

procedure TBCEditorWordWrap.SetIndicator(const Value: TBCEditorGlyph);
begin
  FIndicator.Assign(Value);
end;

procedure TBCEditorWordWrap.SetPosition(const Value: Integer);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    DoChange;
  end;
end;

procedure TBCEditorWordWrap.SetStyle(const Value: TBCEditorWordWrapStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    DoChange;
  end;
end;

end.

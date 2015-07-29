unit BCEditor.Editor.WordWrap;

interface

uses
  System.Classes, BCEditor.Editor.Glyph, Vcl.Graphics, BCEditor.Editor.WordWrap.Colors;

type
  TBCEditorWordWrapStyle = (wwsClientWidth, wwsRightMargin, wwsSpecified);

  TBCEditorWordWrap = class(TPersistent)
  strict private
    FBitmap: Vcl.Graphics.TBitmap;
    FColors: TBCEditorWordWrapColors;
    FEnabled: Boolean;
    FPosition: Integer;
    FOnChange: TNotifyEvent;
    FIndicator: TBCEditorGlyph;
    FStyle: TBCEditorWordWrapStyle;
    procedure CreateInternalBitmap;
    procedure DoChange;
    procedure OnColorsChange(Sender: TObject);
    procedure SetColors(const Value: TBCEditorWordWrapColors);
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
    property Colors: TBCEditorWordWrapColors read FColors write SetColors;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Position: Integer read FPosition write SetPosition;
    property Style: TBCEditorWordWrapStyle read FStyle write SetStyle;
    property Indicator: TBCEditorGlyph read FIndicator write SetIndicator;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

implementation

{ TBCEditorWordWrap }

constructor TBCEditorWordWrap.Create;
begin
  inherited;

  FColors := TBCEditorWordWrapColors.Create;

  FEnabled := False;
  FPosition := 80;
  FIndicator := TBCEditorGlyph.Create(HINSTANCE, '', clFuchsia);
  CreateInternalBitmap;
  FStyle := wwsClientWidth;
end;

destructor TBCEditorWordWrap.Destroy;
begin
  FBitmap.Free;
  FIndicator.Free;
  FColors.Free;

  inherited;
end;

procedure TBCEditorWordWrap.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorWordWrap) then
  with Source as TBCEditorWordWrap do
  begin
    Self.FColors.Assign(FColors);
    Self.FEnabled := FEnabled;
    Self.FPosition := FPosition;
    Self.FStyle := FStyle;
    Self.FIndicator.Assign(FIndicator);
    Self.DoChange;
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorWordWrap.CreateInternalBitmap;
begin
  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
  FBitmap := Vcl.Graphics.TBitmap.Create;
  with FBitmap do
  begin
    Canvas.Brush.Color := clFuchsia;
    Width := 15;
    Height := 14;
    Canvas.Pen.Color := FColors.Arrow;
    Canvas.MoveTo(6, 4);
    Canvas.LineTo(13, 4);
    Canvas.MoveTo(13, 5);
    Canvas.LineTo(13, 9);
    Canvas.MoveTo(12, 9);
    Canvas.LineTo(7, 9);
    Canvas.MoveTo(10, 7);
    Canvas.LineTo(10, 12);
    Canvas.MoveTo(9, 8);
    Canvas.LineTo(9, 11);
    Canvas.Pen.Color := FColors.Lines;
    Canvas.MoveTo(2, 6);
    Canvas.LineTo(7, 6);
    Canvas.MoveTo(2, 8);
    Canvas.LineTo(5, 8);
    Canvas.MoveTo(2, 10);
    Canvas.LineTo(5, 10);
    Canvas.MoveTo(2, 12);
    Canvas.LineTo(7, 12);
  end;
  FIndicator.MaskColor := clFuchsia;
  FIndicator.Glyph.Handle := FBitmap.Handle;
end;

procedure TBCEditorWordWrap.SetOnChange(Value: TNotifyEvent);
begin
  FOnChange := Value;
  FIndicator.OnChange := Value;
  FColors.OnChange := OnColorsChange;
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

procedure TBCEditorWordWrap.SetColors(const Value: TBCEditorWordWrapColors);
begin
  FColors.Assign(Value);
end;

procedure TBCEditorWordWrap.OnColorsChange(Sender: TObject);
begin
  CreateInternalBitmap;
end;

end.

unit BCEditor.Editor.Glyph;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorGlyph = class(TPersistent)
  strict private
    FInternalGlyph: TBitmap;
    FGlyph: TBitmap;
    FInternalMaskColor: TColor;
    FLeft: Integer;
    FMaskColor: TColor;
    FOnChange: TNotifyEvent;
    FVisible: Boolean;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure GlyphChange(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetLeft(Value: Integer);
    procedure SetMaskColor(Value: TColor);
    procedure SetVisible(Value: Boolean);
  public
    constructor Create(AModule: THandle; const AName: string; AMaskColor: TColor);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Draw(ACanvas: TCanvas; X, Y: Integer; ALineHeight: Integer);
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
  published
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Left: Integer read FLeft write SetLeft default 2;
    property MaskColor: TColor read FMaskColor write SetMaskColor default clNone;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

implementation

uses
  Winapi.Windows, System.SysUtils;

{ TBCEditorGlyph }

constructor TBCEditorGlyph.Create(AModule: THandle; const AName: string; AMaskColor: TColor);
begin
  inherited Create;

  if AName <> '' then
  begin
    FInternalGlyph := Vcl.Graphics.TBitmap.Create;
    FInternalGlyph.Handle := LoadBitmap(AModule, PChar(AName));
    FInternalMaskColor := AMaskColor;
  end
  else
    FInternalMaskColor := clNone;

  FVisible := True;
  FGlyph := Vcl.Graphics.TBitmap.Create;
  FGlyph.OnChange := GlyphChange;
  FMaskColor := clNone;
  FLeft := 2;
end;

destructor TBCEditorGlyph.Destroy;
begin
  if Assigned(FInternalGlyph) then
  begin
    FInternalGlyph.Free;
    FInternalGlyph := nil;
  end;

  FGlyph.Free;

  inherited Destroy;
end;

procedure TBCEditorGlyph.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorGlyph) then
  with Source as TBCEditorGlyph do
  begin
    if Assigned(FInternalGlyph) then
      Self.FInternalGlyph.Assign(FInternalGlyph);
    Self.FInternalMaskColor := FInternalMaskColor;
    Self.FVisible := FVisible;
    Self.FGlyph.Assign(FGlyph);
    Self.FMaskColor := FMaskColor;
    Self.FLeft := FLeft;
    if Assigned(Self.FOnChange) then
      Self.FOnChange(Self);
  end
  else
    inherited;
end;

procedure TBCEditorGlyph.Draw(ACanvas: TCanvas; X, Y: Integer; ALineHeight: Integer);
var
  SourceRect, DestinationRect: TRect;
  GlyphBitmap: Vcl.Graphics.TBitmap;
  MaskColor: TColor;
begin
  if not FGlyph.Empty then
  begin
    GlyphBitmap := FGlyph;
    MaskColor := FMaskColor;
  end
  else
  if Assigned(FInternalGlyph) then
  begin
    GlyphBitmap := FInternalGlyph;
    MaskColor := FInternalMaskColor;
  end
  else
    Exit;

  if aLineHeight >= GlyphBitmap.Height then
  begin
    SourceRect := Rect(0, 0, GlyphBitmap.Width, GlyphBitmap.Height);
    Inc(Y, (aLineHeight - GlyphBitmap.Height) div 2);
    DestinationRect := Rect(X, Y, X + GlyphBitmap.Width, Y + GlyphBitmap.Height);
  end
  else
  begin
    DestinationRect := Rect(X, Y, X + GlyphBitmap.Width, Y + aLineHeight);
    Y := (GlyphBitmap.Height - aLineHeight) div 2;
    SourceRect := Rect(0, Y, GlyphBitmap.Width, Y + aLineHeight);
  end;

  ACanvas.BrushCopy(DestinationRect, GlyphBitmap, SourceRect, MaskColor);
end;

procedure TBCEditorGlyph.SetGlyph(Value: Vcl.Graphics.TBitmap);
begin
  FGlyph.Assign(Value);
end;

procedure TBCEditorGlyph.GlyphChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorGlyph.SetMaskColor(Value: TColor);
begin
  if FMaskColor <> Value then
  begin
    FMaskColor := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TBCEditorGlyph.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TBCEditorGlyph.SetLeft(Value: Integer);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

function TBCEditorGlyph.GetWidth: Integer;
begin
  if not FGlyph.Empty then
    Result := FGlyph.Width
  else
  if Assigned(FInternalGlyph) then
    Result := FInternalGlyph.Width
  else
    Result := 0;
end;

function TBCEditorGlyph.GetHeight: Integer;
begin
  if not FGlyph.Empty then
    Result := FGlyph.Height
  else
  if Assigned(FInternalGlyph) then
    Result := FInternalGlyph.Height
  else
    Result := 0;
end;

end.

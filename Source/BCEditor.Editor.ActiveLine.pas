unit BCEditor.Editor.ActiveLine;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Editor.Glyph, BCEditor.Consts;

type
  TBCEditorActiveLine = class(TPersistent)
  strict private
    FColor: TColor;
    FIndicator: TBCEditorGlyph;
    FOnChange: TNotifyEvent;
    FVisible: Boolean;
    procedure DoChange(Sender: TObject);
    procedure SetColor(const Value: TColor);
    procedure SetIndicator(const Value: TBCEditorGlyph);
    procedure SetOnChange(Value: TNotifyEvent);
    procedure SetVisible(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clActiveLineBackground;
    property Indicator: TBCEditorGlyph read FIndicator write SetIndicator;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

implementation

{ TBCEditorActiveLine }

constructor TBCEditorActiveLine.Create;
begin
  inherited;

  FColor := clActiveLineBackground;
  FIndicator := TBCEditorGlyph.Create(HINSTANCE, 'BCEDITORACTIVELINE', clFuchsia);
  FIndicator.Visible := False;
  FVisible := True;
end;

destructor TBCEditorActiveLine.Destroy;
begin
  FIndicator.Free;

  inherited;
end;

procedure TBCEditorActiveLine.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorActiveLine) then
  with Source as TBCEditorActiveLine do
  begin
    Self.FColor := FColor;
    Self.FVisible := FVisible;
    Self.FIndicator.Assign(FIndicator);
    Self.DoChange(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorActiveLine.SetOnChange(Value: TNotifyEvent);
begin
  FOnChange := Value;
  FIndicator.OnChange := Value;
end;

procedure TBCEditorActiveLine.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Sender);
end;

procedure TBCEditorActiveLine.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    DoChange(Self);
  end;
end;

procedure TBCEditorActiveLine.SetIndicator(const Value: TBCEditorGlyph);
begin
  FIndicator.Assign(Value);
end;

procedure TBCEditorActiveLine.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    DoChange(Self);
  end;
end;

end.

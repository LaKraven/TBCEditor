unit BCEditor.Editor.RightMargin;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Editor.RightMargin.Colors, BCEditor.Types;

type
  TBCEditorRightMargin = class(TPersistent)
  strict private
    FColors: TBCEditorRightMarginColors;
    FMouseOver: Boolean;
    FMoving: Boolean;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorRightMarginOptions;
    FPosition: Integer;
    FVisible: Boolean;
    procedure DoChange;
    procedure SetColors(const Value: TBCEditorRightMarginColors);
    procedure SetOnChange(Value: TNotifyEvent);
    procedure SetPosition(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Moving: Boolean read FMoving write FMoving;
    property MouseOver: Boolean read FMouseOver write FMouseOver;
  published
    property Colors: TBCEditorRightMarginColors read FColors write SetColors;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Options: TBCEditorRightMarginOptions read FOptions write FOptions default [rmoMouseMove, rmoShowMovingHint];
    property Position: Integer read FPosition write SetPosition;
    property Visible: Boolean read FVisible write SetVisible;
  end;

implementation

{ TBCEditorRightMargin }

constructor TBCEditorRightMargin.Create;
begin
  inherited;

  FVisible := True;
  FPosition := 80;
  FColors := TBCEditorRightMarginColors.Create;
  FOptions := [rmoMouseMove, rmoShowMovingHint];
  FMoving := False;
  FMouseOver := False;
end;

destructor TBCEditorRightMargin.Destroy;
begin
  FColors.Free;
  inherited;
end;

procedure TBCEditorRightMargin.SetOnChange(Value: TNotifyEvent);
begin
  FOnChange := Value;
  FColors.OnChange := Value;
end;

procedure TBCEditorRightMargin.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorRightMargin) then
  with Source as TBCEditorRightMargin do
  begin
    Self.FVisible := FVisible;
    Self.FPosition := FPosition;
    Self.FColors.Assign(fColors);
    Self.FOptions := FOptions;
    Self.DoChange;
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorRightMargin.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorRightMargin.SetColors(const Value: TBCEditorRightMarginColors);
begin
  FColors.Assign(Value);
end;

procedure TBCEditorRightMargin.SetPosition(const Value: Integer);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    DoChange
  end;
end;

procedure TBCEditorRightMargin.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    DoChange
  end;
end;


end.

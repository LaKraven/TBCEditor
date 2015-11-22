unit BCEditor.Editor.LineSpacing;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TLineSpacingRule = (lsSingle, lsOneAndHalf, lsDouble, lsSpecified);

  TBCEditorLineSpacing = class(TPersistent)
  strict private
    FOnChange: TNotifyEvent;
    FRule: TLineSpacingRule;
    FSpacing: Integer;
    procedure DoChange;
    procedure SetRule(const AValue: TLineSpacingRule);
    procedure SetSpacing(const AValue: Integer);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Rule: TLineSpacingRule read FRule write SetRule default lsSpecified;
    property Spacing: Integer read FSpacing write SetSpacing default 1;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TBCEditorLineSpacing }

procedure TBCEditorLineSpacing.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorLineSpacing) then
  with Source as TBCEditorLineSpacing do
  begin
    Self.FSpacing := Spacing;
    Self.FRule := Rule;
    Self.DoChange;
  end
  else
    inherited Assign(Source);
end;

constructor TBCEditorLineSpacing.Create;
begin
  inherited;

  FSpacing := 1;
  FRule := lsSpecified;
end;

procedure TBCEditorLineSpacing.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorLineSpacing.SetSpacing(const AValue: Integer);
begin
  if AValue <> FSpacing then
  begin
    FSpacing := AValue;
    DoChange;
  end;
end;

procedure TBCEditorLineSpacing.SetRule(const AValue: TLineSpacingRule);
begin
  if AValue <> FRule then
  begin
    FRule := AValue;
    DoChange;
  end;
end;

end.


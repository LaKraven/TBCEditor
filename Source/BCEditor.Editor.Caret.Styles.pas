unit BCEditor.Editor.Caret.Styles;

interface

uses
  System.Classes, BCEditor.Types;

type
  TBCEditorCaretStyles = class(TPersistent)
  strict private
    FOverwrite: TBCEditorCaretStyle;
    FInsert: TBCEditorCaretStyle;
    FOnChange: TNotifyEvent;
    procedure DoChange;
    procedure SetInsert(const Value: TBCEditorCaretStyle);
    procedure SetOverwrite(const Value: TBCEditorCaretStyle);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Insert: TBCEditorCaretStyle read FInsert write SetInsert default csThinVerticalLine;
    property Overwrite: TBCEditorCaretStyle read FOverwrite write SetOverwrite default csThinVerticalLine;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TBCEditorCaretStyle }

constructor TBCEditorCaretStyles.Create;
begin
  inherited;

  FInsert := csThinVerticalLine;
  FOverwrite := csThinVerticalLine;
end;

procedure TBCEditorCaretStyles.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorCaretStyles) then
  with Source as TBCEditorCaretStyles do
  begin
    Self.FOverwrite := FOverwrite;
    Self.FInsert := FInsert;
    Self.DoChange;
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorCaretStyles.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TBCEditorCaretStyles.SetInsert(const Value: TBCEditorCaretStyle);
begin
  if FInsert <> Value then
  begin
    FInsert := Value;
    DoChange;
  end;
end;

procedure TBCEditorCaretStyles.SetOverwrite(const Value: TBCEditorCaretStyle);
begin
  if FOverwrite <> Value then
  begin
    FOverwrite := Value;
    DoChange;
  end;
end;

end.

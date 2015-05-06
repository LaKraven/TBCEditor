unit BCEditor.Editor.Caret;

interface

uses
  System.Classes, BCEditor.Editor.Caret.NonBlinking, BCEditor.Editor.Caret.Styles, BCEditor.Editor.Caret.Offsets,
  BCEditor.Types;

type
  TBCEditorCaret = class(TPersistent)
  strict private
    FNonBlinking: TBCEditorCaretNonBlinking;
    FOffsets: TBCEditorCaretOffsets;
    FOnChange: TNotifyEvent;
    FOptions: TBCEditorCaretOptions;
    FStyles: TBCEditorCaretStyles;
    FVisible: Boolean;
    procedure DoChange(Sender: TObject);
    procedure SetNonBlinking(Value: TBCEditorCaretNonBlinking);
    procedure SetOffsets(Value: TBCEditorCaretOffsets);
    procedure SetOnChange(Value: TNotifyEvent);
    procedure SetOptions(const Value: TBCEditorCaretOptions);
    procedure SetStyles(const Value: TBCEditorCaretStyles);
    procedure SetVisible(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property NonBlinking: TBCEditorCaretNonBlinking read FNonBlinking write SetNonBlinking;
    property Offsets: TBCEditorCaretOffsets read FOffsets write SetOffsets;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Options: TBCEditorCaretOptions read FOptions write SetOptions;
    property Styles: TBCEditorCaretStyles read FStyles write SetStyles;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

implementation

{ TBCEditorCaret }

constructor TBCEditorCaret.Create;
begin
  inherited;

  FNonBlinking := TBCEditorCaretNonBlinking.Create;
  FOffsets := TBCEditorCaretOffsets.Create;
  FStyles := TBCEditorCaretStyles.Create;
  FVisible := True;
end;

destructor TBCEditorCaret.Destroy;
begin
  FNonBlinking.Free;
  FOffsets.Free;
  FStyles.Free;

  inherited;
end;

procedure TBCEditorCaret.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorCaret) then
  with Source as TBCEditorCaret do
  begin
    Self.FStyles.Assign(FStyles);
    Self.FNonBlinking.Assign(FNonBlinking);
    Self.FOffsets.Assign(FOffsets);
    Self.FOptions := FOptions;
    Self.FVisible := FVisible;
    Self.DoChange(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TBCEditorCaret.SetOnChange(Value: TNotifyEvent);
begin
  FOnChange := Value;
  FOffsets.OnChange := Value;
  FStyles.OnChange := Value;
  FNonBlinking.OnChange := Value;
end;

procedure TBCEditorCaret.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Sender);
end;

procedure TBCEditorCaret.SetStyles(const Value: TBCEditorCaretStyles);
begin
  FStyles.Assign(Value);
end;

procedure TBCEditorCaret.SetNonBlinking(Value: TBCEditorCaretNonBlinking);
begin
  FNonBlinking.Assign(Value);
end;

procedure TBCEditorCaret.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    DoChange(Self);
  end;
end;

procedure TBCEditorCaret.SetOffsets(Value: TBCEditorCaretOffsets);
begin
  FOffsets.Assign(Value);
end;

procedure TBCEditorCaret.SetOptions(const Value: TBCEditorCaretOptions);
begin
  if Value <> FOptions then
  begin
    FOptions := Value;
    DoChange(Self);
  end;
end;

end.

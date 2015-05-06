unit BCEditor.Highlighter.Attributes;

interface

uses
  Winapi.Windows, System.Classes, Vcl.Graphics;

type
  TBCEditorHighlighterAttribute = class(TPersistent)
  strict private
    FBackground: TColor;
    FBackgroundDefault: TColor;
    FElement: string;
    FForeground: TColor;
    FForegroundDefault: TColor;
    FName: string;
    FOnChange: TNotifyEvent;
    FParentForeground: Boolean;
    FParentBackground: Boolean;
    FStyle: TFontStyles;
    FStyleDefault: TFontStyles;
    FUseParentElementForTokens: Boolean;
    function GetBackgroundColorStored: Boolean;
    function GetFontStyleStored: Boolean;
    function GetForegroundColorStored: Boolean;
    procedure Changed; virtual;
    procedure SetBackground(Value: TColor);
    procedure SetForeground(Value: TColor);
    procedure SetStyle(Value: TFontStyles);
  public
    constructor Create(const AttributeName: string);
    procedure Assign(Source: TPersistent); override;
    procedure AssignColorAndStyle(Source: TBCEditorHighlighterAttribute);
    procedure InternalSaveDefaultValues;
  public
    property Name: string read FName write FName;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Background: TColor read FBackground write SetBackground stored GetBackgroundColorStored;
    property Element: string read FElement write FElement;
    property Foreground: TColor read FForeground write SetForeground stored GetForegroundColorStored;
    property ParentForeground: Boolean read FParentForeground write FParentForeground;
    property ParentBackground: Boolean read FParentBackground write FParentBackground;
    property Style: TFontStyles read FStyle write SetStyle stored GetFontStyleStored;
    property UseParentElementForTokens: Boolean read FUseParentElementForTokens write FUseParentElementForTokens default False;
  end;

implementation

uses
  System.SysUtils;

{ TBCEditorHighlighterAttribute }

procedure TBCEditorHighlighterAttribute.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorHighlighterAttribute) then
    with Source as TBCEditorHighlighterAttribute do
    begin
      Self.FName := FName;
      Self.AssignColorAndStyle(Source as TBCEditorHighlighterAttribute);
    end
  else
    inherited Assign(Source);
end;

procedure TBCEditorHighlighterAttribute.AssignColorAndStyle(Source: TBCEditorHighlighterAttribute);
var
  IsChanged: Boolean;
begin
  IsChanged := False;
  if FBackground <> Source.FBackground then
  begin
    FBackground := Source.FBackground;
    IsChanged := True;
  end;
  if FForeground <> Source.FForeground then
  begin
    FForeground := Source.FForeground;
    IsChanged := True;
  end;
  if FStyle <> Source.FStyle then
  begin
    FStyle := Source.FStyle;
    IsChanged := True;
  end;
  FParentForeground := Source.ParentForeground;
  FParentBackground := Source.ParentBackground;
  if IsChanged then
    Changed;
end;

procedure TBCEditorHighlighterAttribute.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TBCEditorHighlighterAttribute.Create(const AttributeName: string);
begin
  inherited Create;

  FBackground := clNone;
  FForeground := clNone;
  FName := AttributeName;
  FUseParentElementForTokens := False;
end;

function TBCEditorHighlighterAttribute.GetBackgroundColorStored: Boolean;
begin
  Result := FBackground <> FBackgroundDefault;
end;

function TBCEditorHighlighterAttribute.GetForegroundColorStored: Boolean;
begin
  Result := FForeground <> FForegroundDefault;
end;

function TBCEditorHighlighterAttribute.GetFontStyleStored: Boolean;
begin
  Result := FStyle <> FStyleDefault;
end;

procedure TBCEditorHighlighterAttribute.InternalSaveDefaultValues;
begin
  FForegroundDefault := FForeground;
  FBackgroundDefault := FBackground;
  FStyleDefault := FStyle;
end;

procedure TBCEditorHighlighterAttribute.SetBackground(Value: TColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    Changed;
  end;
end;

procedure TBCEditorHighlighterAttribute.SetForeground(Value: TColor);
begin
  if FForeground <> Value then
  begin
    FForeground := Value;
    Changed;
  end;
end;

procedure TBCEditorHighlighterAttribute.SetStyle(Value: TFontStyles);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;

end.

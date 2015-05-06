unit BCEditor.Editor.Caret.NonBlinking.Colors;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorCaretNonBlinkingColors = class(TPersistent)
  strict private
    FBackground: TColor;
    FForeground: TColor;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Background: TColor read FBackground write FBackground default clBlack;
    property Foreground: TColor read FForeground write FForeground default clWhite;
  end;

implementation

constructor TBCEditorCaretNonBlinkingColors.Create;
begin
  inherited;

  FBackground := clBlack;
  FForeground := clWhite;
end;

procedure TBCEditorCaretNonBlinkingColors.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorCaretNonBlinkingColors) then
  with Source as TBCEditorCaretNonBlinkingColors do
  begin
    Self.FBackground := FBackground;
    Self.FForeground := FForeground;
  end
  else
    inherited Assign(Source);
end;

end.

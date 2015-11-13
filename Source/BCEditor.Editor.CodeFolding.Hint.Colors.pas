unit BCEditor.Editor.CodeFolding.Hint.Colors;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorCodeFoldingHintColors = class(TPersistent)
  strict private
    FBackground: TColor;
    FBorder: TColor;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Background: TColor read FBackground write FBackground default clWindow;
    property Border: TColor read FBorder write FBorder default clBtnFace;
  end;

implementation

{ TBCEditorCompletionProposalColors }

constructor TBCEditorCodeFoldingHintColors.Create;
begin
  inherited;

  FBackground := clWindow;
  FBorder := clBtnFace;
end;

procedure TBCEditorCodeFoldingHintColors.Assign(Source: TPersistent);
begin
  if Source is TBCEditorCodeFoldingHintColors then
  with Source as TBCEditorCodeFoldingHintColors do
  begin
    Self.FBackground := FBackground;
    Self.FBorder := FBorder;
  end
  else
    inherited;
end;

end.

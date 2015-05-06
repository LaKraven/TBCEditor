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
    property Border: TColor read FBorder write FBorder default clBtnFace;
    property Background: TColor read FBackground write FBackground default clWindow;
  end;

implementation

{ TBCEditorCompletionProposalColors }

constructor TBCEditorCodeFoldingHintColors.Create;
begin
  inherited;

  FBorder := clBtnFace;
  FBackground := clWindow;
end;

procedure TBCEditorCodeFoldingHintColors.Assign(Source: TPersistent);
begin
  if Source is TBCEditorCodeFoldingHintColors then
  with Source as TBCEditorCodeFoldingHintColors do
  begin
    Self.FBorder := FBorder;
    Self.FBackground := FBackground;
  end
  else
    inherited;
end;

end.

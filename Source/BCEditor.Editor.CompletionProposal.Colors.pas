unit BCEditor.Editor.CompletionProposal.Colors;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorCompletionProposalColors = class(TPersistent)
  strict private
    FBackground: TColor;
    FBorder: TColor;
    FSelectedBackground: TColor;
    FSelectedText: TColor;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Background: TColor read FBackground write FBackground default clWindow;
    property Border: TColor read FBorder write FBorder default clBtnFace;
    property SelectedBackground: TColor read FSelectedBackground write FSelectedBackground default clHighlight;
    property SelectedText: TColor read FSelectedText write FSelectedText default clHighlightText;
  end;

implementation

{ TBCEditorCompletionProposalColors }

constructor TBCEditorCompletionProposalColors.Create;
begin
  inherited;

  FBackground := clWindow;
  FBorder := clBtnFace;
  FSelectedBackground := clHighlight;
  FSelectedText := clHighlightText;
end;

procedure TBCEditorCompletionProposalColors.Assign(Source: TPersistent);
begin
  if Source is TBCEditorCompletionProposalColors then
  with Source as TBCEditorCompletionProposalColors do
  begin
    Self.FBackground := FBackground;
    Self.FBorder := FBorder;
    Self.FSelectedBackground := FSelectedBackground;
    Self.FSelectedText := FSelectedText;
  end
  else
    inherited;
end;

end.

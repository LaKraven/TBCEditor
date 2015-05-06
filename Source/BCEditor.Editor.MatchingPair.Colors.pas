unit BCEditor.Editor.MatchingPair.Colors;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TBCEditorMatchingPairColors = class(TPersistent)
  strict private
    FMatched: TColor;
    FUnmatched: TColor;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Matched: TColor read FMatched write FMatched default clAqua;
    property Unmatched: TColor read FUnmatched write FUnmatched default clYellow;
  end;

implementation

{ TBCEditorMatchingPairColors }

constructor TBCEditorMatchingPairColors.Create;
begin
  inherited;

  FMatched := clAqua;
  FUnmatched := clYellow;
end;

procedure TBCEditorMatchingPairColors.Assign(Source: TPersistent);
begin
  if Source is TBCEditorMatchingPairColors then
  with Source as TBCEditorMatchingPairColors do
  begin
    Self.FMatched := FMatched;
    Self.FUnmatched := FUnmatched;
  end
  else
    inherited;
end;

end.

unit BCEditor.Editor.MatchingPair;

interface

uses
  System.Classes, BCEditor.Editor.MatchingPair.Colors, BCEditor.Types;

type
  TBCEditorMatchingPair = class(TPersistent)
  strict private
    FColors: TBCEditorMatchingPairColors;
    FEnabled: Boolean;
    FOptions: TBCEditorMatchingPairOptions;
    procedure SetColors(const Value: TBCEditorMatchingPairColors);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Colors: TBCEditorMatchingPairColors read FColors write SetColors;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Options: TBCEditorMatchingPairOptions read FOptions write FOptions default [mpoHighlightAfterToken, mpoUseMatchedColor];
  end;

implementation

{ TBCEditorMatchingPair }

constructor TBCEditorMatchingPair.Create;
begin
  inherited;

  FColors := TBCEditorMatchingPairColors.Create;
  FEnabled := True;
  FOptions := [mpoHighlightAfterToken, mpoUseMatchedColor];
end;

destructor TBCEditorMatchingPair.Destroy;
begin
  FColors.Free;

  inherited;
end;

procedure TBCEditorMatchingPair.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorMatchingPair) then
  with Source as TBCEditorMatchingPair do
  begin
    Self.FEnabled := FEnabled;
    Self.FColors.Assign(FColors);
  end
  else
    inherited;
end;

procedure TBCEditorMatchingPair.SetColors(const Value: TBCEditorMatchingPairColors);
begin
  FColors.Assign(Value);
end;

end.

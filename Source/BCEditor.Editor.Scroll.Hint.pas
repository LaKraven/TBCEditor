unit BCEditor.Editor.Scroll.Hint;

interface

uses
  System.Classes, Vcl.Graphics;

type
  TScrollHintFormat = (shfTopLineOnly, shfTopToBottom);

  TBCEditorScrollHint = class(TPersistent)
  strict private
    FFormat: TScrollHintFormat;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Format: TScrollHintFormat read FFormat write FFormat default shfTopLineOnly;
  end;

implementation

constructor TBCEditorScrollHint.Create;
begin
  inherited;

  FFormat := shfTopLineOnly;
end;

procedure TBCEditorScrollHint.Assign(Source: TPersistent);
begin
  if Source is TBCEditorScrollHint then
  with Source as TBCEditorScrollHint do
    Self.FFormat := FFormat
  else
    inherited Assign(Source);
end;

end.

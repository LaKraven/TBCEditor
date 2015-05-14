unit BCEditor.Editor.Directories;

interface

uses
  System.Classes;

type
  TBCEditorDirectories = class(TPersistent)
  strict private
    FColors: string;
    FHighlighters: string;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Colors: string read FColors write FColors;
    property Highlighters: string read FHighlighters write FHighlighters;
  end;

implementation

constructor TBCEditorDirectories.Create;
begin
  inherited;

  FColors := 'Colors';
  FHighlighters := 'Highlighters'
end;

procedure TBCEditorDirectories.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorDirectories) then
  with Source as TBCEditorDirectories do
  begin
    Self.FColors := FColors;
    Self.FHighlighters := FHighlighters;
  end
  else
    inherited Assign(Source);
end;

end.

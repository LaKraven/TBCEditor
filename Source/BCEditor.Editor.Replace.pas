unit BCEditor.Editor.Replace;

interface

uses
  System.Classes, BCEditor.Types;

type
  TBCEditorReplace = class(TPersistent)
  strict private
    FAction: TBCEditorReplaceActionOption;
    FEngine: TBCEditorSearchEngine;
    FOptions: TBCEditorReplaceOptions;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Action: TBCEditorReplaceActionOption read FAction write FAction default eraReplace;
    property Engine: TBCEditorSearchEngine read FEngine write FEngine default seNormal;
    property Options: TBCEditorReplaceOptions read FOptions write FOptions default [roPrompt];
  end;

implementation

{ TBCEditorReplace }

constructor TBCEditorReplace.Create;
begin
  inherited;

  FAction := eraReplace;
  FEngine := seNormal;
  FOptions := [roPrompt];
end;

procedure TBCEditorReplace.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorReplace) then
  with Source as TBCEditorReplace do
  begin
    Self.FEngine := Engine;
    Self.FOptions := Options;
    Self.FAction := Action;
  end
  else
    inherited Assign(Source);
end;

end.

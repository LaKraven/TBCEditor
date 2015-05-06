unit BCEditor.Highlighter.Colors;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Highlighter.Info;

type
  TBCEditorHighlighterElement = record
    Background: TColor;
    Foreground: TColor;
    Name: string;
    Style: TFontStyles;
  end;
  PBCEditorHighlighterElement = ^TBCEditorHighlighterElement;

  TBCEditorHighlighterColors = class(TObject)
  strict private
    FFileName: string;
    FInfo: TBCEditorHighlighterInfo;
    FElements: TList;
    FName: string;
  public
    constructor Create;
    destructor Destroy; override;

    function GetElement(Name: string): PBCEditorHighlighterElement;
    procedure Clear;
    property FileName: string read FFileName write FFileName;
    property Info: TBCEditorHighlighterInfo read FInfo write FInfo;
    property Name: string read FName write FName;
    property Styles: TList read FElements write FElements;
  end;

implementation

{ TBCEditorHighlighterColors }

constructor TBCEditorHighlighterColors.Create;
begin
  inherited;
  FElements := TList.Create;
  FInfo := TBCEditorHighlighterInfo.Create;
end;

destructor TBCEditorHighlighterColors.Destroy;
begin
  Clear;
  FElements.Free;
  FInfo.Free;

  inherited;
end;

procedure TBCEditorHighlighterColors.Clear;
var
  i: Integer;
begin
  for i := FElements.Count - 1 downto 0 do
    Dispose(FElements.Items[i]);
  FElements.Clear;
end;

function TBCEditorHighlighterColors.GetElement(Name: string): PBCEditorHighlighterElement;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FElements.Count - 1 do
    if PBCEditorHighlighterElement(FElements.Items[i])^.Name = Name then
    begin
      Result := FElements.Items[i];
      Break;
    end;
end;

end.

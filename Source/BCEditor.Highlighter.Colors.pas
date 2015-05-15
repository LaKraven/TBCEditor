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
    FOwner: TObject;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;

    function GetElement(Name: string): PBCEditorHighlighterElement;
    procedure Clear;
    procedure LoadFromFile(AFileName: string);
    property FileName: string read FFileName write FFileName;
    property Info: TBCEditorHighlighterInfo read FInfo write FInfo;
    property Name: string read FName write FName;
    property Styles: TList read FElements write FElements;
  end;

implementation

uses
  System.SysUtils, BCEditor.Editor.Base, BCEditor.Highlighter, BCEditor.Highlighter.JSONImporter;

{ TBCEditorHighlighterColors }

constructor TBCEditorHighlighterColors.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
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
    Dispose(PBCEditorHighlighterElement(FElements.Items[i]));
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

procedure TBCEditorHighlighterColors.LoadFromFile(AFileName: string);
var
  LStream: TStream;
  LHighlighter: TBCEditorHighlighter;
  LEditor: TBCBaseEditor;
begin
  FFileName := AFileName;
  FName := ExtractFileName(AFileName);
  FName := Copy(FName, 1, Pos('.', FName) - 1);

  LHighlighter := TBCEditorHighlighter(FOwner);
  LEditor := LHighlighter.Editor as TBCBaseEditor;
  LStream := LEditor.CreateFileStream(LEditor.GetColorsFileName(AFileName));
  try
    with TBCEditorHighlighterJSONImporter.Create(LHighlighter) do
    try
      ImportColorsFromStream(LStream);
    finally
      Free;
    end;
  finally
    LStream.Free;
  end;
  LHighlighter.UpdateColors;
end;

end.

unit BCEditor.Export.HTML;

interface

uses
  System.Classes, BCEditor.Lines, BCEditor.Highlighter;

type
  TBCEditorExportHTML = class(TObject)
  private
    FEncodingName: string;
    FHighlighter: TBCEditorHighlighter;
    FLines: TBCEditorLines;
    FStringList: TStrings;
    procedure Execute;
    procedure CreateHeader;
    procedure CreateInternalCSS;
    procedure CreateLines;
    procedure CreateFooter;
  public
    constructor Create(ALines: TBCEditorLines; AHighlighter: TBCEditorHighlighter; AEncodingName: string); overload;
    destructor Destroy; override;

    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(var AStream: TStream);
  end;

implementation

uses
  BCEditor.Highlighter.Attributes, BCEditor.Highlighter.Colors, System.SysUtils, System.UITypes;

constructor TBCEditorExportHTML.Create(ALines: TBCEditorLines; AHighlighter: TBCEditorHighlighter; AEncodingName: string);
begin
  inherited Create;

  FStringList := TStringList.Create;

  FLines := ALines;
  FHighlighter := AHighlighter;
  FEncodingName := AEncodingName;
end;

destructor TBCEditorExportHTML.Destroy;
begin
  FStringList.Free;

  inherited Destroy;
end;

procedure TBCEditorExportHTML.Execute;
begin
  if not Assigned(FHighlighter) then
    Exit;
  if FLines.Count = 0 then
    Exit;

  CreateHeader;
  CreateLines;
  CreateFooter;
end;

procedure TBCEditorExportHTML.CreateHeader;
begin
  FStringList.Add('<!DOCTYPE HTML>');
  FStringList.Add('');
  FStringList.Add('<html>');
  FStringList.Add('<head>');
	FStringList.Add('  <meta charset="' + FEncodingName + '">');

  CreateInternalCSS;

  FStringList.Add('</head>');
  FStringList.Add('');
  FStringList.Add('<body>');
end;

procedure TBCEditorExportHTML.CreateInternalCSS;
var
  i: Integer;
  LStyles: TList;
  LElement: PBCEditorHighlighterElement;
begin
  FStringList.Add('  <style>');

  LStyles := FHighlighter.Colors.Styles;

  for i := 0 to LStyles.Count - 1 do
  begin
    LElement := LStyles.Items[i];

    FStringList.Add('    ' + LElement^.Name + ' { ');
    FStringList.Add('      color: #' + IntToHex(LElement^.Foreground, 6) + ';');
    FStringList.Add('      background-color: #' + IntToHex(LElement^.Background, 6) + ';');

    if TFontStyle.fsBold in LElement^.Style then
      FStringList.Add('      font-weight: bold;');

    if TFontStyle.fsItalic in LElement^.Style then
      FStringList.Add('      font-style: italic;');

    if TFontStyle.fsUnderline in LElement^.Style then
      FStringList.Add('      text-decoration: underline;');

    if TFontStyle.fsStrikeOut in LElement^.Style then
      FStringList.Add('      text-decoration: line-through;');

    FStringList.Add('    }');
  end;
  FStringList.Add('  </style>');
end;

procedure TBCEditorExportHTML.CreateLines;
var
  i: Integer;
  LHighlighterAttribute: TBCEditorHighlighterAttribute;
begin
  for i := 0 to FLines.Count - 1 do
  begin
    if i = 0 then
      FHighlighter.ResetCurrentRange
    else
      FHighlighter.SetCurrentRange(FLines.Ranges[i]);
    FHighlighter.SetCurrentLine(FLines[i]);
    while not FHighlighter.GetEndOfLine do
    begin
      LHighlighterAttribute := FHighlighter.GetTokenAttribute;


      FHighlighter.Next;
    end;
  end;
end;

procedure TBCEditorExportHTML.CreateFooter;
begin
  FStringList.Add('</body>');
  FStringList.Add('</html>');
end;

procedure TBCEditorExportHTML.SaveToFile(const AFileName: string);
begin
  Execute;
  FStringList.SaveToFile(AFileName);
end;

procedure TBCEditorExportHTML.SaveToStream(var AStream: TStream);
begin
  Execute;
  FStringList.SaveToStream(AStream);
end;

end.

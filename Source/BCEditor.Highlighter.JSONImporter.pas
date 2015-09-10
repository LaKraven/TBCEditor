unit BCEditor.Highlighter.JSONImporter;

interface

uses
  System.Classes, BCEditor.Editor.Base, BCEditor.Highlighter, BCEditor.Highlighter.Colors, BCEditor.Highlighter.Info,
  BCEditor.Highlighter.Attributes, BCEditor.Highlighter.Rules, BCEditor.Editor.SkipRegions,
  BCEditor.Editor.CodeFolding.Regions, JsonDataObjects;

type
  TBCEditorHighlighterJSONImporter = class(TObject)
  private
    FHighlighter: TBCEditorHighlighter;
    procedure ImportAttributes(AHighlighterAttribute: TBCEditorHighlighterAttribute; AAttributesObject: TJsonObject;
      AElementPrefix: string);
    procedure ImportCodeFolding(ACodeFoldingObject: TJsonObject);
    procedure ImportCodeFoldingFoldRegions(ACodeFoldingRegions: TBCEditorCodeFoldingRegions; ACodeFoldingObject: TJsonObject);
    procedure ImportCodeFoldingOptions(ACodeFoldingRegions: TBCEditorCodeFoldingRegions; ACodeFoldingObject: TJsonObject);
    procedure ImportCodeFoldingSkipRegions(ACodeFoldingRegions: TBCEditorCodeFoldingRegions; ACodeFoldingObject: TJsonObject);
    procedure ImportColors(AJSONObject: TJsonObject);
    procedure ImportColorsEditorProperties(AEditorObject: TJsonObject);
    procedure ImportColorsInfo(AInfoObject: TJsonObject);
    procedure ImportCompletionProposal(ACompletionProposalObject: TJsonObject);
    procedure ImportEditorProperties(AEditorObject: TJsonObject);
    procedure ImportElements(AColorsObject: TJsonObject);
    procedure ImportHighlighter(AJSONObject: TJsonObject);
    procedure ImportInfo(AInfoObject: TJsonObject);
    procedure ImportKeyList(AKeyList: TBCEditorKeyList; KeyListObject: TJsonObject; AElementPrefix: string);
    procedure ImportMatchingPair(AMatchingPairObject: TJsonObject);
    procedure ImportRange(ARange: TBCEditorRange; RangeObject: TJsonObject; AParentRange: TBCEditorRange = nil;
      ASkipBeforeSubRules: Boolean = False; AElementPrefix: string = '');
    procedure ImportSet(ASet: TBCEditorSet; SetObject: TJsonObject; AElementPrefix: string);
  public
    constructor Create(AHighlighter: TBCEditorHighlighter); overload;
    procedure ImportFromStream(AStream: TStream);
    procedure ImportColorsFromStream(AStream: TStream);
  end;

implementation

uses
  System.SysUtils, Vcl.Graphics, Vcl.Forms, BCEditor.Consts, BCEditor.Types, Vcl.Dialogs,
  BCEditor.Highlighter.Token,
   BCEditor.Editor.CodeFolding.Types;

function StringToColorDef(const AString: string; const DefaultColor: TColor): Integer;
begin
  if Trim(AString) = '' then
    Result := DefaultColor
  else
    Result := StringToColor(AString);
end;

function StrToSet(const AString: string): TBCEditorCharSet;
var
  i: Integer;
begin
  Result := [];
  for i := 1 to Length(AString) do
    Result := Result + [AString[i]];
end;

function StrToStrDef(const AString: string; const AStringDef: string): string;
begin
  if Trim(AString) = '' then
    Result := AStringDef
  else
    Result := AString
end;

function StrToFontStyle(const AString: string): TFontStyles;
begin
  Result := [];
  if Pos('Bold', AString) > 0 then
    Include(Result, fsBold);
  if Pos('Italic', AString) > 0 then
    Include(Result, fsItalic);
  if Pos('Underline', AString) > 0 then
    Include(Result, fsUnderline);
  if Pos('StrikeOut', AString) > 0 then
    Include(Result, fsStrikeOut);
end;

function StrToRegionType(const AString: string): TBCEditorSkipRegionItemType;
begin
  if AString = 'SingleLine' then
    Result := ritSingleLineComment
  else
  if AString = 'MultiLine' then
    Result := ritMultiLineComment
  else
    Result := ritString;
end;

function StrToRangeType(const AString: string): TBCEditorRangeType;
begin
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_ADDRESS then
    Result := ttAddress
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_ATTRIBUTE then
    Result := ttAttribute
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_CHARACTER then
    Result := ttChar
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_COMMENT then
    Result := ttComment
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_DIRECTIVE then
    Result := ttDirective
   else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_FLOAT then
    Result := ttFloat
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_HEX then
    Result := ttHex
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_MAIL_TO_LINK then
    Result := ttMailtoLink
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_METHOD then
    Result := ttMethod
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_METHOD_NAME then
    Result := ttMethodName
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_NUMBER then
    Result := ttNumber
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_OCTAL then
    Result := ttOctal
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_RESERVED_WORD then
    Result := ttReservedWord
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_STRING then
    Result := ttString
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_SYMBOL then
    Result := ttSymbol
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_WEB_LINK then
    Result := ttWebLink
  else
    Result := ttUnspecified;
end;

function StrToTokenKind(const AString: string): Integer;
begin
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_ADDRESS then
    Result := Integer(ttAddress)
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_CHARACTER then
    Result := Integer(ttChar)
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_COMMENT then
    Result := Integer(ttComment)
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_DIRECTIVE then
    Result := Integer(ttDirective)
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_FLOAT then
    Result := Integer(ttFloat)
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_HEX then
    Result := Integer(ttHex)
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_MAIL_TO_LINK then
    Result := Integer(ttMailtoLink)
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_NUMBER then
    Result := Integer(ttNumber)
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_OCTAL then
    Result := Integer(ttOctal)
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_RESERVED_WORD then
    Result := Integer(ttReservedWord)
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_STRING then
    Result := Integer(ttString)
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_SYMBOL then
    Result := Integer(ttSymbol)
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_WEB_LINK then
    Result := Integer(ttWebLink)
  else
    Result := Integer(ttUnspecified);
end;

{ TBCEditorHighlighterJSONImporter }

constructor TBCEditorHighlighterJSONImporter.Create(AHighlighter: TBCEditorHighlighter);
begin
  inherited Create;
  FHighlighter := AHighlighter;
end;

procedure TBCEditorHighlighterJSONImporter.ImportInfo(AInfoObject: TJsonObject);
var
  i: Integer;
  LSampleArray: TJsonArray;
  LHighlighterInfo: TBCEditorHighlighterInfo;
begin
  if Assigned(AInfoObject) then
  begin
    LHighlighterInfo := FHighlighter.Info;
    { General }
    FHighlighter.MultiHighlighter := AInfoObject['General'].B['MultiHighlighter'];
    LHighlighterInfo.General.Version := AInfoObject['General']['Version'].Value;
    LHighlighterInfo.General.Date := AInfoObject['General']['Date'].Value;
    LSampleArray := AInfoObject['General'].A['Sample'];
    for i := 0 to LSampleArray.Count - 1 do
      LHighlighterInfo.General.Sample := LHighlighterInfo.General.Sample + LSampleArray.S[i];
    { Author }
    LHighlighterInfo.Author.Name := AInfoObject['Author']['Name'].Value;
    LHighlighterInfo.Author.Email := AInfoObject['Author']['Email'].Value;
    LHighlighterInfo.Author.Comments := AInfoObject['Author']['Comments'].Value;
  end;
end;

procedure TBCEditorHighlighterJSONImporter.ImportColorsInfo(AInfoObject: TJsonObject);
var
  LHighlighterInfo: TBCEditorHighlighterInfo;
begin
  if Assigned(AInfoObject) then
  begin
    LHighlighterInfo := FHighlighter.Colors.Info;
    { General }
    LHighlighterInfo.General.Version := AInfoObject['General']['Version'].Value;
    LHighlighterInfo.General.Date := AInfoObject['General']['Date'].Value;
    { Author }
    LHighlighterInfo.Author.Name := AInfoObject['Author']['Name'].Value;
    LHighlighterInfo.Author.Email := AInfoObject['Author']['Email'].Value;
    LHighlighterInfo.Author.Comments := AInfoObject['Author']['Comments'].Value;
  end;
end;

procedure TBCEditorHighlighterJSONImporter.ImportEditorProperties(AEditorObject: TJsonObject);
begin
  if Assigned(AEditorObject) then
    TBCBaseEditor(FHighlighter.Editor).URIOpener := StrToBoolDef(AEditorObject['URIOpener'].Value, False);
end;

procedure TBCEditorHighlighterJSONImporter.ImportColorsEditorProperties(AEditorObject: TJsonObject);
var
  LColorsObject, LFontsObject, LFontSizesObject: TJsonObject;
  LEditor: TBCBaseEditor;
begin
  if Assigned(AEditorObject) then
  begin
    LEditor := FHighlighter.Editor as TBCBaseEditor;
    LColorsObject := AEditorObject['Colors'].ObjectValue;
    if Assigned(LColorsObject) then
    begin
      LEditor.BackgroundColor := StringToColorDef(LColorsObject['Background'].Value, LEditor.BackgroundColor);
      LEditor.ActiveLine.Color := StringToColorDef(LColorsObject['ActiveLineBackground'].Value, LEditor.ActiveLine.Color);
      LEditor.CodeFolding.Colors.ActiveLineBackground := StringToColorDef(LColorsObject['CodeFoldingActiveLineBackground'].Value, LEditor.CodeFolding.Colors.ActiveLineBackground);
      LEditor.CodeFolding.Colors.Background := StringToColorDef(LColorsObject['CodeFoldingBackground'].Value, LEditor.CodeFolding.Colors.Background);
      LEditor.CodeFolding.Colors.CollapsedLine := StringToColorDef(LColorsObject['CodeFoldingCollapsedLine'].Value, LEditor.CodeFolding.Colors.CollapsedLine);
      LEditor.CodeFolding.Colors.FoldingLine := StringToColorDef(LColorsObject['CodeFoldingFoldingLine'].Value, LEditor.CodeFolding.Colors.FoldingLine);
      LEditor.CodeFolding.Colors.FoldingLineHighlight := StringToColorDef(LColorsObject['CodeFoldingFoldingLineHighlight'].Value, LEditor.CodeFolding.Colors.FoldingLineHighlight);
      LEditor.CodeFolding.Colors.Indent := StringToColorDef(LColorsObject['CodeFoldingIndent'].Value, LEditor.CodeFolding.Colors.Indent);
      LEditor.CodeFolding.Colors.IndentHighlight := StringToColorDef(LColorsObject['CodeFoldingIndentHighlight'].Value, LEditor.CodeFolding.Colors.IndentHighlight);
      LEditor.CodeFolding.Hint.Colors.Background := StringToColorDef(LColorsObject['CodeFoldingHintBackground'].Value, LEditor.CodeFolding.Hint.Colors.Background);
      LEditor.CodeFolding.Hint.Colors.Border := StringToColorDef(LColorsObject['CodeFoldingHintBorder'].Value, LEditor.CodeFolding.Hint.Colors.Border);
      LEditor.CodeFolding.Hint.Font.Color := StringToColorDef(LColorsObject['CodeFoldingHintText'].Value, LEditor.CodeFolding.Hint.Font.Color);
      LEditor.CompletionProposal.Colors.Background := StringToColorDef(LColorsObject['CompletionProposalBackground'].Value, LEditor.CompletionProposal.Colors.Background);
      LEditor.CompletionProposal.Font.Color := StringToColorDef(LColorsObject['CompletionProposalForeground'].Value, LEditor.CompletionProposal.Font.Color);
      LEditor.CompletionProposal.Colors.Border := StringToColorDef(LColorsObject['CompletionProposalBorder'].Value, LEditor.CompletionProposal.Colors.Border);
      LEditor.CompletionProposal.Colors.SelectedBackground := StringToColorDef(LColorsObject['CompletionProposalSelectedBackground'].Value, LEditor.CompletionProposal.Colors.SelectedBackground);
      LEditor.CompletionProposal.Colors.SelectedText := StringToColorDef(LColorsObject['CompletionProposalSelectedText'].Value, LEditor.CompletionProposal.Colors.SelectedText);
      LEditor.LeftMargin.Colors.ActiveLineBackground := StringToColorDef(LColorsObject['LeftMarginActiveLineBackground'].Value, LEditor.LeftMargin.Colors.ActiveLineBackground);
      LEditor.LeftMargin.Colors.Background := StringToColorDef(LColorsObject['LeftMarginBackground'].Value, LEditor.LeftMargin.Colors.Background);
      LEditor.LeftMargin.Colors.Border := StringToColorDef(LColorsObject['LeftMarginBorder'].Value, LEditor.LeftMargin.Colors.Border);
      LEditor.LeftMargin.Colors.LineNumberLine := StringToColorDef(LColorsObject['LeftMarginLineNumberLine'].Value, LEditor.LeftMargin.Colors.LineNumberLine);
      LEditor.LeftMargin.Font.Color := StringToColorDef(LColorsObject['LeftMarginLineNumbers'].Value, LEditor.LeftMargin.Font.Color);
      LEditor.LeftMargin.Colors.BookmarkPanelBackground := StringToColorDef(LColorsObject['LeftMarginBookmarkPanel'].Value, LEditor.LeftMargin.Colors.BookmarkPanelBackground);
      LEditor.LeftMargin.Colors.LineStateModified := StringToColorDef(LColorsObject['LeftMarginLineStateModified'].Value, LEditor.LeftMargin.Colors.LineStateModified);
      LEditor.LeftMargin.Colors.LineStateNormal := StringToColorDef(LColorsObject['LeftMarginLineStateNormal'].Value, LEditor.LeftMargin.Colors.LineStateNormal);
      LEditor.Minimap.Colors.VisibleLines := StringToColorDef(LColorsObject['MinimapVisibleLines'].Value, LEditor.Minimap.Colors.VisibleLines);
      LEditor.MatchingPair.Colors.Matched := StringToColorDef(LColorsObject['MatchingPairMatched'].Value, LEditor.MatchingPair.Colors.Matched);
      LEditor.MatchingPair.Colors.Underline := StringToColorDef(LColorsObject['MatchingPairUnderline'].Value, LEditor.MatchingPair.Colors.Underline);
      LEditor.MatchingPair.Colors.Unmatched := StringToColorDef(LColorsObject['MatchingPairUnmatched'].Value, LEditor.MatchingPair.Colors.Unmatched);
      LEditor.RightMargin.Colors.Edge := StringToColorDef(LColorsObject['RightEdge'].Value, LEditor.RightMargin.Colors.Edge);
      LEditor.RightMargin.Colors.MovingEdge := StringToColorDef(LColorsObject['RightMovingEdge'].Value, LEditor.RightMargin.Colors.MovingEdge);
      LEditor.Search.Highlighter.Colors.Background := StringToColorDef(LColorsObject['SearchHighlighterBackground'].Value, LEditor.Search.Highlighter.Colors.Background);
      LEditor.Search.Highlighter.Colors.Foreground := StringToColorDef(LColorsObject['SearchHighlighterForeground'].Value, LEditor.Search.Highlighter.Colors.Foreground);
      LEditor.Search.Map.Colors.ActiveLine := StringToColorDef(LColorsObject['SearchMapActiveLine'].Value, LEditor.Search.Map.Colors.ActiveLine);
      LEditor.Search.Map.Colors.Background := StringToColorDef(LColorsObject['SearchMapBackground'].Value, LEditor.Search.Map.Colors.Background);
      LEditor.Search.Map.Colors.Foreground := StringToColorDef(LColorsObject['SearchMapForeground'].Value, LEditor.Search.Map.Colors.Foreground);
      LEditor.Selection.Colors.Background := StringToColorDef(LColorsObject['SelectionBackground'].Value, LEditor.Selection.Colors.Background);
      LEditor.Selection.Colors.Foreground := StringToColorDef(LColorsObject['SelectionForeground'].Value, LEditor.Selection.Colors.Foreground);
      LEditor.WordWrap.Colors.Arrow := StringToColorDef(LColorsObject['WordWrapIndicatorArrow'].Value, LEditor.WordWrap.Colors.Arrow);
      LEditor.WordWrap.Colors.Lines := StringToColorDef(LColorsObject['WordWrapIndicatorLines'].Value, LEditor.WordWrap.Colors.Lines);
    end;
    LFontsObject := AEditorObject['Fonts'].ObjectValue;
    if Assigned(LFontsObject) then
    begin
      LEditor.LeftMargin.Font.Name := StrToStrDef(LFontsObject['LineNumbers'].Value, LEditor.LeftMargin.Font.Name);
      LEditor.Font.Name := StrToStrDef(LFontsObject['Text'].Value, LEditor.Font.Name);
      LEditor.Minimap.Font.Name := StrToStrDef(LFontsObject['Minimap'].Value, LEditor.Minimap.Font.Name);
      LEditor.CodeFolding.Hint.Font.Name := StrToStrDef(LFontsObject['CodeFoldingHint'].Value, LEditor.CodeFolding.Hint.Font.Name);
      LEditor.CompletionProposal.Font.Name := StrToStrDef(LFontsObject['CompletionProposal'].Value, LEditor.CompletionProposal.Font.Name);
    end;
    LFontSizesObject := AEditorObject['FontSizes'].ObjectValue;
    if Assigned(LFontSizesObject) then
    begin
      LEditor.LeftMargin.Font.Size := StrToIntDef(LFontSizesObject['LineNumbers'].Value, LEditor.LeftMargin.Font.Size);
      LEditor.Font.Size := StrToIntDef(LFontSizesObject['Text'].Value, LEditor.Font.Size);
      LEditor.Minimap.Font.Size := StrToIntDef(LFontSizesObject['Minimap'].Value, LEditor.Minimap.Font.Size);
      LEditor.CodeFolding.Hint.Font.Size := StrToIntDef(LFontSizesObject['CodeFoldingHint'].Value, LEditor.CodeFolding.Hint.Font.Size);
      LEditor.CompletionProposal.Font.Size := StrToIntDef(LFontSizesObject['CompletionProposal'].Value, LEditor.CompletionProposal.Font.Size);
    end;
  end;
end;

procedure TBCEditorHighlighterJSONImporter.ImportAttributes(AHighlighterAttribute: TBCEditorHighlighterAttribute;
  AAttributesObject: TJsonObject; AElementPrefix: string);
begin
  if Assigned(AAttributesObject) then
  begin
    AHighlighterAttribute.Element := AElementPrefix + AAttributesObject['Element'].Value;
    AHighlighterAttribute.ParentForeground := AAttributesObject.B['ParentForeground'];
    AHighlighterAttribute.ParentBackground := AAttributesObject.B['ParentBackground'];
  end;
end;

procedure TBCEditorHighlighterJSONImporter.ImportKeyList(AKeyList: TBCEditorKeyList; KeyListObject: TJsonObject;
  AElementPrefix: string);
var
  i: Integer;
  LWordArray: TJsonArray;
begin
  if Assigned(KeyListObject) then
  begin
    AKeyList.TokenType := StrToRangeType(KeyListObject['Type'].Value);
    LWordArray := KeyListObject.A['Words'];
    for i := 0 to LWordArray.Count - 1 do
      AKeyList.KeyList.Add(LWordArray.S[i]);
    ImportAttributes(AKeyList.Attribute, KeyListObject['Attributes'].ObjectValue, AElementPrefix);
  end;
end;

procedure TBCEditorHighlighterJSONImporter.ImportSet(ASet: TBCEditorSet; SetObject: TJsonObject;
  AElementPrefix: string);
begin
  if Assigned(SetObject) then
  begin
    ASet.CharSet := StrToSet(SetObject['Symbols'].Value);
    ImportAttributes(ASet.Attribute, SetObject['Attributes'].ObjectValue, AElementPrefix);
  end;
end;

procedure TBCEditorHighlighterJSONImporter.ImportRange(ARange: TBCEditorRange; RangeObject: TJsonObject;
  AParentRange: TBCEditorRange = nil; ASkipBeforeSubRules: Boolean = False;
  AElementPrefix: string = ''); { Recursive method }
var
  i, j: Integer;
  LFileName: string;
  NewRange: TBCEditorRange;
  NewKeyList: TBCEditorKeyList;
  NewSet: TBCEditorSet;
  SubRulesObject, PropertiesObject, TokenRangeObject: TJsonObject;
  LJSONObject, LJSONSubRulesObject: TJsonObject;
  LAlternativeCloseArray: TJsonArray;
  LFileStream: TStream;
  LEditor: TBCBaseEditor;
  LElementPrefix: string;
begin
  if Assigned(RangeObject) then
  begin
    LFileName := RangeObject['File'].Value;
    if FHighlighter.MultiHighlighter and (LFileName <> '') then
    begin
      LElementPrefix := RangeObject['ElementPrefix'].Value;
      LEditor := FHighlighter.Editor as TBCBaseEditor;
      LFileStream := LEditor.CreateFileStream(LEditor.GetHighlighterFileName(LFileName));
      LJSONObject := TJsonObject.ParseFromStream(LFileStream) as TJsonObject;
      if Assigned(LJSONObject) then
      try
        TokenRangeObject := LJSONObject['Highlighter']['MainRules'].ObjectValue;
        { You can include MainRules... }
        if TokenRangeObject['Name'].Value = RangeObject['IncludeRange'].Value then
          ImportRange(AParentRange, TokenRangeObject, nil, True, LElementPrefix)
        else
        { or SubRules... }
        begin
          SubRulesObject := TokenRangeObject['SubRules'].ObjectValue;
          if Assigned(SubRulesObject) then
          for i := 0 to SubRulesObject.Count - 1 do
          begin
            if SubRulesObject.Names[i] = 'Range' then
            for j := 0 to SubRulesObject.Items[i].ArrayValue.Count - 1 do
            begin
              LJSONSubRulesObject := SubRulesObject.Items[i].ArrayValue.O[j];
              if LJSONSubRulesObject.S['Name'] = RangeObject['IncludeRange'].Value then
              begin
                ImportRange(ARange, LJSONSubRulesObject, nil, False, LElementPrefix);
                Break;
              end;
            end;
          end;
        end;
      finally
        LJSONObject.Free;
        LFileStream.Free;
      end;
    end
    else
    begin
      if not ASkipBeforeSubRules then
      begin
        ARange.Clear;
        ARange.CaseSensitive := RangeObject.B['CaseSensitive'];
        ImportAttributes(ARange.Attribute, RangeObject['Attributes'].ObjectValue, AElementPrefix);
        if RangeObject['Delimiters'].Value <> '' then
          ARange.Delimiters := StrToSet(RangeObject['Delimiters'].Value);
        ARange.TokenType := StrToRangeType(RangeObject['Type'].Value);

        PropertiesObject := RangeObject['Properties'].ObjectValue;
        if Assigned(PropertiesObject) then
        begin
          ARange.CloseOnEndOfLine := PropertiesObject.B['CloseOnEndOfLine'];
          ARange.CloseOnTerm := PropertiesObject.B['CloseOnTerm'];
          ARange.SkipWhitespace := PropertiesObject.B['SkipWhitespace'];
          ARange.CloseParent := PropertiesObject.B['CloseParent'];
          LAlternativeCloseArray := PropertiesObject['AlternativeClose'].ArrayValue;
          if LAlternativeCloseArray.Count > 0 then
          begin
            ARange.AlternativeCloseArrayCount := LAlternativeCloseArray.Count;
            for i := 0 to ARange.AlternativeCloseArrayCount - 1 do
              ARange.AlternativeCloseArray[i] := LAlternativeCloseArray.Items[i].Value;
          end;
          ARange.OpenBeginningOfLine := PropertiesObject.B['OpenBeginningOfLine'];
        end;

        ARange.OpenToken.Clear;
        ARange.OpenToken.BreakType := btAny;
        ARange.CloseToken.Clear;
        ARange.CloseToken.BreakType := btAny;

        TokenRangeObject := RangeObject['TokenRange'].ObjectValue;
        if Assigned(TokenRangeObject) then
          ARange.AddTokenRange(TokenRangeObject['Open'].Value, TokenRangeObject['Close'].Value);
      end;
      { Sub rules }
      SubRulesObject := RangeObject['SubRules'].ObjectValue;

      if Assigned(SubRulesObject) then
      begin
        for i := 0 to SubRulesObject.Count - 1 do
        begin
          if SubRulesObject.Names[i] = 'Range' then
          for j := 0 to SubRulesObject.Items[i].ArrayValue.Count - 1 do
          begin
            NewRange := TBCEditorRange.Create;
            ImportRange(NewRange, SubRulesObject.Items[i].ArrayValue.O[j], ARange); { ARange is for the MainRules include }
            ARange.AddRange(NewRange);
          end
          else
          if SubRulesObject.Names[i] = 'KeyList' then
          for j := 0 to SubRulesObject.Items[i].ArrayValue.Count - 1 do
          begin
            NewKeyList := TBCEditorKeyList.Create;
            ImportKeyList(NewKeyList, SubRulesObject.Items[i].ArrayValue.O[j], AElementPrefix);
            ARange.AddKeyList(NewKeyList);
          end
          else
          if SubRulesObject.Names[i] = 'Set' then
          for j := 0 to SubRulesObject.Items[i].ArrayValue.Count - 1 do
          begin
            NewSet := TBCEditorSet.Create;
            ImportSet(NewSet, SubRulesObject.Items[i].ArrayValue.O[j], AElementPrefix);
            ARange.AddSet(NewSet);
          end
        end;
      end;
    end;
  end;
end;

procedure TBCEditorHighlighterJSONImporter.ImportCompletionProposal(ACompletionProposalObject: TJsonObject);
var
  i: Integer;
  LSkipRegionItem: TBCEditorSkipRegionItem;
  LJsonDataValue: PJsonDataValue;
  LFileName: string;
  LEditor: TBCBaseEditor;
  LFileStream: TStream;
  LJSONObject: TJsonObject;
begin
  if not Assigned(ACompletionProposalObject) then
    Exit;
  { Skip regions }
  for i := 0 to ACompletionProposalObject['SkipRegion'].ArrayValue.Count - 1 do
  begin
    LJsonDataValue := ACompletionProposalObject['SkipRegion'].ArrayValue.Items[i];

    if FHighlighter.MultiHighlighter then
    begin
      { Multi highlighter code folding skip region include }
      LFileName := LJsonDataValue.ObjectValue['File'].Value;
      if LFileName <> '' then
      begin
        LEditor := FHighlighter.Editor as TBCBaseEditor;
        LFileStream := LEditor.CreateFileStream(LEditor.GetHighlighterFileName(LFileName));
        LJSONObject := TJsonObject.ParseFromStream(LFileStream) as TJsonObject;
        if Assigned(LJSONObject) then
        try
          if LJSONObject.Contains('CodeFolding') then
            ImportCompletionProposal(LJSONObject['CompletionProposal'].ObjectValue);
        finally
          LJSONObject.Free;
          LFileStream.Free;
        end;
      end;
      { Skip duplicates }
      if FHighlighter.CompletionProposalSkipRegions.Contains(LJsonDataValue.ObjectValue['OpenToken'].Value, LJsonDataValue.ObjectValue['CloseToken'].Value) then
        Continue;
    end;

    LSkipRegionItem := FHighlighter.CompletionProposalSkipRegions.Add(LJsonDataValue.ObjectValue['OpenToken'].Value,
      LJsonDataValue.ObjectValue['CloseToken'].Value);
    LSkipRegionItem.RegionType := StrToRegionType(LJsonDataValue.ObjectValue['RegionType'].Value);
    LSkipRegionItem.SkipEmptyChars := LJsonDataValue.ObjectValue.B['SkipEmptyChars'];
  end;
end;

procedure TBCEditorHighlighterJSONImporter.ImportCodeFoldingSkipRegions(ACodeFoldingRegions: TBCEditorCodeFoldingRegions;
  ACodeFoldingObject: TJsonObject);
var
  i: Integer;
  LJsonDataValue: PJsonDataValue;
  LSkipRegionType: TBCEditorSkipRegionItemType;
  LRegionItem: TBCEditorCodeFoldingRegionItem;
  LSkipRegionItem: TBCEditorSkipRegionItem;
  LFileName: string;
  LEditor: TBCBaseEditor;
  LFileStream: TStream;
  LJSONObject: TJsonObject;
  LOpenToken, LCloseToken: string;
begin
  if ACodeFoldingObject.Contains('SkipRegion') then
  for i := 0 to ACodeFoldingObject['SkipRegion'].ArrayValue.Count - 1 do
  begin
    LJsonDataValue := ACodeFoldingObject['SkipRegion'].ArrayValue.Items[i];
    LOpenToken := LJsonDataValue.ObjectValue['OpenToken'].Value;
    LCloseToken := LJsonDataValue.ObjectValue['CloseToken'].Value;

    if FHighlighter.MultiHighlighter then
    begin
      { Multi highlighter code folding skip region include }
      LFileName := LJsonDataValue.ObjectValue['File'].Value;
      if LFileName <> '' then
      begin
        LEditor := FHighlighter.Editor as TBCBaseEditor;
        LFileStream := LEditor.CreateFileStream(LEditor.GetHighlighterFileName(LFileName));
        LJSONObject := TJsonObject.ParseFromStream(LFileStream) as TJsonObject;
        if Assigned(LJSONObject) then
        try
          if LJSONObject.Contains('CodeFolding') then
            ImportCodeFoldingSkipRegions(ACodeFoldingRegions, LJSONObject['CodeFolding']['Ranges'].ArrayValue.Items[0].ObjectValue);
        finally
          LJSONObject.Free;
          LFileStream.Free;
        end;
      end;
      { Skip duplicates }
      if ACodeFoldingRegions.SkipRegions.Contains(LOpenToken, LCloseToken) then
        Continue;
    end;

    LSkipRegionType := StrToRegionType(LJsonDataValue.ObjectValue['RegionType'].Value);
    if (LSkipRegionType = ritMultiLineComment) and (cfoFoldMultilineComments in TBCBaseEditor(FHighlighter.Editor).CodeFolding.Options) then
    begin
      LRegionItem := ACodeFoldingRegions.Add(LOpenToken, LCloseToken);
      LRegionItem.NoSubs := True;
      FHighlighter.AddKeyChar(ctFoldOpen, LOpenToken[1]);
      if LCloseToken <> '' then
        FHighlighter.AddKeyChar(ctFoldClose, LCloseToken[1]);
    end
    else
    begin
      LSkipRegionItem := ACodeFoldingRegions.SkipRegions.Add(LOpenToken, LCloseToken);
      LSkipRegionItem.RegionType := LSkipRegionType;
      LSkipRegionItem.SkipEmptyChars := LJsonDataValue.ObjectValue.B['SkipEmptyChars'];
      LSkipRegionItem.SkipIfNextCharIsNot := BCEDITOR_NONE_CHAR;
      if LJsonDataValue.ObjectValue.Contains('NextCharIsNot') then
        LSkipRegionItem.SkipIfNextCharIsNot := LJsonDataValue.ObjectValue['NextCharIsNot'].Value[1];
      if LOpenToken <> '' then
        FHighlighter.AddKeyChar(ctSkipOpen, LOpenToken[1]);
      if LCloseToken <> '' then
        FHighlighter.AddKeyChar(ctSkipClose, LCloseToken[1]);
    end;
  end;
end;

procedure TBCEditorHighlighterJSONImporter.ImportCodeFoldingFoldRegions(ACodeFoldingRegions: TBCEditorCodeFoldingRegions;
  ACodeFoldingObject: TJsonObject);
var
  i, j: Integer;
  LJsonDataValue: PJsonDataValue;
  LRegionItem: TBCEditorCodeFoldingRegionItem;
  LMemberObject: TJsonObject;
  LFileName: string;
  LEditor: TBCBaseEditor;
  LFileStream: TStream;
  LJSONObject: TJsonObject;
  LOpenToken, LCloseToken: string;
  LSkipIfFoundAfterOpenTokenArray: TJsonArray;
begin
  if ACodeFoldingObject.Contains('FoldRegion') then
  for i := 0 to ACodeFoldingObject['FoldRegion'].ArrayValue.Count - 1 do
  begin
    LJsonDataValue := ACodeFoldingObject['FoldRegion'].ArrayValue.Items[i];
    LOpenToken := LJsonDataValue.ObjectValue['OpenToken'].Value;
    LCloseToken := LJsonDataValue.ObjectValue['CloseToken'].Value;

    if FHighlighter.MultiHighlighter then
    begin
      { Multi highlighter code folding fold region include }
      LFileName := LJsonDataValue.ObjectValue['File'].Value;
      if LFileName <> '' then
      begin
        LEditor := FHighlighter.Editor as TBCBaseEditor;
        LFileStream := LEditor.CreateFileStream(LEditor.GetHighlighterFileName(LFileName));
        LJSONObject := TJsonObject.ParseFromStream(LFileStream) as TJsonObject;
        if Assigned(LJSONObject) then
        try
          if LJSONObject.Contains('CodeFolding') then
            ImportCodeFoldingFoldRegions(ACodeFoldingRegions, LJSONObject['CodeFolding']['Ranges'].ArrayValue.Items[0].ObjectValue);
        finally
          LJSONObject.Free;
          LFileStream.Free;
        end;
      end;
      { Skip duplicates }
      if ACodeFoldingRegions.Contains(LOpenToken, LCloseToken) then
        Continue;
    end;

    LRegionItem := ACodeFoldingRegions.Add(LOpenToken, LCloseToken);

    LMemberObject := LJsonDataValue.ObjectValue['Properties'].ObjectValue;
    if Assigned(LMemberObject) then
    begin
      { Options }
      LRegionItem.OpenTokenBeginningOfLine := LMemberObject.B['OpenTokenBeginningOfLine'];
      LRegionItem.CloseTokenBeginningOfLine := LMemberObject.B['CloseTokenBeginningOfLine'];
      LRegionItem.SharedClose := LMemberObject.B['SharedClose'];
      LRegionItem.OpenIsClose := LMemberObject.B['OpenIsClose'];
      LRegionItem.TokenEndIsPreviousLine := LMemberObject.B['TokenEndIsPreviousLine'];
      LRegionItem.NoSubs := LMemberObject.B['NoSubs'];

      LSkipIfFoundAfterOpenTokenArray := LMemberObject['SkipIfFoundAfterOpenToken'].ArrayValue;
      if LSkipIfFoundAfterOpenTokenArray.Count > 0 then
      begin
        LRegionItem.SkipIfFoundAfterOpenTokenArrayCount := LSkipIfFoundAfterOpenTokenArray.Count;
        for j := 0 to LRegionItem.SkipIfFoundAfterOpenTokenArrayCount - 1 do
          LRegionItem.SkipIfFoundAfterOpenTokenArray[j] := LSkipIfFoundAfterOpenTokenArray.Items[j].Value;
      end;

      LRegionItem.BreakIfNotFoundBeforeNextRegion := LMemberObject['BreakIfNotFoundBeforeNextRegion'].Value;
      LRegionItem.OpenTokenEnd := LMemberObject['OpenTokenEnd'].Value;
    end;
    if LOpenToken <> '' then
      FHighlighter.AddKeyChar(ctFoldOpen, LOpenToken[1]);
    if LRegionItem.BreakIfNotFoundBeforeNextRegion <> '' then
      FHighlighter.AddKeyChar(ctFoldOpen, LRegionItem.BreakIfNotFoundBeforeNextRegion[1]);
    if LCloseToken <> '' then
      FHighlighter.AddKeyChar(ctFoldClose, LCloseToken[1]);
  end;
end;

procedure TBCEditorHighlighterJSONImporter.ImportCodeFoldingOptions(ACodeFoldingRegions: TBCEditorCodeFoldingRegions;
  ACodeFoldingObject: TJsonObject);
var
  LCodeFoldingObject: TJsonObject;
begin
  ACodeFoldingRegions.StringEscapeChar := BCEDITOR_NONE_CHAR;

  if ACodeFoldingObject.Contains('Options') then
  begin
    LCodeFoldingObject := ACodeFoldingObject['Options'].ObjectValue;

    if LCodeFoldingObject.Contains('OpenToken') then
      ACodeFoldingRegions.OpenToken := LCodeFoldingObject['OpenToken'].Value;

    if LCodeFoldingObject.Contains('CloseToken') then
      ACodeFoldingRegions.CloseToken := LCodeFoldingObject['CloseToken'].Value;

    if LCodeFoldingObject.Contains('StringEscapeChar') then
      ACodeFoldingRegions.StringEscapeChar := LCodeFoldingObject['StringEscapeChar'].Value[1];

    if LCodeFoldingObject.Contains('MatchingPairHighlight') then
      FHighlighter.MatchingPairHighlight := LCodeFoldingObject.B['MatchingPairHighlight'];
  end;
end;

procedure TBCEditorHighlighterJSONImporter.ImportCodeFolding(ACodeFoldingObject: TJsonObject);
var
  i, LCount: Integer;
  LCodeFoldingObject: TJsonObject;
  LArray: TJsonArray;
begin
  if not Assigned(ACodeFoldingObject) then
    Exit;
  LArray := ACodeFoldingObject['Ranges'].ArrayValue;
  LCount := LArray.Count;
  if LCount > 0 then
  begin
    FHighlighter.CodeFoldingRangeCount := LCount;
    for i := 0 to LCount - 1 do
    begin
      FHighlighter.CodeFoldingRanges[i] := TBCEditorCodeFoldingRegions.Create(TBCEditorCodeFoldingRegionItem);
      LCodeFoldingObject := LArray.Items[i].ObjectValue;

      ImportCodeFoldingOptions(FHighlighter.CodeFoldingRanges[i], LCodeFoldingObject);
      ImportCodeFoldingSkipRegions(FHighlighter.CodeFoldingRanges[i], LCodeFoldingObject);
      ImportCodeFoldingFoldRegions(FHighlighter.CodeFoldingRanges[i], LCodeFoldingObject);
    end;
  end;
end;

procedure TBCEditorHighlighterJSONImporter.ImportMatchingPair(AMatchingPairObject: TJsonObject);
var
  i, j: Integer;
  LTokenMatch: PBCEditorMatchingPairToken;
  LJsonDataValue: PJsonDataValue;
  LFileName: string;
  LEditor: TBCBaseEditor;
  LFileStream: TStream;
  LJSONObject: TJsonObject;
  LArray: TJsonArray;
begin
  if not Assigned(AMatchingPairObject) then
    Exit;
  { Matching token pairs }
  LArray := AMatchingPairObject['Pairs'].ArrayValue;
  for i := 0 to LArray.Count - 1 do
  begin
    LJsonDataValue := LArray.Items[i];

    if FHighlighter.MultiHighlighter then
    begin
      { Multi highlighter code folding fold region include }
      LFileName := LJsonDataValue.ObjectValue['File'].Value;
      if LFileName <> '' then
      begin
        LEditor := FHighlighter.Editor as TBCBaseEditor;
        LFileStream := LEditor.CreateFileStream(LEditor.GetHighlighterFileName(LFileName));
        LJSONObject := TJsonObject.ParseFromStream(LFileStream) as TJsonObject;
        if Assigned(LJSONObject) then
        try
          if LJSONObject.Contains('MatchingPair') then
            ImportMatchingPair(LJSONObject['MatchingPair'].ObjectValue);
        finally
          LJSONObject.Free;
          LFileStream.Free;
        end;
      end;
      { Skip duplicates }
      for j := 0 to FHighlighter.MatchingPairs.Count - 1 do
        if (PBCEditorMatchingPairToken(FHighlighter.MatchingPairs.Items[j])^.OpenToken = LJsonDataValue.ObjectValue['OpenToken'].Value) and
          (PBCEditorMatchingPairToken(FHighlighter.MatchingPairs.Items[j])^.CloseToken = LJsonDataValue.ObjectValue['CloseToken'].Value) then
          Continue;
    end;

    New(LTokenMatch);
    LTokenMatch.OpenToken := LJsonDataValue.ObjectValue['OpenToken'].Value;
    LTokenMatch.CloseToken := LJsonDataValue.ObjectValue['CloseToken'].Value;
    FHighlighter.MatchingPairs.Add(LTokenMatch)
  end;
end;

procedure TBCEditorHighlighterJSONImporter.ImportElements(AColorsObject: TJsonObject);
var
  i: Integer;
  LElement: PBCEditorHighlighterElement;
  LJsonDataValue: PJsonDataValue;
begin
  if not Assigned(AColorsObject) then
    Exit;
  for i := 0 to AColorsObject['Elements'].ArrayValue.Count - 1 do
  begin
    LJsonDataValue := AColorsObject['Elements'].ArrayValue.Items[i];
    New(LElement);
    LElement.Background := StringToColorDef(LJsonDataValue.ObjectValue['Background'].Value, clWindow);
    LElement.Foreground := StringToColorDef(LJsonDataValue.ObjectValue['Foreground'].Value, clWindowText);
    LElement.Name := LJsonDataValue.ObjectValue['Name'].Value;
    LElement.Style := StrToFontStyle(LJsonDataValue.ObjectValue['Style'].Value);
    FHighlighter.Colors.Styles.Add(LElement)
  end;
end;

procedure TBCEditorHighlighterJSONImporter.ImportHighlighter(AJSONObject: TJsonObject);
begin
  FHighlighter.Clear;

  ImportInfo(AJSONObject['Highlighter']['Info'].ObjectValue);
  ImportEditorProperties(AJSONObject['Highlighter']['Editor'].ObjectValue);
  ImportRange(FHighlighter.MainRules, AJSONObject['Highlighter']['MainRules'].ObjectValue);
  ImportCodeFolding(AJSONObject['CodeFolding'].ObjectValue);
  ImportMatchingPair(AJSONObject['MatchingPair'].ObjectValue);
  ImportCompletionProposal(AJSONObject['CompletionProposal'].ObjectValue);
end;

procedure TBCEditorHighlighterJSONImporter.ImportColors(AJSONObject: TJsonObject);
begin
  FHighlighter.Colors.Clear;

  ImportColorsInfo(AJSONObject['Colors']['Info'].ObjectValue);
  ImportColorsEditorProperties(AJSONObject['Colors']['Editor'].ObjectValue);
  ImportElements(AJSONObject['Colors'].ObjectValue);
end;

procedure TBCEditorHighlighterJSONImporter.ImportFromStream(AStream: TStream);
var
  JSONObject: TJsonObject;
begin
  JSONObject := TJsonObject.ParseFromStream(AStream) as TJsonObject;
  if Assigned(JSONObject) then
  try
    ImportHighlighter(JSONObject);
  finally
    JSONObject.Free;
  end;
end;

procedure TBCEditorHighlighterJSONImporter.ImportColorsFromStream(AStream: TStream);
var
  JSONObject: TJsonObject;
begin
  JSONObject := TJsonObject.ParseFromStream(AStream) as TJsonObject;
  if Assigned(JSONObject) then
  try
    ImportColors(JSONObject);
  finally
    JSONObject.Free;
  end;
end;

end.

unit BCEditor.Highlighter.JSONImporter;

interface

uses
  System.Classes, BCEditor.Highlighter, BCEditor.Highlighter.Colors;

procedure ImportFromFile(Highlighter: TBCEditorHighlighter; AFileName: string);
procedure ImportFromStream(Highlighter: TBCEditorHighlighter; AStream: TStream);
procedure ImportColorsFromFile(Highlighter: TBCEditorHighlighter; AFileName: string);
procedure ImportColorsFromStream(Highlighter: TBCEditorHighlighter; AStream: TStream);

implementation

uses
  JsonDataObjects, System.SysUtils, Vcl.Graphics, Vcl.Forms, BCEditor.Consts, BCEditor.Editor.Base, BCEditor.Utils,
  BCEditor.Highlighter.Attributes, BCEditor.Highlighter.Rules, BCEditor.Types, Vcl.Dialogs,
  BCEditor.Highlighter.Token, BCEditor.Editor.CodeFolding.FoldRegions, BCEditor.Language,
  BCEditor.Editor.SkipRegions, BCEditor.Highlighter.Info, BCEditor.Editor.CodeFolding.Types;

var
  GMultiHighlighter: Boolean;

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

function StrToBoolean(const AString: string): Boolean;
begin
  if AString = '' then
    Result := False
  else
    Result := StrToBool(AString);
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

function StrToBreakType(const AString: string): TBCEditorBreakType;
begin
  if (AString = 'Any') or (AString = '') then
    Result := btAny
  else
  if AString = 'Term' then
    Result := btTerm
  else
    Result := btUnspecified;
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
    Result :=  ttAddress
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_CHARACTER then
    Result :=  ttChar
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_COMMENT then
    Result :=  ttComment
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_DIRECTIVE then
    Result :=  ttDirective
   else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_FLOAT then
    Result :=  ttFloat
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_HEX then
    Result :=  ttHex
   else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_MAIL_TO_LINK then
    Result := ttMailtoLink
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_NUMBER then
    Result :=  ttNumber
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_OCTAL then
    Result :=  ttOctal
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_RESERVED_WORD then
    Result :=  ttReservedWord
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_STRING then
    Result :=  ttString
  else
  if AString = BCEDITOR_ATTRIBUTE_ELEMENT_SYMBOL then
    Result :=  ttSymbol
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

procedure ImportInfo(AInfo: TBCEditorHighlighterInfo; InfoObject: TJsonObject);
begin
  if Assigned(InfoObject) then
  begin
    { General }
    GMultiHighlighter := StrToBoolean(InfoObject['General']['MultiHighlighter'].Value);
    AInfo.General.Version := InfoObject['General']['Version'].Value;
    AInfo.General.Date := InfoObject['General']['Date'].Value;
    AInfo.General.Sample := InfoObject['General']['Sample'].Value;
    { Author }
    AInfo.Author.Name := InfoObject['Author']['Name'].Value;
    AInfo.Author.Email := InfoObject['Author']['Email'].Value;
    AInfo.Author.Comments := InfoObject['Author']['Comments'].Value;
  end;
end;

procedure ImportColorsInfo(AInfo: TBCEditorHighlighterInfo; InfoObject: TJsonObject);
begin
  if Assigned(InfoObject) then
  begin
    { General }
    AInfo.General.Version := InfoObject['General']['Version'].Value;
    AInfo.General.Date := InfoObject['General']['Date'].Value;
    { Author }
    AInfo.Author.Name := InfoObject['Author']['Name'].Value;
    AInfo.Author.Email := InfoObject['Author']['Email'].Value;
    AInfo.Author.Comments := InfoObject['Author']['Comments'].Value;
  end;
end;

procedure ImportEditorProperties(AEditor: TBCBaseEditor; EditorObject: TJsonObject);
begin
  if Assigned(EditorObject) then
    AEditor.URIOpener := StrToBoolDef(EditorObject['URIOpener'].Value, False);
end;

procedure ImportColorsEditorProperties(AEditor: TBCBaseEditor; EditorObject: TJsonObject);
var
  ColorsObject, FontsObject, FontSizesObject: TJsonObject;
begin
  if Assigned(EditorObject) then
  begin
    ColorsObject := EditorObject['Colors'].ObjectValue;
    if Assigned(ColorsObject) then
    begin
      AEditor.BackgroundColor := StringToColorDef(ColorsObject['Background'].Value, AEditor.BackgroundColor);
      AEditor.ActiveLine.Color := StringToColorDef(ColorsObject['ActiveLine'].Value, AEditor.ActiveLine.Color);
      AEditor.CodeFolding.Colors.Background := StringToColorDef(ColorsObject['CodeFoldingBackground'].Value, AEditor.CodeFolding.Colors.Background);
      AEditor.CodeFolding.Colors.CollapsedLine := StringToColorDef(ColorsObject['CodeFoldingCollapsedLine'].Value, AEditor.CodeFolding.Colors.CollapsedLine);
      AEditor.CodeFolding.Colors.FoldingLine := StringToColorDef(ColorsObject['CodeFoldingFoldingLine'].Value, AEditor.CodeFolding.Colors.FoldingLine);
      AEditor.CodeFolding.Colors.IndentHighlight := StringToColorDef(ColorsObject['CodeFoldingIndentHighlight'].Value, AEditor.CodeFolding.Colors.IndentHighlight);
      AEditor.CodeFolding.Hint.Colors.Background := StringToColorDef(ColorsObject['CodeFoldingHintBackground'].Value, AEditor.CodeFolding.Hint.Colors.Background);
      AEditor.CodeFolding.Hint.Colors.Border := StringToColorDef(ColorsObject['CodeFoldingHintBorder'].Value, AEditor.CodeFolding.Hint.Colors.Border);
      AEditor.CodeFolding.Hint.Font.Color := StringToColorDef(ColorsObject['CodeFoldingHintText'].Value, AEditor.CodeFolding.Hint.Font.Color);
      AEditor.CompletionProposal.Colors.Background := StringToColorDef(ColorsObject['CompletionProposalBackground'].Value, AEditor.CompletionProposal.Colors.Background);
      AEditor.CompletionProposal.Font.Color := StringToColorDef(ColorsObject['CompletionProposalForeground'].Value, AEditor.CompletionProposal.Font.Color);
      AEditor.CompletionProposal.Colors.Border := StringToColorDef(ColorsObject['CompletionProposalBorder'].Value, AEditor.CompletionProposal.Colors.Border);
      AEditor.CompletionProposal.Colors.SelectedBackground := StringToColorDef(ColorsObject['CompletionProposalSelectedBackground'].Value, AEditor.CompletionProposal.Colors.SelectedBackground);
      AEditor.CompletionProposal.Colors.SelectedText := StringToColorDef(ColorsObject['CompletionProposalSelectedText'].Value, AEditor.CompletionProposal.Colors.SelectedText);
      AEditor.LeftMargin.Color := StringToColorDef(ColorsObject['LeftMarginBackground'].Value, AEditor.LeftMargin.Color);
      AEditor.LeftMargin.Font.Color := StringToColorDef(ColorsObject['LeftMarginLineNumbers'].Value, AEditor.LeftMargin.Font.Color);
      AEditor.LeftMargin.Bookmarks.Panel.Color := StringToColorDef(ColorsObject['LeftMarginBookmarkPanel'].Value, AEditor.LeftMargin.Bookmarks.Panel.Color);
      AEditor.LeftMargin.LineState.Colors.Modified := StringToColorDef(ColorsObject['LeftMarginLineStateModified'].Value, AEditor.LeftMargin.LineState.Colors.Modified);
      AEditor.LeftMargin.LineState.Colors.Normal := StringToColorDef(ColorsObject['LeftMarginLineStateNormal'].Value, AEditor.LeftMargin.LineState.Colors.Normal);
      AEditor.MatchingPair.Colors.Matched := StringToColorDef(ColorsObject['MatchingPairMatched'].Value, AEditor.MatchingPair.Colors.Matched);
      AEditor.MatchingPair.Colors.Unmatched := StringToColorDef(ColorsObject['MatchingPairUnmatched'].Value, AEditor.MatchingPair.Colors.Unmatched);
      AEditor.RightMargin.Colors.Edge := StringToColorDef(ColorsObject['RightEdge'].Value, AEditor.RightMargin.Colors.Edge);
      AEditor.RightMargin.Colors.MovingEdge := StringToColorDef(ColorsObject['RightMovingEdge'].Value, AEditor.RightMargin.Colors.MovingEdge);
      AEditor.Search.Highlighter.Colors.Background := StringToColorDef(ColorsObject['SearchHighlighterBackground'].Value, AEditor.Search.Highlighter.Colors.Background);
      AEditor.Search.Highlighter.Colors.Foreground := StringToColorDef(ColorsObject['SearchHighlighterForeground'].Value, AEditor.Search.Highlighter.Colors.Foreground);
      AEditor.Search.Map.Colors.ActiveLine := StringToColorDef(ColorsObject['SearchMapActiveLine'].Value, AEditor.Search.Map.Colors.ActiveLine);
      AEditor.Search.Map.Colors.Background := StringToColorDef(ColorsObject['SearchMapBackground'].Value, AEditor.Search.Map.Colors.Background);
      AEditor.Search.Map.Colors.Foreground := StringToColorDef(ColorsObject['SearchMapForeground'].Value, AEditor.Search.Map.Colors.Foreground);
      AEditor.Selection.Colors.Background := StringToColorDef(ColorsObject['SelectionBackground'].Value, AEditor.Selection.Colors.Background);
      AEditor.Selection.Colors.Foreground := StringToColorDef(ColorsObject['SelectionForeground'].Value, AEditor.Selection.Colors.Foreground);
    end;
    FontsObject := EditorObject['Fonts'].ObjectValue;
    if Assigned(FontsObject) then
    begin
      AEditor.LeftMargin.Font.Name := StrToStrDef(FontsObject['LineNumbers'].Value, AEditor.LeftMargin.Font.Name);
      AEditor.Font.Name := StrToStrDef(FontsObject['Text'].Value, AEditor.Font.Name);
      AEditor.Minimap.Font.Name := StrToStrDef(FontsObject['Minimap'].Value, AEditor.Minimap.Font.Name);
      AEditor.CodeFolding.Hint.Font.Name := StrToStrDef(FontsObject['CodeFoldingHint'].Value, AEditor.CodeFolding.Hint.Font.Name);
      AEditor.CompletionProposal.Font.Name := StrToStrDef(FontsObject['CompletionProposal'].Value, AEditor.CompletionProposal.Font.Name);
    end;
    FontSizesObject := EditorObject['FontSizes'].ObjectValue;
    if Assigned(FontSizesObject) then
    begin
      AEditor.LeftMargin.Font.Size := StrToIntDef(FontSizesObject['LineNumbers'].Value, AEditor.LeftMargin.Font.Size);
      AEditor.Font.Size := StrToIntDef(FontSizesObject['Text'].Value, AEditor.Font.Size);
      AEditor.Minimap.Font.Size := StrToIntDef(FontSizesObject['Minimap'].Value, AEditor.Minimap.Font.Size);
      AEditor.CodeFolding.Hint.Font.Size := StrToIntDef(FontSizesObject['CodeFoldingHint'].Value, AEditor.CodeFolding.Hint.Font.Size);
      AEditor.CompletionProposal.Font.Size := StrToIntDef(FontSizesObject['CompletionProposal'].Value, AEditor.CompletionProposal.Font.Size);
    end;
  end;
end;

procedure ImportAttributes(AHighlighterAttribute: TBCEditorHighlighterAttribute; AttributesObject: TJsonObject);
begin
  if Assigned(AttributesObject) then
  begin
    AHighlighterAttribute.Element := AttributesObject['Element'].Value;
    AHighlighterAttribute.ParentForeground := StrToBoolean(AttributesObject['ParentForeground'].Value);
    AHighlighterAttribute.ParentBackground := StrToBoolean(AttributesObject['ParentBackground'].Value);
    AHighlighterAttribute.UseParentElementForTokens := StrToBoolean(AttributesObject['UseParentElementForTokens'].Value);
  end;
end;

procedure ImportKeyList(AKeyList: TBCEditorKeyList; KeyListObject: TJsonObject);
begin
  if Assigned(KeyListObject) then
  begin
    AKeyList.TokenType := StrToRangeType(KeyListObject['Type'].Value);
    AKeyList.KeyList.Text := KeyListObject['Words'].Value;
    ImportAttributes(AKeyList.Attribute, KeyListObject['Attributes'].ObjectValue);
  end;
end;

procedure ImportSet(ASet: TBCEditorSet; SetObject: TJsonObject);
begin
  if Assigned(SetObject) then
  begin
    ASet.CharSet := StrToSet(SetObject['Symbols'].Value);
    ImportAttributes(ASet.Attribute, SetObject['Attributes'].ObjectValue);
  end;
end;

procedure ImportRange(ARange: TBCEditorRange; RangeObject: TJsonObject; AParentRange: TBCEditorRange = nil; ASkipBeforeSubRules: Boolean = False); { Recursive method }
var
  i, j: Integer;
  LFileName: string;
  NewRange: TBCEditorRange;
  NewKeyList: TBCEditorKeyList;
  NewSet: TBCEditorSet;
  SubRulesObject, PropertiesObject, TokenRangeObject: TJsonObject;
  JSONObject: TJsonObject;
begin
  if Assigned(RangeObject) then
  begin
    LFileName := RangeObject['File'].Value;
    if GMultiHighlighter and (LFileName <> '') then
    begin
      JSONObject := TJsonObject.ParseFromFile(Format('%sHighlighters\%s', [ExtractFilePath(Application.ExeName), LFileName])) as TJsonObject;
      if Assigned(JSONObject) then
      try
        TokenRangeObject := JSONObject['Highlighter']['MainRules'].ObjectValue;
        { You can include MainRules... }
        if TokenRangeObject['Name'].Value = RangeObject['IncludeRange'].Value then
          ImportRange(AParentRange, TokenRangeObject, nil, True)
        else
        { or SubRules... }
        begin
          SubRulesObject := TokenRangeObject['SubRules'].ObjectValue;
          if Assigned(SubRulesObject) then
          for i := 0 to SubRulesObject.Count - 1 do
          begin
            if SubRulesObject.Names[i] = 'Range' then
            for j := 0 to SubRulesObject.Items[i].ArrayValue.Count - 1 do
              if SubRulesObject.Items[i].ArrayValue.O[j].S['Name'] = RangeObject['IncludeRange'].Value then
              begin
                ImportRange(ARange, SubRulesObject.Items[i].ArrayValue.O[j]);
                Break;
              end;
          end;
        end;
      finally
        JSONObject.Free;
      end;
    end
    else
    begin
      if not ASkipBeforeSubRules then
      begin
        ARange.Clear;
        ARange.CaseSensitive := StrToBoolean(RangeObject['CaseSensitive'].Value);
        ImportAttributes(ARange.Attribute, RangeObject['Attributes'].ObjectValue);
        if RangeObject['Delimiters'].Value <> '' then
          ARange.Delimiters := StrToSet(RangeObject['Delimiters'].Value);
        ARange.TokenType := StrToRangeType(RangeObject['Type'].Value);

        PropertiesObject := RangeObject['Properties'].ObjectValue;
        if Assigned(PropertiesObject) then
        begin
          ARange.CloseOnEol := StrToBoolean(PropertiesObject['CloseOnEol'].Value);
          ARange.CloseOnTerm := StrToBoolean(PropertiesObject['CloseOnTerm'].Value);
          ARange.AlternativeClose := PropertiesObject['AlternativeClose'].Value;
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
            ImportKeyList(NewKeyList, SubRulesObject.Items[i].ArrayValue.O[j]);
            ARange.AddKeyList(NewKeyList);
          end
          else
          if SubRulesObject.Names[i] = 'Set' then
          for j := 0 to SubRulesObject.Items[i].ArrayValue.Count - 1 do
          begin
            NewSet := TBCEditorSet.Create;
            ImportSet(NewSet, SubRulesObject.Items[i].ArrayValue.O[j]);
            ARange.AddSet(NewSet);
          end
        end;
      end;
    end;
  end;
end;

procedure ImportCompletionProposal(ASkipRegions: TBCEditorSkipRegions; CodeFoldingObject: TJsonObject);
var
  i: Integer;
  SkipRegionItem: TBCEditorSkipRegionItem;
begin
  if not Assigned(CodeFoldingObject) then
    Exit;
  { Skip regions }
  for i := 0 to CodeFoldingObject['SkipRegion'].ArrayValue.Count - 1 do
  begin
    SkipRegionItem := ASkipRegions.Add(CodeFoldingObject['SkipRegion'].ArrayValue.Items[i].ObjectValue['OpenToken'].Value,
      CodeFoldingObject['SkipRegion'].ArrayValue.Items[i].ObjectValue['CloseToken'].Value);
    SkipRegionItem.RegionType := StrToRegionType(CodeFoldingObject['SkipRegion'].ArrayValue.Items[i].ObjectValue['RegionType'].Value);
    SkipRegionItem.SkipEmptyChars := StrToBoolean(CodeFoldingObject['SkipRegion'].ArrayValue.Items[i].ObjectValue['SkipEmptyChars'].Value);
  end;
end;

procedure ImportCodeFolding(AEditor: TBCBaseEditor; AFoldRegions: TBCEditorCodeFoldingRegions; CodeFoldingObject: TJsonObject);
var
  i: Integer;
  SkipRegionType: TBCEditorSkipRegionItemType;
  MemberObject: TJsonObject;
  FoldRegionItem: TBCEditorFoldRegionItem;
  SkipRegionItem: TBCEditorSkipRegionItem;
begin
  if not Assigned(CodeFoldingObject) then
    Exit;
  if CodeFoldingObject.Contains('Options') then
    AFoldRegions.StringEscapeChar := CodeFoldingObject['Options'].ObjectValue['StringEscapeChar'].Value[1]
  else
    AFoldRegions.StringEscapeChar := #0;
  { Skip regions }
  if CodeFoldingObject.Contains('SkipRegion') then
  for i := 0 to CodeFoldingObject['SkipRegion'].ArrayValue.Count - 1 do
  begin
    SkipRegionType := StrToRegionType(CodeFoldingObject['SkipRegion'].ArrayValue.Items[i].ObjectValue['RegionType'].Value);
    if (SkipRegionType = ritMultiLineComment) and (cfoFoldMultilineComments in AEditor.CodeFolding.Options) then
    begin
      FoldRegionItem := AFoldRegions.Add(CodeFoldingObject['SkipRegion'].ArrayValue.Items[i].ObjectValue['OpenToken'].Value,
        CodeFoldingObject['SkipRegion'].ArrayValue.Items[i].ObjectValue['CloseToken'].Value);
      FoldRegionItem.NoSubs := True;
    end
    else
    begin
      SkipRegionItem := AFoldRegions.SkipRegions.Add(CodeFoldingObject['SkipRegion'].ArrayValue.Items[i].ObjectValue['OpenToken'].Value,
        CodeFoldingObject['SkipRegion'].ArrayValue.Items[i].ObjectValue['CloseToken'].Value);
      SkipRegionItem.RegionType := SkipRegionType;
      SkipRegionItem.SkipEmptyChars := StrToBoolean(CodeFoldingObject['SkipRegion'].ArrayValue.Items[i].ObjectValue['SkipEmptyChars'].Value);
    end;
  end;
  { Fold regions }
  if CodeFoldingObject.Contains('FoldRegion') then
  for i := 0 to CodeFoldingObject['FoldRegion'].ArrayValue.Count - 1 do
  begin
    FoldRegionItem := AFoldRegions.Add(CodeFoldingObject['FoldRegion'].ArrayValue.Items[i].ObjectValue['OpenToken'].Value,
      CodeFoldingObject['FoldRegion'].ArrayValue.Items[i].ObjectValue['CloseToken'].Value);

    MemberObject := CodeFoldingObject['FoldRegion'].ArrayValue.Items[i].ObjectValue['Properties'].ObjectValue;
    if Assigned(MemberObject) then
    begin
      { Options }
      FoldRegionItem.BeginningOfLine := StrToBoolean(MemberObject['BeginningOfLine'].Value);
      FoldRegionItem.SharedClose := StrToBoolean(MemberObject['SharedClose'].Value);
      FoldRegionItem.OpenIsClose := StrToBoolean(MemberObject['OpenIsClose'].Value);
      FoldRegionItem.NoSubs := StrToBoolean(MemberObject['NoSubs'].Value);
      FoldRegionItem.SkipIfFoundAfterOpenToken := MemberObject['SkipIfFoundAfterOpenToken'].Value;
      FoldRegionItem.BreakIfNotFoundBeforeNextRegion := MemberObject['BreakIfNotFoundBeforeNextRegion'].Value;
    end;
  end;
end;

procedure ImportMatchingPair(AMatchingPairs: TList; MatchingPairObject: TJsonObject);
var
  i: Integer;
  TokenMatch: PBCEditorMatchingPairToken;
begin
  if not Assigned(MatchingPairObject) then
    Exit;
  { Matching token pairs }
  for i := 0 to MatchingPairObject['Pairs'].ArrayValue.Count - 1 do
  begin
    { Add element }
    New(TokenMatch);
    TokenMatch.OpenToken := MatchingPairObject['Pairs'].ArrayValue.Items[i].ObjectValue['OpenToken'].Value;
    TokenMatch.CloseToken := MatchingPairObject['Pairs'].ArrayValue.Items[i].ObjectValue['CloseToken'].Value;
    AMatchingPairs.Add(TokenMatch)
  end;
end;

procedure ImportElements(AStyles: TList; ColorsObject: TJsonObject);
var
  i: Integer;
  Element: PBCEditorHighlighterElement;
begin
  if not Assigned(ColorsObject) then
    Exit;
  for i := 0 to ColorsObject['Elements'].ArrayValue.Count - 1 do
  begin
    New(Element);
    Element.Background := StringToColorDef(ColorsObject['Elements'].ArrayValue.Items[i].ObjectValue['Background'].Value, clWindow);
    Element.Foreground := StringToColorDef(ColorsObject['Elements'].ArrayValue.Items[i].ObjectValue['Foreground'].Value, clWindowText);
    Element.Name := ColorsObject['Elements'].ArrayValue.Items[i].ObjectValue['Name'].Value;
    Element.Style := StrToFontStyle(ColorsObject['Elements'].ArrayValue.Items[i].ObjectValue['Style'].Value);
    AStyles.Add(Element)
  end;
end;

procedure ImportHighlighter(Highlighter: TBCEditorHighlighter; JSONObject: TJsonObject);
var
  Editor: TBCBaseEditor;
begin
  Editor := Highlighter.Editor as TBCBaseEditor;

  Highlighter.Clear;

  ImportInfo(Highlighter.Info, JSONObject['Highlighter']['Info'].ObjectValue);
  ImportEditorProperties(Editor, JSONObject['Highlighter']['Editor'].ObjectValue);
  ImportRange(Highlighter.MainRules, JSONObject['Highlighter']['MainRules'].ObjectValue);
  ImportCodeFolding(Editor, Highlighter.CodeFoldingRegions, JSONObject['CodeFolding'].ObjectValue);
  ImportMatchingPair(Highlighter.MatchingPairs, JSONObject['MatchingPair'].ObjectValue);
  ImportCompletionProposal(Highlighter.CompletionProposalSkipRegions, JSONObject['CompletionProposal'].ObjectValue);
end;

procedure ImportColors(Highlighter: TBCEditorHighlighter; JSONObject: TJsonObject);
begin
  Highlighter.Colors.Clear;

  ImportColorsInfo(Highlighter.Colors.Info, JSONObject['Colors']['Info'].ObjectValue);
  ImportColorsEditorProperties(Highlighter.Editor as TBCBaseEditor, JSONObject['Colors']['Editor'].ObjectValue);
  ImportElements(Highlighter.Colors.Styles, JSONObject['Colors'].ObjectValue);
end;

procedure ImportFromStream(Highlighter: TBCEditorHighlighter; AStream: TStream);
var
  JSONObject: TJsonObject;
begin
  JSONObject := TJsonObject.ParseFromStream(AStream) as TJsonObject;
  if Assigned(JSONObject) then
  try
    ImportHighlighter(Highlighter, JSONObject);
  finally
    JSONObject.Free;
  end;
end;

procedure ImportFromFile(Highlighter: TBCEditorHighlighter; AFileName: string);
var
  JSONObject: TJsonObject;
begin
  if FileExists(AFileName) then
  begin
    JSONObject := TJsonObject.ParseFromFile(AFileName) as TJsonObject;
    try
      if Assigned(JSONObject) then
        ImportHighlighter(Highlighter, JSONObject);
    finally
      JSONObject.Free
    end;
  end
  else
    MessageDialog(Format(SBCEditorImporterFileNotFound, [AFileName]), mtError, [mbOK]);
end;

procedure ImportColorsFromStream(Highlighter: TBCEditorHighlighter; AStream: TStream);
var
  JSONObject: TJsonObject;
begin
  JSONObject := TJsonObject.ParseFromStream(AStream) as TJsonObject;
  if Assigned(JSONObject) then
  try
    ImportColors(Highlighter, JSONObject);
  finally
    JSONObject.Free;
  end;
end;

procedure ImportColorsFromFile(Highlighter: TBCEditorHighlighter; AFileName: string);
var
  JSONObject: TJsonObject;
begin
  if FileExists(AFileName) then
  begin
    JSONObject := TJsonObject.ParseFromFile(AFileName) as TJsonObject;
    if Assigned(JSONObject) then
    try
      ImportColors(Highlighter, JSONObject);
    finally
      JSONObject.Free;
    end;
  end
  else
    MessageDialog(Format(SBCEditorImporterFileNotFound, [AFileName]), mtError, [mbOK]);
end;

end.

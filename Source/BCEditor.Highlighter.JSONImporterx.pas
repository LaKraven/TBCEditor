unit BCEditor.Highlighter.JSONImporterx;

interface

uses
  System.Classes, BCEditor.Highlighter, BCEditor.Highlighter.Colors;

procedure ImportFromFile(Highlighter: TBCEditorHighlighter; AFileName: string);
procedure ImportFromStream(Highlighter: TBCEditorHighlighter; AStream: TStream);
procedure ImportColorsFromFile(Highlighter: TBCEditorHighlighter; AFileName: string);
procedure ImportColorsFromStream(Highlighter: TBCEditorHighlighter; AStream: TStream);

implementation

uses
  XSuperObject, System.SysUtils, Vcl.Graphics, Vcl.Forms, BCEditor.Consts, BCEditor.Editor.Base,
  BCEditor.Editor.CodeFolding.Types, BCEditor.Utils, BCEditor.Highlighter.Attributes, BCEditor.Highlighter.Rules,
  BCEditor.Types, Vcl.Dialogs, BCEditor.Highlighter.Token, BCEditor.Editor.CodeFolding.FoldRegions, BCEditor.Language,
  BCEditor.Editor.SkipRegions, BCEditor.Highlighter.Info;

var
  GMultiHighlighter: Boolean;

function StringToColorDef(const AString: string; const DefaultColor: TColor): TColor;
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

{function StrToStartLine(const AString: string): TBCEditorStartLine;
begin
  Result := slNotFirst;
  if (AString = 'True') or (AString = 'First') then
    Result := slFirst
  else
  if AString = 'NonSpace' then
    Result := slFirstNonSpace
  else
  if (AString = 'False') or (AString = '') then
    Result := slNotFirst
end;  }

{function StrToStartType(const AString: string): TBCEditorStartType;
begin
  if (AString = 'Any') or (AString = '') then
    Result := stAny
  else
  if AString = 'Term' then
    Result := stTerm
  else
    Result := stUnspecified
end;    }

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

procedure ImportInfo(AInfo: TBCEditorHighlighterInfo; InfoObject: ISuperObject);
begin
  if Assigned(InfoObject) then
  begin
    { General }
    GMultiHighlighter := StrToBoolean(InfoObject['General."MultiHighlighter"'].AsString);
    AInfo.General.Version := InfoObject['General."Version"'].AsString;
    AInfo.General.Date := InfoObject['General."Date"'].AsString;
    AInfo.General.Sample := InfoObject['General."Sample"'].AsString;
    { Author }
    AInfo.Author.Name := InfoObject['Author."Name"'].AsString;
    AInfo.Author.Email := InfoObject['Author."Email"'].AsString;
    AInfo.Author.Comments := InfoObject['Author."Comments"'].AsString;
  end;
end;

procedure ImportColorsInfo(AInfo: TBCEditorHighlighterInfo; InfoObject: ISuperObject);
begin
  if Assigned(InfoObject) then
  begin
    { General }
    AInfo.General.Version := InfoObject['General."Version"'].AsString;
    AInfo.General.Date := InfoObject['General."Date"'].AsString;
    { Author }
    AInfo.Author.Name := InfoObject['Author."Name"'].AsString;
    AInfo.Author.Email := InfoObject['Author."Email"'].AsString;
    AInfo.Author.Comments := InfoObject['Author."Comments"'].AsString;
  end;
end;

procedure ImportEditorProperties(AEditor: TBCBaseEditor; EditorObject: ISuperObject);
begin
  if Assigned(EditorObject) then
    AEditor.URIOpener := StrToBoolDef(EditorObject['URIOpener'].AsString, False);
end;

procedure ImportColorsEditorProperties(AEditor: TBCBaseEditor; EditorObject: ISuperObject);
var
  ColorsObject, FontsObject, FontSizesObject: ISuperObject;
begin
  if Assigned(EditorObject) then
  begin
    ColorsObject := EditorObject['Colors'].AsObject;
    if Assigned(ColorsObject) then
    begin
      AEditor.BackgroundColor := StringToColorDef(ColorsObject['Background'].AsString, AEditor.BackgroundColor);
      AEditor.ActiveLine.Color := StringToColorDef(ColorsObject['ActiveLine'].AsString, AEditor.ActiveLine.Color);
      AEditor.CodeFolding.Colors.Background := StringToColorDef(ColorsObject['CodeFoldingBackground'].AsString, AEditor.CodeFolding.Colors.Background);
      AEditor.CodeFolding.Colors.CollapsedLine := StringToColorDef(ColorsObject['CodeFoldingCollapsedLine'].AsString, AEditor.CodeFolding.Colors.CollapsedLine);
      AEditor.CodeFolding.Colors.FoldingLine := StringToColorDef(ColorsObject['CodeFoldingFoldingLine'].AsString, AEditor.CodeFolding.Colors.FoldingLine);
      AEditor.CodeFolding.Colors.IndentHighlight := StringToColorDef(ColorsObject['CodeFoldingIndentHighlight'].AsString, AEditor.CodeFolding.Colors.IndentHighlight);
      AEditor.CodeFolding.Hint.Colors.Background := StringToColorDef(ColorsObject['CodeFoldingHintBackground'].AsString, AEditor.CodeFolding.Hint.Colors.Background);
      AEditor.CodeFolding.Hint.Colors.Border := StringToColorDef(ColorsObject['CodeFoldingHintBorder'].AsString, AEditor.CodeFolding.Hint.Colors.Border);
      AEditor.CodeFolding.Hint.Font.Color := StringToColorDef(ColorsObject['CodeFoldingHintText'].AsString, AEditor.CodeFolding.Hint.Font.Color);
      AEditor.CompletionProposal.Colors.Background := StringToColorDef(ColorsObject['CompletionProposalBackground'].AsString, AEditor.CompletionProposal.Colors.Background);
      AEditor.CompletionProposal.Font.Color := StringToColorDef(ColorsObject['CompletionProposalForeground'].AsString, AEditor.CompletionProposal.Font.Color);
      AEditor.CompletionProposal.Colors.Border := StringToColorDef(ColorsObject['CompletionProposalBorder'].AsString, AEditor.CompletionProposal.Colors.Border);
      AEditor.CompletionProposal.Colors.SelectedBackground := StringToColorDef(ColorsObject['CompletionProposalSelectedBackground'].AsString, AEditor.CompletionProposal.Colors.SelectedBackground);
      AEditor.CompletionProposal.Colors.SelectedText := StringToColorDef(ColorsObject['CompletionProposalSelectedText'].AsString, AEditor.CompletionProposal.Colors.SelectedText);
      AEditor.LeftMargin.Color := StringToColorDef(ColorsObject['LeftMarginBackground'].AsString, AEditor.LeftMargin.Color);
      AEditor.LeftMargin.Font.Color := StringToColorDef(ColorsObject['LeftMarginLineNumbers'].AsString, AEditor.LeftMargin.Font.Color);
      AEditor.LeftMargin.Bookmarks.Panel.Color := StringToColorDef(ColorsObject['LeftMarginBookmarkPanel'].AsString, AEditor.LeftMargin.Bookmarks.Panel.Color);
      AEditor.LeftMargin.LineState.Colors.Modified := StringToColorDef(ColorsObject['LeftMarginLineStateModified'].AsString, AEditor.LeftMargin.LineState.Colors.Modified);
      AEditor.LeftMargin.LineState.Colors.Normal := StringToColorDef(ColorsObject['LeftMarginLineStateNormal'].AsString, AEditor.LeftMargin.LineState.Colors.Normal);
      AEditor.MatchingPair.Colors.Matched := StringToColorDef(ColorsObject['MatchingPairMatched'].AsString, AEditor.MatchingPair.Colors.Matched);
      AEditor.MatchingPair.Colors.Unmatched := StringToColorDef(ColorsObject['MatchingPairUnmatched'].AsString, AEditor.MatchingPair.Colors.Unmatched);
      AEditor.RightMargin.Colors.Edge := StringToColorDef(ColorsObject['RightMargin'].AsString, AEditor.RightMargin.Colors.Edge);
      AEditor.RightMargin.Colors.MovingEdge := StringToColorDef(ColorsObject['RightMovingEdge'].AsString, AEditor.RightMargin.Colors.MovingEdge);
      AEditor.Search.Highlighter.Colors.Background := StringToColorDef(ColorsObject['SearchHighlighterBackground'].AsString, AEditor.Search.Highlighter.Colors.Background);
      AEditor.Search.Highlighter.Colors.Foreground := StringToColorDef(ColorsObject['SearchHighlighterForeground'].AsString, AEditor.Search.Highlighter.Colors.Foreground);
      AEditor.Search.Map.Colors.ActiveLine := StringToColorDef(ColorsObject['SearchMapActiveLine'].AsString, AEditor.Search.Map.Colors.ActiveLine);
      AEditor.Search.Map.Colors.Background := StringToColorDef(ColorsObject['SearchMapBackground'].AsString, AEditor.Search.Map.Colors.Background);
      AEditor.Search.Map.Colors.Foreground := StringToColorDef(ColorsObject['SearchMapForeground'].AsString, AEditor.Search.Map.Colors.Foreground);
      AEditor.Selection.Colors.Background := StringToColorDef(ColorsObject['SelectionBackground'].AsString, AEditor.Selection.Colors.Background);
      AEditor.Selection.Colors.Foreground := StringToColorDef(ColorsObject['SelectionForeground'].AsString, AEditor.Selection.Colors.Foreground);
    end;
    FontsObject := EditorObject['Fonts'].AsObject;
    if Assigned(FontsObject) then
    begin
      AEditor.LeftMargin.Font.Name := StrToStrDef(FontsObject['LineNumbers'].AsString, AEditor.LeftMargin.Font.Name);
      AEditor.Font.Name := StrToStrDef(FontsObject['Text'].AsString, AEditor.Font.Name);
      AEditor.Minimap.Font.Name := StrToStrDef(FontsObject['Minimap'].AsString, AEditor.Minimap.Font.Name);
      AEditor.CodeFolding.Hint.Font.Name := StrToStrDef(FontsObject['CodeFoldingHint'].AsString, AEditor.CodeFolding.Hint.Font.Name);
      AEditor.CompletionProposal.Font.Name := StrToStrDef(FontsObject['CompletionProposal'].AsString, AEditor.CompletionProposal.Font.Name);
    end;
    FontSizesObject := EditorObject['FontSizes'].AsObject;
    if Assigned(FontSizesObject) then
    begin
      AEditor.LeftMargin.Font.Size := StrToIntDef(FontSizesObject['LineNumbers'].AsString, AEditor.LeftMargin.Font.Size);
      AEditor.Font.Size := StrToIntDef(FontSizesObject['Text'].AsString, AEditor.Font.Size);
      AEditor.Minimap.Font.Size := StrToIntDef(FontSizesObject['Minimap'].AsString, AEditor.Minimap.Font.Size);
      AEditor.CodeFolding.Hint.Font.Size := StrToIntDef(FontSizesObject['CodeFoldingHint'].AsString, AEditor.CodeFolding.Hint.Font.Size);
      AEditor.CompletionProposal.Font.Size := StrToIntDef(FontSizesObject['CompletionProposal'].AsString, AEditor.CompletionProposal.Font.Size);
    end;
  end;
end;

procedure ImportAttributes(AHighlighterAttribute: TBCEditorHighlighterAttribute; AttributesObject: ISuperObject);
begin
  if Assigned(AttributesObject) then
  begin
    AHighlighterAttribute.Element := AttributesObject['Element'].AsString;
    AHighlighterAttribute.ParentForeground := StrToBoolean(AttributesObject['ParentForeground'].AsString);
    AHighlighterAttribute.ParentBackground := StrToBoolean(AttributesObject['ParentBackground'].AsString);
    AHighlighterAttribute.UseParentElementForTokens := StrToBoolean(AttributesObject['UseParentElementForTokens'].AsString);
  end;
end;

procedure ImportKeyList(AKeyList: TBCEditorKeyList; KeyListObject: ISuperObject);
begin
  if Assigned(KeyListObject) then
  begin
    AKeyList.TokenType := StrToRangeType(KeyListObject['Type'].AsString);
    AKeyList.KeyList.Text := KeyListObject['Words'].AsString;
    ImportAttributes(AKeyList.Attribute, KeyListObject['Attributes'].AsObject);
  end;
end;

procedure ImportSet(ASet: TBCEditorSet; SetObject: ISuperObject);
begin
  if Assigned(SetObject) then
  begin
    ASet.CharSet := StrToSet(SetObject['Symbols'].AsString);
    ImportAttributes(ASet.Attribute, SetObject['Attributes'].AsObject);
  end;
end;

procedure ImportRange(ARange: TBCEditorRange; RangeObject: ISuperObject; AParentRange: TBCEditorRange = nil; ASkipBeforeSubRules: Boolean = False); { Recursive method }
var
  NewRange: TBCEditorRange;
  NewKeyList: TBCEditorKeyList;
  NewSet: TBCEditorSet;
  SubRulesObject, PropertiesObject, TokenRangeObject: ISuperObject;
  Member: IMember;
  JSONObject: ISuperObject;
begin
  if Assigned(RangeObject) then
  begin
    if GMultiHighlighter and (RangeObject['File'].AsString <> '') then
    begin
      JSONObject := TSuperObject.ParseFile(Format('%sHighlighters\%s', [ExtractFilePath(Application.ExeName),
        RangeObject['File'].AsString]));
      if Assigned(JSONObject) then
      begin
        TokenRangeObject := JSONObject['Highlighter."MainRules"'].AsObject;
        { include MainRules... }
        if TokenRangeObject['Name'].AsString = RangeObject['IncludeRange'].AsString then
          ImportRange(AParentRange, TokenRangeObject, nil, True)
        else
        { or SubRules... }
        begin
          SubRulesObject := TokenRangeObject['SubRules'].AsObject;
          if Assigned(SubRulesObject) then
          while not SubRulesObject.Eof do
          begin
            if SubRulesObject.CurrentKey = 'Range' then
              for Member in SubRulesObject['Range'].AsArray do
                if Member.AsObject['Name'].AsString = RangeObject['IncludeRange'].AsString then
                begin
                  ImportRange(ARange, Member.AsObject);
                  Break;
                end;
            SubRulesObject.Next;
          end;
        end;
      end;
    end
    else
    begin
      if not ASkipBeforeSubRules then
      begin
        ARange.Clear;
        ARange.CaseSensitive := StrToBoolean(RangeObject['CaseSensitive'].AsString);
        ImportAttributes(ARange.Attribute, RangeObject['Attributes'].AsObject);
        if RangeObject['Delimiters'].AsString <> '' then
          ARange.Delimiters := StrToSet(RangeObject['Delimiters'].AsString);
        ARange.TokenType := StrToRangeType(RangeObject['Type'].AsString);

        PropertiesObject := RangeObject['Properties'].AsObject;
        if Assigned(PropertiesObject) then
        begin
          ARange.CloseOnEol := StrToBoolean(PropertiesObject['CloseOnEol'].AsString);
          ARange.CloseOnTerm := StrToBoolean(PropertiesObject['CloseOnTerm'].AsString);
          ARange.AlternativeClose := PropertiesObject['AlternativeClose'].AsString;
        end;

        ARange.OpenToken.Clear;
        ARange.OpenToken.BreakType := btAny;
        ARange.CloseToken.Clear;
        ARange.CloseToken.BreakType := btAny;

        TokenRangeObject := RangeObject['TokenRange'].AsObject;
        if Assigned(TokenRangeObject) then
          ARange.AddTokenRange(TokenRangeObject['Open'].AsString, TokenRangeObject['Close'].AsString);
      end;
      { Sub rules }
      SubRulesObject := RangeObject['SubRules'].AsObject;

      if Assigned(SubRulesObject) then
      begin
        SubRulesObject.First;
        while not SubRulesObject.Eof do
        begin
          if SubRulesObject.CurrentKey = 'Range' then
          begin
            for Member in SubRulesObject['Range'].AsArray do
            begin
              NewRange := TBCEditorRange.Create;
              ImportRange(NewRange, Member.AsObject, ARange); { ARange is for the MainRules include }
              ARange.AddRange(NewRange);
            end;
          end
          else
          if SubRulesObject.CurrentKey = 'KeyList' then
          begin
            for Member in SubRulesObject['KeyList'].AsArray do
            begin
              NewKeyList := TBCEditorKeyList.Create;
              ImportKeyList(NewKeyList, Member.AsObject);
              ARange.AddKeyList(NewKeyList);
            end;
          end
          else
          if SubRulesObject.CurrentKey = 'Set' then
          begin
            for Member in SubRulesObject['Set'].AsArray do
            begin
              NewSet := TBCEditorSet.Create;
              ImportSet(NewSet, Member.AsObject);
              ARange.AddSet(NewSet);
            end;
          end;
          SubRulesObject.Next;
        end;
      end;
    end;
  end;
end;

procedure ImportCompletionProposal(ASkipRegions: TBCEditorSkipRegions; CodeFoldingObject: ISuperObject);
var
  SkipRegionItem: TBCEditorSkipRegionItem;
  Member: IMember;
begin
  if not Assigned(CodeFoldingObject) then
    Exit;
  { Skip regions }
  for Member in CodeFoldingObject['SkipRegion'].AsArray do
  begin
    SkipRegionItem := ASkipRegions.Add(Member.AsObject['OpenToken'].AsString, Member.AsObject['CloseToken'].AsString);
    SkipRegionItem.RegionType := StrToRegionType(Member.AsObject['RegionType'].AsString);
    SkipRegionItem.SkipEmptyChars := StrToBoolean(Member.AsObject['SkipEmptyChars'].AsString);
  end;
end;

procedure ImportCodeFolding(AEditor: TBCBaseEditor; AFoldRegions: TBCEditorCodeFoldingRegions; CodeFoldingObject: ISuperObject);
var
  SkipRegionType: TBCEditorSkipRegionItemType;
  MemberObject: ISuperObject;
  Member: IMember;
  FoldRegionItem: TBCEditorFoldRegionItem;
  SkipRegionItem: TBCEditorSkipRegionItem;
begin
  if not Assigned(CodeFoldingObject) then
  begin
    AEditor.CodeFolding.Visible := False;
    Exit;
  end;
  MemberObject := CodeFoldingObject['Options'].AsObject;
  if Assigned(MemberObject) then
    AFoldRegions.StringEscapeChar := MemberObject.AsObject['StringEscapeChar'].AsString[1]
  else
    AFoldRegions.StringEscapeChar := #0;
  { Skip regions }
  if Assigned(CodeFoldingObject['SkipRegion'].AsArray) then
  for Member in CodeFoldingObject['SkipRegion'].AsArray do
  begin
    SkipRegionType := StrToRegionType(Member.AsObject['RegionType'].AsString);
    if (SkipRegionType = ritMultiLineComment) and (cfoFoldMultilineComments in AEditor.CodeFolding.Options) then
    begin
      FoldRegionItem := AFoldRegions.Add(Member.AsObject['OpenToken'].AsString, Member.AsObject['CloseToken'].AsString);
      FoldRegionItem.NoSubs := True;
    end
    else
    begin
      SkipRegionItem := AFoldRegions.SkipRegions.Add(Member.AsObject['OpenToken'].AsString, Member.AsObject['CloseToken'].AsString);
      SkipRegionItem.RegionType := SkipRegionType;
      SkipRegionItem.SkipEmptyChars := StrToBoolean(Member.AsObject['SkipEmptyChars'].AsString);
    end;
  end;
  { Fold regions }
  if Assigned(CodeFoldingObject['FoldRegion'].AsArray) then
  for Member in CodeFoldingObject['FoldRegion'].AsArray do
  begin
    FoldRegionItem := AFoldRegions.Add(Member.AsObject['OpenToken'].AsString, Member.AsObject['CloseToken'].AsString);

    MemberObject := Member.AsObject['Properties'].AsObject;
    if Assigned(MemberObject) then
    begin
      { Options }
      FoldRegionItem.BeginningOfLine := StrToBoolean(MemberObject['BeginningOfLine'].AsString);
      FoldRegionItem.SharedClose := StrToBoolean(MemberObject['SharedClose'].AsString);
      FoldRegionItem.OpenIsClose := StrToBoolean(MemberObject['OpenIsClose'].AsString);
      FoldRegionItem.NoSubs := StrToBoolean(MemberObject['NoSubs'].AsString);
      FoldRegionItem.SkipIfFoundAfterOpenToken := MemberObject['SkipIfFoundAfterOpenToken'].AsString;
      FoldRegionItem.BreakIfNotFoundBeforeNextRegion := MemberObject['BreakIfNotFoundBeforeNextRegion'].AsString;
    end;
  end;
end;

procedure ImportMatchingPair(AMatchingPairs: TList; MatchingPairObject: ISuperObject);
var
  Member: IMember;
  TokenMatch: PBCEditorMatchingPairToken;
begin
  if not Assigned(MatchingPairObject) then
    Exit;
  { Matching token pairs }
  for Member in MatchingPairObject['Pairs'].AsArray do
  begin
    { Add element }
    New(TokenMatch);
    TokenMatch.OpenToken := Member.AsObject['OpenToken'].AsString;
    TokenMatch.CloseToken := Member.AsObject['CloseToken'].AsString;
    AMatchingPairs.Add(TokenMatch)
  end;
end;

procedure ImportElements(AStyles: TList; ColorsObject: ISuperObject);
var
  Member: IMember;
  Element: PBCEditorHighlighterElement;
begin
  if not Assigned(ColorsObject) then
    Exit;
  for Member in ColorsObject['Elements'].AsArray do
  begin
    New(Element);
    Element.Background := StringToColorDef(Member.AsObject['Background'].AsString, clWindow);
    Element.Foreground := StringToColorDef(Member.AsObject['Foreground'].AsString, clWindowText);
    Element.Name := Member.AsObject['Name'].AsString;
    Element.Style := StrToFontStyle(Member.AsObject['Style'].AsString);
    AStyles.Add(Element)
  end;
end;

procedure ImportHighlighter(Highlighter: TBCEditorHighlighter; JSONObject: ISuperObject);
begin
  Highlighter.Clear;

  ImportInfo(Highlighter.Info, JSONObject['Highlighter."Info"'].AsObject);
  ImportEditorProperties(Highlighter.Editor as TBCBaseEditor, JSONObject['Highlighter."Editor"'].AsObject);
  ImportRange(Highlighter.MainRules, JSONObject['Highlighter."MainRules"'].AsObject);
  ImportCodeFolding(Highlighter.Editor as TBCBaseEditor, Highlighter.CodeFoldingRegions, JSONObject['CodeFolding'].AsObject);
  ImportMatchingPair(Highlighter.MatchingPairs, JSONObject['MatchingPair'].AsObject);
  ImportCompletionProposal(Highlighter.CompletionProposalSkipRegions, JSONObject['CompletionProposal'].AsObject);
end;

procedure ImportColors(Highlighter: TBCEditorHighlighter; JSONObject: ISuperObject);
begin
  Highlighter.Colors.Clear;

  ImportColorsInfo(Highlighter.Colors.Info, JSONObject['Colors."Info"'].AsObject);
  ImportColorsEditorProperties(Highlighter.Editor as TBCBaseEditor, JSONObject['Colors."Editor"'].AsObject);
  ImportElements(Highlighter.Colors.Styles, JSONObject['Colors'].AsObject);
end;

procedure ImportFromStream(Highlighter: TBCEditorHighlighter; AStream: TStream);
var
  JSONObject: ISuperObject;
begin
  JSONObject := TSuperObject.ParseStream(AStream);
  if Assigned(JSONObject) then
    ImportHighlighter(Highlighter, JSONObject);
end;

procedure ImportFromFile(Highlighter: TBCEditorHighlighter; AFileName: string);
var
  JSONObject: ISuperObject;
begin
  if FileExists(AFileName) then
  begin
    JSONObject := TSuperObject.ParseFile(AFileName);
    if Assigned(JSONObject) then
      ImportHighlighter(Highlighter, JSONObject);
  end
  else
    MessageDialog(Format(SBCEditorImporterFileNotFound, [AFileName]), mtError, [mbOK]);
end;

procedure ImportColorsFromStream(Highlighter: TBCEditorHighlighter; AStream: TStream);
var
  JSONObject: ISuperObject;
begin
  JSONObject := TSuperObject.ParseStream(AStream);
  if Assigned(JSONObject) then
    ImportColors(Highlighter, JSONObject);
end;

procedure ImportColorsFromFile(Highlighter: TBCEditorHighlighter; AFileName: string);
var
  JSONObject: ISuperObject;
begin
  if FileExists(AFileName) then
  begin
    JSONObject := TSuperObject.ParseFile(AFileName);
    if Assigned(JSONObject) then
      ImportColors(Highlighter, JSONObject);
  end
end;

end.




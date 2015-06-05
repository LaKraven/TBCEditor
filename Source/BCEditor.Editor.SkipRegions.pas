unit BCEditor.Editor.SkipRegions;

interface

uses
  System.Classes, System.SysUtils;

type
  TBCEditorSkipRegionItemType = (ritUnspecified, ritString, ritMultiLineComment, ritSingleLineComment);

  TBCEditorSkipRegionItem = class(TCollectionItem)
  strict private
    FCloseToken: string;
    FOpenToken: string;
    FRegionType: TBCEditorSkipRegionItemType;
    FSkipEmptyChars: Boolean;
  public
    property OpenToken: string read FOpenToken write FOpenToken;
    property CloseToken: string read FCloseToken write FCloseToken;
    property RegionType: TBCEditorSkipRegionItemType read FRegionType write FRegionType;
    property SkipEmptyChars: Boolean read FSkipEmptyChars write FSkipEmptyChars;
  end;

  TBCEditorSkipRegions = class(TCollection)
  strict private
    function GetSkipRegionItem(Index: Integer): TBCEditorSkipRegionItem;
  public
    function Add(const AOpenToken, ACloseToken: string): TBCEditorSkipRegionItem;
    function Contains(const AOpenToken, ACloseToken: string): Boolean;
    property SkipRegions[index: Integer]: TBCEditorSkipRegionItem read GetSkipRegionItem; default;
  end;

implementation

uses
  Winapi.Windows;

{ TBCEditorSkipRegions }

function TBCEditorSkipRegions.Add(const AOpenToken, ACloseToken: string): TBCEditorSkipRegionItem;
begin
  Result := TBCEditorSkipRegionItem(inherited Add);
  with Result do
  begin
    OpenToken := AOpenToken;
    CloseToken := ACloseToken;
  end;
end;

function TBCEditorSkipRegions.Contains(const AOpenToken, ACloseToken: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
    if (SkipRegions[i].OpenToken = AOpenToken) and (SkipRegions[i].CloseToken = ACloseToken) then
      Exit(True);
end;

function TBCEditorSkipRegions.GetSkipRegionItem(Index: Integer): TBCEditorSkipRegionItem;
begin
  Result := TBCEditorSkipRegionItem(inherited Items[index]);
end;

end.

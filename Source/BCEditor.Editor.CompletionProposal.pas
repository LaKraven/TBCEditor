unit BCEditor.Editor.CompletionProposal;

interface

uses
  System.Classes, Vcl.Graphics, BCEditor.Editor.CompletionProposal.Colors, BCEditor.Editor.CompletionProposal.Columns,
  BCEditor.Editor.CompletionProposal.Trigger, BCEditor.Types;

type
  TBCEditorCompletionProposal = class(TPersistent)
  strict private
    FCloseChars: string;
    FColors: TBCEditorCompletionProposalColors;
    FColumns: TBCEditorProposalColumns;
    FEnabled: Boolean;
    FFont: TFont;
    FItemList: TStrings;
    FOptions: TBCEditorCompletionProposalOptions;
    FShortCut: TShortCut;
    FTrigger: TBCEditorCompletionProposalTrigger;
    FVisibleLines: Integer;
    FWidth: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property CloseChars: string read FCloseChars write FCloseChars;
    property Colors: TBCEditorCompletionProposalColors read FColors write FColors;
    property Columns: TBCEditorProposalColumns read FColumns write FColumns;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Font: TFont read FFont write FFont;
    property ItemList: TStrings read FItemList write FItemList;
    property Options: TBCEditorCompletionProposalOptions read FOptions write FOptions default [cpoFiltered, cpoParseItemsFromText, cpoResizeable];
    property ShortCut: TShortCut read FShortCut write FShortCut;
    property Trigger: TBCEditorCompletionProposalTrigger read FTrigger write FTrigger;
    property VisibleLines: Integer read FVisibleLines write FVisibleLines default 8;
    property Width: Integer read FWidth write FWidth default 260;
  end;

implementation

uses
  Vcl.Menus;

{ TBCEditorCompletionProposal }

constructor TBCEditorCompletionProposal.Create;
begin
  inherited;

  FCloseChars := '()[]. ';
  FColors := TBCEditorCompletionProposalColors.Create;
  FColumns := TBCEditorProposalColumns.Create(Self, TBCEditorProposalColumn);
  FEnabled := True;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;
  FItemList := TStringList.Create;
  FOptions := [cpoFiltered, cpoParseItemsFromText, cpoResizeable];
  FShortCut := Vcl.Menus.ShortCut(Ord(' '), [ssCtrl]);
  FTrigger := TBCEditorCompletionProposalTrigger.Create;
  FVisibleLines := 8;
  FWidth := 260;
end;

destructor TBCEditorCompletionProposal.Destroy;
begin
  FColors.Free;
  FFont.Free;
  FItemList.Free;
  FTrigger.Free;
  FColumns.Free;

  inherited;
end;

procedure TBCEditorCompletionProposal.Assign(Source: TPersistent);
begin
  if Source is TBCEditorCompletionProposal then
  with Source as TBCEditorCompletionProposal do
  begin
    Self.FCloseChars := FCloseChars;
    Self.FColors.Assign(FColors);
    Self.FColumns.Assign(FColumns);
    Self.FEnabled := FEnabled;
    Self.FFont.Assign(FFont);
    Self.FItemList.Assign(FItemList);
    Self.FOptions := FOptions;
    Self.FShortCut := FShortCut;
    Self.FTrigger.Assign(FTrigger);
    Self.FVisibleLines := FVisibleLines;
    Self.FWidth := FWidth;
  end
  else
    inherited;
end;

end.

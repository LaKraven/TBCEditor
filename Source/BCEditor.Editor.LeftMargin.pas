unit BCEditor.Editor.LeftMargin;

interface

uses
  System.Classes, Vcl.Graphics, System.UITypes, BCEditor.Editor.LeftMargin.Bookmarks, BCEditor.Editor.Bookmarks,
  BCEditor.Editor.LeftMargin.Border, BCEditor.Consts, BCEditor.Editor.LeftMargin.LineState,
  BCEditor.Editor.LeftMargin.LineNumbers;

type
  TLeftMarginGetTextEvent = procedure(Sender: TObject; ALine: Integer; var AText: string) of object;
  TLeftMarginPaintEvent = procedure(Sender: TObject; ALine: Integer; X, Y: Integer) of object;
  TLeftMarginClickEvent = procedure(Sender: TObject; Button: TMouseButton; X, Y, Line: Integer; Bookmark: TBCEditorBookmark) of object;

  TBCEditorLeftMargin = class(TPersistent)
  strict private
    FAutosize: Boolean;
    FBookMarks: TBCEditorLeftMarginBookMarks;
    FBorder: TBCEditorLeftMarginBorder;
    FColor: TColor;
    FCursor: TCursor;
    FFont: TFont;
    FLineState: TBCEditorLeftMarginLineState;
    FLineNumbers: TBCEditorLeftMarginLineNumbers;
    FOnChange: TNotifyEvent;
    FVisible: Boolean;
    FWidth: Integer;
    procedure DoChange;
    procedure SetAutosize(const Value: Boolean);
    procedure SetBookMarks(const Value: TBCEditorLeftMarginBookMarks);
    procedure SetColor(const Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetOnChange(Value: TNotifyEvent);
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(Value: Integer);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    function GetWidth: Integer;
    function FormatLineNumber(Line: Integer): string;
    function RealLeftMarginWidth(CharWidth: Integer): Integer;
    procedure Assign(Source: TPersistent); override;
    procedure AutosizeDigitCount(LinesCount: Integer);
  published
    property Autosize: Boolean read FAutosize write SetAutosize default True;
    property Bookmarks: TBCEditorLeftMarginBookMarks read FBookMarks write SetBookMarks;
    property Border: TBCEditorLeftMarginBorder read FBorder write FBorder;
    property Color: TColor read FColor write SetColor default clLeftMarginBackground;
    property Cursor: TCursor read FCursor write FCursor default crDefault;
    property Font: TFont read FFont write SetFont;
    property LineNumbers: TBCEditorLeftMarginLineNumbers read FLineNumbers write FLineNumbers;
    property LineState: TBCEditorLeftMarginLineState read FLineState write FLineState;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Width: Integer read FWidth write SetWidth default 57;
  end;

implementation

uses
  System.SysUtils, System.Math, BCEditor.Types;

{ TBCEditorLeftMargin }

constructor TBCEditorLeftMargin.Create(AOwner: TComponent);
begin
  inherited Create;

  FAutosize := True;
  FColor := clLeftMarginBackground;
  FCursor := crDefault;
  FBorder := TBCEditorLeftMarginBorder.Create;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;
  FFont.Style := [];
  FFont.Color := clLeftMarginFontForeground;
  FWidth := 57;
  FVisible := True;

  FBookmarks := TBCEditorLeftMarginBookmarks.Create(AOwner);
  FLineState := TBCEditorLeftMarginLineState.Create;
  FLineNumbers := TBCEditorLeftMarginLineNumbers.Create;
end;

procedure TBCEditorLeftMargin.SetOnChange(Value: TNotifyEvent);
begin
  FOnChange := Value;
  FFont.OnChange := Value;
  FBookmarks.OnChange := Value;
  FBorder.OnChange := Value;
  FLineState.OnChange := Value;
  FLineNumbers.OnChange := Value;
end;

destructor TBCEditorLeftMargin.Destroy;
begin
  FFont.Free;
  FBookmarks.Free;
  FBorder.Free;
  FLineState.Free;
  FLineNumbers.Free;
  inherited Destroy;
end;

procedure TBCEditorLeftMargin.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TBCEditorLeftMargin.RealLeftMarginWidth(CharWidth: Integer): Integer;
var
  PanelWidth: Integer;
begin
  //Result := GetWidth;
  //Exit;

  PanelWidth := FBookmarks.Panel.Width;
  if not FBookmarks.Panel.Visible and not FBookmarks.Visible then
    PanelWidth := 0;

  if not FVisible then
    Result := 0
  else
  if FLineNumbers.Visible then
    Result := PanelWidth + FLineState.Width + FLineNumbers.AutosizeDigitCount * CharWidth + 5
  else
    Result := FWidth;
end;

procedure TBCEditorLeftMargin.Assign(Source: TPersistent);
begin
  if Source is TBCEditorLeftMargin then
  with Source as TBCEditorLeftMargin do
  begin
    Self.FAutosize := FAutosize;
    Self.FBookmarks.Assign(FBookmarks);
    Self.FColor := FColor;
    Self.FBorder := FBorder;
    Self.FCursor := FCursor;
    Self.FFont.Assign(FFont);
    Self.FLineNumbers.Assign(FLineNumbers);
    Self.FWidth := FWidth;
    Self.FVisible := FVisible;
    Self.DoChange;
  end
  else
    inherited;
end;

function TBCEditorLeftMargin.GetWidth: Integer;
begin
  if FVisible then
    Result := FWidth
  else
    Result := 0;
end;

procedure TBCEditorLeftMargin.SetAutosize(const Value: Boolean);
begin
  if FAutosize <> Value then
  begin
    FAutosize := Value;
    DoChange
  end;
end;

procedure TBCEditorLeftMargin.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    DoChange
  end;
end;

procedure TBCEditorLeftMargin.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TBCEditorLeftMargin.SetWidth(Value: Integer);
begin
  Value := Max(0, Value);
  if FWidth <> Value then
  begin
    FWidth := Value;
    DoChange
  end;
end;

procedure TBCEditorLeftMargin.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    DoChange
  end;
end;

procedure TBCEditorLeftMargin.SetBookMarks(const Value: TBCEditorLeftMarginBookmarks);
begin
  FBookmarks.Assign(Value);
end;

procedure TBCEditorLeftMargin.AutosizeDigitCount(LinesCount: Integer);
var
  NumberOfDigits: Integer;
begin
  if FLineNumbers.Visible and FAutosize then
  begin
    if FLineNumbers.StartFrom = 0 then
      Dec(LinesCount)
    else
      if FLineNumbers.StartFrom > 1 then
        Inc(LinesCount, FLineNumbers.StartFrom - 1);

    NumberOfDigits := Max(Length(LinesCount.ToString), FLineNumbers.DigitCount);
    if FLineNumbers.AutosizeDigitCount <> NumberOfDigits then
    begin
      FLineNumbers.AutosizeDigitCount := NumberOfDigits;
      if Assigned(FOnChange) then
        FOnChange(Self);
    end;
  end
  else
    FLineNumbers.AutosizeDigitCount := FLineNumbers.DigitCount;
end;

function TBCEditorLeftMargin.FormatLineNumber(Line: Integer): string;
var
  i: Integer;
begin
  if FLineNumbers.StartFrom = 0 then
    Dec(Line)
  else
  if FLineNumbers.StartFrom > 1 then
    Inc(Line, FLineNumbers.StartFrom - 1);
  Result := Format('%*d', [FLineNumbers.AutosizeDigitCount, Line]);
  if lnoLeadingZeros in FLineNumbers.Options then
    for i := 1 to FLineNumbers.AutosizeDigitCount - 1 do
    begin
      if Result[i] <> ' ' then
        Break;
      Result[i] := '0';
    end;
end;

end.

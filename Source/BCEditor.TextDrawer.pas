unit BCEditor.TextDrawer;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, System.Math, System.Types, System.UITypes,
  BCEditor.Utils;

const
  FontStyleCount = Ord(High(TFontStyle)) + 1;
  FontStyleCombineCount = 1 shl FontStyleCount;

type
  TBCEditorStockFontPatterns = 0 .. FontStyleCombineCount - 1;

  TBCEditorFontData = record
    Style: TFontStyles;
    Handle: HFont;
    CharAdv: Integer;
    CharHeight: Integer;
  end;
  PBCEditorFontData = ^TBCEditorFontData;

  TBCEditorFontsData = array [TBCEditorStockFontPatterns] of TBCEditorFontData;

  TBCEditorSharedFontsInfo = record
    RefCount: Integer;
    LockCount: Integer;
    BaseFont: TFont;
    BaseLogFont: TLogFont;
    IsTrueType: Boolean;
    FontsData: TBCEditorFontsData;
  end;

  PBCEditorSharedFontsInfo = ^TBCEditorSharedFontsInfo;

  { TBCEditorFontsInfoManager }

  TBCEditorFontsInfoManager = class
  strict private
    FFontsInfo: TList;
    function FindFontsInfo(const LogFont: TLogFont): PBCEditorSharedFontsInfo;
    function CreateFontsInfo(ABaseFont: TFont; const LogFont: TLogFont): PBCEditorSharedFontsInfo;
    procedure DestroyFontHandles(SharedFontsInfo: PBCEditorSharedFontsInfo);
    procedure RetrieveLogFontForComparison(ABaseFont: TFont; var LogFont: TLogFont);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LockFontsInfo(SharedFontsInfo: PBCEditorSharedFontsInfo);
    procedure UnLockFontsInfo(SharedFontsInfo: PBCEditorSharedFontsInfo);
    function GetFontsInfo(ABaseFont: TFont): PBCEditorSharedFontsInfo;
    procedure ReleaseFontsInfo(SharedFontsInfo: PBCEditorSharedFontsInfo);
  end;

  TBCEditorTextOutOptions = set of (tooOpaque, tooClipped);

  TBCEditorFontStock = class
  strict private
    FCurrentFont: HFont;
    FCurrentStyle: TFontStyles;
    FHandle: HDC;
    FHandleRefCount: Integer;
    FPSharedFontsInfo: PBCEditorSharedFontsInfo;
    FUsingFontHandles: Boolean;
    FPCurrentFontData: PBCEditorFontData;
    FBaseLogFont: TLogFont;
    function GetBaseFont: TFont;
    function GetIsTrueType: Boolean;
  protected
    function CalcFontAdvance(AHandle: HDC; pCharHeight: PInteger): Integer; virtual;
    function GetCharAdvance: Integer; virtual;
    function GetCharHeight: Integer; virtual;
    function GetFontData(Index: Integer): PBCEditorFontData; virtual;
    function InternalGetHandle: HDC; virtual;
    function InternalCreateFont(Style: TFontStyles): HFont; virtual;
    procedure InternalReleaseDC(Value: HDC); virtual;
    procedure ReleaseFontsInfo;
    procedure SetBaseFont(Value: TFont); virtual;
    procedure SetStyle(Value: TFontStyles); virtual;
    procedure UseFontHandles;
    property FontData[Index: Integer]: PBCEditorFontData read GetFontData;
    property FontsInfo: PBCEditorSharedFontsInfo read FPSharedFontsInfo;
  public
    constructor Create(InitialFont: TFont); virtual;
    destructor Destroy; override;

    procedure ReleaseFontHandles; virtual;
    property BaseFont: TFont read GetBaseFont;
    property Style: TFontStyles read FCurrentStyle write SetStyle;
    property FontHandle: HFont read FCurrentFont;
    property CharAdvance: Integer read GetCharAdvance;
    property CharHeight: Integer read GetCharHeight;
    property IsTrueType: Boolean read GetIsTrueType;
  end;

  { TBCEditorTextDrawer }
  ETextDrawerException = class(Exception);

  TBCEditorTextDrawer = class(TObject)
  strict private
    FBackgroundColor: TColor;
    FBaseCharHeight: Integer;
    FBaseCharWidth: Integer;
    FCalcExtentBaseStyle: TFontStyles;
    FCharABCWidthCache: array [0 .. 127] of TABC;
    FCharExtra: Integer;
    FCharWidthCache: array [0 .. 127] of Integer;
    FColor: TColor;
    FCurrentFont: HFont;
    FDrawingCount: Integer;
    FExtTextOutDistance: PIntegerArray;
    FFontStock: TBCEditorFontStock;
    FHandle: HDC;
    FSaveHandle: Integer;
    FStockBitmap: TBitmap;
  protected
    function GetCachedABCWidth(AChar: Cardinal; var AABC: TABC): Boolean;
    procedure AfterStyleSet; virtual;
    procedure DoSetCharExtra(Value: Integer); virtual;
    procedure FlushCharABCWidthCache;
    procedure ReleaseExtTextOutDistance; virtual;
    property BaseCharHeight: Integer read FBaseCharHeight;
    property BaseCharWidth: Integer read FBaseCharWidth;
    property DrawingCount: Integer read FDrawingCount;
    property FontStock: TBCEditorFontStock read FFontStock;
    property StockHandle: HDC read FHandle;
  public
    constructor Create(CalcExtentBaseStyle: TFontStyles; BaseFont: TFont); virtual;
    destructor Destroy; override;

    function GetCharWidth: Integer; virtual;
    function GetCharHeight: Integer; virtual;
    procedure BeginDrawing(AHandle: HDC); virtual;
    procedure EndDrawing; virtual;
    procedure TextOut(X, Y: Integer; Text: PChar; Length: Integer); virtual;
    procedure ExtTextOut(X, Y: Integer; AOptions: TBCEditorTextOutOptions; ARect: TRect; AText: PChar; ALength: Integer); virtual;
    function TextExtent(const Text: string): TSize; overload;
    function TextExtent(Text: PChar; Count: Integer): TSize; overload;
    function TextWidth(const Text: string): Integer; overload;
    function TextWidth(Text: PChar; Count: Integer): Integer; overload;
    procedure SetBaseFont(Value: TFont); virtual;
    procedure SetBaseStyle(const Value: TFontStyles); virtual;
    procedure SetStyle(Value: TFontStyles); virtual;
    procedure SetForegroundColor(Value: TColor); virtual;
    procedure SetBackgroundColor(Value: TColor); virtual;
    procedure SetCharExtra(Value: Integer); virtual;
    property CharWidth: Integer read GetCharWidth;
    property CharHeight: Integer read GetCharHeight;
    property BaseFont: TFont write SetBaseFont;
    property BaseStyle: TFontStyles write SetBaseStyle;
    property ForegroundColor: TColor write SetForegroundColor;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property Style: TFontStyles write SetStyle;
    property CharExtra: Integer read FCharExtra write SetCharExtra;
  end;

function GetFontsInfoManager: TBCEditorFontsInfoManager;

function UniversalExtTextOut(AHandle: HDC; X, Y: Integer; AOptions: TBCEditorTextOutOptions; ARect: TRect; AStr: PChar;
  ACount: Integer; AExtTextOutDistance: PIntegerArray): Boolean;

implementation

var
  GFontsInfoManager: TBCEditorFontsInfoManager;

  { utility routines }

function GetFontsInfoManager: TBCEditorFontsInfoManager;
begin
  if not Assigned(GFontsInfoManager) then
    GFontsInfoManager := TBCEditorFontsInfoManager.Create;
  Result := GFontsInfoManager;
end;

function UniversalExtTextOut(AHandle: HDC; X, Y: Integer; AOptions: TBCEditorTextOutOptions; ARect: TRect; AStr: PChar;
  ACount: Integer; AExtTextOutDistance: PIntegerArray): Boolean;
var
  LTextOutFlags: DWORD;
begin
  LTextOutFlags := 0;
  if tooOpaque in AOptions then
    LTextOutFlags := LTextOutFlags or ETO_OPAQUE;
  if tooClipped in AOptions then
    LTextOutFlags := LTextOutFlags or ETO_CLIPPED;

  Result := Winapi.Windows.ExtTextOut(AHandle, X, Y, LTextOutFlags, @ARect, AStr, ACount, Pointer(AExtTextOutDistance));
end;

{ TFontsInfoManager }

procedure TBCEditorFontsInfoManager.LockFontsInfo(SharedFontsInfo: PBCEditorSharedFontsInfo);
begin
  Inc(SharedFontsInfo^.LockCount);
end;

constructor TBCEditorFontsInfoManager.Create;
begin
  inherited;

  FFontsInfo := TList.Create;
end;

function TBCEditorFontsInfoManager.CreateFontsInfo(ABaseFont: TFont; const LogFont: TLogFont): PBCEditorSharedFontsInfo;
begin
  New(Result);
  FillChar(Result^, SizeOf(TBCEditorSharedFontsInfo), 0);
  with Result^ do
  try
    BaseFont := TFont.Create;
    BaseFont.Assign(ABaseFont);
    BaseLogFont := LogFont;
    IsTrueType := (0 <> (TRUETYPE_FONTTYPE and LogFont.lfPitchAndFamily));
  except
    Result^.BaseFont.Free;
    Dispose(Result);
    raise;
  end;
end;

procedure TBCEditorFontsInfoManager.UnLockFontsInfo(SharedFontsInfo: PBCEditorSharedFontsInfo);
begin
  with SharedFontsInfo^ do
  begin
    Dec(LockCount);
    if 0 = LockCount then
      DestroyFontHandles(SharedFontsInfo);
  end;
end;

destructor TBCEditorFontsInfoManager.Destroy;
begin
  GFontsInfoManager := nil;

  if Assigned(FFontsInfo) then
  begin
    while FFontsInfo.Count > 0 do
    begin
      Assert(1 = PBCEditorSharedFontsInfo(FFontsInfo[FFontsInfo.Count - 1])^.RefCount);
      ReleaseFontsInfo(PBCEditorSharedFontsInfo(FFontsInfo[FFontsInfo.Count - 1]));
    end;
    FFontsInfo.Free;
  end;

  inherited;
end;

procedure TBCEditorFontsInfoManager.DestroyFontHandles(SharedFontsInfo: PBCEditorSharedFontsInfo);
var
  i: Integer;
begin
  with SharedFontsInfo^ do
    for i := Low(TBCEditorStockFontPatterns) to High(TBCEditorStockFontPatterns) do
      with FontsData[i] do
        if Handle <> 0 then
        begin
          DeleteObject(Handle);
          Handle := 0;
        end;
end;

function TBCEditorFontsInfoManager.FindFontsInfo(const LogFont: TLogFont): PBCEditorSharedFontsInfo;
var
  i: Integer;
begin
  for i := 0 to FFontsInfo.Count - 1 do
  begin
    Result := PBCEditorSharedFontsInfo(FFontsInfo[i]);
    if CompareMem(@(Result^.BaseLogFont), @LogFont, SizeOf(TLogFont)) then
      Exit;
  end;
  Result := nil;
end;

function TBCEditorFontsInfoManager.GetFontsInfo(ABaseFont: TFont): PBCEditorSharedFontsInfo;
var
  LogFont: TLogFont;
begin
  Assert(Assigned(ABaseFont));

  RetrieveLogFontForComparison(ABaseFont, LogFont);
  Result := FindFontsInfo(LogFont);
  if not Assigned(Result) then
  begin
    Result := CreateFontsInfo(ABaseFont, LogFont);
    FFontsInfo.Add(Result);
  end;

  if Assigned(Result) then
    Inc(Result^.RefCount);
end;

procedure TBCEditorFontsInfoManager.ReleaseFontsInfo(SharedFontsInfo: PBCEditorSharedFontsInfo);
begin
  Assert(Assigned(SharedFontsInfo));

  with SharedFontsInfo^ do
  begin
    Assert(LockCount < RefCount);
    if RefCount > 1 then
      Dec(RefCount)
    else
    begin
      FFontsInfo.Remove(SharedFontsInfo);
      // free all objects
      BaseFont.Free;
      Dispose(SharedFontsInfo);
    end;
  end;
end;

procedure TBCEditorFontsInfoManager.RetrieveLogFontForComparison(ABaseFont: TFont; var LogFont: TLogFont);
var
  pEnd: PChar;
begin
  GetObject(ABaseFont.Handle, SizeOf(TLogFont), @LogFont);
  with LogFont do
  begin
    lfItalic := 0;
    lfUnderline := 0;
    lfStrikeOut := 0;
    pEnd := StrEnd(lfFaceName);
    FillChar(pEnd[1], @lfFaceName[high(lfFaceName)] - pEnd, 0);
  end;
end;

{ TFontStock }

function TBCEditorFontStock.CalcFontAdvance(AHandle: HDC; pCharHeight: PInteger): Integer;
var
  LTextMetric: TTextMetric;
  LCharInfo: TABC;
  LHasABC: Boolean;
begin
  GetTextMetrics(AHandle, LTextMetric);
  LHasABC := GetCharABCWidths(AHandle, Ord('M'), Ord('M'), LCharInfo);
  if not LHasABC then
  begin
    with LCharInfo do
    begin
      abcA := 0;
      abcB := LTextMetric.tmAveCharWidth;
      abcC := 0;
    end;
    LTextMetric.tmOverhang := 0;
  end;

  with LCharInfo do
    Result := abcA + Integer(abcB) + abcC + LTextMetric.tmOverhang;
  if Assigned(pCharHeight) then
    pCharHeight^ := Abs(LTextMetric.tmHeight)
end;

constructor TBCEditorFontStock.Create(InitialFont: TFont);
begin
  inherited Create;

  SetBaseFont(InitialFont);
end;

destructor TBCEditorFontStock.Destroy;
begin
  ReleaseFontsInfo;
  Assert(FHandleRefCount = 0);

  inherited;
end;

function TBCEditorFontStock.GetBaseFont: TFont;
begin
  Result := FPSharedFontsInfo^.BaseFont;
end;

function TBCEditorFontStock.GetCharAdvance: Integer;
begin
  Result := FPCurrentFontData^.CharAdv;
end;

function TBCEditorFontStock.GetCharHeight: Integer;
begin
  Result := FPCurrentFontData^.CharHeight;
end;

function TBCEditorFontStock.GetFontData(Index: Integer): PBCEditorFontData;
begin
  Result := @FPSharedFontsInfo^.FontsData[Index];
end;

function TBCEditorFontStock.GetIsTrueType: Boolean;
begin
  Result := FPSharedFontsInfo^.IsTrueType
end;

function TBCEditorFontStock.InternalCreateFont(Style: TFontStyles): HFont;
const
  Bolds: array [Boolean] of Integer = (400, 700);
begin
  with FBaseLogFont do
  begin
    lfWeight := Bolds[fsBold in Style];
    lfItalic := Ord(BOOL(fsItalic in Style));
    lfUnderline := Ord(BOOL(fsUnderline in Style));
    lfStrikeOut := Ord(BOOL(fsStrikeOut in Style));
  end;
  Result := CreateFontIndirect(FBaseLogFont);
end;

function TBCEditorFontStock.InternalGetHandle: HDC;
begin
  if FHandleRefCount = 0 then
  begin
    Assert(FHandle = 0);
    FHandle := GetDC(0);
  end;
  Inc(FHandleRefCount);
  Result := FHandle;
end;

procedure TBCEditorFontStock.InternalReleaseDC(Value: HDC);
begin
  Dec(FHandleRefCount);
  if FHandleRefCount <= 0 then
  begin
    Assert((FHandle <> 0) and (FHandle = Value));
    ReleaseDC(0, FHandle);
    FHandle := 0;
    Assert(FHandleRefCount = 0);
  end;
end;

procedure TBCEditorFontStock.ReleaseFontHandles;
begin
  if FUsingFontHandles then
    with GetFontsInfoManager do
    begin
      UnLockFontsInfo(FPSharedFontsInfo);
      FUsingFontHandles := False;
    end;
end;

procedure TBCEditorFontStock.ReleaseFontsInfo;
begin
  if Assigned(FPSharedFontsInfo) then
    with GetFontsInfoManager do
    begin
      if FUsingFontHandles then
      begin
        UnLockFontsInfo(FPSharedFontsInfo);
        FUsingFontHandles := False;
      end;
      ReleaseFontsInfo(FPSharedFontsInfo);
      FPSharedFontsInfo := nil;
    end;
end;

procedure TBCEditorFontStock.SetBaseFont(Value: TFont);
var
  SharedFontsInfo: PBCEditorSharedFontsInfo;
begin
  if Assigned(Value) then
  begin
    SharedFontsInfo := GetFontsInfoManager.GetFontsInfo(Value);
    if SharedFontsInfo = FPSharedFontsInfo then
      GetFontsInfoManager.ReleaseFontsInfo(SharedFontsInfo)
    else
    begin
      ReleaseFontsInfo;
      FPSharedFontsInfo := SharedFontsInfo;
      FBaseLogFont := FPSharedFontsInfo^.BaseLogFont;
      SetStyle(Value.Style);
    end;
  end
  else
    raise Exception.Create('SetBaseFont: ''Value'' must be specified.');
end;

procedure TBCEditorFontStock.SetStyle(Value: TFontStyles);
var
  Index: Integer;
  LHandle: HDC;
  OldFont: HFont;
  FontDataPointer: PBCEditorFontData;
begin
  Assert(SizeOf(TFontStyles) = 1);

  Index := Byte(Value);
  Assert(Index <= High(TBCEditorStockFontPatterns));

  UseFontHandles;
  FontDataPointer := FontData[Index];
  if FPCurrentFontData = FontDataPointer then
    Exit;

  FPCurrentFontData := FontDataPointer;
  with FontDataPointer^ do
    if Handle <> 0 then
    begin
      FCurrentFont := Handle;
      FCurrentStyle := Style;
      Exit;
    end;

  FCurrentFont := InternalCreateFont(Value);
  LHandle := InternalGetHandle;
  OldFont := SelectObject(LHandle, FCurrentFont);

  with FPCurrentFontData^ do
  begin
    Handle := FCurrentFont;
    CharAdv := CalcFontAdvance(LHandle, @CharHeight);
  end;

  SelectObject(LHandle, OldFont);
  InternalReleaseDC(LHandle);
end;

procedure TBCEditorFontStock.UseFontHandles;
begin
  if not FUsingFontHandles then
    with GetFontsInfoManager do
    begin
      LockFontsInfo(FPSharedFontsInfo);
      FUsingFontHandles := True;
    end;
end;

{ TBCEditorTextDrawer }

constructor TBCEditorTextDrawer.Create(CalcExtentBaseStyle: TFontStyles; BaseFont: TFont);
begin
  inherited Create;

  FFontStock := TBCEditorFontStock.Create(BaseFont);
  FStockBitmap := TBitmap.Create;
  FCalcExtentBaseStyle := CalcExtentBaseStyle;
  SetBaseFont(BaseFont);
  FColor := clWindowText;
  FBackgroundColor := clWindow;
end;

destructor TBCEditorTextDrawer.Destroy;
begin
  FStockBitmap.Free;
  FFontStock.Free;
  ReleaseExtTextOutDistance;

  inherited;
end;

procedure TBCEditorTextDrawer.ReleaseExtTextOutDistance;
begin
  if Assigned(FExtTextOutDistance) then
  begin
    FreeMem(FExtTextOutDistance);
    FExtTextOutDistance := nil;
  end;
end;

procedure TBCEditorTextDrawer.BeginDrawing(AHandle: HDC);
begin
  if FHandle = AHandle then
    Assert(FHandle <> 0)
  else
  begin
    Assert((FHandle = 0) and (AHandle <> 0) and (FDrawingCount = 0));
    FHandle := AHandle;
    FSaveHandle := SaveDC(AHandle);
    SelectObject(AHandle, FCurrentFont);
    Winapi.Windows.SetTextColor(AHandle, ColorToRGB(FColor));
    Winapi.Windows.SetBkColor(AHandle, ColorToRGB(FBackgroundColor));
    DoSetCharExtra(FCharExtra);
  end;
  Inc(FDrawingCount);
end;

procedure TBCEditorTextDrawer.EndDrawing;
begin
  Assert(FDrawingCount >= 1);
  Dec(FDrawingCount);
  if FDrawingCount <= 0 then
  begin
    if FHandle <> 0 then
      RestoreDC(FHandle, FSaveHandle);
    FSaveHandle := 0;
    FHandle := 0;
    FDrawingCount := 0;
  end;
end;

function TBCEditorTextDrawer.GetCharWidth: Integer;
begin
  Result := FBaseCharWidth + FCharExtra;
end;

function TBCEditorTextDrawer.GetCharHeight: Integer;
begin
  Result := FBaseCharHeight;
end;

procedure TBCEditorTextDrawer.SetBaseFont(Value: TFont);
begin
  if Assigned(Value) then
  begin
    FlushCharABCWidthCache;
    ReleaseExtTextOutDistance;
    FStockBitmap.Canvas.Font.Assign(Value);
    FStockBitmap.Canvas.Font.Style := [];
    with FFontStock do
    begin
      SetBaseFont(Value);
      Style := FCalcExtentBaseStyle;
      FBaseCharWidth := CharAdvance;
      FBaseCharHeight := CharHeight;
    end;
    SetStyle(Value.Style);
  end
  else
    raise ETextDrawerException.Create('SetBaseFont: ''Value'' must be specified.');
end;

procedure TBCEditorTextDrawer.SetBaseStyle(const Value: TFontStyles);
begin
  if FCalcExtentBaseStyle <> Value then
  begin
    FlushCharABCWidthCache;
    FCalcExtentBaseStyle := Value;
    ReleaseExtTextOutDistance;
    with FFontStock do
    begin
      Style := Value;
      FBaseCharWidth := CharAdvance;
      FBaseCharHeight := CharHeight;
    end;
  end;
end;

procedure TBCEditorTextDrawer.SetStyle(Value: TFontStyles);
begin
  with FFontStock do
  begin
    SetStyle(Value);
    Self.FCurrentFont := FontHandle;
  end;
  AfterStyleSet;
end;

procedure TBCEditorTextDrawer.FlushCharABCWidthCache;
begin
  FillChar(FCharABCWidthCache, SizeOf(TABC) * Length(FCharABCWidthCache), 0);
  FillChar(FCharWidthCache, SizeOf(Integer) * Length(FCharWidthCache), 0);
end;

procedure TBCEditorTextDrawer.AfterStyleSet;
begin
  if FHandle <> 0 then
    SelectObject(FHandle, FCurrentFont);
end;

function TBCEditorTextDrawer.GetCachedABCWidth(AChar: Cardinal; var AABC: TABC): Boolean;
begin
  if AChar > High(FCharABCWidthCache) then
  begin
    Result := GetCharABCWidthsW(FHandle, AChar, AChar, AABC);
    Exit;
  end;
  AABC := FCharABCWidthCache[AChar];
  if (AABC.abcA or Integer(AABC.abcB) or AABC.abcC) = 0 then
  begin
    Result := GetCharABCWidthsW(FHandle, AChar, AChar, AABC);
    if Result then
      FCharABCWidthCache[AChar] := AABC;
  end
  else
    Result := True;
end;

procedure TBCEditorTextDrawer.SetForegroundColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if FHandle <> 0 then
      SetTextColor(FHandle, ColorToRGB(Value));
  end;
end;

procedure TBCEditorTextDrawer.SetBackgroundColor(Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    if FHandle <> 0 then
      Winapi.Windows.SetBkColor(FHandle, ColorToRGB(Value));
  end;
end;

procedure TBCEditorTextDrawer.SetCharExtra(Value: Integer);
begin
  if FCharExtra <> Value then
  begin
    FCharExtra := Value;
    DoSetCharExtra(FCharExtra);
  end;
end;

procedure TBCEditorTextDrawer.DoSetCharExtra(Value: Integer);
begin
  if FHandle <> 0 then
    SetTextCharacterExtra(FHandle, Value);
end;

procedure TBCEditorTextDrawer.TextOut(X, Y: Integer; Text: PChar; Length: Integer);
var
  TempRect: TRect;
begin
  TempRect := Rect(X, Y, X, Y);
  UniversalExtTextOut(FHandle, X, Y, [], TempRect, Text, Length, nil);
end;

procedure TBCEditorTextDrawer.ExtTextOut(X, Y: Integer; AOptions: TBCEditorTextOutOptions; ARect: TRect; AText: PChar;
  ALength: Integer);

  procedure InitExtTextOutDistance(ACharWidth: Integer);
  var
    i: Integer;
  begin
    ReallocMem(FExtTextOutDistance, ALength * SizeOf(Integer));
    for i := 0 to ALength - 1 do
      FExtTextOutDistance[i] := CharWidthTable(AText[i]) * ACharWidth;
  end;

  procedure AdjustLastCharWidthAndRect;
  var
    LLastChar: Cardinal;
    LRealCharWidth, LCharWidth: Integer;
    LCharInfo: TABC;
    LTextMetricA: TTextMetricA;
  begin
    if ALength <= 0 then
      Exit;
    LLastChar := Ord(AText[ALength - 1]);
    LCharWidth := FExtTextOutDistance[ALength - 1];
    LRealCharWidth := LCharWidth;

    if GetCachedABCWidth(LLastChar, LCharInfo) then
    begin
      LRealCharWidth := LCharInfo.abcA + Integer(LCharInfo.abcB);
      if LCharInfo.abcC >= 0 then
        Inc(LRealCharWidth, LCharInfo.abcC);
    end
    else if LLastChar < Ord(High(AnsiChar)) then
    begin
      GetTextMetricsA(FHandle, LTextMetricA);
      LRealCharWidth := LTextMetricA.tmAveCharWidth + LTextMetricA.tmOverhang;
    end;

    if LRealCharWidth > LCharWidth then
      Inc(ARect.Right, LRealCharWidth - LCharWidth);
    FExtTextOutDistance[ALength - 1] := Max(LRealCharWidth, LCharWidth);
  end;

begin
  InitExtTextOutDistance(GetCharWidth);
  AdjustLastCharWidthAndRect;
  UniversalExtTextOut(FHandle, X, Y, AOptions, ARect, AText, ALength, FExtTextOutDistance);
end;

function TBCEditorTextDrawer.TextExtent(const Text: string): TSize;
begin
  Result := BCEditor.Utils.TextExtent(FStockBitmap.Canvas, Text);
end;

function TBCEditorTextDrawer.TextExtent(Text: PChar; Count: Integer): TSize;
begin
  Result := BCEditor.Utils.GetTextSize(FStockBitmap.Canvas.Handle, Text, Count);
end;

function TBCEditorTextDrawer.TextWidth(const Text: string): Integer;
var
  LCharCode: Cardinal;
begin
  if Length(Text) = 1 then
  begin
    LCharCode := Ord(Text[1]);
    if LCharCode <= High(FCharWidthCache) then begin
       Result := FCharWidthCache[LCharCode];
       if Result=0 then
       begin
          Result := BCEditor.Utils.TextExtent(FStockBitmap.Canvas, Text).cX;
          FCharWidthCache[LCharCode] := Result;
       end;
       Exit;
    end;
  end;
  Result := BCEditor.Utils.TextExtent(FStockBitmap.Canvas, Text).cX;
end;

function TBCEditorTextDrawer.TextWidth(Text: PChar; Count: Integer): Integer;
begin
  Result := BCEditor.Utils.GetTextSize(FStockBitmap.Canvas.Handle, Text, Count).cX;
end;

initialization

finalization

if Assigned(GFontsInfoManager) then
  GFontsInfoManager.Free;

end.

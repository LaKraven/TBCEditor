unit BCEditor.Print.Margins;

interface

uses
  System.Classes, System.SysUtils, Vcl.Graphics, BCEditor.Print.Types, BCEditor.Print.PrinterInfo, BCEditor.Utils;

type
  TBCEditorPrintMargins = class(TPersistent)
  strict private
    FBottom: Double;
    FFooter: Double;
    FHeader: Double;
    FInternalMargin: Double;
    FLeft: Double;
    FLeftTextIndent: Double;
    FMargin: Double;
    FMirrorMargins: Boolean;
    FRight: Double;
    FRightTextIndent: Double;
    FTop: Double;
    FUnitSystem: TBCEditorUnitSystem;
    function ConvertFrom(Value: Double): Double;
    function ConvertTo(Value: Double): Double;
    function GetBottom: Double;
    function GetFooter: Double;
    function GetHeader: Double;
    function GetInternalMargin: Double;
    function GetLeft: Double;
    function GetLeftTextIndent: Double;
    function GetMargin: Double;
    function GetRight: Double;
    function GetRightTextIndent: Double;
    function GetTop: Double;
    procedure SetBottom(const Value: Double);
    procedure SetFooter(const Value: Double);
    procedure SetHeader(const Value: Double);
    procedure SetInternalMargin(const Value: Double);
    procedure SetLeft(const Value: Double);
    procedure SetLeftTextIndent(const Value: Double);
    procedure SetMargin(const Value: Double);
    procedure SetRight(const Value: Double);
    procedure SetRightTextIndent(const Value: Double);
    procedure SetTop(const Value: Double);
  public
    PixelBottom: Integer;
    PixelFooter: Integer;
    PixelHeader: Integer;
    PixelInternalMargin: Integer;
    PixelLeft: Integer;
    PixelLeftTextIndent: Integer;
    PixelMargin: Integer;
    PixelRight: Integer;
    PixelRightTextIndent: Integer;
    PixelTop: Integer;
    constructor Create;

    procedure Assign(Source: TPersistent); override;
    procedure InitPage(ACanvas: TCanvas; PageNum: Integer; PrinterInfo: TBCEditorPrinterInfo;
      LineNumbers, LineNumbersInMargin: Boolean; MaxLineNum: Integer);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
  published
    property Bottom: Double read GetBottom write SetBottom;
    property Footer: Double read GetFooter write SetFooter;
    property Header: Double read GetHeader write SetHeader;
    property InternalMargin: Double read GetInternalMargin write SetInternalMargin;
    property Left: Double read GetLeft write SetLeft;
    property LeftTextIndent: Double read GetLeftTextIndent write SetLeftTextIndent;
    property Margin: Double read GetMargin write SetMargin;
    property MirrorMargins: Boolean read FMirrorMargins write FMirrorMargins;
    property Right: Double read GetRight write SetRight;
    property RightTextIndent: Double read GetRightTextIndent write SetRightTextIndent;
    property Top: Double read GetTop write SetTop;
    property UnitSystem: TBCEditorUnitSystem read FUnitSystem write FUnitSystem default usMM;
  end;

implementation

{ TBCEditorPrintMargins }

const
  mmPrInch = 25.4;
  mmPrCm = 10;

constructor TBCEditorPrintMargins.Create;
begin
  inherited;
  FUnitSystem := usMM;
  FLeft := BCEDITOR_DEFAULT_LEFT_MARGIN_MM;
  FRight := BCEDITOR_DEFAULT_RIGHT_MARGIN_MM;
  FTop := BCEDITOR_DEFAULT_TOP_MARGIN_MM;
  FBottom := BCEDITOR_DEFAULT_BOTTOM_MM;
  FHeader := BCEDITOR_DEFAULT_HEADER_MM;
  FFooter := BCEDITOR_DEFAULT_FOOTER_MM;
  FLeftTextIndent := BCEDITOR_DEFAULT_LEFT_TEXT_INDENT_MM;
  FRightTextIndent := BCEDITOR_DEFAULT_RIGHT_TEXT_INDENT_MM;
  FInternalMargin := BCEDITOR_DEFAULT_INTERNAL_MARGIN_MM;
  FMargin := BCEDITOR_DEFAULT_MARGIN_MM;
  FMirrorMargins := False;
end;

function TBCEditorPrintMargins.ConvertTo(Value: Double): Double;
begin
  case FUnitSystem of
    usCM:
      Result := Value * mmPrCm;
    usInch:
      Result := Value * mmPrInch;
    muThousandthsOfInches:
      Result := mmPrInch * Value / 1000;
  else
    Result := Value;
  end;
end;

function TBCEditorPrintMargins.ConvertFrom(Value: Double): Double;
begin
  case FUnitSystem of
    usCM:
      Result := Value / mmPrCm;
    usInch:
      Result := Value / mmPrInch;
    muThousandthsOfInches:
      Result := 1000 * Value / mmPrInch;
  else
    Result := Value;
  end;
end;

function TBCEditorPrintMargins.GetBottom: Double;
begin
  Result := ConvertFrom(FBottom);
end;

function TBCEditorPrintMargins.GetFooter: Double;
begin
  Result := ConvertFrom(FFooter);
end;

function TBCEditorPrintMargins.GetMargin: Double;
begin
  Result := ConvertFrom(FMargin);
end;

function TBCEditorPrintMargins.GetHeader: Double;
begin
  Result := ConvertFrom(FHeader);
end;

function TBCEditorPrintMargins.GetLeft: Double;
begin
  Result := ConvertFrom(FLeft);
end;

function TBCEditorPrintMargins.GetRight: Double;
begin
  Result := ConvertFrom(FRight);
end;

function TBCEditorPrintMargins.GetTop: Double;
begin
  Result := ConvertFrom(FTop);
end;

function TBCEditorPrintMargins.GetLeftTextIndent: Double;
begin
  Result := ConvertFrom(FLeftTextIndent);
end;

function TBCEditorPrintMargins.GetRightTextIndent: Double;
begin
  Result := ConvertFrom(FRightTextIndent);
end;

function TBCEditorPrintMargins.GetInternalMargin: Double;
begin
  Result := ConvertFrom(FInternalMargin);
end;

procedure TBCEditorPrintMargins.SetBottom(const Value: Double);
begin
  FBottom := ConvertTo(Value);
end;

procedure TBCEditorPrintMargins.SetFooter(const Value: Double);
begin
  FFooter := ConvertTo(Value);
end;

procedure TBCEditorPrintMargins.SetMargin(const Value: Double);
begin
  FMargin := ConvertTo(Value);
end;

procedure TBCEditorPrintMargins.SetHeader(const Value: Double);
begin
  FHeader := ConvertTo(Value);
end;

procedure TBCEditorPrintMargins.SetLeft(const Value: Double);
begin
  FLeft := ConvertTo(Value);
end;

procedure TBCEditorPrintMargins.SetRight(const Value: Double);
begin
  FRight := ConvertTo(Value);
end;

procedure TBCEditorPrintMargins.SetTop(const Value: Double);
begin
  FTop := ConvertTo(Value);
end;

procedure TBCEditorPrintMargins.SetLeftTextIndent(const Value: Double);
begin
  FLeftTextIndent := ConvertTo(Value);
end;

procedure TBCEditorPrintMargins.SetRightTextIndent(const Value: Double);
begin
  FRightTextIndent := ConvertTo(Value);
end;

procedure TBCEditorPrintMargins.SetInternalMargin(const Value: Double);
begin
  FInternalMargin := ConvertTo(Value);
end;

procedure TBCEditorPrintMargins.InitPage(ACanvas: TCanvas; PageNum: Integer; PrinterInfo: TBCEditorPrinterInfo;
  LineNumbers, LineNumbersInMargin: Boolean; MaxLineNum: Integer);
begin
  if FMirrorMargins and ((PageNum mod 2) = 0) then
  begin
    PixelLeft := PrinterInfo.PixFromLeft(FRight);
    PixelRight := PrinterInfo.PrintableWidth - PrinterInfo.PixFromRight(FLeft + FMargin);
  end
  else
  begin
    PixelLeft := PrinterInfo.PixFromLeft(FLeft + FMargin);
    PixelRight := PrinterInfo.PrintableWidth - PrinterInfo.PixFromRight(FRight);
  end;
  if LineNumbers and (not LineNumbersInMargin) then
    PixelLeft := PixelLeft + TextWidth(ACanvas, IntToStr(MaxLineNum) + ': ');
  PixelTop := PrinterInfo.PixFromTop(FTop);
  PixelBottom := PrinterInfo.PrintableHeight - PrinterInfo.PixFromBottom(FBottom);
  PixelHeader := PrinterInfo.PixFromTop(FHeader);
  PixelFooter := PrinterInfo.PrintableHeight - PrinterInfo.PixFromBottom(FFooter);
  PixelInternalMargin := RoundCorrect(PrinterInfo.YPixPermm * FInternalMargin);
  PixelMargin := RoundCorrect(PrinterInfo.XPixPermm * FMargin);
  PixelRightTextIndent := PixelRight - RoundCorrect(PrinterInfo.XPixPermm * FRightTextIndent);
  PixelLeftTextIndent := PixelLeft + RoundCorrect(PrinterInfo.XPixPermm * FLeftTextIndent);
end;

procedure TBCEditorPrintMargins.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TBCEditorPrintMargins) then
  with Source as TBCEditorPrintMargins do
  begin
    Self.FLeft := FLeft;
    Self.FRight := FRight;
    Self.FTop := FTop;
    Self.FBottom := FBottom;
    Self.FHeader := FHeader;
    Self.FFooter := FFooter;
    Self.FLeftTextIndent := FLeftTextIndent;
    Self.FRightTextIndent := FRightTextIndent;
    Self.FInternalMargin := FInternalMargin;
    Self.FMargin := FMargin;
    Self.FMirrorMargins := FMirrorMargins;
    Self.FUnitSystem := FUnitSystem;
  end
  else
    inherited;
end;

procedure TBCEditorPrintMargins.LoadFromStream(AStream: TStream);
begin
  with AStream do
  begin
    Read(FUnitSystem, SizeOf(FUnitSystem));
    Read(FLeft, SizeOf(FLeft));
    Read(FRight, SizeOf(FRight));
    Read(FTop, SizeOf(FTop));
    Read(FBottom, SizeOf(FBottom));
    Read(FHeader, SizeOf(FHeader));
    Read(FFooter, SizeOf(FFooter));
    Read(FLeftTextIndent, SizeOf(FLeftTextIndent));
    Read(FRightTextIndent, SizeOf(FRightTextIndent));
    Read(FInternalMargin, SizeOf(FInternalMargin));
    Read(FMargin, SizeOf(FMargin));
    Read(FMirrorMargins, SizeOf(FMirrorMargins));
  end;
end;

procedure TBCEditorPrintMargins.SaveToStream(AStream: TStream);
begin
  with AStream do
  begin
    Write(FUnitSystem, SizeOf(FUnitSystem));
    Write(FLeft, SizeOf(FLeft));
    Write(FRight, SizeOf(FRight));
    Write(FTop, SizeOf(FTop));
    Write(FBottom, SizeOf(FBottom));
    Write(FHeader, SizeOf(FHeader));
    Write(FFooter, SizeOf(FFooter));
    Write(FLeftTextIndent, SizeOf(FLeftTextIndent));
    Write(FRightTextIndent, SizeOf(FRightTextIndent));
    Write(FInternalMargin, SizeOf(FInternalMargin));
    Write(FMargin, SizeOf(FMargin));
    Write(FMirrorMargins, SizeOf(FMirrorMargins));
  end;
end;

end.

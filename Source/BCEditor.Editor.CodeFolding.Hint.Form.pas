unit BCEditor.Editor.CodeFolding.Hint.Form;

interface

uses
  Winapi.Messages, System.Classes, System.Types, Vcl.StdCtrls, Vcl.Forms, Vcl.Controls, Vcl.Graphics;

type
  TBCEditorCodeFoldingHintForm = class(TCustomForm)
  strict private
    FBackgroundColor: TColor;
    FBufferBitmap: TBitmap;
    FBorderColor: TColor;
    FCaseSensitive: Boolean;
    FEffectiveItemHeight: Integer;
    FFont: TFont;
    FFontHeight: Integer;
    FFormWidth: Integer;
    FHeightBuffer: Integer;
    FItemHeight: Integer;
    FItemList: TStrings;
    FMargin: Integer;
    FVisibleLines: Integer;
    procedure AdjustMetrics;
    procedure FontChange(Sender: TObject);
    procedure RecalcItemHeight;
    procedure SetFont(const Value: TFont);
    procedure SetItemHeight(const Value: Integer);
    procedure SetItemList(const Value: TStrings);
  protected
    procedure Activate; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Deactivate; override;
    procedure DoKeyPressW(Key: Char);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPressW(var Key: Char); virtual;
    procedure Paint; override;
    procedure WMEraseBackgrnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Execute(ACurrentString: string; X, Y: Integer);

    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor default clWindow;
    property BorderColor: TColor read FBorderColor write FBorderColor default clBtnFace;
    property Font: TFont read FFont write SetFont;
    property FormWidth: Integer read FFormWidth write FFormWidth; { Don't use the width because it triggers resizing }
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 0;
    property ItemList: TStrings read FItemList write SetItemList;
    property Margin: Integer read FMargin write FMargin default 2;
    property VisibleLines: Integer read FVisibleLines write FVisibleLines;
  end;

implementation

uses
  Winapi.Windows, System.SysUtils, System.UITypes, BCEditor.Editor.Base, BCEditor.Editor.KeyCommands, BCEditor.Utils,
  BCEditor.Editor.Utils, BCEditor.Consts, System.Math{$IFDEF USE_ALPHASKINS}, sSkinProvider, sMessages{$ENDIF};

{ TBCEditorCodeFoldingHintForm }

constructor TBCEditorCodeFoldingHintForm.Create(AOwner: TComponent);
{$IFDEF USE_ALPHASKINS}
var
  LSkinProvider: TsSkinProvider;
{$ENDIF}
begin
  CreateNew(AOwner);

  FBufferBitmap := Vcl.Graphics.TBitmap.Create;
  Visible := False;

  Color := FBackgroundColor;

  FItemList := TStringList.Create;

  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 8;

  FBackgroundColor := clWindow;
  FBorderColor := clBtnFace;

  FCaseSensitive := False;

  FormStyle := fsStayOnTop;

  FItemHeight := 0;
  FMargin := 2;
  FEffectiveItemHeight := 0;
  RecalcItemHeight;

  FHeightBuffer := 0;
  FFont.OnChange := FontChange;

  {$IFDEF USE_ALPHASKINS}
  LSkinProvider := TsSkinProvider(SendMessage(Handle, SM_ALPHACMD, MakeWParam(0, AC_GETPROVIDER), 0));
  if Assigned(LSkinProvider) then
  begin
    LSkinProvider.AllowExtBorders := False;
    LSkinProvider.DrawNonClientArea := False;
    LSkinProvider.DrawClientArea := False;
  end;
  {$ENDIF}
end;

destructor TBCEditorCodeFoldingHintForm.Destroy;
begin
  FBufferBitmap.Free;
  FItemList.Free;
  FFont.Free;

  inherited Destroy;
end;

procedure TBCEditorCodeFoldingHintForm.CreateParams(var Params: TCreateParams);
begin
  BorderStyle := bsNone;
  inherited;
  Params.Style := WS_POPUP or WS_CLIPSIBLINGS or WS_CLIPCHILDREN or WS_SYSMENU;

  with Params do
    if ((Win32Platform and VER_PLATFORM_WIN32_NT) <> 0) and (Win32MajorVersion > 4) and (Win32MinorVersion > 0) then
      WindowClass.Style := WindowClass.Style or CS_DROPSHADOW;
end;

procedure TBCEditorCodeFoldingHintForm.Activate;
begin
  Visible := True;
end;

procedure TBCEditorCodeFoldingHintForm.Deactivate;
begin
  Close;
end;

procedure TBCEditorCodeFoldingHintForm.KeyDown(var Key: Word; Shift: TShiftState);
var
  LChar: Char;
  LData: Pointer;
  LEditorCommand: TBCEditorCommand;
begin
  with Owner as TBCBaseEditor do
  begin
    LData := nil;
    LChar := BCEDITOR_NONE_CHAR;
    LEditorCommand := TranslateKeyCode(Key, Shift, LData);
    CommandProcessor(LEditorCommand, LChar, LData);
  end;
  Invalidate;
end;

procedure TBCEditorCodeFoldingHintForm.DoKeyPressW(Key: Char);
begin
  if Key <> BCEDITOR_NONE_CHAR then
    KeyPressW(Key);
end;

procedure TBCEditorCodeFoldingHintForm.KeyPressW(var Key: Char);
begin
  if Assigned(OnKeyPress) then
    OnKeyPress(Self, Key);
  Invalidate;
end;

procedure TBCEditorCodeFoldingHintForm.Paint;

  procedure ResetCanvas;
  begin
    with FBufferBitmap.Canvas do
    begin
      Pen.Color := FBackgroundColor;
      Brush.Color := FBackgroundColor;
      Font.Assign(FFont);
    end;
  end;

const
  TitleMargin = 2;
var
  TmpRect: TRect;
  i: Integer;
begin
  ResetCanvas;
  TmpRect := ClientRect;
  FBufferBitmap.Canvas.FillRect(TmpRect);
  FBufferBitmap.Canvas.Pen.Color := FBorderColor;
  FBufferBitmap.Canvas.Rectangle(TmpRect);

  for i := 0 to FItemList.Count - 1 do
    BCEditor.Utils.TextOut(FBufferBitmap.Canvas, FMargin + 1, FEffectiveItemHeight * i + FMargin, FItemList[i]);

  Canvas.Draw(0, 0, FBufferBitmap);
end;

procedure TBCEditorCodeFoldingHintForm.SetItemList(const Value: TStrings);
begin
  FItemList.Assign(Value);
end;

procedure TBCEditorCodeFoldingHintForm.SetItemHeight(const Value: Integer);
begin
  if Value <> FItemHeight then
  begin
    FItemHeight := Value;
    RecalcItemHeight;
  end;
end;

procedure TBCEditorCodeFoldingHintForm.RecalcItemHeight;
begin
  Canvas.Font.Assign(FFont);
  FFontHeight := TextHeight(Canvas, 'X');
  if FItemHeight > 0 then
    FEffectiveItemHeight := FItemHeight
  else
    FEffectiveItemHeight := FFontHeight;
end;

procedure TBCEditorCodeFoldingHintForm.WMEraseBackgrnd(var Message: TMessage);
begin
  message.Result := 1;
end;

procedure TBCEditorCodeFoldingHintForm.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  message.Result := message.Result or DLGC_WANTTAB;
end;

procedure TBCEditorCodeFoldingHintForm.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  RecalcItemHeight;
  AdjustMetrics;
end;

procedure TBCEditorCodeFoldingHintForm.FontChange(Sender: TObject);
begin
  RecalcItemHeight;
  AdjustMetrics;
end;

procedure TBCEditorCodeFoldingHintForm.Execute(ACurrentString: string; x, y: Integer);

  function GetWorkAreaWidth: Integer;
  begin
    Result := Screen.DesktopWidth;
  end;

  function GetWorkAreaHeight: Integer;
  begin
    Result := Screen.DesktopHeight;
  end;

  procedure RecalcFormPlacement;
  var
    i: Integer;
    LWidth: Integer;
    LHeight: Integer;
    LX: Integer;
    LY: Integer;
    LStr: string;
    LBorderWidth: Integer;
    LNewWidth: Integer;
  begin
    LX := X;
    LY := Y;
    LWidth := 0;

    LBorderWidth := 2;
    LHeight := FEffectiveItemHeight * ItemList.Count + LBorderWidth + 2 * Margin;

    Canvas.Font.Assign(Font);
    for i := 0 to ItemList.Count - 1 do
    begin
      LStr := ItemList[i];
      LNewWidth := Canvas.TextWidth(LStr);
      if LNewWidth > LWidth then
        LWidth := LNewWidth;
    end;

    Inc(LWidth, 2 * Margin + LBorderWidth + 4);

    if LX + LWidth > GetWorkAreaWidth then
    begin
      LX := GetWorkAreaWidth - LWidth - 5;
      if LX < 0 then
        LX := 0;
    end;

    if LY + LHeight > GetWorkAreaHeight then
    begin
      LY := LY - LHeight - (Owner as TBCBaseEditor).LineHeight - 2;
      if LY < 0 then
        LY := 0;
    end;

    Width := LWidth;
    Height := LHeight;
    Top := LY;
    Left := LX;
  end;

begin
  RecalcFormPlacement;
  AdjustMetrics;
  Show;
end;

procedure TBCEditorCodeFoldingHintForm.AdjustMetrics;
begin
  if (ClientWidth > 0) and (ClientHeight > 0) then
  begin
    FBufferBitmap.Width := ClientWidth;
    FBufferBitmap.Height := ClientHeight;
  end;
end;

end.
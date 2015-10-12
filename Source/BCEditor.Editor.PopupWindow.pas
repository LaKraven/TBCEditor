unit BCEditor.Editor.PopupWindow;

interface

uses
  Winapi.Messages, System.Classes, System.Types, Vcl.Controls;

type
  TCloseUpEvent = procedure(Sender: TObject; Accept: Boolean) of object;

  TBCEditorPopupWindow = class(TCustomControl)
  private
    FEditor: TWinControl;
    FCloseUp: TCloseUpEvent;
    FDropShadow: Boolean;
    procedure WMMouseActivate(var Msg: TMessage); message WM_MOUSEACTIVATE;
    procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;
  protected
    FActiveControl: TWinControl;
    FIsFocusable: Boolean;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure InvalidateEditor;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CloseUp(Accept: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function GetPopupText: string; virtual;
    procedure Hide;
    procedure Show(Origin: TPoint); virtual;
    property ActiveControl: TWinControl read FActiveControl;
    property DropShadow: Boolean read FDropShadow write FDropShadow default True;
    property IsFocusable: Boolean read FIsFocusable;
    property OnCloseUp: TCloseUpEvent read FCloseUp write FCloseUp;
  end;

implementation

uses
  Winapi.Windows, System.SysUtils;

constructor TBCEditorPopupWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEditor := AOwner as TWinControl;
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];

  if not (csDesigning in ComponentState) then
    ControlStyle := ControlStyle + [csAcceptsControls];

  Ctl3D := False;
  FDropShadow := True;
  ParentCtl3D := False;
  Parent := FEditor;
  Visible := False;
end;

procedure TBCEditorPopupWindow.CloseUp(Accept: Boolean);
begin
  if Assigned(FCloseUp) then
    FCloseUp(Self, Accept);
end;

procedure TBCEditorPopupWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP or WS_BORDER or WS_CLIPCHILDREN;
    ExStyle := WS_EX_TOOLWINDOW;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;

    if FDropShadow and CheckWin32Version(5, 1) then
      WindowClass.Style := WindowClass.Style or CS_DROPSHADOW;
  end;
end;

function TBCEditorPopupWindow.GetPopupText: string;
begin
  Result := '';
end;

procedure TBCEditorPopupWindow.Hide;
begin
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  Visible := False;
end;

procedure TBCEditorPopupWindow.InvalidateEditor;
var
  R: TRect;
begin
  R := FEditor.ClientRect;
  Winapi.Windows.InvalidateRect(FEditor.Handle, @R, False);
  UpdateWindow(FEditor.Handle);
end;

procedure TBCEditorPopupWindow.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
    CloseUp(PtInRect(ClientRect, Point(X, Y)));
end;

procedure TBCEditorPopupWindow.Show(Origin: TPoint);
begin
  SetBounds(Origin.X, Origin.Y, Width, Height);
  SetWindowPos(Handle, HWND_TOP, Origin.X, Origin.Y, 0, 0,
    SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);
  Visible := True;
end;

procedure TBCEditorPopupWindow.WMActivate(var Msg: TWMActivate);
begin
  inherited;
  if Msg.Active = WA_INACTIVE then
    CloseUp(False);
end;

procedure TBCEditorPopupWindow.WMMouseActivate(var Msg: TMessage);
begin
  if FIsFocusable then
    inherited
  else
    Msg.Result := MA_NOACTIVATE;
end;

end.

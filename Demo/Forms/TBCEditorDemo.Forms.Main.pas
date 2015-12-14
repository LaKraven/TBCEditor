unit TBCEditorDemo.Forms.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  BCCommon.Forms.Base, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, BCEditor.Editor, BCEditor.Highlighter,
  BCEditor.Editor.Base, BCCommon.Frames.Search, Vcl.Buttons, Vcl.AppEvnts, System.Actions, Vcl.ActnList, BCEditor.Print,
  BCCommon.Images, BCComponents.SkinProvider, BCComponents.SkinManager, BCControls.Panel, BCControls.StatusBar,
  BCComponents.TitleBar, Vcl.Menus, ToolCtrlsEh, DBGridEhToolCtrls, EhLibVCL, DBAxisGridsEh, ObjectInspectorEh,
  BCControls.Splitter, GridsEh, BCCommon.Frames.Base, sPanel, BCComponents.MultiStringHolder, sSkinManager, sStatusBar,
  sSplitter, acTitleBar, sSkinProvider, System.Win.TaskbarCore, Vcl.Taskbar, sDialogs, Vcl.StdCtrls, sButton,
  BCControls.Button, System.Diagnostics, BCCommon.Dialog.Popup.Highlighter, BCCommon.Dialog.Popup.Highlighter.Color;

const
  BCEDITORDEMO_CAPTION = 'TBCEditor Control Demo v1.1';
  TITLE_BAR_HIGHLIGHTER = 2;
  TITLE_BAR_COLORS = 4;

type
  TMainForm = class(TBCBaseForm)
    ActionFileOpen: TAction;
    ActionPreview: TAction;
    ActionSearch: TAction;
    Editor: TBCEditor;
    MenuItemExit: TMenuItem;
    MenuItemFileOpen: TMenuItem;
    MenuItemPrintPreview: TMenuItem;
    MenuItemSeparator1: TMenuItem;
    MenuItemSeparator2: TMenuItem;
    MultiStringHolderFileTypes: TBCMultiStringHolder;
    ObjectInspectorEh: TObjectInspectorEh;
    PanelLeft: TBCPanel;
    PanelProperty: TBCPanel;
    PopupMenuFile: TPopupMenu;
    PopupMenuDummy: TPopupMenu;
    Splitter: TBCSplitter;
    OpenDialog: TsOpenDialog;
    SearchFrame: TBCSearchFrame;
    MenuItemSkins: TMenuItem;
    ActionSkins: TAction;
    procedure ActionFileOpenExecute(Sender: TObject);
    procedure ActionPreviewExecute(Sender: TObject);
    procedure ActionSearchExecute(Sender: TObject);
    procedure ApplicationEventsMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EditorCaretChanged(Sender: TObject; X, Y: Integer);
    procedure ActionSkinsExecute(Sender: TObject);
    procedure SelectedHighlighterClick(AHighlighterName: string);
    procedure SelectedHighlighterColorClick(AHighlighterColorName: string);
    procedure TitleBarItems2Click(Sender: TObject);
    procedure TitleBarItems4Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FStopWatch: TStopWatch;
    FPopupHighlighterDialog: TPopupHighlighterDialog;
    FPopupHighlighterColorDialog: TPopupHighlighterColorDialog;
    FHighlighterStrings: TStringList;
    FHighlighterColorStrings: TStringList;
    function GetTitleBarItemLeftBottom(AIndex: Integer): TPoint;
    procedure InitializeEditorPrint(EditorPrint: TBCEditorPrint);
    procedure LockFormPaint;
    procedure PrintPreview;
    procedure UnlockFormPaint;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  BCCommon.Language.Strings, BCCommon.Forms.Print.Preview, BCEditor.Print.Types, BCCommon.StringUtils,
  BCCommon.Dialogs.SkinSelect, BCCommon.FileUtils;

procedure TMainForm.ActionSkinsExecute(Sender: TObject);
begin
  inherited;
  TSkinSelectDialog.ClassShowModal(SkinManager);
end;

procedure TMainForm.ApplicationEventsMessage(var Msg: tagMSG; var Handled: Boolean);
var
  InfoText: string;
  KeyState: TKeyboardState;
begin
  SearchFrame.Visible := Editor.Search.Enabled;
  if SearchFrame.Visible then
    Editor.Margins.Bottom := 0
  else
    Editor.Margins.Bottom := 5;
  if Editor.Modified then
    InfoText := LanguageDataModule.GetConstant('Modified')
  else
    InfoText := '';
  if StatusBar.Panels[2].Text <> InfoText then
    StatusBar.Panels[2].Text := InfoText;
  GetKeyboardState(KeyState);
  if KeyState[VK_INSERT] = 0 then
    if StatusBar.Panels[1].Text <> LanguageDataModule.GetConstant('Insert') then
      StatusBar.Panels[1].Text := LanguageDataModule.GetConstant('Insert');
  if KeyState[VK_INSERT] = 1 then
    if StatusBar.Panels[1].Text <> LanguageDataModule.GetConstant('Overwrite') then
      StatusBar.Panels[1].Text := LanguageDataModule.GetConstant('Overwrite');
end;

procedure TMainForm.EditorCaretChanged(Sender: TObject; X, Y: Integer);
var
  InfoText: string;
begin
  inherited;
  InfoText := Format('%d: %d', [Y, X]);
  if StatusBar.Panels[0].Text <> InfoText then
    StatusBar.Panels[0].Text := InfoText;
end;

procedure TMainForm.InitializeEditorPrint(EditorPrint: TBCEditorPrint);
var
  Alignment: TAlignment;

  procedure SetHeaderFooter(Option: Integer; Value: string);
  begin
    case Option of
      0, 1:
        with EditorPrint.Footer do
        begin
          case Option of
            0:
              Alignment := taLeftJustify;
            1:
              Alignment := taRightJustify;
          end;
          Add(Value, nil, Alignment, 1);
        end;
      2, 3:
        with EditorPrint.Header do
        begin
          case Option of
            2:
              Alignment := taLeftJustify;
            3:
              Alignment := taRightJustify;
          end;
          Add(Value, nil, Alignment, 1);
        end;
    end;
  end;

begin
  EditorPrint.Header.Clear;
  EditorPrint.Footer.Clear;

  SetHeaderFooter(0, Format(LanguageDataModule.GetConstant('PrintedBy'), [Application.Title]));
  SetHeaderFooter(1, LanguageDataModule.GetConstant('PreviewDocumentPage'));
  SetHeaderFooter(2, Editor.DocumentName);
  SetHeaderFooter(3, '$DATE$ $TIME$');

  EditorPrint.Header.FrameTypes := [ftLine];
  EditorPrint.Footer.FrameTypes := [ftLine];
  EditorPrint.LineNumbersInMargin := True;
  EditorPrint.LineNumbers := True;
  EditorPrint.Wrap := False;
  EditorPrint.Colors := True;

  EditorPrint.Editor := Editor;
  EditorPrint.Title := Editor.DocumentName;
end;

procedure TMainForm.ActionPreviewExecute(Sender: TObject);
begin
  PrintPreview
end;

procedure TMainForm.PrintPreview;
begin
  with PrintPreviewDialog do
  begin
    InitializeEditorPrint(PrintPreview.EditorPrint);
    ShowModal;
  end;
end;

procedure TMainForm.ActionFileOpenExecute(Sender: TObject);
var
  LFileName: string;
begin
  OpenDialog.Title := 'Open';
  if OpenDialog.Execute(Handle) then
  begin
    FStopWatch.Reset;
    FStopWatch.Start;
    LFileName := OpenDialog.Files[0];
    TitleBar.Items[1].Caption := Format('%s - %s', [BCEDITORDEMO_CAPTION, LFileName]);
    Editor.LoadFromFile(LFileName);
    FStopWatch.Stop;
    StatusBar.Panels[3].Text := 'Load: ' + FormatDateTime('s.zzz "s"', FStopWatch.ElapsedMilliseconds / MSecsPerDay);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  inherited;

  { IDE can lose these properties }
  SearchFrame.SpeedButtonFindPrevious.Images := ImagesDataModule.ImageListSmall;
  SearchFrame.SpeedButtonFindNext.Images := ImagesDataModule.ImageListSmall;
  SearchFrame.SpeedButtonOptions.Images := ImagesDataModule.ImageListSmall;
  PopupMenuFile.Images := ImagesDataModule.ImageList;
  TitleBar.Images := ImagesDataModule.ImageListSmall;

  FHighlighterStrings := GetHighlighters;
  FHighlighterColorStrings := GetHighlighterColors;

  SelectedHighlighterClick('Object Pascal');
  SelectedHighlighterColorClick('Default');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FHighlighterStrings.Free;
  FHighlighterColorStrings.Free;

  inherited;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  ObjectInspectorEh.Component := Editor;
  ObjectInspectorEh.LabelColWidth := 145;

  SearchFrame.Editor := Editor;
end;

function TMainForm.GetTitleBarItemLeftBottom(AIndex: Integer): TPoint;
var
  LRect: TRect;
begin
  Result.X := TitleBar.Items[AIndex].Rect.Left;
  Result.Y := TitleBar.Items[AIndex].Rect.Bottom;

  if Assigned(TitleBar.Items[AIndex].ExtForm) then
  begin
    Inc(Result.X, TitleBar.Items[AIndex].ExtForm.Left);
    Inc(Result.Y, TitleBar.Items[AIndex].ExtForm.Top);
  end
  else
  begin
    GetWindowRect(Handle, LRect);
    Inc(Result.Y, LRect.Top);
    Inc(Result.X, LRect.Left);
  end;
end;

procedure TMainForm.TitleBarItems2Click(Sender: TObject);
var
  LPoint: TPoint;
begin
  inherited;

  if Assigned(FPopupHighlighterDialog) then
  begin
    FPopupHighlighterDialog.Visible := False;
    FPopupHighlighterDialog := nil;
  end
  else
  begin
    FPopupHighlighterDialog := TPopupHighlighterDialog.Create(Self);
    FPopupHighlighterDialog.PopupParent := Self;
    FPopupHighlighterDialog.OnSelectHighlighter := SelectedHighlighterClick;

    LPoint := GetTitleBarItemLeftBottom(TITLE_BAR_HIGHLIGHTER);

    FPopupHighlighterDialog.Left := LPoint.X;
    FPopupHighlighterDialog.Top := LPoint.Y;

    LockFormPaint;

    FPopupHighlighterDialog.Execute(FHighlighterStrings, TitleBar.Items[TITLE_BAR_HIGHLIGHTER].Caption);

    UnlockFormPaint;

    while Assigned(FPopupHighlighterDialog) and FPopupHighlighterDialog.Visible do
      Application.HandleMessage;
    FPopupHighlighterDialog := nil;
  end;
end;

procedure TMainForm.TitleBarItems4Click(Sender: TObject);
var
  LPoint: TPoint;
begin
  inherited;
  if Assigned(FPopupHighlighterColorDialog) then
  begin
    FPopupHighlighterColorDialog.Visible := False;
    FPopupHighlighterColorDialog := nil;
  end
  else
  begin
    FPopupHighlighterColorDialog := TPopupHighlighterColorDialog.Create(Self);
    FPopupHighlighterColorDialog.PopupParent := Self;
    FPopupHighlighterColorDialog.OnSelectHighlighterColor := SelectedHighlighterColorClick;

    LPoint := GetTitleBarItemLeftBottom(TITLE_BAR_COLORS);

    FPopupHighlighterColorDialog.Left := LPoint.X;
    FPopupHighlighterColorDialog.Top := LPoint.Y;

    LockFormPaint;

    FPopupHighlighterColorDialog.Execute(FHighlighterColorStrings, TitleBar.Items[TITLE_BAR_COLORS].Caption);

    UnlockFormPaint;

    while Assigned(FPopupHighlighterColorDialog) and FPopupHighlighterColorDialog.Visible do
      Application.HandleMessage;
    FPopupHighlighterColorDialog := nil;
  end;
end;

procedure TMainForm.ActionSearchExecute(Sender: TObject);
begin
  Editor.Search.Enabled := True;
  Application.ProcessMessages; { search frame visible }
  SearchFrame.ComboBoxSearchText.SetFocus;
end;

procedure TMainForm.SelectedHighlighterClick(AHighlighterName: string);
begin
  with Editor do
  begin
    Highlighter.LoadFromFile(Format('%s.json', [AHighlighterName]));
    CodeFolding.Visible := Highlighter.CodeFoldingRangeCount > 0;
  end;
  TitleBar.Items[TITLE_BAR_HIGHLIGHTER].Caption := Editor.Highlighter.Name;

  Editor.Lines.Text := Editor.Highlighter.Info.General.Sample;
  Editor.CaretZero;
  StatusBar.Panels[3].Text := '';
  Caption := BCEDITORDEMO_CAPTION;
  SearchFrame.ClearText;

  if Assigned(FPopupHighlighterDialog) then
  begin
    FPopupHighlighterDialog.Visible := False;
    FPopupHighlighterDialog := nil;
  end;
end;

procedure TMainForm.SelectedHighlighterColorClick(AHighlighterColorName: string);
begin
  with Editor do
  begin
    Highlighter.Colors.LoadFromFile(Format('%s.json', [AHighlighterColorName]));
    Invalidate;
  end;
  TitleBar.Items[TITLE_BAR_COLORS].Caption := Editor.Highlighter.Colors.Name;
  if Assigned(FPopupHighlighterDialog) then
  begin
    FPopupHighlighterColorDialog.Visible := False;
    FPopupHighlighterColorDialog := nil;
  end;
end;

procedure TMainForm.LockFormPaint;
begin
  SkinProvider.SkinData.BeginUpdate;
  SkinProvider.Form.Perform(WM_SETREDRAW, 0, 0);
end;

procedure TMainForm.UnlockFormPaint;
begin
  SkinProvider.SkinData.EndUpdate;
  SkinProvider.Form.Perform(WM_SETREDRAW, 1, 0);
end;

end.

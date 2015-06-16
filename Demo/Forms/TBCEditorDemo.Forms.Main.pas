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
  BCControls.Button, System.Diagnostics;

const
  BCEDITORDEMO_CAPTION = 'TBCEditor Control Demo v1.0b';

type
  TMainForm = class(TBCForm)
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
    PopupMenuColors: TPopupMenu;
    PopupMenuFile: TPopupMenu;
    PopupMenuHighlighters: TPopupMenu;
    PopupMenuSkins: TPopupMenu;
    Splitter: TBCSplitter;
    OpenDialog: TsOpenDialog;
    SearchFrame: TBCSearchFrame;
    procedure ActionFileOpenExecute(Sender: TObject);
    procedure ActionPreviewExecute(Sender: TObject);
    procedure ActionSearchExecute(Sender: TObject);
    procedure ActionSelectHighlighterColorExecute(Sender: TObject);
    procedure ActionSelectHighlighterExecute(Sender: TObject);
    procedure ApplicationEventsMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SkinManagerGetMenuExtraLineData(FirstItem: TMenuItem; var SkinSection, Caption: string; var Glyph: TBitmap; var LineVisible: Boolean);
    procedure EditorCaretChanged(Sender: TObject; X, Y: Integer);
  private
    FStopWatch: TStopWatch;
    procedure InitializeEditorPrint(EditorPrint: TBCEditorPrint);
    procedure PrintPreview;
    procedure SetHighlighterColors;
    procedure SetHighlighters;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  BCCommon.Language.Strings, BCCommon.Forms.Print.Preview, BCEditor.Print.Types, BCCommon.StringUtils;

procedure TMainForm.ActionSelectHighlighterExecute(Sender: TObject);
begin
  with Editor do
  begin
    Highlighter.LoadFromFile(Format('%s.json', [TAction(Sender).Caption]));
    ClearCodeFolding;
    Lines.Text := Highlighter.Info.General.Sample;
    InitCodeFolding;
    SetFocus;
  end;
  StatusBar.Panels[3].Text := '';
  TitleBar.Items[4].Caption := TAction(Sender).Caption;
  Caption := BCEDITORDEMO_CAPTION;
  SearchFrame.ClearText;
end;

procedure TMainForm.ActionSelectHighlighterColorExecute(Sender: TObject);
begin
  Editor.Highlighter.Colors.LoadFromFile(Format('%s.json', [TAction(Sender).Caption]));
  TitleBar.Items[6].Caption := TAction(Sender).Caption;
  Editor.SetFocus;
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
    if StatusBar.Panels[1].Text <> LanguageDataModule.GetConstant('Overwrite')
    then
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
  i: Integer;
  FileName, Ext, ItemString, Token: string;
begin
  OpenDialog.Title := 'Open';
  if OpenDialog.Execute(Handle) then
  begin
    FStopWatch.Reset;
    FStopWatch.Start;
    FileName := OpenDialog.Files[0];
    Ext := LowerCase(ExtractFileExt(FileName));

    for i := 0 to MultiStringHolderFileTypes.MultipleStrings.Count - 1 do
    begin
      ItemString := MultiStringHolderFileTypes.MultipleStrings.Items[i].Strings.Text;
      while ItemString <> '' do
      begin
        Token := GetNextToken(';', ItemString);
        ItemString := RemoveTokenFromStart(';', ItemString);
        if Ext = Token then
        begin
          PopupMenuHighlighters.Items.Find(MultiStringHolderFileTypes.MultipleStrings.Items[i].Name).Action.Execute;
          Break;
        end;
      end;
    end;
    TitleBar.Items[3].Caption := Format('%s - %s', [BCEDITORDEMO_CAPTION, FileName]);
    Editor.LoadFromFile(FileName);
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

  CreateSkinsMenu(PopupMenuSkins.Items);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  ObjectInspectorEh.Component := Editor;
  ObjectInspectorEh.LabelColWidth := 145;

  SearchFrame.Editor := Editor;
  SetHighlighters;
  SetHighlighterColors;
end;

procedure TMainForm.SetHighlighters;
var
  FindFileHandle: THandle;
  Win32FindData: TWin32FindData;
  FileName: string;
  LMenuItem: TMenuItem;
  LAction: TAction;
begin
  PopupMenuHighlighters.Items.Clear;
{$WARNINGS OFF}
  FindFileHandle := FindFirstFile(PChar(IncludeTrailingBackSlash(ExtractFilePath(Application.ExeName)) +
    'Highlighters\*.json'), Win32FindData);
{$WARNINGS ON}
  if FindFileHandle <> INVALID_HANDLE_VALUE then
  try
    //i := 1;
    repeat
      FileName := ExtractFileName(StrPas(Win32FindData.cFileName));
      FileName := Copy(FileName, 1, Pos('.', FileName) - 1);

      LAction := TAction.Create(Self);
      LAction.Caption := FileName;
      LAction.OnExecute := ActionSelectHighlighterExecute;
      LMenuItem := TMenuItem.Create(PopupMenuHighlighters);
      LMenuItem.Action := LAction;
      LMenuItem.RadioItem := True;
      LMenuItem.AutoCheck := True;
      PopupMenuHighlighters.Items.Add(LMenuItem);
      if LAction.Caption = 'Object Pascal' then
      begin
        LAction.Checked := True;
        LAction.Execute;
      end;
    until not FindNextFile(FindFileHandle, Win32FindData);
  finally
    Winapi.Windows.FindClose(FindFileHandle);
  end;
end;

procedure TMainForm.SetHighlighterColors;
var
  FindFileHandle: THandle;
  Win32FindData: TWin32FindData;
  FileName: string;
  LMenuItem: TMenuItem;
  LAction: TAction;
begin
  PopupMenuColors.Items.Clear;
{$WARNINGS OFF}
  FindFileHandle := FindFirstFile
    (PChar(IncludeTrailingBackSlash(ExtractFilePath(Application.ExeName)) +
    'Colors\*.json'), Win32FindData);
{$WARNINGS ON}
  if FindFileHandle <> INVALID_HANDLE_VALUE then
  try
    repeat
      FileName := ExtractFileName(StrPas(Win32FindData.cFileName));
      FileName := Copy(FileName, 1, Pos('.', FileName) - 1);
      LAction := TAction.Create(Self);
      LAction.Caption := FileName;
      LAction.OnExecute := ActionSelectHighlighterColorExecute;
      LMenuItem := TMenuItem.Create(PopupMenuColors);
      LMenuItem.Action := LAction;
      LMenuItem.RadioItem := True;
      LMenuItem.AutoCheck := True;
      PopupMenuColors.Items.Add(LMenuItem);
      if LAction.Caption = 'Default' then
      begin
        LAction.Checked := True;
        LAction.Execute;
      end;
    until not FindNextFile(FindFileHandle, Win32FindData);
  finally
    Winapi.Windows.FindClose(FindFileHandle);
  end;
end;

procedure TMainForm.SkinManagerGetMenuExtraLineData(FirstItem: TMenuItem; var SkinSection, Caption: string;
  var Glyph: TBitmap; var LineVisible: Boolean);
begin
  inherited;
  if FirstItem = PopupMenuSkins.Items[0] then
  begin
    LineVisible := True;
    Caption := 'Skin';
  end
  else
  if FirstItem = PopupMenuHighlighters.Items[0] then
  begin
    LineVisible := True;
    Caption := 'Highlighter';
  end
  else
  if FirstItem = PopupMenuColors.Items[0] then
  begin
    LineVisible := True;
    Caption := 'Color';
  end
  else
    LineVisible := False;
end;

procedure TMainForm.ActionSearchExecute(Sender: TObject);
begin
  Editor.Search.Enabled := True;
  Application.ProcessMessages; { search frame visible }
  SearchFrame.ComboBoxSearchText.SetFocus;
end;

end.

unit BCEditor.Editor.DB;

interface

uses
  Winapi.Messages, System.SysUtils, System.Classes, Vcl.Controls, Vcl.DbCtrls, BCEditor.Editor,
  BCEditor.Editor.KeyCommands, Data.DB;

type
  TBCCustomDBEditor = class(TBCCustomEditor)
  strict private
    FDataLink: TFieldDataLink;
    FEditing: boolean;
    FBeginEdit: boolean;
    FLoadData: TNotifyEvent;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetEditing(Value: boolean);
    procedure UpdateData(Sender: TObject);
    procedure CMEnter(var Msg: TCMEnter); message CM_ENTER;
    procedure CMExit(var Msg: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
  protected
    function GetReadOnly: boolean; override;
    procedure Loaded; override;
    procedure DoChange; override;
    procedure SetReadOnly(Value: boolean); override;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Field: TField read GetField;
    property OnLoadData: TNotifyEvent read FLoadData write FLoadData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure ExecuteCommand(Command: TBCEditorCommand; AChar: Char; Data: pointer); override;
    procedure LoadMemo;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

  TBCDBEditor = class(TBCCustomDBEditor)
  published
    property ActiveLine;
    property Align;
    property Anchors;
    property BackgroundColor;
    property BorderStyle;
    property OnBookmarkPanelAfterPaint;
    property OnBookmarkPanelBeforePaint;
    property OnBookmarkPanelLinePaint;
    property Caret;
    property CodeFolding;
    property CompletionProposal;
    property Constraints;
    property Ctl3D;
    property DataField;
    property DataSource;
    property Directories;
    property Enabled;
    property Field;
    property Font;
    property Height;
    property ImeMode;
    property ImeName;
    property InsertMode;
    property KeyCommands;
    property LeftMargin;
    property LineSpacing;
    property MatchingPair;
    property Minimap;
    property Name;
    property OnChange;
    property OnClearBookmark;
    property OnClick;
    property OnCommandProcessed;
    property OnContextHelp;
    property OnCustomLineColors;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropFiles;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLeftMarginClick;
    property OnLoadData;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnPlaceBookmark;
    property OnProcessCommand;
    property OnProcessUserCommand;
    property OnReplaceText;
    property OnRightMarginMouseUp;
    property OnScroll;
    property OnStartDock;
    property OnStartDrag;
    property Options;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property Replace;
    property RightMargin;
    property Scroll;
    property Search;
    property Selection;
    property SpecialChars;
    property ShowHint;
    property TabOrder;
    property Tabs;
    property TabStop;
    property Tag;
    property Undo;
    property WantReturns;
    property Width;
    property Visible;
    property WordWrap;
  end;

implementation

uses
  BCEditor.Consts;

constructor TBCCustomDBEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TBCCustomDBEditor.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TBCCustomDBEditor.CMEnter(var Msg: TCMEnter);
begin
  SetEditing(True);
  inherited;
end;

procedure TBCCustomDBEditor.CMExit(var Msg: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  SetEditing(False);
  inherited;
end;

procedure TBCCustomDBEditor.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := Integer(FDataLink);
end;

procedure TBCCustomDBEditor.DataChange(Sender: TObject);
begin
  if Assigned(FDataLink.Field) then
  begin
    if FBeginEdit then
    begin
      FBeginEdit := False;
      Exit;
    end;
    if FDataLink.Field.IsBlob then
      LoadMemo
    else
      Text := FDataLink.Field.Text;
    if Assigned(FLoadData) then
      FLoadData(Self);
  end
  else
  begin
    if csDesigning in ComponentState then
      Text := name
    else
      Text := '';
  end;
end;

procedure TBCCustomDBEditor.DragDrop(Source: TObject; X, Y: Integer);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TBCCustomDBEditor.EditingChange(Sender: TObject);
begin
  if FDataLink.Editing then
  begin
    if Assigned(FDataLink.DataSource) and (FDataLink.DataSource.State <> dsInsert) then
      FBeginEdit := True;
  end;
end;

procedure TBCCustomDBEditor.ExecuteCommand(Command: TBCEditorCommand; AChar: Char; Data: pointer);
begin
  if (Command = ecChar) and (AChar = BCEDITOR_ESCAPE) then
    FDataLink.Reset
  else
  if (Command >= ecEditCommandFirst) and (Command <= ecEditCommandLast) then
    if not FDataLink.Edit then
      Exit;

  inherited;
end;

function TBCCustomDBEditor.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TBCCustomDBEditor.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TBCCustomDBEditor.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TBCCustomDBEditor.GetReadOnly: boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TBCCustomDBEditor.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    DataChange(Self);
end;

procedure TBCCustomDBEditor.LoadMemo;
var
  BlobStream: TStream;
begin
  try
    BlobStream := FDataLink.DataSet.CreateBlobStream(FDataLink.Field, bmRead);
    Lines.BeginUpdate;
    Lines.LoadFromStream(BlobStream, TEncoding.Default);
    Lines.EndUpdate;
    BlobStream.Free;
    Modified := False;
    ClearUndo;
  except
    // Memo too large
    on E: EInvalidOperation do
      Lines.Text := Format('(%s)', [E.Message]);
  end;
  EditingChange(Self);
end;

procedure TBCCustomDBEditor.DoChange;
begin
  FDataLink.Modified;
  inherited;
end;

procedure TBCCustomDBEditor.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and Assigned(FDataLink) and (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TBCCustomDBEditor.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TBCCustomDBEditor.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Assigned(Value) then
    Value.FreeNotification(Self);
end;

procedure TBCCustomDBEditor.SetEditing(Value: boolean);
begin
  if FEditing <> Value then
  begin
    FEditing := Value;
    if not Assigned(FDataLink.Field) or not FDataLink.Field.IsBlob then
      FDataLink.Reset;
  end;
end;

procedure TBCCustomDBEditor.SetReadOnly(Value: boolean);
begin
  FDataLink.ReadOnly := Value;
end;

procedure TBCCustomDBEditor.UpdateData(Sender: TObject);
var
  BlobStream: TStream;
begin
  if FDataLink.Field.IsBlob then
  begin
    BlobStream := FDataLink.DataSet.CreateBlobStream(FDataLink.Field, bmWrite);
    Lines.SaveToStream(BlobStream);
    BlobStream.Free;
  end
  else
    FDataLink.Field.AsString := Text;
end;

end.

unit BCEditor.MacroRecorder;

interface

uses
  Winapi.Windows, System.WideStrUtils, System.Classes, Vcl.Controls, Vcl.Graphics, BCEditor.Language,
  Vcl.Menus, BCEditor.Editor.Base, BCEditor.Editor.KeyCommands, BCEditor.Types;

type
  TBCEditorMacroState = (msStopped, msRecording, msPlaying, msPaused);

  TBCEditorMacroEvent = class(TObject)
  protected
    FRepeatCount: Byte;
    function GetAsString: string; virtual; abstract;
    procedure InitEventParameters(AString: string); virtual; abstract;
  public
    constructor Create; virtual;
    procedure Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer); virtual; abstract;

    procedure LoadFromStream(AStream: TStream); virtual; abstract;
    procedure SaveToStream(AStream: TStream); virtual; abstract;
    procedure Playback(AEditor: TBCBaseEditor); virtual; abstract;
    property AsString: string read GetAsString;
    property RepeatCount: Byte read FRepeatCount write FRepeatCount;
  end;

  TBCEditorBasicEvent = class(TBCEditorMacroEvent)
  protected
    FCommand: TBCEditorCommand;
    function GetAsString: string; override;
    procedure InitEventParameters(AString: string); override;
  public
    procedure Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToStream(AStream: TStream); override;
    procedure Playback(AEditor: TBCBaseEditor); override;
  public
    property Command: TBCEditorCommand read FCommand write FCommand;
  end;

  TBCEditorCharEvent = class(TBCEditorMacroEvent)
  protected
    FKey: Char;
    function GetAsString: string; override;
    procedure InitEventParameters(AString: string); override;
  public
    procedure Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToStream(AStream: TStream); override;
    procedure Playback(AEditor: TBCBaseEditor); override;
  public
    property Key: Char read FKey write FKey;
  end;

  TBCEditorStringEvent = class(TBCEditorMacroEvent)
  protected
    FString: string;
    function GetAsString: string; override;
    procedure InitEventParameters(AString: string); override;
  public
    procedure Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToStream(AStream: TStream); override;
    procedure Playback(AEditor: TBCBaseEditor); override;
  public
    property Value: string read FString write FString;
  end;

  TBCEditorPositionEvent = class(TBCEditorBasicEvent)
  protected
    FPosition: TBCEditorTextPosition;
    function GetAsString: string; override;
    procedure InitEventParameters(AString: string); override;
  public
    procedure Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToStream(AStream: TStream); override;
    procedure Playback(AEditor: TBCBaseEditor); override;
  public
    property Position: TBCEditorTextPosition read FPosition write FPosition;
  end;

  TBCEditorDataEvent = class(TBCEditorBasicEvent)
  protected
    FData: Pointer;
  public
    procedure Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToStream(AStream: TStream); override;
    procedure Playback(AEditor: TBCBaseEditor); override;
  end;

  TBCBaseEditorMacroRecorder = class;

  TBCEditorUserCommandEvent = procedure(aSender: TBCBaseEditorMacroRecorder; ACommand: TBCEditorCommand;
    var AEvent: TBCEditorMacroEvent) of object;

  TBCBaseEditorMacroRecorder = class(TComponent)
  strict private
    FPlaybackShortCut: TShortCut;
    FRecordShortCut: TShortCut;
    FOnStateChange: TNotifyEvent;
    FOnUserCommand: TBCEditorUserCommandEvent;
    FMacroName: string;
    FSaveMarkerPos: Boolean;
    function GetEvent(AIndex: Integer): TBCEditorMacroEvent;
    function GetEventCount: Integer;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetEditors(AIndex: Integer): TBCBaseEditor;
    procedure SetEditor(const Value: TBCBaseEditor);
    function GetEditor: TBCBaseEditor;
    function GetEditorCount: Integer;
  protected
    FEditors: TList;
    FCurrentEditor: TBCBaseEditor;
    FState: TBCEditorMacroState;
    FEvents: TList;
    FRecordCommandID: TBCEditorCommand;
    FPlaybackCommandID: TBCEditorCommand;
    procedure Notification(aComponent: TComponent; aOperation: TOperation); override;
    procedure SetRecordShortCut(const Value: TShortCut);
    procedure SetPlaybackShortCut(const Value: TShortCut);
    function GetIsEmpty: Boolean;
    procedure StateChanged;
    procedure DoAddEditor(AEditor: TBCBaseEditor);
    procedure DoRemoveEditor(AEditor: TBCBaseEditor);
    procedure OnCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean; var Command: TBCEditorCommand;
      var AChar: Char; Data: Pointer; HandlerData: Pointer);
    function CreateMacroEvent(ACommand: TBCEditorCommand): TBCEditorMacroEvent;
  protected
    property RecordCommandID: TBCEditorCommand read FRecordCommandID;
    property PlaybackCommandID: TBCEditorCommand read FPlaybackCommandID;
    procedure HookEditor(AEditor: TBCBaseEditor; ACommandID: TBCEditorCommand; AOldShortCut, ANewShortCut: TShortCut);
    procedure UnHookEditor(AEditor: TBCBaseEditor; ACommandID: TBCEditorCommand; AShortCut: TShortCut);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Error(const AMessage: string);
    function AddEditor(AEditor: TBCBaseEditor): Integer;
    function RemoveEditor(AEditor: TBCBaseEditor): Integer;
    procedure RecordMacro(AEditor: TBCBaseEditor);
    procedure PlaybackMacro(AEditor: TBCBaseEditor);
    procedure Stop;
    procedure Pause;
    procedure Resume;
    property IsEmpty: Boolean read GetIsEmpty;
    property State: TBCEditorMacroState read FState;
    procedure Clear;
    procedure AddEvent(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer);
    procedure InsertEvent(AIndex: Integer; ACommand: TBCEditorCommand; AChar: Char; AData: Pointer);
    procedure AddCustomEvent(AEvent: TBCEditorMacroEvent);
    procedure InsertCustomEvent(AIndex: Integer; AEvent: TBCEditorMacroEvent);
    procedure DeleteEvent(AIndex: Integer);
    procedure LoadFromStream(ASource: TStream; AClear: Boolean = True);
    procedure SaveToStream(ADestination: TStream);
    procedure LoadFromFile(AFilename: string);
    procedure SaveToFile(AFilename: string);
    property Editors[AIndex: Integer]: TBCBaseEditor read GetEditors;
    property EditorCount: Integer read GetEditorCount;
    property EventCount: Integer read GetEventCount;
    property Events[AIndex: Integer]: TBCEditorMacroEvent read GetEvent;
    property RecordShortCut: TShortCut read FRecordShortCut write SetRecordShortCut;
    property PlaybackShortCut: TShortCut read FPlaybackShortCut write SetPlaybackShortCut;
    property SaveMarkerPos: Boolean read FSaveMarkerPos write FSaveMarkerPos default False;
    property AsString: string read GetAsString write SetAsString;
    property MacroName: string read FMacroName write FMacroName;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property OnUserCommand: TBCEditorUserCommandEvent read FOnUserCommand write FOnUserCommand;
  published
    property Editor: TBCBaseEditor read GetEditor write SetEditor;
  end;

  TBCEditorMacroRecorder = class(TBCBaseEditorMacroRecorder)
  published
    property SaveMarkerPos;
    property RecordShortCut;
    property PlaybackShortCut;
    property OnStateChange;
    property OnUserCommand;
  end;

implementation

uses
  Vcl.Forms, System.SysUtils, BCEditor.Editor.Utils, BCEditor.Consts, System.Types;

{ TBCEditorDatAEvent }

procedure TBCEditorDataEvent.Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer);
begin
  FCommand := ACommand;
  Assert(AChar = BCEDITOR_NONE_CHAR);
  FData := AData;
end;

procedure TBCEditorDataEvent.LoadFromStream(AStream: TStream);
begin
  AStream.Read(FData, SizeOf(FData));
end;

procedure TBCEditorDataEvent.Playback(AEditor: TBCBaseEditor);
begin
  AEditor.CommandProcessor(Command, BCEDITOR_NONE_CHAR, FData);
end;

procedure TBCEditorDataEvent.SaveToStream(AStream: TStream);
begin
  inherited;
  AStream.Write(FData, SizeOf(FData));
end;

{ TBCBaseEditorMacroRecorder }

procedure TBCBaseEditorMacroRecorder.AddCustomEvent(AEvent: TBCEditorMacroEvent);
begin
  InsertCustomEvent(EventCount, AEvent);
end;

function TBCBaseEditorMacroRecorder.AddEditor(AEditor: TBCBaseEditor): Integer;
begin
  if not Assigned(FEditors) then
    FEditors := TList.Create
  else
  if FEditors.IndexOf(AEditor) >= 0 then
  begin
    Result := -1;
    Exit;
  end;
  AEditor.FreeNotification(Self);
  Result := FEditors.Add(AEditor);
  DoAddEditor(AEditor);
end;

procedure TBCBaseEditorMacroRecorder.AddEvent(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer);
begin
  InsertEvent(EventCount, ACommand, AChar, AData);
end;

procedure TBCBaseEditorMacroRecorder.Clear;
var
  I: Integer;
  LObject: TObject;
begin
  if Assigned(FEvents) then
  begin
    for I := FEvents.Count - 1 downto 0 do
    begin
      LObject := FEvents[I];
      FEvents.Delete(I);
      LObject.Free;
    end;
    FEvents.Free;
    FEvents := nil;
  end;
end;

const
  ecPluginBase = 64000;

var
  GCurrentCommand: Integer = ecPluginBase;

function NewPluginCommand: TBCEditorCommand;
begin
  Result := GCurrentCommand;
  Inc(GCurrentCommand);
end;

constructor TBCBaseEditorMacroRecorder.Create(AOwner: TComponent);
begin
  inherited;
  FMacroName := 'unnamed';
  FRecordCommandID := NewPluginCommand;
  FPlaybackCommandID := NewPluginCommand;
  FRecordShortCut := Vcl.Menus.ShortCut(Ord('R'), [ssCtrl, ssShift]);
  FPlaybackShortCut := Vcl.Menus.ShortCut(Ord('P'), [ssCtrl, ssShift]);
end;

function TBCBaseEditorMacroRecorder.CreateMacroEvent(ACommand: TBCEditorCommand): TBCEditorMacroEvent;

  function WantDefaultEvent(var AEvent: TBCEditorMacroEvent): Boolean;
  begin
    if Assigned(OnUserCommand) then
      OnUserCommand(Self, ACommand, AEvent);
    Result := not Assigned(AEvent);
  end;

begin
  case ACommand of
    ecGotoXY, ecSelectionGotoXY, ecSetBookmark1 .. ecSetBookmark9:
      begin
        Result := TBCEditorPositionEvent.Create;
        TBCEditorPositionEvent(Result).Command := ACommand;
      end;
    ecChar:
      Result := TBCEditorCharEvent.Create;
    ecString:
      Result := TBCEditorStringEvent.Create;
  else
    begin
      Result := nil;
      if (ACommand < ecUserFirst) or WantDefaultEvent(Result) then
      begin
        Result := TBCEditorBasicEvent.Create;
        TBCEditorBasicEvent(Result).Command := ACommand;
      end;
    end;
  end;
end;

function TBCBaseEditorMacroRecorder.GetEditors(AIndex: Integer): TBCBaseEditor;
begin
  Result := TBCBaseEditor(FEditors[AIndex]);
end;

procedure TBCBaseEditorMacroRecorder.DeleteEvent(AIndex: Integer);
var
  LObject: Pointer;
begin
  LObject := FEvents[AIndex];
  FEvents.Delete(AIndex);
  TObject(LObject).Free;
end;

procedure ReleasePluginCommand(ACommand: TBCEditorCommand);
begin
  if ACommand = GCurrentCommand - 1 then
    GCurrentCommand := ACommand;
end;

destructor TBCBaseEditorMacroRecorder.Destroy;
begin
  while Assigned(FEditors) do
    RemoveEditor(Editors[0]);
  Clear;
  inherited;
  ReleasePluginCommand(PlaybackCommandID);
  ReleasePluginCommand(RecordCommandID);
end;

function TBCBaseEditorMacroRecorder.GetEditor: TBCBaseEditor;
begin
  if Assigned(FEditors) then
    Result := FEditors[0]
  else
    Result := nil;
end;

procedure TBCBaseEditorMacroRecorder.SetEditor(const Value: TBCBaseEditor);
var
  LEditor: TBCBaseEditor;
begin
  LEditor := Editor;
  if LEditor <> Value then
    try
      if Assigned(LEditor) and (FEditors.Count = 1) then
        RemoveEditor(LEditor);
      if Assigned(Value) then
        AddEditor(Value);
    except
      if [csDesigning] * ComponentState = [csDesigning] then
        Application.HandleException(Self)
      else
        raise;
    end;
end;

function TBCBaseEditorMacroRecorder.GetEditorCount: Integer;
begin
  if Assigned(FEditors) then
    Result := FEditors.Count
  else
    Result := 0;
end;

procedure TBCBaseEditorMacroRecorder.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited;
  if AOperation = opRemove then
    if (AComponent = Editor) or (AComponent is TBCBaseEditor) then
      RemoveEditor(TBCBaseEditor(AComponent));
end;

procedure TBCBaseEditorMacroRecorder.DoAddEditor(AEditor: TBCBaseEditor);
begin
  HookEditor(AEditor, RecordCommandID, 0, RecordShortCut);
  HookEditor(AEditor, PlaybackCommandID, 0, PlaybackShortCut);
end;

procedure TBCBaseEditorMacroRecorder.DoRemoveEditor(AEditor: TBCBaseEditor);
begin
  UnHookEditor(AEditor, RecordCommandID, RecordShortCut);
  UnHookEditor(AEditor, PlaybackCommandID, PlaybackShortCut);
end;

procedure TBCBaseEditorMacroRecorder.Error(const AMessage: string);
begin
  raise Exception.Create(AMessage);
end;

function TBCBaseEditorMacroRecorder.GetEvent(AIndex: Integer): TBCEditorMacroEvent;
begin
  Result := TBCEditorMacroEvent(FEvents[AIndex]);
end;

function TBCBaseEditorMacroRecorder.GetEventCount: Integer;
begin
  if not Assigned(FEvents) then
    Result := 0
  else
    Result := FEvents.Count;
end;

function TBCBaseEditorMacroRecorder.GetIsEmpty: Boolean;
begin
  Result := not Assigned(FEvents) or (FEvents.Count = 0);
end;

procedure TBCBaseEditorMacroRecorder.InsertCustomEvent(AIndex: Integer; AEvent: TBCEditorMacroEvent);
begin
  if not Assigned(FEvents) then
    FEvents := TList.Create;
  FEvents.Insert(AIndex, AEvent);
end;

procedure TBCBaseEditorMacroRecorder.InsertEvent(AIndex: Integer; ACommand: TBCEditorCommand; AChar: Char;
  AData: Pointer);
var
  LEvent: TBCEditorMacroEvent;
begin
  LEvent := CreateMacroEvent(ACommand);
  try
    LEvent.Initialize(ACommand, AChar, AData);
    InsertCustomEvent(AIndex, LEvent);
  except
    LEvent.Free;
    raise;
  end;
end;

procedure TBCBaseEditorMacroRecorder.LoadFromStream(ASource: TStream; AClear: Boolean = True);
var
  LCommand: TBCEditorCommand;
  LEvent: TBCEditorMacroEvent;
  LCount, i: Integer;
begin
  Stop;
  if AClear then
    Clear;
  FEvents := TList.Create;
  ASource.Read(LCount, SizeOf(LCount));
  i := 0;
  FEvents.Capacity := ASource.Size div SizeOf(TBCEditorCommand);
  while (ASource.Position < ASource.Size) and (i < LCount) do
  begin
    ASource.Read(LCommand, SizeOf(TBCEditorCommand));
    LEvent := CreateMacroEvent(LCommand);
    LEvent.Initialize(LCommand, BCEDITOR_NONE_CHAR, nil);
    LEvent.LoadFromStream(ASource);
    FEvents.Add(LEvent);
    Inc(i);
  end;
end;

procedure TBCBaseEditorMacroRecorder.OnCommand(Sender: TObject; AfterProcessing: Boolean; var Handled: Boolean;
  var Command: TBCEditorCommand; var AChar: Char; Data, HandlerData: Pointer);
var
  LEvent: TBCEditorMacroEvent;
begin
  if AfterProcessing then
  begin
    if (Sender = FCurrentEditor) and (State = msRecording) and (not Handled) then
    begin
      LEvent := CreateMacroEvent(Command);
      LEvent.Initialize(Command, AChar, Data);
      FEvents.Add(LEvent);
      if SaveMarkerPos and (Command >= ecSetBookmark1) and (Command <= ecSetBookmark9) and not Assigned(Data) then
        TBCEditorPositionEvent(LEvent).Position := FCurrentEditor.TextCaretPosition;
    end;
  end
  else
  begin
    { not AfterProcessing }
    case State of
      msStopped:
        if Command = RecordCommandID then
        begin
          RecordMacro(TBCBaseEditor(Sender));
          Handled := True;
        end
        else
        if Command = PlaybackCommandID then
        begin
          PlaybackMacro(TBCBaseEditor(Sender));
          Handled := True;
        end;
      msPlaying:
        ;
      msPaused:
        if Command = PlaybackCommandID then
        begin
          Resume;
          Handled := True;
        end;
      msRecording:
        if Command = PlaybackCommandID then
        begin
          Pause;
          Handled := True;
        end
        else
        if Command = RecordCommandID then
        begin
          Stop;
          Handled := True;
        end;
    end;
  end;
end;

procedure TBCBaseEditorMacroRecorder.Pause;
begin
  if State <> msRecording then
    Error(SBCEditorCannotPause);
  FState := msPaused;
  StateChanged;
end;

procedure TBCBaseEditorMacroRecorder.PlaybackMacro(AEditor: TBCBaseEditor);
var
  i: Integer;
begin
  if State <> msStopped then
    Error(SBCEditorCannotPlay);
  FState := msPlaying;
  try
    StateChanged;
    for i := 0 to EventCount - 1 do
    begin
      Events[i].Playback(AEditor);
      if State <> msPlaying then
        break;
    end;
  finally
    if State = msPlaying then
    begin
      FState := msStopped;
      StateChanged;
    end;
  end;
end;

procedure TBCBaseEditorMacroRecorder.RecordMacro(AEditor: TBCBaseEditor);
begin
  if FState <> msStopped then
    Error(SBCEditorCannotRecord);
  Clear;
  FEvents := TList.Create;
  FEvents.Capacity := 512;
  FState := msRecording;
  FCurrentEditor := AEditor;
  StateChanged;
end;

function TBCBaseEditorMacroRecorder.RemoveEditor(AEditor: TBCBaseEditor): Integer;
begin
  if not Assigned(FEditors) then
  begin
    Result := -1;
    Exit;
  end;
  Result := FEditors.Remove(AEditor);
  if FEditors.Count = 0 then
  begin
    FEditors.Free;
    FEditors := nil;
  end;
  if Result >= 0 then
    DoRemoveEditor(AEditor);
end;

procedure TBCBaseEditorMacroRecorder.Resume;
begin
  if FState <> msPaused then
    Error(SBCEditorCannotResume);
  FState := msRecording;
  StateChanged;
end;

procedure TBCBaseEditorMacroRecorder.SaveToStream(ADestination: TStream);
var
  i, LCount: Integer;
begin
  LCount := EventCount;
  ADestination.Write(LCount, SizeOf(LCount));
  for i := 0 to LCount - 1 do
    Events[i].SaveToStream(ADestination);
end;

procedure TBCBaseEditorMacroRecorder.SetRecordShortCut(const Value: TShortCut);
var
  i: Integer;
begin
  if FRecordShortCut <> Value then
  begin
    if Assigned(FEditors) then
      if Value <> 0 then
      for i := 0 to FEditors.Count - 1 do
        HookEditor(Editors[i], FRecordCommandID, FRecordShortCut, Value)
      else
      for i := 0 to FEditors.Count - 1 do
        UnHookEditor(Editors[i], FRecordCommandID, FRecordShortCut);
    FRecordShortCut := Value;
  end;
end;

procedure TBCBaseEditorMacroRecorder.SetPlaybackShortCut(const Value: TShortCut);
var
  i: Integer;
begin
  if FPlaybackShortCut <> Value then
  begin
    if Assigned(FEditors) then
      if Value <> 0 then
      for i := 0 to FEditors.Count - 1 do
        HookEditor(Editors[i], FPlaybackCommandID, FPlaybackShortCut, Value)
      else
      for i := 0 to FEditors.Count - 1 do
        UnHookEditor(Editors[i], FPlaybackCommandID, FPlaybackShortCut);
    FPlaybackShortCut := Value;
  end;
end;

procedure TBCBaseEditorMacroRecorder.StateChanged;
begin
  if Assigned(OnStateChange) then
    OnStateChange(Self);
end;

procedure TBCBaseEditorMacroRecorder.Stop;
begin
  if FState = msStopped then
    Exit;
  FState := msStopped;
  FCurrentEditor := nil;
  if FEvents.Count = 0 then
  begin
    FEvents.Free;
    FEvents := nil;
  end;
  StateChanged;
end;

function TBCBaseEditorMacroRecorder.GetAsString: string;
var
  i: Integer;
  s: string;
begin
  Result := 'macro ' + MacroName + SLineBreak + 'begin' + SLineBreak;
  if Assigned(FEvents) then
  begin
    for i := 0 to FEvents.Count - 1 do
    begin
      s := Events[i].AsString;
      if s <> '' then
        Result := Result + '  ' + s + SLineBreak;
    end;
  end;
  Result := Result + 'end';
end;

procedure TBCBaseEditorMacroRecorder.SetAsString(const Value: string);
var
  I, Position, LCommand: Integer;
  StringList: TStrings;
  CommandString: string;
  LEvent: TBCEditorMacroEvent;
begin
  Stop;
  Clear;
  FEvents := TList.Create;
  StringList := TStringList.Create;
  try
    StringList.Text := Value;
    for I := 0 to StringList.Count - 1 do
    begin
      CommandString := Trim(StringList[I]);
      Position := Pos(' ', CommandString);
      if Position = 0 then
        Position := Length(CommandString) + 1;
      LCommand := ecNone;
      if IdentToEditorCommand(Copy(CommandString, 1, Position - 1), Longint(LCommand)) then
      begin
        Delete(CommandString, 1, Position);
        LEvent := CreateMacroEvent(LCommand);
        try
          FEvents.Add(LEvent);
          LEvent.InitEventParameters(CommandString);
        except
          LEvent.Free;
        end;
      end;
    end;
  finally
    StringList.Free;
  end;
end;

procedure TBCBaseEditorMacroRecorder.LoadFromFile(AFilename: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFilename, fmOpenRead);
  try
    LoadFromStream(FileStream);
    MacroName := ChangeFileExt(ExtractFileName(AFilename), '');
  finally
    FileStream.Free;
  end;
end;

procedure TBCBaseEditorMacroRecorder.SaveToFile(AFilename: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFilename, fmCreate);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TBCBaseEditorMacroRecorder.HookEditor(AEditor: TBCBaseEditor; ACommandID: TBCEditorCommand;
  AOldShortCut, ANewShortCut: TShortCut);
var
  Index: Integer;
  KeyCommand: TBCEditorKeyCommand;
begin
  Assert(ANewShortCut <> 0);
  if [csDesigning] * ComponentState = [csDesigning] then
    if TBCBaseEditor(AEditor).KeyCommands.FindShortcut(ANewShortCut) >= 0 then
      raise Exception.Create('Shortcut already exists')
    else
      Exit;
  if AOldShortCut <> 0 then
  begin
    Index := TBCBaseEditor(AEditor).KeyCommands.FindShortcut(AOldShortCut);
    if Index >= 0 then
    begin
      KeyCommand := TBCBaseEditor(AEditor).KeyCommands[Index];
      if KeyCommand.Command = ACommandID then
      begin
        KeyCommand.ShortCut := ANewShortCut;
        Exit;
      end;
    end;
  end;
  KeyCommand := TBCBaseEditor(AEditor).KeyCommands.NewItem;
  try
    KeyCommand.ShortCut := ANewShortCut;
  except
    KeyCommand.Free;
    raise;
  end;
  KeyCommand.Command := ACommandID;
  AEditor.RegisterCommandHandler(OnCommand, Self);
end;

procedure TBCBaseEditorMacroRecorder.UnHookEditor(AEditor: TBCBaseEditor; ACommandID: TBCEditorCommand;
  AShortCut: TShortCut);
var
  Index: Integer;
begin
  AEditor.UnregisterCommandHandler(OnCommand);
  if Assigned(AEditor) and Assigned(AEditor.KeyCommands) then
  begin
    Index := AEditor.KeyCommands.FindShortcut(AShortCut);
    if (Index >= 0) and (AEditor.KeyCommands[Index].Command = ACommandID) then
      AEditor.KeyCommands[Index].Free;
  end;
end;

{ TBCEditorBasicEvent }

function TBCEditorBasicEvent.GetAsString: string;
var
  Ident: string;
begin
  EditorCommandToIdent(Command, Ident);
  Result := Ident;
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TBCEditorBasicEvent.InitEventParameters(AString: string);
begin
  RepeatCount := StrToIntDef(Trim(AString), 1);
end;

procedure TBCEditorBasicEvent.Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer);
begin
  Command := ACommand;
end;

procedure TBCEditorBasicEvent.LoadFromStream(AStream: TStream);
begin
  AStream.Read(FRepeatCount, SizeOf(FRepeatCount));
end;

procedure TBCEditorBasicEvent.Playback(AEditor: TBCBaseEditor);
var
  i: Integer;
begin
  for i := 1 to RepeatCount do
    AEditor.CommandProcessor(Command, BCEDITOR_NONE_CHAR, nil);
end;

procedure TBCEditorBasicEvent.SaveToStream(AStream: TStream);
begin
  AStream.Write(Command, SizeOf(TBCEditorCommand));
  AStream.Write(RepeatCount, SizeOf(RepeatCount));
end;

{ TBCEditorCharEvent }

function TBCEditorCharEvent.GetAsString: string;
var
  Ident: string;
begin
  EditorCommandToIdent(ecChar, Ident);
  Result := Ident + ' ' + Key;
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TBCEditorCharEvent.InitEventParameters(AString: string);
begin
  if Length(AString) >= 1 then
    Key := AString[1]
  else
    Key := ' ';
  Delete(AString, 1, 1);
  RepeatCount := StrToIntDef(Trim(AString), 1);
end;

procedure TBCEditorCharEvent.Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer);
begin
  Key := AChar;
  Assert(not Assigned(AData));
end;

procedure TBCEditorCharEvent.LoadFromStream(AStream: TStream);
begin
  AStream.Read(FKey, SizeOf(Key));
  AStream.Read(FRepeatCount, SizeOf(FRepeatCount));
end;

procedure TBCEditorCharEvent.Playback(AEditor: TBCBaseEditor);
var
  i: Integer;
begin
  for i := 1 to RepeatCount do
    AEditor.CommandProcessor(ecChar, Key, nil);
end;

procedure TBCEditorCharEvent.SaveToStream(AStream: TStream);
const
  CharCommand: TBCEditorCommand = ecChar;
begin
  AStream.Write(CharCommand, SizeOf(TBCEditorCommand));
  AStream.Write(Key, SizeOf(Key));
  AStream.Write(RepeatCount, SizeOf(RepeatCount));
end;

{ TBCEditorPositionEvent }

function TBCEditorPositionEvent.GetAsString: string;
begin
  Result := inherited GetAsString;

  Result := Result + Format(' (%d, %d)', [Position.Char, Position.Line]);
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TBCEditorPositionEvent.InitEventParameters(AString: string);
var
  DotPosition, OpenPosition, ClosePosition, X, Y: Integer;
  valStr: string;
begin
  inherited;
  AString := Trim(AString);
  DotPosition := Pos(',', AString);
  OpenPosition := Pos('(', AString);
  ClosePosition := Pos(')', AString);
  if (not((DotPosition = 0) or (OpenPosition = 0) or (ClosePosition = 0))) and ((DotPosition > OpenPosition) and
    (DotPosition < ClosePosition)) then
  begin
    valStr := Copy(AString, OpenPosition + 1, DotPosition - OpenPosition - 1);
    X := StrToIntDef(valStr, 1);
    Delete(AString, 1, DotPosition);
    AString := Trim(AString);
    ClosePosition := Pos(')', AString);
    valStr := Copy(AString, 1, ClosePosition - 1);
    Y := StrToIntDef(valStr, 1);
    Position := GetTextPosition(X, Y);
    Delete(AString, 1, ClosePosition);
    AString := Trim(AString);
    RepeatCount := StrToIntDef(AString, 1);
  end;
end;

procedure TBCEditorPositionEvent.Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer);
begin
  inherited;
  if Assigned(AData) then
    Position := TBCEditorTextPosition(AData^)
  else
    Position := GetTextPosition(0, 0);
end;

procedure TBCEditorPositionEvent.LoadFromStream(AStream: TStream);
begin
  AStream.Read(FPosition, SizeOf(Position));
end;

procedure TBCEditorPositionEvent.Playback(AEditor: TBCBaseEditor);
begin
  if (Position.Char <> 0) or (Position.Line <> 0) then
    AEditor.CommandProcessor(Command, BCEDITOR_NONE_CHAR, @Position)
  else
    AEditor.CommandProcessor(Command, BCEDITOR_NONE_CHAR, nil);
end;

procedure TBCEditorPositionEvent.SaveToStream(AStream: TStream);
begin
  inherited;
  AStream.Write(Position, SizeOf(Position));
end;

{ TBCEditorStringEvent }

function TBCEditorStringEvent.GetAsString: string;
var
  Ident: string;
begin
  EditorCommandToIdent(ecString, Ident);
  Result := Ident + ' ' + WideQuotedStr(Value, #39);
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TBCEditorStringEvent.InitEventParameters(AString: string);
var
  OpenPosition, ClosePosition: Integer;
  LValue: string;

  function WideLastDelimiter(const Delimiters, S: string): Integer;
  var
    P: PChar;
  begin
    Result := Length(S);
    P := PChar(Delimiters);
    while Result > 0 do
    begin
      if (S[Result] <> BCEDITOR_NONE_CHAR) and Assigned(WStrScan(P, S[Result])) then
        Exit;
      Dec(Result);
    end;
  end;

begin
  OpenPosition := Pos('''', AString);
  ClosePosition := WideLastDelimiter('''', AString);
  LValue := Copy(AString, OpenPosition + 1, ClosePosition - OpenPosition - 1);
  Value := StringReplace(LValue, '''''', '''', [rfReplaceAll]);
  Delete(AString, 1, ClosePosition);
  RepeatCount := StrToIntDef(Trim(AString), 1);
end;

procedure TBCEditorStringEvent.Initialize(ACommand: TBCEditorCommand; AChar: Char; AData: Pointer);
begin
  Value := string(AData);
end;

procedure TBCEditorStringEvent.LoadFromStream(AStream: TStream);
var
  LLength: Integer;
  Buffer: PChar;
begin
  AStream.Read(LLength, SizeOf(LLength));
  GetMem(Buffer, LLength * SizeOf(Char));
  try
    FillMemory(Buffer, LLength, 0);
    AStream.Read(Buffer^, LLength * SizeOf(Char));
    FString := Buffer;
  finally
    FreeMem(Buffer);
  end;
  AStream.Read(FRepeatCount, SizeOf(FRepeatCount));
end;

procedure TBCEditorStringEvent.Playback(AEditor: TBCBaseEditor);
var
  i, j: Integer;
begin
  for j := 1 to RepeatCount do
    for i := 1 to Length(Value) do
      AEditor.CommandProcessor(ecChar, Value[i], nil);
end;

procedure TBCEditorStringEvent.SaveToStream(AStream: TStream);
const
  Command: TBCEditorCommand = ecString;
var
  LLength: Integer;
  Buffer: PChar;
begin
  AStream.Write(Command, SizeOf(Command));
  LLength := Length(Value) + 1;
  AStream.Write(LLength, SizeOf(LLength));
  GetMem(Buffer, LLength * SizeOf(Char));
  try
    FillMemory(Buffer, LLength, 0);
    WStrCopy(Buffer, PChar(Value));
    AStream.Write(Buffer^, LLength * SizeOf(Char));
  finally
    FreeMem(Buffer);
  end;
  AStream.Write(RepeatCount, SizeOf(RepeatCount));
end;

{ TBCEditorMacroEvent }

constructor TBCEditorMacroEvent.Create;
begin
  inherited;

  FRepeatCount := 1;
end;

end.

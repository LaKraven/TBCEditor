program TBCEditorDemo;

uses
  Vcl.Forms,
  TBCEditorDemo.Forms.Main in 'Forms\TBCEditorDemo.Forms.Main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'BCEditor - Property Demo';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

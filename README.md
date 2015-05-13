Version 1.0 beta released. Use the Issues page to report bugs or send them directly to lasse@bonecode.com.

<h3>Build requirements</h3>

<a href="https://github.com/ahausladen/JsonDataObjects">Json Data Objects</a> (included)

Delphi versions from XE4 to XE8 supported. 

Eric Grange has made a Delphi XE backport: https://bitbucket.org/egrange/tbceditorxe

<h3>Conditional compilation</h3>

Define | Description 
--- | --- 
USE_ALPHASKINS | Use <a href="http://www.alphaskins.com/">AlphaSkins</a>. AlphaSkins are most powerful theming solutions for apps developed in Delphi.
USE_VCL_STYLES | Use VCL styles. A set of graphical details that define the look and feel of a VCL application.

<h3>Usage example</h3>

```
TYourForm = class(TForm)
  Editor: TBCEditor;
...
var
  LFilePath: string;
begin
  LFilePath := ExtractFilePath(Application.ExeName); 
  with Editor do 
  begin
    Highlighter.LoadFromFile(Format('%sHighlighters\%s.json', [LFilePath, 'JSON']); { Highlighter }
    Highlighter.LoadColorsFromFile(Format('%sColors\%s.json', [LFilePath, 'Default']); { Color }
    LoadFromFile(Format('%sHighlighters\%s.json', [LFilePath, 'JSON']); { Editor file } 
    ...
    ClearCodeFolding;
    Lines.Text := Highlighter.Info.General.Sample; { Set text }
    InitCodeFolding;
  end;
end;
```

<h3>Demo</h3>

TBCEditor Control Demo v1.0b. 

  * <a href="http://www.bonecode.com/downloads/BCEditorComponentDemo32.zip">32-bit Windows</a>
  * <a href="http://www.bonecode.com/downloads/BCEditorComponentDemo64.zip">64-bit Windows</a>

The latest update: 13.05.2015 20:17, UTC+02:00

<h3>Screenshots</h3>

![bceditor0](https://cloud.githubusercontent.com/assets/11475177/7427348/174542e4-efe2-11e4-9913-14500cc787e5.png)
![bceditor1](https://cloud.githubusercontent.com/assets/11475177/7427349/1766adc6-efe2-11e4-8a2f-a59ec668d217.png)
![bceditor2](https://cloud.githubusercontent.com/assets/11475177/7427350/177ba3c0-efe2-11e4-92dc-946b026cbfab.png)
![bceditor3](https://cloud.githubusercontent.com/assets/11475177/7427351/177f5f4c-efe2-11e4-8388-179a0947eb5f.png)
![bceditor4](https://cloud.githubusercontent.com/assets/11475177/7427352/17843c06-efe2-11e4-8c03-7a3daa4639be.png)
![bceditor5](https://cloud.githubusercontent.com/assets/11475177/7427346/173de47c-efe2-11e4-9b68-ce2ae7ffb1a2.png)
![bceditor6](https://cloud.githubusercontent.com/assets/11475177/7427347/1743c07c-efe2-11e4-9c90-318cdc2b09a0.png)





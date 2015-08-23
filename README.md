<h3>Description</h3>

A syntax highlighting edit control with code folding, minimap, external JSON highlighter and color scheme files, etc.

Version 1.0 beta released. Use the Issues page to report bugs or send them directly to lasse@bonecode.com.

<h3>Build requirements</h3>

* <a href="https://github.com/ahausladen/JsonDataObjects">Json Data Objects</a> (included)
* Delphi versions from XE4 to XE8 are supported - XE7: Update 1 required
* C++ Builder XE7 supported (more to come)

Eric Grange has made a Delphi XE backport: https://bitbucket.org/egrange/tbceditorxe

<h3>Conditional compilation</h3>

Define | Description 
--- | --- 
USE_ALPHASKINS | Use <a href="http://www.alphaskins.com/">AlphaSkins</a>. AlphaSkins are most powerful theming solutions for apps developed in Delphi.
USE_VCL_STYLES | Use VCL styles. A set of graphical details that define the look and feel of a VCL application.

<h3>Usage example</h3>

```
  with Editor do 
  begin
    { Load highlighter from file }
    Highlighter.LoadFromFile('JSON.json');
    { Load color from file }
    Highlighter.Colors.LoadFromFile('Default.json'); 
    { Load a file into editor }  
    LoadFromFile(GetHighlighterFileName('JSON.json')); 
    ...
    { Set editor lines }
    ClearCodeFolding;
    Lines.Text := Highlighter.Info.General.Sample; 
    InitCodeFolding;
  end;
```

<h3>Demo</h3>

TBCEditor Control Demo v. 1.0b. 

  * <a href="http://www.bonecode.com/downloads/BCEditorComponentDemo32.zip">32-bit Windows</a>
  * <a href="http://www.bonecode.com/downloads/BCEditorComponentDemo64.zip">64-bit Windows</a>

The latest update: 23.08.2015 22:10, UTC+02:00

<h3>Project's using the control</h3>

EditBone v. <a href="http://bonecode.com/downloads/EditBone/changes.html" target="_blank">10.0.0 beta</a>

  * <a href="http://www.bonecode.com/downloads/EditBone32.zip">32-bit Windows</a> 
  * <a href="http://www.bonecode.com/downloads/EditBone64.zip">64-bit Windows</a>

<h3>Screenshots</h3>

![bceditor0](https://cloud.githubusercontent.com/assets/11475177/7647152/44552956-fad8-11e4-9994-8c0ac8a21572.png)
![bceditor1](https://cloud.githubusercontent.com/assets/11475177/7427349/1766adc6-efe2-11e4-8a2f-a59ec668d217.png)
![bceditor2](https://cloud.githubusercontent.com/assets/11475177/7427350/177ba3c0-efe2-11e4-92dc-946b026cbfab.png)
![bceditor3](https://cloud.githubusercontent.com/assets/11475177/7427351/177f5f4c-efe2-11e4-8388-179a0947eb5f.png)
![bceditor4](https://cloud.githubusercontent.com/assets/11475177/7427352/17843c06-efe2-11e4-8c03-7a3daa4639be.png)
![bceditor5](https://cloud.githubusercontent.com/assets/11475177/7634347/04a52aa6-fa63-11e4-97d4-5d2a9e93e0b8.png)
![bceditor6](https://cloud.githubusercontent.com/assets/11475177/7427347/1743c07c-efe2-11e4-9c90-318cdc2b09a0.png)





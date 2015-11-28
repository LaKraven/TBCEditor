Version 1.0 beta. Use the Issues page to report bugs or send them directly to lasse@bonecode.com.

<h3>Description</h3>

A syntax highlighting edit control with code folding, minimap, external JSON highlighter and color scheme files, etc.

<h3>Build requirements</h3>

* <a href="https://github.com/ahausladen/JsonDataObjects">Json Data Objects</a> (included)
* Delphi versions XE4, XE5, XE6, XE7, XE8, and Seattle are supported 
* Delphi XE7: Update 1 required
* C++ Builder XE7, XE8, and Seattle are supported

<h3>Conditional compilation</h3>

Define | Description 
--- | --- 
USE_ALPHASKINS | Use <a href="http://www.alphaskins.com/">AlphaSkins</a>. AlphaSkins are most powerful theming solutions for apps developed in Delphi.
USE_VCL_STYLES | Use VCL styles. A set of graphical details that define the look and feel of a VCL application.

<h3>Usage example</h3>

```
  with Editor do 
  begin
    Highlighter.LoadFromFile('JSON.json');
    Highlighter.Colors.LoadFromFile('Default.json'); 
    LoadFromFile(GetHighlighterFileName('JSON.json')); 
    ...
    Lines.Text := Highlighter.Info.General.Sample; 
  end;
```
Note! LoadFromStream does not support multi-highlighters (for example HTML with Scripts.json). Override TBCBaseEditor.CreateFileStream function, if you want to load multi-highlighters from a stream. 

<h3>Demo</h3>

TBCEditor Control Demo v. 1.0b. 

  * <a href="http://www.bonecode.com/downloads/BCEditorComponentDemo32.zip">32-bit Windows</a>
  * <a href="http://www.bonecode.com/downloads/BCEditorComponentDemo64.zip">64-bit Windows</a>

The latest update: 27.11.2015 15:18, UTC+02:00

Demo source build requires <a href="http://www.alphaskins.com/">AlphaSkins</a> and <a href="http://www.ehlib.com/">EhLib</a>. 
<h3>Documentation</h3>

Documentation will be written after the project stabilizes and dust settles. This project is developed in my spare time without sources of income and as long as this is the case there is no timetable for anything. 

<h3>Projects using the control</h3>

* <a href="http://www.bonecode.com">EditBone</a>
* <a href="http://www.mitec.cz/ibq.html">MiTeC Interbase Query</a>
* <a href="http://www.mitec.cz/sqliteq.html">MiTeC SQLite Query</a>

<h3>Screenshots</h3>

![bceditor1](https://cloud.githubusercontent.com/assets/11475177/11452946/6d4d8064-9601-11e5-9d4b-47341e3fd9b5.png)
![bceditor2](https://cloud.githubusercontent.com/assets/11475177/11452947/6d7633f6-9601-11e5-840f-c65deea43a65.png)
![bceditor3](https://cloud.githubusercontent.com/assets/11475177/11452948/6d7aa0e4-9601-11e5-963b-83050a710840.png)
![bceditor4](https://cloud.githubusercontent.com/assets/11475177/11452949/6d87a690-9601-11e5-89be-a3ffd80d2dca.png)
![bceditor5](https://cloud.githubusercontent.com/assets/11475177/11452950/6d88e776-9601-11e5-904d-ad9c3466037e.png)
![bceditor6](https://cloud.githubusercontent.com/assets/11475177/11452945/6d4afde4-9601-11e5-8a44-b93ee64f6dd9.png)






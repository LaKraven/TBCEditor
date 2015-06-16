inherited MainForm: TMainForm
  Caption = 'TBCEditor Control Version 1.0b - Property Demo'
  ClientHeight = 644
  ClientWidth = 1100
  Color = clWhite
  Position = poScreenCenter
  ShowHint = True
  OnShow = FormShow
  ExplicitWidth = 1116
  ExplicitHeight = 682
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TBCSplitter [0]
    Left = 754
    Top = 0
    Height = 625
    Align = alRight
    SkinData.SkinSection = 'SPLITTER'
  end
  inherited StatusBar: TBCStatusBar
    Top = 625
    Width = 1100
    Panels = <
      item
        Alignment = taCenter
        Width = 86
      end
      item
        Width = 86
      end
      item
        Width = 86
      end
      item
        Width = 50
      end>
    ExplicitTop = 625
    ExplicitWidth = 1100
  end
  object PanelProperty: TBCPanel [2]
    Left = 760
    Top = 0
    Width = 340
    Height = 625
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alRight
    BevelOuter = bvNone
    DoubleBuffered = False
    ParentColor = True
    ParentDoubleBuffered = False
    TabOrder = 1
    SkinData.SkinSection = 'TRANSPARENT'
    object ObjectInspectorEh: TObjectInspectorEh
      AlignWithMargins = True
      Left = 0
      Top = 5
      Width = 340
      Height = 615
      Margins.Left = 0
      Margins.Top = 5
      Margins.Right = 0
      Margins.Bottom = 5
      Options = [goFixedVertLineEh, goVertLineEh, goEditingEh, goAlwaysShowEditorEh]
    end
  end
  object PanelLeft: TBCPanel [3]
    Left = 0
    Top = 0
    Width = 754
    Height = 625
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 2
    SkinData.SkinSection = 'TRANSPARENT'
    object Editor: TBCEditor
      AlignWithMargins = True
      Left = 0
      Top = 5
      Width = 754
      Height = 593
      Cursor = crIBeam
      Margins.Left = 0
      Margins.Top = 5
      Margins.Right = 0
      Margins.Bottom = 0
      ActiveLine.Indicator.Visible = False
      Align = alClient
      Caret.NonBlinking.Enabled = False
      Caret.Options = []
      CodeFolding.Colors.Indent = clBlack
      CodeFolding.Hint.Font.Charset = DEFAULT_CHARSET
      CodeFolding.Hint.Font.Color = clWindowText
      CodeFolding.Hint.Font.Height = -11
      CodeFolding.Hint.Font.Name = 'Courier New'
      CodeFolding.Hint.Font.Style = []
      CodeFolding.Visible = True
      CompletionProposal.CloseChars = '()[]. '
      CompletionProposal.Columns = <>
      CompletionProposal.Font.Charset = DEFAULT_CHARSET
      CompletionProposal.Font.Color = clWindowText
      CompletionProposal.Font.Height = -11
      CompletionProposal.Font.Name = 'Courier New'
      CompletionProposal.Font.Style = []
      CompletionProposal.ShortCut = 16416
      CompletionProposal.Trigger.Chars = '.'
      CompletionProposal.Trigger.Enabled = False
      Directories.Colors = 'Colors'
      Directories.Highlighters = 'Highlighters'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      LeftMargin.Font.Charset = DEFAULT_CHARSET
      LeftMargin.Font.Color = 13408665
      LeftMargin.Font.Height = -11
      LeftMargin.Font.Name = 'Courier New'
      LeftMargin.Font.Style = []
      LeftMargin.Width = 55
      Lines.Strings = (
        '')
      LineSpacing.Spacing = 1
      MatchingPair.Enabled = True
      Minimap.Font.Charset = DEFAULT_CHARSET
      Minimap.Font.Color = clWindowText
      Minimap.Font.Height = -4
      Minimap.Font.Name = 'Courier New'
      Minimap.Font.Style = []
      Minimap.Width = 140
      OnCaretChanged = EditorCaretChanged
      RightMargin.Position = 80
      RightMargin.Visible = True
      Search.Enabled = False
      SpecialChars.Style = scsDot
      TabOrder = 0
      WordWrap.Enabled = False
      WordWrap.Position = 80
      WordWrap.Style = wwsClientWidth
    end
    object PanelSearchFrame: TBCPanel
      Left = 0
      Top = 598
      Width = 754
      Height = 27
      Align = alBottom
      AutoSize = True
      BevelOuter = bvNone
      Padding.Top = 3
      Padding.Bottom = 3
      TabOrder = 1
      SkinData.SkinSection = 'CHECKBOX'
      inline SearchFrame: TBCSearchFrame
        Left = 0
        Top = 3
        Width = 754
        Height = 21
        Align = alBottom
        Color = clWindow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentBackground = False
        ParentColor = False
        ParentFont = False
        TabOrder = 0
        ExplicitTop = 3
        ExplicitWidth = 754
        inherited SearchPanel: TBCPanel
          Width = 754
          ExplicitWidth = 754
          inherited SpeedButtonSearchClose: TBCSpeedButton
            Left = 733
            ExplicitLeft = 733
          end
          inherited PanelRight: TBCPanel
            Width = 400
            ExplicitWidth = 400
            inherited LabelSearchResultCount: TBCLabelFX
              Left = 388
              Height = 21
              ExplicitLeft = 388
            end
          end
        end
      end
    end
  end
  inherited SkinManager: TBCSkinManager
    IsDefault = False
    ThirdParty.ThirdEdits = ' '#13#10'TBCEditor'#13#10'TBCEditorPrintPreview'#13#10
    ThirdParty.ThirdButtons = 'TButton'#13#10
    ThirdParty.ThirdBitBtns = ' '#13#10
    ThirdParty.ThirdCheckBoxes = ' '#13#10
    ThirdParty.ThirdGroupBoxes = ' '#13#10
    ThirdParty.ThirdListViews = ' '#13#10
    ThirdParty.ThirdPanels = ' '#13#10
    ThirdParty.ThirdGrids = ' '#13#10
    ThirdParty.ThirdTreeViews = ' '#13#10
    ThirdParty.ThirdComboBoxes = ' '#13#10
    ThirdParty.ThirdWWEdits = ' '#13#10
    ThirdParty.ThirdVirtualTrees = ' '#13#10
    ThirdParty.ThirdGridEh = ' '#13#10
    ThirdParty.ThirdPageControl = ' '#13#10
    ThirdParty.ThirdTabControl = ' '#13#10
    ThirdParty.ThirdToolBar = ' '#13#10
    ThirdParty.ThirdStatusBar = ' '#13#10
    ThirdParty.ThirdSpeedButton = ' '#13#10
    ThirdParty.ThirdScrollControl = ' '#13#10
    ThirdParty.ThirdUpDown = ' '#13#10
    ThirdParty.ThirdScrollBar = ' '#13#10
    ThirdParty.ThirdStaticText = ' '#13#10
    ThirdParty.ThirdNativePaint = ' '#13#10
    OnGetMenuExtraLineData = SkinManagerGetMenuExtraLineData
    Left = 166
    Top = 26
  end
  inherited TitleBar: TBCTitleBar
    Items = <
      item
        Caption = 'File'
        DropdownMenu = PopupMenuFile
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Height = 22
        Index = 0
        Name = 'TitleBarItemFile'
        ShowHint = True
        Style = bsMenu
        Width = 48
      end
      item
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Index = 1
        Name = 'TitleBarItemSpacing1'
        ShowHint = False
        Style = bsSpacing
        Width = 6
      end
      item
        Caption = 'Skin'
        DropdownMenu = PopupMenuSkins
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Height = 22
        Index = 2
        Name = 'TitleBarItemSkins'
        ShowHint = False
        Style = bsMenu
        Width = 52
      end
      item
        Align = tbaCenterInSpace
        Caption = 'TBCEditor Control Demo v1.0b'
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Height = 21
        Index = 3
        Name = 'TitleBarItemCaption'
        ShowHint = False
        Style = bsInfo
        Width = 168
      end
      item
        Align = tbaRight
        Caption = 'Object Pascal'
        DropdownMenu = PopupMenuHighlighters
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Height = 22
        Index = 4
        Name = 'TitleBarItemHighlighter'
        ShowHint = False
        Style = bsMenu
        Width = 101
      end
      item
        Align = tbaRight
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Index = 5
        Name = 'TitleBarItemSpacing2'
        ShowHint = False
        Style = bsSpacing
        Width = 6
      end
      item
        Align = tbaRight
        Caption = 'Default'
        DropdownMenu = PopupMenuColors
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Height = 22
        Index = 6
        Name = 'TitleBarItemColors'
        ShowHint = False
        Style = bsMenu
        Width = 68
      end
      item
        Align = tbaRight
        FontData.Font.Charset = DEFAULT_CHARSET
        FontData.Font.Color = clWindowText
        FontData.Font.Height = -11
        FontData.Font.Name = 'Tahoma'
        FontData.Font.Style = []
        Index = 7
        Name = 'TitleBarItemSpacing3'
        ShowHint = False
        Style = bsSpacing
        Width = 2
      end>
    Left = 88
    Top = 22
  end
  inherited SkinProvider: TBCSkinProvider
    Left = 250
    Top = 26
  end
  inherited ApplicationEvents: TApplicationEvents
    OnMessage = ApplicationEventsMessage
    Left = 88
    Top = 88
  end
  inherited ActionList: TActionList
    Left = 178
    Top = 92
    object ActionSearch: TAction
      Caption = 'ActionSearch'
      ShortCut = 16454
      OnExecute = ActionSearchExecute
    end
    object ActionFileOpen: TAction
      Caption = 'Open...'
      ImageIndex = 1
      ShortCut = 16463
      OnExecute = ActionFileOpenExecute
    end
    object ActionPreview: TAction
      Caption = 'Print preview...'
      ImageIndex = 10
      OnExecute = ActionPreviewExecute
    end
  end
  object PopupMenuSkins: TPopupMenu
    Left = 86
    Top = 338
  end
  object PopupMenuFile: TPopupMenu
    Images = ImagesDataModule.ImageList
    Left = 84
    Top = 164
    object MenuItemFileOpen: TMenuItem
      Action = ActionFileOpen
      RadioItem = True
    end
    object MenuItemSeparator1: TMenuItem
      Caption = '-'
    end
    object MenuItemPrintPreview: TMenuItem
      Action = ActionPreview
      RadioItem = True
    end
    object MenuItemSeparator2: TMenuItem
      Caption = '-'
    end
    object MenuItemExit: TMenuItem
      Action = ActionFileExit
    end
  end
  object PopupMenuHighlighters: TPopupMenu
    Left = 84
    Top = 220
  end
  object PopupMenuColors: TPopupMenu
    Left = 84
    Top = 276
  end
  object MultiStringHolderFileTypes: TBCMultiStringHolder
    MultipleStrings = <
      item
        Name = 'Assembler (68HC11)'
        Strings.Strings = (
          '.asm')
      end
      item
        Name = 'AutoIt v3'
        Strings.Strings = (
          '.au3')
      end
      item
        Name = 'AWK'
        Strings.Strings = (
          '.awk')
      end
      item
        Name = 'C#'
        Strings.Strings = (
          '.cs')
      end
      item
        Name = 'C++'
        Strings.Strings = (
          '.c;.cpp;.h;.hpp')
      end
      item
        Name = 'CSS'
        Strings.Strings = (
          '.css')
      end
      item
        Name = 'Delphi Form Module'
        Strings.Strings = (
          '.dfm')
      end
      item
        Name = 'HTML with Scripts'
        Strings.Strings = (
          '.htm;.html')
      end
      item
        Name = 'Java'
        Strings.Strings = (
          '.java')
      end
      item
        Name = 'JavaScript'
        Strings.Strings = (
          '.js')
      end
      item
        Name = 'JSON'
        Strings.Strings = (
          '.json')
      end
      item
        Name = 'MS-DOS Batch'
        Strings.Strings = (
          '.bat')
      end
      item
        Name = 'Object Pascal'
        Strings.Strings = (
          '.pas;.dpr')
      end
      item
        Name = 'Perl'
        Strings.Strings = (
          '.pl')
      end
      item
        Name = 'PHP'
        Strings.Strings = (
          '.php')
      end
      item
        Name = 'Python'
        Strings.Strings = (
          '.py')
      end
      item
        Name = 'SQL (Standard)'
        Strings.Strings = (
          '.sql')
      end
      item
        Name = 'Visual Basic'
        Strings.Strings = (
          '.vb')
      end
      item
        Name = 'XML'
        Strings.Strings = (
          '.xml')
      end>
    Left = 324
    Top = 108
  end
  object OpenDialog: TsOpenDialog
    Left = 342
    Top = 52
  end
end

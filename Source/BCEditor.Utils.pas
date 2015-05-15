unit BCEditor.Utils;

interface

uses
  Winapi.Windows, System.Math, System.Classes, Vcl.Graphics, System.UITypes, BCEditor.Consts, BCEditor.Types;

  function CharWidthTable(AChar: Char): SmallInt;
  function GetBestTabConvertProc(TabWidth: Integer): TBCEditorTabConvertProc;
  function GetLeadingExpandedLength(const AStr: string; ATabWidth: Integer; ABorder: Integer = 0): Integer;
  function GetTextSize(AHandle: HDC; AText: PChar; ACount: Integer): TSize;
  function MessageDialog(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): Integer;
  function MinMax(Value, MinValue, MaxValue: Integer): Integer;
  function RoundCorrect(Value: Real): LongInt;
  function TextExtent(ACanvas: TCanvas; const Text: string): TSize;
  function TextWidth(ACanvas: TCanvas; const Text: string): Integer;
  function TextHeight(ACanvas: TCanvas; const Text: string): Integer;
  procedure ClearList(var List: TList);
  procedure FreeList(var List: TList);
  procedure TextOut(ACanvas: TCanvas; X, Y: Integer; const Text: string);
  procedure TextRect(ACanvas: TCanvas; Rect: TRect; x, Y: Integer; const Text: string);

implementation

uses
  Vcl.Forms, Vcl.Dialogs, System.SysUtils, Vcl.Clipbrd;

procedure FreeList(var List: TList);
begin
  ClearList(List);
  if Assigned(List) then
  begin
    List.Free;
    List := nil;
  end;
end;

procedure ClearList(var List: TList);
var
  i: Integer;
begin
  if not Assigned(List) then
    Exit;
  for i := 0 to List.Count - 1 do
  if Assigned(List[i]) then
  begin
    TObject(List[i]).Free;
    List[i] := nil;
  end;
  List.Clear;
end;

function MessageDialog(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons): Integer;
begin
  with CreateMessageDialog(Msg, DlgType, Buttons) do
  try
    HelpContext := 0;
    HelpFile := '';
    Position := poMainFormCenter;
    Result := ShowModal;
  finally
    Free;
  end;
end;

function MinMax(Value, MinValue, MaxValue: Integer): Integer;
begin
  Value := Min(Value, MaxValue);
  Result := Max(Value, MinValue);
end;

function GetHasTabs(Line: PChar; var CharsBefore: Integer): Boolean;
begin
  Result := False;
  CharsBefore := 0;
  if Assigned(Line) then
  begin
    while Line^ <> BCEDITOR_NONE_CHAR do
    begin
      if Line^ = BCEDITOR_TAB_CHAR then
        Exit(True);
      Inc(CharsBefore);
      Inc(Line);
    end;
  end
end;

function ConvertTabs1(const Line: string; TabWidth: Integer; var HasTabs: Boolean): string;
//var
//  pDest: PChar;
//  nBeforeTab: Integer;
begin
 { Result := Line; // increment reference count only
  if GetHasTabs(Pointer(Line), nBeforeTab) then
  begin
    HasTabs := True;
    pDest := @Result[nBeforeTab + 1]; // this will make a copy of Line
    // We have at least one tab in the string, and the tab width is 1.
    // pDest points to the first tab char. We overwrite all tabs with spaces.
    repeat
      if (pDest^ = BCEDITOR_TAB_CHAR) then
        pDest^ := ' ';
      Inc(pDest);
    until (pDest^ = BCEDITOR_NONE_CHAR);
  end
  else
    HasTabs := False;   }
end;

function ConvertTabs2n(const Line: string; TabWidth: Integer; var HasTabs: Boolean): string;
var
  i, DestLen, TabCount, TabMask: Integer;
  pSrc, pDest: PChar;
begin
  Result := Line; // increment reference count only
  if GetHasTabs(Pointer(Line), DestLen) then
  begin
    HasTabs := True;
    pSrc := @Line[1 + DestLen];
    // We have at least one tab in the string, and the tab width equals 2^n.
    // pSrc points to the first tab char in Line. We get the number of tabs
    // and the length of the expanded string now.
    TabCount := 0;
    TabMask := (TabWidth - 1) xor $7FFFFFFF;
    repeat
      if pSrc^ = BCEDITOR_TAB_CHAR then
      begin
        DestLen := (DestLen + TabWidth) and TabMask;
        Inc(TabCount);
      end
      else
        Inc(DestLen);
      Inc(pSrc);
    until (pSrc^ = BCEDITOR_NONE_CHAR);
    // Set the length of the expanded string.
    SetLength(Result, DestLen);
    DestLen := 0;
    pSrc := PChar(Line);
    pDest := PChar(Result);
    // We use another TabMask here to get the difference to 2^n.
    TabMask := TabWidth - 1;
    repeat
      if pSrc^ = BCEDITOR_TAB_CHAR then
      begin
        i := TabWidth - (DestLen and TabMask);
        Inc(DestLen, i);
        // This is used for both drawing and other stuff and is meant to be BCEDITOR_TAB_CHAR and not BCEDITOR_SPACE_CHAR
        repeat
          pDest^ := BCEDITOR_TAB_CHAR;
          Inc(pDest);
          Dec(i);
        until (i = 0);
        Dec(TabCount);
        if TabCount = 0 then
        begin
          repeat
            Inc(pSrc);
            pDest^ := pSrc^;
            Inc(pDest);
          until (pSrc^ = BCEDITOR_NONE_CHAR);
          Exit;
        end;
      end
      else
      begin
        pDest^ := pSrc^;
        Inc(pDest);
        Inc(DestLen);
      end;
      Inc(pSrc);
    until (pSrc^ = BCEDITOR_NONE_CHAR);
  end
  else
    HasTabs := False;
end;

function ConvertTabs(const Line: string; TabWidth: Integer; var HasTabs: Boolean): string;
var
  pSrc{, pDest}: PChar;
begin
  HasTabs := False;
  Result := '';
  pSrc := PChar(Line);
  while pSrc^ <> BCEDITOR_NONE_CHAR do
  begin
    if pSrc^ = BCEDITOR_TAB_CHAR then
    begin
      HasTabs := True;
      Result := Result + StringOfChar(BCEDITOR_SPACE_CHAR, TabWidth);
    end
    else
      Result := Result + pSrc^;
    Inc(pSrc);
  end;

  {Result := Line; // increment reference count only
  if GetHasTabs(Pointer(Line), DestLen) then
  begin
    HasTabs := True;
    pSrc := @Line[1 + DestLen];
    // We have at least one tab in the string, and the tab width is greater
    // than 1. pSrc points to the first tab char in Line. We get the number
    // of tabs and the length of the expanded string now.
    TabCount := 0;
    repeat
      if pSrc^ = BCEDITOR_TAB_CHAR then
      begin
        DestLen := DestLen + TabWidth - DestLen mod TabWidth;
        Inc(TabCount);
      end
      else
        Inc(DestLen);
      Inc(pSrc);
    until (pSrc^ = BCEDITOR_NONE_CHAR);
    // Set the length of the expanded string.
    SetLength(Result, DestLen);
    DestLen := 0;
    pSrc := PChar(Line);
    pDest := PChar(Result);
    repeat
      if pSrc^ = BCEDITOR_TAB_CHAR then
      begin
        i := TabWidth - (DestLen mod TabWidth);
        Inc(DestLen, i);
        repeat
          pDest^ := BCEDITOR_TAB_CHAR;
          Inc(pDest);
          Dec(i);
        until (i = 0);
        Dec(TabCount);
        if TabCount = 0 then
        begin
          repeat
            Inc(pSrc);
            pDest^ := pSrc^;
            Inc(pDest);
          until (pSrc^ = BCEDITOR_NONE_CHAR);
          Exit;
        end;
      end
      else
      begin
        pDest^ := pSrc^;
        Inc(pDest);
        Inc(DestLen);
      end;
      Inc(pSrc);
    until (pSrc^ = BCEDITOR_NONE_CHAR);
  end
  else
    HasTabs := False;  }
end;

function IsPowerOfTwo(TabWidth: Integer): Boolean;
var
  nW: Integer;
begin
  nW := 2;
  repeat
    if (nW >= TabWidth) then
      Break;
    Inc(nW, nW);
  until (nW >= $10000); // we don't want 64 kByte spaces...
  Result := (nW = TabWidth);
end;

function GetBestTabConvertProc(TabWidth: Integer): TBCEditorTabConvertProc;
begin
  {if (TabWidth < 2) then
    Result := TBCEditorTabConvertProc(@ConvertTabs1)
  else
  if IsPowerOfTwo(TabWidth) then
    Result := TBCEditorTabConvertProc(@ConvertTabs2n)
  else      }
    Result := TBCEditorTabConvertProc(@ConvertTabs);
end;

function GetLeadingExpandedLength(const AStr: string; ATabWidth: Integer; ABorder: Integer = 0): Integer;
var
  iRun: PChar;
  Len: Integer;
begin
  Result := 0;
  iRun := PChar(AStr);
  if ABorder > 0 then
    Len := Min(PInteger(iRun - 2)^, ABorder)
  else
    Len := PInteger(iRun - 2)^;
  while Len > 0 do
  begin
    if iRun^ = BCEDITOR_TAB_CHAR then
      Inc(Result, ATabWidth - (Result mod ATabWidth))
    else
    if iRun^ = BCEDITOR_SPACE_CHAR then
      Inc(Result)
    else
      Exit;
    Inc(iRun);
    Dec(Len);
  end;
end;

{
 Full-width ranges:

 U-16  U-8              U-16  U-8
 1100  e1 84 80     ..  115F  e1 85 9f
 2329  e2 8c a9     ..  232A  e2 8c aa
 2E80  e2 ba 80     ..  303E  e3 80 be
 3041  e3 81 81     ..  33FF  e3 8f bf
 3400  e3 90 80     ..  4DB5  e4 b6 b5
 4E00  e4 b8 80     ..  9FC3  e9 bf 83
 A000  ea 80 80     ..  A4C6  ea 93 86
 AC00  ea b0 80     ..  D7A3  ed 9e a3
 F900  ef a4 80     ..  FAD9  ef ab 99
 FE10  ef b8 90     ..  FE19  ef b8 99
 FE30  ef b8 b0     ..  FE6B  ef b9 ab
 FF01  ef bc 81     ..  FF60  ef bd a0
 FFE0  ef bf a0     ..  FFE6  ef bf a6
 20000 f0 a0 80 80  ..  2FFFD f0 af bf bd
 30000 f0 b0 80 80  ..  3FFFD f0 bf bf bd

 TODO:
 Actually, they are incomplete. One can easily find character out of this
 range which is actually full-width but doesn't fall in any of ranges above.
 Also, there are characters even wider than full-width, which take 3 or even 4
 regular spaces in text }
function CharWidthTable(AChar: Char): SmallInt;
begin
  Result := 1;
  if (AChar >= #$1100) and (AChar <= #$115F) then
    Result := 2
  else
  if (AChar >= #$2329) and (AChar <= #$232A) then
    Result := 2
  else
  if (AChar >= #$2E80) and (AChar <= #$303E) then
    Result := 2
  else
  if (AChar >= #$3041) and (AChar <= #$33FF) then
    Result := 2
  else
  if (AChar >= #$3400) and (AChar <= #$4DB5) then
    Result := 2
  else
  if (AChar >= #$4E00) and (AChar <= #$9FC3) then
    Result := 2
  else
  if (AChar >= #$A000) and (AChar <= #$A4C6) then
    Result := 2
  else
  if (AChar >= #$AC00) and (AChar <= #$D7A3) then
    Result := 2
  else
  if (AChar >= #$F900) and (AChar <= #$FAD9) then
    Result := 2
  else
  if (AChar >= #$FE10) and (AChar <= #$FE19) then
    Result := 2
  else
  if (AChar >= #$FE30) and (AChar <= #$FE6B) then
    Result := 2
  else
  if (AChar >= #$FF01) and (AChar <= #$FF60) then
    Result := 2
  else
  if (AChar >= #$FFE0) and (AChar <= #$FFE6) then
    Result := 2;
end;

function GetTextSize(AHandle: HDC; AText: PChar; ACount: Integer): TSize;
begin
  Result.cx := 0;
  Result.cy := 0;
  GetTextExtentPoint32W(AHandle, AText, ACount, Result);
end;

type
  TAccessCanvas = class(TCanvas)
  end;

function TextExtent(ACanvas: TCanvas; const Text: string): TSize;
begin
  with TAccessCanvas(ACanvas) do
  begin
    RequiredState([csHandleValid, csFontValid]);
    Result := GetTextSize(Handle, PChar(Text), Length(Text));
  end;
end;

function TextWidth(ACanvas: TCanvas; const Text: string): Integer;
begin
  Result := TextExtent(ACanvas, Text).cx;
end;

function TextHeight(ACanvas: TCanvas; const Text: string): Integer;
begin
  Result := TextExtent(ACanvas, Text).cy;
end;

procedure TextOut(ACanvas: TCanvas; x, Y: Integer; const Text: string);
begin
  with TAccessCanvas(ACanvas) do
  begin
    Changing;
    RequiredState([csHandleValid, csFontValid, csBrushValid]);
    if CanvasOrientation = coRightToLeft then
      Inc(x, BCEditor.Utils.TextWidth(ACanvas, Text) + 1);
    Winapi.Windows.ExtTextOutW(Handle, x, Y, TextFlags, nil, PChar(Text), Length(Text), nil);
    MoveTo(x + BCEditor.Utils.TextWidth(ACanvas, Text), Y);
    Changed;
  end;
end;

procedure TextRect(ACanvas: TCanvas; Rect: TRect; X, Y: Integer; const Text: string);
var
  Options: Longint;
begin
  with TAccessCanvas(ACanvas) do
  begin
    Changing;
    RequiredState([csHandleValid, csFontValid, csBrushValid]);
    Options := ETO_CLIPPED or TextFlags;
    if Brush.Style <> bsClear then
      Options := Options or ETO_OPAQUE;
    if ((TextFlags and ETO_RTLREADING) <> 0) and (CanvasOrientation = coRightToLeft) then
      Inc(X, BCEditor.Utils.TextWidth(ACanvas, Text) + 1);
    Winapi.Windows.ExtTextOutW(Handle, X, Y, Options, @Rect, PChar(Text), Length(Text), nil);
    Changed;
  end;
end;

function RoundCorrect(Value: Real): LongInt;
begin
  Result:= Trunc(Value);
  if Frac(Value) >= 0.5 then
    Result := Result + 1;
end;

end.

unit BCEditor.Print.Types;

interface

uses
  System.Classes, System.SysUtils;

const
  BCEDITOR_DEFAULT_LEFT_MARGIN_MM = 20;
  BCEDITOR_DEFAULT_RIGHT_MARGIN_MM = 15;
  BCEDITOR_DEFAULT_TOP_MARGIN_MM = 18;
  BCEDITOR_DEFAULT_BOTTOM_MM = 18;
  BCEDITOR_DEFAULT_HEADER_MM = 15;
  BCEDITOR_DEFAULT_FOOTER_MM = 15;
  BCEDITOR_DEFAULT_LEFT_TEXT_INDENT_MM = 2;
  BCEDITOR_DEFAULT_RIGHT_TEXT_INDENT_MM = 2;
  BCEDITOR_DEFAULT_INTERNAL_MARGIN_MM = 0.5;
  BCEDITOR_DEFAULT_MARGIN_MM = 0;

type
  TBCEditorFrameType = (ftLine, ftBox, ftShaded);
  TBCEditorFrameTypes = set of TBCEditorFrameType;
  TBCEditorUnitSystem = (usMM, usCm, usInch, muThousandthsOfInches);
  TBCEditorPrintStatus = (psBegin, psNewPage, psEnd);
  TBCEditorPrintStatusEvent = procedure(Sender: TObject; Status: TBCEditorPrintStatus; PageNumber: Integer;
    var Abort: Boolean) of object;
  TBCEditorPrintLineEvent = procedure(Sender: TObject; LineNumber, PageNumber: Integer) of object;

type
  TBCEditorWrapPosition = class
  public
    Index: Integer;
  end;

function IntToRoman(Value: Integer): string;
function WrapTextEx(const Line: string; BreakChars: TSysCharSet; MaxCol: Integer; AList: TList): Boolean;

implementation

function WrapTextEx(const Line: string; BreakChars: TSysCharSet; MaxCol: Integer; AList: TList): Boolean;
var
  WrapPosition: TBCEditorWrapPosition;
  Position, PreviousPosition: Integer;
  Found: Boolean;
begin
  if Length(Line) <= MaxCol then
  begin
    Result := True;
    Exit;
  end;

  Result := False;
  Position := 1;
  PreviousPosition := 0;
  WrapPosition := TBCEditorWrapPosition.Create;
  while Position <= Length(Line) do
  begin
    Found := (Position - PreviousPosition > MaxCol) and (WrapPosition.Index <> 0);
    if not Found and (Line[Position] <= High(Char)) and CharInSet(Char(Line[Position]), BreakChars) then
      WrapPosition.Index := Position;

    if Found then
    begin
      Result := True;
      AList.Add(WrapPosition);
      PreviousPosition := WrapPosition.Index;

      if ((Length(Line) - PreviousPosition) > MaxCol) and (Position < Length(Line)) then
        WrapPosition := TBCEditorWrapPosition.Create
      else
        Break;
    end;
    Inc(Position);
  end;

  if (AList.Count = 0) or (AList.Last <> WrapPosition) then
    WrapPosition.Free;
end;

function IntToRoman(Value: Integer): string;
begin
  Result := '';
  while Value >= 1000 do
  begin
    Result := Result + 'M';
    Value := Value - 1000;
  end;

  if Value >= 900 then
  begin
    Result := Result + 'CM';
    Value := Value - 900;
  end;

  while Value >= 500 do
  begin
    Result := Result + 'D';
    Value := Value - 500;
  end;

  if Value >= 400 then
  begin
    Result := Result + 'CD';
    Value := Value - 400;
  end;

  while Value >= 100 do
  begin
    Result := Result + 'C';
    Value := Value - 100;
  end;

  if Value >= 90 then
  begin
    Result := Result + 'XC';
    Value := Value - 90;
  end;

  while Value >= 50 do
  begin
    Result := Result + 'L';
    Value := Value - 50;
  end;

  if Value >= 40 then
  begin
    Result := Result + 'XL';
    Value := Value - 40;
  end;

  while Value >= 10 do
  begin
    Result := Result + 'X';
    Value := Value - 10;
  end;

  if Value >= 9 then
  begin
    Result := Result + 'IX';
    Value := Value - 9;
  end;

  while Value >= 5 do
  begin
    Result := Result + 'V';
    Value := Value - 5;
  end;

  if Value >= 4 then
  begin
    Result := Result + 'IV';
    Value := Value - 4;
  end;

  while Value > 0 do
  begin
    Result := Result + 'I';
    Dec(Value);
  end;
end;

end.

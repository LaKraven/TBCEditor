unit BCEditor.Editor.InternalImage;

interface

uses
  Vcl.Graphics;

type
  TBCEditorInternalImage = class(TObject)
  strict private
    FCount: Integer;
    FHeight: Integer;
    FImages: Vcl.Graphics.TBitmap;
    FWidth: Integer;
    function CreateBitmapFromInternalList(AModule: THandle; const Name: string): Vcl.Graphics.TBitmap;
    procedure FreeBitmapFromInternalList;
  public
    constructor Create(AModule: THandle; const Name: string; const Count: Integer);
    destructor Destroy; override;

    procedure Draw(ACanvas: TCanvas; const Number: Integer; const X: Integer; const Y: Integer; const LineHeight: Integer);
    procedure DrawTransparent(ACanvas: TCanvas; const Number: Integer; const X: Integer; const Y: Integer;
      const LineHeight: Integer; const TransparentColor: TColor);
  end;

implementation

uses
  Winapi.Windows, System.Classes, System.Types, System.SysUtils;

type
  TInternalResource = class(TObject)
  public
    UsageCount: Integer;
    Name: string;
    Bitmap: Vcl.Graphics.TBitmap;
  end;

var
  InternalResources: TList;

{ TBCEditorInternalImage }

constructor TBCEditorInternalImage.Create(AModule: THandle; const Name: string; const Count: Integer);
begin
  inherited Create;
  FImages := CreateBitmapFromInternalList(AModule, name);
  FWidth := (FImages.Width + Count shr 1) div Count;
  FHeight := FImages.Height;
  FCount := Count;
end;

destructor TBCEditorInternalImage.Destroy;
begin
  FreeBitmapFromInternalList;

  inherited Destroy;
end;

function TBCEditorInternalImage.CreateBitmapFromInternalList(AModule: THandle; const Name: string): Vcl.Graphics.TBitmap;
var
  i: Integer;
  InternalResource: TInternalResource;
begin
  for i := 0 to InternalResources.Count - 1 do
    if TInternalResource(InternalResources[i]).Name = UpperCase(Name) then
      with TInternalResource(InternalResources[i]) do
      begin
        UsageCount := UsageCount + 1;
        Result := Bitmap;
        Exit;
      end;

  Result := Vcl.Graphics.TBitmap.Create;
  Result.Handle := LoadBitmap(AModule, PChar(name));

  InternalResource := TInternalResource.Create;
  with InternalResource do
  begin
    UsageCount := 1;
    Name := UpperCase(Name);
    Bitmap := Result;
  end;
  InternalResources.Add(InternalResource);
end;

procedure TBCEditorInternalImage.FreeBitmapFromInternalList;
var
  i: Integer;
  InternalResource: TInternalResource;

  function FindImageIndex: Integer;
  begin
    for Result := 0 to InternalResources.Count - 1 do
      if TInternalResource(InternalResources[Result]).Bitmap = FImages then
        Exit;
    Result := -1;
  end;

begin
  i := FindImageIndex;
  if i = -1 then
    Exit;

  InternalResource := TInternalResource(InternalResources[i]);
  with InternalResource do
  begin
    UsageCount := UsageCount - 1;
    if UsageCount = 0 then
    begin
      Bitmap.Free;
      Bitmap := nil;
      InternalResources.Delete(i);
      InternalResource.Free;
    end;
  end;
end;

procedure TBCEditorInternalImage.Draw(ACanvas: TCanvas; const Number: Integer; const X: Integer; const Y: Integer;
  const LineHeight: Integer);
var
  SourceRect, DestinationRect: TRect;
  LY: Integer;
begin
  if (Number >= 0) and (Number < FCount) then
  begin
    LY := Y;
    if LineHeight >= FHeight then
    begin
      SourceRect := Rect(Number * FWidth, 0, (Number + 1) * FWidth, FHeight);
      Inc(LY, (LineHeight - FHeight) div 2);
      DestinationRect := Rect(X, LY, X + FWidth, LY + FHeight);
    end
    else
    begin
      DestinationRect := Rect(X, LY, X + FWidth, LY + LineHeight);
      LY := (FHeight - LineHeight) div 2;
      SourceRect := Rect(Number * FWidth, LY, (Number + 1) * FWidth, LY + LineHeight);
    end;
    ACanvas.CopyRect(DestinationRect, FImages.Canvas, SourceRect);
  end;
end;

procedure TBCEditorInternalImage.DrawTransparent(ACanvas: TCanvas; const Number: Integer; const X: Integer;
  const Y: Integer; const LineHeight: Integer; const TransparentColor: TColor);
var
  SourceRect, DestinationRect: TRect;
  LY: Integer;
begin
  LY := Y;
  if (Number >= 0) and (Number < FCount) then
  begin
    if LineHeight >= FHeight then
    begin
      SourceRect := Rect(Number * FWidth, 0, (Number + 1) * FWidth, FHeight);
      Inc(LY, (LineHeight - FHeight) div 2);
      DestinationRect := Rect(X, LY, X + FWidth, LY + FHeight);
    end
    else
    begin
      DestinationRect := Rect(X, LY, X + FWidth, LY + LineHeight);
      LY := (FHeight - LineHeight) div 2;
      SourceRect := Rect(Number * FWidth, LY, (Number + 1) * FWidth, LY + LineHeight);
    end;
    ACanvas.BrushCopy(DestinationRect, FImages, SourceRect, TransparentColor);
  end;
end;

initialization

  InternalResources := TList.Create;

finalization

  InternalResources.Free;
  InternalResources := nil;

end.

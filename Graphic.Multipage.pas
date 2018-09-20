unit Graphic.Multipage;

// Created by Glen Kleidon 2018 - released under MIT license
// Re-implementation of some functions from Borland/CodeGear/Embarcadero
// VCL.graphics and Win.ActiveX units.

interface

uses Classes, Graphics, ExtCtrls;

type
  TIMageHelper = Class Helper for TImage
  private
    function GetPageNumber: Integer;
    procedure SetPageNumber(const Value: Integer);
    function GetPageCount: integer;
  public
    Property PageNumber: Integer read GetPageNumber write SetPageNumber;
    Property PageCount : integer read GetPageCount;
  End;

  TPictureHelper = Class Helper for TPicture
  private
    function GetPageCount: Integer;
  public
    Procedure LoadFromMultiPageFile(const Filename: string; APageNo: Integer);
    Procedure LoadFromMultiPageStream(Stream: TStream; PageNo: Integer;
      FileType: String = 'tif');
    Procedure SetPageNo(PageNo: Integer);
    Property PageCount: Integer read GetPageCount;
  End;

  TWICImageHelper = Class Helper for TWICImage
  private
    Function GetPageCount: Integer; overload;
  public
    Procedure LoadFromFile(const Filename: string; APageNo: Integer); overload;
    Procedure LoadFromStream(Stream: TStream; APageNo: Integer); overload;
    Function GetPageCount(const Filename: string): Integer; overload;
    Function GetPageCount(Stream: TStream): Integer; overload;
    Procedure SetPageNo(PageNo: Integer);
    Property PageCount: Integer read GetPageCount;
  End;

implementation

uses SysUtils, Wincodec, Consts, ActiveX;

procedure WicCheck(Result: HResult);
begin
  if Failed(Result) then
    raise EInvalidGraphic.Create(SInvalidImage);
end;

{ TWICImageHelper }

function TWICImageHelper.GetPageCount(const Filename: string): Integer;
var
  lStream: TFileStream;
begin

  lStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  try
    Result := GetPageCount(lStream);
  finally
    lStream.free;
  end;

end;

function TWICImageHelper.GetPageCount(Stream: TStream): Integer;
var
  lStream: TStreamAdapter;
  BitmapDecoder: IWICBitmapDecoder;
  lPageCount: Cardinal;

begin
  lStream := TStreamAdapter.Create(Stream);

  WicCheck(self.FImagingFactory.CreateDecoderFromStream(lStream, guid_null,
    WICDecodeMetadataCacheOnDemand, BitmapDecoder));
  BitmapDecoder.GetFrameCount(lPageCount);
  Result := lPageCount;
  lStream := nil;
end;

function TWICImageHelper.GetPageCount: Integer;
begin
  Result := -1;
  if self.fData <> nil then
  begin
    self.fData.Position := 0;
    Result := self.GetPageCount(self.fData);
  end;
end;

procedure TWICImageHelper.LoadFromFile(const Filename: string;
  APageNo: Integer);
var
  lStream: TFileStream;
begin
  if not sameText(copy(ExtractFileExt(Filename), 2, 3), 'tif') then
  begin
    self.LoadFromFile(Filename);
    exit;
  end;

  lStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(lStream, APageNo);
  finally
    lStream.free;
  end;

end;

procedure TWICImageHelper.LoadFromStream(Stream: TStream; APageNo: Integer);
var
  lStream: TStreamAdapter;
  BitmapDecoder: IWICBitmapDecoder;
  LBitmapFrame: IWICBitmapFrameDecode;
  // LGUID: TGUID;

  procedure WicCheck(Result: HResult);
  begin
    if Failed(Result) then
      raise EInvalidGraphic.Create(SInvalidImage);
  end;

begin
  freeandNil(self.FBitmap);
  if (Stream = nil) then
  begin
    if self.fData = nil then
      WicCheck(1);
  end
  else
  begin
    self.fData.Clear;
    self.fData.CopyFrom(Stream, Stream.Size - Stream.Position);
  end;
  self.fData.Position := 0;

  lStream := TStreamAdapter.Create(self.fData);

  WicCheck(self.FImagingFactory.CreateDecoderFromStream(lStream, guid_null,
    WICDecodeMetadataCacheOnDemand, BitmapDecoder));
  // WicCheck(BitmapDecoder.GetContainerFormat(LGUID));
  // EncoderContainerFormat := LGUID;
  WicCheck(BitmapDecoder.GetFrame(APageNo, LBitmapFrame));
  WicCheck(self.FImagingFactory.CreateBitmapFromSource(LBitmapFrame,
    WICBitmapCacheOnLoad, self.FWicBitmap));
  WicCheck(self.FWicBitmap.GetSize(self.FWidth, self.FHeight));
  lStream := nil;
  self.FFormatChanged := False;
end;

procedure TWICImageHelper.SetPageNo(PageNo: Integer);
begin
  if self.fData <> nil then
  begin
    self.fData.Position := 0;
    // Passing in nil tells the helper to use the existing data
    self.LoadFromStream(Nil, PageNo);
  end;
end;

{ TPictureHelper }

function TPictureHelper.GetPageCount: Integer;
begin
  if self.FGraphic.InheritsFrom(TWICImage) then
    Result := TWICImage(self.FGraphic).PageCount
  else
    Result := 1;
end;

procedure TPictureHelper.LoadFromMultiPageFile(const Filename: string;
  APageNo: Integer);
var
  NewGraphic: TWICImage;
begin
  if not sameText(copy(ExtractFileExt(Filename), 2, 3), 'tif') then
  begin
    self.LoadFromFile(Filename);
    exit;
  end;

  NewGraphic := TWICImage.Create;
  try
    NewGraphic.OnProgress := Progress;
    NewGraphic.LoadFromFile(Filename, APageNo);
  except
    NewGraphic.free;
    raise;
  end;
  self.FGraphic.free;
  self.FGraphic := NewGraphic;
  self.FGraphic.OnChange := Changed;
  Changed(self);

end;

procedure TPictureHelper.LoadFromMultiPageStream(Stream: TStream;
  PageNo: Integer; FileType: String = 'tif');
begin
  if not pos('tif',lowercase(FileType))<1 then
    self.LoadFromStream(Stream)
  else
    self.LoadFromMultiPageStream(Stream, PageNo);

end;

procedure TPictureHelper.SetPageNo(PageNo: Integer);
begin
  if Assigned(self.Graphic) and (self.Graphic.InheritsFrom(TWICImage)) then
    TWICImage(self.Graphic).SetPageNo(PageNo);

end;

{ TIMageHelper }

function TIMageHelper.GetPageCount: integer;
begin
  result := self.Picture.PageCount;
end;

function TIMageHelper.GetPageNumber: Integer;
begin
  Result := 0;
  if not Assigned(self.Picture.Graphic) then
    exit;
  if self.Picture.Graphic.InheritsFrom(TWICImage) then
    Result := self.Tag;

end;

procedure TIMageHelper.SetPageNumber(const Value: Integer);
begin
  if not Assigned(self.Picture.Graphic) then
    exit;
  if self.Picture.Graphic.InheritsFrom(TWICImage) then
  begin
    self.Picture.SetPageNo(Value);
    self.Tag := Value;
    self.Repaint;
  end;

end;

end.

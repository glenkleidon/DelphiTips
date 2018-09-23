unit MultiPageImage;

interface

uses

{$IF CompilerVersion >= 23.0}
{$DEFINE UPDATED_CLIPBOARD}
{$IFEND}
{$IF DEFINED(CLR)}
  System.Drawing, System.Drawing.Imaging, System.Reflection,
  System.Globalization,
{$IFEND}
{$IF DEFINED(LINUX)}
  WinUtils, Libc,
{$IFEND}
  Windows, SysUtils, Classes, Graphics, ExtCtrls
{$IF NOT DEFINED(CLR)}
{$IF CompilerVersion < 21.0}
  // Delphi 2009 did not support WinCodec
    , Wincodec_PRE_D2010
{$ELSE}
    , Wincodec
{$IFEND}
{$IFEND}
    ;

type

  TImageHelper = Class Helper for TImage
  private
    function GetPageNumber: Integer;
    procedure SetPageNumber(const Value: Integer);
    function GetPageCount: Integer;
  public
    Property PageNumber: Integer read GetPageNumber write SetPageNumber;
    Property PageCount: Integer read GetPageCount;
  End;

  TPictureHelper = Class Helper for TPicture
  private
    function GetPageCount: Integer;
    procedure SetSupportsMultipage(const Value: Boolean);
    function GetSupportsMultipage: Boolean;
    function GetPageNo: Integer;
    Procedure SetPageNo(const PageNo: Integer);
  public
    Procedure LoadFromMultiPageFile(const Filename: string; APageNo: Integer);
    Procedure LoadFromMultiPageStream(Stream: TStream; PageNo: Integer;
      FileType: String = 'tif');
    Property PageCount: Integer read GetPageCount;
    Property PageNumber: Integer read GetPageNo write SetPageNo;
    Property SupportsMultipage: Boolean read GetSupportsMultipage
      write SetSupportsMultipage;
  End;
{$IF CompilerVersion < 21.0}

  TWICImageFormat = (wifBmp, wifPng, wifJpeg, wifGif, wifTiff, wifWMPhoto,
    wifOther);
{$IFEND}

// --- MOST OF THIS WORK IS COPYRIGHT OF Embarcadero
{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2011 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}
// Users of Delphi 2010 and later already have permission
// to use this code as it essentially duplicated from the
// GRAPHICS unit of those versions of DELPHI.
// Restrictions in Delphi 10.0 and later prevent the helper
// methods from accessing the private members of the TWICImage
// class and therefore requires that the code be duplicated.


  TMPWICImage = class(TGraphic)
  private
    FPageNo: Integer;
    FWidth, FHeight: Cardinal;
    FBitmap: TBitmap;
    FData: TMemoryStream;
    FWicBitmap: IWICBitmap;

    FImageFormat: TWICImageFormat;
    FEncoderContainerFormat: TGUID;
    FFormatChanged: Boolean;

    procedure SetImageFormat(const Value: TWICImageFormat);
    procedure SetEncoderContainerFormat(const Value: TGUID);
    procedure CreateWicBitmap;
    function GetHandle: IWICBitmap;
    procedure SetHandle(const Value: IWICBitmap);

    class var FImagingFactory: IWICImagingFactory;
    class function GetImagingFactory: IWICImagingFactory; static;
  protected
    Function GetPageCount: Integer; overload;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
    procedure RequireBitmap;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromFile(const Filename: string); overload;
    procedure LoadFromFile(const Filename: string; APageNo: Integer); overload;
    procedure LoadFromStream(Stream: TStream); overload; override;
    procedure LoadFromStream(Stream: TStream; APageNo: Integer); overload;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromClipboardFormat(AFormat: Word;
{$IFNDEF UPDATED_CLIPBOARD}
      AData: Cardinal;
{$ELSE}
      AData: THandle;
{$ENDIF}
      APalette: HPALETTE); override;
    property Handle: IWICBitmap read GetHandle write SetHandle;
    property ImageFormat: TWICImageFormat read FImageFormat
      write SetImageFormat;
    property EncoderContainerFormat: TGUID read FEncoderContainerFormat
      write SetEncoderContainerFormat;
    Function GetPageCount(const Filename: string): Integer; overload;
    Function GetPageCount(Stream: TStream): Integer; overload;
    Procedure SetPageNo(const PageNo: Integer);
    Property PageCount: Integer read GetPageCount;
    Property PageNo: Integer read FPageNo write SetPageNo;

    class property ImagingFactory: IWICImagingFactory read GetImagingFactory;
  end;

{$IF CompilerVersion < 21.0}

resourceString
  SChangeWicSize = 'Cannot change the size of a WIC Image';
{$IFEND}

implementation

uses
  Types, Consts, ActiveX
{$IF CompilerVersion < 21.0}
    , PropertyBagInterfaces
{$IFEND}
    ;

procedure InvalidOperation(Str: PResStringRec);
begin
  raise EInvalidGraphicOperation.CreateRes(Str);
end;

procedure WicCheck(Result: HRESULT);
begin
  if Failed(Result) then
    raise EInvalidGraphic.Create(SInvalidImage);
end;

{ TMPWICGraphic }

procedure TMPWICImage.Assign(Source: TPersistent);
var
  LImage: TMPWICImage;
begin
  if Source is TMPWICImage then
  begin
    LImage := TMPWICImage(Source);

    if Assigned(LImage.FWicBitmap) then
      FImagingFactory.CreateBitmapFromSource(LImage.FWicBitmap,
        WICBitmapNoCache, FWicBitmap);

    if Assigned(LImage.FBitmap) then
    begin
      if FBitmap = nil then
        FBitmap := TBitmap.Create;

      try
        FBitmap.Assign(LImage.FBitmap);
      except
        FreeAndNil(FBitmap);
        raise;
      end;
    end;

    FEncoderContainerFormat := LImage.FEncoderContainerFormat;
    FImageFormat := LImage.FImageFormat;
    FWidth := LImage.FWidth;
    FHeight := LImage.FHeight;
    FData.Clear;
    TMPWICImage(Source).FData.Position := 0;
    FData.CopyFrom(TMPWICImage(Source).FData, TMPWICImage(Source).FData.Size);

    FFormatChanged := TMPWICImage(Source).FFormatChanged;
  end
  else if Source is TBitmap then
  begin
    FWicBitmap := nil;
    if FBitmap = nil then
      FBitmap := TBitmap.Create;
    FBitmap.Assign(Source);
    CreateWicBitmap;
    ImageFormat := wifBmp;
  end
  else
    inherited;

end;

procedure TMPWICImage.AssignTo(Dest: TPersistent);
begin
  if Dest is TBitmap then
  begin
    RequireBitmap;
    Dest.Assign(FBitmap);
  end
  else
    inherited;
end;

class function TMPWICImage.GetImagingFactory: IWICImagingFactory;
begin
  Result := FImagingFactory;
end;

constructor TMPWICImage.Create;
begin
  inherited;
  FPageNo := -1;
  if FImagingFactory = nil then
    CoCreateInstance(CLSID_WICImagingFactory, nil, CLSCTX_INPROC_SERVER or
      CLSCTX_LOCAL_SERVER, IUnknown, FImagingFactory)
  else
    FImagingFactory._AddRef;

  FEncoderContainerFormat := GUID_ContainerFormatBmp;
  FImageFormat := wifBmp;
  FData := TMemoryStream.Create;
  FFormatChanged := False;

end;

destructor TMPWICImage.Destroy;
begin
  if Assigned(FBitmap) then
    FreeAndNil(FBitmap);
  FData.Free;
  if FImagingFactory._Release = 0 then
    Pointer(FImagingFactory) := nil;

  inherited;
end;

procedure TMPWICImage.CreateWicBitmap;
var
  PixelFormat: TGUID;
  BitmapInfo: TBitmapInfo;
  Buffer: array of byte;
begin
  FData.Clear;
  FFormatChanged := True;

  if FBitmap.AlphaFormat = afDefined then
    PixelFormat := GUID_WICPixelFormat32bppBGRA
  else
    PixelFormat := GUID_WICPixelFormat32bppBGR;

  FBitmap.PixelFormat := pf32bit;

  FWidth := FBitmap.Width;
  FHeight := FBitmap.Height;

  SetLength(Buffer, FWidth * 4 * FHeight);

  FillChar(BitmapInfo, sizeof(BitmapInfo), 0);
  BitmapInfo.bmiHeader.biSize := sizeof(BitmapInfo);
  BitmapInfo.bmiHeader.biWidth := FWidth;
  BitmapInfo.bmiHeader.biHeight := -FHeight;
  BitmapInfo.bmiHeader.biPlanes := 1;
  BitmapInfo.bmiHeader.biBitCount := 32;

  GetDIBits(FBitmap.Canvas.Handle, FBitmap.Handle, 0, FHeight, @Buffer[0],
    BitmapInfo, DIB_RGB_COLORS);

  FImagingFactory.CreateBitmapFromMemory(FWidth, FHeight, PixelFormat,
    FWidth * 4, Length(Buffer), @Buffer[0], FWicBitmap);
end;

procedure TMPWICImage.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  RequireBitmap;
  if FBitmap <> nil then
    ACanvas.StretchDraw(Rect, FBitmap);
end;

function TMPWICImage.GetEmpty: Boolean;
begin
  Result := FWicBitmap = nil;
end;

function TMPWICImage.GetHandle: IWICBitmap;
begin
  Result := FWicBitmap;
end;

function TMPWICImage.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TMPWICImage.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure TMPWICImage.LoadFromStream(Stream: TStream; APageNo: Integer);
var
  LStream: TStreamAdapter;
  BitmapDecoder: IWICBitmapDecoder;
  LBitmapFrame: IWICBitmapFrameDecode;
  LGUID: TGUID;
  doChange: Boolean;
begin
  if (Stream = nil) then
  begin
    if self.FData = nil then
      WicCheck(1);
    doChange := True;
  end
  else
  begin
    self.FData.Clear;
    self.FData.CopyFrom(Stream, Stream.Size - Stream.Position);
    doChange := False;
  end;
  self.FData.Position := 0;

  FreeAndNil(FBitmap);
  LStream := TStreamAdapter.Create(FData);

  WicCheck(FImagingFactory.CreateDecoderFromStream(LStream, guid_null,
    WICDecodeMetadataCacheOnDemand, BitmapDecoder));
  WicCheck(BitmapDecoder.GetContainerFormat(LGUID));
  EncoderContainerFormat := LGUID;
  WicCheck(BitmapDecoder.GetFrame(APageNo, LBitmapFrame));
  WicCheck(FImagingFactory.CreateBitmapFromSource(LBitmapFrame,
    WICBitmapCacheOnLoad, FWicBitmap));
  WicCheck(FWicBitmap.GetSize(FWidth, FHeight));
  if (doChange) then
    changed(self)
  else
    FFormatChanged := False;
  self.FPageNo := APageNo;
  if self.FPageNo > self.PageCount then
    self.FPageNo := self.PageCount;

end;

procedure TMPWICImage.SaveToStream(Stream: TStream);
var
  Encoder: IWICBitmapEncoder;
  Frame: IWICBitmapFrameEncode;
  Props: IPropertyBag2;
  LStreamAdapter: TStreamAdapter;
  PixelFormat: TGUID;
  LStream: IWICStream;
  Palette: IWICPalette;
begin
  if FFormatChanged then
  begin
    FData.Clear;
    LStreamAdapter := TStreamAdapter.Create(FData);
    IUnknown(LStreamAdapter)._AddRef;

    FImagingFactory.CreateStream(LStream);
    LStream.InitializeFromIStream(LStreamAdapter);
    FImagingFactory.CreateEncoder(FEncoderContainerFormat, guid_null, Encoder);

    Encoder.Initialize(LStream, WICBitmapEncoderNoCache);
    Encoder.CreateNewFrame(Frame, Props);

    Frame.Initialize(Props);
    FWicBitmap.GetPixelFormat(PixelFormat);
    Frame.SetPixelFormat(PixelFormat);

    Frame.SetSize(FWidth, FHeight);

    FImagingFactory.CreatePalette(Palette);
    FWicBitmap.CopyPalette(Palette);
    Frame.SetPalette(Palette);
    Frame.WriteSource(FWicBitmap, nil);
    Frame.Commit;
    Encoder.Commit;

    FFormatChanged := False;
  end;

  FData.Position := 0;
  Stream.CopyFrom(FData, FData.Size);
end;

procedure TMPWICImage.LoadFromClipboardFormat(AFormat: Word;
{$IFNDEF UPDATED_CLIPBOARD}
  AData: Cardinal;
{$ELSE}
  AData: THandle;
{$ENDIF}
  APalette: HPALETTE);
begin
  FWicBitmap := nil;
  if FBitmap = nil then
    FBitmap := FBitmap.Create;
  try
    FBitmap.LoadFromClipboardFormat(AFormat, AData, APalette);
    CreateWicBitmap;
  except
    FreeAndNil(FBitmap);
    raise;
  end;
end;

procedure TMPWICImage.LoadFromFile(const Filename: string);
begin
  inherited;
end;

procedure TMPWICImage.LoadFromFile(const Filename: string; APageNo: Integer);
var
  LStream: TFileStream;
begin
  self.FPageNo := 0;
  if not sameText(copy(ExtractFileExt(Filename), 2, 3), 'tif') then
  begin
    self.LoadFromFile(Filename);
    exit;
  end;

  LStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(LStream, APageNo);
  finally
    LStream.Free;
  end;
end;

procedure TMPWICImage.LoadFromStream(Stream: TStream);
begin
  Inherited LoadFromStream(Stream);
  // LoadFromStream(Stream, 0);
end;

procedure TMPWICImage.SetEncoderContainerFormat(const Value: TGUID);
begin
  if not IsEqualGUID(Value, FEncoderContainerFormat) then
  begin
    FFormatChanged := True;
    FEncoderContainerFormat := Value;
    if IsEqualGUID(Value, GUID_ContainerFormatBmp) then
      FImageFormat := wifBmp
    else if IsEqualGUID(Value, GUID_ContainerFormatPng) then
      FImageFormat := wifPng
    else if IsEqualGUID(Value, GUID_ContainerFormatJpeg) then
      FImageFormat := wifJpeg
    else if IsEqualGUID(Value, GUID_ContainerFormatTiff) then
      FImageFormat := wifTiff
    else if IsEqualGUID(Value, GUID_ContainerFormatGif) then
      FImageFormat := wifGif
    else if IsEqualGUID(Value, GUID_ContainerFormatWmp) then
      FImageFormat := wifWMPhoto
    else
      FImageFormat := wifOther;
  end;
end;

procedure TMPWICImage.SetImageFormat(const Value: TWICImageFormat);
begin
  if Value <> FImageFormat then
  begin
    FFormatChanged := True;
    FImageFormat := Value;
    case Value of
      wifBmp:
        FEncoderContainerFormat := GUID_ContainerFormatBmp;
      wifPng:
        FEncoderContainerFormat := GUID_ContainerFormatPng;
      wifJpeg:
        FEncoderContainerFormat := GUID_ContainerFormatJpeg;
      wifGif:
        FEncoderContainerFormat := GUID_ContainerFormatGif;
      wifTiff:
        FEncoderContainerFormat := GUID_ContainerFormatTiff;
      wifWMPhoto:
        FEncoderContainerFormat := GUID_ContainerFormatWmp;
      wifOther:
        ;
    end;
  end;
end;

procedure TMPWICImage.SetHandle(const Value: IWICBitmap);
begin
  if Assigned(FBitmap) then
    FreeAndNil(FBitmap);
  FWicBitmap := nil;

  FData.Clear;
  FFormatChanged := True;

  FImagingFactory.CreateBitmapFromSource(Value, WICBitmapCacheOnLoad,
    FWicBitmap);
  FWicBitmap.GetSize(FWidth, FHeight);
end;

procedure TMPWICImage.SetHeight(Value: Integer);
begin
  InvalidOperation(@SChangeWicSize);
end;

procedure TMPWICImage.SetWidth(Value: Integer);
begin
  InvalidOperation(@SChangeWicSize);
end;

procedure TMPWICImage.RequireBitmap;
var
  LWicBitmap: IWICBitmapSource;
  Stride: Cardinal;
  Buffer: array of byte;
  BitmapInfo: TBitmapInfo;
begin
  if Assigned(FBitmap) then
    exit;
  if FWicBitmap = nil then
    exit;

  FWicBitmap.GetSize(FWidth, FHeight);
  Stride := FWidth * 4;
  SetLength(Buffer, Stride * FHeight);

  WICConvertBitmapSource(GUID_WICPixelFormat32bppBGRA, FWicBitmap, LWicBitmap);
  LWicBitmap.CopyPixels(nil, Stride, Length(Buffer), @Buffer[0]);

  FillChar(BitmapInfo, sizeof(BitmapInfo), 0);
  BitmapInfo.bmiHeader.biSize := sizeof(BitmapInfo);
  BitmapInfo.bmiHeader.biWidth := FWidth;
  BitmapInfo.bmiHeader.biHeight := -FHeight;
  BitmapInfo.bmiHeader.biPlanes := 1;
  BitmapInfo.bmiHeader.biBitCount := 32;

  FBitmap := TBitmap.Create;
  try
    FBitmap.PixelFormat := pf32bit;
    FBitmap.SetSize(FWidth, FHeight);
    SetDIBits(FBitmap.Canvas.Handle, FBitmap.Handle, 0, FHeight, @Buffer[0],
      BitmapInfo, DIB_RGB_COLORS);
    FBitmap.AlphaFormat := afDefined;
  except
    FreeAndNil(FBitmap);
    raise;
  end;
end;

function TMPWICImage.GetPageCount(const Filename: string): Integer;
var
  LStream: TFileStream;
begin

  LStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  try
    Result := GetPageCount(LStream);
  finally
    LStream.Free;
  end;

end;

function TMPWICImage.GetPageCount(Stream: TStream): Integer;
var
  LStream: TStreamAdapter;
  BitmapDecoder: IWICBitmapDecoder;
  lPageCount: LongWord;

begin
  LStream := TStreamAdapter.Create(Stream);

  WicCheck(self.ImagingFactory.CreateDecoderFromStream(LStream, guid_null,
    WICDecodeMetadataCacheOnDemand, BitmapDecoder));
  BitmapDecoder.GetFrameCount(lPageCount);
  Result := Integer(lPageCount);
  LStream := nil;
end;

function TMPWICImage.GetPageCount: Integer;
var
  lCount: LongWord;
begin
  Result := -1;
  if self.FData <> nil then
  begin
    self.FData.Position := 0;
    lCount := self.GetPageCount(self.FData);
  end;
  Result := lCount;
end;

procedure TMPWICImage.SetPageNo(const PageNo: Integer);
begin
  if self.FData <> nil then
  begin
    self.FData.Position := 0;
    // Passing in nil tells the helper to use the existing data
    self.LoadFromStream(Nil, PageNo);
  end;
end;

{ TPictureHelper }

function TPictureHelper.GetPageCount: Integer;
begin
  if self.Graphic is TMPWICImage then
    Result := TMPWICImage(self.Graphic).PageCount
  else
    Result := 1;
end;

function TPictureHelper.GetPageNo: Integer;
begin
  if self.SupportsMultipage then
    Result := 1 + TMPWICImage(self.Graphic).PageNo
  else
    Result := 1;
end;

function TPictureHelper.GetSupportsMultipage: Boolean;
begin
  Result := (self.Graphic <> nil) and (self.Graphic.InheritsFrom(TMPWICImage));
end;

procedure TPictureHelper.LoadFromMultiPageFile(const Filename: string;
  APageNo: Integer);
begin
  if not sameText(copy(ExtractFileExt(Filename), 2, 3), 'tif') then
  begin
    self.LoadFromFile(Filename);
    exit;
  end;

  SupportsMultipage := True;
  TMPWICImage(self.Graphic).LoadFromFile(Filename, APageNo - 1);

  changed(self);
end;

procedure TPictureHelper.LoadFromMultiPageStream(Stream: TStream;
  PageNo: Integer; FileType: String = 'tif');
begin
  {$IF CompilerVersion >= 21.0}
  if not pos('tif', lowercase(FileType)) < 1 then
     self.LoadFromStream(Stream)
  else
  {$IFEND}
  begin
    SupportsMultipage := True;
    TMPWICImage(self.Graphic).LoadFromStream(Stream, PageNo - 1);
  end;

end;

procedure TPictureHelper.SetPageNo(const PageNo: Integer);
begin
  if Assigned(self.Graphic) and (self.Graphic is TMPWICImage) then
    TMPWICImage(self.Graphic).SetPageNo(PageNo - 1);
end;

procedure TPictureHelper.SetSupportsMultipage(const Value: Boolean);
var
  NewGraphic: TMPWICImage;
begin
  if SupportsMultipage then
    exit;

  NewGraphic := TMPWICImage.Create;
  try
    NewGraphic.OnProgress := Progress;
    // if self.Bitmap <> nil then
    // NewGraphic.Assign(Self.Bitmap);
  except
    NewGraphic.Free;
    raise;
  end;

  self.Graphic := NewGraphic;
  self.Graphic.OnChange := changed;

end;

{ TIMageHelper }

function TImageHelper.GetPageCount: Integer;
begin
  Result := self.Picture.PageCount;
end;

function TImageHelper.GetPageNumber: Integer;
begin
  Result := 0;
  if not Assigned(self.Picture.Graphic) then
    exit;
  Result := self.Picture.PageNumber;
end;

procedure TImageHelper.SetPageNumber(const Value: Integer);
begin
  if not Assigned(self.Picture.Graphic) then
    exit;

  self.Picture.SetPageNo(Value);
  // self.Repaint;
end;

end.

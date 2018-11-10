unit fmtestMultipage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ExtDlgs, Spin,
  // support for standard images -- these are not required for multimage
  // but show that all formats are still supported.
  jpeg, gifimg, pngimage,
  // Now add multipage image support
  MultiPageImage;

type
  TForm1 = class(TForm)
    OpenPictureDialog1: TOpenPictureDialog;
    Image1: TImage;
    Button1: TButton;
    Panel1: TPanel;
    SpinEdit1: TSpinEdit;
    Label1: TLabel;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure UpdateLabel;
    procedure Button2Click(Sender: TObject);
  private
    fCurrentFile: String;
    fLoading: boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  startDir: string;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  tstringlist.Create;
  if not OpenPictureDialog1.execute then
    exit;
  fCurrentFile := OpenPictureDialog1.Filename;
  fLoading := true;
  try
    self.Image1.Picture.LoadFromMultiPageFile(OpenPictureDialog1.Filename, 1);
    self.SpinEdit1.MaxValue := self.Image1.Picture.PageCount;
    self.SpinEdit1.MinValue := 1;
    self.SpinEdit1.Value := 1;
  finally
    fLoading := false;
  end;
  UpdateLabel;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  lSinglePage: TImage;
begin
  lSinglePage := TImage.Create(nil);
  try
    lSinglePage.Width := Image1.Picture.Width;
    lSinglePage.Height := Image1.Picture.Height;
    lSinglePage.Canvas.Draw(0, 0, Image1.Picture.Graphic);
    lSinglePage.Picture.SaveToFile(ChangeFileExt(OpenPictureDialog1.Filename,
      IntTostr(Image1.PageNumber) + '.' +
      ExtractFileExt(OpenPictureDialog1.Filename)));
  finally
    freeandnil(lSinglePage);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  startDir := extractfilepath(application.ExeName);
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  if self.SpinEdit1.Value > self.SpinEdit1.MaxValue then
  begin
    self.SpinEdit1.Value := self.SpinEdit1.MaxValue;
    exit;
  end;
  if self.SpinEdit1.Value < self.SpinEdit1.MinValue then
  begin
    self.SpinEdit1.Value := self.SpinEdit1.MinValue;
    exit;
  end;

  if (not fLoading) and (assigned(self.Image1.Picture)) then
    self.Image1.PageNumber := self.SpinEdit1.Value;

  UpdateLabel;
end;

procedure TForm1.UpdateLabel;
begin
  self.Label1.Caption := format('Page %u of %u', [self.Image1.PageNumber,
    self.Image1.PageCount]);

end;

end.

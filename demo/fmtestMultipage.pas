unit fmtestMultipage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ExtDlgs, Spin;

type
  TForm1 = class(TForm)
    OpenPictureDialog1: TOpenPictureDialog;
    Image1: TImage;
    Button1: TButton;
    Panel1: TPanel;
    SpinEdit1: TSpinEdit;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
  private
    fCurrentFile: String;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  startDir: string;

implementation

uses Graphic.multipage;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  tstringlist.Create;
  if not OpenPictureDialog1.execute then
    exit;
  fCurrentFile := OpenPictureDialog1.Filename;
  self.Image1.Picture.LoadFromMultiPageFile(OpenPictureDialog1.Filename, 0);
  self.SpinEdit1.Tag := -1;
  self.SpinEdit1.MaxValue := self.Image1.Picture.PageCount + 1;
  self.SpinEdit1.MinValue := 1;
  self.SpinEdit1.Value := 1;
  self.SpinEdit1.Tag := 0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  startDir := extractfilepath(application.ExeName);
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  if (self.SpinEdit1.Tag >= 0) and (assigned(self.Image1.Picture)) then
    self.Image1.PageNumber := self.SpinEdit1.Value - 1;
  self.Label1.Caption := format('Page %u of %u',
    [self.SpinEdit1.Value, self.Image1.PageCount]);
end;

end.

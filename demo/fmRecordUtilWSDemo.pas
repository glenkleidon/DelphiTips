unit fmRecordUtilWSDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdHTTP, IdCustomTCPServer, IdCustomHTTPServer, IdHTTPServer, RecordUtils,
  IdContext,  ExtCtrls, Spin, WebCardsApi;



type
  TForm1 = class(TForm)
    IdHTTP1: TIdHTTP;
    Button1: TButton;
    IdHTTPServer1: TIdHTTPServer;
    ListBox1: TListBox;
    CardPanel: TPanel;
    SpinEdit1: TSpinEdit;
    DealButton: TButton;
    ListBox2: TListBox;
    CardsLeftLabel: TLabel;
    ServerGroupBox: TGroupBox;
    Memo1: TMemo;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure DealButtonClick(Sender: TObject);
    procedure IdHTTPServer1CommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    fServerCards: TCards;
    Procedure EnableDealing(AEnable: boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}



procedure TForm1.Button1Click(Sender: TObject);
begin
  self.fServerCards.Shuffle;
  self.ListBox1.Items.Text := fServerCards.Names;
  self.ListBox1.ItemIndex := 0;
  self.ListBox1Click(Self);
end;

procedure TForm1.DealButtonClick(Sender: TObject);
var lCardsDealt,i : integer;
    lCard : TWebCard;
    lElement: PWebCard;
begin
  lCardsDealt:=0;
  i :=-1;
  self.ListBox2.Items.Clear;
  while lCardsDealt<Self.SpinEdit1.Value do
  begin
    inc(i);
    lCard := self.fServerCards.Deck.Values[i];
    if not lCard.isDealt then
    begin
      inc(lCardsDealt);
      // IMPORTANT DISTINCTION
      //   lCard here is a copy of the pointer to the Element
      //   lElement is a Pointer TO the 'i'th Record element.
      listbox1.ItemIndex := i;
      lElement := self.fServerCards.Deck.Elements[i];
      lElement.isDealt := true;
      listBox2.Items.Add(lCard.Name);
      ListBox1Click(self);
    end;

    if i>ListBox1.items.count-1 then exit;
  end;
  listBox1.SetFocus;
  Self.EnableDealing(true);
end;

procedure TForm1.EnableDealing(AEnable: boolean);
begin
  self.DealButton.Enabled := AEnable;
  self.SpinEdit1.Enabled := AEnable;
  self.ListBox2.Enabled := AEnable;
  self.CardsLeftLabel.Caption := IntToStr(Self.fServerCards.Count);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  //
  EnableDealing(false);
end;

procedure TForm1.IdHTTPServer1CommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
Type TSWebCardRequest = TRecordSerializer<TWebCardRequest>;
  var lWebRequest : TSWebCardRequest;
begin

  lWebRequest.Parse(ARequestInfo.Params);
  lWebRequest.Value.AcceptResponse := ARequestInfo.Accept;
  try
    AResponseInfo.ContentText := Self.fServerCards.Deal(lWebRequest);
  except
    on e:exception do
    begin
      AResponseInfo.ResponseNo := 500;
      AResponseInfo.ContentText := E.Message;
    end;
  end;

end;

procedure TForm1.ListBox1Click(Sender: TObject);
var lCard: TSWebCard;
begin
  self.EnableDealing(false);
  self.CardPanel.Caption := '';
  if Listbox1.ItemIndex=-1 then exit;

  self.EnableDealing(True);
  lCard := self.fServerCards.Deck.Values[ListBox1.ItemIndex];
  self.CardPanel.Font.Color := lCard.value.Colour;
  self.CardPanel.Caption := lCard.value.Name;
  if lCard.value.isDealt then
    self.CardPanel.Font.Style := [fsItalic]
  else
    self.CardPanel.Font.Style := [fsBold];
  memo1.Text := lCard.AsJSON;
end;


procedure TForm1.Timer1Timer(Sender: TObject);
begin
  self.ListBox1Click(Self);
end;

end.

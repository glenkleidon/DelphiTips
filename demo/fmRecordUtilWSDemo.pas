unit fmRecordUtilWSDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdHTTP, IdCustomTCPServer, IdCustomHTTPServer, IdHTTPServer, RecordUtils,
  IdContext,  ExtCtrls, Spin, WebCardsApi;



type
  TServerForm = class(TForm)
    Button1: TButton;
    DeckWebServer: TIdHTTPServer;
    ListBox1: TListBox;
    CardPanel: TPanel;
    SpinEdit1: TSpinEdit;
    DealButton: TButton;
    ListBox2: TListBox;
    CardsLeftLabel: TLabel;
    ServerGroupBox: TGroupBox;
    Timer1: TTimer;
    DealerGroupBox: TGroupBox;
    GroupBox1: TGroupBox;
    ClientType: TComboBox;
    AddClientButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure DealButtonClick(Sender: TObject);
    procedure DeckWebServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure Timer1Timer(Sender: TObject);
    procedure AddClientButtonClick(Sender: TObject);
  private
    { Private declarations }
    fServerCards: TCards;
    Procedure EnableDealing(AEnable: boolean);
  public
    { Public declarations }
  end;

var
  ServerForm: TServerForm;

implementation

{$R *.dfm}
   uses fmRecordUtilsWSClient, Shellapi;


procedure TServerForm.AddClientButtonClick(Sender: TObject);
var lPlayerForm: TPlayerForm;
    lURL : string;
begin
  lURL := Format('http://localhost:%u/',
                       [self.DeckWebServer.DefaultPort]);
  case self.ClientType.ItemIndex of
    0 : //
        begin
          lPlayerForm:= TPlayerForm.Create(Self);
          lPlayerForm.URLEdit.Text := lUrl+'Deal';
          lPlayerForm.Show; // now owned by the form
        end;
    1 : //
        begin
          shellexecute(self.Handle,'open',pchar(lURL+'/Client'),'','',SW_SHOWNORMAL);
        end;
  end;
end;

procedure TServerForm.Button1Click(Sender: TObject);
begin
  self.fServerCards.Shuffle;
  self.ListBox1.Items.Text := fServerCards.Names;
  self.ListBox1.ItemIndex := 0;
  self.ListBox1Click(Self);
end;

procedure TServerForm.DealButtonClick(Sender: TObject);
var lCards : TCards;
    lWebRequest : TWebCardRequest;
begin
  self.ListBox2.Items.Clear;
  lWebRequest.NumberOfCards := self.SpinEdit1.Value;
  lWebRequest.PlayersName := 'Server';
  lCards.Deck := self.fServerCards.Deal(lWebRequest);
  listBox2.Items.Text := lCards.Names;
  listBox1.SetFocus;
  Self.EnableDealing(true);
end;

procedure TServerForm.EnableDealing(AEnable: boolean);
begin
  self.DealButton.Enabled := AEnable;
  self.SpinEdit1.Enabled := AEnable;
  self.ListBox2.Enabled := AEnable;
  self.CardsLeftLabel.Caption := IntToStr(Self.fServerCards.Count);
end;

procedure TServerForm.FormCreate(Sender: TObject);
begin
  EnableDealing(false);
end;

procedure TServerForm.DeckWebServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  var lWebRequest : TSWebCardRequest;
  Function contentStreamAsString: string;
  var lStream: TStringStream;
  begin
     lStream := TStringStream.Create();
     try
       lStream.CopyFrom(ARequestInfo.PostStream,0);
       result := lStream.DataString;
     finally
       freeAndNil(lStream);
     end;
  end;
begin
  if ARequestInfo.ContentLength<1 then
    lWebRequest.FromURLEncoded(ARequestInfo.QueryParams)
  else if pos('json',lowercase(ARequestInfo.ContentType))>0 then
    lWebREquest.FromJSON(ContentStreamAsString)
  else lWebRequest := ContentStreamAsString;

  // ensure we have a good return type.
  if lWebRequest.Value.AcceptResponse='' then
      lWebRequest.Value.AcceptResponse := ARequestInfo.Accept;
  if lWebRequest.Value.AcceptResponse='' then
      lWebRequest.Value.AcceptResponse := ARequestInfo.ContentType;

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

procedure TServerForm.ListBox1Click(Sender: TObject);
var lCard: TSWebCard;
begin
  self.EnableDealing(false);
  self.CardPanel.Caption := '';
  if Listbox1.ItemIndex=-1 then exit;

  self.EnableDealing(True);
  lCard := self.fServerCards.Deck.Values[ListBox1.ItemIndex];

  DrawCard(Self.CardPanel, lCard.Value);
end;


procedure TServerForm.Timer1Timer(Sender: TObject);
var lPosition: Integer;
begin
  lPosition := Self.fServerCards.Deck.Position;
  if timer1.Tag <> lPosition then
  begin
     self.ListBox1Click(Self);
     self.ListBox1.ItemIndex := lPosition;
  end;
  timer1.Tag := lPosition;
end;

end.

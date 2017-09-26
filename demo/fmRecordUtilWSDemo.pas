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
    procedure CardPanelClick(Sender: TObject);
    procedure DeckWebServerCommandOther(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  private
    { Private declarations }
    fServerCards: TCards;
    fWebClientCSSStream : TMemoryStream;
    fWebClientStream: TMemoryStream;
    Procedure EnableDealing(AEnable: boolean);
    Procedure GetWebClient(AStream: TStream);
    Procedure GetStyleSheet(AStream: TStream);
    procedure GetLocalFile(AFilename: string; AStream: TStream);
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
          shellexecute(self.Handle,'open',pchar(lURL+'WebClient'),'','',SW_SHOWNORMAL);
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

procedure TServerForm.CardPanelClick(Sender: TObject);
begin
  // Quick reset of Web Client for debugging purposes
  freeandnil(self.fWebClientStream);
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

procedure TServerForm.GetStyleSheet(AStream: TStream);
begin
  if self.fWebClientCSSStream=nil then
  begin
    fWebClientCSSStream := TMemoryStream.Create;
    GetLocalFile('webclient.css', fWebClientCSSStream);
  end;
  AStream.copyfrom(fWebClientCSSStream,0);
end;

procedure TServerForm.GetWebClient(AStream: TStream);
begin
  if self.fWebClientStream=nil then
  begin
    fWebClientStream := TMemoryStream.Create;
    GetLocalFile('webclient.html', fWebClientStream);
  end;
  AStream.copyfrom(fWebClientStream,0);
end;

procedure TServerForm.GetLocalFile(AFilename: string; AStream: TStream);
var lFileStream : TFileStream;
    lFilePath: String;
begin
  lFilePath := ExtractFilePath(ParamStr(0)) + AFileName;
  lFileStream := TFileStream.Create(lFilePath, fmOpenRead or fmShareDenyNone);
  try
    AStream.CopyFrom(lFileStream,0);
  finally
    freeandnil(lFileStream);
  end;
end;

procedure TServerForm.DeckWebServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  var lWebRequest : TSWebCardRequest;
      lOrigin : string;
  Function contentStreamAsString: string;
  var lStream: TStringStream;
  begin
     if ARequestInfo.PostStream=nil then
       Result := ARequestInfo.FormParams
     else
     begin
       lStream := TStringStream.Create();
       try
         lStream.CopyFrom(ARequestInfo.PostStream,0);
         result := lStream.DataString;
       finally
         freeAndNil(lStream);
       end;
     end;
  end;
begin
  // Access control - allow AJAX redirects.
  lOrigin := ArequestInfo.RawHeaders.values['Origin'];
  if length(lOrigin)=0 then lOrigin := '*';
  AresponseInfo.CustomHeaders.Values['Access-Control-Allow-Origin'] := lOrigin;


  if Pos('favicon',lowercase(ARequestInfo.URI))>0 then
  begin
    AResponseInfo.ResponseNo := 404;
    Exit;
  end;

  /// Style sheet for the client
  if pos('.css',lowercase(ARequestInfo.URI))>0 then
  begin
     AResponseInfo.ContentStream := TMemoryStream.create;
     GetStyleSheet(AResponseInfo.ContentStream);
     AResponseInfo.ContentType := 'text/css; charset=utf-8';
     AResponseInfo.ResponseNo := 200;
     exit;
  end;

  // Are they requesting the WebClient?
  if pos('webclient',lowercase(ARequestInfo.URI))>0 then
  begin
     AResponseInfo.ContentStream := TMemoryStream.create;
     GetWebClient(AResponseInfo.ContentStream);
     AResponseInfo.ContentType := 'text/html; charset=utf-8';
     AResponseInfo.ResponseNo := 200;
     exit;
  end;


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

procedure TServerForm.DeckWebServerCommandOther(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  var lOrigin: string;
begin
  // Set any access control...
  if sameText(ARequestInfo.Command ,'OPTIONS') then
  begin
    lOrigin := ArequestInfo.RawHeaders.values['Origin'];
    if length(lOrigin)=0 then lOrigin := '*';
    // Return Access Control and Options.
    AResponseInfo.ResponseNo := 200;
    AResponseINfo.ResponseText := '';
    AResponseInfo.Date := Now;
    AResponseinfo.CustomHeaders.Values['Access-Control-Allow-Origin'] := lOrigin;
    AResponseinfo.CustomHeaders.Values['Access-Control-Allow-Methods'] := 'POST, GET, OPTIONS';
    AResponseinfo.CustomHeaders.Values['Access-Control-Allow-Headers'] := 'Content-Type, Origin, Access-Control-Allow-Origin, Access-Control-Allow-Methods';
    AResponseinfo.CustomHeaders.Values['Access-Control-Max-Age'] := '86400';
    AResponseinfo.CustomHeaders.Values['Vary'] := 'Accept-Encoding, Origin';
    exit;
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

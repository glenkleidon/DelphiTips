unit fmRecordUtilsWSClient;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, WebCardsApi, StdCtrls, Spin, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP;

type
  TPlayerForm = class(TForm)
    DefaultCardPanel: TPanel;
    ScrollBox1: TScrollBox;
    CardHolderPanel: TPanel;
    DealButton: TButton;
    SpinEdit1: TSpinEdit;
    PlayersNameEdit: TLabeledEdit;
    ResponseFormat: TComboBox;
    Label1: TLabel;
    URLEdit: TLabeledEdit;
    ErrorMsg: TLabel;
    procedure DealButtonClick(Sender: TObject);
    procedure PlayersNameEditChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    fCards: TCards;
    function Fetch(AWebRequest: TSWebCardRequest): String;
    procedure AddCard(AWebCard: TWebCard);
    Procedure ShowCards;
  public
    { Public declarations }
  end;

var
  PlayerForm: TPlayerForm;

Procedure DrawCard(APanel: TPanel; AWebCard: TWebCard);

implementation
  uses RecordUtils;

procedure DrawCard(APanel: TPanel; AWebCard: TWebCard);
begin
  APanel.Font.Color := AWebCard.Colour;
  APanel.Caption := AWebCard.Name;
  if AWebCard.isDealt then
    APanel.Font.Style := [fsItalic]
  else
    APanel.Font.Style := [fsBold];
end;


{$R *.dfm}

procedure TPlayerForm.AddCard(AWebCard: TWebCard);
var lPanel: TPanel;
begin
  lPanel := TPanel.Create(nil);
  lPanel.Color := DefaultCardPanel.Color;
  lPanel.Align := AlLeft;
  lPanel.Height := DefaultCardPanel.Height;
  lPanel.Width := DefaultCardPanel.Width;
  lPanel.BevelOuter := bvNone;
  lPanel.Left :=  CardHolderPanel.ComponentCount*lPanel.Width;
  lPanel.Parent := CardHolderPanel;
  DrawCard(lPanel, AWebCard);
end;

procedure TPlayerForm.DealButtonClick(Sender: TObject);
var lWebRequest: TSWebCardRequest;
begin
  self.ErrorMsg.Visible := false;
  lWebRequest.Value.PlayersName := self.PlayersNameEdit.Text;
  lWebRequest.Value.NumberOfCards := Self.SpinEdit1.Value;
  lWebRequest.Value.AcceptResponse := self.ResponseFormat.Text;

  self.fCards.Deck.Parse(Fetch(lWebRequest),spmAppend, -1,
              SerializerEncodingFromText(lWebRequest.Value.AcceptResponse));
  self.ShowCards;
end;

function TPlayerForm.Fetch(AWebRequest: TSWebCardRequest): String;
var lhttp:TidHttp;
    lList : TStringlist;
    lFormat: String;
begin
  lHttp := TidHttp.Create(nil);
  lList := TStringlist.Create;
  try

    lFormat := lowercase(AWebRequest.value.AcceptResponse);
    lhttp.Request.Accept := lFormat;
    lhttp.Request.ContentType := lFormat;

    case SerializerEncodingFromText(lFormat) of
        seValuePairs: lList.Text := AWebRequest.AsValuePairs;
        seJSON: lList.Text := AWebRequest.AsJSON;
        seURLEncoding: lList.Text := AWebRequest.AsURLEncoded;
    end;
    try
     result := lhttp.Post(self.URLEdit.Text,lList);
    Except
     on e:EIdHTTPProtocolException do
     begin
       self.ErrorMsg.Caption := e.ErrorMessage;
       self.ErrorMsg.Visible := true;
     end;
    end;

  finally
    freeandnil(lHttp);
    freeandnil(lList);
  end;

end;

procedure TPlayerForm.FormShow(Sender: TObject);
begin
  self.fCards.Deck.clear;
end;

procedure TPlayerForm.PlayersNameEditChange(Sender: TObject);
begin
  Caption := 'Player: '+ Self.PlayersNameEdit.Text;
end;

procedure TPlayerForm.ShowCards;
begin
  showMessage(intToStr(Self.fCards.Deck.Count));
end;

end.

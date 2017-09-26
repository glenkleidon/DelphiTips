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
    UserPanel: TPanel;
    procedure DealButtonClick(Sender: TObject);
    procedure PlayersNameEditChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    fCards: TCards;
    function Fetch(AWebRequest: TSWebCardRequest): String;
    procedure AddCard(AWebCard: TWebCard);
    procedure RemoveCards;
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
    lWidth : integer;
begin
  if AWebCard.Rank='' then exit;
  lPanel := TPanel.Create(nil);
  lPanel.Font := DefaultCardPanel.font;
  lPanel.Color := DefaultCardPanel.Color;
  lPanel.Align := AlLeft;
  lPanel.Height := DefaultCardPanel.Height;
  lPanel.Width := DefaultCardPanel.Width;
  lPanel.BevelOuter := bvLowered;
  lWidth := (1+CardHolderPanel.ControlCount)*lPanel.Width+8;
  if CardHolderPanel.width<lWidth then CardHolderPanel.Width := lWidth;
  lPanel.Left := lWidth-1;
  lPanel.Parent := CardHolderPanel;
  lPanel.Tag := -100; // easy way to identify them
  DrawCard(lPanel, AWebCard);
  lPanel.SetFocus;
  ScrollBox1.ScrollInView(lPanel);
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
    lRequestContent : TStringStream;
    lFormat: String;
begin
  lHttp := TidHttp.Create(nil);
  lREquestContent := nil;
  try

    lFormat := lowercase(AWebRequest.value.AcceptResponse);
    lhttp.Request.Accept := lFormat;
    lhttp.Request.ContentType := lFormat;

    case SerializerEncodingFromText(lFormat) of
        seValuePairs: lRequestContent :=
                TStringStream.Create(AWebRequest.AsValuePairs);
        seJSON: lRequestContent :=
                TStringStream.Create(AWebRequest.AsJSON);
        seURLEncoding: lRequestContent :=
                TStringStream.Create(AWebRequest.AsURLEncoded);
    end;
    try
     result := lhttp.Post(self.URLEdit.Text,lRequestContent);
    Except
     on e:EIdHTTPProtocolException do
     begin
       self.ErrorMsg.Caption := e.ErrorMessage;
       self.ErrorMsg.Visible := true;
     end;
    end;

  finally
    freeandnil(lHttp);
    freeandnil(lRequestContent);
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

procedure TPlayerForm.RemoveCards;
var lCardPanel: TControl;
    i: integer;
begin
  for i := CardHolderPanel.ControlCount-1 downto 0 do
  begin
    lCardPanel := CardHolderPanel.Controls[i];
    if (lCardPanel.ClassNameIs('TPanel')) and (lCardPanel.Tag=-100) then
    begin
       lCardPanel.parent.RemoveControl(lCardPanel);
       lCardPanel.Free;
    end;
  end;
end;

procedure TPlayerForm.ShowCards;
var lCard: TWebCard;
begin
  // Remove all The Cards
  RemoveCards;
  // ReAdd all the cards, including any new ones.
  for lCard in self.fCards.Deck.Values do
    if lCard.Rank<>'' then AddCard(lCard);

end;

end.

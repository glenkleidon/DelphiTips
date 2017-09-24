unit WebCardsApi;

interface
 uses Graphics, SysUtils,RecordUtils;

const
  CARD_RANKS = ' 2 3 4 5 6 7 8 910 J Q K A';

  ENCODING_FORMATS : array[0..2] of string = (
      'application/octet-stream',
      'application/json',
      'application/x-www-form-urlencoded');

type
  TCardSuit = (Harts,Diamonds,Clubs,Spades);

  PWebCard = ^TWebCard;
  TWebCard = Record
    Rank : string;
    Suit : TCardSuit;
    isDealt : boolean;
  End;

  TWebCardHelper = Record Helper for TWebCard
    Function Name : string;
    Function Colour : TColor;
  end;

  TSWebCard = TRecordSerializer<TWebCard>;

  TWebCardRequest = Record
    NumberOfCards: Integer;
    PlayersName: String;
    AcceptResponse: String;
  End;

  TSWebCardRequest = TRecordSerializer<TWebCardRequest>;

  TCards = Record
    Deck : TSWebCard;
    public
     Procedure Shuffle;
     Function Names: String;
     Function Count: integer;
     Function Deal(AWebRequest: TWebCardRequest): String;
  End;

function SerializerEncodingFromText(AText: String): TSerializerEncoding;
function TextFromSerializerEncoding(AEncoding: TSerializerEncoding): string;

implementation

{ TDeck }

function SerializerEncodingFromText(AText: String): TSerializerEncoding;
var i: integer;
begin
  result := TSerializerEncoding.seJSON;
  for i := 0 to 2 do
    if sameText(AText,ENCODING_FORMATS[i]) then
    begin
      Result := TSerializerEncoding(i);
      exit;
    end;
end;

function TextFromSerializerEncoding(AEncoding: TSerializerEncoding): string;
begin
  result := ENCODING_FORMATS[ord(AEncoding)];
end;

function TCards.Count: integer;
var lCard: TWebCard;
begin
  result := 0;
  for lCard in Deck.Values do
    if not lCard.isDealt then inc(result);
end;

function TCards.Deal(AWebRequest: TWebCardRequest): String;
var lCards: TSWebCard;
    lCard : PWebCard;
    i,c : integer;
begin
  lCards.count := AWebRequest.NumberOfCards;
  if (Deck.Count<1) then raise Exception.Create('Deck not Shuffled.');
  if (Deck.position >= Deck.Count-1) then raise Exception.Create('No Cards left in Deck');
  
  i := Deck.position-1;
  c := 0;
  while c<AWebRequest.NumberOfCards do
  begin
    inc(i);
    lCard := Self.Deck.Elements[i];
    if lCard=nil then exit;
    if not lCard.isDealt then
    begin
       lCards.Clone(lCard^,c);
       inc(c);
       lCard.isDealt := true;
    end;
    if (i>Self.Deck.Count-1) then break;
  end;
  self.Deck.Position := i;
  lCards.Count := c;

  if pos('application/json',lowercase(AWebRequest.AcceptResponse))>0 then
    Result := lCards.AsJSON
  else if pos('urlencoded',lowercase(AWebRequest.AcceptResponse))>0 then
    Result := lCards.AsURLEncoded
  else
    Result := lCards.AsValuePairs;
end;

function TCards.Names: String;
var lName: string;
    lCard : TWebCard;
begin
  result := '';
  for lCard in Self.Deck.Values do
    result := Result + lCard.Name + #13#10;
end;

procedure TCards.Shuffle;
var lCard: TWebCard;
    lRank: string;
    lSuit: TCardSuit;
    i,p: integer;
begin
  self.Deck.Clear;
  Self.Deck.isArray := true;
  self.Deck.Count := 52;
  i := 0;
  while i<13 do
  begin
    lRank := trim(copy(CARD_RANKS,1+(i*2),2));
    inc(i);
    for lSuit in [Harts,Diamonds,Clubs,Spades] do
      begin
        lCard.Rank := lRank;
        lCard.Suit := lSuit;
        lCard.isDealt := false;
        p := Random(52);
        repeat
          inc(p);
          if p>51 then p:=0;
          if Deck.values[p].Rank='' then
          begin
            Deck.Clone(lCard,p);
            break;
          end;
        until (false);
      end;
  end;
end;

{ TWebCard }

function TWebCardHelper.Colour: TColor;
begin
  case self.Suit of
    Harts,
    Diamonds: result := clRed;
    Clubs,
    Spades: Result := clBlack;
  end;
end;

function TWebCardHelper.Name: string;
begin
  result := Self.Rank+char(ord(self.Suit)+3);
end;


end.

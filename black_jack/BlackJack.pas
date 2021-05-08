unit BlackJack;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, pngimage, Vcl.StdCtrls;

type
  CardType = (Hearts, Diamonds, Clubs, Spades);

type
  Card = Record
    cType : CardType;
    cNumber : integer;
  End;

type
  Player = Record
    name : string;
    money : integer;
    card1 : Card;
    card2 : Card;
  End;

const MAX_CARD_NUMBER = 13;
const DECK_SIZE = 52;

type
  TForm1 = class(TForm)
    Player1Card1: TImage;
    NewGameButton: TButton;
    Player1Card2: TImage;
    Player2Card1: TImage;
    Player2Card2: TImage;
    Player3Card1: TImage;
    Player3Card2: TImage;
    DealerCard1: TImage;
    DealerCard2: TImage;
    procedure NewGameButtonClick(Sender: TObject);
  private

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

var deckActualSize : integer;


implementation

{$R *.dfm}

function GetCardImagePath(card : Card) : string;
var
  path : string;
  i : integer;
begin
  path := 'C:\Users\w\Desktop\delphi\black_jack\images\';


  if card.cNumber = 11 then
  begin
    path := path + 'J';
  end
  else if card.cNumber = 12 then
  begin
    path := path + 'Q';
  end
  else if card.cNumber = 13 then
  begin
    path := path + 'K';
  end
  else if card.cNumber = 1 then
  begin
    path := path + 'A';
  end
  else
  begin
    path := path + IntToStr(card.cNumber);
  end;

  if card.cType = Hearts then
  begin
    path := path + 'H';
  end
  else if card.cType = Diamonds then
  begin
    path := path + 'D';
  end
  else if card.cType = Clubs then
  begin
    path := path + 'C';
  end
  else if card.cType = Spades then
  begin
    path := path + 'S';
  end;

  path := path + '.PNG';

  Result := path;
end;

procedure MakeDeck(var d : array of Card);
var i : CardType;
var j : integer;
var index : integer;
var c : Card;
begin
  index := 0;
  for i := Hearts to Spades do
  begin
    for j := 1 to 13 do
    begin
      with c do
      begin
        cType := i;
        cNumber := j;
      end;
      d[index] := c;
      Inc(index);
    end;
  end;

  deckActualSize := 1;
end;

procedure ShuffleDeck(var d : array of Card);
var i : integer;
var r : integer;
var tmpCard : Card;
begin
  for i := 1 to DECK_SIZE do
  begin
    r := i + random(DECK_SIZE - i);
    tmpCard := d[r];
    d[r] := d[i];
    d[i] := tmpCard;
  end;
end;

function GetCard(var d : array of Card) : Card;
begin
  Result := d[Length(d) - deckActualSize];
  Inc(deckActualSize);
end;

procedure TForm1.NewGameButtonClick(Sender: TObject);
var deck : array[1..DECK_SIZE] of Card;
var card1 : Card;
var card2 : Card;
begin
  MakeDeck(deck);
  ShuffleDeck(deck);

  card1 := GetCard(deck);
  card2 := GetCard(deck);

  Player1Card1.Picture.LoadFromFile(GetCardImagePath(card1));
  Player1Card2.Picture.LoadFromFile(GetCardImagePath(card2));

end;

end.

unit BlackJack;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, pngimage, Vcl.StdCtrls;

const CardBackPath = 'C:\Users\w\Desktop\delphi\black_jack\images\red_back.PNG';

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
    cards : array[1..6] of Card;
    handSize : integer;
  End;

const MAX_CARD_NUMBER = 13;
const DECK_SIZE = 52;

var player1 : Player;
var dealer : Player;

var moneyIn : integer;
var deck : array[1..DECK_SIZE] of Card;

type
  TForm1 = class(TForm)
    Player1Card1: TImage;
    NewGameButton: TButton;
    Player1Card2: TImage;
    DealerCard1: TImage;
    DealerCard2: TImage;
    Player1Card3: TImage;
    Player1Card4: TImage;
    Player1Card5: TImage;
    Player1Card6: TImage;
    DealerCard3: TImage;
    DealerCard4: TImage;
    DealerCard5: TImage;
    DealerCard6: TImage;
    PlayerValueLabel: TLabel;
    DealerValueLabel: TLabel;
    StartButton: TButton;
    PutMoneyEdit: TEdit;
    MoneyInLabel: TLabel;
    MoneyHasLabel: TLabel;
    HitButton: TButton;
    EndGamePanel: TPanel;
    StayButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure NewGameButtonClick(Sender: TObject);
    procedure PutMoneyEditKeyPress(Sender: TObject; var Key: Char);
    procedure HitButtonClick(Sender: TObject);
    procedure StayButtonClick(Sender: TObject);
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

function DrawCard(var d : array of Card) : Card;
begin
  Result := d[Length(d) - deckActualSize];
  Inc(deckActualSize);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  EndGamePanel.Visible := false;

  Player1Card1.Visible := false;
  Player1Card2.Visible := false;
  Player1Card3.Visible := false;
  Player1Card4.Visible := false;
  Player1Card5.Visible := false;
  Player1Card6.Visible := false;

  DealerCard1.Visible := false;
  DealerCard2.Visible := false;
  DealerCard3.Visible := false;
  DealerCard4.Visible := false;
  DealerCard5.Visible := false;
  DealerCard6.Visible := false;

  StartButton.Visible := false;

  MoneyInLabel.Visible := false;
  PutMoneyEdit.Visible := false;
  MoneyHasLabel.Visible := false;

  HitButton.Visible := false;
  StayButton.Visible := false;
  PlayerValueLabel.Visible := false;
end;

function EvaluateHand(p : Player): integer;
var i : integer;
var aceCount : integer;
var aceValue : integer;
var value : integer;
begin
   value := 0;

   for i := 1 to p.handSize do
   begin
      if p.cards[i].cNumber > 9 then
      begin
          value := value + 10;
      end
      else if p.cards[i].cNumber <> 1 then
      begin
          value := value + p.cards[i].cNumber;
      end;
   end;

   aceCount := 0;
   for i := 1 to p.handSize do
   begin
     if p.cards[i].cNumber = 1 then
     begin
       aceCount := aceCount + 1;
     end;
   end;

   if aceCount = 1 then
   begin
     if value > 11 then
     begin
        aceValue := 1;
     end
     else
     begin
        aceValue := 11;
     end;
   end
   else if aceCount = 2 then
   begin
     if value = 0 then
     begin
        aceValue := 11;
     end
     else
     begin
        aceValue := 1;
     end;
   end
   else
   begin
      aceValue := 1;
   end;

   Result := value + aceValue * aceCount;
end;

procedure TForm1.HitButtonClick(Sender: TObject);
var value : integer;
begin
  player1.handSize := player1.handSize + 1;
  player1.cards[player1.handSize] := DrawCard(deck);

  if player1.handSize = 3 then
  begin
    Player1Card3.Visible := true;
    Player1Card3.Picture.LoadFromFile(GetCardImagePath(player1.cards[3]));
  end
  else if player1.handSize = 4 then
  begin
    Player1Card4.Visible := true;
    Player1Card4.Picture.LoadFromFile(GetCardImagePath(player1.cards[4]));
  end
  else if player1.handSize = 5 then
  begin
    Player1Card5.Visible := true;
    Player1Card5.Picture.LoadFromFile(GetCardImagePath(player1.cards[5]));
  end
  else if player1.handSize = 6 then
  begin
    Player1Card6.Visible := true;
    Player1Card6.Picture.LoadFromFile(GetCardImagePath(player1.cards[6]));
  end;

  value := EvaluateHand(player1);
  PlayerValueLabel.Caption := 'Value: ' + IntToStr(value);

  if value > 21 then
  begin
    ShowMessage('You are over!');
    EndGamePanel.Visible := true;
  end;
end;

procedure TForm1.NewGameButtonClick(Sender: TObject);
begin
  EndGamePanel.Visible := false;

  Player1Card1.Visible := false;
  Player1Card2.Visible := false;

  DealerCard1.Visible := false;
  DealerCard2.Visible := false;

  PlayerValueLabel.Visible := false;
  DealerValueLabel.Visible := false;

  StartButton.Visible := true;
  MoneyInLabel.Visible := true;
  PutMoneyEdit.Visible := true;
  MoneyHasLabel.Visible := true;
  player1.money := 100;
  moneyIn := 0;
  MoneyHasLabel.Caption := 'Money: ' + IntToStr(player1.money);
  MoneyInLabel.Caption := 'Money in: ' + IntToStr(moneyIn);
  PutMoneyEdit.Enabled := true;
  PlayerValueLabel.Visible := false;
  StayButton.Visible := false;
  HitButton.Visible := false;
end;

procedure TForm1.PutMoneyEditKeyPress(Sender: TObject; var Key: Char);
var i : integer;
begin
  if ord(Key) = VK_RETURN then
  begin
    if not TryStrToInt(PutMoneyEdit.Text, i) then
    begin
      ShowMessage('"' + PutMoneyEdit.Text + '" is not a valid value for money');
    end
    else if player1.money - StrToInt(PutMoneyEdit.Text) >= 0 then
    begin
      moneyIn := moneyIn + StrToInt(PutMoneyEdit.Text);
      player1.money := player1.money - StrToInt(PutMoneyEdit.Text);
      MoneyInLabel.Caption := 'Money in: ' + IntToStr(moneyIn);
      MoneyHasLabel.Caption := 'Money: ' + IntToStr(player1.money);
      PutMoneyEdit.Text := '';
    end
    else
    begin
      ShowMessage('You does not have that much money!');
    end;
  end;
end;

procedure TForm1.StartButtonClick(Sender: TObject);
begin
  if moneyIn <= 0 then
  begin
     ShowMessage('Put money in!');
  end
  else
  begin
    PutMoneyEdit.Enabled := false;
    PlayerValueLabel.Visible := true;
    HitButton.Visible := true;
    StayButton.Visible := true;

    MakeDeck(deck);
    ShuffleDeck(deck);

    //player
    Player1Card1.Visible := true;
    Player1Card2.Visible := true;
    player1.cards[1] := DrawCard(deck);
    player1.cards[2] := DrawCard(deck);
    player1.handSize := 2;

    Player1Card1.Picture.LoadFromFile(GetCardImagePath(player1.cards[1]));
    Player1Card2.Picture.LoadFromFile(GetCardImagePath(player1.cards[2]));

    PlayerValueLabel.Caption := 'Value: ' + IntToStr(EvaluateHand(player1));

    //dealer
    DealerCard1.Visible := true;
    DealerCard2.Visible := true;
    dealer.cards[1] := DrawCard(deck);
    dealer.cards[2]:= DrawCard(deck);
    dealer.handSize := 2;

    DealerCard1.Picture.LoadFromFile(GetCardImagePath(dealer.cards[1]));
    DealerCard2.Picture.LoadFromFile(CardBackPath);
  end;
end;

procedure TForm1.StayButtonClick(Sender: TObject);
begin
  DealerCard2.Picture.LoadFromFile(GetCardImagePath(dealer.cards[2]));
  DealerValueLabel.Caption := 'Value: ' + IntToStr(EvaluateHand(dealer));
end;

end.

unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type TFruit = Class(TObject)
  private
    isRound  : Boolean;
    length   : single;
    width    : single;
    diameter : single;
  protected
  public
    constructor Create(diameter : single); overload;
    constructor Create(length : single; width : single); overload;
  published
    property round : Boolean
      read   isRound;
    property len   : single
      read   length;
    property wide  : single
      read   width;
    property diam  : single
      read   diameter;
end;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure ShowFruit(fruit : TFruit);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

// Create a round fruit object
constructor TFruit.Create(diameter: single);
begin
  // Indicate that we have a round fruit, and set its size
  isRound       := true;
  self.diameter := diameter;
end;

// Create a long fruit object
constructor TFruit.Create(length, width: single);
begin
  // Indicate that we have a long fruit, and set its size
  isRound     := false;
  self.length := length;
  self.width  := width;
end;

procedure TForm1.FormCreate(Sender: TObject);
var apple, banana : TFruit;
begin
  apple  := TFruit.Create(3.5);
  banana := TFruit.Create(7.0, 1.75);

  ShowFruit(apple);
  ShowFruit(banana);
end;

procedure TForm1.ShowFruit(fruit: TFruit);
begin
  if fruit.round
  then ShowMessage('We have a round fruit, with diam = '+
                   FloatToStr(fruit.diam))
  else
  begin
    ShowMessage('We have a long fruit with length: ' + FloatToStr(fruit.len) + ' and width: ' + FloatToStr(fruit.wide));
  end;
end;

end.
